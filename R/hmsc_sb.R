# Hmsc for seed bank data
# maybe set rdnbr to 0, incorporate unburned seedbank without being collinear

# setup ========================================================================
# source("R/topo_extract.R")
# source("R/rdnbr_calculation.R")
source("R/a_data_prep.R")
load("data/topo.Rda")
load("data/rdnbr_mtbs.Rda")
load("data/dnbr_mtbs.Rda")

library(knitr)
library(ape)
library(MASS)
library(fields)
library(Hmsc)
library(parallel)
library(ggthemes)
library(corrplot)
library(ggpubr)
set.seed(1)
# theme_set(theme_pubr())

lut_vars <- c("depthtop2" = "Depth",
              "depth" = "Depth",
              "elevation" = "Elevation",
              "f_aspect" = "Folded Aspect",
              "rdnbr" = "Burn Severity",
              "sh_sb_u_top2" = "Pre-Fire Diversity",
              "Random: plot" = "Random: Plot")

# data wrangling ===============================================================
# veg community

veg<- sb_wide %>%
  filter(burned == "b") %>%
  as.data.frame %>%
  # filter(plot != "02", plot != "03")%>% # need this to be able to use fuel continuity
  mutate(row = paste(plot, burned, depth, sep="_")) %>%
  tibble::column_to_rownames("row") %>%
  dplyr::select(-plot,-burned,-depth)

C <- veg %>%
  as.matrix

C[C>0] <-1
C<- C[,colSums(C)>1]

colnames(C) <- str_replace_all(colnames(C), " ", "_")
rownames(C) <- str_replace_all(rownames(C), " ", "_")
dim(veg)

prevalence<- colSums(C) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value)

# colnames(C) <- str_replace_all(colnames(C), " ", "_")

# coords <-  plots %>%
#   rbind(plots) %>% # repeating
#   arrange(Name) %>%
#   st_coordinates %>%
#   as.data.frame()  %>%
#   dplyr::rename("x-coordinate" = X, "y-coordinate" = Y)
#env data

XData <- sb_wide %>%
  filter(burned == "b")%>%
  mutate(row = paste(plot, burned, depth, sep="_")) %>%
  dplyr::select(plot,depth, row, burned)%>%
  left_join(rdnbr) %>%
  left_join(topo  %>% dplyr::select(plot, f_aspect)) %>%
  left_join(shannon_wide %>% dplyr::select(sh_sb_u_top2, plot))%>%
  mutate(depth = as.factor(depth),
         burned = as.factor(burned),
         plot=as.factor(plot))%>%
  as.data.frame %>% 
  tibble::column_to_rownames("row") %>%
  dplyr::select(plot, depth, burned, f_aspect,sh_sb_u_top2,
                elevation, rdnbr=rdnbr_b120)#%>%
  # mutate("y-coordinate" = coords[,1],
  #        "x-coordinate" = coords[,2])


# dim(XData)
#species traits
# traits <- as.data.frame(veg_traits_b) %>%
#   dplyr::select(-fg) %>%
#   transmute_all((as.factor)) %>%
#   tibble::column_to_rownames("species")
#rownames(traits) <- str_replace_all(rownames(traits), " ", "_")


# tr_form <- ~origin+duration+cots

XFormula <- ~depth+elevation+rdnbr + f_aspect + sh_sb_u_top2
studyDesign <- data.frame(plot = XData$plot)
rownames(studyDesign) <- rownames(XData)
rL = HmscRandomLevel(units = studyDesign$plot)
# rL$nfMax = 15

studyDesignSp <- data.frame(plot = as.factor(1:nrow(XData)))
rownames(studyDesign) <- rownames(XData)

rLs = HmscRandomLevel(sData = coords%>% as.matrix()) %>%
  setPriors(nfMin=1, nfMax=1)

# fitting the model ============================================================
# mpois = Hmsc(Y = C, XData = XData, XFormula = XFormula, distr = "poisson",
#          studyDesign = studyDesign, ranLevels = list(plot=rL))
mprob = Hmsc(Y = C, XData = XData, XFormula = XFormula, distr = "probit",
          studyDesign = studyDesign, ranLevels = list(plot=rL))

nChains = 2
test.run = TRUE
if (test.run){
  #with this option, the vignette evaluates in ca. 10 minutes in a laptop
  thin = 1
  samples = 100
  transient = 50
}else{
  #with this option, the vignette evaluates in ca. 2 hrs in a laptop
  thin = 10
  samples = 1000
  transient = ceiling(thin*samples*.5)
}
final_run <- TRUE
if(final_run){
  nChains <- 4
  thin <-100
  samples <- 1000
  transient = ceiling(thin*samples*.5)
}
mpfn <- "data/mp_mod_prob_landsat.Rda"
mpfn <- "data/mp_mod_prob_mtbs_dnbr.Rda"
dir.create("mcmc")
if(final_run) mpfn <- str_replace(mpfn, ".Rda", "_final.Rda")

if(!file.exists(mpfn)){
  t0 <- Sys.time()
  m = sampleMcmc(mprob, thin = thin, 
                 samples = samples, 
                 transient = transient,
                 adaptNf = rep(ceiling(0.4*samples*thin),1),
                 nChains = nChains, 
                 nParallel = nChains)
  print(Sys.time()-t0)
  save(m, file = mpfn)
  system(paste("aws s3 cp",
               mpfn,
               file.path("s3://earthlab-amahood/seed_bank", mpfn)))
}else{load(mpfn)}

#calculating things ===================
mpost <- convertToCodaObject(m)

preds = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=preds)
mfp_df <- data.frame(Species = colnames(m$Y),
                     SR2 = MF$TjurR2,
                     RMSE = MF$RMSE,
                     AUC = MF$AUC)
VP <- computeVariancePartitioning(m)

# plotting model convergence ===================================================
fn_conv <- "images/geldman_sb_oc.png"
if(final_run) fn_conv<- str_replace(fn_conv, ".png", "_final.png")

ess.beta <- effectiveSize(mpost$Beta) %>%
  as_tibble() %>% dplyr::rename(ess_beta = value)
psrf.beta <- gelman.diag(mpost$Beta, multivariate=FALSE)$psrf%>%
  as_tibble() %>% dplyr::rename(psrf_beta = `Point est.`)

conv_plot<- ggarrange(ggplot(ess.beta, aes(x=ess_beta)) + 
                        geom_histogram() +
                        scale_x_continuous(n.breaks = 3)+
                        theme_pubr(),
          ggplot(psrf.beta, aes(x=psrf_beta)) + 
            geom_histogram() +
            scale_x_continuous(n.breaks =3)+
            theme_pubr()+
            theme(axis.title.y=element_blank(),
                  axis.text.x = element_text(hjust = 0.8)))+
  theme(panel.border = element_rect(fill=NA, size=0.5))+
  ggsave(filename = fn_conv, width = 7.5, height =4, bg="white")

# plotting variance partitioning ===============================================
sbquants <- summary(mpost$Beta)$quantiles %>%
  as_tibble(rownames = "variable") %>% 
  mutate(sign = `2.5%` * `97.5%`) %>%
  filter(sign>0) %>%
  separate(variable,
          into = c("variable", "species"),
          sep = ",") %>%
  mutate(variable = str_sub(variable, 3,nchar(variable)-5),
         species = str_sub(species, 2,nchar(species)-6)) %>%
  filter(variable!= "(Intercept)") %>%
  dplyr::select(variable,species,`2.5%`,`50%`,`97.5%`) %>%
  arrange(species)

mean_vp <- VP$vals %>%
  as_tibble(rownames = "var") %>%
  pivot_longer(cols = names(.)[2:ncol(.)],names_to = "species", values_to = "value") %>%
  group_by(var) %>%
  dplyr::summarise(mean_vp = mean(value)) %>%
  ungroup

vp_df <- VP$vals%>%
  as_tibble(rownames = "variable") %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "value")%>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species])

vp_order_n <- vp_df %>%
  left_join(prevalence%>%
              mutate(Species = lut_prevalent_species[Species])) %>%
  filter(variable == "rdnbr") %>%
  arrange((prevalence)) %>%
  mutate(Species_f = factor(Species, levels = .$Species)) %>%
  dplyr::select(Species, Species_f)

vp_plot<- vp_df %>%
  left_join(vp_order_n) %>%
  group_by(variable) %>%
  arrange(value) %>%
  ungroup() %>%
  left_join(mean_vp, by= c("variable"= "var")) %>%
  mutate(value = value,
         variable = paste0(lut_vars[variable], 
                           " (",
                           round(mean_vp*100), 
                           "%)")) %>%
  ggplot(aes(x=value,y=reorder(Species_f,Species), 
             fill = variable)) +
  geom_bar(stat="identity")+
  theme_clean() +
  theme(legend.position = "right")+
  ylab("Species") +
  xlab("Variance Explained") +
  scale_fill_colorblind(name = "Variable\n(Avg Proportion Explaned)")



# betaplots! ===================================================================
fn_beta <- "images/betas_binomial.png"
if(final_run) fn_beta<- str_replace(fn_beta, ".png", "_final.png")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

postBeta <- getPostEstimate(m, parName = "Beta")

means <- postBeta$mean %>%
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], names_to = "Species", values_to = "Mean")%>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species])

supported <- postBeta$support %>% 
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "Support") %>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species],
         support = ifelse(Support<0.5,(1-Support)/1,Support),
         over90 = ifelse(support>0.95, "0", "1"))%>%
  filter(support > 0.80)%>%
  left_join(means, by = c("env_var", "Species"))%>%
  mutate(sign = ifelse(Mean>0, "+", "-"))%>%
  left_join(vp_order_n) %>%
  filter(env_var !="intercept") %>%
  mutate(env_var = lut_vars[env_var])

beta_plot<-ggplot(supported, aes(x=env_var,y=reorder(Species_f,Species), fill = sign)) +
  geom_tile(lwd=.5,aes(alpha = support, color = over90)) +
  theme_clean()+
  guides(alpha="none", color = "none")+
  scale_color_manual(values = c("black", "transparent"))+
  scale_fill_manual(values = c("firebrick", "#007DBA")) +
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust = 1),
        axis.title = element_blank()) +
  ggsave(fn_beta)

# sanity check
plotBeta(m, post = postBeta, param = "Support",
         supportLevel = 0.90, split=.4, spNamesNumbers = c(T,F))


# all together

ggarrange(conv_plot,beta_plot, nrow=2, heights = c(1,2), labels = c("(a)", "(b)")) %>%
  ggarrange(vp_plot, nrow=1, widths = c(1,1.5), labels = c("", "(c)")) +
  ggsave("images/jsdm_stuff.png", height = 6, width = 12.5)

#

# gradients ====================================================================
fn_rdnbr<-"images/probit_preds_rdnbr_d.png"
if(final_run) fn_rdnbr <- str_replace(fn_rdnbr, ".png", "_final.png")

Gradient = constructGradient(m, focalVariable = "rdnbr")#, 
                             #non.focalVariables = list(depth=list(3,"top2")))
predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew, 
                ranLevels=Gradient$rLNew, expected=TRUE)

n_runs <- nChains*samples

pred_df <- do.call("rbind", predY) %>%
  as_tibble() %>%
  mutate(rdnbr = rep(Gradient$XDataNew$rdnbr, n_runs)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -rdnbr) %>%
  group_by(Species, rdnbr) %>%
  dplyr::summarise(mean = median(cover),
            upr = quantile(cover, 0.975),
            lwr = quantile(cover, 0.025)) %>%
  ungroup() %>%
  mutate(Species = str_replace_all(Species," ", "_"),
         Species = lut_prevalent_species[Species]) %>%
  left_join(prevalence) %>%
  arrange(desc(prevalence)) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species))) %>%
  filter(Species != "unknown_forb") 

pred_df_raw <-do.call("rbind", predY) %>%
  as_tibble() %>%
  mutate(rdnbr = rep(Gradient$XDataNew$rdnbr, n_runs),
         run = rep(1:n_runs,each=20)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -c(rdnbr,run))%>%
  mutate(Species = str_replace_all(Species," ", "_")) %>%
  left_join(prevalence) %>%
  mutate(origin = lut_origin[Species]) %>%
  arrange(origin,desc(prevalence)) %>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species]) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species)))

p_preds <- pred_df_raw  %>%
  ggplot(aes(x=rdnbr, y=cover)) +
  geom_line(alpha=0.05, aes(group=run, color = origin), key_glyph = "rect")+
  geom_line(data = pred_df,lwd=1, alpha=0.95, color="black", aes(y=mean))+
  facet_wrap(~Species_f, nrow=2)+
  xlab("Burn Severity (RdNBR)") +
  ylab("Probability of Occurrence") +
  guides(color=guide_legend(override.aes = list(alpha=1)))+
  scale_color_manual(values = c("#FFC845", "#007DBA"), 
                     labels = c("Introduced", "Native"))+
  theme_pubr()+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        strip.text = element_text(face = "italic"),
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank(),
        panel.border = element_rect(fill=NA, size =.75))+
  ggsave(fn_rdnbr, width=10.5, height=6.5)

save(p_preds, file = "data/binomial_preds_plot.Rda")

# gradient depth ==================

Gradient = constructGradient(m, focalVariable = "depth",
                             non.focalVariables=list(elevation=list(1),
                                                     sh_sb_u_top2=list(1),
                                                     rdnbr=list(1),
                                                     f_aspect=list(1)))

predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew, 
                ranLevels=Gradient$rLNew, expected=TRUE)

n_runs <- nChains*samples

pred_df <- do.call("rbind", predY) %>%
  as_tibble() %>%
  mutate(depth = rep(Gradient$XDataNew$depth, n_runs)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -depth) %>%
  group_by(Species, depth) %>%
  dplyr::summarise(mean = median(cover),
                   upr = quantile(cover, 0.975),
                   lwr = quantile(cover, 0.025)) %>%
  ungroup() %>%
  mutate(Species = str_replace_all(Species," ", "_"),
         Species = lut_prevalent_species[Species]) %>%
  left_join(prevalence) %>%
  arrange(desc(prevalence)) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species))) %>%
  filter(Species != "unknown_forb")

pred_df_raw <-do.call("rbind", predY) %>%
  as_tibble() %>%
  mutate(depth = rep(Gradient$XDataNew$depth, n_runs),
         run = rep(1:n_runs,each=2)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -c(depth,run))%>%
  mutate(Species = str_replace_all(Species," ", "_")) %>%
  left_join(prevalence) %>%
  mutate(origin = lut_origin[Species]) %>%
  arrange(origin,desc(prevalence)) %>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species]) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species)))

pred_df_raw  %>%
  mutate(depth = ifelse(depth == "top2", "0-2", "2-6")) %>%
  ggplot(aes(x=depth, y=cover)) +
  geom_jitter(alpha=0.05, aes(color = origin))+
  geom_boxplot(fill = "transparent", outlier.shape = NA)+
  facet_wrap(~Species_f, nrow=2)+
  xlab("Soil Depth (cm)") +
  ylab("Probability of Occurrence") +
  guides(color=guide_legend(override.aes = list(alpha=1)))+
  scale_color_manual(values = c("#FFC845", "#007DBA"), 
                     labels = c("Introduced", "Native"))+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.background = element_rect(fill="transparent"),
        legend.title = element_blank())+
  ggsave("images/probit_preds_depth.png", width=10.5, height=6.5)

