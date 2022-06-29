# Hmsc for seed bank data
# maybe set rdnbr to 0, incorporate unburned seedbank without being collinear

# setup ========================================================================
source("R/topo_extract.R")
source("R/rdnbr_calculation.R")

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
theme_set(theme_classic())


# data wrangling ===============================================================
# veg community

veg<- sb_wide_p%>%
  filter(burned == "b") %>%
  as.data.frame %>%
  # filter(plot != "02", plot != "03")%>% # need this to be able to use fuel continuity
  mutate(row = paste(plot, burned, sep="_")) %>%
  tibble::column_to_rownames("row") %>%
  dplyr::select(-plot,-burned)

C <- veg %>%
  as.matrix

C[C>0] <-1
C<- C[,colSums(C)>1]

colnames(C) <- str_replace_all(colnames(C), " ", "_")
rownames(C) <- str_replace_all(rownames(C), " ", "_")
dim(C)

prevalence<- colSums(C) %>%
  as_tibble(rownames = "Species") %>%
  rename(prevalence = value)

# colnames(C) <- str_replace_all(colnames(C), " ", "_")

#env data

XData <- sb_wide_p %>%
  filter(burned == "b")%>%
  mutate(row = paste(plot, burned, sep="_")) %>%
  dplyr::select(plot, row, burned)%>%
  left_join(rdnbr) %>%
  left_join(topo  %>% dplyr::select(plot, f_aspect)) %>%
  left_join(shannon_wide %>% dplyr::select(sh_sb_u_top2, plot))%>%
  mutate(burned = as.factor(burned),
         plot=as.factor(plot))%>%
  as.data.frame %>% 
  tibble::column_to_rownames("row") %>%
  dplyr::select(plot, burned, f_aspect,sh_sb_u_top2,
                elevation = Elevation, rdnbr=rdnbr_b20)


dim(XData)
#species traits
# traits <- as.data.frame(veg_traits_b) %>%
#   dplyr::select(-fg) %>%
#   transmute_all((as.factor)) %>%
#   tibble::column_to_rownames("species")
#rownames(traits) <- str_replace_all(rownames(traits), " ", "_")


# tr_form <- ~origin+duration+cots

XFormula <- ~elevation+rdnbr + f_aspect + sh_sb_u_top2
studyDesign <- data.frame(plot = XData$plot)
rownames(studyDesign) <- rownames(XData)
rL = HmscRandomLevel(units = studyDesign$plot)
# rL$nfMax = 15

# fitting the model ============================================================
# mpois = Hmsc(Y = C, XData = XData, XFormula = XFormula, distr = "poisson",
#          studyDesign = studyDesign, ranLevels = list(plot=rL))
mprob = Hmsc(Y = C, XData = XData, XFormula = XFormula, distr = "probit",
             studyDesign = studyDesign, ranLevels = list(plot=rL))
nChains = 2
test.run = FALSE
if (test.run){
  #with this option, the vignette evaluates in ca. 10 minutes in a laptop
  thin = 1
  samples = 100
  transient = 50
}else{
  #with this option, the vignette evaluates in ca. 2 hrs in a laptop
  thin = 10
  samples = 1000
  transient = 500
}
verbose = TRUE

mpfn <- "mcmc/mp_mod_prob_p.Rda"

if(!file.exists(mpfn)){
  m = sampleMcmc(mprob, thin = thin, samples = samples, transient = transient,
                 nChains = nChains, nParallel = nChains, verbose = verbose)
  save(m, file = mpfn)
}else{load(mpfn)}

ns = 50

# mcmc convergence ===================
mpost <- convertToCodaObject(m)

ess.beta <- effectiveSize(mpost$Beta) %>%
  as_tibble() %>% rename(ess_beta = value)
psrf.beta <- gelman.diag(mpost$Beta, multivariate=FALSE)$psrf%>%
  as_tibble() %>% rename(psrf_beta = `Point est.`)

ess.gamma = effectiveSize(mpost$Gamma)%>%
  as_tibble() %>% rename(ess_gamma = value)
psrf.gamma = gelman.diag(mpost$Gamma, multivariate=FALSE)$psrf%>%
  as_tibble() %>% rename(psrf_gamma = `Point est.`)

# sppairs = matrix(sample(x = 1:ns^2, size = 100))
# tmp = mpost$Omega[[1]]
# for (chain in 1:length(tmp)){
#   tmp[[chain]] = tmp[[chain]][,sppairs]
# }
# 
# ess.omega = effectiveSize(tmp)%>%
#   as_tibble() %>% rename(ess_omega = value)
# psrf.omega = gelman.diag(tmp, multivariate=FALSE)$psrf%>%
#   as_tibble() %>% rename(psrf_omega = `Point est.`)

ggarrange(ggplot(ess.beta, aes(x=ess_beta)) + geom_histogram(),
          ggplot(ess.gamma, aes(x=ess_gamma)) + geom_histogram(),
          # ggplot(ess.omega, aes(x=ess_omega)) + geom_histogram(),
          ggplot(psrf.beta, aes(x=psrf_beta)) + geom_histogram(),
          ggplot(psrf.gamma, aes(x=psrf_gamma)) + geom_histogram()#,
          #ggplot(psrf.omega, aes(x=psrf_omega)) + geom_histogram()
          ,align = "v")

# 

preds = computePredictedValues(m)
# preds = computePredictedValues(mp)

MF = evaluateModelFit(hM=m, predY=preds)
# MF = evaluateModelFit(hM=mp, predY=preds)

MF$RMSE %>% hist()
MF$AUC%>% hist()
MF$TjurR2 %>% hist()


mfp_df <- data.frame(Species = colnames(m$Y),
                     SR2 = MF$TjurR2,
                     RMSE = MF$RMSE,
                     AUC = MF$AUC)
# plotting variance partitioning ===============================================

VP <- computeVariancePartitioning(m)

VP$R2T$Beta
VP$R2T$Y


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


vp_df <- VP$vals%>%
  as_tibble(rownames = "variable") %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "value") #%>%
# left_join(mf_df)

vp_order_n <- vp_df %>%
  left_join(prevalence) %>%
  filter(variable == "rdnbr") %>%
  arrange((prevalence)) %>%
  mutate(Species_f = factor(Species, levels = .$Species)) %>%
  dplyr::select(Species, Species_f)# %>%
#left_join(mf_df)

vp_df %>%
  left_join(vp_order_n) %>%
  group_by(variable) %>%
  arrange(value) %>%
  ungroup() %>%
  mutate(value = value) %>%
  ggplot(aes(x=value,y=reorder(Species_f,Species), fill = variable)) +
  geom_bar(stat="identity")+
  theme_classic() +
  # geom_hline(yintercept = table(veg_traits$origin)[1]+.5) +
  # geom_hline(yintercept = nrow(veg_traits)+.5) +
  # annotate("text", x = .65, y=4, label="Introduced", angle=90, vjust="bottom",
  #          hjust="left", size=8)+
  # annotate("text", x = .65, y=44, label="Native", angle=90, vjust="bottom",
  #          hjust="left", size=8)+
  ylab("Species") +
  xlab("Variance Explained") +
  scale_fill_colorblind()#+
# theme(legend.position = c(1,.315),
#       legend.justification = c(1,0),
#       legend.background = element_rect(color="black"))


# table(veg_traits$origin)[1]

# species niches ...basically ==================================================

postBeta <- getPostEstimate(m, parName = "Beta")

means <- postBeta$mean %>%
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], names_to = "Species", values_to = "Mean")

supported <- postBeta$support %>% 
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "Support") %>%
  filter(Support >0.95|Support<0.05,
         env_var != "intercept") %>%
  left_join(means, by = c("env_var", "Species"))%>%
  mutate(sign = ifelse(Mean>0, "+", "-"))%>%
  left_join(vp_order_n)

ggplot(supported, aes(x=env_var,y=reorder(Species_f,Species), fill = Mean, color = sign)) +
  geom_tile(lwd=.5) +
  theme_clean()+
  scale_fill_gradient2(mid = "grey90") +
  scale_color_manual(values = c(("red"), ("blue"))) +
  guides(color = "none")+
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust = 1),
        axis.title = element_blank()) +
  ggsave("images/betas_binomial.png")



plotBeta(m, post = postBeta, param = "Support",
         supportLevel = 0.95, split=.4, spNamesNumbers = c(T,F))

# gammas are something else =========
# postGamma = getPostEstimate(m, parName = "Gamma")
# plotGamma(m, post=postGamma, param="Support", supportLevel = 0.95)

# =============================
# OmegaCor = computeAssociations(m)
# supportLevel = 0.60
# toPlot = ((OmegaCor[[1]]$support>supportLevel) + 
#             (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean
# corrplot(toPlot, method = "color", 
#          col=colorRampPalette(c("blue","white","red"))(200),
#          tl.cex=.6, tl.col="black",
#          title=paste("random effect level:", m$rLNames[1]), mar=c(0,0,1,0))
# 
# hmdf <- OmegaCor[[1]]$mean
# heatmap(hmdf)

# gradients ====================================================================
# gradient==========

Gradient = constructGradient(m, focalVariable = "rdnbr",
                             non.focalVariables=list(elevation=list(1),
                                                     sh_sb_u_top2=list(1),
                                                     #depth=list(1),
                                                     f_aspect=list(1)))

predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew, 
                ranLevels=Gradient$rLNew, expected=TRUE)
# plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE)
# plotGradient(m, Gradient, pred=predY, measure="Y", index = 1, showData = TRUE)
# plotGradient(m, Gradient, pred=predY, measure="Y", index = 2, showData = TRUE)
# plotGradient(m, Gradient, pred=predY, measure="Y", index = 3, showData = TRUE)
# plotGradient(m, Gradient, pred=predY, measure="Y", index = 4, showData = TRUE)
# plotGradient(m, Gradient, pred=predY, measure="Y", index = 7, showData = TRUE)

# plotGradient(m, Gradient, pred=predY, measure="T", index = 1, showData = TRUE)


n_runs <- nChains*samples

pred_df <- do.call("rbind", predY) %>%
  as_tibble() %>%
  mutate(rdnbr = rep(Gradient$XDataNew$rdnbr, n_runs)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -rdnbr) %>%
  group_by(Species, rdnbr) %>%
  summarise(mean = median(cover),
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
  arrange(desc(prevalence)) %>%
  mutate(origin = lut_origin[Species]) %>%
  filter(Species != "unknown_forb")%>%
  mutate(Species = lut_prevalent_species[Species]) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species)))

# veg_pa<-veg_burned[,2:68]
# veg_pa[veg_pa>0] <- 1
# veg_pa$plot <- veg_burned$plot
# obs <- left_join(veg_pa, sites_w_grazing)

# pred_df %>% filter(species == 'Bromus tectorum'|
#                      species == 'Poa secunda'|
#                      species == 'Lepidium perfoliatum'|
#                      species == 'Artemisia tridentata ssp. wyomingensis')%>%
#   mutate(species = replace(species,
#                            species =="Artemisia tridentata ssp. wyomingensis",
#                            "A. tridentata"))%>%
#   ggplot(aes(x=tsf, y= mean)) +
#   geom_line()+
#   geom_line(lty=2, aes(y=upr))+
#   geom_line(lty=2, aes(y=lwr))+
#   facet_wrap(~species,nrow=1)+
#   xlab("Time Since Fire") +
#   ylab("Predicted Probability of Occurrence") +
#   ggsave("images/binomial_preds_tsf.png", width=7, height=3)

pred_df_raw  %>%
  ggplot(aes(x=rdnbr, y=cover)) +
  geom_line(alpha=0.05, aes(group=run, color = origin))+
  geom_line(data = pred_df,lwd=1, alpha=0.95, color="black", aes(y=mean))+
  facet_wrap(~Species_f, scales = "free_y")+
  xlab("Burn Severity (RdNBR)") +
  ylab("Probability of Occurrence") +
  guides(color=guide_legend(override.aes = list(alpha=1)))+
  scale_color_manual(values = c("#FFC845", "#007DBA"), 
                     labels = c("Introduced", "Native"))+
  theme(legend.position = c(.74,0.1))+
  ggsave("images/probit_preds_rdnbr_p.png", width=7.5, height=7.5)

# pred_df %>%
#   #filter(fg != "NC" & fg != "IW" & fg != "IPG") %>%
#   ggplot(aes(x=rdnbr, y= mean, group=species)) +
#   geom_line(lwd=1, alpha=0.5)+
#   facet_wrap(~species, scales = "free_y") +
#   xlab("Burn Severity (RdNBR") +
#   ylab("Probability of Occurrence") +
#   theme(legend.position = c(1,.45),
#         legend.justification = c(1,1))+
#   ggsave("images/fg_preds_tsf_binomial.png", height=7, width=7) 
# 
# ggplot(pred_df, aes(x=tsf, y= mean, group=species,color = origin)) +
#   geom_line(lwd=1, alpha=0.5)+
#   geom_line(lty=2, aes(y=upr))+
#   geom_line(lty=2, aes(y=lwr)) +
#   facet_wrap(~species)+
#   theme(axis.text=element_blank())