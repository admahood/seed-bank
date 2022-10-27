# specific review response figs

# r1.3.1: burn severity and post-fire presence of IAG in the seedbank are affected by pre-fire IAG abundance.

source("R/a_data_prep.R")
theme_set(theme_classic())
library(geomtextpath)
library(ggnewscale)
library(broom)
dw<-div_wide %>%
  filter(year == 2016, burned == 'u') %>%
  dplyr::select(plot, year, brte) %>%
  mutate(plot = str_remove_all(plot,"s"))

d <- left_join(sb_counts_brte, rdnbr) %>%
  filter(burned == "b") %>%
  dplyr::select(postfire_brte_count = count, plot, rdnbr_mean, Elevation) %>%
  filter() %>%
  left_join(dw) %>%
  left_join(fuel_continuity %>% filter(state == "unburned"))

library(MASS)

# alternate hypotheses =========================================================
mod1<-lm(rdnbr_mean ~ brte + Elevation, d)
mod2<-glm.nb(postfire_brte_count ~ brte+Elevation, data=d);summary(mod2)

rrf1 <- d %>%
  rename(`Pre-fire connectivity`= ff_continuity,
         `Burn Severity` = rdnbr_mean,
         `Post-fire Cheatgrass Seed Count` = postfire_brte_count) %>%
  pivot_longer(cols = c(`Post-fire Cheatgrass Seed Count`, `Burn Severity`, `Pre-fire connectivity`),
               names_to = "response",
              values_to = "value") %>%
  ggplot(aes(y=value, x=brte)) +
  geom_point() +
  xlab("Prefire Cheatgrass Cover") +
  facet_wrap(~response, scales="free") +
  theme(axis.title.y=element_blank())

ggsave(rrf1, filename = "images/revresp1.png", width=7.5, height=3.5)


mod3 <- lm(ff_continuity ~ brte, d); summary(mod3)
mod3.1 <- lm(TotalFolia ~ InvAnnGras, blm_aim);summary(mod3.1)
mod3.1 <- lm(TotalFolia ~ InvAnnGras, blm_aim %>% filter(SagebrushC > 20));summary(mod3.1)

# straight up unburned seeds predicting burned seeds ===========================

seed2seed<-sb_counts_brte %>%
  pivot_wider(names_from = burned, values_from = count) %>%
  ggplot(aes(x=u, y=b)) +
  geom_point() +
  xlab("Pre-fire Cheatgrass Seed Count") +
  ylab("Post-fire Cheatgrass Seed Count")

ggsave(seed2seed, filename = "images/seed2seed.png", height=3.5, width=3.5, bg="white")

mod4<- sb_counts_brte %>%
  pivot_wider(names_from = burned, values_from = count) %>%
  glm.nb(b~u, data=.)

bind_rows(broom::tidy(mod1) %>% mutate(model = "burn_severity ~ prefire_brte_cover"),
          broom::tidy(mod2) %>% mutate(model = "postfire_brte_seeds ~ prefire_brte_cover"),
          broom::tidy(mod2) %>% mutate(model = "prefire_fuel_connectivity ~ prefire_brte_cover"),
          broom::tidy(mod4) %>% mutate(model = "postfire_brte_seeds ~ prefire_brte_seeds"))

# barplots =====================================================================
# r1.14 L 352. “almost all of the plots … lacked any structural heterogeneity.” 
# This is interesting. Do your data demonstrate this? 

barplot <- div %>%
  mutate(fg = lut_duration_lf[species]) %>%
  filter(species != "little") %>%
    mutate(plot = str_remove_all(plot,"s")) %>%
  mutate(burned = str_sub(plot, 3,3)) %>%
  mutate(state = str_c(burned, "_", year)) %>%
  mutate(state = factor(state, levels = c("u_2016", "b_2016", "b_2019"),
                        labels = c("Unburned", "Burned, 2016", "Burned, 2019"))) %>%
  mutate(plot = str_remove_all(plot,"u")) %>%
  mutate(plot = str_remove_all(plot,"b")) %>%
  mutate(fg = ifelse(fg == "sh", "woody", "herbaceous")) %>%
  filter(plot != "02", plot != "03") %>%
  # filter(state == "Burned, 2019") %>%
  group_by(plot, state, fg) %>%
  summarise(cover = sum(cover, na.rm=TRUE)/11,
            state=first(state)) %>%
  ungroup() %>%
  na.omit() %>%
  ggplot(aes(x=state, y=cover, fill=fg)) +
  geom_bar(stat="identity", color="black") +
  facet_wrap(~plot)

ggsave(barplot, filename = "images/revresp4.png", height=3.5, width=5.5)


boxplot<-barplot_str <-
  li %>%
  mutate(plot = str_remove_all(plot,"s")) %>%
  mutate(plot = str_remove_all(plot,"b")) %>%
  mutate(plot = str_remove_all(plot,"u")) %>%
  mutate(state = str_c(burned, "_", year)) %>%
  mutate(state = factor(state, levels = c("u_2016", "b_2016", "b_2019"),
                        labels = c("Unburned", "Burned, 2016", "Burned, 2019"))) %>%
  ggplot(aes(x=state, y=cover, fill=variable)) +
  # geom_bar(stat="identity", color="black") +
  geom_boxplot()+
  # facet_wrap(~plot) +
   theme(legend.position = c(.65,1),
         legend.background = element_rect(fill="transparent", color="black"),
         legend.justification = c(1,1))

ggarrange(rrf1, ggarrange(seed2seed, boxplot, nrow=1, 
                          labels = c("(b)","(c)"), label.x = 0.12), 
          nrow=2, labels = c("(a)", "")) %>%
  ggsave(filename = "images/revresp1.png", height=7.5, width=7.5, bg="white")

ggsave(barplot_str, filename = "images/revresp4.png", height=8.5, width=8.5)


table_str <-  li %>%
  mutate(plot = str_remove_all(plot,"s")) %>%
  mutate(plot = str_remove_all(plot,"b")) %>%
  mutate(plot = str_remove_all(plot,"u")) %>%
  mutate(state = str_c(burned, "_", year)) %>%
  mutate(state = factor(state, levels = c("u_2016", "b_2016", "b_2019"),
                        labels = c("Unburned", "Burned, 2016", "Burned, 2019"))) %>%
  group_by(state, variable) %>%
  summarise(cover = mean(cover, na.rm=TRUE))

# aim plots ====================================================================

aim <- st_read("data/blm_aim/gbd_plots_w_precip_5_20_19_points.gpkg")

ggplot(aim, aes(x = InvAnnGras, y=TotalFolia)) +
  geom_point()

ggplot(aim, aes(x = InvAnnGras+SagebrushC, y=TotalFolia)) +
  geom_point()

mod5 <- lm(TotalFolia ~ InvAnnGras, aim); summary(mod5)
mod6 <- lm(TotalFolia ~ InvAnnGras*SagebrushC, aim); summary(mod6)


# Path model ================================================
new_node_names <- c("prefire_TVC" = "Pre-fire\nconnectivity",
                    "burn_sev" = "Burn\nSeverity",
                    "Bromus_seeds_post" = "Post-fire Bromus Seeds",
                    "postfire_TVC" = "Post-fire\nconnectivity",
                    "Bromus_cv_pre" = "Pre-fire\nBromus cover",
                    "sb_div_pre" = "Pre-fire\nSeedbank\nDiversity",
                    "ag_div_pre" = "Pre-fire\nAboveground\nDiversity",
                    "elv" = "Elevation")
# h1
library(lavaan)
source("R/ggplot_sem_sb.R")

d1 <- d %>%
  left_join(fuel_continuity %>% filter(state == "postfire3") %>%
              dplyr::select(postfire_TVC = ff_continuity, plot)) %>%
  dplyr::rename(prefire_TVC = ff_continuity, 
                elv = Elevation,
                Bromus_cv_pre = brte) %>%
  mutate(Bromus_seeds_post = log(postfire_brte_count + 1),
         burn_sev = sqrt(rdnbr_mean)) %>%
  left_join(div_stats %>% filter(state == "postfire3")%>%dplyr::select(ag_div_post=shannon, plot))%>%
  left_join(div_stats %>% filter(state == "unburned")%>%dplyr::select(ag_div_pre=shannon, plot)) %>%
  left_join(sb_div_stats_p %>% filter(state == "sb_u_total") %>%
              dplyr::select(sb_div_pre=shannon, plot)) %>%
  left_join(sb_div_stats_p %>% filter(state == "sb_b_total") %>%
              dplyr::select(sb_div_post=shannon, plot))
  
  
pathmod <- 'Bromus_seeds_post ~ burn_sev + ag_div_pre + sb_div_pre  + prefire_TVC#+ Bromus_cv_pre #+elv
            prefire_TVC ~ elv + ag_div_pre + Bromus_cv_pre
            ag_div_pre ~ Bromus_cv_pre + sb_div_pre
            # Bromus_cv_pre ~ elv
            sb_div_pre ~ prefire_TVC + Bromus_cv_pre + elv # h2a
            burn_sev ~  prefire_TVC + ag_div_pre + elv + Bromus_cv_pre + sb_div_pre
            postfire_TVC ~ Bromus_seeds_post  + prefire_TVC + ag_div_pre + burn_sev + Bromus_cv_pre + elv
           ' %>%
  lavaan::sem(data=d1)

summary(pathmod)

ggsem(pathmod, layout="auto", alpha = 0.1)
lavaan::modificationIndices(pathmod) %>% arrange(desc(mi))
fit_stats<-lavaan::fitMeasures(pathmod) %>%
  round(3) %>%
  as_tibble(rownames = "metric") %>%
  filter(metric %in% c("tli", "cfi", "rmsea", "srmr"))

save(pathmod, file = "data/pathmod.Rda")
resid(pathmod, "cor")$cov


layout_df <-  random_layout(pathmod) %>%
  dplyr::mutate(x=replace(x, metric=="prefire_TVC", 0),
         y=replace(y, metric=="prefire_TVC", 1),
         x=replace(x, metric=="burn_sev", 1),
         y=replace(y, metric=="burn_sev", 0),
         x=replace(x, metric=="Bromus_seeds_post", 0),
         y=replace(y, metric=="Bromus_seeds_post", -1),
         x=replace(x, metric=="postfire_TVC", -1),
         y=replace(y, metric=="postfire_TVC", 0),
         x=replace(x, metric=="Bromus_cv_pre", 1.2),
         y=replace(y, metric=="Bromus_cv_pre", 1.2),
         x=replace(x, metric=="elv", -1.2),
         y=replace(y, metric=="elv", 1.2),
         x=replace(x, metric=="ag_div_pre", 1.2),
         y=replace(y, metric=="ag_div_pre", -1.2),
         x=replace(x, metric=="sb_div_pre", -1.2),
         y=replace(y, metric=="sb_div_pre", -1.2))

# edit to show p thresholds... maybe add stars to edge labels?

pm_fig<-ggsem(pathmod, layout = "manual", layout_df = layout_df, alpha=0.05, 
              rename_nodes = T,new_node_names = new_node_names,
              title = "")
ggsave(pm_fig, filename = "images/path_mod.png", height = 9, width=11, bg="white")
save(pm_fig, file = "data/path_model.Rda")


# sup fig ======================================================================
library(ggimage)
blm_aim <- st_read("data/blm_aim/gbd_plots_w_precip_5_20_19_points.gpkg") %>%
  st_set_geometry(NULL) %>%
  # filter(NonInvShru > 10 & InvAnnGras>0) %>%
  arrange(NonInvShru, InvAnnGras) %>%
  mutate(rank = 1:nrow(.))

point_data <- blm_aim %>%
  dplyr::select(NonInvShru, rank,
                `Native Shrub plus Annual Grass Cover` = InvAnnGras, 
                TotalFolia) %>%
  mutate( `Native Shrub Cover`= NonInvShru,
          `Native Shrub plus Annual Grass Cover` = 
           `Native Shrub plus Annual Grass Cover` + `Native Shrub Cover`
        ) %>%
  pivot_longer(names_to = "variable", values_to = "value", cols = c("Native Shrub Cover", "Native Shrub plus Annual Grass Cover"))


# ranks for x axis
ff_fig1 <- ggplot(blm_aim,# %>% filter(NonInvShru > 10 & InvAnnGras>0), 
                  aes(y=NonInvShru+InvAnnGras, x=rank)) +
  geom_segment(aes(y=NonInvShru, yend = InvAnnGras+NonInvShru,
                   xend=rank, color = InvAnnGras))+
  scale_color_viridis_c(name = "Annual\nGrass\nCover") +
  ggnewscale::new_scale_color()+
  geom_point(data = point_data, aes(y=value, color = variable)) +
  scale_color_manual(values = c("grey20","orangered3"), name ="")+
  geom_labelhline(yintercept=60, lty=2, color="grey10", alpha=0.75,
                  label="Hypothetical Connectivity Threshold", hjust=0.02, size=5)+
  xlab("Observations Ranked by Native Shrub Cover") +
  ylab("Percent Cover") +
  theme(legend.position = c(.99,.01),
        text= element_text(size=15),
        legend.justification = c(1,0), legend.background = element_rect(fill="transparent"),
        legend.box = "horizontal",legend.box.just = "bottom",
        plot.background = element_rect(color="black", fill="transparent"));ff_fig1

ggsave(ff_fig1, filename= "images/connectivity_is_not_brte_rank_double.png",
       width=11, height=9, bg="white")



ggarrange(pm_fig, ff_fig1, nrow=1, labels = c("(a)", "(b)")) %>%
  ggsave(filename = "images/concept_2pan.png", 
         height = 9, width=22, bg="white")


# seed count visualizations

pp11 <- read.csv("/home/a/projects/seed_bank/data/counts.csv") %>%
  filter(Total>1) %>%
  pivot_longer(cols = names(.)[4:18], names_to = "plot", values_to = 'count') %>%
  filter(plot != "Total", species != "brte", species != "pose") %>%
  ggplot(aes(x=count, y=species, fill=plot)) +
  geom_bar(stat="sum", color="black", lwd = 0.1) +
  ggtitle("Non-Graminoids (n>1)") +
  theme_classic() +
  ylab("Species") +
  xlab("Seed Count") 

pp22<-read.csv("/home/a/projects/seed_bank/data/counts.csv") %>%
  pivot_longer(cols = names(.)[4:18], names_to = "plot", values_to = 'count') %>%
  filter(plot != "Total", species %in% c("brte", "pose")) %>%
  ggplot(aes(x=count, y=species, fill=plot)) +
  geom_bar(stat="sum", color="black", lwd = 0.1, position = "dodge")+
  scale_x_log10() +
  theme_classic() +
  ggtitle("Graminoids") +
  ylab("Species") +
  xlab("Seed Count")

ggarrange(pp11, pp22, nrow=2, labels = c("(a)", "(b)")) %>%
  ggsave(plot=., filename = "images/seed_counts.png", bg="white",
         height=8, width=6)
