# conceptual figure
# multipanel figure with all three hypotheses, plus alternate hypothesis
library(lavaan)
library(tidyverse)

source("R/ggplot_sem_sb.R")


new_node_names <- c("prefire_TVC" = "Pre-fire\nconnectivity",
                    "burn_sev" = "Burn\nSeverity",
                    "Bromus_seeds_post" = "Post-fire Bromus Seeds",
                    "postfire_TVC" = "Post-fire\nconnectivity",
                    "Bromus_cv_pre" = "Pre-fire\nBromus\ncover",
                    "sb_div_pre" = "Pre-fire\nSeedbank\nDiversity")
# h1

dh1<- tibble(prefire_TVC = runif(n = 20000),
       burn_sev = prefire_TVC + rnorm(20000,sd = .2),
       Bromus_seeds_post = rnorm(20000),
       postfire_TVC = rnorm(20000))

pm_h1 <- 'burn_sev ~ prefire_TVC
          Bromus_seeds_post ~ burn_sev + prefire_TVC
          postfire_TVC ~ Bromus_seeds_post
          postfire_TVC ~ prefire_TVC
          ' %>%
  sem(data = dh1)


layout_df <-  random_layout(pm_h1) %>%
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

ph1 <- ggsem(pm_h1, 
      layout = "manual", layout_df = layout_df, alpha=0.05, 
      rename_nodes = T, new_node_names = new_node_names,
      cols =  c( "#377EB8", "grey80"),
      title = c("Hypothesis 1", "Pre-fire connectivity drives burn severity"));ph1

# h2

dh2<- tibble(prefire_TVC = runif(n = 20000),
             burn_sev = rnorm(20000,sd = .2),
             Bromus_seeds_post = burn_sev + rnorm(20000, sd = .2),
             postfire_TVC = rnorm(20000))

pm_h2 <- 'burn_sev ~ prefire_TVC
          Bromus_seeds_post ~ burn_sev + prefire_TVC
          postfire_TVC ~ Bromus_seeds_post
          postfire_TVC ~ prefire_TVC
          ' %>%
  sem(data = dh2)

ph2 <- ggsem(pm_h2, 
             layout = "manual", layout_df = layout_df, alpha=0.05, 
             rename_nodes = T, new_node_names = new_node_names,
             cols =  c( "#377EB8", "grey80"),
             title = c("Hypothesis 2", "Burn severity drives post-fire seed bank abundance of cheatgrass"));ph2
# h2a

dh2a<- tibble(prefire_TVC = runif(n = 2000),
              burn_sev =rnorm(2000),
              postfire_TVC = rnorm(2000),
              Bromus_cv_pre = rnorm(2000),
              sb_div_pre = rnorm(2000, .2) - Bromus_cv_pre - prefire_TVC,
              Bromus_seeds_post = rnorm(2000, .2) - sb_div_pre )

pm_h2a <- 'burn_sev ~ prefire_TVC
           Bromus_seeds_post ~ burn_sev + sb_div_pre + prefire_TVC
           postfire_TVC ~ Bromus_seeds_post + prefire_TVC 
           sb_div_pre ~ prefire_TVC + Bromus_cv_pre + prefire_TVC
          ' %>%
  sem(data = dh2a)

layout_df <-  random_layout(pm_h2a) %>%
  dplyr::mutate(x=replace(x, metric=="prefire_TVC", 0),
         y=replace(y, metric=="prefire_TVC", 1),
         x=replace(x, metric=="burn_sev", 1),
         y=replace(y, metric=="burn_sev", 0),
         x=replace(x, metric=="Bromus_seeds_post", 0),
         y=replace(y, metric=="Bromus_seeds_post", -1),
         x=replace(x, metric=="postfire_TVC", -1),
         y=replace(y, metric=="postfire_TVC", 0),
         
         x=replace(x, metric=="Bromus_cv_pre", 1.2),
         y=replace(y, metric=="Bromus_cv_pre", 0.75),
         x=replace(x, metric=="elv", -1.2),
         y=replace(y, metric=="elv", 1.2),
         x=replace(x, metric=="ag_div_pre", 1.2),
         y=replace(y, metric=="ag_div_pre", -1.2),
         x=replace(x, metric=="sb_div_pre", -1.2),
         y=replace(y, metric=="sb_div_pre", -.8))

ph2a <- ggsem(pm_h2a, h2bmask = TRUE,
             layout = "manual", layout_df = layout_df, alpha=0.05, 
             rename_nodes = T, new_node_names = new_node_names,
             cols =  c( "#E41A1C", "grey80"),
             title = c("Hypothesis 2a", "Invasion of annual grasses depletes seed bank diversity before fire"));ph2a
# h2b

dh2b <- tibble(Bromus_cv_pre = runif(2000),
               prefire_TVC = rnorm(2000,.2) + Bromus_cv_pre,
              burn_sev =rnorm(2000),
              
              postfire_TVC = rnorm(2000, .2),
              sb_div_pre = rnorm(2000),
              Bromus_seeds_post = rnorm(2000, .2) + prefire_TVC )

pm_h2b <- 'burn_sev ~ prefire_TVC
           Bromus_seeds_post ~ burn_sev + prefire_TVC
           prefire_TVC ~ Bromus_cv_pre
           postfire_TVC ~ Bromus_seeds_post + prefire_TVC
          ' %>%
  sem(data = dh2b)

layout_df <-  random_layout(pm_h2b) %>%
  dplyr::mutate(x=replace(x, metric=="prefire_TVC", 0),
                y=replace(y, metric=="prefire_TVC", 1),
                x=replace(x, metric=="burn_sev", 1),
                y=replace(y, metric=="burn_sev", 0),
                x=replace(x, metric=="Bromus_seeds_post", 0),
                y=replace(y, metric=="Bromus_seeds_post", -1),
                x=replace(x, metric=="postfire_TVC", -1),
                y=replace(y, metric=="postfire_TVC", 0),
                
                x=replace(x, metric=="Bromus_cv_pre", 1.2),
                y=replace(y, metric=="Bromus_cv_pre", 0.75),
                x=replace(x, metric=="elv", -1.2),
                y=replace(y, metric=="elv", 1.2),
                x=replace(x, metric=="ag_div_pre", 1.2),
                y=replace(y, metric=="ag_div_pre", -1.2),
                x=replace(x, metric=="sb_div_pre", -1.2),
                y=replace(y, metric=="sb_div_pre", -1.1))

ph2b <- ggsem(pm_h2b, 
              layout = "manual", layout_df = layout_df, alpha=0.05, 
              rename_nodes = T, new_node_names = new_node_names,
              cols =  c( "#377EB8", "grey80"),
              title = c("Hypothesis 2b", 
                        "prefire TVC is simply a reflection of cheatgrass cover"));ph2b

# h3

dh3<- tibble(prefire_TVC = runif(n = 20000),
             burn_sev = rnorm(20000),
             Bromus_seeds_post = rnorm(20000),
             postfire_TVC = rnorm(20000,.2) + Bromus_seeds_post+prefire_TVC)

pm_h3 <- 'burn_sev ~ prefire_TVC
          Bromus_seeds_post ~ burn_sev + prefire_TVC
          postfire_TVC ~ Bromus_seeds_post
          postfire_TVC ~ prefire_TVC
          ' %>%
  sem(data = dh3)

ph3 <- ggsem(pm_h3, 
             layout = "manual", layout_df = layout_df, alpha=0.05, 
             rename_nodes = T, new_node_names = new_node_names,
             cols =  c( "#377EB8", "grey80"),
             title = c("Hypothesis 3", "Bromus seed abundance and prefire TVC drive postfire TVC"));ph3


p_final<- ggarrange(ph1, ph2, ph3, ph2a, ph2b, make_legend(), nrow=2, ncol=3)
extrafont::loadfonts()
ggsave(plot = p_final, 
         filename = "images/conceptual_figure.png", 
         bg="white", height=11, width=17, dpi=600)
ggsave(plot = p_final, 
       filename = "images/conceptual_figure.jpg", 
       bg="white", height=11, width=17, dpi=600)
ggsave(plot = p_final, 
       filename = "images/conceptual_figure.png", 
       bg="white", height=11, width=17)
