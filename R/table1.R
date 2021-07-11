# table of model results

source("R/rs_analysis.R")
source("R/modelling.R")

library(performance)

broom::tidy(mod_aim)
broom::tidy(predmod)

car::Anova(mod_aim)
car::Anova(predmod)


performance::compare_performance(mod_aim, predmod, rdnbr_mod,mod_pf_ff,mod_divsh_ff) %>%
  as_tibble() %>%
  mutate(Formula = c("TVC ~ NDSVI + Green NDVI",
                     "dNBR ~ TVC(modelled)",
                     "dNBR ~ TVC(in situ)",
                     "Post-Fire Fuel Connectivity ~ # Cheatgrass Seeds + covariates",
                     "Post-Fire Diversity ~ Post-Fire Fuel Connectivity"),
         Sign = c("+", "+", "+", "+", "-")) %>%
  dplyr::select(Formula, R2, R2_adjusted, Sign)
