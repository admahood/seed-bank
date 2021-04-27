# real modelling script
# setup =========================
# source("R/topo_extract.R")
# source("R/rdnbr_calculation.R")
source("R/a_data_prep.R")
load("data/topo.Rda")
load("data/dnbr_mtbs.Rda")
library(car);library(lmtest); library(MASS); library(boot)


# path 1: fuel continuity predicting rdnbr =====================================
d <-  left_join(fuel_continuity %>% dplyr::filter(state == "unburned"),
               rdnbr %>% dplyr::select(plot, rdnbr_p, rdnbr_b30, rdnbr_b60, 
                                       rdnbr_b90, rdnbr_b120), by="plot") %>%
  left_join(topo, by = "plot") 


rdnbr_mod_2 <- d %>%
  lm(rdnbr_b120~ff_continuity*f_aspect*elevation, data=.)

rdnbr_mod_1<- d %>%
  lm(rdnbr_b120~ff_continuity+elevation, data=.)
rdnbr_mod <- d %>%
  lm(rdnbr_b120~ff_continuity, data=.) ;summary(rdnbr_mod)

rdnbr_mod_3 <- d %>%
  lm(rdnbr_b120~ff_continuity+f_aspect*elevation, data=.)
rdnbr_mod_4 <- d %>%
  lm(rdnbr_b120~ff_continuity+f_aspect+elevation, data=.)
rdnbr_mod_5 <- d %>%
  lm(rdnbr_b120~ff_continuity+f_aspect+elevation, data=.)
AIC(rdnbr_mod, rdnbr_mod_1, rdnbr_mod_2,rdnbr_mod_3,rdnbr_mod_4,rdnbr_mod_5)

# lrtest(rdnbr_mod_f,rdnbr_mod )
# drop1(rdnbr_mod)
# car::vif(rdnbr_mod)
# plot(rdnbr_mod)
car::Anova(rdnbr_mod)
summary(rdnbr_mod)

# path 2: rdnbr affecting seed counts ==========================================

db <- left_join(sb_counts_brte_d %>% filter(burned == "b" & depth=="top2"), 
                rdnbr %>% dplyr::select(rdnbr_b120, plot), by="plot") %>%
  left_join(x=. ,y= fuel_continuity %>% filter(state == "unburned"), by="plot") %>%
  left_join(topo, by="plot") 
dp <- left_join(sb_counts_pose_d %>% filter(burned == "b" & depth == "top2"), 
                rdnbr %>% dplyr::select(rdnbr_b120, plot), by="plot") %>%
  left_join(x=. ,y= fuel_continuity %>% filter(year==2016)%>%filter(burned=="u") ) %>%
  left_join(topo)
do <- left_join(sb_counts_other_d %>% filter(burned == "b" & depth == "top2"), 
                rdnbr %>% dplyr::select(rdnbr_b120, plot), by="plot") %>%
  left_join(fuel_continuity %>% filter(year==2016) %>%filter(burned=="u"),
            by="plot") %>%
  left_join(topo)
ds <- left_join(sb_counts_sage_d %>% 
                  filter(burned == "b" & depth == "top2"), 
                rdnbr %>% dplyr::select(rdnbr_b120, plot), by="plot") %>%
  left_join(fuel_continuity %>% filter(year==2016) %>%filter(burned=="u")) %>%
  left_join(topo) %>%
  mutate(presence = ifelse(count>0, 1, 0))
# summary(db)
  
mod_brte_counts <- glm.nb(count ~ rdnbr_b120*elevation*f_aspect,data=db, 
                       maxit=100000)
# Anova(mod_brte_counts)
# summary(mod_brte_counts)

mod_pose_counts <- glm.nb(count ~(rdnbr_b120+elevation)*f_aspect,data=dp, maxit=10000)
Anova(mod_pose_counts)
vif(mod_pose_counts)
summary(mod_pose_counts)

mod_other_counts <- glm.nb(count ~ rdnbr_b120+elevation+f_aspect,
                           data=do, maxit=10000)
vif(mod_other_counts)
Anova(mod_other_counts)
summary(mod_other_counts)

mod_sage_counts <- glm(presence ~rdnbr_b120*elevation + rdnbr_b120*f_aspect+ elevation*f_aspect,
                       data=ds, maxit=100000, 
                       family = "binomial")
Anova(mod_sage_counts)
summary(mod_sage_counts)

# path 2: affects on seed bank diversity and richness ==========================
d2a <- fc_wide %>%
  left_join(topo) %>%
  left_join(rdnbr %>% dplyr::select(plot, rdnbr_b120)) %>%
  left_join(shannon_wide) %>%
  left_join(richness_wide)

shan_modt <- lm(sh_sb_b_top2 ~ elevation+ff_unburned*rdnbr_b120, data = d2a)
shan_modb <- lm(sh_sb_b_bottom4 ~ elevation+ff_unburned*rdnbr_b120, data = d2a)

# shan_mod_pf <- lm(sh_sb_u_bottom4 ~ elevation+ff_unburned*rdnbr_b120, data = d2a)
# summary(shan_mod_pf)

# drop1(shan_mod)
# Anova(shan_modt)
# vif(shan_mod)
# summary(shan_modt)

# rich_mod <- glm.nb(r_sb_b_top2 ~ elevation+ff_unburned+rdnbr_b120, 
#                 data = d2a, maxit=10000)
# vif(rich_mod)

rich_mod_pf <- glm.nb(r_sb_u_top2 ~ elevation+ff_unburned+rdnbr_b120, 
                   data = d2a)
summary(rich_mod_pf)
# summary(rich_mod)
# Anova(rich_mod)

# path 3 ===========
seeds_per_sq_meter<- function(seeds){
  area_m2<-(pi*(.025^2))*12
  return(seeds/area_m2)
}

d3 <- fuel_continuity %>% 
  filter(state == "postfire3") %>%
  left_join(topo) %>%
  left_join(rdnbr %>% dplyr::select(plot, rdnbr_b120)) %>%
  left_join(sb_counts_brte%>% filter(burned == "b")%>% dplyr::rename(n_brte=count)) %>%
  left_join(sb_counts_pose%>% filter(burned == "b")%>% dplyr::rename(n_pose=count)) %>%
  left_join(sb_counts_sial%>% filter(burned == "b")%>% dplyr::rename(n_sial=count)) %>%
  left_join(sb_counts_other%>% filter(burned == "b")%>% dplyr::rename(n_other=count)) %>%
  left_join(shannon_wide) %>%
  left_join(richness_wide) %>%
  mutate(n_brte = seeds_per_sq_meter(n_brte),
         n_pose = seeds_per_sq_meter(n_pose))
  
mod_pf_ff <- lm(ff_continuity ~ #f_aspect+
                  elevation+
                  n_brte+
                  # n_sial+
                  # r_ag_postfire0+
                  # r_sb_u_total+
                  # r_sb_b_total+
                  n_pose+#n_other+
                  r_ag_unburned, data=d3)
save(mod_pf_ff, file = "data/h3_mod.Rda")
drop1(mod_pf_ff)
# plot(mod_pf_ff)
vif(mod_pf_ff)
summary(mod_pf_ff)
Anova(mod_pf_ff)

# path 4 ===========================
# maybe do a gam??

mod_divr_ff <- glm(r_ag_postfire3 ~ elevation+ff_continuity+f_aspect, 
                   data=d3, family=poisson)
mod_divsh_ff <- lm(sh_ag_postfire3 ~ elevation*ff_continuity+f_aspect, data=d3)
# plot(mod_divr_ff)
Anova(mod_divr_ff)
summary(mod_divr_ff)
summary(mod_divsh_ff)

