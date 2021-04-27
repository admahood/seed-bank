#severity vs continuity analysis
source("R/a_data_prep.R")
source("R/rdnbr_calculation.R")
theme_set(theme_classic())
library(MASS)
# devtools::install_github("hohenstein/remef")
library(remef)
library(effects)

# note - do full seed count too
# note - analyze diversity from cover estimates
# basic models =================================================================

mod1 <- left_join(fuel_continuity, rdnbr, by="plot") %>%
  filter(year == 2016 & burned == "u") %>%
  lm(rdnbr_p~ff_continuity, data=.)

mod2 <- left_join(sb_counts_no_brte, rdnbr, by="plot") %>%
  left_join(fuel_continuity %>% filter(year==2016), y=.) %>%
  filter(burned=="b") %>%
  glm.nb(count ~rdnbr_p,data=.)

summary(mod1);summary(mod2)

eff2<-Effect("rdnbr_p", partial.residuals=T, mod2) # oops
mod2_eff <- data.frame(lwr = exp(eff2$lower), 
                       upr = exp(eff2$upper), 
                       fit = exp(eff2$fit), 
                       rdnbr_p = eff2$x$rdnbr_p)

mod3<- left_join(fuel_continuity, rdnbr, by="plot") %>%
  left_join(sb_counts,.) %>%
  filter(year == 2019) %>%
  lm(ff_continuity~rdnbr_p+count+Elevation, data=.)

 

eff<-Effect("rdnbr_p", partial.residuals=T, mod3)
mod3_eff <- data.frame(lwr = eff$lower, upr = eff$upper, fit = eff$fit,
                       rdnbr_p = eff$x$rdnbr_p)

mod_ff <- left_join(sb_counts_sp,fuel_continuity) %>%
  filter(year == 2019,
         species == "brte") %>%
  left_join(rdnbr) %>%
  lm(ff_continuity ~ count * rdnbr_p+Elevation, data =.)

eff1<-Effect("count", partial.residuals=T, mod_ff)
modff_eff <- data.frame(lwr = eff1$lower, 
                       upr = eff1$upper, fit = eff1$fit, count = eff1$x$count)
eff3<-Effect("rdnbr_p", partial.residuals=T, mod_ff)
modff_eff3 <- data.frame(lwr = eff3$lower, 
                         upr = eff3$upper, 
                         fit = eff3$fit, 
                         rdnbr_p = eff3$x$rdnbr_p)

mod_5 <- div_wide %>% 
  dplyr::select(plot, brte) %>% 
  filter(year == 2019) %>%
  mutate(plot = str_sub(plot, 2,3)) %>%
  left_join(x=., y=rdnbr, by = "plot") %>%
    lm(brte~ rdnbr_p, data =.)

# plots ========================================================================

p1 <- left_join(fuel_continuity, rdnbr, by="plot") %>%
  filter(year == 2016 & burned == "u") %>%
  cbind(predict(mod1, interval = "confidence", level=0.95)) %>%
  ggplot(aes(x=ff_continuity, y=rdnbr_p)) +
  geom_point() +
  xlab("Pre-fire Fuel Continuity") +
  ylab("Burn Severity (RdNBR)") +
  geom_line(aes(y=fit))+
  geom_line(aes(y=upr),lty=2)+
  geom_line(aes(y=lwr),lty=2)

p2 <- left_join(sb_counts_no_brte, rdnbr, by="plot") %>%
  left_join(fuel_continuity %>% filter(year==2016), y=.) %>%
  filter(burned=="b") %>%
  ggplot(aes(y=log(count), x=(rdnbr_p))) +
  geom_point() +
  ylab("Total Seeds Germinated in Greenhouse (Cheatgrass Excluded)") +
  xlab("Burn Severity (RdNBR)")+
  geom_smooth(method="lm")

p2a <- left_join(sb_counts_brte, rdnbr, by="plot") %>%
  filter(burned=="b") %>%
  ggplot(aes(y=count, x=rdnbr_p)) +
  geom_point() +
  ylab("Total Cheatgrass Seeds Germinated in Greenhouse") +
  xlab("Burn Severity (RdNBR)")+
  geom_smooth(method="glm",  method.args = list(family = "quasipoisson"))



p3 <- left_join(fuel_continuity, rdnbr, by="plot") %>%
  filter(year == 2019) %>%
  ggplot(aes(y=ff_continuity, x=rdnbr_p)) +
  geom_point() +
  geom_line(data=mod3_eff,aes(y=fit))+
  geom_line(data=mod3_eff,aes(y=upr),lty=2)+
  geom_line(data=mod3_eff,aes(y=lwr),lty=2)+
  xlab("Burn Severity (RdNBR)") +
  ylab("Fuel Continuity, 3 Years Post-fire")

p4 <- left_join(sb_counts_sp,fuel_continuity) %>%
  filter(year == 2019,
         species == "brte") %>%
  left_join(rdnbr)%>%
  ggplot(aes(y=ff_continuity, x=count)) +
  geom_point() +
  geom_line(data=modff_eff,aes(y=fit))+
  geom_line(data=modff_eff,aes(y=upr),lty=2)+
  geom_line(data=modff_eff,aes(y=lwr),lty=2)+
  xlab("Greenhouse-germinated Cheatgrass Seeds") +
  ylab("Fuel Continuity, 3 Years Post-fire")

p5 <- div_wide %>% 
  dplyr::select(plot, brte, artr) %>% 
  filter(year == 2019) %>%
  mutate(plot = str_sub(plot, 2,3)) %>%
  left_join(x=., y=rdnbr, by = "plot") %>%
  ggplot(aes(x=rdnbr_p, y=brte)) +
  geom_point() +
  ylab("Cheatgrass, Sagebrush Cover (%), 3 Years Post-fire") +
  xlab("Burn Severity (RdNBR)") +
  geom_point(aes(y=artr), shape = 3, stroke =2, color = "darkgreen")+
  geom_smooth(method="lm", color = "black")+
  geom_text(x=1700, y=2, label="Sagebrush",color = "darkgreen",size=5)+
  geom_text(x=1000, y=13, label="Cheatgrass", color = "black", size=5);p5

p6 <- div_wide %>% 
  dplyr::select(plot, artr) %>% 
  filter(year == 2019) %>%
  mutate(plot = str_sub(plot, 2,3),
         artr=ifelse(artr >0, 1,0)) %>%
  left_join(x=., y=rdnbr, by = "plot") %>%
  ggplot(aes(x=rdnbr_p, y=artr)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  xlab("Burn Severity (RdNBR)") +
  ylab("Sagebrush Presence, 3 Years Post-fire")

ggarrange(p1,p2, p2a, p3,p4, nrow = 2, ncol=3) +
  ggsave("images/multipanel_continuity.png", height=5, width=10)

# some kind of relationship between cheatgrass and 2019 ff or just cite nafus& davies

left_join(sb_counts_sp,rdnbr) %>%
  group_by(species)%>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n>15, burned=="b")%>%
  ggplot(aes(y=count, x=rdnbr_p)) +
  geom_point() +
  facet_wrap(~species,scales = "free")+
  geom_smooth(method="glm",  method.args = list(family = "poisson")) +
  ggtitle("2016, seedlings remaining immediately post-fire")

left_join(sb_counts_sp,fuel_continuity) %>%
  filter(year == 2019) %>%
  group_by(species)%>%
  mutate(n=n()) %>%
  ungroup() %>%
  filter(n>8,burned=="b")%>%
  ggplot(aes(y=count, x=ff_continuity)) +
  geom_point() +
  facet_wrap(~species,scales = "free")+
  geom_smooth(method="lm") +
  ggtitle("2016, seedlings remaining immediately post-fire")

#
d_ff <- left_join(sb_counts_sp,
                  fuel_continuity %>%
                    filter(burned == "u")) %>%
  filter(year == 2016,
         species == "brte") %>%
  left_join(rdnbr)
  
mod_counts<-MASS::glm.nb(count ~ ff_continuity + Elevation* rdnbr_p, 
                         data =d_ff); summary(mod_counts)
mod_countsq<-glm(count ~ ff_continuity + Elevation* rdnbr_p, 
                 data =d_ff, family = "quasipoisson"); summary(mod_countsq)
mod_countsp<-glm(count ~ ff_continuity + Elevation* rdnbr_p, 
                 data =d_ff, family = "poisson"); summary(mod_countsp)

# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/

# nb more appropriate if it's significant
pchisq(2 * (logLik(mod_counts) - logLik(mod_countsp)), df = 1, lower.tail = FALSE)



anova(mod_counts)
anova(mod_ff,test="LR")
summary(mod_ff)
summary(mod_counts)

# pre-fire condition on brte seeds =============================================
d_ff <-  left_join(sb_counts_sp%>%
                     filter(burned == "b") %>%
                     dplyr::select(-burned),
                   fuel_continuity %>%
                     filter(burned == "u") %>%
                     dplyr::select(-burned)) %>%
  filter(year == 2016,
         species == "brte") %>%
  left_join(rdnbr)

mod_counts<-MASS::glm.nb(count ~ ff_continuity + Elevation*rdnbr_p, 
                         data =d_ff); summary(mod_counts)

effc<-Effect("rdnbr_p", partial.residuals=T, mod_counts)
mod_effc <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       rdnbr_p = effc$x$rdnbr_p)
effc<-Effect("Elevation", partial.residuals=T, mod_counts)
mod_effe <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       Elevation = effc$x$Elevation)
effc<-Effect("ff_continuity", partial.residuals=T, mod_counts)
mod_efff <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       ff_continuity = effc$x$ff_continuity)
pb1<-ggplot(d_ff, aes(y=count, x=rdnbr_p)) +
  geom_line(data=mod_effc,aes(y=fit))+
  geom_line(data=mod_effc,aes(y=upr),lty=2)+
  geom_line(data=mod_effc,aes(y=lwr),lty=2) +
  ylab("Post-fire Cheatgrass Seeds") +
  xlab("Burn Severity (RdNBR)")
pb2<-ggplot(d_ff, aes(y=count, x=Elevation)) +
  geom_line(data=mod_effe,aes(y=fit))+
  geom_line(data=mod_effe,aes(y=upr),lty=2)+
  geom_line(data=mod_effe,aes(y=lwr),lty=2) +
  ylab("Post-fire Cheatgrass Seeds") +
  xlab("Elevation")
pb3<-ggplot(d_ff, aes(y=count, x=ff_continuity)) +
  geom_line(data=mod_efff,aes(y=fit))+
  geom_line(data=mod_efff,aes(y=upr),lty=2)+
  geom_line(data=mod_efff,aes(y=lwr),lty=2) +
  ylab("Post-fire Cheatgrass Seeds") +
  xlab("Pre-fire Fuel Continuity (Percent Cover)")

# prefire conditions on pose seeds =============================================
d_ffp <-  left_join(sb_counts_sp%>%
                      filter(burned == "b") %>%
                      dplyr::select(-burned),
                    fuel_continuity %>%
                      filter(burned == "u") %>%
                      dplyr::select(-burned)) %>%
  filter(year == 2016,
         species == "pose") %>%
  left_join(rdnbr)

mod_countsp<-MASS::glm.nb(count ~ ff_continuity *Elevation* rdnbr_p, 
                          data =d_ffp); summary(mod_countsp)

effc<-Effect("rdnbr_p", partial.residuals=T, mod_countsp)
mod_effc <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       rdnbr_p = effc$x$rdnbr_p)
effc<-Effect("Elevation", partial.residuals=T, mod_countsp)
mod_effe <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       Elevation = effc$x$Elevation)
effc<-Effect("ff_continuity", partial.residuals=T, mod_countsp)
mod_efff <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       ff_continuity = effc$x$ff_continuity)
pp1<-ggplot(d_ffp, aes(y=count, x=rdnbr_p)) +
  geom_line(data=mod_effc,aes(y=fit))+
  geom_line(data=mod_effc,aes(y=upr),lty=2)+
  geom_line(data=mod_effc,aes(y=lwr),lty=2) +
  ylab("Post-fire Poa Seeds") +
  xlab("Burn Severity (RdNBR)")
pp2<-ggplot(d_ffp, aes(y=count, x=Elevation)) +
  geom_line(data=mod_effe,aes(y=fit))+
  geom_line(data=mod_effe,aes(y=upr),lty=2)+
  geom_line(data=mod_effe,aes(y=lwr),lty=2) +
  ylab("Post-fire Poa Seeds") +
  xlab("Elevation")
pp3<-ggplot(d_ffp, aes(y=count, x=ff_continuity)) +
  geom_line(data=mod_efff,aes(y=fit))+
  geom_line(data=mod_efff,aes(y=upr),lty=2)+
  geom_line(data=mod_efff,aes(y=lwr),lty=2) +
  ylab("Post-fire Poa Seeds") +
  xlab("Pre-fire Fuel Continuity (Percent Cover)")

# everything but brte & pose    ================================================

d_ffe <- left_join(sb_counts_sp%>%
                     filter(burned == "b") %>%
                     dplyr::select(-burned),
                   fuel_continuity %>%
                     filter(burned == "u") %>%
                     dplyr::select(-burned)) %>%
  filter(year == 2016,
         species != "brte"&& species != "pose") %>%
  left_join(rdnbr) %>%
  group_by(plot) %>%
  summarise(count=sum(count),
            Elevation = first(Elevation),
            rdnbr_p = first(rdnbr_p),
            ff_continuity = first(ff_continuity)) %>%
  ungroup

mod_countse<-MASS::glm.nb(count ~ ff_continuity * Elevation * rdnbr_p, 
                          data =d_ffe); summary(mod_countse)

effc<-Effect("rdnbr_p", partial.residuals=T, mod_countse)
mod_effc <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       rdnbr_p = effc$x$rdnbr_p)
effc<-Effect("Elevation", partial.residuals=T, mod_countse)
mod_effe <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       Elevation = effc$x$Elevation)
effc<-Effect("ff_continuity", partial.residuals=T, mod_countse)
mod_efff <- data.frame(lwr = exp(effc$lower), 
                       upr = exp(effc$upper), 
                       fit = exp(effc$fit), 
                       ff_continuity = effc$x$ff_continuity)
pe1<-ggplot(d_ffe, aes(y=count, x=rdnbr_p)) +
  geom_line(data=mod_effc,aes(y=fit))+
  geom_line(data=mod_effc,aes(y=upr),lty=2)+
  geom_line(data=mod_effc,aes(y=lwr),lty=2) +
  ylab("Post-fire non-Grass Seeds") +
  xlab("Burn Severity (RdNBR)")
pe2<-ggplot(d_ffe, aes(y=count, x=Elevation)) +
  geom_line(data=mod_effe,aes(y=fit))+
  geom_line(data=mod_effe,aes(y=upr),lty=2)+
  geom_line(data=mod_effe,aes(y=lwr),lty=2) +
  ylab("Post-fire non-Grass Seeds") +
  xlab("Elevation")
pe3<-ggplot(d_ffe, aes(y=count, x=ff_continuity)) +
  geom_line(data=mod_efff,aes(y=fit))+
  geom_line(data=mod_efff,aes(y=upr),lty=2)+
  geom_line(data=mod_efff,aes(y=lwr),lty=2) +
  ylab("Post-fire non-Grass Seeds") +
  xlab("Pre-fire Fuel Continuity (Percent Cover)")

# partial effects for all three models of post-fire seeds ======================
ggarrange(pp1,pp2,pp3, pb1,pb2,pb3, pe1,pe2,pe3, nrow=3, ncol=3) +
  ggsave(filename="images/multipanel9.png", height=10, width=10)

# richness and ff continuity 
richmod <- left_join(fuel_continuity, div_stats, by=c("plot","state")) %>%
  dplyr::filter(state == "postfire3") %>%
  glm.nb(richness ~ ff_continuity, data =.)#, family = "poisson")
summary(richmod)

left_join(fuel_continuity, div_stats, by=c("plot","state")) %>%
  dplyr::filter(state == "postfire3") %>%
  ggplot(aes(x=ff_continuity, y=richness)) +
  geom_point() +
  xlab("Fuel Continuity, 2019") +
  ylab("Speciees Richness, 2019")+
  # facet_wrap(~state, scales="free") +
  geom_smooth(method = "glm.nb") +
  ggsave("images/richnes_ffc.png", width=3.5, height=3.5)


## all together
ggarrange(p1, p3,p4, nrow = 1, ncol=3) +
  ggsave("images/multipanel_continuity.png", height=5, width=12)

ggarrange(p2a, p2, p5, p6, nrow = 2, ncol=2) +
  ggsave("images/multipanel_rdnbr.png", height=8.5, width=8.5)

# depth visualization ==========================================================

left_join(sb_counts_brte_d, rdnbr, by="plot") %>%
  filter(burned=="b") %>%
  glm.nb(count~rdnbr_p*depth, data=.) %>%
  summary

dp1 <- left_join(sb_counts_brte_d, rdnbr, by="plot") %>%
  filter(burned=="b") %>%
  ggplot(aes(y=count, x=rdnbr_p, color=depth)) +
  geom_point() +
  # facet_wrap(~burned, scales = "free")+
  ylab("Total Cheatgrass Seeds Germinated in Greenhouse") +
  xlab("Burn Severity (RdNBR)")+
  geom_smooth(method="glm.nb")#,  method.args = list(family = "quasipoisson"))

dp2 <- left_join(sb_counts_pose_d, rdnbr, by="plot") %>%
  filter(burned=="b") %>%
  ggplot(aes(y=count, x=rdnbr_p, color=depth)) +
  geom_point() +
  # facet_wrap(~burned, scales = "free")+
  ylab("Total Poa Seeds Germinated in Greenhouse") +
  xlab("Burn Severity (RdNBR)")+
  geom_smooth(method="glm.nb")#,  method.args = list(family = "quasipoisson"))

dp3 <- left_join(sb_counts_other_d, rdnbr, by="plot") %>%
  # filter(burned=="b") %>%
  ggplot(aes(y=count, x=rdnbr_p, color=depth)) +
  geom_point() +
  # facet_wrap(~burned, scales = "free")+
  ylab("Total non-grass Seeds Germinated in Greenhouse") +
  xlab("Burn Severity (RdNBR)")+
  geom_smooth(method="glm.nb")

dp4 <- left_join(sb_counts_brte_d, fuel_continuity %>% 
                   filter(year == 2016, burned == "u") %>%
                   dplyr::select(-burned), by=c("plot")) %>%
  # filter(burned=="b") %>%
  ggplot(aes(y=count, x=ff_continuity, color=depth)) +
  geom_point() +
  # facet_wrap(~species, scales = "free")+
  ylab("Total Cheatgrass Seeds Germinated in Greenhouse") +
  xlab("Fuel Continuity (% Cover)")+
  geom_smooth(method="glm.nb")

dp5 <- left_join(sb_counts_pose_d, fuel_continuity %>% 
                   filter(year == 2016, burned == "u") %>%
                   dplyr::select(-burned), by=c("plot")) %>%
  # filter(burned=="b") %>%
  ggplot(aes(y=count, x=ff_continuity, color=depth)) +
  geom_point() +
  # facet_wrap(~species, scales = "free")+
  ylab("Total Poa Seeds Germinated in Greenhouse") +
  xlab("Fuel Continuity (% Cover)")+
  geom_smooth(method="glm.nb")

dp6 <- left_join(sb_counts_other_d, fuel_continuity %>% 
                   filter(year == 2016, burned == "u") %>%
                   dplyr::select(-burned), by=c("plot")) %>%
  # filter(burned=="b") %>%
  ggplot(aes(y=count, x=ff_continuity, color=depth)) +
  geom_point() +
  # facet_wrap(~species, scales = "free")+
  ylab("Total non-grass Seeds Germinated in Greenhouse") +
  xlab("Fuel Continuity (% Cover)")+
  geom_smooth(method="glm.nb")

ggarrange(dp1, dp2, dp3, nrow=1, ncol=3, common.legend = T) +
  ggsave("images/rdnbr_depth.png", height = 4, width=9.5)

ggarrange(dp4, dp5, dp6, nrow=1, ncol=3, common.legend = T) +
  ggsave("images/ff_depth.png", height = 4, width=9.5)

