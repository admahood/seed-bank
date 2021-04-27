## All together plot===================

# figures for paper
library(effects)
library(scales)
library(ggtext)

source("R/modelling.R")
closest <- function(x, x0) apply(outer(x, x0, FUN=function(x, x0) abs(x - x0)), 1, which.min)

# Hypothesis 1 =======================================================================
eff2<-Effect("ff_continuity", partial.residuals=T, rdnbr_mod) # oops
mod2_eff <- data.frame(lwr = (eff2$lower), 
                       upr = (eff2$upper), 
                       fit = (eff2$fit), 
                       ff_continuity = eff2$x$ff_continuity)

p_h1 <- ggplot(d, aes(x=ff_continuity, y=rdnbr_b20)) +
  geom_point() +
  geom_line(aes(y=predict(rdnbr_mod))) +
  geom_line(data = mod2_eff,lty=2, aes(y=lwr))+
  geom_line(data = mod2_eff,lty=2, aes(y=upr)) +
  xlab("Pre-fire Fuel Connectivity (% Cover)") +
  ylab("Burn Severity (RdNBR)") +
  theme_classic()+
  theme(panel.border = element_rect(fill=NA, size =1))

# Hypothesis 2 ==============
load("data/binomial_preds.Rda")

# Hypothesis 3 =================================================================

plot_h3 <- function(mod){
  effr<-Effect("n_brte", partial.residuals=T, mod)
  efff<-Effect("n_pose", partial.residuals=T, mod)

  res_dfb <- data.frame(res =  effr$fit[closest(effr$data$n_brte,
                                                effr$x$n_brte)] + effr$residuals, 
                             value = effr$data$n_brte,
                             variable = "*B. tectorum* Seeds (Count)")
  res_dfp <- data.frame(res =  efff$fit[closest(efff$data$n_pose,
                                                efff$x$n_pose)] + efff$residuals, 
                             value = efff$data$n_pose,
                             variable = "*P. secunda* Seeds (Count)")
  
  
  
  mod_effr <- data.frame(lwr = (effr$lower), 
                         upr = (effr$upper), 
                         fit = (effr$fit), 
                         variable = "*B. tectorum* Seeds (Count)",
                         value = effr$x$n_brte)
  mod_efff <- data.frame(lwr = (efff$lower), 
                         upr = (efff$upper), 
                         fit = (efff$fit), 
                         variable = "*P. secunda* Seeds (Count)",
                         value = efff$x$n_pose)
  p1<-mod_efff %>%
    ggplot(aes(x=value)) +
    geom_line(aes(y=fit), color = "grey") +
    geom_point(data = res_dfp,aes(y=res)) +
    geom_line(aes(y=upr), lty=2, color = "grey") +
    geom_line(aes(y=lwr), lty=2, color = "grey") +
    ylab("Post-Fire Fuel Connectivity") +
    xlab("*Poa secunda* Seeds (Count)") +
    theme_classic()+
    theme(legend.title = element_blank(),
          axis.title.x = ggtext::element_markdown(),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "transparent"),
          panel.border = element_rect(fill=NA, size =.75))
  
  p2<-mod_effr %>%
    ggplot(aes(x=value)) +
    geom_line(aes(y=fit)) +
    geom_point(data = res_dfb,aes(y=res)) +
    geom_line(aes(y=upr), lty=2) +
    geom_line(aes(y=lwr), lty=2) +
    ylab("Post-Fire Fuel Connectivity") +
    xlab("*Bromus tectorum* Seeds (Count)") +
    theme_classic()+
    theme(legend.title = element_blank(),
          strip.text = ggtext::element_markdown(),
          axis.title.x = ggtext::element_markdown(),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = "transparent"),
          panel.border = element_rect(fill=NA, size =.75))
  
  return(ggarrange(p2, p1, nrow=1))
}
plot_h3(mod_pf_ff)




ggarrange(p_h1, plot_h3(mod_pf_ff), 
          nrow=1, widths = c(1,2), labels = c("(a)", "(b)")) %>%
  ggarrange(p_preds,
            # plot4(mod_pf_ff, label = "2019 Fuel Connectivity (% Cover)",nrow=1),
            nrow=2, heights = c(1,1.5), labels = c("", "(c)")) +
  ggsave("images/big_plot_reduced.png", height = 15, width=15)
