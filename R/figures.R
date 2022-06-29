# figures for paper
library(effects)
library(scales)
library(ggtext)

source("R/modelling.R")
theme_set(theme_classic()+
            theme(panel.border = element_rect(fill=NA, size = 1)))

closest <- function(x, x0) apply(outer(x, x0, FUN=function(x, x0) abs(x - x0)), 1, which.min)

# H1 =======================================================================
eff2<-Effect("ff_continuity", partial.residuals=T, rdnbr_mod) # oops
mod2_eff <- data.frame(lwr = (eff2$lower), 
                       upr = (eff2$upper), 
                       fit = (eff2$fit), 
                       ff_continuity = eff2$x$ff_continuity)

p1 <- ggplot(d, aes(x=ff_continuity, y=rdnbr_b120)) +
  geom_point() +
  geom_line(data = mod2_eff,aes(y=fit)) +
  geom_line(data = mod2_eff,lty=2, aes(y=lwr))+
  geom_line(data = mod2_eff,lty=2, aes(y=upr)) +
  xlab("Pre-Fire *in situ* TVC") +
  ylab("Burn Severity (dNBR)") +
  theme_classic()+
  theme(panel.border = element_rect(fill=NA, size =1),
        axis.title.x = element_markdown())

# ggsave("images/fc_fig.png")

load("data/rs_H1.Rda")

rs_h1<-rs_h1 +
  xlab("Pre-Fire Modelled TVC")

# H2a effects on div ==================

plot3a <- function(mod, label){
  effr<-Effect("rdnbr_b20", partial.residuals=T, mod)
  effe<-Effect("elevation", partial.residuals=T, mod)
  # efff<-Effect("f_aspect", partial.residuals=T, mod)
  efff<-Effect("ff_unburned", partial.residuals=T, mod)
  
  
  p <- Anova(mod) %>% 
    as.data.frame() %>%
    mutate(var=rownames(.),
           colorz = ifelse(`Pr(>F)` < 0.05, "black", "grey"),
           col = ifelse(`Pr(>F)` < 0.05, "p < 0.05", "p > 0.05")) %>%
    filter(!str_detect(var, ":"))%>%
    dplyr::select(var, col, colorz)
  
  mod_effr <- data.frame(lwr = (effr$lower), 
                         upr = (effr$upper), 
                         fit = (effr$fit), 
                         variable = "RdNBR",
                         col = p %>% filter(var == "rdnbr_b20") %>% pull(col),
                         value = effr$x$rdnbr_b20)
  mod_effe <- data.frame(lwr = (effe$lower), 
                         upr = (effe$upper), 
                         fit = (effe$fit), 
                         col = p %>% filter(var == "elevation") %>% pull(col),
                         variable = "Elevation",
                         value = effe$x$elevation)
  mod_efff <- data.frame(lwr = (efff$lower), 
                         upr = (efff$upper), 
                         fit = (efff$fit), 
                         col = p %>% filter(var == "ff_unburned") %>% pull(col),
                         variable = "Pre-fire Fuel Continuity",
                         value = efff$x$ff_unburned)
  
  return(
    rbind(mod_effr,mod_effe,mod_efff) %>%
      ggplot(aes(x=value, color = col)) +
      geom_line(aes(y=fit)) +
      scale_color_manual(values = unique(p$colorz))+
      geom_line(aes(y=upr), lty=2) +
      geom_line(aes(y=lwr), lty=2) +
      facet_wrap(~variable, scales = "free", strip.position = "top") +
      xlab("") +
      # scale_y_continuous(labels = scales::label_number_si())+
      ylab(label) +
      theme(legend.title = element_blank())+
      theme_classic()+
      theme(panel.border = element_rect(fill=NA, size =.75))
  )
}



# H3 veg/div effects on continuity =========================================
plot_h3 <- function(mod, label,nrow, which = "all"){
  effr<-Effect("n_brte", partial.residuals=T, mod)
  effe<-Effect("elevation", partial.residuals=T, mod)
  efff<-Effect("n_pose", partial.residuals=T, mod)
  effg<-Effect("r_ag_unburned", partial.residuals=T, mod)
  
  p <- Anova(mod) %>% 
    as.data.frame() %>%
    mutate(var=rownames(.),
           col = ifelse(`Pr(>F)` < 0.05, " p < 0.05", "p > 0.05")) %>%
    dplyr::select(var, col)
  
  res_df <- rbind(data.frame(res =  effr$fit[closest(effr$data$n_brte, 
                                                     effr$x$n_brte)] + effr$residuals, 
                             value = effr$data$n_brte,
                             col = p %>% filter(var == "n_brte") %>% pull(col),
                             variable = "*B. tectorum* Seeds m^-2"),
                  data.frame(res =  effe$fit[closest(effe$data$elevation, 
                                                     effe$x$elevation)] + effe$residuals,  
                             variable = "Elevation",
                             col = p %>% filter(var == "elevation") %>% pull(col),
                             value = effe$data$elevation),
                  data.frame(res =  efff$fit[closest(efff$data$n_pose, 
                                                     efff$x$n_pose)] + efff$residuals, 
                             value = efff$data$n_pose,
                             col = p %>% filter(var == "n_pose") %>% pull(col),
                             variable = "*P. secunda* Seeds m^-2"),
                  data.frame(res =  effg$fit[closest(effg$data$r_ag_unburned, 
                                                     effg$x$r_ag_unburned)] + effg$residuals, 
                             value = effg$data$r_ag_unburned,
                             col = p %>% filter(var == "r_ag_unburned") %>% pull(col),
                             variable = "Pre-fire Aboveground Richness"))
  

  
  mod_effr <- data.frame(lwr = (effr$lower), 
                         upr = (effr$upper), 
                         fit = (effr$fit), 
                         variable = "*B. tectorum* Seeds m^-2",
                         col = p %>% filter(var == "n_brte") %>% pull(col),
                         value = effr$x$n_brte)
  
  if(which == "brte"){
    return(ggplot(mod_effr, aes(x = value))+
             geom_line(aes(y=fit)) +
             geom_point(data = res_df %>%
                          dplyr::filter(variable == "*B. tectorum* Seeds m^-2"),
                        aes(y=res)) +
             scale_color_manual(values = c("black", "grey"))+
             geom_line(aes(y=upr), lty=2) +
             geom_line(aes(y=lwr), lty=2) +
             xlab("*B. tectorum* Seeds m^-2") +
             ylab("Post-Fire Fuel Connectivity")+
             ylim(c(39.5,110))+
             theme_classic()+
             theme(legend.title = element_blank(),
                   axis.title.x = ggtext::element_markdown(),
                   legend.position = "none",
                   legend.justification = c(0,1),
                   legend.background = element_rect(fill = "transparent"))+
             theme(panel.border = element_rect(fill=NA, size =.75))
           )
  }
  
  mod_effe <- data.frame(lwr = (effe$lower), 
                         upr = (effe$upper), 
                         fit = (effe$fit), 
                         col = p %>% filter(var == "elevation") %>% pull(col),
                         variable = "Elevation",
                         value = effe$x$elevation)
  mod_efff <- data.frame(lwr = (efff$lower), 
                         upr = (efff$upper), 
                         fit = (efff$fit), 
                         col = p %>% filter(var == "n_pose") %>% pull(col),
                         variable = "*P. secunda* Seeds m^-2",
                         value = efff$x$n_pose)
  mod_effg <- data.frame(lwr = (effg$lower), 
                         upr = (effg$upper), 
                         fit = (effg$fit), 
                         col = p %>% filter(var == "r_ag_unburned") %>% pull(col),
                         variable = "Pre-fire Aboveground Richness",
                         value = effg$x$r_ag_unburned)
  
  bigdf <- rbind(mod_effr,mod_effe,mod_efff, mod_effg)
  pp<- list()
  counter <- 1
  for(i in unique(bigdf$variable)){
    col <- ifelse(i == "*P. secunda* Seeds m^-2", "grey", "black")
    pp[[counter]] <- bigdf %>%
      dplyr::filter(variable == i) %>%
      ggplot(aes(x=value)) +
      geom_line(aes(y=fit), color = col) +
      geom_point(data = res_df %>%
                   dplyr::filter(variable == i),aes(y=res)) +
      scale_color_manual(values = c("black", "grey"))+
      geom_line(aes(y=upr), color = col, lty=2) +
      geom_line(aes(y=lwr), color = col, lty=2) +
      xlab(i) +
      ylim(c(39.5,110))+
      theme_classic()+
      theme(legend.title = element_blank(),
            axis.title.x = ggtext::element_markdown(),
            legend.position = "none",
            legend.justification = c(0,1),
            legend.background = element_rect(fill = "transparent"))+
      theme(panel.border = element_rect(fill=NA, size =.75))
    
    if(i == "*B. tectorum* Seeds m^-2"){
     pp[[counter]] <- pp[[counter]]+
       ylab(label = "Post-Fire Fuel Connectivity")
    }else{
      pp[[counter]] <- pp[[counter]]+
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank() )
        }
    
    counter = counter+1
    }
    return(ggarrange(plotlist = pp, nrow = 1))
}



# H4 =======================================================================

plot_h4 <- function(mod, label, which = "all"){
  effe<-Effect("elevation", partial.residuals=T, mod)
  efff<-Effect("ff_continuity", partial.residuals=T, mod)
  

  res_df <- rbind(data.frame(res =  effe$fit[closest(effe$data$elevation, 
                                                     effe$x$elevation)] + effe$residuals,  
                             variable = "Elevation",
                             value = effe$data$elevation),
                  data.frame(res =  efff$fit[closest(efff$data$ff_continuity, 
                                                     efff$x$ff_continuity)] + efff$residuals, 
                             value = efff$data$ff_continuity,
                             variable = "Post-Fire Fuel Connectivity"))
  mod_effe <- data.frame(lwr = (effe$lower), 
                         upr = (effe$upper), 
                         fit = (effe$fit), 
                         variable = "Elevation",
                         value = effe$x$elevation)
  mod_efff <- data.frame(lwr = (efff$lower), 
                         upr = (efff$upper), 
                         fit = (efff$fit), 
                         variable = "Post-Fire Fuel Connectivity",
                         value = efff$x$ff_continuity)
  
  if(which == "ff"){
    return(
      ggplot(mod_efff, aes(x=value)) +
        geom_line(aes(y=fit)) +
        geom_point(data = res_df %>%
                     dplyr::filter(variable == "Post-Fire Fuel Connectivity"),aes(y=res)) +
        geom_line(aes(y=upr), lty=2) +
        geom_line(aes(y=lwr), lty=2) +
        xlab("Post-Fire Fuel Connectivity") +
        ylab("Post-Fire Diversity")+
        ylim(c(1,2)) +
        theme_classic() +
        theme(legend.title = element_blank(),
              axis.title.x = ggtext::element_markdown(),
              legend.position = "none",
              legend.justification = c(0,1),
              legend.background = element_rect(fill = "transparent"),
              panel.border = element_rect(fill=NA, size =.75))
    )
  }
  
  bigdf <- rbind(mod_efff,mod_effe)
  pp<- list()
  counter <- 1
  for(i in unique(bigdf$variable)){
    pp[[counter]] <- bigdf %>%
      dplyr::filter(variable == i) %>%
      ggplot(aes(x=value)) +
      geom_line(aes(y=fit)) +
      geom_point(data = res_df %>%
                   dplyr::filter(variable == i),aes(y=res)) +
      geom_line(aes(y=upr), lty=2) +
      geom_line(aes(y=lwr), lty=2) +
      xlab(i) +
      ylim(c(1,2))
      theme_classic()+
      theme(legend.title = element_blank(),
            axis.title.x = ggtext::element_markdown(),
            legend.position = "none",
            legend.justification = c(0,1),
            legend.background = element_rect(fill = "transparent"),
            panel.border = element_rect(fill=NA, size =.75))
    
    if(i == "Post-Fire Fuel Connectivity"){
      pp[[counter]] <- pp[[counter]]+
        ylab(label = "Post-Fire Diversity")+
        theme(panel.border = element_rect(fill=NA, size = 1))
    }else{
      pp[[counter]] <- pp[[counter]]+
        theme(axis.title.y=element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())+
        theme(panel.border = element_rect(fill=NA, size = 1))
    }
    
    counter = counter+1
  }
  return(ggarrange(plotlist = pp, nrow = 1)+
           theme(panel.border = element_rect(fill=NA, size = 1)))
}
# ggarrange(
#   plot3b(mod_divsh_ff, label = "2019 Shannon-Weaver"),
#   plot3b(mod_divr_ff, label = "2019 Species Richness"),
#   nrow =2) +
#   ggsave("images/path4.png", width = 7.5, height = 6, bg="white")

## All together plot===================
load("data/binomial_preds_plot.Rda")
load("data/path_model.Rda")

H1 <- ggarrange(p1,rs_h1 + geom_point(alpha=0.25),labels=c("(b)", ""), nrow=1)+
  theme(panel.border = element_rect(fill=NA, size = 1))

H4 <-  ggarrange(plot_h4(mod_divsh_ff, label = "Post-Fire Diversity", which = "ff"),
                 labels = c("(d)"))+
            theme(panel.border = element_rect(fill=NA, size = 1))
H14 <- ggarrange(H1, H4, nrow=1, labels = c("(a)", "(d)"))


H2<-  ggarrange(p_preds + xlab ("Burn Severity (dNBR)"), labels = c("(e)"))+
    theme(panel.border = element_rect(fill=NA, size = 1))

H3<- ggarrange(plot_h3(mod_pf_ff, 
                       label = "Post-Fire Fuel Connectivity",nrow=1, which="brte"),labels="(c)")+
  theme(panel.border = element_rect(fill=NA, size = 1))

H34 <- ggarrange(H3, H4, nrow=1)

H134 <- ggarrange(H1, H34, nrow = 2)

Hp134 <- ggarrange(pm_fig, H134, nrow=1, labels = c("(a)", ""))+
  theme(panel.border = element_rect(fill=NA, size = 1))

bp<-ggarrange(Hp134, H2, nrow=2, ncol=1)

ggsave(bp, filename = "images/big_plot_v2.pdf", height = 10, width=10, bg="white")

# diversity for supplement =======================
seeds_per_sq_meter<- function(seeds){
  area_m2<-(pi*(.025^2))*12
  return(seeds/area_m2)
}

div_stats_all %>%
  filter(str_detect(state,"ag") | str_detect(state, "total")) %>%
  mutate(location = str_sub(state, 1,2),
         location = ifelse(location == "ag","Aboveground","Seedbank"),
         state = str_replace_all(state, "total", ""),
         state = str_replace_all(state, "_b_", "Burned"),
         state = str_replace_all(state, "_u_", "Unburned"),
         state = str_replace_all(state, "_", ""),
         state = str_replace_all(state, "sb", ""),
         state = str_replace_all(state, "ag", ""),
         state = str_replace_all(state, "postfire0", "Spring Post-Fire"),
         state = str_replace_all(state, "postfire3", "3 Years Post-Fire"),
         state = str_replace_all(state, "unburned", "Unburned")) %>%
ggplot(aes(x=state, y=richness, fill = location)) +
  geom_boxplot() +
  xlab("") +
  facet_wrap(~location, scales="free_x")+
  ylab("Species Richness")+
  theme_classic() +
  theme(panel.border = element_rect(fill=NA, size=0.8),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_rect(fill=NA)) +
  ggsave("images/richness_fig.png", width = 7, height = 5.5)

# sb counts
sb_counts %>%
  mutate(Depth = ifelse(depth == "bottom4", "2-6 cm", "0-2 cm"),
         burned = ifelse(burned == "b", "Burned", "Unburned")) %>%
ggplot(aes(y=seeds_per_sq_meter(count), x=Depth, fill = burned)) +
  geom_boxplot() +
  ylab("Seeds m^-2")+
  scale_y_continuous(labels =scales::label_number_si())+
  theme_classic()+
  theme(legend.position = c(.9,.9),
        axis.title.y = element_markdown(),
        legend.justification = c(1,1),
        legend.title = element_blank())+
  ggsave("images/depth_x_burned_counts.png", width = 5.5, height=5.5)

sb_counts %>%
  group_by(burned, depth) %>%
  dplyr::summarise(mean_count = mean(count),
                   sd_count = sd(count))
