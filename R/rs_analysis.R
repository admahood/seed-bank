# rs analysis for  figure 2a
library(raster)
library(tidyverse)
library(sf)
library(effects)
library(car)
set.seed(1312)

# funcitons --------------
get_satvi <- function(band6, band7, band4,L=0.5){
 leftside<- ((band6-band4) / (band6+band4+L))*(1+L)
 return(leftside-(band7/2))
}

get_savi <- function(band5, band4,L=0.5){
  return((band5-band4) / (band5+band4+L))*(1+L)

}

get_ndsvi <- function(band4, band6) {
  x = (band6 - band4) / (band6 + band4)
  return(x)
}
get_ndti <- function(band6, band7) {
  x = (band6 - band7)/(band6 + band7)
  return(x)
}

get_green_ndvi <- function(band5, band3) {
  x = (band5 - band3)/(band5 + band3)
  return(x)
}
get_ndvi8 <- function(band4, band5){
  return((band5 - band4)/ (band5 + band4))}
get_evi8 <- function(band2,band4,band5){
  return(2.5 * ((band5 - band4)/(band5 + (6 * band4) - (7.5 * band2) + 1)))
}


l8 <- stack("data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_l8_refl.tif")

perim<-st_read("data/hotpot_perim.gpkg") %>%
  st_transform(st_crs(l8, astext=T))

aim<-st_read("data/blm_aim/gbd_plots_w_precip_5_20_19_points.gpkg")

mtbsmask <- st_read("data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_mask.shp")

classes <- raster("data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_dnbr6.tif")
names(classes) <- "class"
rdnbr<- raster("data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_rdnbr.tif") %>%
  mask(perim) %>%
  mask(mtbsmask, inverse=T)
names(rdnbr)<-"rdnbr"

dnbr<- raster("data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_dnbr.tif")
names(dnbr) <- "dnbr"
# satvi <- get_satvi(band6 = l8$Layer_6,
#                    band7 = l8$Layer_7,
#                    band4 = l8$Layer_4)
# names(satvi)<- "satvi"
savi <- get_savi(band5 = l8$Layer_5,
                   band4 = l8$Layer_4)
names(savi) <- "savi"
ndsvi<- get_ndsvi(band4 = l8$Layer_4,
                  band6 = l8$Layer_6)
names(ndsvi) <- "ndsvi"
ndti <- get_ndti(band6 = l8$Layer_6,
                 band7 = l8$Layer_7)
names(ndti) <- "ndti"
green_ndvi <- get_green_ndvi(band5 = l8$Layer_5,
                             band3 = l8$Layer_3)
names(green_ndvi) <- "green_ndvi"
ndvi <- get_ndvi8(band4 = l8$Layer_4,
                  band5 = l8$Layer_5)
names(ndvi) <- "ndvi"
evi <- get_evi8(band2 = l8$Layer_2,
                band4 = l8$Layer_4,
                band5 = l8$Layer_5)
names(evi) <- "evi"

aim<- aim %>%
  mutate(ndsvi = get_ndsvi(band4 = sr_band3, band6 = sr_band5),
         green_ndvi = get_green_ndvi(band5 = sr_band4, band3 = sr_band2)) 


aim_long<- aim %>%
  st_set_geometry(NULL) %>%
  dplyr::select(TotalFolia, green_ndvi, ndsvi, ndvi) %>%
  pivot_longer(cols = names(.)[2:4], names_to="variable", values_to = "value")

stk <- stack(rdnbr,dnbr, classes, ndvi,savi, ndsvi, ndti, green_ndvi, evi) %>%
  raster::sampleRegular(size=5000) %>%
  as.data.frame() %>%
  na.omit() %>%
  filter(class>1 & class<5, !is.infinite(evi))

stk_long<- stk%>%
  pivot_longer(cols = names(.)[4:ncol(.)], 
               names_to = "variable", values_to = "value")

# d <- stk[sample(nrow(stk), 5000),] 
# 
# for(i in 2:ncol(d)){
#   if(is.numeric(as.data.frame(d)[,i])==TRUE){
#     d[,i] <- as.numeric(scale(d[,i]))
#   }
# }

# 
# d_long <- d %>%
#   pivot_longer(cols = names(.)[2:ncol(.)], 
#                names_to = "variable", values_to = "value")
# 
# lm(log(rdnbr)~poly(savi,2), d) %>% summary
# #lm(rdnbr~satvi, stk) %>% summary
# lm(rdnbr~poly(ndsvi,2), d) %>% summary
# lm(rdnbr~poly(ndti,2), d) %>% summary
# lm(log(rdnbr)~ndvi, stk)->mod0
# lm(log(rdnbr)~green_ndvi+ndsvi*ndvi, stk) ->mod1
# lm(log(rdnbr)~green_ndvi+ndvi, stk) ->mod2
# lm(log(rdnbr)~green_ndvi, stk) ->mod3
# summary(mod3)

aim %>%
  mutate(veg01 = TotalFolia/100) %>%
  betareg::betareg(veg01 ~ green_ndvi*ndsvi, data=.) -> mod_aim;summary(mod_aim)

# lm(TotalFolia ~ green_ndvi*ndsvi, aim) ->mod_aim

stk_pred <- stack(ndsvi, green_ndvi,evi) %>%
  predict(mod_aim)

stk_rdnbr <- stack(stk_pred*100, rdnbr, dnbr, classes) 

# takes a few minutes
usdm::Variogram(dnbr, lag=100, cutoff = 3000) %>% plot() # 1500 m
usdm::Variogram(stk_pred, lag=100, cutoff = 3000) %>% plot() # 2500-ish
#

df_preds<- stk_rdnbr %>%
  raster::sampleRegular(size=1000, xy=TRUE) %>%
  as.data.frame() %>%
  na.omit() %>%
  filter(class>1 & class<5)

ggplot(df_preds, aes(x=x,y=y)) +geom_point()
  
lm(dnbr~poly(layer,2),df_preds)-> predmod;predmod %>% summary
lm((dnbr)~(layer), df_preds) %>% summary
# mgcv::gam(dnbr~s(layer),data=df_preds)-> predmod;predmod %>% summary


eff<-Effect("layer", partial.residuals=T, predmod) # oops
mod_eff <- data.frame(lwr = (eff$lower), 
                       upr = (eff$upper), 
                       fit = (eff$fit), 
                       layer = eff$x$layer)

rs_h1 <- ggplot(df_preds, aes(x=layer, y=dnbr)) +
  geom_point(alpha = 0.1)+
  # geom_smooth(method = "lm",formula = "y~poly(x,2)", se=F,lwd=2, color='red') +
  geom_line(data=mod_eff, aes(y=upr), lty=2)+
  geom_line(data=mod_eff, aes(y=lwr), lty=2)+
  geom_line(data=mod_eff, aes(y=fit), lty=1, lwd=1)+
  theme_classic()+
  theme(panel.border = element_rect(fill=NA, size =1),
        axis.title.y=element_blank())+
  xlab("Modelled TVC") 

save(rs_h1, file ="data/rs_H1.Rda")

# ggplot(d_long, aes(x=value)) +
#   geom_histogram() +
#   facet_wrap(~variable, scales = "free")
# 
# ggplot(d, aes(x=(rdnbr))) +
#   geom_histogram()
# 
# ggplot(d_long, aes(x=(value), y=(rdnbr))) +
#   geom_point(alpha=0.1) +
#   facet_wrap(~variable, scales = "free")+
#   geom_smooth(method = "lm")

ggplot(stk_long %>% filter(variable %in% c("ndvi", "ndsvi", "green_ndvi")), 
       aes(x=value, y=log(rdnbr), color = as.factor(class))) +
  geom_point(alpha=0.2)+
  geom_smooth(method="lm",formula = "(y)~(x)")+
  facet_grid(variable~class, scales = "free")
  # geom_line(aes(y=predict(lm(log(rdnbr)~poly(evi), d))))


ggplot(stk_long %>% filter(variable %in% c("ndvi", "ndsvi", "green_ndvi")),
       aes(x=value, y = log(dnbr))) +
  geom_bin2d()+
  facet_grid(class~variable, scales="free") +
  scale_fill_viridis() +
  theme_classic()

ggplot(aim_long,
       aes(x=(value), y = TotalFolia)) +
  geom_point()+
  facet_wrap(~variable, scales="free") +
  scale_fill_viridis() +
  theme_classic()+
  geom_smooth()


ggplot(df_preds, aes(x=layer, y=log(rdnbr))) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm",formula = "y~poly(x,2)")
