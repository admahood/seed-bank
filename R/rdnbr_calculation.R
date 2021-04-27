# making rdnbr from sentinel 

# setup ========================================================================
library(raster);library(sf); library(tidyverse)

sent_path <- "data/sentinel/"
points <- "data/burned.gpkg"
source("R/a_data_prep.R")

# functions ====================================================================
# note it's the NIR band and the SWIR band
# Landsat 7: bands 4 & 7
# Landsat 8: bands 5 & 7
# Sentinel 2: bands 8a (or 8) & 12
NBR<- function(nir, swir) {
  return(
    (nir-swir)/(nir+swir)
  )
}


dNBR <- function(prenbr, postnbr) {
  return(
    prenbr-postnbr
  )
}

RdNBR <- function(dnbr, prenbr){
  return(
    dnbr/(sqrt(abs(prenbr/1000)))
  )
}

parks_RdNBR <- function(dnbr, prenbr){
  prenbr[prenbr < 0.001] <- 0.001
  return(
    dnbr/(sqrt(abs(prenbr/1000)))
  )
}

get_rdnbr<- function(nirb,nira, swirb,swira) {
  b <- NBR(nirb, swirb)
  a <- NBR(nira, swira)
  d <- dNBR(b,a)
  return(
    RdNBR(d, b)
  )
}
get_rdnbr_p<- function(nirb,nira, swirb,swira) {
  b <- NBR(nirb, swirb)
  a <- NBR(nira, swira)
  d <- dNBR(b,a)
  return(
    parks_RdNBR(d, b)
  )
}

# landsat paths ================================================================
# before_path_swir <- "data/landsat/c1_ard/LC08_CU_005007_20160622_20181211_C01_V01_SRB7.tif"
# before_path_nir  <- "data/landsat/c1_ard/LC08_CU_005007_20160622_20181211_C01_V01_SRB5.tif"
# 
# after_path_swir <- "data/landsat/c1_ard/LC08_CU_005007_20160708_20181209_C01_V01_SRB7.tif"
# after_path_nir <- "data/landsat/c1_ard/LC08_CU_005007_20160708_20181209_C01_V01_SRB5.tif"

mtbs<- TRUE
type <- "dnbr"
if(type == "dnbr"){
  mtbs_path <- "data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_dnbr.tif"
}else{
  mtbs_path <- "data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_rdnbr.tif"
}
# grabbing the proj ========
# proj <- raster::crs(raster(after_path_nir), asText=TRUE)
if(mtbs)proj <- raster::crs(raster(mtbs_path), asText=TRUE)

hotpot<- st_read("data/hotpot_perim.gpkg") %>%
  st_transform(proj)
# 
# roi_box <- st_read(points) %>%
#   st_bbox() %>%
#   st_as_sfc() %>% 
#   st_transform(crs=proj) %>%
#   st_buffer(dist = 2475) %>%
#   as("Spatial")
# 
# roi_box <- hotpot %>%
#   st_bbox() %>%
#   st_as_sfc() %>% 
#   st_buffer(dist = 200) %>%
#   as("Spatial")
# 
# # clipping to hasten processing ===========
# after_nir <- raster(after_path_nir)  %>%
#   raster::crop(roi_box)
# after_swir <- raster(after_path_swir) %>%
#   raster::crop(roi_box) 
# 
# before_nir <- raster(before_path_nir)%>%
#   raster::crop(roi_box)
# before_swir <- raster(before_path_swir)%>%
#   raster::crop(roi_box)
# 
# # actually calculating the RdNBR ===============================================
# rdnbr_n <- get_rdnbr(before_nir, after_nir, before_swir, after_swir)
# rdnbr_p <- get_rdnbr_p(before_nir, after_nir, before_swir, after_swir)
# 
# rdnbr_n[rdnbr_n<10] <- NA
# rdnbr_p[rdnbr_p<10] <- NA

if(mtbs == TRUE) {
  rdnbr_n <-raster(mtbs_path)
  names(rdnbr_n) <- "layer"
  rdnbr_n[rdnbr_n<0] <- NA}

rdnbr_ndf <- rdnbr_n %>%
  mask(hotpot) %>%
  as.data.frame(xy=TRUE) %>%
  mutate(layer = ifelse(layer < 10, NA, layer)) %>%
  na.omit()

# rdnbr_pdf <- rdnbr_p %>%
#   mask(hotpot) %>%
#   as.data.frame(xy=TRUE) %>%
#   mutate(layer = ifelse(layer < 0, NA, layer)) %>%
#   na.omit()
# importing plots ==============================================================

plots <- st_read(points) %>%
  st_transform(crs=proj)

# making a map =================
# still need the hotpot perimeter to do it right
map <- ggplot() +
  # geom_tile(data=rbind(rdnbr_ndf, rdnbr_mdf), aes(x=x,y=y, fill=log(layer))) +
  geom_tile(data=rdnbr_ndf, aes(x=x,y=y, fill=(layer))) +
  geom_sf(data=plots, aes(color="")) +
  scale_fill_viridis_c(name="Burn Severity\n(dNBR)", option = "B") +
  scale_color_manual(values = "deepskyblue", name = "Plot\nLocations")+
  theme_classic() +
  theme(panel.border = element_rect(fill=NA, size=1),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        axis.title = element_blank(),
        legend.background = element_rect(fill=NA))+
  ggsave("images/map.png")

# map <- ggplot() +
#   # geom_tile(data=rbind(rdnbr_ndf, rdnbr_mdf), aes(x=x,y=y, fill=log(layer))) +
#   geom_tile(data=rdnbr_pdf, aes(x=x,y=y, fill=log(layer))) +
#   geom_sf(data=plots, aes(color="")) +
#   scale_fill_viridis_c(name="ln(RdNBR)", option = "B") +
#   scale_color_manual(values = "deepskyblue", name = "Plot\nLocations")+
#   theme_classic() +
#   theme(panel.border = element_rect(fill=NA, size=1),
#         legend.position = c(0,1),
#         legend.justification = c(0,1),
#         axis.title = element_blank(),
#         legend.background = element_rect(fill=NA))+
#   ggsave("images/map.png")

# extracting rdnbr values to plots =============================================
# # function no longer needed
# 
# ex_buff <- function(x,y,buff){
#   raster::extract(x, y=y, buffer=buff, na.rm=T, df=T) %>%
#     na.omit()%>%
#     as_tibble%>% 
#     group_by(ID) %>% 
#     dplyr::summarise(layer=mean(layer)) %>%
#     pull(layer)
#   
# }
# full_scene <- raster::mosaic(rdnbr_m, rdnbr_n, fun=mean)
# full_scene <- rdnbr_p
full_scene <- rdnbr_n %>%
  mask(hotpot)

# something not exactly right betwee landsat & sentinel -- multiplying by 50 
# gets it pretty close to the landsat -- see plot below
rdnbr_extracts <- plots %>%
  dplyr::select(plot = Name, elevation = Elevation) %>%
  mutate(rdnbr_p = raster::extract(full_scene, y=plots),
         rdnbr_b30 = raster::extract(full_scene, y=plots, buffer=30, fun=mean),
         rdnbr_b60 = raster::extract(full_scene, y=plots, buffer=60, fun=mean),
         rdnbr_b90 = raster::extract(full_scene, y=plots, buffer=90, fun=mean),
         rdnbr_b120 = raster::extract(full_scene, y=plots, buffer=120, fun=mean),
         plot = plot %>% as.character%>%str_sub(2,3)) 

# source("R/a_data_prep.R")
rdnbr<-  rdnbr_extracts

# save(rdnbr, file = "data/rdnbr_c1_ard.Rda")
if(type == "dnbr"){
save(rdnbr, file = "data/dnbr_mtbs.Rda")}else{
save(rdnbr, file = "data/rdnbr_mtbs.Rda")}
