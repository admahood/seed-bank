# making rdnbr from sentinel 

# setup ========================================================================
library(raster);library(sf); library(tidyverse)

sent_path <- "data/sentinel/"
points <- "data/burned.gpkg"
source("R/a_data_prep.R")


mtbs<- TRUE
type <- "dnbr"
if(type == "dnbr"){
  mtbs_path <- "data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_dnbr.tif"
}else{
  mtbs_path <- "data/landsat/mtbs_bundle/mtbs/2016/nv4102211692120160702_20160622_20160708_rdnbr.tif"
}

if(mtbs) proj <- raster::crs(raster(mtbs_path), asText=TRUE)

hotpot<- st_read("data/hotpot_perim.gpkg") %>%
  st_transform(proj)

if(mtbs == TRUE) {
  rdnbr_n <-raster(mtbs_path)
  names(rdnbr_n) <- "layer"
  rdnbr_n[rdnbr_n<0] <- NA}

rdnbr_ndf <- rdnbr_n %>%
  mask(hotpot) %>%
  as.data.frame(xy=TRUE) %>%
  mutate(layer = ifelse(layer < 10, NA, layer)) %>%
  na.omit()

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

full_scene <- rdnbr_n %>%
  mask(hotpot)

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
