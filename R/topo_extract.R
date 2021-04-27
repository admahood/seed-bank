# topo
source("R/a_data_prep.R")
srtm_path <- "/home/a/data/elevation"
files <- list.files(srtm_path, full.names = TRUE)
library(raster)
get_folded_aspect <- function(aspect) {
  abs(180 - abs(aspect - 225))
}

for(i in files) {
  unzip(i, exdir=srtm_path)
}

hgt_files <- list.files(srtm_path, full.names = TRUE, pattern = ".hgt$")

hgts <- sapply(hgt_files, FUN = raster)


elevation<-raster::mosaic(hgts$`/home/a/data/elevation/N40W117.hgt`, 
               hgts$`/home/a/data/elevation/N40W118.hgt`,
               hgts$`/home/a/data/elevation/N41W117.hgt`,
               hgts$`/home/a/data/elevation/N41W118.hgt`,
               fun=mean)


aspect <- terrain(elevation, opt="aspect", unit="degrees") %>%
  get_folded_aspect()

topo <- st_read("data/burned.gpkg") %>%
  mutate(f_aspect = raster::extract(aspect, y=.),
         elevation = raster::extract(elevation, y=.)) %>%
  mutate(plot = str_sub(Name, 2,3)) %>%
  dplyr::select(f_aspect, elevation, plot) %>%
  st_set_geometry(NULL)

save(topo, file = "data/topo.Rda")

