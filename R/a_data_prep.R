# data prep for seed bank study
# to do:
# group species by functional group
# hmsc for 2019 diversity
# get pre-fire evt
# need nspp and div indexes for seed bank to predict post-fire div and nspp
# rdnbr predict seed bank indexes and nspp, also split between native/non-native

# setup ========================================================================
libs <- c("vegan","viridis", "agricolae","ggplot2", "Rmisc",
          "ggrepel","sf", "ggpubr", "gridExtra","tidyverse")

if(!"ggrepel" %in% rownames(installed.packages())){
  devtools::install_github("slowkow/ggrepel")}

iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}

lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
options(stringsAsFactors = FALSE)
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

# lookup tables

lut_origin<-c("alde" = "i",
              "artrw8" = "n",
              "brte" = "i",
              "cete" = "i",
              "hapa" = "n",
              "not_a_mint" = "n",
              "pose" = "n",
              "silvery_thin" = "n",
              "unknown_forb" = "u",
              "sial" = "i",
              "uk9" = "n",
              "copa" = "n",
              "garo" = "n",
              "misu" = "n",
              "elel" = "n",
              "lepe" = "i",
              "linear_glandular" = "n",
              "migr" = "n",
              "plantago" = "n",
              "spiny_edge_guy" = "u",
              "astragalusy_thing" = "n",
              "dral" = "n",
              "forb" = "u",
              "uk13" = "u",
              "upf1" = "u",
              "Bristly_marge" = "u",
              "decussant_aster" = "n",
              "unknown_monocot" = "u",
              "allium_sp" = "n",
              "amin"= "n") 

lut_duration_lf <-c("alde" = "af",
              "artrw8" = "sh",
              "brte" = "ag",
              "cete" = "af",
              "hapa" = "pf",
              "not_a_mint" = "pf",
              "pose" = "pg",
              "silvery_thin" = "pf",
              "unknown_forb" = "uf",
              "sial" = "af",
              "luar" = "pf",
              "artr" = "sh",
              "aggl" = "pf",
              "phlo" = "pf",
              "elci" = "pg",
              "vubr" = "ag",
              "uk9" = "af",
              "copa" = "af",
              "copr" = "af",
              "garo" = "af",
              "misu" = "af",
              "elel" = "pg",
              "gara" = "af",
              "grsp" = "sh",
              "depl" = "pf",
              "lepe" = "af",
              "cepe" = "af",
              "mim" = "af",
              "croc" = "pf",
              "defl" = "pf",
              "degl" = "pf",
              "draba" = "af",
              "chvi" = "sh",
              "brta" = "ag",
              "elec" = "pg",
              "depi" = "pf",
              "chui" = "sh",
              "aloe" = "af",
              "tesp" = "sh",
              "erna" = "sh",
              "arte" = "sh",
              "cryptanthon" = "af",
              "amw" = "pf",
              "erci" = "af",
              "satr" = "af",
              "chry" = "af",
              "asti" = "pf",
              "lagl" = "pf",
              "cadr" = "pf",
              "lase" = "af",
              "copi" = "af",
              "cabr" = 'pf',
              "brsp" = "ag",
              "ivax" = "pf",
              "seedling" = "sh",
              "poa" = "pg",
              "lete" = "af",
              "acmi" = "pf",
              "linear_glandular" = "uf",
              "migr" = "af",
              "plantago" = "af",
              "spiny_edge_guy" = "uf",
              "astragalusy_thing" = "uf",
              "dral" = "af",
              "forb" = "uf",
              "uk13" = "uf",
              "upf1" = "pf",
              "Bristly_marge" = "uf",
              "decussant_aster" = "pf",
              "unknown_monocot" = "uf",
              "allium_sp" = "pf",
              "amin"= "af") 

lut_prevalent_species <- c("brte" = "Bromus tectorum",
                           "pose" = "Poa secunda",
                           "artrw8" = "Artemisia tridentata",
                           "sial" = "Sisymbrium altissimum",
                           "uk9" = "Phlox longifolia",
                           "copa" = "Collinsia parviflora",
                           "alde" = "Alyssum desertorum",
                           "cete" = "Ceratocephala testiculata",
                           "misu" = "Mimulus suksdorfii",
                           "lepe" = "Lepidium perfoliatum")

# seedbank counts ==============================================================
seedbank_data <- read_csv("data/seed_bank_study.csv")[-c(1,2),-c(1)] %>% 
  # removing a few test columns and the timestamp colum
  # correcting typos and replacing unk IDs
  mutate(Observer = ifelse(Observer == "adam", "adam", "hakim")) %>% 
  mutate(Species = replace(Species, Species == "btre", "brte")) %>%
  mutate(Species = replace(Species, Species == "BRTE", "brte")) %>%
  mutate(Species = replace(Species, Species == "BRTE ", "brte")) %>%
  mutate(Species = replace(Species, Species == "brteq", "brte")) %>%
  mutate(Species = replace(Species, Species == "POSE", "pose")) %>%
  mutate(Species = replace(Species, Species == "linear_smooth", "garo")) %>%
  mutate(Species = replace(Species, Species == "Mimulus_sp_1", "misu")) %>%
  mutate(Species = replace(Species, Species == "uk7", "hapa")) %>%
  mutate(Species = replace(Species, Species == "uk12", "plantago")) %>%
  mutate(Species = replace(Species, Species == "uk1", "artrw8")) %>%
  mutate(Species = replace(Species, Species == "UK1", "artrw8")) %>%
  mutate(Species = replace(Species, Species == "unk1", "artrw8")) %>%
  mutate(Species = replace(Species, Species == "UK4", "alde")) %>%
  mutate(Species = replace(Species, Species == "UK6", "copa")) %>%
  mutate(Species = replace(Species, Species == "uk6", "copa")) %>%
  mutate(Species = replace(Species, Species == "uk5", "cete")) %>%
  mutate(Species = replace(Species, Species == "UK3", "sial")) %>%
  mutate(Species = replace(Species, Species == "UK2", "lepe")) %>%
  mutate(Species = replace(Species, Species == "uk2", "lepe")) %>%
  mutate(Species = replace(Species, Species == "UK5", "cete")) %>%
  mutate(Species = replace(Species, Species == "ALDE", "alde")) %>%
  mutate(Species = replace(Species, Species == "artr", "artrw8")) %>%
  dplyr::select(plot = "plot #", 
                burned = "burned/unburned",
                depth = Depth, 
                pot = "Pot #",
                species = Species, count = Count) %>%
  mutate(count = as.numeric(count),
         pot = as.factor(pot),
         plot = as.factor(plot),
         depth = ifelse(depth == "Top 2", "top2", "bottom4"),
         burned = ifelse(burned == "Burned", "b", "u"),
         burned_depth = paste0(burned, "_", depth)) %>%
  mutate(plot_pair = as.factor(paste(plot,
                                     burned,
                                     depth,
                                     sep = "_"))) %>%
  mutate(count = replace(count, is.na(count) == TRUE, 4)) # fixing row 634

# wideform counts ==============================================================

sb_wide <- seedbank_data %>%
  group_by(plot, species, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(plot, burned, depth),
              names_from = species, values_from = count,
              values_fill = list(count=0))

sb_wide_p <- seedbank_data %>%
  group_by(plot, species, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(plot, burned),
              names_from = species, values_from = count,
              values_fill = list(count=0))

# plot counts ===================================================================
# going from wide form to incorporate zeros
sb_long <- sb_wide %>%
  pivot_longer(cols = names(.)[4:ncol(.)], 
               names_to = "species", 
               values_to = "count")

sb_counts <- sb_long %>%
  group_by(plot, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_p <- sb_long %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_sp <- sb_long %>%
  group_by(plot, burned, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_no_brte <- sb_long %>%
  filter(species != "brte") %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_brte <- sb_long %>%
  filter(species == "brte") %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))
sb_counts_pose <- sb_long %>%
  filter(species == "pose") %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_sial <- sb_long %>%
  filter(species == "sial") %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_other <- sb_long %>%
  filter(species != "pose" & species != "brte") %>%
  group_by(plot, burned) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_brte_d <- sb_long %>%
  filter(species == "brte") %>%
  group_by(plot, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_pose_d <- sb_long %>%
  filter(species == "pose") %>%
  group_by(plot, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_sage_d <- sb_long %>%
  filter(species == "artrw8") %>%
  group_by(plot, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_other_d <- sb_long %>%
  filter(species != "pose" & species != "brte") %>%
  group_by(plot, burned, depth) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

sb_counts_all_d <- sb_long %>%
  filter(species == "pose" | species == "brte" | species == "sial" |
           species == "artrw8" | species == "uk9") %>%
  group_by(plot, burned, depth, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(plot = as.character(plot))

# rdnbr ========================================================================
rdnbr <- st_read("data/sb_burned.gpkg", quiet = TRUE) %>%
  mutate(plot = str_pad(as.character(plot),pad = "0",
                                               width=2, side="left")) %>%
  st_set_geometry(NULL) %>%
  dplyr::select(plot, Elevation, rdnbr_mean,rdnbr_th_mean, 
                rbr_mean, rbr_th_mean) %>%
  as_tibble()

# plot locations ===============================================================
plots_b <- st_read("data/burned.gpkg")

# line intercept ===============================================================

line_intercept <- read.csv("data/sb_veg_line_intercept.csv")
line_intercept$stop_m[762] <- 48.81
li<- line_intercept %>%
  as_tibble() %>%
  mutate(plot = tolower(plot),
         variable = tolower(variable)) %>%
  mutate(variable = replace(variable, variable == "fine fuels", "fine_fuels")) %>%
  mutate(variable = replace(variable, variable == "fine fuels ", "fine_fuels")) %>%
  mutate(distance_m = stop_m - start_m) %>%
  dplyr::select(-start_m, -stop_m) %>%
  group_by(plot, variable, year) %>%
  dplyr::summarise_all(function(x) sum(x)/50*100) %>%
  mutate(burned = substr(plot,4,4)) %>%
  dplyr::rename(cover = distance_m)%>%
  ungroup()

# fuel continuity ==============================================================
fuel_continuity <- li %>%
  group_by(year, plot, burned) %>%
  dplyr::summarise(ff_continuity = sum(cover)) %>%
  ungroup() %>%
  mutate(plot = str_sub(plot, 2,3),
          state = str_c(year, burned, sep="_")%>%
            factor(levels = c("2016_u", "2016_b", "2019_b"), 
                   labels = c("unburned", "postfire0", "postfire3")))

fc_wide<- fuel_continuity %>%
  pivot_wider(id_cols = "plot",
              names_from = "state", 
              names_prefix = "ff_",
              values_from = "ff_continuity")

# diversity ====================================================================
lut_sp <- c("acmi" = "Achillea millefolium",
            "aggl" = "Agoseris glauca",
            "alde" = "Alyssum desertorum",
            "aloe" = "Alyssum desertorum",
            "amin" = "Amsinckia menziesii",
            "amw" = "Amsinckia menziesii",
            "arte" = "Artemisia tridentata ssp. wyomingensis",
            "artr" = "Artemisia tridentata ssp. wyomingensis",
            "asti" = "Agoseris glauca",
            "bare" = "bare",
            "brsp" = "Bromus sp.",
            "brta" = "Bromus tectorum",
            "brte" = "Bromus tectorum",
            "cabr" = "Calochortus bruneaunis",
            "cadr" = "Cardaria chalepensis",
            "cepe" = "Ceratocephala testiculata",
            "cete" = "Ceratocephala testiculata",
            "chry" = "Cryptantha sp.",
            "chui" = "Chrysothamnus viscidiflorus",
            "chvi" = "Chrysothamnus viscidiflorus",
            "copa" = "Collinsia parviflora",
            "copi" = "Collinsia parviflora",
            "copr" = "Collinsia parviflora", # need to double check this
            "croc" = "Crepis occidentalis",
            "cryptanthon" = "Cryptantha sp.",
            "defl" = "Delphinium glaucum",
            "degl" = "Delphinium glaucum",
            "depi" = "Descurainia pinnata",
            "depl" = "Descurainia pinnata",
            "draba" = "Draba albertina",
            "dung" = "dung",
            "elci" = "Leymus cinereus",
            "elec" = "Elymus elymoides",
            "elel" = "Elymus elymoides",
            "erci" = "Erodium cicutarium",
            "erna" = "Ericameria nauseosa",
            "gara" = "Gayophytum ramosissimum",
            "grsp" = "Grayia spinosa",
            "ivax" = "Iva axillaris",
            "lagl" = "Layia glandulosa",
            "lase" = "Lactuca serriola",
            "lepe" = "Lepidium perfoliatum",
            "lete" = "Lepidium perfoliatum",
            "litter" = "litter",
            "little" = "litter",
            "luar" = "Lupinus argenteus",
            "migr" = "Microsteris gracilis",
            "mim" = "Mimulus suksdorfii",
            "phlo" = "Phlox longifolia",
            "plantago" = "Plantago sp.",
            "poa" = "Poa sp.",
            "pose" = "Poa secunda",
            "rock" = "rock",
            "satr" = "Salsola tragus",
            "seedling" = "Artemisia tridentata ssp. wyomingensis",
            "sial" = "Sisymbrium altissimum",
            "tesp" = "Tetradymia spinosa",
            "vubr" = "Vulpia bromoides")


div <- read_csv("data/sb_veg_diversity.csv") %>%
  mutate(species = tolower(species),
         species_full = lut_sp[species],
         plot=tolower(plot)) %>%
  filter(species != "rock", species != "bare", species != "litter", 
         species != "dung")

div_wide <- div %>%
  group_by(plot, year, species) %>%
  dplyr::summarise(cover = sum(cover, na.rm=T)/11) %>%
  pivot_wider(id_cols = c(plot, year), 
              names_from="species", 
              values_from = "cover",
              values_fill = 0) %>%
  mutate(burned = str_sub(plot, 4,4) %>% tolower(),
         plot = str_sub(plot, 1,3))%>%
  mutate(state = str_c(year, burned, sep="_")%>%
           factor(levels = c("2016_u", "2016_b", "2019_b")))


vdiv <- div_wide[,3:85] %>%
  as.data.frame()
rownames(vdiv) = str_c(div_wide$plot, "_",div_wide$state)

div_stats <- vdiv %>% vegan::diversity(index = "shannon") %>%
  as_tibble(rownames = "plot") %>%
  dplyr::rename(shannon = value) %>%
  mutate(richness = vegan::specnumber(vdiv)) %>%
  separate(plot, c("plot", "year", "burned"), "_")%>%
  mutate(plot = str_sub(plot, 2,3),
         state = str_c(year, burned, sep="_")%>%
           factor(levels = c("2016_u", "2016_b", "2019_b"), 
                  labels = c("unburned", "postfire0", "postfire3")))
# seed bank div
sbdiv<- sb_wide[,4:ncol(sb_wide)] %>%
  as.data.frame()
rownames(sbdiv) = str_c(sb_wide$plot, "_",sb_wide$burned, "_", sb_wide$depth)

sb_div_stats_d <- sbdiv %>%
  vegan::diversity(index = "shannon") %>%
  as_tibble(rownames = "plot") %>%
  dplyr::rename(shannon = value) %>%
  mutate(richness = vegan::specnumber(sbdiv)) %>%
  separate(plot, c("plot", "burned", "depth"), "_") %>%
  mutate(state = str_c("sb_", burned, "_",depth))

sbdiv_p<- sb_wide_p[,4:ncol(sb_wide_p)] %>%
  as.data.frame()
rownames(sbdiv_p) = str_c(sb_wide_p$plot, "_",sb_wide_p$burned)

sb_div_stats_p <- sbdiv_p %>%
  vegan::diversity(index = "shannon") %>%
  as_tibble(rownames = "plot") %>%
  dplyr::rename(shannon = value) %>%
  mutate(richness = vegan::specnumber(sbdiv_p)) %>%
  separate(plot, c("plot", "burned"), "_")%>%
  mutate(state = str_c("sb_", burned, "_total"))

div_stats_all <- rbind(sb_div_stats_d%>% dplyr::select(shannon, richness, state, plot), 
                       sb_div_stats_p%>% dplyr::select(shannon, richness, state, plot),
                       div_stats%>% dplyr::select(shannon, richness, state, plot)) %>%
  mutate(state= factor(state, levels = c("unburned","sb_u_top2", "sb_u_bottom4",
                                  "sb_u_total","sb_b_top2", "sb_b_bottom4",
                                  "sb_b_total","2016_b", "2019_b", "postfire0",
                                  "postfire3"),
                       labels = c("ag_unburned","sb_u_top2", "sb_u_bottom4",
                                  "sb_u_total","sb_b_top2", "sb_b_bottom4",
                                  "sb_b_total","2016_b", "2019_b", "ag_postfire0",
                                  "ag_postfire3")))
  
# ggplot(div_stats_all, aes(x=state, y=richness, fill=state))+
#   geom_boxplot()
# 
# ggplot(div_stats_all, aes(x=state, y=shannon, fill=state))+
#   geom_boxplot()

richness_wide <- div_stats_all %>%
  pivot_wider(id_cols = "plot",
              names_from = "state", 
              names_prefix = "r_",
              values_from = "richness")

shannon_wide <- div_stats_all %>%
  pivot_wider(id_cols = "plot",
              names_from = "state", 
              names_prefix = "sh_",
              values_from = "shannon")
# density ======================================================================

density_raw <- read_csv("data/sb_veg_density.csv") %>%
  separate(distance_bin, into = c("d_start", "d_end"), sep = "-", convert=TRUE) %>%
  mutate(burned = str_sub(plot, 4,4) %>% tolower(),
         plot = str_sub(plot, 1,3)%>% tolower(),
         species = tolower(species),
         species = ifelse(species == "sage", "sage", "other"))

plot_summaries <- density_raw %>%
  dplyr::group_by(plot, species, burned) %>%
  dplyr::summarise(seedlings = sum(`tally_0-20`)) %>%
  ungroup()

# ggplot(plot_summaries, aes(x=plot, y = seedlings, color=burned)) +
#   geom_point(size=3, shape=19) +
#   facet_wrap(~species,scales="free") +
#   theme_classic() +
#   scale_y_log10()
# 
# ggplot(plot_summaries %>% filter(species == "sage"), aes(x=burned, y = seedlings, fill=burned)) +
#   geom_boxplot() +
#   # facet_wrap(~species,scales="free") +
#   theme_classic() +
#   scale_y_log10() +
#   ggsave("images/seedlings_b_u.png", width=3.5, height=3.5)

# 
# burned_seedlings <- plot_summaries %>%
#   filter(burned == "b", species == "sage") %>%
#   mutate(plot = str_sub(plot, 2,3))
# 
# 
# left_join(burned_seedlings, rdnbr, by = "plot") %>%
#   ggplot(aes(x=rdnbr_mean, y=seedlings)) +
#   geom_point()
# 
# left_join(burned_seedlings %>% filter(seedlings <90), fuel_continuity %>% filter(burned == "u"),
#           by = "plot") %>%
#   ggplot(aes(x=ff_continuity, y=seedlings)) +
#   geom_jitter() +
#   geom_smooth()
# 
# 
# left_join(burned_seedlings, fuel_continuity,
#           by = "plot") %>%
#   glm(seedlings ~ ff_continuity, data =., family = "quasipoisson") %>%
#   summary()

