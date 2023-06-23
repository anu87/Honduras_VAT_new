
#  download population data and aggregate it to municipality level to calculate eligible populations for vaccines
# devtools::install_github('wpgp/wopr')
# library(wopr)
library(terra)
# library(wpgpDownloadR)

# 
# # Retrieve the WOPR data catalogue
# catalogue <- getCatalogue()
# 
# 
# # # Select files from the catalogue by subsetting the data frame
# selection <- subset(catalogue,
#                     country == 'HND' &
#                       category == 'Population' &
#                       filetype == 'agesex'&
#                       version == 'v1.1')
# # 
# # # downloadData(selection)
# # 
# df.peds <- wopr::getPop(feature = admin2_bounds[1,],
#                   country == 'HND',
#                   version = 'v1.1',
#                   agesex_select = c('f0', 'f1', 'f5', 'f10', 'f15',
#                                     'm0', 'm1', 'm5', 'm10', 'm15')
#                   )


# read and combine pop raster files
# manually downloaded data from WorldPop
rasters.hon <- list.files(path = "data/hnd_population_v1_1_agesex", pattern = ".tif", full.names = TRUE)

raster.peds <- c("data/hnd_population_v1_1_agesex/hnd_f_0_2020.tif", 
                 "data/hnd_population_v1_1_agesex/hnd_f_1_2020.tif",
                 "data/hnd_population_v1_1_agesex/hnd_f_5_2020.tif", 
                 "data/hnd_population_v1_1_agesex/hnd_m_0_2020.tif", 
                 "data/hnd_population_v1_1_agesex/hnd_m_1_2020.tif", 
                 "data/hnd_population_v1_1_agesex/hnd_m_5_2020.tif")


raster.adults <- rasters.hon[!(rasters.hon %in% raster.peds)]

# read all rasters from the lists
merged_adults <- rast(raster.adults)
merged_peds <- rast(raster.peds)

# sum all population across all the age bands
# apps fn is like apply fn

sum_merged_adults <- terra::app(merged_adults, sum)

sum_merged_peds <- terra::app(merged_peds, sum)


# calculate pop data by adm2 shape file -----------------------------------
# 
# peds.raster <- readRDS("data/peds_pop_raster.rds")
# adults.raster <- readRDS("data/adults_pop_raster.rds")



#admin2_bounds <- st_read("data/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm2_sinit_20161005.shp")

# adults_admin2 <- terra::extract(sum_merged_adults, admin2_bounds, fun=sum, na.rm=T)
admin2_terra <- terra::vect("data/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm2_sinit_20161005.shp")

#Get all vaues --> mean causes issues when run in the extract function so we'll apply function separately
# adm2_ped_vals2 <- terra::extract(sum_merged_peds, admin2_terra, fun='sum',na.rm=TRUE, exact = TRUE)
adm2_ped_vals <- terra::extract(sum_merged_peds, admin2_terra, fun='sum',na.rm=TRUE)

admin2_bounds2 <- st_read("data/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm2_sinit_20161005.shp")

admin2_bounds2$ped_pop <- adm2_ped_vals %>%
  mutate(sum = round(sum, 0)) |> 
  dplyr::select(-ID) %>% 
  unlist() %>%
  as.numeric()

# sum(values(sum_merged_peds), na.rm =T)
sum(admin2_bounds2$ped_pop)
#Pretty close to matching --> 3859584 vs 3838470 --> good enough

adm2_adult_vals <- terra::extract(sum_merged_adults, admin2_terra, fun='sum', na.rm=TRUE)

admin2_bounds2$adult_pop <- adm2_adult_vals %>%
  mutate(sum = round(sum, 0)) |>
  dplyr::select(-ID) %>% 
  unlist() %>%
  as.numeric()

sum(admin2_bounds2$adult_pop)

# update adm2 names to match vax data
admin2_bounds2$ADM2_ES[170] <- "San Juan Intibuca"
admin2_bounds2$ADM2_ES[238] <- "San Juan La Paz"

saveRDS(admin2_bounds2, "data/Adm2_w_Pops.rds")
