
# start-up script

pkgs <- c("sf", "dplyr", "zoo", "stringr", "geosphere", "leaflet", "RColorBrewer", "readxl", "DT", 
          "tidyverse", "clock", "raster", "terra", "exactextractr", "stringi", "lpSolveAPI", "lpSolve")

lapply(pkgs, library, character.only = TRUE)

# read adm2 shapefile with population data from WorldPop_data.R
admin2_shp_pop <- readRDS('data/Adm2_w_Pops.rds')

# read cleaned site coords data

site_coords_sf <- readRDS("data/Sites_w_Coords.rds")


# site level historical vaccination data

site_vax <- readRDS("data/Site_Hist.rds")

Municipio_Hist <- readRDS("data/Municipio_Hist.rds")

Mun_shp_vax <- readRDS("data/joined_adm2.rds")

vax_network_codes <- readRDS('data/site_mun_dep_codes.rds')

site_hist <- readRDS("data/Site_Hist.rds")

mun.doses <- readRDS("data/mun_doses_needed.rds")

mun_avg_vax_rate <- readRDS('data/mun_avg_vax_rate.rds')

salmi_inventory2 <- readRDS('data/salmi_inventory2.rds')

