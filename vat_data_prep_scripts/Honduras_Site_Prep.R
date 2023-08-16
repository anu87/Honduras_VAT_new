
# load libraries ----------------------------------------------------------


library(sf)
library(dplyr)
library(tidyr)
library(zoo)
library(stringr)
library(geosphere)
library(leaflet)
library(RColorBrewer)


# site coordinates data ---------------------------------------------------


site_coords <- readxl::read_xlsx("data/Establecimientos de salud longitud y latitud.xlsx")

#Fix names
colnames(site_coords) <- site_coords[2,]
site_coords <- site_coords[-(1:2),]

#Fix typo in data
site_coords$Latitud[1626] <- sub("\\]", "", site_coords$Latitud[1626])

#Ignore sites with missing coords for now
site_coords <- site_coords %>%
  dplyr::filter(!is.na(Longitud) & !is.na(Latitud)) %>%
  dplyr::mutate(Latitud = as.numeric(Latitud), 
                Longitud = as.numeric(Longitud))

site_coords_sf <- st_as_sf(site_coords, 
                   coords = c("Longitud", "Latitud"), 
                   crs = 4326)

saveRDS(site_coords_sf, "data/Sites_w_Coords.rds")


# warehouse data ----------------------------------------------------------


#Now do the warehouses
warehouses <- readxl::read_xlsx("data/travel time matrix between warehouses.xlsx", sheet = 1)

warehouses |> distinct(`Warehouse name`)

warehouses_coords_sf  <- st_as_sf(warehouses, 
                                  coords = c("Longitude", "Latitude"), 
                                  crs = 4326) 

saveRDS(warehouses_coords_sf, "data/Warehouses_w_Coords.rds")


# vaccination data --------------------------------------------------------

#Historic data --> 2022 clean for rhot
raw_hist <- readxl::read_xlsx("data/Total dosis aplicadas contra la COVID_2021-2022 Dep_Mun_ES.xlsx")

#Need to transform from this format to tidy data with following columns
#Department, Municipality, ES, Dose (1, 2, both), Month
#Fill in months for everything
#Pivot that longer into a column
#Pivot row 1 into doses
#Get Department + Municipality + ES for each row --> Removing 
#This dataset is terrifying so going to retool rather than try to just use reshaping

#Create separate data frames for sites, Muns, Deps

#One problem at a time --> get geographic info for everything
geog_col <- raw_hist$`Departamento/Municipio/ES`
#Remove first two rows bc they're not data
geog_col <- geog_col[-c(1:2)]
#Remove last row because it's a total --> happens in the procses I think

#All sites have numeric codes --> can flag site/not site from that
#I think all numeric codes are 4 numbers but just to be safe

#Is it numeric --> get index of vax sites
is_site <- !is.na(as.numeric(substring(geog_col, 1, 2)))
geog_type <- c() #Lev 1 --> Dep, Lev 2 --> Mun, Site --> Site
for(i in 1:(length(is_site)-1)) {
  checking_index <- i
  next_index <- i+1
  
  if(is_site[checking_index]) {
    geog_type[i] <- "Site"
  } else if(is_site[next_index]) {
    geog_type[i] <- "Level 2"
  } else {
    geog_type[i] <- "Level 1"
  }
}

#Create empty data frame with nrow = number of sites
#This df will store the geographic info for each site

geog_df <- data.frame(matrix(NA, nrow= sum(is_site), ncol = 3))
colnames(geog_df) <- c("Dep", "Mun", "Site")

tmp_lev1 <- ifelse(geog_type == "Level 1", geog_col, NA)
tmp_lev1 <- zoo::na.locf(tmp_lev1) #Fill in the missing ones with last non-missing
geog_df$Dep <- tmp_lev1[is_site]

tmp_lev2 <- ifelse(geog_type == "Level 2", geog_col, NA)
tmp_lev2 <- zoo::na.locf(tmp_lev2) #Fill in the missing ones with last non-missing
geog_df$Mun <- tmp_lev2[is_site]

geog_df$Site <- geog_col[is_site]

# use geo_df to create adm with codes

geo_codes_df <- geog_df |> 
  separate(Site, c('site_code', 'site'), sep = " - ")


#Need both the month and the dose info --> create unique colnames for each then turn that into a row and break it out
raw_months <- colnames(raw_hist)
raw_months <- ifelse(substr(raw_months, 1, 3) == "...", NA, raw_months)
raw_months <- zoo::na.locf(raw_months)

raw_age_info <- unlist(as.character(raw_hist[1,]))
raw_age_info <- zoo::na.locf(raw_age_info)
raw_dose_info <- unlist(as.character(raw_hist[2,]))
raw_dose_info <- zoo::na.locf(raw_dose_info)

#Going to need to remove the total columns from the rhot
is_totaldose_col <- ifelse(substring(as.character(raw_hist[1,]), 1, 4) == "Tota", 1, NA)

#Create combined name column
colnames(raw_hist) <- paste(raw_months, raw_age_info, raw_dose_info, sep = "_")

#Now that we've captured all that info we need to get rid of the top few columns then add back the new ones and pivot them
#Filter to only places with site level data for now
raw_site_hist <- raw_hist[!is.na(as.numeric(substring(raw_hist$`Departamento/Municipio/ES_Adulto_1ra`, 1, 2))),]

#Pivot that now
pivoted_raw_site_hist <- raw_site_hist %>%
  pivot_longer(cols = 2:ncol(raw_site_hist), 
                names_to = 'Combined_Info', 
                values_to = 'Num_Doses')

#I should do this in mutate but I just want to be done with it and that's causing problems
tmp_split <- str_split(pivoted_raw_site_hist$Combined_Info, "_")
pivoted_raw_site_hist$Mes <- unlist(lapply(tmp_split, "[", 1))
pivoted_raw_site_hist$Edad <- unlist(lapply(tmp_split, "[", 2))
pivoted_raw_site_hist$Dos <- unlist(lapply(tmp_split, "[", 3))

#Join with geog info
joined_site_hist <- left_join(pivoted_raw_site_hist, geog_df,
          by = c("Departamento/Municipio/ES_Adulto_1ra" = "Site"))

site_hist <- joined_site_hist %>%
  dplyr::rename(Sitio = `Departamento/Municipio/ES_Adulto_1ra`) %>%
  dplyr::select(Sitio, Dep, Mun, Mes, Edad, Dos, Num_Doses,)

# fix Dos nomenclature - 1R = R1; 2R = R2

site_hist <- site_hist |> 
  mutate(Dos = case_when(
    Dos == 'R1' ~ '1R',
    Dos == 'R2' ~ '2R',
    .default = Dos
  ))

# separate site code

site_hist <- site_hist |> 
  separate(Sitio, c('site_code', 'Sitio'), sep = " - ") |> 
  filter(!str_detect(site_hist$Edad, "^Total")) |> 
  filter( Num_Doses>0) |> 
  mutate(Num_Doses = as.numeric(Num_Doses))

#Export
saveRDS(site_hist, "data/Site_Hist.rds")



#Create Level 2 data output --> filter raw hist to level 2 ------
#Not numeric and doesn't start with department
nrow(raw_hist)
length(geog_type)

#First two rows and last row don't count
raw_lev2_hist <- raw_hist[(which(geog_type == "Level 2") + 2),]

#Pivot that now
pivoted_raw_lev2_hist <- raw_lev2_hist %>%
  pivot_longer(cols = 2:ncol(raw_lev2_hist), 
               names_to = 'Combined_Info', 
               values_to = 'Num_Doses')

#I should do this in mutate but I just want to be done with it and that's causing problems
tmp_lev2_split <- str_split(pivoted_raw_lev2_hist$Combined_Info, "_")
pivoted_raw_lev2_hist$Mes <- unlist(lapply(tmp_lev2_split, "[", 1))
pivoted_raw_lev2_hist$Edad <- unlist(lapply(tmp_lev2_split, "[", 2))
pivoted_raw_lev2_hist$Dos <- unlist(lapply(tmp_lev2_split, "[", 3))

# #Join with geog info
# joined_lev2_hist <- left_join(pivoted_raw_lev2_hist, geog_df,
#                               by = c("Departamento/Municipio/ES_Adulto_1ra_Adulto_1ra" = "Mun"))

lev2_hist <- pivoted_raw_lev2_hist %>%
  dplyr::rename(Mun = `Departamento/Municipio/ES_Adulto_1ra`) %>%
  dplyr::select(Mun, Mes, Edad, Dos, Num_Doses,)


# throw out rows where category (Edad) is total feb or jan (any month)
# numbers for this only exist for 2R and don't match the actual total for that month

lev2_hist <- lev2_hist |> 
  filter(!str_detect(lev2_hist$Edad, "^Total")) |> 
  filter( Num_Doses>0) |> 
  mutate(Num_Doses = as.numeric(Num_Doses))

table(lev2_hist$Dos)

lev2_hist = lev2_hist |> 
  mutate(Dos = case_when(
    Dos == 'R1' ~ '1R',
    Dos == 'R2' ~ '2R',
    .default = Dos
  ))

#Export
saveRDS(lev2_hist, "data/Municipio_Hist.rds")

#First two rows and last row don't count
raw_lev1_hist <- raw_hist[(which(geog_type == "Level 1") + 2),]

#Pivot that now
pivoted_raw_lev1_hist <- raw_lev1_hist %>%
  pivot_longer(cols = 2:ncol(raw_lev1_hist), 
               names_to = 'Combined_Info', 
               values_to = 'Num_Doses')

#I should do this in mutate but I just want to be done with it and that's causing problems
tmp_lev1_split <- str_split(pivoted_raw_lev1_hist$Combined_Info, "_")
pivoted_raw_lev1_hist$Mes <- unlist(lapply(tmp_lev1_split, "[", 1))
pivoted_raw_lev1_hist$Edad <- unlist(lapply(tmp_lev1_split, "[", 2))
pivoted_raw_lev1_hist$Dos <- unlist(lapply(tmp_lev1_split, "[", 3))

# #Join with geog info
# joined_lev2_hist <- left_join(pivoted_raw_lev2_hist, geog_df,
#                               by = c("Departamento/Municipio/ES_Adulto_1ra_Adulto_1ra" = "Mun"))

lev1_hist <- pivoted_raw_lev1_hist %>%
  dplyr::rename(Dep = `Departamento/Municipio/ES_Adulto_1ra`) %>%
  dplyr::select(Dep, Mes, Edad, Dos, Num_Doses,)

#Export
saveRDS(lev1_hist, "data/Departamento_Hist.rds")


#Join shapefiles to data ---------
historic_mun_data <- readRDS("data/Municipio_Hist.rds")
historic_dep_data <- readRDS("data/Departamento_Hist.rds")
admin2_bounds <- readRDS('data/Adm2_w_Pops.rds')

admin1_bounds <- st_read("data/hnd_adm_sinit_20161005_SHP/hnd_admbnda_adm1_sinit_20161005.shp")


#Join shapefiles to vax data --> do in preprocessing instead 
unique(historic_mun_data$Mun)[!unique(historic_mun_data$Mun) %in% admin2_bounds$ADM2_ES]

missing_muns <- unique(historic_mun_data$Mun)[!unique(historic_mun_data$Mun) %in% admin2_bounds$ADM2_ES]

# create duplicate Mun to modify
historic_mun_data$Mun2 <- historic_mun_data$Mun

#Issue with accents --> sub out
still_missing <- c()
for(i in seq_along(missing_muns)) {
  this_mun <- missing_muns[i]
  tmp_change_name <- gsub("á", "a", this_mun)
  tmp_change_name <- gsub("ó", "o", tmp_change_name)
  tmp_change_name <- gsub("í", "i", tmp_change_name)
  tmp_change_name <- gsub("é", "e", tmp_change_name)
  
  if(tmp_change_name %in%admin2_bounds$ADM2_ES) {
    historic_mun_data$Mun2[historic_mun_data$Mun2 == this_mun] <- tmp_change_name
  } else {
    print(paste("Still missing", this_mun))
    still_missing <- c(still_missing, this_mun)
  }
}

#That took care of about half --> use agrep on the rest
still_still_missing <- c()
for(i in seq_along(still_missing)) {
  this_mun <- still_missing[i]
  
  if(length(agrep(this_mun, admin2_bounds$ADM2_ES)) == 1) {
    historic_mun_data$Mun2[historic_mun_data$Mun2 == this_mun] <- admin2_bounds$ADM2_ES[agrep(this_mun, admin2_bounds$ADM2_ES)]
  } else {
    print(paste("Still missing", this_mun))
    still_still_missing <- c(still_still_missing, this_mun)
  }
 
}

historic_mun_data$Mun2[historic_mun_data$Mun2 == "Lamasica"] <- "La Masica"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "Santa Rosa"] <- "Santa Rosa de Aguan"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "Las Trojes"] <- "Trojes"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "San Juan de Guarita"] <- "San Juan Guarita"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "Nueva Ocotepeque"] <- "Ocotepeque"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "La Arada"] <- "Arada"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "San Pedro de Zacapa"] <- "San Pedro Zacapa"
historic_mun_data$Mun2[historic_mun_data$Mun2 == "Tegucigalpa M.D.C."] <- "Distrito Central"

admin2_bounds$ADM2_ES[170] <- "San Juan Intibuca"
admin2_bounds$ADM2_ES[238] <- "San Juan La Paz"
#historic_mun_data$Mun[historic_mun_data$Mun == "San Júan"] <- "" #need to figure out which one it is

# for optimization model
municipio_vax <- historic_mun_data |>
  filter(!(is.na(Mun))) |> 
  group_by(Mun, Mun2, Edad, Dos) |> 
  summarise(Num_Doses=sum(Num_Doses, na.rm = T), .groups = 'drop')

municipio_vax <- municipio_vax |> 
  pivot_wider(names_from = Dos, values_from = Num_Doses)

#Arbitrarily filter the data to remove dupes --> need to fix later
historic_mun_data <- historic_mun_data %>%
  dplyr::mutate(unique_id = paste(Mun, Mun2, Mes, Edad, Dos, sep = "_")) %>%
  dplyr::filter(!duplicated(unique_id)) %>%
  dplyr::select(-unique_id)

#Re-pivot the data
pivoted_hist_mun_data <- historic_mun_data %>%
  pivot_wider(names_from = c(Mes, Edad, Dos), 
              values_from = Num_Doses) 

joined_adm2 <- left_join(admin2_bounds, pivoted_hist_mun_data, by = c("ADM2_ES" = "Mun2"))
saveRDS(joined_adm2, "data/joined_adm2.rds")

historic_dep_data$Dep <- sub("Departamental de ", "", historic_dep_data$Dep)
unique(historic_dep_data$Dep) %in% admin1_bounds$ADM1_ES
missing_deps <- unique(historic_dep_data$Dep)[!unique(historic_dep_data$Dep) %in% admin1_bounds$ADM1_ES]
#Issue with accents --> sub out
still_missing <- c()
for(i in seq_along(missing_deps)) {
  this_dep <- missing_deps[i]
  tmp_change_name <- gsub("á", "a", this_dep)
  tmp_change_name <- gsub("ó", "o", tmp_change_name)
  tmp_change_name <- gsub("í", "i", tmp_change_name)
  tmp_change_name <- gsub("é", "e", tmp_change_name)
  
  if(tmp_change_name %in%admin1_bounds$ADM1_ES) {
    historic_dep_data$Dep[historic_dep_data$Dep == this_dep] <- tmp_change_name
  } else {
    print(paste("Still missing", this_dep))
    still_missing <- c(still_missing, this_dep)
  }
}

historic_dep_data$Dep[historic_dep_data$Dep == "Islas de la Bahía"] <- "Islas de La Bahia"
#historic_dep_data$Dep[historic_dep_data$Dep == "Metropolitana de San Pedro Sula"] <- ""
#historic_dep_data$Dep[historic_dep_data$Dep == "Metropolitana del Distrito Central"] <- ""

#Arbitrarily filter the data to remove dupes --> need to fix later
# historic_dep_data <- historic_dep_data %>%
#   dplyr::mutate(unique_id = paste(Dep, Mes, Edad, Dos, sep = "_")) %>%
#   dplyr::filter(!duplicated(unique_id)) %>%
#   dplyr::select(-unique_id)

#Re-pivot the data
pivoted_hist_dep_data <- historic_dep_data %>%
  pivot_wider(names_from = c(Mes, Edad, Dos), 
              values_from = Num_Doses) 

#Add Capital to Francisco Morazan and SPS to Cortes
fm1 <- pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Francisco Morazan",2:ncol(pivoted_hist_dep_data)] 
fm2 <- pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Metropolitana del Distrito Central",2:ncol(pivoted_hist_dep_data)] 
fm_new <- fm2
for(i in 1:ncol(fm_new)) {
  fm_new[,i] <- as.character(as.numeric(fm1[,i]) + as.numeric(fm2[,i]))
}

pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Francisco Morazan",2:ncol(pivoted_hist_dep_data)] <- fm_new

#Add SPS to Cortes
c1 <- pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Cortes",2:ncol(pivoted_hist_dep_data)] 
c2 <- pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Metropolitana de San Pedro Sula",2:ncol(pivoted_hist_dep_data)] 
c_new <- c2
for(i in 1:ncol(c_new)) {
  c_new[,i] <- as.character(as.numeric(c1[,i]) + as.numeric(c2[,i]))
}

pivoted_hist_dep_data[pivoted_hist_dep_data$Dep == "Cortes",2:ncol(pivoted_hist_dep_data)] <- c_new


joined_adm1 <- left_join(admin1_bounds, pivoted_hist_dep_data, by = c("ADM1_ES" = "Dep"))
joined_adm1 <- st_simplify(joined_adm1, dTolerance = 1000)

saveRDS(joined_adm1, "data/joined_adm1.rds")


#Connect the site data
vax_sites <- readRDS("data/Sites_w_Coords.rds")
historic_site_data <- readRDS("data/Site_Hist.rds")

#Re-pivot the data
pivoted_site_data <- historic_site_data %>%
  pivot_wider(names_from = c(Mes, Edad, Dos), 
              values_from = Num_Doses) 

vax_sites$tmp_code <- gsub("US", "", vax_sites$codigo_us)
#Sub all after initial string of numbers after dash
pivoted_site_data$tmp_code <- gsub(" - .*", "", pivoted_site_data$Sitio)

joined_site <- left_join(vax_sites, pivoted_site_data, by = "tmp_code")
joined_sites <- joined_site
saveRDS(joined_sites, "data/joined_sites.rds")


# distribution network data -----------------------------------------------

# 
# #Now the warehouse connections
# connections <- readxl::read_xlsx("data/travel time matrix between warehouses.xlsx", sheet = 2)
# 
# # remove rows where origin and destination warehouse are the same
# connections <- connections |> 
#   filter(if_all(`origin warehouse`, ~.x != `destination warehouse`))
# 
# 
# 
# names(connections)
# flows <- gcIntermediate(connections[,c("Latitude1", "Longitude1")], 
#                connections[,c("Latitude2", "Longitude2")],
#                sp = T,
#                addStartEnd = T)
# flows$Hours <- connections$Hours
# flows$origins <- connections$`origin warehouse`
# flows$destinations <- connections$`destination warehouse`
# 
# 
# hover <- paste0(flows$origins, " a ", 
#                 flows$destinations, ': ')
# 
# pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)
# 
# origin_wh <-  connections |> 
#   dplyr::select(`origin warehouse`, Longitude1, Latitude1) |> 
#   st_as_sf(coords = c("Latitude1", "Longitude1"))
# 
# dest_wh <-  connections |> 
#   filter(!(`destination warehouse` %in% `origin warehouse`)) |> 
#   dplyr::select(`destination warehouse`, Longitude2, Latitude2) |> 
#   st_as_sf(coords = c("Latitude2", "Longitude2"))
# 
# flow_leaflet <-leaflet() %>%
#   addProviderTiles("OpenStreetMap") %>%
#   addPolylines(data = flows, 
#                # label = hover,
#                group = ~origins, color = ~pal(origins)) %>%
#   addCircleMarkers(data = origin_wh, radius = 2, label = ~as.character(`origin warehouse`)) |>
#   addCircleMarkers(data = dest_wh, radius = 1, color = 'red', label = ~as.character(`destination warehouse`)) |>
#   addLayersControl(overlayGroups = unique(flows$origins),
#                    options = layersControlOptions(collapsed = T))
# 
# flow_leaflet
# saveRDS(flow_leaflet, "data/flow_leaflet.rds")

