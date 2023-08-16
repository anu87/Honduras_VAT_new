#THIS FILE GENERATES CONNECTIONS.RDS --> USED TO SHOW warehouse, warehouse code, region, dept, lat/longs
#PREREQs --> WAREHOUSE_MASTER_KEY_GENRATION.R to generate full_info.rds; Honduras_Site_Prep.R for joined_site/joined_adm2


library(sf)

adm2 <- readRDS("data/joined_adm2.rds")
adm2_centroids <- st_centroid(adm2)
adm2_centroids$ADM2_ES

joined_site <- read_rds("data/joined_sites.rds")

key_base <- read_rds("appdata/full_info.rds") #GENERATE FROM WAREHOUSE_MASTER_KEY_GENRATION.R
warehouse_codes <- key_base %>%
  dplyr::select(dep_clean, warehouse_code) %>%
  distinct() %>%
  arrange(warehouse_code)


#298 rows --> 596 in warehouse codes because double for alm nacional
#Join on code
key_base$mun_code
adm2_centroids$mun_code <- gsub("HN", "", adm2_centroids$ADM2_PCODE) 

all(adm2_centroids$mun_code %in% key_base$mun_code)

adm2_clean <- adm2_centroids %>% distinct(mun_code, geometry)
adm2_clean$lon1 <- st_coordinates(adm2_clean)[,1]
adm2_clean$lat1 <- st_coordinates(adm2_clean)[,2]
adm2_clean<- st_drop_geometry(adm2_clean)

#Join muni centroids with key
key_base_joined <- left_join(key_base, adm2_clean, by = "mun_code")

#Join to warehouse coords
warehouse_coords <- read_rds("data/Warehouses_w_Coords.rds")
warehouse_coords$`Warehouse name`

key_base_joined$dep_clean %>% unique()

key_base_joined$wh_join_name <- dplyr::case_when(key_base_joined$dep_clean == "Almacen Nacional" ~ "Almacén Naciónal de Biológicos" ,
                                                 key_base_joined$dep_clean == "Atlantida" ~ "Almacen Regional Atlantida\r\nRS Atlántida",
                                                 key_base_joined$dep_clean == "Colon" ~ "RS Colón",
                                                 key_base_joined$dep_clean == "Comayagua" ~ "Almacen Regional Comayagua\r\nRS Comayagua",
                                                 key_base_joined$dep_clean == "Copan" ~ "Almacen Regional Copán\r\nRS Copán",
                                                 key_base_joined$dep_clean == "Cortes" ~ "RS cortes",
                                                 key_base_joined$dep_clean == "El Paraiso" ~ "RS El Paraíso",
                                                 key_base_joined$dep_clean == "Francisco Morazan" ~ "RS Francisco Morazán",
                                                 key_base_joined$dep_clean == "Gracias a Dios"  ~ "RS Gracias a Dios",
                                                 key_base_joined$dep_clean == "Intibuca" ~ "RS Intibucá",
                                                 key_base_joined$dep_clean == "Islas de la Bahia" ~ "RS Islas de la Bahía" ,
                                                 key_base_joined$dep_clean == "La Paz" ~ "RS La Paz",
                                                 key_base_joined$dep_clean == "Lempira" ~ "RS Lempira",
                                                 key_base_joined$dep_clean == "Olancho" ~ "RS Olancho",
                                                 key_base_joined$dep_clean == "Santa Barbara" ~ "RS Santa Bárbara",
                                                 key_base_joined$dep_clean == "Valle"  ~ "RS Valle",
                                                 key_base_joined$dep_clean == "Yoro"  ~ "RS  Yoro",
                                                 key_base_joined$dep_clean == "Metropolitana de San Pedro Sula" ~ "RS cortes", #FLAG THAT IT'S NOT IN THE DATASET SO GOING CLOSE AS CAN
                                                 key_base_joined$dep_clean == "Distrito Central"  ~ "RS Metropolitana Distrito Central",
                                                 key_base_joined$dep_clean == "Choluteca"  ~ "RS Choluteca",
                                                 key_base_joined$dep_clean == "Ocotepeque"  ~ "RS Ocotepeque")

warehouse_coords_distinct <- warehouse_coords %>%
  dplyr::select(`Warehouse name`, geometry) %>% distinct()
#Manually remove last one because there's two nat warehouses
warehouse_coords_distinct <- warehouse_coords_distinct[-nrow(warehouse_coords_distinct),]
warehouse_coords_distinct$lon2 <- st_coordinates(warehouse_coords_distinct)[,1]
warehouse_coords_distinct$lat2 <- st_coordinates(warehouse_coords_distinct)[,2]
warehouse_coords_distinct <- st_drop_geometry(warehouse_coords_distinct)

connections <-left_join(key_base_joined, warehouse_coords_distinct, by = c("wh_join_name" = "Warehouse name"))

#Need to swap lon1/lat1 bc those got messed up
connections[,c("lon2", "lat2")] <- connections[,c("lat2", "lon2")]


saveRDS(connections, "data/connections.rds")
saveRDS(connections, "appdata/connections.rds")
# #Need connections file with every muni centroid (lat1, lon1) and 
# #the corresponding warehouse location (lat2, lon2)


connections <- connections %>%
  filter(warehouse_code != "A")

flows <- gcIntermediate(connections[,c("lon1", "lat1")],
                        connections[,c("lon2", "lat2")],
                        sp = T,
                        addStartEnd = T)


flows$origins <- connections$mun_name
flows$destinations <- connections$dep_clean


hover <- paste0(flows$origins, " a ",
                flows$destinations, ': ')

pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)

origin_wh <-  connections |>
  dplyr::select(dep_clean, lon2, lat2) |>
  st_as_sf(coords = c("lon2", "lat2"))

dest_wh <-  connections |>
  dplyr::select(mun_name, lon1, lat1) |>
  st_as_sf(coords = c("lon1", "lat1"))

out_leaf <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolylines(data = flows,
               # label = hover,
               group = ~origins, color = ~pal(origins)) %>%
  addCircleMarkers(data = origin_wh, radius = 1.5, label = ~as.character(dep_clean)) |>
  addCircleMarkers(data = dest_wh, radius = 0.75, color = 'red', label = ~as.character(mun_name)) |>
  addLayersControl(overlayGroups = unique(flows$origins),
                   options = layersControlOptions(collapsed = T))

out_leaf

saveRDS(out_leaf, "appdata/full_net_leaflet.rds")

