library(sf)

adm2 <- readRDS("data/joined_adm2.rds")
adm2_centroids <- st_centroid(adm2)
adm2_centroids$ADM2_ES

joined_site <- read_rds("data/joined_sites.rds")

key_base <- read_rds("data/full_info.rds")
warehouse_codes <- read_rds("data/warehouse_codes.rds")

key_base_2 <- salmi_vax_inventory %>% #I think this is salmi_inventory2
  dplyr::select(Almacen, Dep) %>%
  distinct()

key_base_2$is_externo <- ifelse(grepl("EXT", key_base_2$Almacen), T, F)
key_base_2 <- key_base_2 %>%
  dplyr::filter(!is_externo)

#Same as last time for consistent codes --> manually fix a few
key_base_2$warehouse_code <-LETTERS[1:nrow(key_base_2)]
key_base_2$warehouse_code[13] <- "O"
key_base_2$warehouse_code[14] <- "M"
key_base_2$warehouse_code[15] <- "N"

key_base_2<- key_base_2 %>%
  dplyr::select(Almacen, warehouse_code)


key_base <- left_join(key_base, key_base_2, by = "warehouse_code")

#298 rows in each
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

key_base_joined$Almacen %>% unique()

key_base_joined$wh_join_name <- dplyr::case_when(key_base_joined$Almacen == "ALMACEN NACIONAL" ~ "Almacén Naciónal de Biológicos" ,
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS ATLANTIDA" ~ "Almacen Regional Atlantida\r\nRS Atlántida",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS COLON" ~ "RS Colón",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS COMAYAGUA" ~ "Almacen Regional Comayagua\r\nRS Comayagua",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS COPAN" ~ "Almacen Regional Copán\r\nRS Copán",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS CORTES" ~ "RS cortes",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS EL PARAISO" ~ "RS El Paraíso",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS FRANCISCO MORAZAN" ~ "RS Francisco Morazán",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS GRACIAS A DIOS"  ~ "RS Gracias a Dios",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS INTIBUCA" ~ "RS Intibucá",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS ISLAS DE LA BAHIA" ~ "RS Islas de la Bahía" ,
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS LA PAZ" ~ "RS La Paz",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS LEMPIRA" ~ "RS Lempira",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS OLANCHO" ~ "RS Olancho",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS SANTA BARBARA" ~ "RS Santa Bárbara",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS VALLE"  ~ "RS Valle",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS YORO"  ~ "RS  Yoro",
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS METROPOLITANA SPS" ~ "RS cortes", #FLAG THAT IT'S NOT IN THE DATASET SO GOING CLOSE AS CAN
                                                 key_base_joined$Almacen == "ALM REGIONAL DE BIOLOGICOS METROPOLITANA DC"  ~ "RS Metropolitana Distrito Central")

warehouse_coords_distinct <- warehouse_coords %>%
  dplyr::select(`Warehouse name`, geometry) %>% distinct()
#Manually remove last one because there's two nat warehouses
warehouse_coords_distinct <- warehouse_coords_distinct[-nrow(warehouse_coords_distinct),]
warehouse_coords_distinct$lon2 <- st_coordinates(warehouse_coords_distinct)[,1]
warehouse_coords_distinct$lat2 <- st_coordinates(warehouse_coords_distinct)[,2]
warehouse_coords_distinct <- st_drop_geometry(warehouse_coords_distinct)

connections <-left_join(key_base_joined, warehouse_coords_distinct, by = c("wh_join_name" = "Warehouse name"))
saveRDS(connections, "data/connections.rds")
saveRDS(connections, "appdata/connections.rds")
#Need connections file with every muni centroid (lat1, lon1) and 
#the corresponding warehouse location (lat2, lon2)

connections <- connections |> rename(lat12 = lon1, lon1 = lat1)

connections <- connections |> rename(lat1 = lat12)

connections <- connections %>%
  filter(warehouse_code != "A")

flows <- gcIntermediate(connections[,c("lon1", "lat1")], 
                        connections[,c("lat2", "lon2")],
                        sp = T,
                        addStartEnd = T)


flows$origins <- connections$mun_name
flows$destinations <- connections$Almacen


hover <- paste0(flows$origins, " a ", 
                flows$destinations, ': ')

pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)

origin_wh <-  connections |> 
  dplyr::select(Almacen, lon2, lat2) |> 
  st_as_sf(coords = c("lat2", "lon2"))

dest_wh <-  connections |> 
  dplyr::select(mun_name, lon1, lat1) |> 
  st_as_sf(coords = c("lon1", "lat1"))

out_leaf <- leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolylines(data = flows, 
               # label = hover,
               group = ~origins, color = ~pal(origins)) %>%
  addCircleMarkers(data = origin_wh, radius = 1.5, label = ~as.character(Almacen)) |>
  addCircleMarkers(data = dest_wh, radius = 0.75, color = 'red', label = ~as.character(mun_name)) |>
  addLayersControl(overlayGroups = unique(flows$origins),
                   options = layersControlOptions(collapsed = T))

saveRDS(out_leaf, "appdata/full_net_leaflet.rds")
