

child_allocation <- read_rds("data/child_allocation.rds")
child_allocation$key <- paste(child_allocation$Almacen, child_allocation$mun_code, sep = "_")

connections2$key <- paste(connections2$Almacen, connections2$mun_code, sep = "_")

connections2$Almacen %>% unique()
child_allocation$Almacen %>% unique()

child_allocation$key[!child_allocation$key %in% connections2$key]
child_alloc_joined <- left_join(child_allocation, connections2, by = "key") %>% as.tibble()



flows2 <- gcIntermediate(child_alloc_joined[,c("lat1", "lon1")], 
                         child_alloc_joined[,c("lat2", "lon2")],
                         sp = T,
                         addStartEnd = T)


flows2$Origin <- child_alloc_joined$Almacen.x
flows2$Destination <- child_alloc_joined$mun_name.x
flows2$Suministro <- child_alloc_joined$Suministro
flows2$Batch_Num <- child_alloc_joined$batch_num
flows2$Time_to_Exp <- child_alloc_joined$time_to_exp
flows2$Vax_Allocated <- child_alloc_joined$vax_allocated


label_list <- list()
popup_list <- list()
for(i in 1:nrow(flows2)) {
  label_list[[i]] <- paste0(str_to_title(flows2$Origin[i]), " a ", flows2$Destination[i])
  popup_list[[i]] <- paste(glue::glue("<b>{str_to_title(flows2$Origin[i])} a {flows2$Destination[i]}</b>"),  
                           glue::glue("<i>Suministro</i>: {flows2$Suministro[i]}"),
                           glue::glue("<i>Número de lote</i>: {flows2$Batch_Num[i]}"),
                           glue::glue("<i>Vacunas Asignadas</i>: {flows2$Vax_Allocated[i]}"),
                           sep = "</br>")
}

pal <- colorFactor(brewer.pal(4, "Set2"), flows2$Origin)

leaflet() %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolylines(data = flows2, 
               label = label_list,
               popup = popup_list,
               group = ~Origin, color = ~pal(Origin)) %>%
  addCircleMarkers(data = origin_almacen, radius = 1.5, label = ~as.character(Almacen)) |>
  addCircleMarkers(data = dest_mun, radius = 0.75, color = 'red', label = ~as.character(mun_name)) |>
  addLayersControl(overlayGroups = unique(flows2$origins),
                   options = layersControlOptions(collapsed = T))%>%
  addPolygons(data = adm2,
              stroke = T,
              color = "black",
              weight = 1,
              fill = F,
              opacity = 1)


allocation <- read_rds("data/allocation.rds")
allocation$key <- paste(allocation$Almacen, allocation$mun_code, sep = "_")
allocation$key[!allocation$key %in% connections2$key]
