allocation <- read_rds("appdata/allocation.rds")

connections2 <- readRDS("appdata/connections.rds") 

connections_key <- connections2 %>% 
  filter(warehouse_code != "A")

#Need to separate out rows from national warehouse
regional_allocs <- allocation %>% 
  dplyr::filter(warehouse_code == "A")

#All other ones --> ugly but easier rejoining after manipulation
remaining_allocs <- allocation %>%
  dplyr::filter(warehouse_code != "A")

#Match indices of these against the key to then replace warehouse info 
index_matches <- match(regional_allocs$mun_code,connections_key$mun_code)

#NA ones --> only supplied by national (removed externos from data for now so that's probably why)
remaining_allocs <- rbind(remaining_allocs, regional_allocs[which(is.na(index_matches)),])
regional_allocs <- regional_allocs[-which(is.na(index_matches)),]

index_matches <- match(regional_allocs$mun_code,connections_key$mun_code)

#replace almacen, warehouse code, dep
regional_allocs$warehouse_code <- connections_key$warehouse_code[index_matches]
regional_allocs$Almacen <- connections_key$Almacen[index_matches]
regional_allocs$Dep <- connections_key$dep_clean[index_matches]

#Rejoin with the allocation table --> replace original rows and then output nat'l to regional as another element in list output?
remaining_allocs <- rbind(remaining_allocs, regional_allocs)

clean_allocs <- remaining_allocs %>% 
  dplyr::select(Almacen, mun_name, batch_num, vax_allocated) %>%
  dplyr::rename(Origin = Almacen, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
  dplyr::mutate(Distribution = "Final")

natl_allocs <- regional_allocs %>%#Need two of these --> 1 for allocs from natl to regional 1 from regional to muni
  dplyr::select(Almacen, mun_name, batch_num, vax_allocated) %>%
  dplyr::rename(Origin = Almacen, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
  dplyr::mutate(Destination = Origin,
                Origin = "ALMACEN NACIONAL") %>%
  dplyr::group_by(Origin, Destination, `Batch Num`) %>%
  dplyr::summarise(Allocated = sum(Allocated)) %>%
  ungroup() %>%
  dplyr::mutate(Distribution = "Intermediate")
 
clean_allocs <- rbind(clean_allocs, natl_allocs) %>%
  dplyr::arrange(Origin, Destination, `Batch Num`)

#Create output list for user --> first one has all final distribution legs, second one has all intermediate, third one is clean for data table --> download all of them?
allocation_list <- list(remaining_allocs, natl_allocs, clean_allocs)
names(allocation_list) <- c("final", "intermediate", "clean")
