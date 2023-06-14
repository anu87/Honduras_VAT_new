# child master key data ---------------------------------------------------


child_inventory_data <- salmi_inventory3 %>%
  dplyr::filter(grepl("peds", vax_type))


for(i in 1:length(unique(child_inventory_data$warehouse_code))) {
  this_code <- unique(child_inventory_data$warehouse_code)[i]
  
  filtered_data <- child_inventory_data %>%
    dplyr::filter(warehouse_code == this_code)
  
  filtered_data$batch_num <- filtered_data$`Nº de Lote`
  filtered_data$key <- paste0("w", filtered_data$warehouse_code, "_",
                              filtered_data$batch_num)
  
  add_rows <- filtered_data %>%
    dplyr::select(key, warehouse_code, batch_num, time_to_exp, Cantidad )
  
  if(i == 1) {
    new_df_child <- add_rows
  } else {
    new_df_child <- rbind(new_df_child, add_rows)
  }
}

#Now add rows for every municipality
for(i in 1:nrow(new_df_child)) {
  filtered_munis <- full_info %>% 
    dplyr::filter(warehouse_code == new_df_child$warehouse_code[i])
  
  for(j in 1:nrow(filtered_munis)) {
    merged_info <- merge(new_df_child[i,], filtered_munis[j,]) %>%
      dplyr::select(mun_code, warehouse_code, batch_num, key, Cantidad, time_to_exp)
    
    
    muni_avg <-  mun.doses %>% 
      dplyr::filter(mun_code == merged_info$mun_code, Edad == "Pediátrica") %>% dplyr::select(avg.daily.rate) %>%
      head(1) %>%
      unlist() %>%
      as.numeric()
    
    muni_avl <- muni_avg * as.numeric(merged_info$time_to_exp)
    
    merged_info$avg <- muni_avg
    merged_info$avl <- muni_avl
    
    if(i == 1 & j == 1) {
      master_key_child  <- merged_info
    } else {
      master_key_child  <- rbind(master_key_child , merged_info)
    }
  }
}

# 
# 
# empty_mat <- matrix(data = 0, nrow = length(unique(info_joined$mun_code)), ncol = (length(unique(na.omit(info_joined$warehouse_code)))+1)) #+1 for national
# rownames(empty_mat) <- unique(info_joined$mun_code)
# colnames(empty_mat) <- c(unique(na.omit(info_joined$warehouse_code)), "a") #Alm nat got code a 
# empty_mat[,"a"] <- 1 #All municipalities supplied by national warehouse
# 
# for(i in 1:nrow(empty_mat)) {
#   this_mun <- unique(info_joined$mun_code)[i]
#   selected_warehouses <- info_joined %>%
#     dplyr::filter(mun_code== this_mun) %>%
#     dplyr::select(warehouse_code) %>%
#     unlist() %>%
#     as.character()
#   
#   empty_mat[i, match(selected_warehouses, colnames(empty_mat))] <- 1 #Warehouses connected to this municipality
#   
# }
# mun_warehouse_mat <- empty_mat

mun_adult_pop_mat <- matrix(nrow = length(unique(info_joined$mun_code)), ncol = 1)
rownames(mun_adult_pop_mat) <- unique(info_joined$mun_code)

mun_child_pop_mat <- mun_adult_pop_mat

mun_dose_ref <- mun.doses %>%
  dplyr::filter(!is.na(region_code))


for(i in 1:nrow(mun_adult_pop_mat)) {
  this_mun <- unique(info_joined$mun_code)[i]
  
  selected_adult_pop <- mun_dose_ref %>%
    dplyr::filter(mun_code == this_mun,
                  Edad == "Adulto") %>%
    dplyr::select(world_pop) %>%
    unlist() %>%
    as.numeric()
  
  selected_child_pop <- mun_dose_ref %>%
    dplyr::filter(mun_code == this_mun,
                  Edad == "Pediátrica") %>%
    dplyr::select(world_pop) %>%
    unlist() %>%
    as.numeric()
  
  mun_adult_pop_mat[i,1] <- selected_adult_pop
  mun_child_pop_mat[i,1] <- selected_child_pop
}

mun_mat <- diag(length(unique(info_joined$mun_code)))
rownames(mun_mat) <- unique(info_joined$mun_code)
colnames(mun_mat) <- unique(info_joined$mun_code)






# 
# warehouse_mun_mat <- t(mun_warehouse_mat)
# warehouse_dose_mat_adult <- matrix(nrow = nrow(warehouse_mun_mat), ncol = 1)
# rownames(warehouse_dose_mat_adult) <- rownames(warehouse_mun_mat)
# warehouse_dose_mat_child <- warehouse_dose_mat_adult
# 
# trim_info <- info_joined %>%
#   dplyr::select(dep_clean, warehouse_name, warehouse_code) %>%
#   distinct()
# 
# warehouse_dose_joined <- left_join(salmi_inventory2, trim_info, by = c("Dep" = "dep_clean"))
# warehouse_dose_joined[warehouse_dose_joined$Almacen == "ALMACEN NACIONAL", "warehouse_code"] <- "A"
# 
# for(i in 1:nrow(warehouse_dose_mat)) {
#   this_warehouse <- rownames(warehouse_dose_mat)[i]
#   
#   filtered_wh_doses_adult <- warehouse_dose_joined %>% 
#     dplyr::filter(warehouse_code == this_warehouse,
#                   category == "vaccine", 
#                   grepl("adults", vax_type)) %>%
#     dplyr::mutate(total_doses = sum(Cantidad)) %>%
#     dplyr::select(total_doses) %>%
#     head(1) %>%
#     unlist() %>%
#     as.numeric()
#   
#   
#   if(length(filtered_wh_doses_adult) == 0) {
#     filtered_wh_doses_adult <- 0
#   }
#   
#   warehouse_dose_mat_adult[i, 1] <- filtered_wh_doses_adult
#   
#   filtered_wh_doses_child <- warehouse_dose_joined %>% 
#     dplyr::filter(warehouse_code == this_warehouse,
#                   category == "vaccine", 
#                   grepl("peds", vax_type)) %>%
#     dplyr::mutate(total_doses = sum(Cantidad)) %>%
#     dplyr::select(total_doses) %>%
#     head(1) %>%
#     unlist() %>%
#     as.numeric()
#   
#   if(length(filtered_wh_doses_child) == 0) {
#     filtered_wh_doses_child <- 0
#   }
#   
#   warehouse_dose_mat_child[i, 1] <- filtered_wh_doses_child
#   
# }


#Expiration date in the data -->
#Rows not warehouse but warehouse x batch 
#municipality warehouse matrix not with warehouse but warehouse x batch





