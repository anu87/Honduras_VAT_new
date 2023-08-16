#Prereqs --> just have mun.doses generated (model_data_prep.R)


source("start-up.R")

# datasets to create------
# add average vax data to mun file; anywhere daily average is < 1, make it 5 (mean) - so that they atleast get some vaccines

mun.doses <- mun.doses |> 
  mutate(region_code = case_when(
    is.na(region_code) ~ substr(mun_code,1,2),
    .default = region_code
  ))

mun.doses <-mun.doses|> 
  dplyr::group_by(Dep, region_code, mun_name, mun_code, Edad) |> 
  dplyr::summarise( world_pop = max(world_pop),across(`1R`:eligible_atleast_1dose, ~sum(.x, na.rm = TRUE)), .groups = 'drop')  



mun.doses <- mun.doses |> 
  left_join(mun_avg_vax_rate |> dplyr::select(-mun_name), by='mun_code')

# mun.doses <- mun.doses |>
#   mutate(avg.daily.rate = case_when(
#     avg.daily.rate <10 & eligible_atleast_1dose <300 ~ 10, #300 because avg 10 * 30 days for allocation
#     eligible_atleast_1dose >=300 ~ eligible_atleast_1dose,
#     .default = avg.daily.rate
#   ))

mun.doses$avg.daily.rate <- ifelse(mun.doses$avg.daily.rate<5, 5, mun.doses$avg.daily.rate)

mun.doses$eligible_atleast_1dose <- ifelse(is.na(mun.doses$eligible_atleast_1dose), mun.doses$world_pop, 
                                           mun.doses$eligible_atleast_1dose)

mun.doses$eligible_atleast_1dose <- ifelse(mun.doses$eligible_atleast_1dose<0, mun.doses$world_pop, 
                                           mun.doses$eligible_atleast_1dose)

# 1. matrix of mun with warehouses

mun.doses$Dep2 <- stri_trans_general(gsub("Departamental de |Metropolitana del | Metropolitana de ", "", mun.doses$Dep), 'latin-ascii') %>%
  trimws()

# mun.doses$Dep2 %>% unique()


#We want file with municipal doses with name of the salmi warehouse next to it 
#Just need department, region code, municipal code, municipal name, warehouse name --> remove all the numbers from mun.doses
#Create warehouse code and then a region code


key_base <- mun.doses %>%
  dplyr::select(Dep2, Dep, region_code, mun_name, mun_code) %>%
  dplyr::rename(dep_clean = Dep2,
                dep_full = Dep)

#If they ever add more than 26 warehouses then figure out a better way to do this but until then no thank you
warehouse_codes <- key_base %>%
  dplyr::select(dep_clean) %>%
  distinct()

#National not in this one
natl_df <- data.frame("Almacen Nacional")
names(natl_df) <- c("dep_clean")

warehouse_codes <- rbind(natl_df, warehouse_codes)

warehouse_codes$warehouse_code <-LETTERS[1:nrow(warehouse_codes)]

info_joined <- left_join(key_base, warehouse_codes, by = c("dep_clean")) %>%
  dplyr::filter(!is.na(region_code)) #Double check with anubhuti

nat_info <- info_joined %>%
  dplyr::select(mun_name, mun_code) %>%
  dplyr::mutate(dep_clean = "Almacen Nacional",
                dep_full = "Almacen Nacional",
                region_code = "", 
                warehouse_code = "A")

full_info <- rbind(info_joined, nat_info) %>%
  dplyr::filter(!is.na(warehouse_code)) %>%
  distinct()

saveRDS(full_info, "appdata/full_info.rds")

