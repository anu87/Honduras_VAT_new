
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

key_base_2 <- salmi_inventory2 %>%
  dplyr::select(Dep)

#If they ever add more than 26 warehouses then figure out a better way to do this but until then no thank you
warehouse_codes <- key_base %>%
  dplyr::select(dep_clean) %>%
  distinct()

#National not in this one
natl_df <- data.frame("Almacen Nacional")
names(natl_df) <- c("dep_clean")

warehouse_codes <- rbind(natl_df, warehouse_codes)


warehouse_codes$warehouse_code <-LETTERS[1:nrow(warehouse_codes)]

#warehouse_codes <- read_rds("appdata/warehouse_codes_revised.rds")

salmi_inventory3 <- left_join(salmi_inventory2, warehouse_codes, by = c("Dep")) %>%
  dplyr::filter(category == "vaccine")



tmp_joined <- left_join(key_base, key_base_2, by = c("dep_clean" = "Dep")) %>% distinct()
info_joined <- left_join(tmp_joined, warehouse_codes, by = c("dep_clean" = "Dep")) %>%
  dplyr::filter(!is.na(region_code)) #Double check with anu
nat_info <- info_joined %>%
  dplyr::select(mun_name, mun_code) %>%
  dplyr::mutate(dep_clean = "",
                dep_full = "",
                region_code = "", 
                warehouse_code = "A")

full_info <- rbind(info_joined, nat_info) %>%
  dplyr::filter(!is.na(warehouse_code))

# saveRDS(full_info, "data/full_info.rds")


#Make adult data key
adults_inventory_data <- salmi_inventory3 %>%
  dplyr::filter(grepl("adults", vax_type))

for(i in 1:length(unique(adults_inventory_data$warehouse_code))) {
  this_code <- unique(adults_inventory_data$warehouse_code)[i]
  
  filtered_data <- adults_inventory_data %>%
    dplyr::filter(warehouse_code == this_code)
  

  filtered_data$key <- paste0("w", filtered_data$warehouse_code, "_",
                              filtered_data$batch_num)
  
  add_rows <- filtered_data %>%
    dplyr::select(key, warehouse_code, batch_num, time_to_exp, Cantidad )
  
  if(i == 1) {
    new_df <- add_rows
  } else {
    new_df <- rbind(new_df, add_rows)
  }
}

#Now add rows for every municipality
for(i in 1:nrow(new_df)) {
  filtered_munis <- full_info %>% 
    dplyr::filter(warehouse_code == new_df$warehouse_code[i])
  
  for(j in 1:nrow(filtered_munis)) {
    merged_info <- merge(new_df[i,], filtered_munis[j,]) %>%
      dplyr::select(mun_code, warehouse_code, batch_num, key, Cantidad, time_to_exp)
    
   
    muni_avg <-  mun.doses %>% 
      dplyr::filter(mun_code == merged_info$mun_code, Edad == "Adulto") %>% dplyr::select(avg.daily.rate) %>%
      head(1) %>%
      unlist() %>%
      as.numeric()
    
    muni_avl <- muni_avg * as.numeric(merged_info$time_to_exp)
    
    merged_info$avg <- muni_avg
    merged_info$avl <- muni_avl
    
    if(i == 1 & j == 1) {
      master_key_adult  <- merged_info
    } else {
      master_key_adult  <- rbind(master_key_adult , merged_info)
    }
  }
}

# group by key - to combine different versions of pfizer (purple and gray) - separate later
master_key_adult <- master_key_adult |> 
  group_by(mun_code, warehouse_code, batch_num, key, time_to_exp, avg) |> 
  summarise(Cantidad = sum(Cantidad), .groups = 'drop')

master_key_adult |> group_by(mun_code, key) |> count() |> view()

adults_inventory_data <- adults_inventory_data |> 
  group_by(Almacen, `U. de Emisión`,`Nº de Lote`, `Fecha Vto`,exp_date, time_to_exp, 
           vax_type, category,warehouse_code) |> 
  summarise(Cantidad = sum(Cantidad), .groups = 'drop')

# data for adults vax allocation ------------------------------------------

master_key_adult$dose_quantity <- master_key_adult$Cantidad*6
days_to_allocate = 30



mun.doses.adults <- mun.doses |> filter(Edad == 'Adulto')

# add doses needed data from mun.doses file
master_key_adult <- master_key_adult |> 
  left_join(mun.doses.adults |> dplyr::select(mun_code, eligible_atleast_1dose), by='mun_code')


# sort master key adult on expiration date
master_key_adult <- master_key_adult |> 
  group_by(key) |> 
  arrange(time_to_exp, .by_group = TRUE) |> 
  ungroup()


# save Master_key_adults file ---------------------------------------------

saveRDS(master_key_adult, "appdata/master_key_adult.rds")

# given the time for allocation (30 days); calculate the maximum doses that can be administered given 
# historical avg daily vaccination rate
master_key_adult$vax_admin_const <- round(master_key_adult$avg*days_to_allocate, 0)

# lpsolve API -------------------------------------------------------------

# RHS for constraints -----------------------------------------------------
# 1. municipality population constraint
mun_pop_const = master_key_adult |> 
  distinct(mun_code, eligible_atleast_1dose)

mun_pop_const <- mun_pop_const$eligible_atleast_1dose |> unlist()

# 2. municipality vaccine administration constraint
mun_admin_const = master_key_adult |> 
  distinct(mun_code, vax_admin_const)

mun_admin_const <- mun_admin_const$vax_admin_const |> unlist()

# 3. min vax allocation to Municipality - 10% of admin constraint
mun_min_const = master_key_adult |> 
  distinct(mun_code, vax_admin_const)

mun_min_const$mun_min_const <- round(mun_min_const$vax_admin_const*0.1,0)

mun_min_const <- mun_min_const$mun_min_const |> unlist()

# 4. warehouse-batch doses available
batch_dose_const <- master_key_adult |> 
  distinct(key, dose_quantity)

batch_dose_const <- batch_dose_const |> group_by(key) |> 
  summarise(dose_quantity = sum(dose_quantity), .groups = 'drop')

batch_dose_const <- batch_dose_const$dose_quantity |> unlist()

# LHS constraint matrices -------------------------------------------------
# 1. municipality by by batch-warehouse matrix for population constraint
# 2. municipality by batch-warehouse matrix for administrative constraint
# 3. municipality matrix by batch-warehouse for min dose allocation -  10% of admin constraint
# use the same mun_list for all 3

# create mun level constraint - each list is a set of coeff for each mun
mun_mat <- master_key_adult |> distinct(mun_code)
row.names(master_key_adult) <- rownames(master_key_adult$key)

master_key_adult2 <- rownames_to_column(master_key_adult, 'row.code') 

mun_list <- list()

for(i in 1:nrow(mun_mat)){
  
  code <- mun_mat$mun_code[i]
  master_fil <- master_key_adult2 |> filter(mun_code == code)
  
  
  mun_list[[i]] <- unlist(master_fil$row.code) |> as.numeric()
  
}

saveRDS(mun_list, "appdata/mun_list.rds")

# 4. warehouse-batch by municipality matrix for doses constraint
#### used to calculate total vaccines allocated across all municipalities for each batch
#### which should <= to the total doses available for each batch

# create batch level constraint
batch_list <- list()

batch_codes <- unique(master_key_adult$key)

for (i in 1:length(batch_codes)) {
  
  keyx <- batch_codes[i]
  batch_fil <- master_key_adult2 |> filter(key == keyx)
  
  batch_list[[i]] <- unlist(batch_fil$row.code) |> as.numeric()
}

saveRDS(batch_list, "appdata/batch_list.rds")
# 
# # directions for lp model -------------------------------------------------
# f.dir.pop = rep('<=', length(mun_list))
# f.dir.admin = rep('<=', length(mun_list))
# f.dir.min = rep('>=', length(mun_list))
# f.dir.batch = rep('<=', length(batch_list))
# 
# # constraints - lpsolveAPI ------------------------------------------------
# 
# all.cons <- c(mun_list, mun_list, mun_list, batch_list)
# 
# all.rhs <- c(mun_pop_const, mun_admin_const, mun_min_const, batch_dose_const)
# 
# all.dir <-  c(f.dir.pop, f.dir.admin, f.dir.min, f.dir.batch)
# 
# # create an lp with all constraints and decision variables
# # here each mun-batch combination is a decision variable
# 
# lp <- make.lp(length(all.cons), nrow(master_key_adult))
# 
# col <- 0
# 
# for (i in 1:length(all.cons)) {
#   
#   col <- col+1
#   
#   add.constraint(lp,
#                  rep(1, length(all.cons[[col]])),
#                  type = all.dir[col],
#                  rhs = all.rhs[col],
#                  indices = all.cons[[col]])
#   
# }
# 
# # set objective function-----
# 
# obj.df <- master_key_adult2 |> 
#   distinct(mun_code, key, time_to_exp) |> 
#   mutate(time_to_exp = as.numeric(gsub(" days","",time_to_exp)),
#          time_to_exp_rev = round(400/time_to_exp, 0)) |>
#   # weight regional warehouse higher than national - so that vax are first allocated from there
#   mutate(time_to_exp_rev = case_when(
#     startsWith(key, "wA") ~ time_to_exp_rev,
#     .default = time_to_exp_rev+10
#   ))
#   
# set.objfn(lp, obj.df$time_to_exp_rev)
# 
# lp.control(lp, sense='max')
# solve(lp)
# 
# 
# # check results -----------------------------------------------------------
# 
# #saveRDS(warehouse_codes, "appdata/warehouse_codes.rds")
# 
# master_key_adult <- master_key_adult |> left_join(warehouse_codes, by= 'warehouse_code')
# 
# allocation <- cbind.data.frame(master_key_adult, vax_allocated = get.variables(lp))
# 
# allocation <- allocation |> 
#   left_join(vax_network_codes |> dplyr::select(mun_name, mun_code), by='mun_code')
# 
# 
# 
# allocation <- allocation |> filter(vax_allocated>0)
# sum(allocation$vax_allocated) 
# allocation |> distinct(mun_code, vax_admin_const) |> summarise(sum(vax_admin_const))
# # check if each bacth has been allocated
# allocation |> group_by(key, time_to_exp) |> 
#   summarise(sum(vax_allocated, na.rm = T), .groups = 'drop') |> 
#   view()
# 
# # batch - wQ_FY8211 has only 36 doses available - so it has not been allocated at all
# 
# # check how many different batches each municipality receives vaccines from
# allocation |> 
#   filter(vax_allocated >0) |> 
#   distinct(mun_code, key) |> 
#   count(mun_code) |> view()
# 
# 
# get.bounds(lp)
# get.objective(lp)
# get.constraints(lp)
# print.lpExtPtr(lp)
# get.constr.type(lp)
# get.constr.value(lp)
# get.dual.solution(lp)
# get.primal.solution(lp)
# get.sensitivity.obj(lp) |> view()
# get.sensitivity.objex(lp) |> view()
# get.sensitivity.rhs(lp) |> view()
# get.variables(lp) |> view()

