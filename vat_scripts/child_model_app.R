
# days_allocated = 60

child.vat.model <- function(days_allocated = days_allocated_value,
                            salmi_inventory2){
  
  # source("~/Honduras_VAT/start-up.R")
  master_key_child <- readRDS("appdata/master_key_child.rds")
  batch_list <- readRDS("appdata/batch_list_child.rds")
  mun_list <- readRDS("appdata/mun_list_child.rds")
  warehouse_codes <- readRDS("appdata/warehouse_codes_revised.rds")
  
  # #For some reason warehouse codes are not mapping properly --> manual fix for now
  # warehouse_codes$warehouse_code[warehouse_codes$Dep == "Lempira"] <- "O"
  # warehouse_codes$warehouse_code[warehouse_codes$Dep == "Metropolitana de San Pedro Sula"] <- "N"
  # warehouse_codes$warehouse_code[warehouse_codes$Dep == "Distrito Central"] <- "M"
  # warehouse_codes <- warehouse_codes %>% arrange(warehouse_code)
  # saveRDS(warehouse_codes,"appdata/warehouse_codes_revised.rds")
  #Lempira -- 0; SPS -- N; DC -- M --> need to fix
  
  vax_network_codes <- readRDS('appdata/site_mun_dep_codes.rds')
  connections <- readRDS("appdata/connections.rds")
  salmi_app_data <- readRDS("appdata/salmi_app_data.rds")
  
  #salmi_inventory2 <- readRDS("appdata/salmi_inventory2.rds")
  
  master_key_child$vax_admin_const <- round(master_key_child$avg*days_allocated, 0)
  
  # given the time for allocation (30 days); calculate the maximum doses that can be administered given 
  # historical avg daily vaccination rate
  
  # lpsolve API -------------------------------------------------------------
  
  # RHS for constraints -----------------------------------------------------
  # 1. municipality population constraint
  mun_pop_const = master_key_child |> 
    distinct(mun_code, eligible_atleast_1dose) |> 
    dplyr::select(eligible_atleast_1dose) |> 
    unlist()
  
  # 2. municipality vaccine administration constraint
  mun_admin_const = master_key_child |> 
    distinct(mun_code, vax_admin_const) |> 
    dplyr::select(vax_admin_const) |> 
    unlist()
  
  # 3. min vax allocation to Municipality - 10% of admin constraint
  mun_min_const = master_key_child |> 
    distinct(mun_code, vax_admin_const)
  
  mun_min_const$mun_min_const <- round(mun_min_const$vax_admin_const*0.1,0)
  
  mun_min_const <- mun_min_const$mun_min_const |> unlist()
  
  # 4. warehouse-batch doses available
  batch_dose_const <- master_key_child |> 
    distinct(key, dose_quantity)
  
  batch_dose_const <- batch_dose_const |> group_by(key) |> 
    summarise(dose_quantity = sum(dose_quantity), .groups = 'drop')
  
  batch_dose_const <- batch_dose_const$dose_quantity |> unlist()
  
  # directions for lp model -------------------------------------------------
  f.dir.pop = rep('<=', length(mun_list))
  f.dir.admin = rep('<=', length(mun_list))
  f.dir.min = rep('>=', length(mun_list))
  f.dir.batch = rep('<=', length(batch_list))
  
  # constraints - lpsolveAPI ------------------------------------------------
  
  all.cons <- c(mun_list, mun_list, mun_list, batch_list)
  
  all.rhs <- c(mun_pop_const, mun_admin_const, mun_min_const, batch_dose_const)
  
  all.dir <-  c(f.dir.pop, f.dir.admin, f.dir.min, f.dir.batch)
  
  # create an lp with all constraints and decision variables
  # here each mun-batch combination is a decision variable
  
  lp.vat <- make.lp(length(all.cons), nrow(master_key_child))
  
  col <- 0
  
  for (i in 1:length(all.cons)) {
    
    col <- col+1
    
    add.constraint(lp.vat,
                   rep(1, length(all.cons[[col]])),
                   type = all.dir[col],
                   rhs = all.rhs[col],
                   indices = all.cons[[col]])
  }
  # set objective function-----
  
  master_key_child2 <- rownames_to_column(master_key_child, 'row.code') 
  
  
  obj.df <- master_key_child2 |> 
    distinct(mun_code, key, time_to_exp) |> 
    mutate(time_to_exp = as.numeric(gsub(" days","",time_to_exp)),
           time_to_exp_rev = round(400/time_to_exp, 0)) |>
    # weight regional warehouse higher than national - so that vax are first allocated from there
    mutate(time_to_exp_rev = case_when(
      startsWith(key, "wA") ~ time_to_exp_rev,
      .default = time_to_exp_rev+10
    ))
  
  set.objfn(lp.vat, obj.df$time_to_exp_rev)
  
  lp.control(lp.vat, sense='max')
  solve(lp.vat)
  
  
  # pull out results -----------------------------------------------------------
  
  master_key_child <- master_key_child |> left_join(warehouse_codes, by= 'warehouse_code')
  
  allocation <- cbind.data.frame(master_key_child, vax_allocated = get.variables(lp.vat))
  
  allocation <- allocation %>% 
    filter(vax_allocated > 0) %>% 
    mutate(vax_allocated = round(vax_allocated, 0))
  
  # add back municipality names and region names to the data
  
  clean_codes <- vax_network_codes |> dplyr::select(mun_name, mun_code, region_name) %>% distinct()
  
  allocation <- allocation |> 
    left_join(clean_codes, by='mun_code')
  
  # add back warehouse names to the data
  allocation2 <- allocation %>% 
    left_join(connections %>% distinct(Almacen, warehouse_code), by='warehouse_code')
  
  allocation2 <- allocation2 %>%
    left_join(salmi_inventory2 %>% filter(category=='vaccine') %>% distinct(batch_num, Suministro), by= ("batch_num"))
  
  
}


