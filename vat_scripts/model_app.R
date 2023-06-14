


vat.model <- function(days_allocated = days_allocated_value){
  
  # source("~/Honduras_VAT/start-up.R")
  master_key_adult <- readRDS("appdata/master_key_adult.rds")
  batch_list <- readRDS("appdata/batch_list.rds")
  mun_list <- readRDS("appdata/mun_list.rds")
  warehouse_codes <- readRDS("data/warehouse_codes.rds")
  vax_network_codes <- readRDS('data/site_mun_dep_codes.rds')
  
  
  master_key_adult$vax_admin_const <- round(master_key_adult$avg*days_allocated, 0)
  
  # given the time for allocation (30 days); calculate the maximum doses that can be administered given 
  # historical avg daily vaccination rate
  master_key_adult$vax_admin_const <- round(master_key_adult$avg*days_allocated, 0)
  
  # lpsolve API -------------------------------------------------------------
  
  # RHS for constraints -----------------------------------------------------
  # 1. municipality population constraint
  mun_pop_const = master_key_adult |> 
    distinct(mun_code, eligible_atleast_1dose) |> 
    dplyr::select(eligible_atleast_1dose) |> 
    unlist()
  
  # 2. municipality vaccine administration constraint
  mun_admin_const = master_key_adult |> 
    distinct(mun_code, vax_admin_const) |> 
    dplyr::select(vax_admin_const) |> 
    unlist()
  
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
  
  lp.vat <- make.lp(length(all.cons), nrow(master_key_adult))
  
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
  
  master_key_adult2 <- rownames_to_column(master_key_adult, 'row.code') 
  
  
  obj.df <- master_key_adult2 |> 
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
  
  master_key_adult <- master_key_adult |> left_join(warehouse_codes, by= 'warehouse_code')
  
  allocation <- cbind.data.frame(master_key_adult, vax_allocated = get.variables(lp.vat))
  
  allocation <- allocation |> 
    left_join(vax_network_codes |> dplyr::select(mun_name, mun_code), by='mun_code')
}
