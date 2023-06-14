
salmi_inventory <- readxl::read_xlsx("data/SALMI HON Inventario Biologicos_2022-01-13. Vaccine Inventory .xlsx", skip = 3)

salmi_inventory <- salmi_inventory |> dplyr::select(Almacen:Cantidad)

# throw out expired vaccines

salmi_inventory$exp_date <- as.Date(paste(salmi_inventory$`Fecha Vto`, "-26", sep = ""), "%Y-%m-%d")



# CHANGE EXPIRATION DATE - as data is old and most vaccines have expired

salmi_inventory2 <- salmi_inventory |> 
  mutate(exp_date = exp_date+180) |> 
  filter(exp_date > Sys.Date()) |> 
  # calculate time to expiration
  mutate(time_to_exp = exp_date - Sys.Date(),
         vax_type = case_when(
           Suministro == "Diluyente Pfizer adulto cloruro de sodio 0.9% 6 dosis  10ml" ~ "Dilutant_Pfizer_adult",
           Suministro == "Diluyente Pfizer pediatrico 10 dosis" ~ "Dilutant_Pfizer_peds",
           Suministro == "Diluyente Pfizer pediatrico 6 Meses a 4 Años 10 dosis" ~ "Dilutant_Pfizer_peds",
           Suministro == "Jeringa desechable 0.3 ml con aguja 23 GX 1" ~ "syringe_0.3ml_23gx1",
           Suministro == "Jeringa desechable 1 ml con aguja 22 GX 1 1/2" ~ "syringe_1ml_22gx1.5",
           Suministro == "Jeringa desechable 3 ml con aguja 22 GX 1 1/2" ~ "syringe_3ml_22gx1.5",
           Suministro == "Jeringa desechable 3 ml con aguja 23 GX 1" ~ "syringe_3ml_23gx1",
           Suministro == "Jeringa hipodermica 3 cc x 21 x 1 1/2" ~ "syringe_3cc_21x1.5",
           Suministro == "Jeringas jd 1ml 22x1 1/2" ~ "syringe_1ml_22x1.5",
           Suministro == "Pfizer adulto 6 dosis (Vacuna: COVID-19) tapa morada" ~ "Pfizer_adults", #purple cap pfizer needs dilution
           Suministro == "Pfizer pediatrica 10 dosis (Vacuna: COVID-19) tapa naranja" ~ "Pfizer_peds",
           Suministro == "Pfizer adulto diluida 6 dosis (Vacuna: COVID-19) tapa gris" ~ "Pfizer_adults", #gray cap pfizer does not need dilution
           Suministro == "Pfizer Ped. (6 meses - 4 años) 10 dosis (Vacuna: COVID-19) tapa marron" ~ "Pfizer_peds"
         ),
         category = case_when(
           str_detect(vax_type, "Dilutant") ~ 'dilutant',
           str_detect(vax_type, "syringe") ~ 'syringe',
           str_detect(vax_type, "Pfizer") ~ 'vaccine'
         ))


# match Dep with Almacen in Salmi data
salmi_inventory2$Dep <- stri_trans_general(gsub("ALM REGIONAL DE BIOLOGICOS", "", salmi_inventory2$Almacen), 'latin-ascii') %>%
  trimws() %>%
  str_to_title()

mun.doses$Dep2 <- stri_trans_general(gsub("Departamental de |Metropolitana del | Metropolitana de ", "", mun.doses$Dep), 'latin-ascii') %>%
  trimws()

setdiff(salmi_inventory2$Dep, mun.doses$Dep2)
setdiff(mun.doses$Dep2, salmi_inventory2$Dep)
# [1] "Almacen Nacional"  -- not region
#"Gracias A Dios" -- "Gracias a Dios"       
#"Islas De La Bahia" -- "Islas de la Bahia"     
#"Islas De La Bahia Ext" 
#"Metropolitana Dc" -- "Distrito Central"    
# [6] "Intibuca Externo"      
#"Metropolitana Sps"  --  "Metropolitana de San Pedro Sula"
#"La Paz Externo"        
#"Santa Barbara Externo"
#Combine the externos with the existing ones --> set to original name
#Almacen national --> connected to all municipalities
salmi_inventory2$Dep[salmi_inventory2$Dep == "Gracias A Dios"] <- "Gracias a Dios"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Islas De La Bahia"] <- "Islas de la Bahia"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Islas De La Bahia Ext"] <- "Islas de la Bahia"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Metropolitana Dc"] <- "Distrito Central"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Intibuca Externo"] <- "Intibuca"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Metropolitana Sps"] <- "Metropolitana de San Pedro Sula"
salmi_inventory2$Dep[salmi_inventory2$Dep == "La Paz Externo"] <- "La Paz"
salmi_inventory2$Dep[salmi_inventory2$Dep == "Santa Barbara Externo"] <- "Santa Barbara"
salmi_inventory2$Dep[salmi_inventory2$Dep == "ALM REGIONAL DE BIOLOGICOS SANTA BARBARA EXTERNO"] <- "ALM REGIONAL DE BIOLOGICOS SANTA BARBARA"


saveRDS(salmi_inventory2, 'data/salmi_inventory2.rds')

# salmi_inventory2 <-  salmi_inventory2 |> 
#   group_by(Almacen, `U. de Emisión`, `Nº de Lote`, `Fecha Vto`, exp_date, time_to_exp, vax_type, category) |> 
#   summarise(Cantidad = sum(Cantidad, na.rm = TRUE), .groups = 'drop')
  
  
# salmi_inventory2 |> distinct(Suministro) |> view()


# add region code to Salmi ------------------------------------------------
# to join with mun level vaccination file

unique(salmi_inventory2$Almacen)



