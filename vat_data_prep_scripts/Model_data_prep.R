


# calculate population eligible and vax'd at Mun level --------------------

# pull in health center level adult pop vaccinated and eligible data
# this data is currently only available for only 1 district
eligible_adult_pop_hc <- read_excel("data/Eligible population per ES RSMDC.xlsx", 
                              sheet = "Adultos x dosis")

# read vaccine network data
vax_network <- read_excel("data/Establecimientos de salud longitud y latitud.xlsx", skip = 1)

vax_network_codes <- vax_network |> 
  separate(`Region Sanitaria`, c('region_code', 'region_name'), sep = "\\)", remove = TRUE) |> 
  separate(Municipio, c('mun_name', 'mun_code'), sep = "\\(", remove = TRUE) |> 
  separate(Aldea, c('aldea_name', 'aldea_code'), sep = "\\(", remove = TRUE) |> 
  mutate(region_code = region_code  |>  str_remove_all("\\("),
         mun_code = mun_code |> str_remove_all("\\)"),
         aldea_code = aldea_code |> str_remove_all("\\)"),
         mun_name = str_trim(mun_name),
         site_code = codigo_us |> str_remove_all("US")) |> 
  relocate(site_code, .after=codigo_us)

# some municipalities do have code, so pull the first 4 digits form Aldea code to mun_code
vax_network_codes <- vax_network_codes |> 
  mutate(mun_code = case_when(
    is.na(mun_code) ~ substr(aldea_code,1,4),
    .default = mun_code
  ))



# saveRDS(vax_network_codes, 'data/site_mun_dep_codes.rds')

# create Mun vax file from site
# add mun & dep codes to site hist
# Dep adm Mun assignments from vaccination dat are incorrect. So use site codes to
# pull right, Dep, Mun from the network file shared by the team

site_hist2 <- site_hist |> 
  left_join(vax_network_codes |> distinct(site_code, region_code, region_name, mun_name, mun_code), by='site_code')

# some sites are not present in vax_network_code; so use Mun_names from vax data and manually add codes
site.df.check <- site_hist2 |> filter(is.na(mun_code)) |> distinct(Dep, Mun, mun_code)

# table(site.df.check$Mun)

# match by Mun names
site.df.check <- site.df.check |> 
  mutate(mun_eng = stringi::stri_trans_general(Mun, 'latin-ascii'),
         region_name = stringi::stri_trans_general(str_remove_all(Dep, "Departamental de "), 'latin-ascii'))

vax_network_codes2 <- vax_network_codes |> 
  mutate(region_name2 = gsub(" Región Sanitaria | Región Metropolitana ", "", region_name),
         region_name2 = stri_trans_general(region_name2, 'latin-ascii')) |> 
  distinct(region_name2, mun_name, mun_code)

site.df.check <- site.df.check |> 
  left_join(vax_network_codes2, 
            by=c('region_name'='region_name2','mun_eng'='mun_name'), keep=TRUE)


#  two Mun are still unmatched - add them manually
# LaMascia is 0105 splet as La Masica; "Tegucigalpa M.D.C. is Distrito Central" - 0801

site.df.check <- site.df.check |> 
  mutate(mun_code.y = case_when(
    Mun == 'Lamasica' ~ '0105',
    Mun == 'Tegucigalpa M.D.C.' ~ '0801',
    .default = mun_code.y
  ))


# Add missing mun_code to site vax data
site_hist2 <- site_hist2 |> left_join(site.df.check |> dplyr::select(Dep, Mun, mun_code.y), by=c('Dep', 'Mun'))



site_hist3 <- site_hist2 |> 
  mutate(mun_code = ifelse(is.na(mun_code), mun_code.y, mun_code),
         Mun = ifelse(is.na(Mun)|mun_code=='0801', "Tegucigalpa M.D.C.", Mun),
         region_code = ifelse(Mun=="Tegucigalpa M.D.C.", '19', region_code)) |> 
  mutate(mun_name=ifelse(is.na(mun_name), stri_trans_general(Mun, 'latin-ascii'), mun_name)) |> 
  dplyr::select(-mun_code.y)

# check if any mun-code has multiple mun
site_hist3 |> group_by(mun_code) |> summarise(n_distinct(mun_name)) |> view()

site_hist3 |> group_by(mun_code, mun_name) |> summarise() |> view()

# now fix duplicate mun_codes from shape file
site_hist3 <- site_hist3 |> 
  mutate(mun_name = case_when(
    mun_name == 'La Masica' ~ 'Lamasica',
    mun_name == 'Distrito Central' ~ 'Tegucigalpa M.D.C.',
    .default = mun_name
  ))

# total vaccinated at Mun level -------------------------------------------
mun.hist <- site_hist3 |> 
  group_by(Dep, region_code, mun_name, mun_code, Edad, Dos) |> 
  summarise(Num_Doses = sum(Num_Doses, na.rm = T), .groups = 'drop')

mun.vax.mes <- site_hist3 |> 
  group_by(Dep, region_code, mun_name, mun_code, Mes, Edad, Dos) |> 
  summarise(Num_Doses = sum(Num_Doses, na.rm = T), .groups = 'drop')


# since eligible pop data is only available for Distritco Central, use data from worldpop
# combine with vaccination data aggregated from sites

admin2_pop.df <- admin2_shp_pop |>fortify() |> st_drop_geometry() |> 
  rename(Pediátrica=ped_pop, Adulto = adult_pop) |> 
  pivot_longer(`Pediátrica`:Adulto, names_to = 'Edad', values_to = 'world_pop')|> 
  mutate(ADM2_PCODE = ADM2_PCODE |> str_remove_all("^HN")) |> 
  dplyr::select(ADM2_ES, ADM2_PCODE, ADM1_ES, Edad, world_pop)


mun.hist.pop <- mun.hist |> 
  left_join(admin2_pop.df, by=c('mun_code'='ADM2_PCODE', 'Edad'))


# calculate eligible for doses for each Mun
mun.doses <- mun.hist.pop |> 
  pivot_wider(names_from = Dos, values_from = Num_Doses)


mun.doses <- mun.doses |>
  mutate(`2R` = case_when(
    is.na(`2R`) ~ 0,
    .default = `2R`
  )) %>% 
  rowwise() |> 
  mutate(eligible_4doses = (world_pop-`1ra`)*4, #people who have received 0 doses can get 4 doses in the future
         eligible_3doses = (`1ra` - `2da`)*3,  # people who have received 1 dose and can get 3 more
         eligible_2doses = (`2da`-`1R`)*2, # people who have received 2 doses and can get 2 more
         eligible_1dose = (`1R`-`2R`), # people who have received 3 doses and can get 1 more
         eligible_atleast_1dose = world_pop-`2R`) #no. of people who can get 1 or more doses


# just use - eligible_atleast_1dose for now - others have too many negative numbers. Verify later with country team

saveRDS(mun.doses, "data/mun_doses_needed.rds")


# calculate avg vax rate by Mun -----------------------------------------------

# check if each municipality has last 3 months of 2022 data - October - December
mun.vax.mes |> filter(Mes %in% c("Octubre","Noviembre", "Diciembre")) |> 
  distinct(mun_name, Mes) |> 
  count(mun_name) |> 
  view()

# Not All Mun have data for last 3 months of 2022

# convert month column to date
mun.vax.mes$month_date <- paste("2022",mun.vax.mes$Mes, "1", sep = "-")

# parse Spanish month labels using clock package
mun.vax.mes$month_date <- clock::date_parse(x = mun.vax.mes$month_date, 
                                               format = "%Y-%B-%d", 
                                               locale = clock_locale(labels = 'es'))

# convert first day of month to last day of the month
mun.vax.mes$month_date <- mun.vax.mes$month_date %m+% months(1)-days(1)

# create total dose by month
tot_doses <- mun.vax.mes |> 
  reframe(Num_Doses = sum(Num_Doses), .by = c(Dep, region_code,mun_name, mun_code, Mes, month_date)) |> 
  mutate(Edad = "Adult+Peds", Dos = "Total")

Municipio_Hist2 = rbind(mun.vax.mes, tot_doses)

ggplot(data = Municipio_Hist2, aes(x=(month_date), y=Num_Doses))+
  geom_line(aes(color = Mun))+
  geom_point()+
  scale_x_date(date_labels = "%b", date_breaks = "1 month")+
  theme_minimal()+
  theme(legend.position = "none")+facet_wrap(vars(Dos))

# create average vaccination rates over last 3 months ---------
# last 3 months because vaccination rates decline as more people are vaccinated and as time passes (decrease demand)

mun_avg_vax_rate = mun.vax.mes |> 
  group_by(mun_name, mun_code) |> 
  # filter data for last 3 months from the data
  filter(month_date >= (max(month_date)%m+% -months(3)))|> 
  summarise(avg.mon.vax.rate = round(mean(Num_Doses, na.rm=TRUE),0), .groups = 'drop') |> 
  mutate(avg.daily.rate = round(avg.mon.vax.rate/30,2))

saveRDS(mun_avg_vax_rate, 'data/mun_avg_vax_rate.rds')

# add average dose to mun.doses files



# create warehouse network data

warehouses <- readxl::read_xlsx("data/travel time matrix between warehouses.xlsx", sheet = 1)
