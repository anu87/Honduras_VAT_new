library(dplyr)
# 
# elig_pop_ped <- readxl::read_xlsx("data/Eligible population per ES RSMDC.xlsx", 
#                                   sheet = "Pediátrica x dosis")
# 
# elig_pop_ped$pop_vax_eligible <- dplyr::case_when(is.na(elig_pop_ped$population) ~ 0,
#                                                   elig_pop_ped$population >= elig_pop_ped$`total first doses` ~ (elig_pop_ped$population - elig_pop_ped$`total first reinforcement`),
#                                                   elig_pop_ped$`total first doses` > elig_pop_ped$population ~ (elig_pop_ped$`total first doses` - elig_pop_ped$`total first reinforcement`))
# 
# elig_pop_ad <- readxl::read_xlsx("data/Eligible population per ES RSMDC.xlsx",
#                                  sheet = "Adultos x dosis")
# elig_pop_ad$pop_vax_eligible <- dplyr::case_when(is.na(elig_pop_ad$population) ~ 0,
#                                                  elig_pop_ad$population >= elig_pop_ad$`total first doses` ~ (elig_pop_ad$population - elig_pop_ad$`total first reinforcement`),
#                                                  elig_pop_ad$`total first doses` > elig_pop_ad$population ~ (elig_pop_ad$`total first doses` - elig_pop_ad$`total first reinforcement`))
# 
# elig_pop_joined <- left_join(elig_pop_ad, elig_pop_ped, suffix = c("_ad", "_ped"),
#                              by = c("Region", "ES"))
# 
# writexl::write_xlsx(elig_pop_joined, "Elig_Pop_Joined.xlsx")

