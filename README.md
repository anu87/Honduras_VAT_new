# **Honduras Vaccine Allocation Tool (Honduras_VAT)**

## Instructions on setting up the repository

-   Create a folder named ***data*** inside the repository

-   Save these excel files in the `data/` folder: `Eligible population per ES RSMDC.xlsx` ; `Establecimientos de salud longitud y latitud.xlsx` ; `Total dosis aplicadas contra la COVID_2021-2022 Dep_Mun_ES.xlsx` ; `travel time matrix between warehouses.xlsx`

-   Download shapefile named ***hnd_adm_sinit_20161005_SHP.zipSHP (46.7M)*** from <https://data.humdata.org/dataset/cod-ab-hnd?>

-   Unzip ***hnd_adm_sinit_20161005_shp.zip*** and save the shapefiles in `data/hnd_adm_sinit_20161005_shp/` folder

### Steps to create input data for the model:

-   Clean up site level historical vaccination data so that it is in Tidy format - `Honduras_Site_Prep.R`

-   Aggregate vaccination data up to municipality level (level 2 administrative unit)

-   Calculate average monthly vaccination rate over the last 3 months for each municipality

-   Calculate adult population eligible for dose 1,2, booster 1, booster 2, bivalent dose 1 at municipality level

-   We calculate number of doses that can be administered based on vaccination policy instead of people eligible for vaccination. So, any adult who has only received first dose - is eligible for 3 more doses.

-   Adults eligible for 4 doses (2+2 boosters) = tot pop - 1ra; adults eligible for 3 doses = 1ra - 2da; adults eligible for 2 doses = 2da - 1R; adults eligible for 1 dose = 1R - 2R; adults eligible for bivalent = 2R

-   Pediatrics eligible for 3 doses of Pfizer = tot pop - 1ra; pediatrics eligible for 2 doses = 1ra - 2da; pediatrics eligible for 1 booster = 2da-R1

-   Counting doses to allocate instead of eligible population - problem here is one might become eligible for a 2nd dose in 2/3 months which may be out of the current allocation cycle

-   Nomenclature: 1ra: first dose; 2da; second dose; 1R or R1: first booster; 2R or R2: second booster

-   Calculate pediatric population eligible for dose 1-3 of pfzier at municipality level

-   Create a file combining distribution network, municipality eligible populations

###### To do (next steps for app):

-   add time series graph for municipality level vaccination rate

-   Download WorldPop age-sex data and aggregate population eligible by peds and adults up to municipality level (this is being done because we currently do not have catchment area population for all health care facilities): <https://hub.worldpop.org/geodata/summary?id=16782>

-   Every municipality can get vaccines from `Almacén Naciónal de Biológicos` since all warehouse get vaccines from here. But in the model mun should first exhaust vaccines from regional warehouse before getting vaccines from National depot.

-   Assumption: Each Municipality is connected to the warehouse in its region/Department
