library(shiny)
library(shinydashboard)
library(shiny.i18n)
library(bslib)
library(shinyjs)
library(rhandsontable)
library(leaflet)
library(RColorBrewer)
library(sf)
library(geosphere)
library(lubridate)
library(DT)
library(dplyr)
library(lpSolveAPI)
library(lpSolve)
library(tibble)
library(MASS)
library(readr)
library(stringr)
library(stringi)
library(readxl)
library(writexl)
library(zoo)
library(tidyverse)
library(stringi)

#setwd("~/Honduras_VAT/Honduras_VAT")



#Load new translator ---------
i18n <- Translator$new(translation_csvs_path = "translations")
i18n$set_translation_language("en")
i18n$use_js()

#Theme content --> source in from other script --------
#Make pretty at some point
old_flag_col <- "#0d3b99"
new_flag_col <- "#00bce4" 
font <- font_google("Roboto")

honduras_theme <- bs_theme(
  bg = "white",
  fg = new_flag_col,
  primary = old_flag_col,
  base_font = font,
  code_font = font
)


#Data load -------
# > temp data until final version
#stock_data <- readRDS("data/")
vax_sites <- readRDS("appdata/Sites_w_Coords.rds")
historic_site_data <- readRDS("appdata/Site_Hist.rds")
historic_mun_data <- readRDS("appdata/joined_adm2.rds")
historic_dep_data <- readRDS("appdata/joined_adm1.rds")


joined_sites <- readRDS("appdata/joined_sites.rds")
#low_leaflet <- readRDS("appdata/flow_leaflet.rds")
connections <- readxl::read_xlsx("appdata/travel time matrix between warehouses.xlsx", sheet = 2)

# salmi_data <- readxl::read_xlsx("appdata/SALMI HON Inventario Biologicos_2022-01-13. Vaccine Inventory .xlsx")
# salmi_data <- salmi_data[,1:7]
# colnames(salmi_data) <- salmi_data[3,]
# salmi_data <- salmi_data[-c(1:3),]

salmi_data <- read_rds("appdata/salmi_inventory2.rds")
salmi_data <- salmi_data %>% 
  dplyr::mutate(time_to_exp = as.numeric(time_to_exp))


#Read files relevant to regenerating optimization
# site level historical vaccination data
site_vax <- readRDS("data/Site_Hist.rds")
Municipio_Hist <- readRDS("data/Municipio_Hist.rds")
Mun_shp_vax <- readRDS("data/joined_adm2.rds")
vax_network_codes <- readRDS('data/site_mun_dep_codes.rds')
site_hist <- readRDS("data/Site_Hist.rds")
mun_avg_vax_rate <- readRDS('data/mun_avg_vax_rate.rds')

connections2 <- readRDS("appdata/connections.rds") 

#net_to_munis <- read_rds("appdata/full_net_leaflet.rds")


adm2 <- read_rds("appdata/Adm2_w_Pops.rds")


mun.doses <- readRDS("data/mun_doses_needed.rds")
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



full_info <- read_rds("appdata/full_info.rds")
warehouse_codes <- full_info %>%
  dplyr::select(dep_clean, warehouse_code) %>%
  distinct() %>%
  arrange(warehouse_code)


#adm2 <- st_simplify(adm2, dTolerance = 100) #Improve runtime
#saveRDS(adm2, "appdata/adm2_w_pops.rds")

# source model function ---------------------------------------------------
source("vat_scripts/model_app.R")
source("vat_scripts/child_model_app.R")


# ui ----------------------------------------------------------------------


ui <- fluidPage(tagList(shiny.i18n::usei18n(i18n)),
                navbarPage(title = div(selectInput(inputId = "selected_language", 
                                                   label = "",#i18n$t("Select language"),
                                                   choices = i18n$get_languages(), 
                                                   selected = "es"#i18n$get_key_translation()
                ),
                style = "width:75px; position:fixed; right:40px; top:-20px"),#title =i18n$t("Vaccine Allocation Tool"),
                theme = honduras_theme,
                #position = "fixed-top",
                #Home Page ---------
                tabPanel(i18n$t("Home"),
                         shiny.i18n::usei18n(i18n),
                         fluidPage(
                           fluidRow(
                             #tags$br(),
                             h1(strong(i18n$t("Honduras Vaccine Allocation Tool")), align = "center"),
                             tags$br(),
                             #  h2(strong(i18n$t("Data Overview")), align = "center"),
                             tags$br(),
                             tags$br(),
                             h3(i18n$t("The vaccine allocation tool is an interactive tool that takes in data from multiple sources, optimizes for various supply and demand-side constraints and generates a recommended vaccine allocation per site. The final goal of this tool is to maximize vaccine coverage in priority population and minimize any vaccine wastage."), align = "center"),
                             tags$br(),
                             tags$br(),
                             h4(i18n$t("This dashboard was developed in collaboration with xyz. It is intended to walk through data inputs and methodology for the vaccine allocation tool, and facilitate the collection of feedback from stakeholders."), align = "center"),
                             tags$br(),
                             # column(width = 6, 
                             #   #     h5(i18n$t("")),
                             #        selectInput(inputId = "selected_language", 
                             #                    label = i18n$t("Select language"),
                             #                    choices = i18n$get_languages(), 
                             #                    selected = i18n$get_key_translation()),
                             #        offset = 0.5),
                             tags$br(),
                             tags$br(),
                             tags$br() # breaks here for page spacing
                           )
                         )
                ),
                
                
                #Vaccine Stock Page------------
                tabPanel(title = i18n$t("Vaccine Stock Data"),
                         shiny.i18n::usei18n(i18n),
                         fluidPage(
                           fluidRow(
                             h1(strong(i18n$t("Vaccine Stock Data")), align="center")
                           ),
                           box(
                             width = 12,
                             h5(i18n$t("Edit the table to change stock input data")),
                             status = "warning", solidHeader = TRUE, collapsible = FALSE,
                             tags$br(), tags$br(),
                             rHandsontableOutput("vax_stock_rhot"),
                             #DT::DTOutput("vax_stock_rhot"), #Temp replacement
                             tags$br(), tags$br(),
                             #h5(i18n$t("Once you have updated the table above, please save your changes.")),
                             #tags$br(),
                             actionButton("update_vax_stock_rhot", i18n$t("Save")),
                             actionButton("reset_vax_stock_rhot", i18n$t("Reset")),
                             tags$br(), tags$br(),
                             fileInput(inputId = "upload_new_vax_stock_data", label = NULL, 
                                       buttonLabel = i18n$t("Upload New Data"), 
                                       placeholder = "Ningún fichero seleccionado"),#i18n$t("No file selected")), #Weird issue parsing the translator object
                             tags$br(), tags$br(),
                             column(width = 8,
                                    hidden(
                                      div(id='vax_stock_update_message', 
                                          verbatimTextOutput("vax_stock_update_text"))),
                                    tags$br(), tags$br()
                             )
                           ))
                ),
                
                
                #Distribution Network Page-------------
                tabPanel(title = i18n$t("Distribution Network"),
                         fluidPage(
                           fluidRow(
                             h1(strong(i18n$t("Distribution Network: Maps of distribution centers and vaccination sites")), align="center")
                           ),
                           br(),
                           fluidRow( 
                             box(width = 6,
                                 status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                 p(i18n$t("SALMI Depot to Regional Distribution Network"), style="text-align:center;font-weight:bold"),
                                 leafletOutput("full_network", height = 600)),
                             box(width = 6,
                                 status = "warning", solidHeader = TRUE, collapsible = FALSE,
                                 p(i18n$t("Regional Warehouses to Municipalities"), style="text-align:center;font-weight:bold"),
                                 leafletOutput("full_network_to_munis", height = 600))
                             ),
                           fluidRow(
                             box(width = 12,
                                 status = "warning", solidHeader = T, collapsible = F,
                                 p(i18n$t("Vaccination Sites"),style="text-align:center;font-weight:bold"),
                                 leafletOutput("vax_sites", height = 600)
                             ))
                         )
                ),
                
                #Proposed Distribution Page -------------
                tabPanel(title = i18n$t("Proposed Distribution -- Adults"),
                         fluidPage(
                           fluidRow(
                             h1(strong(i18n$t("Construct the Optimization Model")), align = "center")),
                           br(), br(),
                           fluidRow(
                             column(6,
                                    p(strong(i18n$t("Allocation Duration"), style="font-size:25px; font-weight:bold")),
                                    p(i18n$t("How many days of vaccine supply do you want to allocate? E.g. the next 30 days or the next 60 days")),
                                    numericInput("days_allocated_input", i18n$t("Number of Days:"), value = 30, min = 1, max = 365)
                             ),
                             
                             column(6,
                                    p(strong(i18n$t("Run Model and View Proposed Distribution"), style="font-size:25px; font-weight:bold")), 
                                    p(i18n$t("Please press the button below to run the vaccine allocation model.")),
                                    p(i18n$t("The model may take a few minutes to run. The maps will display once the model is done running.")),
                                    actionButton("run_model", i18n$t("Run Model")),
                                    tags$br(), tags$br(),
                                    hidden(
                                      div(id='run_model_message', verbatimTextOutput("run_model_text")))
                             )
                           ),
                           tags$br(), tags$br(),
                           box(width = 12,
                               leafletOutput("dist_map", height = 500)
                           ),
                           tags$br(), tags$br(),

                           box(width = 12,
                               p(strong(i18n$t("Vaccine Allocation proposed by the model"), style = "font-size:25px;font-weight:bold")), 
                               p(i18n$t("The below map shows the allocation of vaccines from the distribution centers to the vaccination sites.")),
                               status = "warning", solidHeader = TRUE, collapsible = FALSE,
                               #leafletOutput("prop_dist", height = 600),
                               DTOutput("prop_dist_dt")
                           ),
                           box(width = 12,
                               downloadButton("vax_allocation_download", label = i18n$t("Download")))
                           # ,
                           # box(
                           #   title = i18n$t("Proposed Allocation with District Filter"), width = 12,
                           #   h5(i18n$t("If you would like to look at one district at a time, use the filter below.")),
                           #   selectInput("district_selection", i18n$t("Select the District:"), c("Placeholders", "List", "For", "Now")),
                           #   status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           #   leafletOutput("prop_dist_filt", height = 600)
                           # )
                         )
                ),
                
                #Child Vax Distribution Page -------------
                tabPanel(title = i18n$t("Proposed Distribution -- Children"),
                         fluidPage(
                           fluidRow(
                             h1(strong(i18n$t("Construct the Optimization Model")), align = "center")),
                           br(), br(),
                           fluidRow(
                             column(6,
                                    p(strong(i18n$t("Allocation Duration"), style="font-size:25px; font-weight:bold")),
                                    p(i18n$t("How many days of vaccine supply do you want to allocate? E.g. the next 30 days or the next 60 days")),
                                    numericInput("child_days_allocated_input", i18n$t("Number of Days:"), value = 30, min = 1, max = 365)
                             ),
                             
                             column(6,
                                    p(strong(i18n$t("Run Model and View Proposed Distribution"), style="font-size:25px; font-weight:bold")), 
                                    p(i18n$t("Please press the button below to run the vaccine allocation model.")),
                                    p(i18n$t("The model may take a few minutes to run. The maps will display once the model is done running.")),
                                    actionButton("child_run_model", i18n$t("Run Model")),
                                    tags$br(), tags$br(),
                                    hidden(
                                      div(id='child_run_model_message', verbatimTextOutput("child_run_model_text")))
                             )
                           ),
                           tags$br(), tags$br(),
                           box(width = 12,
                               leafletOutput("child_dist_map", height = 500)
                           ),
                           tags$br(), tags$br(),
                           
                           box(width = 12,
                               p(strong(i18n$t("Vaccine Allocation proposed by the model"), style = "font-size:25px;font-weight:bold")), 
                               p(i18n$t("The below map shows the allocation of vaccines from the distribution centers to the vaccination sites.")),
                               status = "warning", solidHeader = TRUE, collapsible = FALSE,
                               #leafletOutput("prop_dist", height = 600),
                               DTOutput("child_prop_dist_dt"),
                               downloadButton("child_vax_allocation_download", label = i18n$t("Download"))
                           )
                           # ,
                           # box(
                           #   title = i18n$t("Proposed Allocation with District Filter"), width = 12,
                           #   h5(i18n$t("If you would like to look at one district at a time, use the filter below.")),
                           #   selectInput("district_selection", i18n$t("Select the District:"), c("Placeholders", "List", "For", "Now")),
                           #   status = "warning", solidHeader = TRUE, collapsible = FALSE,
                           #   leafletOutput("prop_dist_filt", height = 600)
                           # )
                         )
                ),
                
                #Historic Data Page ----------
                tabPanel(title = i18n$t("Historic Data"),
                         fluidPage(
                           fluidRow(
                             h1(strong(i18n$t("Historic Data")), align = "center")),
                           br(), br(),
                           fluidRow(
                             column(3,
                                    #p(strong(i18n$t(""))),
                                    sliderInput("historic_slider", i18n$t("Select the Month:"), 
                                                min = my("01-2021"), max = my("12-2021"), value = my("01-2021"), 
                                                timeFormat = "%m-%y"),
                                    selectInput("age_variable_select", label = i18n$t("Select the Age:"),
                                                choices = c("Adulto", "Pediátrica")),#, "Total")),
                                    selectInput("dose_variable_select", label = i18n$t("Select the Dose:"),
                                                choices = c("1R", "R1", "1ra", "2R", "R2", "2da"))
                             ),
                             column(9,
                                    leafletOutput("historic_data_tmap"),
                                    DTOutput("historic_data_dt")
                             )
                           )
                         )
                ),
                
                #Model Documentation Page 
                tabPanel(title = i18n$t("Model Documentation"),
                         tags$h2(i18n$t("This page contains documentation of the methodology used in the vaccine allocation process.")),
                         tags$br(),
                         tags$h4(i18n$t("The model currently uses the following constraints to allocate vaccines. The constraints on the allocation models for child and adult doses are identical:")),
                         tags$br(),
                         tags$ol(
                           tags$li(i18n$t("Eligible population served by vaccination site: a vaccination site cannot be given more vaccines than the number of people (either adults or children for the respective models) living in its catchment area who have not been vaccinated.")),
                           tags$br(),
                           tags$li(i18n$t("Vaccine administration capacity: a vaccination site cannot receive more vaccines than it can administer, which is calculated by multiplying the days until expiration by the average vaccination capacity per day. Average vaccination capacity per day was calculated using historical vaccination data for each site.")),
                           tags$ul(
                             tags$li(i18n$t("Eligible population for each site was calculated using high resolution modeled population data from WorldPop in conjunction with SALMI-provided historical vaccination data. In order to ensure the optimization algorithm is able to calculate a solution for most time windows and in order to better reflect reality, sites with a minimum eligible population below a certain threshold had their minimum eligible populations increased."))
                           ),
                           tags$br(),
                           tags$li(i18n$t("Batch size: the amount of vaccines distributed must be less than the number of vaccines available in each batch.")),
                           tags$br(),
                           tags$li(i18n$t("Vaccine allocation should be distributed evenly across vaccination sites: all vaccination sites should receive vaccines for at least a certain percentage of the population they serve."))
                ))
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Translate menu
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
  
  
  # network map ----------
  output$full_network <- renderLeaflet({
    
    # remove rows where origin and destination warehouse are the same
    connections <- connections |> 
      filter(if_all(`origin warehouse`, ~.x != `destination warehouse`))
    
    
    
    names(connections)
    flows <- gcIntermediate(connections[,c("Latitude1", "Longitude1")], 
                            connections[,c("Latitude2", "Longitude2")],
                            sp = T,
                            addStartEnd = T)
    flows$Hours <- connections$Hours
    flows$origins <- connections$`origin warehouse`
    flows$destinations <- connections$`destination warehouse`
    
    
    hover <- paste0(flows$origins, " a ", 
                    flows$destinations, ': ')
    
    pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)
    
    origin_wh <-  connections |> 
      dplyr::select(`origin warehouse`, Longitude1, Latitude1) |> 
      st_as_sf(coords = c("Latitude1", "Longitude1"))
    
    dest_wh <-  connections |> 
      filter(!(`destination warehouse` %in% `origin warehouse`)) |> 
      dplyr::select(`destination warehouse`, Longitude2, Latitude2) |> 
      st_as_sf(coords = c("Latitude2", "Longitude2"))
    
    flow_leaflet <-leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolylines(data = flows, 
                   # label = hover,
                   group = ~origins, color = ~pal(origins)) %>%
      addCircleMarkers(data = origin_wh, radius = 2, label = ~as.character(`origin warehouse`)) |>
      addCircleMarkers(data = dest_wh, radius = 1, color = 'red', label = ~as.character(`destination warehouse`)) #|>
      # addLayersControl(overlayGroups = unique(flows$origins),
      #                  options = layersControlOptions(collapsed = T))
    
    flow_leaflet
    
  })
  
  output$full_network_to_munis <- renderLeaflet({

    connections2 <- connections2 %>%
      filter(warehouse_code != "A")

    flows <- gcIntermediate(connections2[,c("lon1", "lat1")], 
                            connections2[,c("lon2", "lat2")],
                            sp = T,
                            addStartEnd = T)
    
    
    flows$origins <- connections2$mun_name
    flows$destinations <- connections2$dep_clean
    
    
    hover <- paste0(flows$origins, " a ", 
                    flows$destinations, ': ')
    
    pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)
    
    origin_wh <-  connections2 |> 
      dplyr::select(dep_clean, lon2, lat2) |> 
      st_as_sf(coords = c("lon2", "lat2"))
    
    dest_wh <-  connections2 |> 
      dplyr::select(mun_name, lon1, lat1) |> 
      st_as_sf(coords = c("lon1", "lat1"))
    
    out_leaf <- leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolylines(data = flows, 
                   # label = hover,
                   group = ~origins, color = ~pal(origins)) %>%
      addCircleMarkers(data = origin_wh, radius = 1.5, label = ~as.character(dep_clean)) |>
      addCircleMarkers(data = dest_wh, radius = 0.75, color = 'red', label = ~as.character(mun_name)) #|>
      # addLayersControl(overlayGroups = unique(flows$origins),
      #                  options = layersControlOptions(collapsed = T))
    
    out_leaf

  })
  
  #Vax sites------
  output$vax_sites <- renderLeaflet({
    
    leaflet(vax_sites) %>%
      addProviderTiles("OpenStreetMap") %>%
      leaflet::addCircles(color = "black",
                          fillOpacity = 1,
                          radius = 500,
                          stroke = F)
  })
  
  #Reactive value to store updated salmi data
  salmi_data_updated <- reactiveValues(dat = salmi_data)
  
  #Update table with new data
  observeEvent(input$upload_new_vax_stock_data, {
    uploaded_data <- readxl::read_xlsx(input$upload_new_vax_stock_data$datapath)
    print(uploaded_data)
    
    #Add check to make sure data won't break the app
    if(names(uploaded_data) == names(salmi_data) & all(uploaded_data$Dep %in% warehouse_codes$dep_clean) & all(is.numeric(uploaded_data$Cantidad))) {
      salmi_data_updated$dat <- uploaded_data
    }
    
  })
  
  # #RHOT
  output$vax_stock_rhot <- renderRHandsontable({
    
    rhot_data <- salmi_data_updated$dat %>%
      dplyr::mutate(exp_date = as.character(exp_date))
    
    rhandsontable(rhot_data, readOnly = F) %>%
      hot_col("exp_date", dateFormat = "YYYY-MM-DD", type = "date")
  })
  
  #Update reactive with changes to RHOT

  observeEvent(input$vax_stock_rhot, {

    rhot_dat <- hot_to_r(input$vax_stock_rhot) %>%
      dplyr::mutate(exp_date = as.Date(exp_date)) %>%
      dplyr::mutate(time_to_exp  = as.numeric(exp_date - Sys.Date()))

    
    if(all(rhot_dat$Dep %in% warehouse_codes$dep_clean) & all(is.numeric(rhot_dat$Cantidad))) {
      salmi_data_updated$dat <- rhot_dat
    }
    
  })
  
  #Historic-----
  output$historic_data_dt <- DT::renderDataTable({
    DT::datatable(historic_site_data)
  })
  
  
  selected_col <- reactive({
    date <- input$historic_slider #need to make Spanish --> manually i guess
    form_date <- ymd(date) #Format to lubridate
    mo <- month(form_date)
    final_month <- dplyr::case_when(mo == 1 ~ "Enero",
                                    mo == 2 ~ "Febrero",
                                    mo == 3 ~ "Marzo",
                                    mo == 4 ~ "Abril",
                                    mo == 5 ~ "Mayo",
                                    mo == 6 ~ "Junio",
                                    mo == 7 ~ "Julio",
                                    mo == 8 ~ "Agosto",
                                    mo == 9 ~ "Septiembre",
                                    mo == 10 ~ "Octubre",
                                    mo == 11 ~ "Noviembre",
                                    mo == 12 ~ "Diciembre")
    #Add year here when that becomes relevant
    
    age <- input$age_variable_select #need to do total 
    if(age == "Total") {
      final_age <- paste0("Total ", final_month)
    } else {
      final_age <- age
    }
    
    dose <- input$dose_variable_select
    
    out <- paste(final_month, final_age, dose, sep = "_")
    
    return(out)
  })
  # VAT model ---------------------------------------------------------------
  # save days allocated
  days_allocated_value <- eventReactive(input$run_model, {
    days_allocated_value <- input$days_allocated_input
  })
  
  # run the model
  allocation <- eventReactive(input$run_model, {
    
    updated_inventory <- salmi_data_updated$dat #Easier than renaming all the vars
    print(updated_inventory)
    days_allocated_value <- days_allocated_value()
    
    print("Starting")
    #Regenerate the inputs if the warehouse data changed at all --> else use defaults
    if(isTruthy(updated_inventory)) { #Change to check if warehouse data changed or not 

      # datasets to create------
      # add average vax data to mun file; anywhere daily average is < 1, make it 5 (mean) - so that they atleast get some vaccines
      # 
      
      salmi_inventory3 <- left_join(updated_inventory, warehouse_codes, by = c("Dep" = "dep_clean")) %>%
        dplyr::filter(category == "vaccine")
      
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
      
      
      #IK FLAG --> NEED TO FIGURE OUT WAY TO RUN FASTER
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
      
      
      adults_inventory_data <- adults_inventory_data |> 
        group_by(Almacen, `U. de Emisión`,`Nº de Lote`, `Fecha Vto`,exp_date, time_to_exp, 
                 vax_type, category,warehouse_code) |> 
        summarise(Cantidad = sum(Cantidad), .groups = 'drop')
      
      # data for adults vax allocation ------------------------------------------
      
      master_key_adult$dose_quantity <- master_key_adult$Cantidad*6
      days_to_allocate = days_allocated_value
      
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
      
      # fix too low eligible cases ----------------------------------------------
      
      master_key_adult <- master_key_adult %>% 
        mutate(vax_admin_const = case_when(
          vax_admin_const > eligible_atleast_1dose ~ eligible_atleast_1dose,
          .default = vax_admin_const
        ))
      
      
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
    } 
    
    # source the model script
    allocation <- vat.model(days_allocated = days_allocated_value,
                            salmi_inventory2 = updated_inventory)
    
    allocation <- allocation %>% 
      relocate(mun_name, .after = mun_code) %>% 
      relocate(region_name, .after = mun_name) #%>% 
      # relocate(Almacen, .after = region_name) %>% 
      # relocate(Suministro, .after = Almacen)
    saveRDS(allocation, "appdata/allocation.rds")
    
    #IK FLAG STOPPING POINT 8/16
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
    
    #I think this is fixed by adding Choluteca/Ocotepeque 
    #NA ones --> only supplied by national (removed externos from data for now so that's probably why)
    # remaining_allocs <- rbind(remaining_allocs, regional_allocs[which(is.na(index_matches)),])
    # regional_allocs <- regional_allocs[-which(is.na(index_matches)),]
    # index_matches <- match(regional_allocs$mun_code,connections_key$mun_code)
    
    #replace almacen, warehouse code, dep
    regional_allocs$warehouse_code <- connections_key$warehouse_code[index_matches]
    regional_allocs$dep_clean <- connections_key$dep_clean[index_matches]
    
    #Rejoin with the allocation table --> replace original rows and then output nat'l to regional as another element in list output?
    remaining_allocs <- rbind(remaining_allocs, regional_allocs)
    
    clean_allocs <- remaining_allocs %>% 
      dplyr::select(dep_clean, mun_name, batch_num, vax_allocated) %>%
      dplyr::rename(Origin = dep_clean, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
      dplyr::mutate(Distribution = "Final")
    
    natl_allocs <- regional_allocs %>%#Need two of these --> 1 for allocs from natl to regional 1 from regional to muni
      dplyr::select(dep_clean, mun_name, batch_num, vax_allocated) %>%
      dplyr::rename(Origin = dep_clean, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
      dplyr::mutate(Destination = Origin,
                    Origin = "Almacen Nacional") %>%
      dplyr::group_by(Origin, Destination, `Batch Num`) %>%
      dplyr::summarise(Allocated = sum(Allocated)) %>%
      ungroup() %>%
      dplyr::mutate(Distribution = "Intermediate")
    
    clean_allocs <- rbind(clean_allocs, natl_allocs) %>%
      dplyr::arrange(Origin, Destination, `Batch Num`)
    
    #Create output list for user --> first one has all final distribution legs, second one has all intermediate, third one is clean for data table --> download all of them?
    allocation_list <- list(remaining_allocs, natl_allocs, clean_allocs)
    names(allocation_list) <- c("final", "intermediate", "clean")
    
    allocation_list
  })
  
  
  
  # display the model finished message-------
  
  proposed_distribution <- reactiveValues(dt = DT::datatable(salmi_data))
  
  observeEvent(input$run_model, {
    toggle('run_model_message')
    
    numdays <- input$days_allocated_input
    
    #proposed_distribution$dt <- proposed_dist
    
    output$run_model_text <- renderText({
      i18n$t("The model has finished generating outputs. You can view the model outputs below and download them.")
    })
  })
  
  # #Reactive object for natl to regional distribution (allocation() is for warehouse to muni need warehouse to warehouse) 
  # nat_to_reg_allocation <- reactive({
  #   allocation <- allocation()
  #   
  # })
  
  # model map-----
  output$dist_map <-  renderLeaflet({
    allocation_list <- allocation()
    
    allocation <- allocation_list$final
    inter_alloc <- allocation_list$intermediate
    
    if(isTruthy(allocation)) {
      allocation$key <- paste(allocation$dep_clean, allocation$mun_code, sep = "_")
      connections2$key <- paste(connections2$dep_clean, connections2$mun_code, sep = "_")
      
      allocation_joined <- left_join(allocation, connections2, by = "key") %>% as.tibble()
      
      #Aggregate allocation by routes (multiple batches along one route currently not shown)
      allocation_agg <- allocation_joined %>%
        dplyr::group_by(dep_clean.x, mun_name.x, lon1, lat1, lon2, lat2) %>%
        dplyr::summarise(vax_allocated = sum(vax_allocated),
                         batch_num = paste(batch_num, collapse = ", "))
      
      flows2 <- gcIntermediate(allocation_agg[,c("lon1", "lat1")], 
                               allocation_agg[,c("lon2", "lat2")],
                               sp = T,
                               addStartEnd = T)
      
      flows2$Origin <- allocation_agg$dep_clean.x
      flows2$Destination <- allocation_agg$mun_name.x
      #flows2$Suministro <- allocation_joined$Suministro
      flows2$Batch_Num <- allocation_agg$batch_num
      #flows2$Time_to_Exp <- allocation_joined$time_to_exp
      flows2$Vax_Allocated <- allocation_agg$vax_allocated
      
      label_list <- list()
      popup_list <- list()
      for(i in 1:nrow(flows2)) {
        label_list[[i]] <- paste0(str_to_title(flows2$Origin[i]), " a ", flows2$Destination[i])
        popup_list[[i]] <- paste(glue::glue("<b>{str_to_title(flows2$Origin[i])} a {flows2$Destination[i]}</b>"),  
                                 #glue::glue("<i>Suministro</i>: {flows2$Suministro[i]}"),
                                 glue::glue("<i>Número de lote</i>: {flows2$Batch_Num[i]}"),
                                 glue::glue("<i>Vacunas Asignadas</i>: {flows2$Vax_Allocated[i]}"),
                                 sep = "</br>")
      }
      
      pal <- colorFactor(brewer.pal(4, "Set2"), flows2$Origin)
      
      origin_almacen <-  connections2 |> 
        dplyr::select(dep_clean, lon2, lat2) |> 
        st_as_sf(coords = c("lon2", "lat2"))
      
      dest_mun <-  connections2 |> 
        dplyr::select(mun_name, lon1, lat1) |> 
        st_as_sf(coords = c("lon1", "lat1"))
      
      #Remove central to muni --> FLAG TEMP SOLUTION TILL MODEL FIXED
      #flows2 <- flows2[flows2$Origin != "ALMACEN NACIONAL",]
      
      out_leaf <- leaflet() %>%
        addProviderTiles("OpenStreetMap") %>%
        addPolylines(data = flows2, 
                     label = label_list,
                     popup = popup_list,
                     group = ~Origin, color = ~pal(Origin)) %>%
        addCircleMarkers(data = origin_almacen, radius = 1.5, label = ~as.character(dep_clean)) |>
        addCircleMarkers(data = dest_mun, radius = 0.75, color = 'red', label = ~as.character(mun_name)) |>
        # addLayersControl(overlayGroups = unique(flows2$origins),
        #                  options = layersControlOptions(collapsed = T))%>%
        addPolygons(data = adm2,
                    stroke = T,
                    color = "black",
                    weight = 1,
                    fill = F,
                    opacity = 1)
    }
    
    if(isTruthy(inter_alloc)) {
      #Join warehouse coords --> I'm sorry Hadley Wickham
      inter_alloc[,c("lon1", "lat1")] <- connections2[match(inter_alloc$Origin,connections2$dep_clean),c("lon2", "lat2")]
      inter_alloc[,c("lon2", "lat2")] <- connections2[match(inter_alloc$Destination,connections2$dep_clean),c("lon2", "lat2")]
      
      #Aggregate intermediate allocation by routes (multiple batches along one route currently not shown)
      inter_alloc_agg <- inter_alloc %>%
        dplyr::group_by(Origin, Destination, lon1, lat1, lon2, lat2) %>%
        dplyr::summarise(Allocated = sum(Allocated),
                         `Batch Num` = paste(`Batch Num`, collapse = ", "))
      
      
      inter_flows <- gcIntermediate(inter_alloc_agg[,c("lon1", "lat1")], 
                                    inter_alloc_agg[,c("lon2", "lat2")],
                                    sp = T,
                                    addStartEnd = T)
      
      inter_flows$Origin <- inter_alloc_agg$Origin
      inter_flows$Destination <- inter_alloc_agg$Destination
      inter_flows$Batch_Num <- inter_alloc_agg$`Batch Num`
      inter_flows$Vax_Allocated <- inter_alloc_agg$Allocated
      
      inter_label_list <- list()
      inter_popup_list <- list()
      for(i in 1:nrow(inter_flows)) {
        inter_label_list[[i]] <- paste0(str_to_title(inter_flows$Origin[i]), " a ", inter_flows$Destination[i])
        inter_popup_list[[i]] <- paste(glue::glue("<b>{str_to_title(inter_flows$Origin[i])} a {inter_flows$Destination[i]}</b>"),  
                                 glue::glue("<i>Número de lote</i>: {inter_flows$Batch_Num[i]}"),
                                 glue::glue("<i>Vacunas Asignadas</i>: {inter_flows$Vax_Allocated[i]}"),
                                 sep = "</br>")
      }
      
      
      inter_origin_almacen <-  inter_alloc_agg |> 
        dplyr::select(Origin, lon1, lat1) |> 
        st_as_sf(coords = c("lon1", "lat1"))
      
      inter_dest_almacen <-  inter_alloc_agg |> 
        dplyr::select(Destination, lon2, lat2) |> 
        st_as_sf(coords = c("lon2", "lat2"))
      
      
      out_leaf <- out_leaf %>%
        addPolylines(data = inter_flows, 
                     label = inter_label_list,
                     popup = inter_popup_list,
                     group = ~Origin, color = "Grey") %>%
        addCircleMarkers(data = inter_origin_almacen, radius = 1.5, label = ~as.character(Origin)) |>
        addCircleMarkers(data = inter_dest_almacen, radius = 0.75, color = 'red', label = ~as.character(Destination)) 
        # addLayersControl(overlayGroups = unique(inter_flows$Origin),
        #                  options = layersControlOptions(collapsed = T))
      
      
    }
    
    
    out_leaf
  })
  
  # display the allocation table -------
  
  output$prop_dist_dt <- DT::renderDT({
    allocation_list <- allocation()
    
    allocation_list$clean
    
    # allocation() |> 
    #   dplyr::select(mun_code, mun_name, region_name, Almacen, Suministro, batch_num, avg, 
    #                 vax_admin_const, eligible_atleast_1dose,vax_allocated) %>% 
    #   rename(Municipality = mun_name, vax_doses_allocated = vax_allocated)
  })
  
  
  output$vax_allocation_download <- downloadHandler(
    filename = function() {
      paste('Adults_Vax-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      writexl::write_xlsx(allocation(), file)
    }
  )
  
  
  #### Child Vax Model Logic #####
  # VAT model ---------------------------------------------------------------
  # save days allocated
  child_days_allocated_value <- eventReactive(input$child_run_model, {
    child_days_allocated_value <- input$child_days_allocated_input
  })
  
  
  # run the model
  child_allocation <- eventReactive(input$child_run_model, {
    
    updated_inventory <- salmi_data_updated$dat 
    child_days_allocated_value <- child_days_allocated_value()
    #Regenerate the inputs if the warehouse data changed at all --> else use defaults
    
    if(isTruthy(updated_inventory)) { #Change to check if warehouse data changed or not
      
      salmi_inventory3 <- left_join(updated_inventory, warehouse_codes, by = c("Dep" = "dep_clean")) %>%
        dplyr::filter(category == "vaccine")
      
      #Make child data key
      child_inventory_data <- salmi_inventory3 %>%
        dplyr::filter(grepl("peds", vax_type)) 
      
      for(i in 1:length(unique(child_inventory_data$warehouse_code))) {
        this_code <- unique(child_inventory_data$warehouse_code)[i]
        
        filtered_data <- child_inventory_data %>%
          dplyr::filter(warehouse_code == this_code)
        
        #Might need to add another element for exp date or something
        filtered_data$key <- paste0("w", filtered_data$warehouse_code, "_",
                                    filtered_data$batch_num)
        
        add_rows <- filtered_data %>%
          dplyr::select(key, warehouse_code, batch_num, time_to_exp, Cantidad)
        
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
          
          
          muni_avg <- mun.doses %>% 
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
      
      # group by key - to combine different versions of pfizer (purple and gray) - separate later
      master_key_child <- master_key_child |> 
        group_by(mun_code, warehouse_code, batch_num, key, time_to_exp, avg) |> 
        summarise(Cantidad = sum(Cantidad), .groups = 'drop')
      
      child_inventory_data <- child_inventory_data |> 
        group_by(Almacen, `U. de Emisión`,batch_num, `Fecha Vto`,exp_date, time_to_exp, 
                 vax_type, category,warehouse_code) |> 
        summarise(Cantidad = sum(Cantidad), .groups = 'drop')
      
      # data for child vax allocation ------------------------------------------
      master_key_child$dose_quantity <- master_key_child$Cantidad*6
      child_days_to_allocate = child_days_allocated_value
      
      mun.doses.child <- mun.doses |> filter(Edad == 'Pediátrica')
      
      # add doses needed data from mun.doses file
      master_key_child <- master_key_child |> 
        left_join(mun.doses.child |> dplyr::select(mun_code, eligible_atleast_1dose), by='mun_code')
      
      
      # sort master key child on expiration date
      master_key_child <- master_key_child |> 
        group_by(key) |> 
        arrange(time_to_exp, .by_group = TRUE) |> 
        ungroup() 
      
      # save Master_key_child file ---------------------------------------------
      saveRDS(master_key_child, "appdata/master_key_child.rds")
      
      master_key_child$vax_admin_const <- round(master_key_child$avg*child_days_to_allocate, 0)
      
      # fix too low eligible cases ----------------------------------------------
      # cases when the `eligible_atleast_1dose` is < vax_admin_const; change vax_admin_const = eligible_atleast_1dose
      
      master_key_child <- master_key_child %>% 
        mutate(vax_admin_const = case_when(
          vax_admin_const > eligible_atleast_1dose ~ eligible_atleast_1dose,
          .default = vax_admin_const
        ))
      
      
      # lpsolve API -------------------------------------------------------------
      
      # RHS for constraints -----------------------------------------------------
      # 1. municipality population constraint
      mun_pop_const = master_key_child |> 
        distinct(mun_code, eligible_atleast_1dose)
      
      mun_pop_const <- mun_pop_const$eligible_atleast_1dose |> unlist()
      
      # 2. municipality vaccine administration constraint
      mun_admin_const = master_key_child |> 
        distinct(mun_code, vax_admin_const)
      
      mun_admin_const <- mun_admin_const$vax_admin_const |> unlist()
      
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
      
      # LHS constraint matrices -------------------------------------------------
      # 1. municipality by by batch-warehouse matrix for population constraint
      # 2. municipality by batch-warehouse matrix for administrative constraint
      # 3. municipality matrix by batch-warehouse for min dose allocation -  10% of admin constraint
      # use the same mun_list for all 3
      
      # create mun level constraint - each list is a set of coeff for each mun --> row codes for that mun
      mun_mat <- master_key_child |> distinct(mun_code)
      row.names(master_key_child) <- rownames(master_key_child$key)
      
      master_key_child2 <- rownames_to_column(master_key_child, 'row.code') 
      
      mun_list <- list()
      
      for(i in 1:nrow(mun_mat)){
        
        code <- mun_mat$mun_code[i]
        master_fil <- master_key_child2 |> filter(mun_code == code)
        
        
        mun_list[[i]] <- unlist(master_fil$row.code) |> as.numeric()
        
      }
      
      saveRDS(mun_list, "appdata/mun_list_child.rds")
      
      # 4. warehouse-batch by municipality matrix for doses constraint
      #### used to calculate total vaccines allocated across all municipalities for each batch
      #### which should <= to the total doses available for each batch
      
      # create batch level constraint -- rows for each batch
      batch_list <- list()
      
      batch_codes <- unique(master_key_child$key)
      
      for (i in 1:length(batch_codes)) {
        
        keyx <- batch_codes[i]
        batch_fil <- master_key_child2 |> filter(key == keyx)
        
        batch_list[[i]] <- unlist(batch_fil$row.code) |> as.numeric()
      }
      
      saveRDS(batch_list, "appdata/batch_list_child.rds")
      
    }
    
    
    
    
    child_allocation <- child.vat.model(days_allocated = child_days_allocated_value,
                                        salmi_inventory2 = salmi_data_updated$dat)
    
    child_allocation <- child_allocation %>% 
      relocate(mun_name, .after = mun_code) %>% 
      relocate(region_name, .after = mun_name) #%>% 
      # relocate(Almacen, .after = region_name) %>% 
      # relocate(Suministro, .after = Almacen)
    saveRDS(child_allocation, "appdata/child_allocation.rds")
    
    connections_key <- connections2 %>% 
      filter(warehouse_code != "A")
    
    #Need to separate out rows from national warehouse
    child_regional_allocs <- child_allocation %>% 
      dplyr::filter(warehouse_code == "A")
    
    #All other ones --> ugly but easier rejoining after manipulation
    child_remaining_allocs <- child_allocation %>%
      dplyr::filter(warehouse_code != "A")
    
    #Match indices of these against the key to then replace warehouse info 
    child_index_matches <- match(child_regional_allocs$mun_code,connections_key$mun_code)
    
    # #NA ones --> only supplied by national (removed externos from data for now so that's probably why)
    # child_remaining_allocs <- rbind(child_remaining_allocs, child_regional_allocs[which(is.na(child_index_matches)),])
    # child_regional_allocs <- child_regional_allocs[-which(is.na(child_index_matches)),]
    # 
    # child_index_matches <- match(child_regional_allocs$mun_code,connections_key$mun_code)
    
    #replace almacen, warehouse code, dep
    child_regional_allocs$warehouse_code <- connections_key$warehouse_code[child_index_matches]
    child_regional_allocs$dep_clean <- connections_key$dep_clean[child_index_matches]
    
    #Rejoin with the allocation table --> replace original rows and then output nat'l to regional as another element in list output?
    child_remaining_allocs <- rbind(child_remaining_allocs, child_regional_allocs)
    
    child_clean_allocs <- child_remaining_allocs %>% 
      dplyr::select(dep_clean, mun_name, batch_num, vax_allocated) %>%
      dplyr::rename(Origin = dep_clean, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
      dplyr::mutate(Distribution = "Final")
    
    child_natl_allocs <- child_regional_allocs %>%#Need two of these --> 1 for allocs from natl to regional 1 from regional to muni
      dplyr::select(dep_clean, mun_name, batch_num, vax_allocated) %>%
      dplyr::rename(Origin = dep_clean, Destination = mun_name, `Batch Num` = batch_num, Allocated = vax_allocated) %>%
      dplyr::mutate(Destination = Origin,
                    Origin = "Almacen Nacional") %>%
      dplyr::group_by(Origin, Destination, `Batch Num`) %>%
      dplyr::summarise(Allocated = sum(Allocated)) %>%
      ungroup() %>%
      dplyr::mutate(Distribution = "Intermediate")
    
    child_clean_allocs <- rbind(child_clean_allocs, child_natl_allocs) %>%
      dplyr::arrange(Origin, Destination, `Batch Num`)
    
    #Create output list for user --> first one has all final distribution legs, second one has all intermediate, third one is clean for data table --> download all of them?
    child_allocation_list <- list(child_remaining_allocs, child_natl_allocs, child_clean_allocs)
    names(child_allocation_list) <- c("final", "intermediate", "clean")
    
    child_allocation_list
   
  })
  
  
  
  # display the model finished message-------
  
  child_proposed_distribution <- reactiveValues(dt = DT::datatable(salmi_data))
  
  observeEvent(input$child_run_model, {
    toggle('child_run_model_message')
    
    child_numdays <- input$child_days_allocated_input
    
    #proposed_distribution$dt <- proposed_dist
    
    output$child_run_model_text <- renderText({
      i18n$t("The model has finished generating outputs. You can view the model outputs below and download them.")
    })
  })
  
  # model map-----
  output$child_dist_map <-  renderLeaflet({
    child_allocation_list <- child_allocation()
    
    child_allocation <- child_allocation_list$final
    child_inter_alloc <- child_allocation_list$intermediate
    
    if(isTruthy(child_allocation)) {
      child_allocation$key <- paste(child_allocation$dep_clean, child_allocation$mun_code, sep = "_")
      connections2$key <- paste(connections2$dep_clean, connections2$mun_code, sep = "_")
      
      child_alloc_joined <- left_join(child_allocation, connections2, by = c("key")) %>% as.tibble()
      
      #Aggregate allocation by routes (multiple batches along one route currently not shown)
      child_alloc_agg <- child_alloc_joined %>%
        dplyr::group_by(dep_clean.x, mun_name.x, lon1, lat1, lon2, lat2) %>%
        dplyr::summarise(vax_allocated = sum(vax_allocated),
                         batch_num = paste(batch_num, collapse = ", "))

      
      flows2 <- gcIntermediate(child_alloc_agg[,c("lon1", "lat1")], 
                               child_alloc_agg[,c("lon2", "lat2")],
                               sp = T,
                               addStartEnd = T)
      
      
      flows2$Origin <- child_alloc_agg$dep_clean.x
      flows2$Destination <- child_alloc_agg$mun_name.x
      #flows2$Suministro <- child_alloc_joined$Suministro
      flows2$Batch_Num <- child_alloc_agg$batch_num
      #flows2$Time_to_Exp <- child_alloc_joined$time_to_exp
      flows2$Vax_Allocated <- child_alloc_agg$vax_allocated
      
      label_list <- list()
      popup_list <- list()
      for(i in 1:nrow(flows2)) {
        label_list[[i]] <- paste0(str_to_title(flows2$Origin[i]), " a ", flows2$Destination[i])
        popup_list[[i]] <- paste(glue::glue("<b>{str_to_title(flows2$Origin[i])} a {flows2$Destination[i]}</b>"),  
                                 #glue::glue("<i>Suministro</i>: {flows2$Suministro[i]}"),
                                 glue::glue("<i>Número de lote</i>: {flows2$Batch_Num[i]}"),
                                 glue::glue("<i>Vacunas Asignadas</i>: {flows2$Vax_Allocated[i]}"),
                                 sep = "</br>")
      }
      
      pal <- colorFactor(brewer.pal(4, "Set2"), flows2$Origin)
      
      origin_almacen <- connections2 |> 
        dplyr::select(dep_clean, lon2, lat2) |> 
        st_as_sf(coords = c("lon2", "lat2"))
      
      dest_mun <-  connections2 |> 
        dplyr::select(mun_name, lon1, lat1) |> 
        st_as_sf(coords = c("lon1", "lat1"))
      
      # #Remove central to muni --> FLAG TEMP SOLUTION TILL MODEL FIXED
      # flows2 <- flows2[flows2$Origin != "ALMACEN NACIONAL",]
      
      out_leaf <- leaflet() %>%
        addProviderTiles("OpenStreetMap") %>%
        addPolylines(data = flows2, 
                     label = label_list,
                     popup = popup_list,
                     group = ~Origin, color = ~pal(Origin)) %>%
        addCircleMarkers(data = origin_almacen, radius = 1.5, label = ~as.character(dep_clean)) |>
        addCircleMarkers(data = dest_mun, radius = 0.75, color = 'red', label = ~as.character(mun_name)) |>
        # addLayersControl(overlayGroups = unique(flows2$origins),
        #                  options = layersControlOptions(collapsed = T))%>%
        addPolygons(data = adm2,
                    stroke = T,
                    color = "black",
                    weight = 1,
                    fill = F,
                    opacity = 1)
    }
    
    
    if(isTruthy(child_inter_alloc)) {
      #Join warehouse coords --> I'm sorry Hadley Wickham
      child_inter_alloc[,c("lon1", "lat1")] <- connections2[match(child_inter_alloc$Origin,connections2$dep_clean),c("lon2", "lat2")]
      child_inter_alloc[,c("lon2", "lat2")] <- connections2[match(child_inter_alloc$Destination,connections2$dep_clean),c("lon2", "lat2")]
      
      #Aggregate intermediate allocation by routes (multiple batches along one route currently not shown)
      child_inter_alloc_agg <- child_inter_alloc %>%
        dplyr::group_by(Origin, Destination, lon1, lat1, lon2, lat2) %>%
        dplyr::summarise(Allocated = sum(Allocated),
                         `Batch Num` = paste(`Batch Num`, collapse = ", ")) %>%
        dplyr::ungroup()
      
      
      
      child_inter_flows <- gcIntermediate(child_inter_alloc[,c("lon1", "lat1")], 
                                          child_inter_alloc[,c("lon2", "lat2")],
                                          sp = T,
                                          addStartEnd = T)
      
      child_inter_flows$Origin <- child_inter_alloc_agg$Origin
      child_inter_flows$Destination <- child_inter_alloc_agg$Destination
      child_inter_flows$Batch_Num <- child_inter_alloc_agg$`Batch Num`
      child_inter_flows$Vax_Allocated <- child_inter_alloc_agg$Allocated
      
      child_inter_label_list <- list()
      child_inter_popup_list <- list()
      for(i in 1:nrow(child_inter_flows)) {
        child_inter_label_list[[i]] <- paste0(str_to_title(child_inter_flows$Origin[i]), " a ", child_inter_flows$Destination[i])
        child_inter_popup_list[[i]] <- paste(glue::glue("<b>{str_to_title(child_inter_flows$Origin[i])} a {child_inter_flows$Destination[i]}</b>"),  
                                       glue::glue("<i>Número de lote</i>: {child_inter_flows$Batch_Num[i]}"),
                                       glue::glue("<i>Vacunas Asignadas</i>: {child_inter_flows$Vax_Allocated[i]}"),
                                       sep = "</br>")
      }
      
      child_inter_origin_almacen <- child_inter_alloc_agg |> 
        dplyr::select(Origin, lon1, lat1) |> 
        st_as_sf(coords = c("lon1", "lat1"))
      
      child_inter_dest_almacen <-  child_inter_alloc_agg |> 
        dplyr::select(Destination, lon2, lat2) |> 
        st_as_sf(coords = c("lon2", "lat2"))
      
      
      out_leaf <- out_leaf %>%
        addPolylines(data = child_inter_flows, 
                     label = child_inter_label_list,
                     popup = child_inter_popup_list,
                     group = ~Origin, color = "Grey") %>%
        addCircleMarkers(data = child_inter_origin_almacen, radius = 1.5, label = ~as.character(Origin)) |>
        addCircleMarkers(data = child_inter_dest_almacen, radius = 0.75, color = 'red', label = ~as.character(Destination)) 
      # addLayersControl(overlayGroups = unique(inter_flows$Origin),
      #                  options = layersControlOptions(collapsed = T))
      
      
    }
    
    out_leaf
  })
  
  # display the allocation table -------
  
  output$child_prop_dist_dt <- DT::renderDT({
    
    child_allocation_list <- child_allocation()
    
    child_allocation_list$clean
    
    
    # 
    # child_allocation() |> 
    #   dplyr::select(mun_code, mun_name, region_name, Almacen, Suministro, batch_num, avg, 
    #                 vax_admin_const, eligible_atleast_1dose,vax_allocated) %>% 
    #   rename(Municipality = mun_name, vax_doses_allocated = vax_allocated)
    
    
  })
  
  output$child_vax_allocation_download <- downloadHandler(
    filename = function() {
      paste('Peds_Vax-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      writexl::write_xlsx(child_allocation(), file)
    }
  )
  
  
  #Render historic data map-----------
  output$historic_data_tmap <- renderLeaflet({
    sc <- selected_col()
    
    #Filter out sites missing data
    filtered_sites <- joined_sites %>%
      dplyr::filter(!is.na(st_drop_geometry(joined_sites[,!!sc])))
    filtered_sites$num_doses <- as.numeric(unlist(st_drop_geometry(filtered_sites[,sc])))
    filtered_sites <- filtered_sites %>%
      dplyr::select(num_doses, codigo_us, `Nombre US`)
    
    filtered_dep <- historic_dep_data%>%
      dplyr::filter(!is.na(st_drop_geometry(historic_dep_data[,!!sc])))
    filtered_dep$num_doses <- as.numeric(unlist(st_drop_geometry(filtered_dep[,sc])))
    filtered_dep <- filtered_dep %>%
      dplyr::select(num_doses, ADM1_ES) 
    # 
    # dep_quants <- quantile(filtered_dep$num_doses, probs = seq(0,1,0.2))
    # filtered_dep$quantile <- cut(filtered_dep$num_doses, dep_quants, include.lowest = T)
    # new_levs <- sapply(levels(filtered_dep$quantile), 
    #        function(x){
    #          tmp <- gsub("\\[|\\]|\\(|\\)", "", x)
    #          tmp <- sub(",", "dos - ", tmp)
    #          tmp <- paste0(tmp, "dos")
    #          return(tmp)
    #        })
    # levels(filtered_dep$quantile) <- new_levs
    # 
    filtered_mun <- historic_mun_data%>%
      dplyr::filter(!is.na(st_drop_geometry(historic_mun_data[,!!sc])))
    filtered_mun$num_doses <- as.numeric(unlist(st_drop_geometry(filtered_mun[,sc])))
    filtered_mun <- filtered_mun %>%
      dplyr::select(num_doses)
    
    pal <- colorBin("Greens", domain = filtered_dep$num_doses)
    
    leaflet(filtered_dep) %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(fillColor = ~pal(num_doses), #Need to change palette to jenks
                  weight = 1,
                  opacity = 1,
                  color = "black",
                  #dashArray = "3",
                  fillOpacity = 0.8,
                  popup = paste0("Departamento: ", 
                                 filtered_dep$ADM1_ES,
                                 "<br>",
                                 "Número de Dosis: ", 
                                 filtered_dep$num_doses)) %>%
      addLegend(pal = pal, 
                values = ~num_doses, 
                opacity = 0.7, 
                title = "Número de Dosis",
                position = "topright") %>%
      addCircleMarkers(data = filtered_sites,
                       fill = T,
                       fillOpacity = 0.001,
                       stroke = T,
                       weight = 1,
                       color = "black",
                       radius = ~(num_doses/200),
                       popup = paste0(filtered_sites$`Nombre US`,
                                      "<br>",
                                      "Número de Dosis: ", 
                                      filtered_sites$num_doses)) 
  })
  
  #Show save success message when new data saved
  observeEvent(input$update_vax_stock_rhot, {
    toggle('vax_stock_update_message')
    output$vax_stock_update_text <- renderText({
      i18n$t("Vaccine stocks updated")
    })
  })
  
  #Show reset success message when new data saved
  observeEvent(input$reset_vax_stock_rhot, {
    toggle('vax_stock_update_message')
    output$vax_stock_update_text <- renderText({
      i18n$t("Vaccine stocks reset")
    })
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

