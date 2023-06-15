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
#library(tmap)
library(lubridate)
library(DT)
library(dplyr)
#setwd("~/Honduras_VAT/Honduras_VAT")


#Load new translator
i18n <- Translator$new(translation_csvs_path = "translations")
i18n$set_translation_language("en")
i18n$use_js()

#Theme content --> source in from other script
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


#Data load --> temp data until final version
#stock_data <- readRDS("data/")
vax_sites <- readRDS("data/Sites_w_Coords.rds")
historic_site_data <- readRDS("data/Site_Hist.rds")
historic_mun_data <- readRDS("data/joined_adm2.rds")
historic_dep_data <- readRDS("data/joined_adm1.rds")


joined_sites <- readRDS("data/joined_sites.rds")
#low_leaflet <- readRDS("data/flow_leaflet.rds")
connections <- readxl::read_xlsx("data/travel time matrix between warehouses.xlsx", sheet = 2)

salmi_data <- readxl::read_xlsx("data/SALMI HON Inventario Biologicos_2022-01-13. Vaccine Inventory .xlsx")
salmi_data <- salmi_data[,1:7]
colnames(salmi_data) <- salmi_data[3,]
salmi_data <- salmi_data[-c(1:3),]

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
                                 p(i18n$t("Vaccination Sites"),style="text-align:center;font-weight:bold"),
                                 leafletOutput("vax_sites", height = 600)
                             ))
                         )
                ),
                
                #Proposed Distribution Page -------------
                tabPanel(title = i18n$t("Proposed Distribution"),
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
                           # fluidRow("La carte et le tableau ci-dessous présentent les résultats du modèle. Les sites de vaccination et les sites de distribution sont représentés par des couleurs différentes. Cliquez sur l'un des sites pour obtenir plus d'informations sur le nombre de vaccins qu'ils ont reçus."),
                           # fluidRow("(The map and table below display the outputs from the model. The vaccination sites and distribution sites are represented by different colors. Click on any of the sites to view more information about how many vaccines they recieved.)"),
                           box(width = 12,
                               p(strong(i18n$t("Vaccine Allocation proposed by the model"), style = "font-size:25px;font-weight:bold")), 
                               p(i18n$t("The below map shows the allocation of vaccines from the distribution centers to the vaccination sites.")),
                               status = "warning", solidHeader = TRUE, collapsible = FALSE,
                               #leafletOutput("prop_dist", height = 600),
                               DTOutput("prop_dist_dt", height = 600),
                               downloadButton("vax_allocation_download", label = i18n$t("Download"))
                           ),
                           box(
                             title = i18n$t("Proposed Allocation with District Filter"), width = 12,
                             h5(i18n$t("If you would like to look at one district at a time, use the filter below.")),
                             selectInput("district_selection", i18n$t("Select the District:"), c("Placeholders", "List", "For", "Now")),
                             status = "warning", solidHeader = TRUE, collapsible = FALSE,
                             leafletOutput("prop_dist_filt", height = 600)
                           )
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
                )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Translate menu
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
  
  
  # network map ----------
  output$full_network <- renderLeaflet({
    # 
    
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
      addCircleMarkers(data = dest_wh, radius = 1, color = 'red', label = ~as.character(`destination warehouse`)) |>
      addLayersControl(overlayGroups = unique(flows$origins),
                       options = layersControlOptions(collapsed = T))
    
    # flows <- gcIntermediate(connections[,c("Latitude1", "Longitude1")], 
    #                         connections[,c("Latitude2", "Longitude2")],
    #                         sp = T,
    #                         addStartEnd = T)
    # flows$Hours <- connections$Hours
    # flows$origins <- connections$`origin warehouse`
    # flows$destinations <- connections$`destination warehouse`
    # 
    # 
    # hover <- paste0(flows$origins, " a ", 
    #                 flows$destinations, ': ')
    # 
    # pal <- colorFactor(brewer.pal(4, "Set2"), flows$origins)
    # 
    # flow_leaflet <-leaflet() %>%
    #   addProviderTiles("OpenStreetMap") %>%
    #   addPolylines(data = flows, label = hover,
    #                group = ~origins, color = ~pal(origins)) %>%
    #   addLayersControl(overlayGroups = unique(flows$origins),
    #                    options = layersControlOptions(collapsed = T))
    # 
    flow_leaflet
    
  })
  
  #Vax sites------
  output$vax_sites <- renderLeaflet({
    
    leaflet(vax_sites) %>%
      addProviderTiles("OpenStreetMap") %>%
      leaflet::addCircles(color = "black",
                          fillOpacity = 1,
                          radius = 500,
                          stroke = F)
    
    # tmap::tm_basemap(c(OpenStreetMap = "OpenStreetMap",
    #              Satellite = "Esri.WorldImagery",
    #              Dark = "CartoDB.DarkMatter")) +
    # tmap::tm_shape(vax_sites) +
    #   tmap::tm_dots()
    
  })
  
  
  # #RHOT
  output$vax_stock_rhot <- renderRHandsontable({
    rhandsontable(salmi_data, readOnly = T)
  })
  # 
  
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
    
    
    # tmap::tm_basemap(c(OpenStreetMap = "OpenStreetMap",
    #              Satellite = "Esri.WorldImagery",
    #              Dark = "CartoDB.DarkMatter")) +
    #   tmap::tm_shape(filtered_dep) +
    #     tmap::tm_fill(col = "num_doses",
    #             alpha = 0.5,
    #             palette = "Greens",
    #             style = "jenks",
    #             id = "ADM1_ES",
    #             popup.vars = c("ADM1_ES", "num_doses")) +
    #     tmap::tm_borders() +
    #   tmap::tm_shape(filtered_mun) +
    #     tmap::tm_fill(col = "num_doses") +
    # tmap::tm_shape(filtered_sites) +
    #   tmap::tm_dots(size = "num_doses", 
    #           alpha = 0,
    #           border.col = "black",
    #           border.lwd = 0.8,
    #           border.alpha = 1,
    #           id = "Nombre US",
    #           popup.vars = c("Nombre US", "codigo_us", "num_doses")) 
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
  
  output$prop_dist_dt <- DT::renderDT({
    proposed_distribution$dt
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

