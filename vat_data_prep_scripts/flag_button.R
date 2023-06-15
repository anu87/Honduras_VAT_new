
countries <- c("es", "en")

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg"
)

ui <- fluidPage(pickerInput(inputId = "selected_language", label = "", multiple = F, inline = T,
                            choices = countries,
                            selected = "en",
                            choicesOpt = list(content =  
                                                mapply(countries, flags, FUN = function(country, flagUrl) {
                                                  HTML(paste(
                                                    tags$img(src=flagUrl, width=20, height=15),
                                                    ""
                                                  ))
                                                }, SIMPLIFY = FALSE, USE.NAMES = FALSE))
                            
))
server <- function(input, output){
  observeEvent(input$selected_language, {
    print(input$selected_language)
  })
}
shinyApp(ui, server)
