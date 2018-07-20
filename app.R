library(shiny)
library(leaflet.extras)
library(leaflet)
library(sf)
library(cpdcrimedata) # devtools::install_github("nathancday/cpdcrimedata")

data(crime)

crime %<>%
    as.tibble() %>%
    filter(complete.cases(.)) %>%
    mutate(year = lubridate::year(DateReported),
           offense = trimws(Offense) %>%
               fct_infreq()) %>%
    filter(offense %in% levels(offense)[1:20]) %>%
    droplevels() %>%
    st_as_sf(coords = c("lon", "lat"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cville Crime Cast"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          h4("Choose one of the 20 most frequent crimes:"),
          selectizeInput("offenses", NULL,
                         levels(crime$offense),
                         "Vandalism"),
          h5("In these years:"),
          selectizeInput("years", NULL,
                         2018:2013,
                         2018,
                         multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    values <- reactiveValues()
    
    print(sort(table(crime$offense)))
    
    observeEvent(c(input$years, input$offenses), {
        
        print(input$offenses)
        
        values$data <- crime %>%
            dplyr::filter(year %in% input$years,
                          offense %in% input$offenses) %>%
            group_by(address) %>%
            count() %>%
            mutate(n = n * 2)
        
    })
    
    
    output$map <- renderLeaflet({
        req(values$data)
        
        print(sort(table(values$data$offense)))
        
       leaflet(values$data) %>%
           addProviderTiles("OpenStreetMap.HOT") %>%
           addCircleMarkers(radius = ~n, color = "#ff5612")
           # addHeatmap(gradient = "viridis", radius = 20, blur = 20, max = .5)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

