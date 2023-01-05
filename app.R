library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)

df_raw_t <- read.csv("december_data_t", encoding = 'UTF-8')
df_raw_w <- read.csv("december_data_w", encoding = 'UTF-8')

df_t <- df_raw_t %>%
  select(placeVisit_location_name, placeVisit_location_address,
         placeVisit_duration_startTimestamp,
         placeVisit_location_latitudeE7, placeVisit_location_longitudeE7
         #i inne potrzebne kolumny
         ) %>%
  mutate(person = "Tymek") %>%
  mutate(color = 'blue')

df_w <- df_raw_w %>%
  select(placeVisit_location_name, placeVisit_location_address,
         placeVisit_duration_startTimestamp,
         placeVisit_location_latitudeE7, placeVisit_location_longitudeE7) %>%
  mutate(person = "Wojtek") %>%
  mutate(color = "yellow")

df_raw = rbind(df_t, df_w)

map_df <- df_raw %>%
  filter(placeVisit_location_address != "") %>%
  mutate(date = substr(placeVisit_duration_startTimestamp, 1, 10)) %>%
  mutate(lat = placeVisit_location_latitudeE7 / 10000000) %>%
  mutate(lng = placeVisit_location_longitudeE7 / 10000000)

map_ui <- fluidPage(
  
  titlePanel("Mapa odwiedzanych miejsc"),
  
  fluidRow(
    column(width = 12, 
    "Mapa odwiedzanych miejsc w danym przedziale czasowym. Im większe kółko, tym 
    częściej odwiedzane.")),
  
  br(),
  
  wellPanel(
    fluidRow(
      column(width = 6,
             checkboxGroupInput(
               inputId = "persons",
               label = "Select persons:",
               choices = c("Tymek", "Czarek", "Wojtek"),
               selected = c("Tymek", "Czarek", "Wojtek"),
               inline = TRUE)
      ),
      column(width = 6,
             dateRangeInput("date_range", "Select date range:",
                            start = "2022-12-07", end = "2022-12-20",
                            min = "2022-12-07", max = "2022-12-20",
                            format = "yyyy-mm-dd", startview = "month",
                            autoclose = TRUE)
             
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(width = 12,
           shinycssloaders::withSpinner(
             leafletOutput("placeMap", height = "400px"))
    )
  )
)


ui2 <- fluidPage(
  
  titlePanel("Czas"),
  "Czas spędzony w miejscach z danej kategorii"
)

server <- function(input, output) {
  
  output$placeMap <- renderLeaflet({
    map_df %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(person %in% input$persons) %>%
      group_by(placeVisit_location_name, placeVisit_location_address,
               lat, lng, color) %>%
      summarise(number = n()) %>%
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       radius = ~number + 5,
                       popup = ~placeVisit_location_name,
                       color = ~color)
  })
  
}

app_ui <- navbarPage(
  title = 'Nasze dane z google maps',
  tabPanel('Mapa', map_ui, icon = icon(name="glyphicon glyphicon-map-marker",lib="glyphicon")),
  tabPanel('Czas', ui2, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  theme = bslib::bs_theme(bootswatch = "cosmo"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Techniki Wizualizacji Danych 2022
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet",
                               href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)

shinyApp(ui = app_ui, server = server)
