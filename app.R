library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)

map_df <- read.csv("map_data/map_df.csv", encoding = 'UTF-8')
filterData <- readRDS(file = "ramkiW/data.rds")
baseFrame <- readRDS(file = "ramkiW/baseFrame.rds")
#time_df <- 
#trans_df <- 

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
                            start = "2022-12-07", end = "2023-01-05",
                            min = "2022-12-07", max = "2023-01-05",
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


time_ui <- fluidPage(
  
  titlePanel("Czas"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("osoby",
                         "Osoby do porównania",
                         choices = c("Wojtek", "Tymek", "Czarek"),
                         selected = "Wojtek"),
      selectInput("typ", 
                  "Typ spędzanego czasu",
                  choices = c("Uczelnia",
                              "Dom",
                              "Rozrywka",
                              "Inne"
                  )),
      dateInput(
        "tydzien",
        "Wybierz tydzień do analizy",
        min = "2022-12-12",
        max = "2023-01-03",
        value = "2022-12-13",
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        language = "en"
      )),
    
    mainPanel(
      plotOutput("linePlot")
    )
  )
)

trans_ui <- fluidPage(
  
  titlePanel("Transport"),
  "transport czarka"
)


server <- function(input, output) {
  
  #Część Tymek
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
                       radius = ~number,
                       popup = ~placeVisit_location_name,
                       color = ~color)
  })
  #Część Wojtek
  
  observeEvent(input$osoby, {
    print(substr(input$osoby, 1, 1))
  })
  
  output$linePlot <- renderPlot({
    filtr <- case_when(
      input$typ == "Uczelnia" ~"uni",
      input$typ == "Dom" ~"home",
      input$typ == "Rozrywka" ~"fun",
      input$typ == "Inne" ~"other"
    )
    
    osoby <- substr(input$osoby, 1, 1)
    
    tydzien <- format(strptime(input$tydzien, '%Y-%m-%d'), format="%Y-%U")
    
    graphData <- filterData %>% 
      filter(week == tydzien & type == filtr & person %in% osoby) %>% 
      select(week, weekday, minutes, person) %>% 
      group_by(weekday, person) %>% 
      summarise(hours = sum(minutes)/60) %>% 
      data.frame()
    
    graphData <- graphData %>% 
      full_join(baseFrame, by = c("weekday", "person")) %>% 
      mutate(hours = coalesce(hours.x, hours.y)) %>% 
      select(-c(hours.x, hours.y)) %>% 
      filter(person %in% osoby)
 
    plot <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, colour = person)) +
      geom_line() + 
      geom_point() +
      theme_bw()+
      scale_x_continuous("weekday", labels = graphData$weekday, breaks = graphData$weekday)
    plot
    
  })
  
}

app_ui <- navbarPage(
  title = 'Nasze dane z google maps',
  tabPanel('Mapa', map_ui, icon = icon(name="glyphicon glyphicon-map-marker",lib="glyphicon")),
  tabPanel('Czas', time_ui, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  tabPanel('Transport', trans_ui),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
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
