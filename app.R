library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)
library(Cairo)
options(shiny.usecairo=T)

map_df <- read.csv("map_data/map_df.csv", encoding = 'UTF-8')
filterData <- readRDS(file = "ramkiW/data.rds")
baseFrame <- readRDS(file = "ramkiW/baseFrame.rds")
trans_df <- read.csv("trans_data/dataC", encoding = 'UTF-8')

map_ui <- fluidPage(
  
  titlePanel("Mapa of visited places"),
  
  fluidRow(
    column(width = 12, 
    "Map of places we visited in a given date range. The bigger the circle, 
    the more visited.")),
  
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
             leafletOutput("placeMap", height = "400px"),
             color = "#2fa4e7")
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
                         selected = c("Wojtek", "Tymek", "Czarek")),
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
      shinycssloaders::withSpinner(
        plotOutput("linePlot"),
        color = "#2fa4e7")
    )
  )
)

trans_ui <- fluidPage(
  
  titlePanel("Transport"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("transport",
                         "mode of transport",
                         choices = c("walk", "bicycle", "car", "tram", "train", "bus", "subway", "plane"),
                         selected = "walk"),
      dateRangeInput("date_range_trans", "Select date range:",
                     start = "2022-12-07", end = "2023-01-05",
                     min = "2022-12-07", max = "2023-01-05",
                     format = "yyyy-mm-dd", startview = "month",
                     autoclose = TRUE)),
    
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("barPlot"),
        color = "#2fa4e7")
      
    )
  )
)


server <- function(input, output) {
  
  #Czesc Czarek
  output$barPlot <- renderPlot({
    trans_df$weekDay <- factor(trans_df$weekDay, levels = c("monday", "tuesday", "wednesday", "friday", "saturday", "sunday"))
    trans_df %>% 
      filter(date >= input$date_range_trans[1] & date <= input$date_range_trans[2]) %>%
      filter(type %in% input$transport) %>% 
      group_by(weekDay, name) %>% 
      summarise(sumKilo = sum(distance)) %>% 
      ggplot(aes(x = weekDay, y = sumKilo, fill = name)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_fill_manual(
        values = c(Czarek = "#4285F4", Wojtek = "#0F9D58", Tymek = "#F4B400")
      ) +
      labs(title = "Sum of kilometers we travelled during the weekday", y = "kilometers", x = "weekday", fill = "person") +
      theme(text=element_text(size = 15)) +
      theme_minimal()
  })
  
  #Część Tymek
  output$placeMap <- renderLeaflet({
    map_df %>% 
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      filter(person %in% input$persons) %>%
      group_by(placeVisit_location_name, placeVisit_location_address,
               lat, lng, color) %>%
      summarise(number = n()) %>%
      mutate(number = ifelse(number > 18, 20, number + 2)) %>%
      leaflet() %>%
      addTiles() %>% 
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       radius = ~number,
                       popup = ~placeVisit_location_name,
                       color = ~color,
                       opacity = 0.7,
                       fillOpacity = 0.3
                       )
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
  
    graphData
    
    plot <- ggplot(data = graphData, aes(x=weekday, y=hours, group = person, color = person)) +
      geom_line() + 
      scale_color_manual(
        values = c(C = "#4285F4", W = "#0F9D58", T = "#F4B400")
      ) +
      geom_point() +
      theme_minimal()+
      scale_x_continuous("weekday", labels = graphData$weekday, breaks = graphData$weekday)
    plot
    
  })
  
}

app_ui <- navbarPage(
  title = 'Our Google Maps acticity',
  tabPanel('Map', map_ui, icon = icon(name="glyphicon glyphicon-map-marker",lib="glyphicon")),
  tabPanel('Time', time_ui, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  tabPanel('Transport', trans_ui, icon = icon(name="glyphicon glyphicon-road",lib="glyphicon")),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Data Visualization Techniques 2023 
                </p>
                </footer>
                "),
  header = tags$head(tags$link(rel = "stylesheet",
                               href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css"))
)

shinyApp(ui = app_ui, server = server)
