library(ggplot2)
library(shiny)
library(dplyr)
library(shinycssloaders)
library(leaflet)
library(Cairo)
library(shinyWidgets)
library(htmltools)
library(shinyBS)
options(shiny.usecairo=T)

map_df <- read.csv("map_data/map_df.csv", encoding = 'UTF-8')
filterData <- readRDS(file = "ramkiW/data.rds")
baseFrame <- readRDS(file = "ramkiW/baseFrame.rds")
trans_df <- read.csv("trans_data/dataC", encoding = 'UTF-8')

info_ui <- fluidPage(
  
  h1(
    "About the project",
    align = "center",
    style = "color: #2fa4e7;"
  ),
  
  fluidRow(
    column(width = 3),
    column(width = 6, 
           "Welcome to our webpage. This shiny app was created for Data 
           Visualisation Techniques by 3 WUT students. For over a month we have
           been collecting data using Google Maps, which had access to our
           location 24 hours a day. Thanks to this, we could acquire information
           such as time spent in a certain place or mean of transport used.
           In our visualizations each person has a unique, assigned color: 
           ",
           style = "text-align: justify;"),
    column(width = 3)),
  br(),
  
  fluidRow(
    column(width = 4,
           img(src = 'czarek.png', height = '200px', style = "display: block; margin: 0 auto;"),
           br(),
           tags$figcaption("Czarek", align = 'center', style = "font-size: 30px;")

    ),
    column(width = 4,
           img(src = 'tymek.png', height = '200px', style = "display: block; margin: 0 auto;"),
           br(),
           tags$figcaption("Tymek", align = 'center', style = "font-size: 30px;")
           
    ),
    column(width = 4,
           img(src = 'wojtek.png', height = '200px', style = "display: block; margin: 0 auto;"),
           br(),
           tags$figcaption("Wojtek", align = 'center', style = "font-size: 30px;")
           
    )
  ),
  
)

map_ui <- fluidPage(
  
  titlePanel("Map of visited places"),
  
  fluidRow(
    column(width = 12, 
    "Map of places we visited in a given date range. The bigger the circle, 
    the more visited.")),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupButtons(
        inputId = "persons",
        label = "Select persons:",
        choices = c("Czarek", "Tymek", "Wojtek"),
        selected = c("Czarek", "Tymek", "Wojtek"),
        individual = TRUE,
        status = c("primary", "secondary" , "success")
      
        ),
        
      dateRangeInput("date_range", "Select date range:",
                    start = "2022-12-07", end = "2023-01-05",
                    min = "2022-12-07", max = "2023-01-05",
                    format = "yyyy-mm-dd", startview = "month",
                    autoclose = TRUE)),
    
    mainPanel(
      shinycssloaders::withSpinner(
        leafletOutput("placeMap", height = "500px"),
        color = "#2fa4e7")
    )
  )
)


time_ui <- fluidPage(
  
  titlePanel("Time spent"),
  fluidRow(
    column(width = 12, 
           "Hours spent in places from certain category.")),
  br(),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupButtons(
        inputId = "osoby",
        label = "Select persons:",
        choices = c("Czarek", "Tymek", "Wojtek"),
        selected = c("Czarek", "Tymek", "Wojtek"),
        individual = TRUE,
        status = c("primary", "secondary" , "success")
        
      ),
      selectInput("typ", 
                  "Select category:",
                  choices = c("University",
                              "Home",
                              "Entertainment",
                              "Other"
                  )),
      dateInput(
        "tydzien",
        "Choose week:",
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
  fluidRow(
    column(width = 12, 
           "Sum of kilometers we travelled during the weekday.")),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("transport",
                         "Select modes of transport:",
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
    trans_df$weekDay <- factor(trans_df$weekDay, levels = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"))
    trans_df %>% 
      filter(date >= input$date_range_trans[1] & date <= input$date_range_trans[2]) %>%
      filter(type %in% input$transport) %>%
      mutate(distance = distance/1000) %>%
      group_by(weekDay, name) %>% 
      summarise(sumKilo = sum(distance)) %>% 
      ggplot(aes(x = weekDay, y = sumKilo, fill = name)) +
      geom_bar(position="dodge", stat = "identity") +
      scale_fill_manual(
        values = c(Czarek = "#4285F4", Wojtek = "#0F9D58", Tymek = "#F4B400")
      ) +
      labs(title = "", y = "kilometers", x = "", fill = "person", ) +
      theme_minimal() +
      theme(axis.text=element_text(size=10.5), legend.title = element_blank())
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
      input$typ == "University" ~"uni",
      input$typ == "Home" ~"home",
      input$typ == "Entertainment" ~"fun",
      input$typ == "Other" ~"other"
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
      scale_x_continuous("Weekday", labels = graphData$weekday, breaks = graphData$weekday) +
      labs(y = "Hours") +
      theme(legend.title = element_blank(),
            legend.position = "none")
    plot
    
  })
  
}

app_ui <- navbarPage(
  title = '',
  tabPanel('About', info_ui, icon = icon(name="glyphicon glyphicon-home",lib="glyphicon")),
  tabPanel('Map', map_ui, icon = icon(name="glyphicon glyphicon-map-marker",lib="glyphicon")),
  tabPanel('Time', time_ui, icon = icon(name="glyphicon glyphicon-time",lib="glyphicon")),
  tabPanel('Transport', trans_ui, icon = icon(name="glyphicon glyphicon-road",lib="glyphicon")),
  theme = bslib::bs_theme(bootswatch = "cerulean",
                          primary = '#4285F4',
                          secondary = '#F4B400',
                          success = '#0F9D58',
                          ),
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
