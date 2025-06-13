library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(leaflet)

#  Charger le dataset retenu et nettoyé
data <- read.csv("positive_cities_standardized.csv", stringsAsFactors = FALSE)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Pollution Dashboard – Europe 2015–2019"),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    fluidRow(
      valueBoxOutput("pm25_box"),
      valueBoxOutput("pm10_box"),
      valueBoxOutput("no2_box")
    ),
    
    fluidRow(
      box(width = 8, title = "Évolution annuelle des polluants (2015–2019)", plotlyOutput("line_trend")),
      box(width = 4, title = "Répartition géographique (PM2.5)", leafletOutput("map_europe", height = 300))
    ),
    
    fluidRow(
      box(width = 6, title = "Pollution moyenne par pays", plotlyOutput("bar_country")),
      box(width = 6, title = "Top 5 villes les plus polluées", DTOutput("top_cities"))
    ),
    
    fluidRow(
      box(width = 12, title = "Pays les plus touchés (PM2.5)", DTOutput("table_polluted_countries"))
    )
  )
)

server <- function(input, output, session) {
  
  # ── KPI
  output$pm25_box <- renderValueBox({
    valueBox(
      paste0(round(mean(data$measure_pm25_ug_m3, na.rm = TRUE), 1), " µg/m³"),
      "PM2.5 moyen (2015–2019)",
      icon = icon("smog"),
      color = "aqua"
    )
  })
  
  output$pm10_box <- renderValueBox({
    valueBox(
      paste0(round(mean(data$measure_pm10_ug_m3, na.rm = TRUE), 1), " µg/m³"),
      "PM10 moyen (2015–2019)",
      icon = icon("cloud"),
      color = "blue"
    )
  })
  
  output$no2_box <- renderValueBox({
    valueBox(
      paste0(round(mean(data$measure_no2_ug_m3, na.rm = TRUE), 1), " µg/m³"),
      "NO2 moyen (2015–2019)",
      icon = icon("wind"),
      color = "navy"
    )
  })
  
  # ── Courbe temporelle
  output$line_trend <- renderPlotly({
    df <- data %>%
      group_by(measure_year) %>%
      summarise(across(c(measure_pm25_ug_m3, measure_pm10_ug_m3, measure_no2_ug_m3), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(-measure_year, names_to = "pollutant", values_to = "value")
    
    p <- ggplot(df, aes(x = measure_year, y = value, color = pollutant)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(x = "Année", y = "Concentration (µg/m³)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ── Carte Leaflet
  output$map_europe <- renderLeaflet({
    fake_map <- data %>%
      group_by(country_name) %>%
      summarise(pm25 = mean(measure_pm25_ug_m3, na.rm = TRUE)) %>%
      mutate(
        lat = runif(n(), 45, 55),
        lng = runif(n(), 0, 20)
      )
    
    leaflet(fake_map) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        label = ~paste(country_name, ":", round(pm25, 1), "µg/m³"),
        radius = ~sqrt(pm25) * 1.5,
        color = ~colorNumeric("YlOrRd", pm25)(pm25),
        fillOpacity = 0.8
      )
  })
  
  # ── Barplot par pays
  output$bar_country <- renderPlotly({
    df <- data %>%
      group_by(country_name, measure_year) %>%
      summarise(
        pm25 = mean(measure_pm25_ug_m3, na.rm = TRUE),
        pm10 = mean(measure_pm10_ug_m3, na.rm = TRUE),
        no2  = mean(measure_no2_ug_m3, na.rm = TRUE)
      ) %>%
      pivot_longer(-c(country_name, measure_year), names_to = "pollutant", values_to = "value")
    
    p <- ggplot(df, aes(x = measure_year, y = value, color = pollutant)) +
      geom_line() +
      facet_wrap(~country_name, scales = "free_y") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ── Top 5 villes polluées
  output$top_cities <- renderDT({
    top <- data %>%
      group_by(city) %>%
      summarise(
        pm25 = round(mean(measure_pm25_ug_m3, na.rm = TRUE), 1),
        pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
        no2  = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(pm25)) %>%
      head(5)
    
    datatable(top)
  })
  
  # ── Tableau pays les plus pollués
  output$table_polluted_countries <- renderDT({
    table <- data %>%
      group_by(country_name) %>%
      summarise(
        pm25 = round(mean(measure_pm25_ug_m3, na.rm = TRUE), 1),
        pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
        no2  = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1)
      ) %>%
      arrange(desc(pm25))
    
    datatable(table)
  })
}

shinyApp(ui = ui, server = server)
