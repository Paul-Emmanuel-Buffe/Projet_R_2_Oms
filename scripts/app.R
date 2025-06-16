#installation des packages


# --- LIBRAIRIES ---
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(DT)


# --- DONNÉES ---
data <- read.csv("positive_cities_standardized.csv", stringsAsFactors = FALSE)
data_monitoring <- read.csv("data_clean_monitoring.csv", stringsAsFactors = FALSE)
sw_data <- read.csv("C:/Users/ndiay/Desktop/lptf/projets/IA/DATA_ANALYSIS/R_RSTUDIO/R_U_shiny/data_suisse.csv")
# Filtrage des villes avec stations de monitoring

# zone avec station de monitoring
#positive_cities_stations <- positive_cities[!is.na(positive_cities$monitoring_station_number), ]

# --- UI ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span("Europe 2017–2019", style = "font-size:24px")),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML("
      body { background-color: #f4f9ff; }
      .small-box { background: #ffffff; border-radius: 8px; box-shadow: 0 0 4px rgba(0,0,0,0.1); }
      .box { border-radius: 8px; box-shadow: 0 0 4px rgba(0,0,0,0.05); }
    "))),
    
    fluidRow(
      valueBoxOutput("pm25_box", width = 4),
      valueBoxOutput("pm10_box", width = 4),
      valueBoxOutput("no2_box",  width = 4)
    ),
    
    fluidRow(
      box(width = 4, title = "Sources PM2.5", plotlyOutput("pie_pm25")),
      box(width = 4, title = "Sources NO2", plotlyOutput("pie_no2")),
      box(width = 4, title = "Proportion PM1.0 dans PM2.5", plotlyOutput("pie_pm1_pm25"))
    ),
    
    fluidRow(
      box(title = "Répartition géographique", width = 6,
          leafletOutput("map_pm10", height = 300), solidHeader = TRUE, status = "primary"),
      box(title = "Matrice de corrélation entre polluants", width = 6,
          plotlyOutput("cor_heatmap", height = 300), solidHeader = TRUE, status = "primary")
    ),
    
    fluidRow(
      box(title = "Évolution annuelle des polluants", width = 12,
          plotlyOutput("evolution_plot", height = 300), solidHeader = TRUE, status = "primary")
    ),
    fluidRow(
      box(title = "Évolution PM10 / NO2 par pays", width = 12, status = "primary", solidHeader = TRUE,
          selectInput("selected_country", "Choisir un pays :", choices = unique(data$country_name)),
          plotlyOutput("evolution_country_plot", height = 300)
      )
    ),
    
    fluidRow(
      box(width = 6, title = "Top 5 des pays les plus pollués", DTOutput("table_polluted_countries"), solidHeader = TRUE, status = "danger"),
      box(width = 6, title = "Top 5 des pays les moins pollués", DTOutput("least_polluted_countries"), solidHeader = TRUE, status = "success")
    ),
    
    fluidRow(
    box(width = 6, title = "Top 5 des villes les plus polluées", DTOutput("top_cities"), solidHeader = TRUE, status = "danger"),
    box(width = 6, title = "Top 5 des villes les moins polluées", DTOutput("low_cities"), solidHeader = TRUE, status = "success")
   ),
   
   fluidRow(
     box(title = "Pays avec stations de monitoring", width = 4, status = "info", solidHeader = TRUE,
                     verbatimTextOutput("monitoring_countries")),
     box(
         title = "Répartition des types de stations de monitoring",
         width = 8, solidHeader = TRUE, status = "primary",
         plotlyOutput("monitor_type_plot", height = 300)
       )
     ),
     
   fluidRow(
     box(title = "Répartition PM10 par type de station",width = 6,
       plotlyOutput("pm10_pie_station"),
       solidHeader = TRUE, status = "primary"),
     box(title = "Répartition NO₂ par type de station",width = 6,
         plotlyOutput("no2_pie_station"),
         solidHeader = TRUE, status = "primary")
     ),
   
   fluidRow(
     box(
       title = "Évolution annuelle des polluants en Suisse (par ville)",
       width = 12,
       solidHeader = TRUE,
       status = "info",
       plotlyOutput("switzerland_trend", height = 350)
     )
   )
   
   
   
   
 )
)


# --- SERVER ---
server <- function(input, output, session) {
  
  # === KPI BOXES ===
  output$pm25_box <- renderValueBox({
    avg <- round(mean(data$measure_pm25_ug_m3, na.rm = TRUE), 1)
    valueBox(paste(avg, "µg/m³"), "PM2.5 moyen (2017–2019)", icon = icon("cloud-rain"), color = "aqua")
  })
  
  output$pm10_box <- renderValueBox({
    avg <- round(mean(data$measure_pm10_ug_m3, na.rm = TRUE), 1)
    valueBox(paste(avg, "µg/m³"), "PM10 moyen (2017–2019)", icon = icon("cloud"), color = "light-blue")
  })
  
  output$no2_box <- renderValueBox({
    avg <- round(mean(data$measure_no2_ug_m3, na.rm = TRUE), 1)
    valueBox(paste(avg, "µg/m³"), "NO₂ moyen (2017–2019)", icon = icon("wind"), color = "navy")
  })
  
  # === CAMEMBERT PM2.5 ===
  output$pie_pm25 <- renderPlotly({
    sources <- data.frame(
      Source = c("Combustion résidentielle", "Transports", "Industrie", "Agriculture", "Production d'électricité", "Autres"),
      Pourcentage = c(38, 18, 14, 14, 5, 11)
    )
    plot_ly(sources, labels = ~Source, values = ~Pourcentage, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#1f77b4", "#a6cee3", "#4393c3", "#c6dbef", "#08306b", "#74add1")),
            hole = 0.3) %>%
      layout(showlegend = FALSE)
  })
  
  # === CAMEMBERT NO2 ===
  output$pie_no2 <- renderPlotly({
    sources <- data.frame(
      Source = c("Transports", "Industrie", "Production d'énergie", "Résidentiel"),
      Pourcentage = c(58, 22, 14, 6)
    )
    plot_ly(sources, labels = ~Source, values = ~Pourcentage, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#3182bd", "#9ecae1", "#6baed6", "#c6dbef")),
            hole = 0.3) %>%
      layout(showlegend = FALSE)
  })
  
  # === CAMEMBERT PM1.0 vs PM2.5 ===
  output$pie_pm1_pm25 <- renderPlotly({
    df <- data.frame(
      Composant = c("PM1.0", "Autres PM2.5"),
      Pourcentage = c(72.5, 27.5)
    )
    plot_ly(df, labels = ~Composant, values = ~Pourcentage, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#1c91c0", "#deebf7")),
            hole = 0.3) %>%
      layout(showlegend = FALSE)
  })
  
  # === MAP TOUS LES PAYS ===
  # Préparation des coordonnées pays une seule fois au lancement
  country_coords <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    st_centroid() %>%
    dplyr::select(country_name = name, geometry) %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    ) %>%
    st_drop_geometry()
  
  # Rendu Leaflet basé uniquement sur les noms des pays
  output$map_pm10 <- renderLeaflet({
    df <- data %>%
      group_by(country_name) %>%
      summarise(pm10 = mean(measure_pm10_ug_m3, na.rm = TRUE), .groups = "drop") %>%
      left_join(country_coords, by = "country_name") %>%
      drop_na(lat, lon)
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        label = ~paste0(country_name, ": ", round(pm10, 1), " µg/m³"),
        radius = ~sqrt(pm10),
        color = "#0066cc",
        fillColor = "#3399ff",
        fillOpacity = 0.8,
        stroke = TRUE
      )
  })
  
  # === HEATMAP CORRÉLATION ===
  output$cor_heatmap <- renderPlotly({
    mat <- cor(data[, c("measure_pm25_ug_m3", "measure_pm10_ug_m3", "measure_no2_ug_m3")], use = "complete.obs")
    cor_df <- as.data.frame(as.table(mat))
    colnames(cor_df) <- c("Polluant1", "Polluant2", "Corrélation")
    
    p <- ggplot(cor_df, aes(Polluant1, Polluant2, fill = Corrélation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Corrélation, 2)), size = 6) +
      scale_fill_gradient2(low = "#cce5ff", mid = "white", high = "#004c99", midpoint = 0.5, limit = c(0,1)) +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
  
  # === HISTOGRAMME ANNUEL ===
  output$evolution_plot <- renderPlotly({
    df <- data %>%
      group_by(measure_year) %>%
      summarise(
        no2 = mean(measure_no2_ug_m3, na.rm = TRUE),
        pm10 = mean(measure_pm10_ug_m3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c("no2", "pm10"), names_to = "pollutant", values_to = "value")
    
    gg <- ggplot(df, aes(x = factor(measure_year), y = value, fill = pollutant)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("no2" = "#66c2ff", "pm10" = "#0059b3")) +
      labs(x = NULL, y = "Concentration moyenne (µg/m³)") +
      theme_minimal(base_size = 14)
    
    ggplotly(gg)
  })
  
  #classement des pays
  
  output$table_polluted_countries <- renderDT({
    data %>%
      group_by(country_name) %>%
      summarise(pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
                no2 = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1),
                .groups = "drop") %>%
      arrange(desc(pm10)) %>%
      head(5) %>%
      datatable()
  })
  
  
  
  output$least_polluted_countries <- renderDT({
    data %>%
      group_by(country_name) %>%
      summarise(pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
                no2 = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1),
                .groups = "drop") %>%
      arrange(pm10) %>%
      head(5) %>%
      datatable()
  })
  # classement par ville
  
  output$top_cities <- renderDT({
    data %>%
      group_by(city) %>%
      summarise(pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
                no2 = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1),
                .groups = "drop") %>%
      arrange(desc(pm10)) %>%
      head(5) %>%
      datatable()
  })
  
  output$low_cities <- renderDT({
    data %>%
      group_by(city) %>%
      summarise(pm10 = round(mean(measure_pm10_ug_m3, na.rm = TRUE), 1),
                no2 = round(mean(measure_no2_ug_m3, na.rm = TRUE), 1),
                .groups = "drop") %>%
      arrange(pm10) %>%
      head(5) %>%
      datatable()
  })
  # evolution dans temps par pays
  output$evolution_country_plot <- renderPlotly({
    req(input$selected_country)  # s’assure qu’un pays est sélectionné
    
    df_country <- data %>%
      filter(country_name == input$selected_country) %>%
      group_by(measure_year) %>%
      summarise(
        pm10 = mean(measure_pm10_ug_m3, na.rm = TRUE),
        no2 = mean(measure_no2_ug_m3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c("pm10", "no2"), names_to = "pollutant", values_to = "value")
    
    p <- ggplot(df_country, aes(x = measure_year, y = value, color = pollutant, group = pollutant)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c("pm10" = "#0059b3", "no2" = "#66c2ff")) +
      labs(title = paste("Évolution PM10 & NO2 en", input$selected_country),
           x = "Année", y = "Concentration (µg/m³)") +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
# pays avec stations de monitoring
  output$monitoring_countries <- renderPrint({
    sort(unique(data_monitoring$country_name))
  })
  #répartition par type des station de monotoring
  
  output$monitor_type_plot <- renderPlotly({
    req(data_monitoring)
    
    type_counts <- data_monitoring %>%
      group_by(type) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n)) %>%
      mutate(type = factor(type, levels = unique(type)))  # Tri des barres
    
    p <- ggplot(type_counts, aes(x = type, y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Places of Monitor", x = "Monitor type", y = "Number of monitors") +
      scale_y_continuous(breaks = seq(0, max(type_counts$n) + 10, by = 10)) +
      theme_minimal(base_size = 14)
    
    ggplotly(p)
  })
 #Proportion des moyennes PM10 par type de station 
  output$pm10_pie_station <- renderPlotly({
    req(data_monitoring)
    
    pm10_moyennes <- data_monitoring %>%
      group_by(type) %>%
      summarise(moyenne_pm10 = mean(measure_PM10_μg_m3, na.rm = TRUE)) %>%
      mutate(proportion = moyenne_pm10 / sum(moyenne_pm10) * 100)
    
    plot_ly(pm10_moyennes,
            labels = ~type,
            values = ~proportion,
            type = "pie",
            textinfo = "label+percent",
            insidetextorientation = "radial",
            marker = list(colors = RColorBrewer::brewer.pal(n = 8, name = "Blues"))) %>%
      layout(title = "Proportion des moyennes PM10 par type de station")
  })
  
  
  #Proportion des moyennes NO2 par type de station
  output$no2_pie_station <- renderPlotly({
    req(data_monitoring)
    
    no2_moyennes <- data_monitoring %>%
      filter(!is.na(type) & !is.na(measure_NO2_μg_m3)) %>%
      group_by(type) %>%
      summarise(moyenne_no2 = mean(measure_NO2_μg_m3, na.rm = TRUE), .groups = "drop") %>%
      mutate(proportion = moyenne_no2 / sum(moyenne_no2) * 100)
    
    validate(
      need(nrow(no2_moyennes) > 0, "Aucune donnée disponible.")
    )
    
    plot_ly(
      no2_moyennes,
      labels = ~type,
      values = ~proportion,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(colors = RColorBrewer::brewer.pal(n = 8, name = "Blues"))
    ) %>%
      layout(title = "Proportion des moyennes NO₂ par type de station")
  })
  
  # Filtrage des données pour la Suisse (2016–2020) 
  output$switzerland_trend <- renderPlotly({
    sw_clean <- sw_data %>%
      filter(measure_year >= 2016,
             measure_year <= 2020,
             !is.na(measure_PM10_μg_m3),
             !is.na(measure_NO2_μg_m3))
    
    # Sélectionner les 9 villes avec le plus d'observations
    top_cities <- sw_clean %>%
      count(city, sort = TRUE) %>%
      slice_head(n = 9) %>%
      pull(city)
    
    sw_filtered <- sw_clean %>% filter(city %in% top_cities)
    
    sw_long <- sw_filtered %>%
      select(city, measure_year,
             NO2 = measure_NO2_μg_m3,
             PM10 = measure_PM10_μg_m3) %>%
      pivot_longer(cols = c(NO2, PM10), names_to = "Polluant", values_to = "Valeur")
    
    p <- ggplot(sw_long, aes(x = measure_year, y = Valeur, color = Polluant)) +
      geom_line(aes(group = interaction(city, Polluant)), alpha = 0.6) +
      geom_point(alpha = 0.6) +
      facet_wrap(~city, scales = "free_y", ncol = 3) +
      scale_color_manual(values = c("NO2" = "#66c2ff", "PM10" = "#0059b3")) +
      labs(title = "Évolution PM10 / NO2 en Suisse – Top 9 villes",
           x = "Année", y = "Concentration (µg/m³)") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  
  
}

# --- LANCEMENT ---
shinyApp(ui = ui, server = server)
