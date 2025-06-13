library("corrplot")
library("dplyr")
library("tidyr")
library("ggplot2")
library("Amelia")

data <- read.csv("C:/Users/alexc/Desktop/AAP_2022_city_v9.csv")

View(data)


colnames(data)[colnames(data) == "WHO.Region"]<- "Region"

newNames <- c("region", "Code_iso3", "country_name", "city", "measure_year", "measure_pm25_μg_m3", "measure_PM10_μg_m3", "measure_NO2_μg_m3", "PM25_temporal_coverage", "PM10_temporal_coverage", "NO2_temporal_coverage", "source", "monitoring_station_number", "database_version", "status")
data <- setNames(data, newNames)

summary(data)

data$measure_pm25_μg_m3 <- as.numeric(gsub(",", ".",data$'measure_pm25_μg_m3'))
data$measure_PM10_μg_m3 <- as.numeric(gsub(",", ".",data$'measure_PM10_μg_m3'))
data$measure_NO2_μg_m3 <- as.numeric(gsub(",", ".",data$'measure_NO2_μg_m3'))
data$PM25_temporal_coverage <- as.numeric(gsub(",", ".",data$PM25_temporal_coverage))
data$PM10_temporal_coverage <- as.numeric(gsub(",", ".",data$PM10_temporal_coverage))
data$NO2_temporal_coverage <- as.numeric(gsub(",", ".",data$NO2_temporal_coverage))

data$status <- NULL

summary(data)

table(is.na(data))

str(data)

missing_data <- sum(is.na(data))  / (nrow(data)* ncol(data))*100
missing_data

col_missing <- colSums(is.na(data))/ nrow(data) * 100
col_missing

missmap(data)


# Avec impression des pays:
# Colonnes à vérifier
cols_to_check <- c("measure_PM10_μg_m3", "measure_NO2_μg_m3")

# Années à conserver
years_to_keep <- 2017:2019

# Villes européennes candidates
european_cities <- unique(data$city[data$region == "European Region" & data$measure_year %in% years_to_keep])

# Initialisation
valid_cities <- c()

# Boucle sur chaque ville
for (ville in european_cities) {
  ville_valide <- TRUE  # on suppose que la ville est valide au départ
  
  for (year in years_to_keep) {
    # Filtrage ville + année
    data_year_city <- subset(data, region == "European Region" & city == ville & measure_year == year)
    
    if (nrow(data_year_city) == 0) {
      ville_valide <- FALSE  # pas de données cette année = non valide
      break
    }
    
    # Vérification de la présence de données pour les colonnes d'intérêt
    if (any(is.na(data_year_city[, cols_to_check]))) {
      ville_valide <- FALSE
      break
    }
  }
  
  # Si la ville est valide pour toutes les années
  if (ville_valide) {
    valid_cities <- c(valid_cities, ville)
  }
}


# Obtenir les pays correspondants aux villes valides dans la région "European Region"
countries_of_valid_cities <- unique(data$country_name[data$city %in% valid_cities & data$region == "European Region"])

# Afficher les villes sélectionnées et les pays correspondants
print("Villes sélectionnées :")
print(valid_cities)

print("Pays représentés :")
print(countries_of_valid_cities)