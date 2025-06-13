library("corrplot")
library("dplyr")
library("tidyr")
library("ggplot2")
library("Amelia")
library("stringr")

data <- read.csv("C:/Users/alexc/Desktop/laplateforme/projet/annee1/Projet_R_2_Oms/AAP_2022_city_v9.csv")

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

data$monitoring_station_number <- tolower(data$monitoring_station_number)


                             
# European Region
# 1. Filtrer les données de l'année 2019 et de la région "European Region"
data_region <- subset(data, region == "European Region" & measure_year == 2019)

# 2. Calculer le pourcentage de valeurs manquantes par ville pour les trois polluants
city_missing <- aggregate(
  cbind(measure_PM10_μg_m3, measure_NO2_μg_m3, measure_pm25_μg_m3) ~ city,
  data = data_region,
  FUN = function(x) mean(is.na(x)) * 100
)



# Filtrage simple en cascade extraire le nouveau dataset "positive_cities"

# Étape 1: Sélectionner les villes de la région Europe
villes_europe <- data[data$region == "European Region", ]

# Étape 2: Garder seulement les années 2017 à 2019
villes_europe_periode <- villes_europe[villes_europe$measure_year >= 2017 & 
                                         villes_europe$measure_year <= 2019, ]

# Étape 3: Garder seulement les lignes où PM10 et NO2 ne sont pas vides
positive_cities <- villes_europe_periode[!is.na(villes_europe_periode$measure_PM10_μg_m3) & 
                                           !is.na(villes_europe_periode$measure_NO2_μg_m3), ]

# Vérification de la nouvelle base de données
print(paste("Nombre de lignes dans positive_cities :", nrow(positive_cities)))
print(paste("Nombre de villes uniques :", length(unique(positive_cities$city))))
print(paste("Nombre de pays uniques :", length(unique(positive_cities$country_name))))

# Affichage des villes et pays
print("Villes sélectionnées :")
print(unique(positive_cities$city))
print("Pays correspondants :")
print(unique(positive_cities$country_name))



# Filtrage des villes avec stations de monitoring

# Garder seulement les lignes où monitoring_station_number n'est pas vide
positive_cities_stations <- positive_cities[!is.na(positive_cities$monitoring_station_number), ]

# Vérification de la nouvelle base
print(paste("Nombre de lignes après filtrage stations :", nrow(positive_cities_stations)))
print(paste("Nombre de villes avec stations :", length(unique(positive_cities_stations$city))))
print(paste("Nombre de pays avec stations :", length(unique(positive_cities_stations$country_name))))

# Affichage final des villes et pays
print("Villes avec stations de monitoring :")
print(unique(positive_cities_stations$city))

print("Pays avec stations de monitoring :")
print(unique(positive_cities_stations$country_name))

# ajout d'un id au dataset
positive_cities_stations$id <- seq_len(nrow(positive_cities_stations))

# filtrage de la colonne monitoring en deux colonnes number et type 
data_clean <- positive_cities_stations %>%
  separate(monitoring_station_number, 
           into = c("number", "type"), 
           sep = " ", 
           extra = "drop", 
           fill = "right") %>%
  filter(!is.na(type)) %>%
  mutate(number = as.numeric(number))

data_clean$type <- gsub(",", "",data_clean$type)

data_clean$type <- gsub("-", "",data_clean$type)

table(data_clean$type)



type_counts <- data_clean %>%
  count(type)

type_counts <- type_counts %>%
  arrange(desc(n)) %>%
  mutate(type = factor(type, levels = type))

ggplot(type_counts, aes(x = type, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Places of monitor", x = "Monitor type", y = "Number of monitors") +
  scale_y_continuous(breaks = seq(0, max(type_counts$n) + 10, by = 10))+
  theme_minimal()

text(x = bp, y = counts, labels = counts, pos = 3, cex = 0.8, col = "blue")

View(data_clean)
