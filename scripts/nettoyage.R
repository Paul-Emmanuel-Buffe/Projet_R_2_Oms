
# ========= PROUVER LES LA FORTE PROPORTION DE VALEUR MANQUANTES PAR REGION ====

regions <- unique(data$region)
# Initialiser une liste vide pour stocker les résultats
missing_by_region <- list()

# Boucle sur chaque région
for (reg in regions) {
  # Filtrer les données pour la région en cours
  data_region <- subset(data, region == reg)
  
  # Calculer le pourcentage de valeurs manquantes par colonne
  col_missing <- colSums(is.na(data_region)) / nrow(data_region) * 100
  
  # Stocker le résultat dans la liste avec le nom de la région
  missing_by_region[[reg]] <- col_missing
}

# accéder aux valeurs manquantes pour la région "Eastern Mediterranean Region"
missing_by_region[["Eastern Mediterranean Region"]]

# accéder aux valeurs manquantes pour la région "Western Pacific Region"
missing_by_region[["Western Pacific Region"]]

# accéder aux valeurs manquantes pour la région "European Region"
missing_by_region[["European Region"]]

# accéder aux valeurs manquantes pour la région "South East Asia Region"
missing_by_region[["South East Asia Region"]]

# accéder aux valeurs manquantes pour la région "Region of the Americas"
missing_by_region[["Region of the Americas"]]

# accéder aux valeurs manquantes pour la région "African Region"
missing_by_region[["African Region"]]


# ----- Filtrage du dataset "data" - année 2019 : Les pays avec avec des mesures complètes ----

# Étape 1: Garder uniquement l'année 2019
data_2019 <- data[data$measure_year == 2019, ]

# Étape 2: Garder seulement les lignes où PM10, NO2 et PM2.5 ne sont pas vides
data_2019_complet <- data_2019[!is.na(data_2019$measure_PM10_μg_m3) & 
                                 !is.na(data_2019$measure_NO2_μg_m3) & 
                                 !is.na(data_2019$measure_pm25_μg_m3), ]

# Vérification de la nouvelle base
print(paste("Nombre de lignes pour 2019 avec mesures complètes :", nrow(data_2019_complet)))
print(paste("Nombre de villes uniques :", length(unique(data_2019_complet$city))))
print(paste("Nombre de pays uniques :", length(unique(data_2019_complet$country_name))))

# Affichage des villes et pays
print("Villes avec données complètes en 2019 :")
print(unique(data_2019_complet$city))

print("Pays avec données complètes en 2019 :")
print(unique(data_2019_complet$country_name))

print("Régions présentes dans les données 2019 complètes :")
print(unique(data_2019_complet$region))



# ====== CORRELATION PAR REGION, année 2019 =====

# Région Afrique

# 1. Filtrer les données de l'année 2019 et de la région "African Region"
data_region <- subset(data, region == "African Region" & measure_year == 2019)

# 2. Calculer le pourcentage de valeurs manquantes par ville pour les trois polluants
city_missing <- aggregate(
  cbind(measure_PM10_μg_m3, measure_NO2_μg_m3, measure_pm25_μg_m3) ~ city,
  data = data_region,
  FUN = function(x) mean(is.na(x)) * 100
)

# 3. Garder les villes où chaque polluant a moins de 10 % de valeurs manquantes
valid_cities <- city_missing$city[
  rowSums(city_missing[, 2:4] < 10) == 3
]

# 4. Filtrer à nouveau les données avec ces villes valides
data_filtered <- subset(data_region, city %in% valid_cities)

# 5. Sélectionner les colonnes numériques à corréler
polluants <- data_filtered[, c("measure_PM10_μg_m3", "measure_NO2_μg_m3", "measure_pm25_μg_m3")]

# 6. Calculer la matrice de corrélation en ignorant les NA
cor_matrix <- cor(polluants, use = "complete.obs")

# 7. Afficher la matrice dans la console
print(cor_matrix)

# 8. Installer et charger le package corrplot si nécessaire
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# 9. Sauvegarder le graphique de corrélation dans un fichier PNG
png("corrplot_afrique.png", width = 800, height = 600)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         tl.cex = 0.8)
dev.off()


# Eastern Mediterranean Region
# 1. Filtrer les données de l'année 2019 et de la région "Eastern Mediterranean Region"
data_region <- subset(data, region == "Eastern Mediterranean Region" & measure_year == 2019)

# 2. Calculer le pourcentage de valeurs manquantes par ville pour les trois polluants
city_missing <- aggregate(
  cbind(measure_PM10_μg_m3, measure_NO2_μg_m3, measure_pm25_μg_m3) ~ city,
  data = data_region,
  FUN = function(x) mean(is.na(x)) * 100
)

# 3. Garder les villes où chaque polluant a moins de 10 % de valeurs manquantes
valid_cities <- city_missing$city[
  rowSums(city_missing[, 2:4] < 10) == 3
]

# 4. Filtrer à nouveau les données avec ces villes valides
data_filtered <- subset(data_region, city %in% valid_cities)

# 5. Sélectionner les colonnes numériques à corréler
polluants <- data_filtered[, c("measure_PM10_μg_m3", "measure_NO2_μg_m3", "measure_pm25_μg_m3")]

# 6. Calculer la matrice de corrélation en ignorant les NA
cor_matrix <- cor(polluants, use = "complete.obs")

# 7. Afficher la matrice dans la console
print(cor_matrix)

# 8. Installer et charger le package corrplot si nécessaire
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# 9. Sauvegarder le graphique de corrélation dans un fichier PNG
png("corrplot_mediteranean.png", width = 800, height = 600)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         tl.cex = 0.8)
dev.off()


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