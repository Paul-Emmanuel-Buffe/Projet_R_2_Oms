# ----- Analyse métier -----

##  Analyse des sources et des impacts des polluants atmosphériques (PM₂.₅, PM₁.₀, NO₂)

###  Sources d’émission des PM₂.₅ dans le monde (en %)

| Source principale                   | Proportion (%) |
|-----------------------------------|----------------|
| Combustion résidentielle (bois, charbon, charbon de bois) | 38 % |
| Transports (carburants fossiles)  | 18 %           |
| Industrie (métallurgie, ciment...)| 14 %           |
| Agriculture (ammoniac, poussières)| 14 %           |
| Production d'électricité          | 5 %            |
| Autres (incinération, déchets, etc.) | 11 %        |

---

###  Estimation des PM₁.₀ dans les PM₂.₅

- Les particules **PM₁.₀** (diamètre < 1 µm) représentent environ **70-75 %** des PM₂.₅ selon IQAir et études scientifiques.
- Cela signifie que **70-75 % des décès liés aux PM₂.₅ sont dus aux PM₁.₀**, car ces particules ultra-fines pénètrent plus profondément dans les poumons et le système sanguin.

---

###  Sources mondiales d’émission de NO₂

| Source principale                  | Proportion (%) |
|----------------------------------|----------------|
| Transports (diesel, essence)     | 58 %           |
| Industrie                        | 22 %           |
| Production d'énergie             | 14 %           |
| Résidentiel (chauffage, cuisson) | 6 %            |

---

###  Proportion des décès annuels par type de polluant (sur 5 millions)

| Polluant | Décès estimés | Proportion (%) | Commentaires |
|----------|----------------|----------------|--------------|
| PM₂.₅    | ≈ 4 200 000     | 84 %           | Particules les plus fines et les plus nocives. |
| NO₂      | ≈ 700 000       | 14 %           | Principalement maladies respiratoires. |
| PM₁₀     | (déjà inclus)   | –              | Les décès liés au PM₁₀ sont déjà comptés dans ceux du PM₂.₅. |

 **Conclusion**  
Environ **84 % des décès** liés à la pollution de l’air sont causés par les **PM₂.₅**, dont une grande majorité (**70-75 %**) est due aux **PM₁.₀**, et **14 %** par le **NO₂**.  
Les **PM₁₀** n’ajoutent pas de décès distincts car leurs effets sont englobés dans ceux des PM₂.₅.

### Sources

- **OMS (2021)** – *Air Quality Guidelines* : [who.int/publications/i/item/9789240034228](https://www.who.int/publications/i/item/9789240034228)  
- **State of Global Air (2023)** – Health Effects Institute : [stateofglobalair.org](https://www.stateofglobalair.org/)  
- **IQAir World Air Quality Report (2023)** : [iqair.com/world-air-quality-report](https://www.iqair.com/world-air-quality-report)  
- **Lancet Commission on Pollution and Health (2017)** : [thelancet.com/commissions/pollution-and-health](https://www.thelancet.com/commissions/pollution-and-health)  
- **Agence européenne pour l’environnement (EEA)** : [eea.europa.eu/themes/air](https://www.eea.europa.eu/themes/air)  


# Informations sur la base de données initiale (Document officiel de l'OMS)

### Base de données qualité de l’air - OMS (2022)

- **Source :** Base de données OMS, avril 2022  
  [lien](https://www.who.int/data/gho/data/themes/air-pollution/who-air-quality-database)

- **Polluants :** PM10, PM2.5, NO₂ (concentrations annuelles moyennes en μg/m³)

- **Données :**  
  Rapports officiels, réseaux régionaux (EEA, Clean Air for Asia, AirNow), projets de recherche, articles scientifiques.  
  Couverture minimale : 50 % de l’année (exceptions possibles).  
  Période : 2010–2019, quelques données 2020.

- **Méthode :**  
  Mesures en zones urbaines représentatives, exclusion des "hot spots" industriels sauf intégrés dans les moyennes urbaines.

- **Limites :**  
  Variabilité des méthodes, couverture inégale, biais saisonnier possible, données surtout urbaines.

- **Valeurs guides OMS :**  
  PM10 : 15 μg/m³ | PM2.5 : 5 μg/m³ | NO₂ : 10 μg/m³  


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




#---- Type de monitiring_station ---- 
# Visualisation de la proportion de chaque type de zone d'émission dans l'émissions totale

# Chargement des packages nécessaires
library(dplyr)    # Pour %>%, count(), arrange(), mutate(), filter()
library(tidyr)    # Pour separate()
library(ggplot2)  # Pour ggplot()
library(stringr)  # Pour str_to_title()

# Pour measure_PM10_μg_m3

# Sélectionner les villes de la région Europe
villes_europe <- data[data$region == "European Region", ]

# Garder seulement les années 2017 à 2019
villes_europe_periode <- villes_europe[villes_europe$measure_year >= 2017 & 
                                         villes_europe$measure_year <= 2019, ]

# Garder seulement les lignes où PM10 ne sont pas vides
positive_cities <- villes_europe_periode[!is.na(villes_europe_periode$measure_PM10_μg_m3), ]

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

# Ajout d'un id au dataset
positive_cities_stations$id <- seq_len(nrow(positive_cities_stations))

# Filtrage de la colonne monitoring en deux colonnes number et type 
data_clean <- positive_cities_stations %>%
  separate(monitoring_station_number, 
           into = c("number", "type"), 
           sep = " ", 
           extra = "drop", 
           fill = "right") %>%
  filter(!is.na(type)) %>%
  mutate(number = as.numeric(number))

# Nettoyage des données type
data_clean$type <- gsub(",", "", data_clean$type)
data_clean$type <- gsub("-", "", data_clean$type)

# Standardisation des noms de types (conversion en format titre)
data_clean$type <- str_to_title(data_clean$type)

# Regroupement de "Rural" et "Ruralregional" en "Rural"
data_clean$type <- ifelse(data_clean$type == "Ruralregional", "Rural", data_clean$type)

# Vérification après regroupement
table(data_clean$type)

# Comptage des types
type_counts <- data_clean %>%
  count(type)

# Arrangement et création du facteur
type_counts <- type_counts %>%
  arrange(desc(n)) %>%
  mutate(type = factor(type, levels = type))

# Création du graphique
ggplot(type_counts, aes(x = type, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Places of monitor", x = "Monitor type", y = "Number of monitors") +
  scale_y_continuous(breaks = seq(0, max(type_counts$n) + 10, by = 10)) +
  theme_minimal()

# Affichage des données nettoyées
View(data_clean)


# Création du graphique en camenbert pour 

# Calcul des moyennes PM10 par type de station
pm10_moyennes <- data_clean %>%
  group_by(type) %>%
  summarise(moyenne_pm10 = mean(measure_PM10_μg_m3, na.rm = TRUE)) %>%
  mutate(proportion = moyenne_pm10 / sum(moyenne_pm10) * 100)

# Affichage des résultats
print(pm10_moyennes)

# Création du graphique en camembert
ggplot(pm10_moyennes, aes(x = "", y = proportion, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion des moyennes PM10 par type de station",
       fill = "Type de station") +
  theme_void() +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5))




# Pour measure_NO2_μg_m3

# Sélectionner les villes de la région Europe
villes_europe <- data[data$region == "European Region", ]

# Garder seulement les années 2017 à 2019
villes_europe_periode <- villes_europe[villes_europe$measure_year >= 2017 & 
                                         villes_europe$measure_year <= 2019, ]

# Garder seulement les lignes où NO2 ne sont pas vides
positive_cities <- villes_europe_periode[!is.na(villes_europe_periode$measure_NO2_μg_m3), ]

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

# Ajout d'un id au dataset
positive_cities_stations$id <- seq_len(nrow(positive_cities_stations))

# Filtrage de la colonne monitoring en deux colonnes number et type 
data_clean <- positive_cities_stations %>%
  separate(monitoring_station_number, 
           into = c("number", "type"), 
           sep = " ", 
           extra = "drop", 
           fill = "right") %>%
  filter(!is.na(type)) %>%
  mutate(number = as.numeric(number))

# Nettoyage des données type
data_clean$type <- gsub(",", "", data_clean$type)
data_clean$type <- gsub("-", "", data_clean$type)

# Standardisation des noms de types (conversion en format titre)
data_clean$type <- str_to_title(data_clean$type)

# Regroupement de "Rural" et "Ruralregional" en "Rural"
data_clean$type <- ifelse(data_clean$type == "Ruralregional", "Rural", data_clean$type)

# Vérification après regroupement
table(data_clean$type)

# Comptage des types
type_counts <- data_clean %>%
  count(type)

# Arrangement et création du facteur
type_counts <- type_counts %>%
  arrange(desc(n)) %>%
  mutate(type = factor(type, levels = type))

# Création du graphique
ggplot(type_counts, aes(x = type, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Places of monitor", x = "Monitor type", y = "Number of monitors") +
  scale_y_continuous(breaks = seq(0, max(type_counts$n) + 10, by = 10)) +
  theme_minimal()

# Affichage des données nettoyées
View(data_clean)


# Création du graphique en camembert pour NO2

# Calcul des moyennes NO2 par type de station
no2_moyennes <- data_clean %>%
  group_by(type) %>%
  summarise(moyenne_no2 = mean(measure_NO2_μg_m3, na.rm = TRUE)) %>%
  mutate(proportion = moyenne_no2 / sum(moyenne_no2) * 100)

# Affichage des résultats
print(no2_moyennes)

# Création du graphique en camembert
ggplot(no2_moyennes, aes(x = "", y = proportion, fill = type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion des moyennes NO2 par type de station",
       fill = "Type de station") +
  theme_void() +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5))