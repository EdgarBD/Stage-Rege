juvenils <- read.csv("C:/Users/edgar/Desktop/AMAP Rege/Données/DB_JUVENILES_final.csv", sep = ";", header = FALSE, 
                     col.names = c("Nom_Placette", "Foret", "Parcelle", "Carre", 
                                   "Essence", "Genre", "Espece", "Nom.Vernaculaire", 
                                   "Hauteur..cm.", "Diametre..cm.", "azimut", 
                                   "Distance.au.centre", "Commentaire", "Type.placette", 
                                   "Traitement", "X_UTM_Placette", "Y_UTM_Placette"),
                     stringsAsFactors = FALSE)

juvenils_paracou <- subset(juvenils, Foret == "Paracou")
juvenils_paracou$Diametre..cm. <- gsub(",", ".", juvenils_paracou$Diametre..cm.)

c(30,50, 130, 300, 450, 600, 750, 900, 1200, 1500, 1800, 2000) # classe fixe choisit par ma personne
c(30, 50, 130, 200, 400, 800, 1600, 2000) # classe selon loi logarithme
c(30, 130, 300, 600, 1000, 2000)       ##Classe selon les statdes de devlopment 


#hauteurs des juvenils

str(juvenils_paracou$Hauteur..cm.)
juvenils_paracou$Hauteur..cm. <- as.numeric(juvenils_paracou$Hauteur..cm.)

bins <- c(30,50, 130, 300, 450, 600, 750, 900, 1200, 1500, 1800, 2000)
    # Création de l'histogramme avec les effectifs
hist(juvenils_paracou$Hauteur..cm., breaks = bins, col = "skyblue", border = "black",
     xlab = "Hauteur (cm)", ylab = "Effectifs",
     main = "Répartition des hauteurs des juvéniles d'arbres",
     freq = TRUE)


effectifs <- table(cut(juvenils_paracou$Hauteur..cm., bins))

text(x = bins[-length(bins)], y = hist(juvenils_paracou$Hauteur..cm., breaks = bins, plot = FALSE)$counts + 5,
     labels = effectifs, pos = 3, col = "black")  # Ajouter les valeurs d'effectif au-dessus de chaque barre

    #infos stat descript
mean_hauteur <- mean(juvenils_paracou$Hauteur..cm., na.rm = FALSE)  # Moyenne
sd_hauteur <- sd(juvenils_paracou$Hauteur..cm., na.rm = TRUE)      # Écart-type
median_hauteur <- median(juvenils_paracou$Hauteur..cm.., na.rm = TRUE)  # Médiane
quantiles_hauteur <- quantile(juvenils_paracou$Hauteur..cm., probs = c(0.25, 0.75), na.rm = TRUE)  # Quartiles Q1 et Q3

cat("Moyenne : ", mean_hauteur, "\n")
cat("Écart-type : ", sd_hauteur, "\n")
cat("Médiane : ", median_hauteur, "\n")
cat("1er quartile (Q1) : ", quantiles_hauteur[1], "\n")
cat("3ème quartile (Q3) : ", quantiles_hauteur[2], "\n")

##Diametre
library(dplyr)

juvenils_paracou_filtered <- juvenils_paracou %>% filter(!is.na(Diametre..cm.))

ggplot(juvenils_paracou_filtered, aes(x = Diametre..cm.)) +
  stat_count(aes(y = ..count..), fill = "skyblue", color = "black") +
  labs(x = "Diamètre (cm)", y = "Effectif", title = "Répartition des diamètres des plantules d'arbres") +
  theme_minimal()


intervalles <- seq(0, 10, by = 1)  # Intervalles de 0 à 10 cm par pas de 1 cm
juvenils_paracou_filtered$interval <- cut(juvenils_paracou_filtered$Diametre..cm., breaks = intervalles, right = FALSE) # Couper les diamètres en intervalles définis manuellement

ggplot(juvenils_paracou_filtered, aes(x = interval)) +
  stat_count(geom = "bar", fill = "skyblue", color = "black") +
  labs(x = "Intervalles de Diamètre (cm)", y = "Effectif", title = "Répartition des diamètres des plantules d'arbres par intervalle de 1 cm") +
  theme_minimal() # regroupee

    #Info
mean_diameter <- mean(juvenils_paracou$Diametre..cm., na.rm = TRUE)  # Moyenne
sd_diameter <- sd(juvenils_paracou$Diametre..cm., na.rm = TRUE)      # Écart-type
median_diameter <- median(juvenils_paracou$Diametre..cm., na.rm = TRUE)  # Médiane
quantiles_diameter <- quantile(juvenils_paracou$Diametre..cm., probs = c(0.25, 0.75), na.rm = TRUE)  # Quartiles Q1 et Q3

cat("Statistiques descriptives pour Diametre..cm. :\n")
cat("Moyenne : ", mean_diameter, "\n")
cat("Écart-type : ", sd_diameter, "\n")
cat("Médiane : ", median_diameter, "\n")
cat("1er quartile (Q1) : ", quantiles_diameter[1], "\n")
cat("3ème quartile (Q3) : ", quantiles_diameter[2], "\n")

  # par parcelle

ggplot(juvenils_paracou, aes(x = Parcelle)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Origine des plantules", y = "Nombre de plantules", title = "Répartition des plantules par origine")

ggplot(juvenils_paracou_filtered, aes(x = Diametre..cm., y = Parcelle)) +
  geom_point(size = 3) +
  labs(x = "Diamètre des plantules (cm)", y = "Origine des plantules", color = "Origine") +
  theme_minimal()


ggplot(juvenils_paracou_filtered, aes(x = Diametre..cm., fill = Parcelle)) +
  geom_bar() +
  labs(x = "Diamètre des plantules (cm)", y = "Effectif", fill = "Origine des plantules") +
  theme_minimal()



ggplot(juvenils_paracou_filtered, aes(x = interval, fill = Parcelle)) +
  geom_bar(position = "stack") +
  labs(x = "Intervalle de diamètre (cm)", y = "Effectif", fill = "Origine des plantules", title = "Répartition des diametres par origine") +
  theme_minimal()
