juvenils <- read.csv("C:/Users/edgar/Desktop/AMAP Rege/Données/DB_JUVENILES_final.csv", sep = ";", header = FALSE, 
                 col.names = c("Nom_Placette", "Foret", "Parcelle", "Carre", 
                               "Essence", "Genre", "Espece", "Nom.Vernaculaire", 
                               "Hauteur..cm.", "Diametre..cm.", "azimut", 
                               "Distance.au.centre", "Commentaire", "Type.placette", 
                               "Traitement", "X_UTM_Placette", "Y_UTM_Placette"),
                 stringsAsFactors = FALSE)

juvenils_paracou <- subset(juvenils, Foret == "Paracou")

##sur quelles parcelles ?

valeurs_parcelle <- unique(data_paracou$Parcelle)

  #abondance d'especes sur l'ensemble des parcelles
species_counts_df <- as.data.frame(table(juvenils_paracou$Nom.Vernaculaire))
colnames(species_counts_df) <- c("Nom_Vernaculaire", "Effectif")

    #histogramme
ggplot(species_counts_df, aes(x = reorder(Nom_Vernaculaire, -Effectif), y = Effectif)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Effectif des différentes espèces d'arbres (Nom vernaculaire)",
       x = "Nom vernaculaire",
       y = "Effectif") +
  theme_minimal()

      #Camambert
species_counts_df$Percentage <- species_counts_df$Effectif / sum(species_counts_df$Effectif) * 100

ggplot(species_counts_df, aes(x = "", y = Percentage, fill = Nom_Vernaculaire)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Répartition des espèces d'arbres (Nom vernaculaire)") +
  theme(legend.position = "right") +
  scale_fill_discrete(name = "Espèces") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4)

  ##Les especes presentes par parcelle
species_counts_dfp <- as.data.frame(table(juvenils_paracou$Nom.Vernaculaire, juvenils_paracou$Parcelle))
colnames(species_counts_dfp) <- c("Nom_Vernaculaire", "Parcelle", "Effectif")
  
    # effectifs (histo)
ggplot(species_counts_dfp, aes(x = reorder(Nom_Vernaculaire, -Effectif), y = Effectif, fill = Parcelle)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Effectif des différentes espèces d'arbres par parcelle",
       x = "Nom vernaculaire",
       y = "Effectif") +
  theme_minimal() +
  scale_fill_discrete(name = "Parcelle")

    

