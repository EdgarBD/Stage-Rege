juvenils <- read.csv("C:/Users/edgar/Desktop/AMAP Rege/Données/DB_JUVENILES_final.csv", sep = ";", header = FALSE, 
                     col.names = c("Nom_Placette", "Foret", "Parcelle", "Carre", 
                                   "Essence", "Genre", "Espece", "Nom.Vernaculaire", 
                                   "Hauteur..cm.", "Diametre..cm.", "azimut", 
                                   "Distance.au.centre", "Commentaire", "Type.placette", 
                                   "Traitement", "X_UTM_Placette", "Y_UTM_Placette"),
                     stringsAsFactors = FALSE)

juvenils_paracou <- subset(juvenils, Foret == "Paracou")


#hauteurs des juvenils

bins <- c(30,40,70, 130, 200,300,400,500,600,700,800,900,1000,1100,1500,2001)

    # Création de l'histogramme avec les effectifs
hist(juvenils_paracou$Hauteur..cm., breaks = bins, col = "skyblue", border = "black",
     xlab = "Hauteur (cm)", ylab = "Effectifs",
     main = "Répartition des hauteurs des juvéniles d'arbres",
     freq = TRUE)


effectifs <- table(cut(juvenils_paracou$Hauteur..cm., bins))

# Ajouter les valeurs d'effectif au-dessus de chaque barre
text(x = bins[-length(bins)], y = hist(juvenils_paracou$Hauteur..cm., breaks = bins, plot = FALSE)$counts + 5,
     labels = effectifs, pos = 3, col = "black")




   