# Comparaison des résultats des simulations
# Auteur : Yansong Huang
# Date de création : 2023-04-03

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)

# chemin pour tous les résultats
results_path <- c("calibration-02-02","calibration-03-19","calibration-03-25_a","calibration-03-25_c")

####### 1. série temporelle biomasse ######

# Chargement des données d'objectif pour la calibration
biomass_reference <- read.csv("Yansong_biomass-index_year.csv")

# Séparation des espèces avec biomasse absolues et indices relatives
biomass_colomns <- setdiff(names(biomass_reference),"year")
biomass_absolute_species <- c("whiting","cod","sole","plaice","mackerel","herring")
biomass_relative_species <- setdiff(biomass_colomns,biomass_absolute_species)

# standardisation
biomass_reference_scaled <- biomass_reference
biomass_reference_scaled[,biomass_relative_species] <- scale(biomass_reference[,biomass_relative_species])

# Transformation en format long
biomass_reference_long <- gather(biomass_reference_scaled, key = "species", value = "biomass_data", -year)

# Chargement les résultats de sortie du modèle
all_biomass <- list()

# Boucler à travers les résultats de chaque modèle
all_biomass <- lapply(results_path, function(current_results_path){
  # Charger les résultats de sortie du modèle
  list_biomass <- list.files(current_results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  
  # Créer un nouveau dataframe pour stocker la biomasse de toutes les espèces
  biomass_total <- data.frame(
    year = integer(),
    species = character(),
    biomass_output_mean = numeric(),
    biomass_output_sd = numeric()
  )
  
  # Boucler à travers chaque espèce
  for (species in 1:16) {
    biomass_species <- data.frame(
      year = c(2002:2021),
      species = rep("", 20),
      stringsAsFactors = FALSE
    )
    # Boucler à travers chaque simulation
    for (simulation in 1:10) {
      biomass_brut <- read.csv(list_biomass[simulation], skip = 1)
      biomass_brut_scaled <- biomass_brut
      biomass_brut_scaled[, biomass_relative_species] <- scale(biomass_brut[, biomass_relative_species])
      
      biomass_species <- cbind(biomass_species, biomass = biomass_brut_scaled[, species + 1])
    }
    # Stocker les noms des espèces
    biomass_species$species[1:20] <- colnames(biomass_brut[species + 1])
    
    # Calculer la moyenne et l'écart type des 10 résultats de simulation
    biomass_species$biomass_output_mean <- rowMeans(biomass_species[, 3:12])
    biomass_species$biomass_output_sd <- apply(biomass_species[, 3:12], 1, sd)
    
    # Combinaison de la biomasse de toutes les espèces
    biomass_total <- rbind(biomass_total, biomass_species[, c(1, 2, 13, 14)])
  }
  
  # Combinaison des sorties du modèle avec les données observées
  biomass_output_data <- cbind(biomass_total, biomass_reference_long)
  # Supprimer les lignes en double
  biomass_comparison <- biomass_output_data[, -c(5, 6)]
  
  return(biomass_comparison)
}) 

# Création du graphique de comparaison

biomass_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
biomass_sd_colour_palette <- biomass_mean_colour_palette[c(3,5,7,9,11,13)]

biomass_comparison_plot <- ggplot() +
  geom_point(data = all_biomass[[1]], aes(x = year, y = biomass_data, color = "observed data")) +
  geom_line(data = all_biomass[[1]], aes(x = year, y = biomass_output_mean, color = "mean model 1")) +
  geom_ribbon(data = all_biomass[[1]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd model 1"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[2]], aes(x = year, y = biomass_output_mean, color = "mean model 2")) +
  geom_ribbon(data = all_biomass[[2]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd model 2"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[3]], aes(x = year, y = biomass_output_mean, color = "mean model 3")) +
  geom_ribbon(data = all_biomass[[3]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd model 3"),
              alpha = 0.2) +
  geom_line(data = all_biomass[[4]], aes(x = year, y = biomass_output_mean, color = "mean model 4")) +
  geom_ribbon(data = all_biomass[[4]], aes(x = year,
                                           ymin = biomass_output_mean - biomass_output_sd,
                                           ymax = biomass_output_mean + biomass_output_sd,
                                           fill = "sd model 4"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = biomass_mean_colour_palette,
                     breaks = c("observed data","mean model 1","sd model 1","mean model 2","sd model 2","mean model 3","sd model 3","mean model 4","sd model 4"),
                     labels = c("observed data","02/02","02/02","19/13","19/13","25/03 a","25/03 a","25/03 c","25/03 c")) +
  scale_fill_manual(name = element_blank(),
                    values = biomass_sd_colour_palette,
                    breaks = c("sd model 1","sd model 2","sd model 3","sd model 4"),
                    labels = c("02/02","19/03","25/03 a","25/03 c")) +
  facet_wrap(~species, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("biomass (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

# Sauvegarder le graphique de comparaison
ggsave(file.path("figures", results_path[3], "biomass_indices_multiple_models_2.png", sep=""), biomass_comparison_plot, width = 15, height = 8, dpi = 600)

####### 2. série temporelle capture ######

# données
yield_data <- read.csv("Yansong_yield_year.csv")
yield_reference_long <- gather(yield_data, key = "species", value = "yield_data", -year)

# Chargement les résultats de sortie du modèle
all_yield <- list()

# Boucler à travers les résultats de chaque modèle
all_yield <- lapply(results_path, function(current_results_path){
  # Charger les résultats de sortie du modèle
  list_yield <- list.files(current_results_path, "Yansong_yield_Simu.", full.names = TRUE)
  
  # Créer un nouveau dataframe pour stocker la capture de toutes les espèces
  yield_total <- data.frame(
    year = integer(),
    species = character(),
    yield_output_mean = numeric(),
    yield_output_sd = numeric()
  )
  
  # Boucler à travers chaque espèce
  for (species in 1:16) {
    yield_species <- data.frame(
      year = c(2002:2021),
      species = rep("", 20),
      stringsAsFactors = FALSE
    )
    # Boucler à travers chaque simulation
    for (simulation in 1:10) {
      yield_brut <- read.csv(list_yield[simulation],skip = 1)
      yield_species <- cbind(yield_species, yield = yield_brut[,species+1])
    }
    # Stocker les noms des espèces
    yield_species$species[1:20] <- colnames(yield_brut[species + 1])
    
    # Calculer la moyenne et l'écart type des 10 résultats de simulation
    yield_species$yield_output_mean <- rowMeans(yield_species[, 3:12])
    yield_species$yield_output_sd <- apply(yield_species[, 3:12], 1, sd)
    
    # Combinaison de la capture de toutes les espèces
    yield_total <- rbind(yield_total, yield_species[, c(1, 2, 13, 14)])
  }
  
  # Combinaison des sorties du modèle avec les données observées
  yield_output_data <- cbind(yield_total, yield_reference_long)
  # Supprimer les lignes en double
  yield_comparison <- yield_output_data[, -c(5, 6)]
  # Supprimer les espèces non-exploitées
  yield_comparison <- yield_comparison %>%
    dplyr::filter(!(species %in% c("poorCod","dragonet")))
  
  return(yield_comparison)
}) 

# Création du graphique de comparaison

yield_mean_colour_palette <- c("brown",brewer.pal(12, "Paired"))
yield_sd_colour_palette <- yield_mean_colour_palette[c(3,5,7,9,11,13)]

yield_comparison_plot <- ggplot() +
  geom_point(data = all_yield[[1]], aes(x = year, y = yield_data, color = "observed data")) +
  geom_line(data = all_yield[[1]], aes(x = year, y = yield_output_mean, color = "mean model 1")) +
  geom_ribbon(data = all_yield[[1]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd model 1"),
              alpha = 0.2) +
  geom_line(data = all_yield[[2]], aes(x = year, y = yield_output_mean, color = "mean model 2")) +
  geom_ribbon(data = all_yield[[2]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd model 2"),
              alpha = 0.2) +
  geom_line(data = all_yield[[3]], aes(x = year, y = yield_output_mean, color = "mean model 3")) +
  geom_ribbon(data = all_yield[[3]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd model 3"),
              alpha = 0.2) +
  geom_line(data = all_yield[[4]], aes(x = year, y = yield_output_mean, color = "mean model 4")) +
  geom_ribbon(data = all_yield[[4]], aes(x = year,
                                         ymin = yield_output_mean - yield_output_sd,
                                         ymax = yield_output_mean + yield_output_sd,
                                         fill = "sd model 4"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = yield_mean_colour_palette,
                     breaks = c("observed data","mean model 1","sd model 1","mean model 2","sd model 2","mean model 3","sd model 3","mean model 4","sd model 4"),
                     labels = c("observed data","02/02","02/02","19/13","19/13","25/03 a","25/03 a","25/03 c","25/03 c")) +
  scale_fill_manual(name = element_blank(),
                    values = yield_sd_colour_palette,
                    breaks = c("sd model 1","sd model 2","sd model 3","sd model 4"),
                    labels = c("02/02","19/03","25/03 a","25/03 c")) +
  facet_wrap(~species, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("yield (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

ggsave(file.path("figures",results_path[3],"yield_multiple_models_2.png",sep=""), yield_comparison_plot, width = 10, height = 5, dpi=600)

###### 3. Taille moyenne de capture ######

