# Comparaison des résultats des simulations
# Autheur : Yansong Huang
# Date de création : 2023-05-03

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)

####### 1. série temporelle biomasse ######
# chemin pour tous les résultats
results_path <- c("calibration-03-13_1","calibration-03-19")

# charge les données d'objectif pour la calibration
biomass_reference <- read.csv("Yansong_biomass-index_year.csv")
# standardise les colonnes des indices relatifs
# sépare les espèces avec biomasse absolues et indices relatives
biomass_colomns <- setdiff(names(biomass_reference),"year")
biomass_absolute_species <- c("whiting","cod","sole","plaice","mackerel","herring")
biomass_relative_species <- setdiff(biomass_colomns,biomass_absolute_species)

# standardise les autres espèces
biomass_reference_scaled <- biomass_reference
biomass_reference_scaled[,biomass_relative_species] <- scale(biomass_reference[,biomass_relative_species])

# transform en forme longue
biomass_reference_long <- gather(biomass_reference_scaled, key = "species", value = "biomass_data", -year)

all_biomass <- list()
# Loop through each model's output results
for (results_path in c(results_path)) {
  # Load the model output results
  list_biomass <- list.files(results_path, "Yansong_biomass_Simu.", full.names = TRUE)
  
  # Create a new dataframe to store the biomass of all species
  biomass_total <- data.frame(
    year = integer(),
    species = character(),
    biomass_output_mean = numeric(),
    biomass_output_sd = numeric()
  )
  
  # Loop through each species
  for (species in 1:16) {
    biomass_species <- data.frame(
      year = c(2002:2021),
      species = rep("", 20),
      stringsAsFactors = FALSE
    )
    # Loop through each simulation
    for (simulation in 1:10) {
      biomass_brut <- read.csv(list_biomass[simulation], skip = 1)
      biomass_brut_scaled <- biomass_brut
      biomass_brut_scaled[, biomass_relative_species] <- scale(biomass_brut[, biomass_relative_species])
      
      biomass_species <- cbind(biomass_species, biomass = biomass_brut_scaled[, species + 1])
    }
    # Store species names
    biomass_species$species[1:20] <- colnames(biomass_brut[species + 1])
    
    # Calculate the mean and standard deviation of 10 simulation results
    biomass_species$biomass_output_mean <- rowMeans(biomass_species[, 3:12])
    biomass_species$biomass_output_sd <- apply(biomass_species[, 3:12], 1, sd)
    
    # Combine the biomass of all species
    biomass_total <- rbind(biomass_total, biomass_species[, c(1, 2, 13, 14)])
  }
  
  # Combine model outputs with observed data
  biomass_output_data <- cbind(biomass_total, biomass_reference_long)
  # Remove duplicate rows
  biomass_comparison <- biomass_output_data[, -c(5, 6)]
  
  # Add the table to the list
  all_biomass[[length(all_biomass) + 1]] <- biomass_comparison
}

biomass_comparison_1 <- data.frame(all_biomass[1])
biomass_comparison_2 <- data.frame(all_biomass[2])

# Use ggplot to draw the comparison graph, grouping species in the same graph
plot <- ggplot() +
  geom_point(data = biomass_comparison_1, aes(x = year, y = biomass_data, color = "darkred")) +
  geom_line(data = biomass_comparison_1, aes(x = year, y = biomass_output_mean, color = "darkblue")) +
  geom_ribbon(data = biomass_comparison_1, aes(x = year,
                  ymin = biomass_output_mean - biomass_output_sd,
                  ymax = biomass_output_mean + biomass_output_sd,
                  fill = "blue"),
              alpha = 0.2) +
  geom_line(data = biomass_comparison_2, aes(x = year, y = biomass_output_mean, color = "darkgreen")) +
  geom_ribbon(data = biomass_comparison_2, aes(x = year,
                                         ymin = biomass_output_mean - biomass_output_sd,
                                         ymax = biomass_output_mean + biomass_output_sd,
                                         fill = "lightgreen"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = c("darkred" = "darkred", "darkblue" = "darkblue", "blue" = "blue","darkgreen"="darkgreen","lightgreen"="lightgreen"),
                     breaks = c("darkred", "darkblue", "blue","darkgreen","lightgreen"),
                     labels = c("observed data", "mean model 1", "sd model 1", "mean model 2", "sd model 2")) +
  scale_fill_manual(name = element_blank(),
                    values = c("blue" = "blue","lightgreen"="lightgreen"),
                    breaks = c("blue","lightgreen"),
                    labels = c("sd model 1","sd model 2")) +
  facet_wrap(~species, scales = "free_y", ncol = 4) +  # Specify ncol parameter as 4
  ylab("biomass (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

# Save the comparison graph
ggsave(file.path("figures", "biomass_indices_test.png"), biomass_comparison_plot, width = 15, height = 8, dpi = 600)

####### 2. série temporelle capture ######

# données
yield_data <- read.csv("Yansong_yield_year.csv")
yield_data_long <- gather(yield_data, key = "species", value = "yield_data", -year)

# sorties du modèle
list_yield <- list.files(results_path,"Yansong_yield_Simu.",full.names = TRUE)

# define a dataframe for the yield of all species
yield_total <- data.frame(row.names = c("year","species","yield_output_mean","yield_output_sd"))

yield_total <- data.frame(
  year = integer(), 
  species = character(), 
  yield_output_mean = numeric(), 
  yield_output_sd = numeric(),
  stringsAsFactors = FALSE
)

for (species in 1:16){
  yield_species <- data.frame(year=c(2002:2021),species=rep("inconnu",20))
  for (simulation in 1:10){
    yield_brut <- read.csv(list_yield[simulation],skip = 1)
    yield_species <- cbind(yield_species, yield = yield_brut[,species+1])
  }
  # put the species name in the column
  yield_species$species[1:20] <- colnames(yield_brut[species+1])
  
  # calculate mean and sd among 10 simulations
  yield_species$yield_output_mean <- rowMeans(yield_species[,3:12])
  yield_species$yield_output_sd <- apply(yield_species[,3:12],1,sd)
  
  # regroup the yield of all species
  yield_total <- rbind(yield_total,yield_species[,c(1,2,13,14)])
}

# regroup the model outputs with observed data
yield_output_data <- merge(yield_total,yield_data_long,by=c("year","species")) 

# delete the species non-exploited
yield_output_data <- yield_output_data %>%
  dplyr::filter(!(species %in% c("poorCod","dragonet")))

library(ggplot2)
library(gridExtra)

yield_comparison_plot <- ggplot(data=yield_output_data)+
  geom_line(aes(x=year, y=yield_data, color = "darkred"))+
  geom_line(aes(x=year, y = yield_output_mean,color="darkblue"))+
  geom_ribbon(aes(x=year,
                  ymin = ifelse(yield_output_mean - yield_output_sd>0,yield_output_mean - yield_output_sd,0),
                  ymax = yield_output_mean + yield_output_sd,
                  fill="blue"),
              alpha = 0.2) +
  scale_color_manual(name = element_blank(),
                     values = c("darkred" = "darkred", "darkblue" = "darkblue","blue"="blue"),
                     breaks = c("darkred", "darkblue","blue"),
                     labels = c("observed data", "mean model outputs", "sd model outputs"))+
  scale_fill_manual(name = element_blank(),
                     values = c("blue"="blue"),
                     breaks = c("blue"),
                     labels = c("sd model outputs"))+
  facet_wrap(~species, scales = "free_y") +
  ylab("catch (t)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
  
ggsave(file.path("figures",results_path,"yield.png",sep=""), yield_comparison_plot, width = 10, height = 5, dpi=600)

####### 3. Validation de courbe de croissance ######
growth_path <- file.path(results_path,"AgeIndicators/Yansong_meanSizeDistribByAge_Simu1.csv")
growth_example <- read.csv(growth_path,skip = 1)

# définir le fonction de croissance Von Bertalanffy
vb_SYC <- function(x) {87.42 * (1-exp(-0.118*(x+1.09)))}
vb_MUR <- function(x) {53.3 * (1-exp(-0.18*(x+1.23)))}
vb_BIB <- function(x) {37.6 * (1-exp(-0.46*(x+0.77)))}
vb_WHG <- function(x) {40.24 * (1-exp(-0.63*(x+0.37)))}
vb_POD <- function(x) {22.23 * (1-exp(-0.4*(x+0.679)))}
vb_COD <- function(x) {103.9 * (1-exp(-0.19*(x+0.1)))}
vb_LYY <- function(x) {28.254 * (1-exp(-0.471*(x+0.443)))}
vb_SOL <- function(x) {37.25 * (1-exp(-0.35*(x+1.61)))}
vb_PLE <- function(x) {48 * (1-exp(-0.16*(x+1)))}
vb_HOM <- function(x) {39.2 * (1-exp(-0.18*(x+1.515)))}
vb_MAC <- function(x) {42 * (1-exp(-0.24*(x+2.07)))}
vb_HER <- function(x) {31 * (1-exp(-0.37*(x+0.67)))}
vb_PIL <- function(x) {24.6 * (1-exp(-0.79*(x+0.22)))}
vb_RJC <- function(x) {108 * (1-exp(-0.14*(x+0.88)))}
vb_SQZ <- function(x) {50 * (1-exp(-2*(x-0.5)))}
# lin_SQZ <- function(x) {Lexp+(Lgom-Lexp)*(a-aexp)/(agom-aexp)}
# gom_SQZ <- function(x) {78*exp(-exp(-1.764*(x-0.91)))}
# gom2_SQZ <- function(x) {65*exp(-exp(-2.5*(x-0.7)))}

age_SYC <- seq(1,10,0.1)
age_MUR <- seq(1,11,0.1)
age_BIB <- seq(1,4,0.1)
age_WHG <- seq(1,20,0.1)
age_POD <- seq(1,5,0.1)
age_COD <- seq(1,25,0.1)
age_LYY <- seq(1,6,0.1)
age_SOL <- seq(1,20,0.1)
age_PLE <- seq(1,15,0.1)
age_HOM <- seq(1,15,0.1)
age_MAC <- seq(1,17,0.1)
age_HER <- seq(1,11,0.1)
age_PIL <- seq(1,15,0.1)
age_RJC <- seq(1,20,0.1)
# age_SQZ <- seq(0.7,2,0.1)
# age_SQZ_gom <- seq(0.33,2,0.1)

length_vb_SYC <- vb_SYC(age_SYC)
length_vb_MUR <- vb_MUR(age_MUR)
length_vb_BIB <- vb_BIB(age_BIB)
length_vb_WHG <- vb_WHG(age_WHG)
length_vb_POD <- vb_POD(age_POD)
length_vb_COD <- vb_COD(age_COD)
length_vb_LYY <- vb_LYY(age_LYY)
length_vb_SOL <- vb_SOL(age_SOL)
length_vb_PLE <- vb_PLE(age_PLE)
length_vb_HOM <- vb_HOM(age_HOM)
length_vb_MAC <- vb_MAC(age_MAC)
length_vb_HER <- vb_HER(age_HER)
length_vb_PIL <- vb_PIL(age_PIL)
length_vb_RJC <- vb_RJC(age_RJC)
# length_vb_SQZ <- vb_SQZ(age_SQZ)
# length_gom_SQZ <- gom_SQZ(age_SQZ)
# length_gom2_SQZ <- gom2_SQZ(age_SQZ)

growth_SYC_plot <- ggplot()+
  geom_line(aes(age_SYC,length_vb_SYC))+
  xlim(1,10)+
  geom_point(data=growth_example,mapping=aes(x=Age,y=lesserSpottedDogfish,colour=Time))

growth_MUR_plot <- ggplot()+
  geom_line(aes(age_MUR,length_vb_MUR))+
  xlim(1,11)+
  geom_point(data=growth_example,mapping=aes(x=Age,y=redMullet,colour=Time))

growth_BIB_plot <- ggplot()+
  geom_line(aes(age_BIB,length_vb_BIB))+
  xlim(1,4)+
  geom_point(data=growth_example,mapping=aes(Age,pouting,colour=Time))

growth_WHG_plot <- ggplot()+
  geom_line(aes(age_WHG,length_vb_WHG))+
  xlim(1,20)+
  geom_point(data=growth_example,mapping=aes(Age,whiting,colour=Time))

growth_POD_plot <- ggplot()+
  geom_line(aes(age_POD,length_vb_POD))+
  xlim(1,3)+
  geom_point(data=growth_example,mapping=aes(Age,poorCod,colour=Time))

growth_COD_plot <- ggplot()+
  geom_line(aes(age_COD,length_vb_COD))+
  xlim(1,25)+
  geom_point(data=growth_example,mapping=aes(Age,cod,colour=Time))

growth_LYY_plot <- ggplot()+
  geom_line(aes(age_LYY,length_vb_LYY))+
  xlim(1,6)+
  geom_point(data=growth_example,mapping=aes(Age,dragonet,colour=Time))

growth_SOL_plot <- ggplot()+
  geom_line(aes(age_SOL,length_vb_SOL))+
  xlim(1,20)+
  geom_point(data=growth_example,mapping=aes(Age,sole,colour=Time))

growth_PLE_plot <- ggplot()+
  geom_line(aes(age_PLE,length_vb_PLE))+
  xlim(1,15)+
  geom_point(data=growth_example,mapping=aes(Age,plaice,colour=Time))

growth_HOM_plot <- ggplot()+
  geom_line(aes(age_HOM,length_vb_HOM))+
  xlim(1,15)+
  geom_point(data=growth_example,mapping=aes(Age,horseMackerel,colour=Time))

growth_MAC_plot <- ggplot()+
  geom_line(aes(age_MAC,length_vb_MAC))+
  xlim(1,17)+
  geom_point(data=growth_example,mapping=aes(Age,mackerel,colour=Time))

growth_HER_plot <- ggplot()+
  geom_line(aes(age_HER,length_vb_HER))+
  xlim(1,11)+
  geom_point(data=growth_example,mapping=aes(Age,herring,colour=Time))

growth_PIL_plot <- ggplot()+
  geom_line(aes(age_PIL,length_vb_PIL))+
  xlim(1,15)+
  geom_point(data=growth_example,mapping=aes(Age,sardine,colour=Time))

growth_RJC_plot <- ggplot()+
  geom_line(aes(age_RJC,length_vb_RJC))+
  xlim(1,20)+
  geom_point(data=growth_example,mapping=aes(Age,thornbackRay,colour=Time))

growth_plot <- ggpubr::ggarrange(growth_SYC_plot,growth_MUR_plot,growth_BIB_plot,
                  growth_WHG_plot,growth_POD_plot,growth_COD_plot,
                  growth_LYY_plot,growth_SOL_plot,growth_PLE_plot,
                  growth_HOM_plot,growth_MAC_plot,growth_HER_plot,
                  growth_PIL_plot,growth_RJC_plot, common.legend = T)
ggsave(file.path("figures",results_path,"growth.png",sep=""), growth_plot, width = 10, height = 10, dpi=600)

# comparaison des deux croissance des encornets
# ggplot()+
#   geom_line(aes(age_SQZ,length_vb_SQZ),color="darkred")+
#   geom_line(aes(age_SQZ,length_gom_SQZ),color="darkblue")+
#   geom_line(aes(age_SQZ,length_gom2_SQZ),color="darkgrey")+
#   xlim(0.7,2)


###### 4.1 Biomasse et capture en taille ######

catch_at_length_path <- file.path(results_path,"SizeIndicators/Yansong_yieldNDistribBySize_Simu0.csv")
catch_at_length <- read.csv(catch_at_length_path, skip = 1)
observed_mean_catch_size_all_years <- readRDS("observed_mean_catch_size_all_years_SACROIS_20240110.rds") 

# année 2002, 2012 et 2021
catch_at_length_2002 <- catch_at_length[1:26,-1]
catch_at_length_2012 <- catch_at_length[261:286,-1]
catch_at_length_2021 <- catch_at_length[495:520,-1]

# transformer en format long
catch_at_length_long <- tidyr::gather(catch_at_length, key = "species", value = "simulated", -c(Time,Size))
catch_at_length_2002_long <- tidyr::gather(catch_at_length_2002, key = "species", value = "simulated", -Size)
catch_at_length_2012_long <- tidyr::gather(catch_at_length_2012, key = "species", value = "simulated", -Size)
catch_at_length_2021_long <- tidyr::gather(catch_at_length_2021, key = "species", value = "simulated", -Size)

# enlever les catégories de taille sans capture 
catch_at_length_long <- dplyr::filter(catch_at_length_long, simulated > 0.1)
catch_at_length_2002_long <- dplyr::filter(catch_at_length_2002_long, simulated > 0.1)
catch_at_length_2012_long <- dplyr::filter(catch_at_length_2012_long, simulated > 0.1)
catch_at_length_2021_long <- dplyr::filter(catch_at_length_2021_long, simulated > 0.1)

# ajout les tailles moyennes observées
catch_at_length_2002_long <- catch_at_length_2002_long %>%
  left_join(observed_mean_catch_size_all_years, by="species")
catch_at_length_2012_long <- catch_at_length_2012_long %>%
  left_join(observed_mean_catch_size_all_years, by="species")
catch_at_length_2021_long <- catch_at_length_2021_long %>%
  left_join(observed_mean_catch_size_all_years, by="species")

catch_at_length_2002_plot <- ggplot(catch_at_length_2002_long) + 
  geom_col(aes(x = Size, y = simulated, fill = "darkblue")) + 
  geom_vline(aes(xintercept = observed, colour = "darkred")) +
  facet_wrap(~species, scales = "free")+
  scale_color_manual(name = element_blank(),
                    values = c("darkred" = "darkred"),
                    breaks = "darkred",
                    labels = "observed data")+
  scale_fill_manual(name = element_blank(),
                     values = c("darkblue" = "darkblue"),
                     breaks = "darkblue",
                     labels = "model outputs")+
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Catch distribution by size in 2002") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"catch_at_length_2002.png",sep=""), catch_at_length_2002_plot, width = 10, height = 5, dpi=600)

catch_at_length_2012_plot <- ggplot(catch_at_length_2012_long) + 
  geom_col(aes(x = Size, y = simulated, fill = "darkblue")) + 
  geom_vline(aes(xintercept = observed, colour = "darkred")) +
  facet_wrap(~species, scales = "free")+
  scale_color_manual(name = element_blank(),
                     values = c("darkred" = "darkred"),
                     breaks = "darkred",
                     labels = "observed data")+
  scale_fill_manual(name = element_blank(),
                    values = c("darkblue" = "darkblue"),
                    breaks = "darkblue",
                    labels = "model outputs")+
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Catch distribution by size in 2012") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"catch_at_length_2012.png",sep=""), catch_at_length_2012_plot, width = 10, height = 5, dpi=600)

catch_at_length_2021_plot <- ggplot(catch_at_length_2021_long) + 
  geom_col(aes(x = Size, y = simulated, fill = "darkblue")) + 
  geom_vline(aes(xintercept = observed, colour = "darkred")) +
  facet_wrap(~species, scales = "free")+
  scale_color_manual(name = element_blank(),
                     values = c("darkred" = "darkred"),
                     breaks = "darkred",
                     labels = "observed data")+
  scale_fill_manual(name = element_blank(),
                    values = c("darkblue" = "darkblue"),
                    breaks = "darkblue",
                    labels = "model outputs")+
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Catch distribution by size in 2021") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"catch_at_length_2021.png",sep=""), catch_at_length_2021_plot, width = 10, height = 5, dpi=600)

###### 4.2 série temporelle de taille moyenne des captures #######
# calculer la taille moyenne pondérée
mean_catch_size <- catch_at_length_long %>%
  mutate(year=Time+2001) %>%
  select(-Time) %>%
  group_by(year,species) %>%
  summarise(simulated = weighted.mean(Size, w=simulated))

# charge les données observées
observed_mean_catch_size <- readRDS("observed_mean_catch_size_by_years_SACROIS_20240109.rds") 
observed_mean_catch_size <- observed_mean_catch_size %>%
  rename(species=spp)

mean_catch_size <- mean_catch_size %>%
  left_join(observed_mean_catch_size, by=c("year","species"))

mean_catch_size_plot <- ggplot(mean_catch_size) +
  geom_line(aes(x=year,y=simulated, color = "darkblue")) +
  geom_point(aes(x=year,y=observed,color = "darkred"))+
  facet_wrap(~species,scales = "free")+
  scale_color_manual(name = element_blank(),
                     values = c("darkred" = "darkred", "darkblue" = "darkblue"),
                     breaks = c("darkred", "darkblue"),
                     labels = c("observed data", "model outputs"))+
  ylab("mean catch length (cm)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))

ggsave(file.path("figures",results_path,"mean_catch_size.png"), mean_catch_size_plot, width = 10, height = 5, dpi=600)

####### 4.3 biomass distribution by size #######
biomass_by_size_path <- file.path(results_path,"Indicators/Yansong_biomassDistribBySize_Simu0.csv")
biomass_by_size <- read.csv(biomass_by_size_path, skip = 1)

# année 2002, 2012 et 2021
biomass_by_size_2002 <- biomass_by_size[1:26,-1]
biomass_by_size_2012 <- biomass_by_size[261:286,-1]
biomass_by_size_2021 <- biomass_by_size[495:520,-1]

# transformer en format long
biomass_by_size_2002_long <- tidyr::gather(biomass_by_size_2002, key = "species", value = "simulated", -Size)
biomass_by_size_2012_long <- tidyr::gather(biomass_by_size_2012, key = "species", value = "simulated", -Size)
biomass_by_size_2021_long <- tidyr::gather(biomass_by_size_2021, key = "species", value = "simulated", -Size)

# enlever les catégories de taille sans biomasse 
biomass_by_size_2002_long <- dplyr::filter(biomass_by_size_2002_long, simulated > 0.1)
biomass_by_size_2012_long <- dplyr::filter(biomass_by_size_2012_long, simulated > 0.1)
biomass_by_size_2021_long <- dplyr::filter(biomass_by_size_2021_long, simulated > 0.1)


biomass_by_size_2002_plot <- ggplot(biomass_by_size_2002_long) + 
  geom_col(aes(x = Size, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by size in 2002") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_size_2002.png",sep=""), biomass_by_size_2002_plot, width = 10, height = 5, dpi=600)

biomass_by_size_2012_plot <- ggplot(biomass_by_size_2012_long) + 
  geom_col(aes(x = Size, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by size in 2012") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_size_2012.png",sep=""), biomass_by_size_2012_plot, width = 10, height = 5, dpi=600)

biomass_by_size_2021_plot <- ggplot(biomass_by_size_2021_long) + 
  geom_col(aes(x = Size, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("length (cm)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by size in 2021") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_size_2021.png",sep=""), biomass_by_size_2021_plot, width = 10, height = 5, dpi=600)

####### 4.4 biomass distribution by age #######
biomass_by_age_path <- file.path(results_path,"Indicators/Yansong_biomassDistribByAge_Simu0.csv")
biomass_by_age <- read.csv(biomass_by_age_path, skip = 1)

# année 2002, 2012 et 2021
biomass_by_age_2002 <- biomass_by_age[1:26,-1]
biomass_by_age_2012 <- biomass_by_age[261:286,-1]
biomass_by_age_2021 <- biomass_by_age[495:520,-1]

# transformer en format long
biomass_by_age_2002_long <- tidyr::gather(biomass_by_age_2002, key = "species", value = "simulated", -Age)
biomass_by_age_2012_long <- tidyr::gather(biomass_by_age_2012, key = "species", value = "simulated", -Age)
biomass_by_age_2021_long <- tidyr::gather(biomass_by_age_2021, key = "species", value = "simulated", -Age)

# enlever les catégories de taille sans biomasse 
biomass_by_age_2002_long <- dplyr::filter(biomass_by_age_2002_long, simulated > 0.1)
biomass_by_age_2012_long <- dplyr::filter(biomass_by_age_2012_long, simulated > 0.1)
biomass_by_age_2021_long <- dplyr::filter(biomass_by_age_2021_long, simulated > 0.1)


biomass_by_age_2002_plot <- ggplot(biomass_by_age_2002_long) + 
  geom_col(aes(x = Age, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("age (y)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by age in 2002") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_age_2002.png",sep=""), biomass_by_age_2002_plot, width = 10, height = 5, dpi=600)

biomass_by_age_2012_plot <- ggplot(biomass_by_age_2012_long) + 
  geom_col(aes(x = Age, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("age (y)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by age in 2012") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_age_2012.png",sep=""), biomass_by_age_2012_plot, width = 10, height = 5, dpi=600)

biomass_by_age_2021_plot <- ggplot(biomass_by_age_2021_long) + 
  geom_col(aes(x = Age, y = simulated), fill = "darkblue") + 
  facet_wrap(~species, scales = "free") +
  xlab("age (y)") +
  ylab("biomass (t)") +
  ggtitle("Biomass distribution by age in 2021") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"biomass_by_age_2021.png",sep=""), biomass_by_age_2021_plot, width = 10, height = 5, dpi=600)

###### 5. Niveau trophique ######
# 
# trophic_path <- file.path(results_path, "Trophic/Yansong_meanTL_Simu8.csv")
# trophic_level <- read.csv(trophic_path,skip = 1)
# trophic_level <- tidyr::gather(trophic_level, key = "species", value = "simulated", -Time)
# 
# ggplot(trophic_level) + 
#   geom_line(aes(Time,simulated)) +
#   facet_wrap(~species) 
# # pas de tendence observée
# 
# biomass_by_TL <- read.csv("calibration-12-29/Indicators/Yansong_biomassDistribByTL_Simu8.csv",skip = 1)
# 
# # année 2012
# biomass_by_TL_2012 <- biomass_by_TL[biomass_by_TL$Time==11,-1]
# biomass_by_TL_2012_long <- tidyr::gather(biomass_by_TL_2012, key = "species", value = "biomass", -TL)
# biomass_by_TL_2012_long <- biomass_by_TL_2012_long %>%
#   dplyr::filter(biomass > 0.1)
# 
# ggplot(biomass_by_TL_2012_long) +
#   geom_col(aes(TL,biomass)) + 
#   facet_wrap(~species, scales = "free") +
#   xlab("Trophic Level")

###### 6. Mortalités ######
# before running the code, add a word to the empty cell in each table, to prevent having ',' at the end of each line
mortality_path <- file.path(results_path,"Mortality")
list_mortality <- list.files(mortality_path,"Yansong_mortalityRate.*Simu8.",full.names = TRUE)
species_name <- stringr::str_match(list_mortality, "mortalityRate-(.*?)(?=_Simu)")[,2]

for(species in 1:16){
  # load data
  original_data <- readr::read_csv(list_mortality[species], col_names = FALSE, skip = 1)
  
  # transpose
  mortality <- original_data %>%
    select(-X1) %>%
    t() %>%
    data.frame()
  
  # rename colomns
  colnames(mortality) <- c("source","stage",c(2002:2021))
  
  # transform to long format
  mortality_long <- mortality %>%
    tidyr::pivot_longer(cols = c(3:22), names_to = "Year", values_to = "mortality")
  
  # convert mortality value to numeric
  mortality_long$mortality <- as.numeric(mortality_long$mortality)
  
  mortality_plot <- ggplot(mortality_long, aes(x = Year, y = mortality, fill = source)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ stage) + 
    theme_minimal()+
    ggtitle(paste("mortality sources of",species_name[species]))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"))
  
  ggsave(file.path("figures",results_path,"mortality",paste("mortality_",species_name[species],".png",sep = "")),mortality_plot,width = 10,height = 5, dpi=600)  
}  

###### 7. Trophic ######
###### 7.1 Diet ######
diet_path <- file.path(results_path,"/Trophic/Yansong_dietMatrix_Simu0.csv")
diet_example <- readr::read_csv(diet_path, skip = 1)
# transform to long format
diet_example_long <- diet_example %>%
  tidyr::pivot_longer(cols = c(3:18), names_to = "Predator", values_to = "Percentage")
# create colomn "year"
diet_example_long <- diet_example_long %>%
  dplyr::mutate(Year=Time+2001) %>%
  select(-Time)

# set species order
diet_example_long$Prey <- factor(diet_example_long$Prey, levels = unique(diet_example_long$Prey))

# set colour palette
extended_colors <- c(viridis_pal(option = "C")(16),brewer.pal(11, "Paired"))

diet_plot <- ggplot(diet_example_long, aes(x = Year, y = Percentage, fill = Prey)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Predator) + 
  scale_fill_manual(values = extended_colors) +
  theme_minimal()+
  ggtitle("Diet composition")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"))

ggsave(file.path("figures",results_path,"diet.png"), diet_plot, width = 10, height = 5, dpi=600)



###### 7.2 Predation pressure ######
# load data
predation_path <- file.path(results_path,"/Trophic/Yansong_predatorPressure_Simu8.csv")
pred_pressure_example <- readr::read_csv(predation_path, skip = 1)
# transform to long format
pred_pressure_example_long <- pred_pressure_example %>%
  tidyr::pivot_longer(cols = c(3:18), names_to = "Predator", values_to = "Biomass_t")
# create colomn "year"
pred_pressure_example_long <- pred_pressure_example_long %>%
  dplyr::mutate(Year=Time+2001) %>%
  select(-Time)

# set species order
pred_pressure_example_long$Predator <- factor(pred_pressure_example_long$Predator, levels = unique(pred_pressure_example_long$Predator))

# set colour palette
colour_palette <- c(viridis_pal(option = "C")(4),brewer.pal(12, "Paired"))

pred_pressure_plot <- ggplot(pred_pressure_example_long, aes(x = Year, y = Biomass_t, fill = Predator)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Prey, scales = "free") + 
  scale_fill_manual(values = colour_palette) +
  theme_minimal()+
  ggtitle("predation pressure")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"))
ggsave(file.path("figures",results_path,"pred_pressure.png"), pred_pressure_plot, width = 20, height = 10, dpi=600)

###### 7.3 Diet by age ######
# diet_by_age_path <- file.path(results_path,"/Trophic/Yansong_dietMatrixDistribByAge-whiting_Simu0.csv")
# diet_by_age_example <- readr::read_csv(diet_by_age_path, skip = 1)
# # transform to long format
# diet_by_age_example_long <- diet_by_age_example %>%
#   tidyr::pivot_longer(cols = c(3:29), names_to = "Prey", values_to = "Biomass_t")
# # create colomn "year"
# diet_by_age_example_long <- diet_by_age_example_long %>%
#   dplyr::mutate(Year=Time+2001) %>%
#   select(-Time)
# 
# # create colomn "year"
# diet_by_age_example_long <- diet_by_age_example_long %>%
#   dplyr::filter(Age <= 10)
# 
# # set species order
# diet_by_age_example_long$Prey <- factor(diet_by_age_example_long$Prey, levels = unique(diet_by_age_example_long$Prey))
# 
# # set colour palette
# extended_colors <- c(viridis_pal(option = "C")(16),brewer.pal(11, "Paired"))
# 
# diet_by_age_plot <- ggplot(diet_by_age_example_long, aes(x = Year, y = Biomass_t, fill = Prey)) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_wrap(~ Age) + 
#   scale_fill_manual(values = extended_colors) +
#   theme_minimal()+
#   ggtitle("Diet composition by age")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.background = element_rect(fill = "white"))
# 
# ggsave(file.path("figures",results_path,"diet_by_age_whiting.png"), diet_by_age_plot, width = 10, height = 5, dpi=600)
# 
# 

# Define a list of species
species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")
life_span_list <- c(9,10,3,19,4,24,5,19,14,14,16,10,14,1,1,19)

# Loop through each species
for (i in seq_along(species_list)) {
  species <- species_list[i]
  life_span <- life_span_list[i]
  
  # Build file path
  diet_by_age_path <- file.path(results_path, paste0("Trophic/Yansong_dietMatrixDistribByAge-", species, "_Simu0.csv"))
  
  # Read data
  diet_by_age_data <- readr::read_csv(diet_by_age_path, skip = 1)
  
  # Transform to long format
  diet_by_age_data_long <- diet_by_age_data %>%
    pivot_longer(cols = c(3:29), names_to = "Prey", values_to = "Biomass_t") %>%
    mutate(Year = Time + 2001) %>%
    select(-Time) %>%
    filter(Age <= life_span)
  
  # Set Prey column order
  diet_by_age_data_long$Prey <- factor(diet_by_age_data_long$Prey, levels = unique(diet_by_age_data_long$Prey))
  
  # Set color palette
  extended_colors <- c(viridis_pal(option = "C")(16), brewer.pal(11, "Paired"))
  
  # Create plot
  diet_by_age_plot <- ggplot(diet_by_age_data_long, aes(x = Year, y = Biomass_t, fill = Prey)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ Age) + 
    scale_fill_manual(values = extended_colors) +
    theme_minimal() +
    ggtitle(paste("Diet composition by age -", species)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"))
  
  # Save plot
  ggsave(file.path("figures", results_path, "diet_by_age", paste0("diet_by_age_", species, ".png")), diet_by_age_plot, width = 10, height = 5, dpi = 600)
}

