# Comparaison des résultats des simulations
# Auteur : Yansong Huang
# Date de création : 2023-05-03

library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(gridExtra)
library(ncdf4)
library(readxl)

# chemin pour tous les résultats
results_path <- "test-06-05-2"
replication_number <- 10

###### 1. série temporelle biomasse ######
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

if (replication_number == 10)
{ # sorties du modèle
list_biomass <- list.files(results_path,"Yansong_biomass_Simu.",full.names = TRUE)

# define a dataframe for the biomass of all species
biomass_total <- data.frame(row.names = c("year","species","biomass_output_mean","biomass_output_sd"))

biomass_total <- data.frame(
  year = integer(),  
  species = character(), 
  biomass_output_mean = numeric(), 
  biomass_output_sd = numeric()
)

for (species in 1:16){
  biomass_species <- data.frame(
    year=c(2002:2021),
    species = rep("",20), 
    stringsAsFactors = FALSE
  )
  for (simulation in 1:10){
    biomass_brut <- read.csv(list_biomass[simulation],skip = 1)
    # standardise les colonnes des indices relatifs
    biomass_brut_scaled <- biomass_brut
    biomass_brut_scaled[,biomass_relative_species] <- scale(biomass_brut[,biomass_relative_species])
    
    biomass_species <- cbind(biomass_species, biomass = biomass_brut_scaled[,species+1])
  }
  # put the species name in the column
  biomass_species$species[1:20] <- colnames(biomass_brut[species+1])
  
  # calculate mean and sd among 10 simulations
  biomass_species$biomass_output_mean <- rowMeans(biomass_species[,3:12])
  biomass_species$biomass_output_sd <- apply(biomass_species[,3:12],1,sd)
  
  # regroup the biomass of all species
  biomass_total <- rbind(biomass_total,biomass_species[,c(1,2,13,14)])
}

# regroup the model outputs with objective data
biomass_output_data <- cbind(biomass_total,biomass_reference_long) 
# delete repeated rows
biomass_comparison <- biomass_output_data[,-c(5,6)]

library(ggplot2)
biomass_comparison_plot <- ggplot(data=biomass_comparison)+
  geom_point(aes(x=year, y=biomass_data, color="darkred"))+
  geom_line(aes(x=year, y = biomass_output_mean, color="darkblue"))+
  geom_ribbon(aes(x=year,
                  ymin = biomass_output_mean - biomass_output_sd,
                  ymax = biomass_output_mean + biomass_output_sd,
                  fill="blue"),
              alpha = 0.2) +
  # 设置颜色图例
  scale_color_manual(
    name = element_blank(),
    values = c("darkred" = "darkred", "darkblue" = "darkblue"),
    breaks = c("darkred", "darkblue"),
    labels = c("Objective data", "Mean model outputs"),
    guide = guide_legend(order = 1) # 图例顺序1
  ) +
  # 设置填充图例
  scale_fill_manual(
    name = element_blank(),
    values = c("blue" = "blue"),
    breaks = c("blue"),
    labels = c("SD model outputs"),
    guide = guide_legend(order = 2) # 图例顺序2
  ) +
  # 分面
  facet_wrap(~species, scales = "free_y") +
  ylab("Biomass (t) or biomass index") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank())

ggsave(file.path("figures",results_path,"biomass_indices.png",sep=""),biomass_comparison_plot, width = 10, height = 5, dpi=600)
} else if(replication_number==1)
{
  biomass_file <- file.path(results_path,"Yansong_biomass_Simu0.csv")
  
  # define a dataframe for the biomass of all species
  biomass_total <- data.frame(row.names = c("year","species","biomass"))
  
  biomass_total <- data.frame(
    year = integer(),  
    species = character(), 
    biomass = numeric()
  )
  
  for (species in 1:16){
    biomass_species <- data.frame(
      year=c(2002:2021),
      species = rep("",20), 
      biomass = rep(NA,20),
      stringsAsFactors = FALSE
    )

      biomass_brut <- read.csv(biomass_file, skip=1)
      # standardise les colonnes des indices relatifs
      biomass_brut_scaled <- biomass_brut
      biomass_brut_scaled[,biomass_relative_species] <- scale(biomass_brut[,biomass_relative_species])
      biomass_species$biomass <- biomass_brut_scaled[,species+1]

    # put the species name in the column
    biomass_species$species[1:20] <- colnames(biomass_brut[species+1])
    
    # regroup the biomass of all species
    biomass_total <- rbind(biomass_total,biomass_species)
  }
  
  # regroup the model outputs with objective data
  biomass_output_data <- cbind(biomass_total,biomass_reference_long) 
  # delete repeated rows
  biomass_comparison <- biomass_output_data[,-c(4,5)]
  
  biomass_comparison_plot <- ggplot(data=biomass_comparison)+
    geom_point(aes(x=year, y=biomass_data, color="darkred"))+
    geom_line(aes(x=year, y=biomass, color="darkblue"))+
    scale_color_manual(name = element_blank(),
                       values = c("darkred" = "darkred", "darkblue" = "darkblue"),
                       breaks = c("darkred", "darkblue"),
                       labels = c("objective data", "model outputs"))+
    facet_wrap(~species, scales = "free_y") +
    ylab("biomass (t) or biomass index")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank())
  
  # force lower limit at -2 (approximation of 0 for evaluated species)
  biomass_comparison_plot <- biomass_comparison_plot +
    geom_blank(data = data.frame(year = unique(biomass_comparison$year), simulated = rep(-2,20)),
               aes(x = year, y = simulated))
  
  ggsave(file.path("figures",results_path,"biomass_indices.png",sep=""),biomass_comparison_plot, width = 10, height = 5, dpi=600)
  
} else {
  print("en cours...")
}

###### 2.1 série temporelle capture ######

# Read objective data
yield_data <- read.csv("Yansong_yield_year.csv")

# Convert data from wide to long format
yield_data_long <- gather(yield_data, key = "species", value = "yield_data", -year)

# Define a function to process simulation outputs for each species
process_species_yield <- function(species_idx, list_yield) {
  # Get species name
  species_name <- colnames(read.csv(list_yield[1], skip = 1))[species_idx + 1]
  
  # Create a dataframe to store species' years and simulation outputs
  yield_species <- data.frame(year = 2002:2021, species = species_name, stringsAsFactors = FALSE)
  
  # Loop through each simulation result file
  for (simulation in seq_len(replication_number)) {
    yield_brut <- read.csv(list_yield[simulation], skip = 1)
    
    # Add each simulation's result to the dataframe
    yield_species <- cbind(yield_species, yield = yield_brut[, species_idx + 1])
  }
  
  # Calculate mean and standard deviation of simulation outputs for each species
  colnames(yield_species) <- make.unique(colnames(yield_species))
  
  yield_species <- yield_species %>%
    mutate(yield_output_mean = rowMeans(select(., -c(year, species))),
           yield_output_sd = apply(select(., -c(year, species)), 1, sd))
  
  # Select columns to output
  yield_species <- yield_species %>%
    select(year, species, yield_output_mean, yield_output_sd)
  
  return(yield_species)
}

# Process data based on different replication numbers
if (replication_number > 1) {
  # Get list of simulation output files
  list_yield <- list.files(results_path, "Yansong_yield_Simu.", full.names = TRUE)
  
  # Process simulation outputs for all species
  yield_total <- bind_rows(lapply(1:16, process_species_yield, list_yield = list_yield))
  
  # Merge simulation outputs with objective data
  yield_output_data <- merge(yield_total, yield_data_long, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  
  # Create comparison plot
  yield_comparison_plot <- ggplot(data = yield_output_data) +
    # 绘制点
    geom_point(aes(x = year, y = yield_data, color = "darkred")) +
    # 绘制线
    geom_line(aes(x = year, y = yield_output_mean, color = "darkblue")) +
    # 绘制阴影
    geom_ribbon(aes(
      x = year,
      ymin = pmax(yield_output_mean - yield_output_sd, 0),
      ymax = yield_output_mean + yield_output_sd,
      fill = "blue"
    ), alpha = 0.2) +
    # 设置颜色图例
    scale_color_manual(
      name = element_blank(),
      values = c("darkred" = "darkred", "darkblue" = "darkblue"),
      breaks = c("darkred", "darkblue"),
      labels = c("Objective data", "Mean model outputs"),
      guide = guide_legend(order = 1) # 图例顺序1
    ) +
    # 设置填充图例
    scale_fill_manual(
      name = element_blank(),
      values = c("blue" = "blue"),
      breaks = c("blue"),
      labels = c("SD model outputs"),
      guide = guide_legend(order = 2) # 图例顺序2
    ) +
    # 分面
    facet_wrap(~species, scales = "free_y") +
    ylab("Yield (t)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.background = element_rect(fill = "white"),
      legend.margin = margin(0, 1, 0, 1),
      legend.title = element_blank(),
      legend.position = c(0.7, 0.04)
    )
  
  
  # Save the plot as an image file
  ggsave(file.path("figures", results_path, "yield.png"), yield_comparison_plot, width = 10, height = 5, dpi = 600)
  
} else if (replication_number == 1) {
  # Process single replication case
  
  # Get single simulation output file
  yield_file <- file.path(results_path, "Yansong_yield_Simu0.csv")
  yield_brut <- read.csv(yield_file, skip = 1)
  
  # Create a dataframe to store species' years and simulation outputs
  yield_total <- data.frame(year = 2002:2021, species = character(20), yield = numeric(20))
  
  # Loop through each species
  for (species in 1:16) {
    yield_species <- data.frame(
      year = 2002:2021,
      species = rep("", 20), 
      yield = yield_brut[, species + 1],
      stringsAsFactors = FALSE
    )
    
    yield_species$species <- colnames(yield_brut)[species + 1]
    yield_total <- rbind(yield_total, yield_species)
  }
  
  # Merge simulation outputs with objective data
  yield_output_data <- merge(yield_total, yield_data_long, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  
  # Create comparison plot
  yield_comparison_plot <- ggplot(data = yield_output_data) +
    geom_point(aes(x = year, y = yield_data, color = "darkred")) +
    geom_line(aes(x = year, y = yield, color = "darkblue")) +
    scale_color_manual(name = element_blank(),
                       values = c("darkred" = "darkred", "darkblue" = "darkblue"),
                       breaks = c("darkred", "darkblue"),
                       labels = c("objective data", "model outputs")) +
    facet_wrap(~species, scales = "free_y") +
    ylab("yield (t)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"),
          legend.title = element_blank())
  
  # Save the plot as an image file
  ggsave(file.path("figures", results_path, "yield.png"), yield_comparison_plot, width = 10, height = 5, dpi = 600)
  
} else {
  # Code for handling other cases
  print("en cours...")
}

###### 2.2 capture par flottille ######

# Read observed yield data by fleet
landing_france <- read.csv("landing_france_by_fleet.csv")
landing_bottom_trawlers <- landing_france %>% filter(fleet=="trawlers_dem")
landing_midwater_trawlers <- landing_france %>% filter(fleet=="trawlers_pel")
landing_netters <- landing_france %>% filter(fleet=="netters")
landing_others <- read.csv("landings_others.csv")


# add species names
species_list <- c("lesserSpottedDogfish", "redMullet", "pouting", "whiting", "poorCod", "cod", "dragonet", "sole", "plaice", "horseMackerel", "mackerel", "herring", "sardine", "squids", "cuttlefish", "thornbackRay")

# function for processing catch by fleet
  process_species_yield_fleet <- function(species_idx, species_list,  list_yield_nc, fleet) {
    # Get species name
    species_name <- species_list[species_idx]
    
    # Create a dataframe to store species' years and simulation outputs
    yield_species <- data.frame(year = 2002:2021, species = species_name, stringsAsFactors = FALSE)
    
    # Loop through each simulation result file
    for (simulation in seq_len(replication_number)) {
      # Open nc file and get the catch by fleet
      catch_by_fleet <- nc_open(list_yield_nc[simulation]) %>%
        ncvar_get(varid = "landings")
      
      catch_bottom_trawler <- catch_by_fleet[fleet,,] %>%
        t() %>%
        as.matrix() %>%
        { array(., c(24, 20, ncol(.))) } %>%
        apply(c(2, 3), sum)
      
      # Add each simulation's result to the dataframe
      yield_species <- yield_species %>%
        mutate(!!paste0("yield_sim_", simulation) := catch_bottom_trawler[, species_idx])
    }
    
    # calculate mean and sd for each species
    yield_species <- yield_species %>%
      mutate(yield_output_mean = rowMeans(select(., -c(year, species))),
             yield_output_sd = apply(select(., -c(year, species)), 1, sd))
    
    # select colomns for output
    yield_species <- yield_species %>%
      select(year, species, yield_output_mean, yield_output_sd)
    
    return(yield_species)
  }

# Process data based on different replication numbers
if (replication_number > 1) {
  # Get list of simulation output files
  list_yield_nc <- list.files(results_path, "Yansong_yieldByFishery_Simu*", full.names = TRUE)
  
  # Process simulation outputs for all species
  yield_bottom_trawlers <- bind_rows(lapply(1:length(species_list), function(species_idx) {
    process_species_yield_fleet(species_idx, species_list = species_list, list_yield_nc = list_yield_nc, fleet = 1)
  }))
  yield_midwater_trawlers <- bind_rows(lapply(1:length(species_list), function(species_idx) {
    process_species_yield_fleet(species_idx, species_list = species_list, list_yield_nc = list_yield_nc, fleet = 2)
  }))
  yield_netters <- bind_rows(lapply(1:length(species_list), function(species_idx) {
    process_species_yield_fleet(species_idx, species_list = species_list, list_yield_nc = list_yield_nc, fleet = 3)
  }))
  yield_others <- bind_rows(lapply(1:length(species_list), function(species_idx) {
    process_species_yield_fleet(species_idx, species_list = species_list, list_yield_nc = list_yield_nc, fleet = 4)
  }))
  
  # Merge simulation outputs with objective data
  yield_bottom_trawlers_output <- left_join(yield_bottom_trawlers, landing_bottom_trawlers, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  yield_midwater_trawlers_output <- left_join(yield_midwater_trawlers, landing_midwater_trawlers, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  yield_netters_output <- left_join(yield_netters, landing_netters, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  yield_others_output <- left_join(yield_others, landing_others, by = c("year", "species")) %>%
    filter(!species %in% c("poorCod", "dragonet")) # Filter out non-exploited species
  
  # function for plotting
  create_yield_plot <- function(data, title) {
    ggplot(data = data) +
      geom_line(aes(x = year, y = yield_data, color = "darkred")) +
      geom_line(aes(x = year, y = yield_output_mean, color = "darkblue")) +
      geom_ribbon(aes(x = year,
                      ymin = pmax(yield_output_mean - yield_output_sd, 0),
                      ymax = yield_output_mean + yield_output_sd,
                      fill = "blue"),
                  alpha = 0.2) +
      scale_color_manual(name = element_blank(),
                         values = c("darkred" = "darkred", "darkblue" = "darkblue"),
                         breaks = c("darkred", "darkblue"),
                         labels = c("objective data", "mean model outputs")) +
      scale_fill_manual(name = element_blank(),
                        values = c("blue" = "blue"),
                        breaks = c("blue"),
                        labels = c("sd model outputs")) +
      facet_wrap(~species, scales = "free_y") +
      ylab("catch (t)") +
      ggtitle(title) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.background = element_rect(fill = "white"),
            legend.margin = margin(0, 1, 0, 1),
            legend.title = element_blank(),
            legend.position = c(0.7, 0.04))
  }
  
  # apply the plotting function to fleets
  yield_bottom_trawlers_plot <- create_yield_plot(yield_bottom_trawlers_output, "Bottom Trawlers Catch")
  yield_midwater_trawlers_plot <- create_yield_plot(yield_midwater_trawlers_output, "Midwater Trawlers Catch")
  yield_netters_plot <- create_yield_plot(yield_netters_output, "Netters Catch")
  yield_others_plot <- create_yield_plot(yield_others_output, "Others Catch")
  
  print(yield_bottom_trawlers_plot)
  print(yield_midwater_trawlers_plot)
  print(yield_netters_plot)
  print(yield_others_plot)
  
  ggsave(file.path("figures", results_path, "yield_bottom_trawlers.png"), yield_bottom_trawlers_plot, width = 10, height = 5, dpi = 600)
  ggsave(file.path("figures", results_path, "yield_midwater_trawlers.png"), yield_midwater_trawlers_plot, width = 10, height = 5, dpi = 600)
  ggsave(file.path("figures", results_path, "yield_netters.png"), yield_netters_plot, width = 10, height = 5, dpi = 600)
  ggsave(file.path("figures", results_path, "yield_others.png"), yield_others_plot, width = 10, height = 5, dpi = 600)
  
} else {
  # Code for handling other cases
  print("en cours...")
}


###### 3. Validation de courbe de croissance ######
growth_path <- file.path(results_path,"AgeIndicators/Yansong_meanSizeDistribByAge_Simu0.csv")
growth_raw <- read.csv(growth_path,skip = 1)

# 移除 Time 列
growth_raw <- growth_raw %>% select(-Time)

# 转换为长数据格式
growth_long <- growth_raw %>%
  pivot_longer(cols = -Age, names_to = "Species", values_to = "Length")

# 计算平均值和标准差
growth_summary <- growth_long %>%
  group_by(Age, Species) %>%
  summarise(
    Mean_Length = mean(Length, na.rm = TRUE),
    SD_Length = sd(Length, na.rm = TRUE),
    .groups = "drop"
  )

# 年龄区间取中间
growth_summary$Age <- growth_summary$Age +0.5

####### définir la croissance Von Bertalanffy ######
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

age_SYC <- seq(0.5,10,0.1)
age_MUR <- seq(1,11,0.1)
age_BIB <- seq(0.5,4,0.1)
age_WHG <- seq(1,20,0.1)
age_POD <- seq(0.5,5,0.1)
age_COD <- seq(0.5,25,0.1)
age_LYY <- seq(0.5,6,0.1)
age_SOL <- seq(0.5,20,0.1)
age_PLE <- seq(0.5,15,0.1)
age_HOM <- seq(1,15,0.1)
age_MAC <- seq(1,17,0.1)
age_HER <- seq(0.5,11,0.1)
age_PIL <- seq(0.5,15,0.1)
age_RJC <- seq(0,20,0.1)
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
######

growth_SYC_plot <- ggplot() +
  geom_line(aes(age_SYC,length_vb_SYC), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="lesserSpottedDogfish",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="lesserSpottedDogfish",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,10) +
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Lesser spotted dogfish")+
  theme_bw()

growth_MUR_plot <- ggplot()+
  geom_line(aes(age_MUR,length_vb_MUR), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="redMullet",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="redMullet",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,11)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Red mullet")+
  theme_bw()

growth_BIB_plot <- ggplot()+
  geom_line(aes(age_BIB,length_vb_BIB), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="pouting",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="pouting",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,4)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Pouting")+
  theme_bw()

growth_WHG_plot <- ggplot()+
  geom_line(aes(age_WHG,length_vb_WHG), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="whiting",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="whiting",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,20)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Whiting")+
  theme_bw()

growth_POD_plot <- ggplot()+
  geom_line(aes(age_POD,length_vb_POD), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="poorCod",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="poorCod",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,5)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Poor cod")+
  theme_bw()

growth_COD_plot <- ggplot()+
  geom_line(aes(age_COD,length_vb_COD), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="cod",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="cod",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,25)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Cod")+
  theme_bw()

growth_LYY_plot <- ggplot()+
  geom_line(aes(age_LYY,length_vb_LYY), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="dragonet",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="dragonet",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,6)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Dragonet")+
  theme_bw()

growth_SOL_plot <- ggplot()+
  geom_line(aes(age_SOL,length_vb_SOL), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="sole",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="sole",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,20)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Sole")+
  theme_bw()

growth_PLE_plot <- ggplot()+
  geom_line(aes(age_PLE,length_vb_PLE), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="plaice",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="plaice",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,15)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Plaice")+
  theme_bw()

growth_HOM_plot <- ggplot()+
  geom_line(aes(age_HOM,length_vb_HOM), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="horseMackerel",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="horseMackerel",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,15)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Horse mackerel")+
  theme_bw()

growth_MAC_plot <- ggplot()+
  geom_line(aes(age_MAC,length_vb_MAC), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="mackerel",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="mackerel",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,17)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Mackerel")+
  theme_bw()

growth_HER_plot <- ggplot()+
  geom_line(aes(age_HER,length_vb_HER), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="herring",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="herring",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,11)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Herring")+
  theme_bw()

growth_PIL_plot <- ggplot()+
  geom_line(aes(age_PIL,length_vb_PIL), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="sardine",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="sardine",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,15)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Sardine")+
  theme_bw()

growth_RJC_plot <- ggplot()+
  geom_line(aes(age_RJC,length_vb_RJC), colour ="darkgrey") +
  geom_line(data=growth_summary[growth_summary$Species=="thornbackRay",],
            mapping=aes(x=Age,y=Mean_Length)) + 
  geom_errorbar(data=growth_summary[growth_summary$Species=="thornbackRay",],
                aes(x=Age,ymin = Mean_Length - SD_Length, ymax = Mean_Length + SD_Length), width = 0.2) +
  xlim(0,20)+
  xlab("age (y)") +
  ylab("length (cm)") +
  ggtitle("Thornback ray")+
  theme_bw()


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


###### 4.1 Capture en taille ######

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
                    labels = "objective data")+
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
                     labels = "objective data")+
  scale_fill_manual(name = element_blank(),
                    values = c("darkblue" = "darkblue"),
                    breaks = "darkblue",
                    labels = "model outputs")+
  xlab("length (cm)") +
  ylab("abundance") +
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
                     labels = "objective data")+
  scale_fill_manual(name = element_blank(),
                    values = c("darkblue" = "darkblue"),
                    breaks = "darkblue",
                    labels = "model outputs")+
  xlab("length (cm)") +
  ylab("abundance") +
  ggtitle("Catch distribution by size in 2021") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))
ggsave(file.path("figures",results_path,"catch_at_length_2021.png",sep=""), catch_at_length_2021_plot, width = 10, height = 5, dpi=600)

###### 4.2 série temporelle de taille moyenne des captures #######

catch_at_length_list <- list.files(paste(results_path, "/SizeIndicators/",sep=""),"Yansong_yieldNDistribBySize_Simu.", full.names = TRUE)

mean_catch_size_total <- data.frame()

for(simulation in  1:10){
  catch_at_length_brut <- read.csv(catch_at_length_list[simulation], skip = 1)
  catch_at_length_long <- tidyr::gather(catch_at_length_brut, key = "species", value = "simulated", -c(Time,Size))
  catch_at_length_long <- dplyr::filter(catch_at_length_long, simulated > 0.1)
  mean_catch_size <- catch_at_length_long %>%
    mutate(year=Time+2001) %>%
    select(-Time) %>%
    group_by(year,species) %>%
    summarise(simulated = weighted.mean(Size, w=simulated))
  
  if (simulation == 1)
    mean_catch_size_total <- mean_catch_size
  else
    mean_catch_size_total <- cbind(mean_catch_size_total,mean_catch_size$simulated)
}

mean_catch_size_total$simulated_mean <- rowMeans(mean_catch_size_total[,3:12])
mean_catch_size_total$simulated_sd <- apply(mean_catch_size_total[,3:12],1,sd)

# charge les données observées
observed_mean_catch_size <- readRDS("observed_mean_catch_size_by_years_SACROIS_20240109.rds")
observed_mean_catch_size <- observed_mean_catch_size %>%
  rename(species=spp)

mean_catch_size_comparison <- mean_catch_size_total %>%
  select(c("year","species","simulated_mean","simulated_sd")) %>%
  left_join(observed_mean_catch_size, by=c("year","species"))

mean_catch_size_plot <- ggplot(mean_catch_size_comparison) +
  geom_point(aes(x=year,y=observed,color = "darkred"))+
  geom_line(aes(x=year,y=simulated_mean, color = "darkblue")) +
  geom_ribbon(data = mean_catch_size_comparison, aes(x = year,
                                                     ymin = simulated_mean - simulated_sd,
                                                     ymax = simulated_mean + simulated_sd,
                                                     fill = "blue"), # 使用 "blue"
              alpha = 0.2) +
  facet_wrap(~species,scales = "free")+
  scale_color_manual(
    name = element_blank(),
    values = c("darkred" = "darkred", "darkblue" = "darkblue"),
    breaks = c("darkred", "darkblue"),
    labels = c("Objective data", "Mean model outputs"),
    guide = guide_legend(order = 1) # 图例顺序1
  ) +
  # 设置填充图例
  scale_fill_manual(
    name = element_blank(),
    values = c("blue" = "blue"),
    breaks = c("blue"),
    labels = c("SD model outputs"),
    guide = guide_legend(order = 2) # 图例顺序2
  ) +
  ylab("Mean catch length (cm)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "white"),
        legend.margin = margin(0,1,0,1),
        legend.title = element_blank(),
        legend.position = c(0.7,0.04))

ggsave(file.path("figures",results_path,"mean_catch_size.png"), mean_catch_size_plot, width = 10, height = 5, dpi=600)


###### 4.3 biomass distribution by size #######
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

###### 4.4 biomass distribution by age #######
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
trophic_path <- file.path(results_path, "Trophic/Yansong_meanTL_Simu0.csv")
trophic_level <- read.csv(trophic_path,skip = 1)
trophic_level <- tidyr::gather(trophic_level, key = "species", value = "simulated", -Time)


biomass_by_TL <- read.csv(file.path(results_path,"Indicators/Yansong_biomassDistribByTL_Simu0.csv"),skip = 1)

biomass_by_TL_2002 <- biomass_by_TL[biomass_by_TL$Time == 1, -1]
biomass_by_TL_2002_long <- tidyr::gather(biomass_by_TL_2002, key = "species", value = "biomass", -TL)
biomass_by_TL_2002_long <- biomass_by_TL_2002_long %>%
  dplyr::filter(biomass > 0.1)

# année 2015
biomass_by_TL_2015 <- biomass_by_TL[biomass_by_TL$Time==14,-1]
biomass_by_TL_2015_long <- tidyr::gather(biomass_by_TL_2015, key = "species", value = "biomass", -TL)
biomass_by_TL_2015_long <- biomass_by_TL_2015_long %>%
  dplyr::filter(biomass > 0.1)

biomass_by_TL_2021 <- biomass_by_TL[biomass_by_TL$Time == 20, -1]
biomass_by_TL_2021_long <- tidyr::gather(biomass_by_TL_2021, key = "species", value = "biomass", -TL)
biomass_by_TL_2021_long <- biomass_by_TL_2021_long %>%
  dplyr::filter(biomass > 0.1)

cresson_2018 <- read_excel("cresson_et_al_2018.xlsx",sheet=2)
dict_species_2018 <- c(
  "Clupea_harengus"="herring",
  "Trachurus_trachurus"="horseMackerel",
  "Scomber_scombrus"="mackerel", 
  "Sardina_pilchardus"="sardine", 
  "Merlangius_merlangus"="whiting", 
  "Pleuronectes_platessa"="plaice", 
  "Solea_solea"="sole", 
  "Sepia_officinalis"="cuttlefish", 
  "Trisopterus_luscus"="pouting", 
  "Mullus_surmuletus"="redMullet", 
  "Scyliorhinus_canicula"="lesserSpottedDogfish", 
  "Loligo_sp."="squids", 
  "Raja_clavata"="thornbackRay", 
  "Gadus_morhua"="cod",
  "Callionymus_lyra"="dragonet",
  "Trisopterus_minutus"="poorCod"
)

cresson_2018$species <- dict_species_2018[match(cresson_2018$Science_name, names(dict_species_2018))]
cresson_2018 <- cresson_2018 %>%
  select("TrophLev","species") %>%
  group_by(species) %>%
  summarise(
    median_TL = median(TrophLev),
    max_TL = max(TrophLev),
    min_TL = min(TrophLev)
  )

TL_plot_2002 <- ggplot() +
  geom_violin(data = biomass_by_TL_2002_long, aes(x = species, y = TL, weight = biomass, group = species), scale = "width") +
  geom_point(data = cresson_2018, aes(x = species, y = median_TL), color = "red", size = 3) +  # données
  geom_segment(data = cresson_2018, aes(x = species, xend = species, y = min_TL, yend = max_TL), linetype = "dashed", color = "darkred") +  # 添加垂直参考线段
  labs(x = "Species", y = "Trophic Level", title = "Trophic Level Distribution in 2002") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

TL_plot_2015 <- ggplot() +
  geom_violin(data = biomass_by_TL_2015_long, aes(x = species, y = TL, weight = biomass, group = species), scale = "width") +
  geom_point(data = cresson_2018, aes(x = species, y = median_TL), color = "red", size = 3) +  # données
  geom_segment(data = cresson_2018, aes(x = species, xend = species, y = min_TL, yend = max_TL), linetype = "dashed", color = "darkred") +  # 添加垂直参考线段
  labs(x = "Species", y = "Trophic Level", title = "Trophic Level Distribution in 2015") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

TL_plot_2021 <- ggplot() +
  geom_violin(data = biomass_by_TL_2021_long, aes(x = species, y = TL, weight = biomass, group = species), scale = "width") +
  geom_point(data = cresson_2018, aes(x = species, y = median_TL), color = "red", size = 3) +  # données
  geom_segment(data = cresson_2018, aes(x = species, xend = species, y = min_TL, yend = max_TL), linetype = "dashed", color = "darkred") +  # 添加垂直参考线段
  labs(x = "Species", y = "Trophic Level", title = "Trophic Level Distribution in 2021") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path("figures",results_path,"trophic_level_2002.png",sep=""),TL_plot_2002, width = 10, height = 5, dpi=600)
ggsave(file.path("figures",results_path,"trophic_level_2015.png",sep=""),TL_plot_2015, width = 10, height = 5, dpi=600)
ggsave(file.path("figures",results_path,"trophic_level_2021.png",sep=""),TL_plot_2021, width = 10, height = 5, dpi=600)


###### 6. Mortality ######
# before running the code, add a word to the empty cell in each table, to prevent having ',' at the end of each line
mortality_path <- file.path(results_path,"Mortality")
list_mortality <- list.files(mortality_path,"Yansong_mortalityRate.*Simu0.",full.names = TRUE)
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
    facet_wrap(~ stage, scales = "free") + 
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

diet_example_long$Prey <- recode(
  diet_example_long$Prey,
  "diatoms" = "phytoplankton",
  "microPhytoplankton" = "phytoplankton",
  "heterotrophicFlagellates" = "zooplankton",
  "microZooplankton" = "zooplankton",
  "mesoZooplankton" = "zooplankton",
  "Macrozoo" = "zooplankton",
  "meioBenthos" = "benthos",
  "depositBenthos" = "benthos",
  "suspensionBenthos" = "benthos",
  "largeBenthos" = "benthos",
  "veryLargeBenthos" = "benthos"
)

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
predation_path <- file.path(results_path,"/Trophic/Yansong_predatorPressure_Simu0.csv")
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
  
  # diet composition as biomass
  diet_by_age_biomass_plot <- ggplot(diet_by_age_data_long, aes(x = Year, y = Biomass_t, fill = Prey)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_wrap(~ Age) + 
    scale_fill_manual(values = extended_colors) +
    theme_minimal() +
    ggtitle(paste("Diet composition by age (biomass) -", species)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"))
  
  # diet composition as percentage
  diet_by_age_percentage_plot <- ggplot(diet_by_age_data_long, aes(x = Year, y = Biomass_t, fill = Prey)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ Age) + 
    scale_fill_manual(values = extended_colors) +
    ylab("proportion of biomass") +
    theme_minimal() +
    ggtitle(paste("Diet composition by age (biomass percentage) -", species)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.background = element_rect(fill = "white"))
  
  # Save plot
  ggsave(file.path("figures", results_path, "diet_by_age", paste0("diet_by_age_biomass_", species, ".png")), diet_by_age_biomass_plot, width = 10, height = 5, dpi = 600)
  ggsave(file.path("figures", results_path, "diet_by_age", paste0("diet_by_age_percentage_", species, ".png")), diet_by_age_percentage_plot, width = 10, height = 5, dpi = 600)
}

