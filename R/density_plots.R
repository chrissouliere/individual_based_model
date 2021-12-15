# Plots (main paper and appendix) and statistical analysis

# Function to load and install packages
getPackages <- function(...) {
  
  pcks <- unlist(list(...))
  req <- lapply(pcks, require, character.only = TRUE)
  need <- pcks[req != TRUE]
  if(length(need) > 0) {
    install.packages(need)
    lapply(need, require, character.only = TRUE)
  }
  
}

# List packages needed for script
getPackages("ggplot2",     # Plotting
            "sf",          # Mapping
            "cowplot",     # Plots in Grids
            "ggrepel",     # Create floating text 
            "betareg",     # Fixed effect Beta Regression
            "glmmTMB",     # Mixed Effect Beta Regression
            "gamlss",      # Zero-inflated Beta Regression 
            "bbmle",       # AIC Tab
            "performance", # Model Diagnostic and Performance Metrics
            "DHARMa",      # Residual diagnostics for hierarchical (multi-level/mixed) regression models
            "mgcv",        # Fitting General Additive Models
            "Metrics",     # AUC
            "visibly",     # Visuals for Gam Check
            "gridExtra",   # Needed for visibly
            "tidyverse"    # Needed for mutate
)

# Read in cleaned location results and cleaned IBM results
location_results <- read.csv("output/BearIBMCleaned/bear_IBMv22_LocationCleanedRv22.csv", 
                             header = TRUE, stringsAsFactors = FALSE)

yearly_results <- read.csv("output/BearIBMCleaned/bear_IBMv22_ResultsCleanedRv22.csv",
                           header = TRUE, stringsAsFactors = FALSE)

# Order location results by run_number and location.who
location_results_re <- location_results[order(location_results$run_number, location_results$location.who), ]

# Merge location results with IBM results
df_all <- merge(location_results_re, yearly_results, by.x = c("run_number", "location.who"),
              by.y = c("run_number", "bear_who"), all = TRUE)

# relevel to older adult to adult and yearling/cub to attendant adult
df_all[which(as.logical(df_all$bear_cub)), "bear_age_class"] <- "adult_w_cub"
df_all[which(as.logical(df_all$bear_yearling)), "bear_age_class"] <- "adult_w_year"
df_all$bear_age_class<- factor(df_all$bear_age_class)
levels(df_all$bear_age_class) <- c("adult", "adult-w-off" , "adult-w-off","adult", "sub-adult")

# Merge sex grouping with age-class grouping
df_all$bear_sex_age_class <- factor(paste(df_all$bear_sex, df_all$bear_age_class, sep = "-"))

# Create current bci factor
#df_all$bear_current_bci_cat <- NA
df_all$bear_current_bci_cat[df_all$bear_current_bci < 0] <- "< 0"
df_all$bear_current_bci_cat[df_all$bear_current_bci >= 0 & df_all$bear_current_bci < 2.5] <- ">= 0 & < 2.5"
df_all$bear_current_bci_cat[df_all$bear_current_bci >= 2.5 & df_all$bear_current_bci < 5] <- ">= 2.5 & < 5"
df_all$bear_current_bci_cat[df_all$bear_current_bci >= 5] <- "> 5"
df_all$bear_current_bci_cat <- factor(df_all$bear_current_bci_cat, 
                                      levels = c("< 0",">= 0 & < 2.5", ">= 2.5 & < 5", "> 5"))

# Create bear survival rate factor
df_all$bear_survival_rates_cat[df_all$bear_survival_rates < 0.25] <- "< 0.25"
df_all$bear_survival_rates_cat[df_all$bear_survival_rates >= 0.25 & df_all$bear_survival_rates < 0.5] <- ">= 0.25 & < 0.5"
df_all$bear_survival_rates_cat[df_all$bear_survival_rates >= 0.5 & df_all$bear_survival_rates < 0.75] <- ">= 0.5 & < 0.75"
df_all$bear_survival_rates_cat[df_all$bear_survival_rates >= 0.75 & df_all$bear_survival_rates <= 1] <- ">= 0.75 & < 0.95"
df_all$bear_survival_rates_cat[df_all$bear_survival_rates >= 0.95] <- ">= 0.95"
df_all$bear_survival_rates_cat <- factor(df_all$bear_survival_rates_cat,
                                         levels = c("< 0.25", ">= 0.25 & < 0.5", ">= 0.5 & < 0.75", ">= 0.75 & < 0.95",
                                                    ">= 0.95"))

# Rows with -9999 in location
rows_9999 <- which(df_all[, "location.xcor"] == -9999 | df_all[, "location.ycor"] == -9999)
df_all[rows_9999, "location.xcor"] <- NA
df_all[rows_9999, "location.ycor"] <- NA

# Rows with 0 in location
rows_0 <- which(df_all[, "location.xcor"] == 0 | df_all[, "location.ycor"] == 0)
df_all[rows_0, "location.xcor"] <- NA
df_all[rows_0, "location.ycor"] <- NA

# Removed rows from df_all; makes sure when concatenating that the rows are not duplicated
df_all2 <- df_all[-c(rows_9999, rows_0), ]

# Keep hour 23 data for plotting maps (can't allocate enough ram)
df_all3 <- df_all2[df_all2$tick %in% sapply(seq(1, 191), function(x) x * 23), ]

# Convert coordinates to a sf object (point)
df_all3 <- sf::st_as_sf(df_all3, coords = c("location.xcor", "location.ycor"), crs = 26911, agr = "constant")

# Split data into males (adult and sub-adult males) and females (adult, sub-adult, and offspring) for graphing
# purposes
df_all3_male   <- df_all3[which(df_all3$bear_sex == "male"), ]
df_all3_female <- df_all3[which(df_all3$bear_sex == "female"), ]

# Get coordinates as a dataframe to plot in stat_density_2d
df_all3_male_Coords <- sf::st_coordinates(df_all3_male)
df_all3_female_Coords <- sf::st_coordinates(df_all3_female)


# Merge watershed boundaries and AreaPercent_0_30, must remove geometry layer to make it merge
# AreaPercent_0_30 is an object that exist in the script "percent_early_forest_figures.R"
watershed_perc <- merge(watershed_boundaries, AreaPercent_0_30[, c("ID", "km_km2", "perc")], by = "ID",
                        all = TRUE)

# Add in 0 to ID's that don't have any disturbances
watershed_perc[which(is.na(watershed_perc$perc)), "perc"] <- c(0, 0)

# Round road density percentages
watershed_perc$round_km_km2.x <- round(watershed_perc$km_km2.x, digits = 2)

#######################################################################################
### Plot showing location density for end of simulation (year) body condition index ###
#######################################################################################

p1_males <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_male, 
                  aes(x = df_all3_male_Coords[, 1], y = df_all3_male_Coords[, 2], 
                      alpha = ..level.., fill = bear_current_bci_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") +  # Density estimate, scaled to a maximum of 1
  facet_grid(bear_sex_age_class ~ bear_current_bci_cat,
             labeller = labeller(
               bear_current_bci_cat = c(`< 0` = "", `>= 0 & < 2.5` = "", `>= 2.5 & < 5` = "" , `> 5` = ""),
               bear_sex_age_class = c(`male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult Male")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "BCI", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 15),
        strip.background.y = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),  
        legend.text = element_text(size = 12),
        legend.position = "none")
        

p1_females <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_female, 
                  aes(x = df_all3_female_Coords[, 1], y = df_all3_female_Coords[, 2], 
                      alpha = ..level.., fill = bear_current_bci_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") +  # Density estimate, scaled to a maximum of 1
  facet_grid(bear_sex_age_class ~ bear_current_bci_cat,
             labeller = labeller(
               bear_current_bci_cat = c(`< 0` = "", `>= 0 & < 2.5` = "", `>= 2.5 & < 5` = "" , `> 5` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult Female", `female-sub-adult` = "Sub-adult Female",
                                      `female-adult-w-off` = "Adult Female \nwith Offspring")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "BCI", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 15),
        strip.background.y = element_rect(fill = "#CCCCFF"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "none")


# Multi-panel plot with cowplot, adjust aspect ratio for PDF
cowplot::plot_grid(p1_females, p1_males,
                   rel_heights = c(1, 1),
                   align = "hv",
                   ncol = 1,
                   labels = c("A","B"),
                   label_size = 18,
                   label_fontfamily = "sans")

df_all3_Coords <- sf::st_coordinates(df_all3)

p1_both <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3, 
                  aes(x = df_all3_Coords[, 1], y = df_all3_Coords[, 2], 
                      alpha = ..level.., fill = bear_current_bci_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") +  # Density estimate, scaled to a maximum of 1
  facet_grid(bear_sex_age_class ~ bear_current_bci_cat,
             labeller = labeller(
               bear_current_bci_cat = c(`< 0` = "", `>= 0 & < 2.5` = "", `>= 2.5 & < 5` = "" , `> 5` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult Female", `female-sub-adult` = "Sub-adult\nFemale",
                                      `female-adult-w-off` = "Adult Female \nwith Offspring", 
                                      `male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult\nMale")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "BCI", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 15),
        strip.background.y = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

#####################################################
### Disturbance percentage and road density plots ###
#####################################################

# Individual labels for road density and disturbance percentage
#road_label <- parse(text = paste(round(watershed_perc$km_km2.x, digits = 2), "~km/km^2"))
road_label_temp <- round(watershed_perc$km_km2.x, digits = 2)
road_label_temp[14] <- NA
road_label <- parse(text = road_label_temp)
#dist_perc_label <- parse(text = paste(round(watershed_perc$perc, digits = 1), "~\"%\""))
dist_perc_label_temp <- round(watershed_perc$perc, digits = 1)
dist_perc_label_temp[14] <- NA
dist_perc_label <- parse(text = dist_perc_label_temp)


p2 <- ggplot(data = watershed_perc) +
  geom_sf(aes(fill = km_km2.x)) +
  geom_sf_text(label = road_label, size = 4, color = "white") +
  coord_sf(datum = st_crs(26911)) +
  scale_fill_continuous(name = bquote(bold(km/km^2))) + 
  labs(x = "Longitude", y = "Latitude") +
  ggrepel::geom_label_repel(data = watershed_perc[14, ],
                            aes(label = round_km_km2.x, geometry = geometry),
                            size = 4,
                            stat = "sf_coordinates",
                            box.padding = 4,
                            nudge_y = -25000,
                            nudge_x = 0,
                            colour = "black",
                            segment.colour = "black") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14),  # need to bold bquote with in scale_fill... or within labs(fill = )
        legend.text = element_text(size = 12))


p3 <- ggplot(data = watershed_perc) +
  geom_sf(aes(fill = perc)) +
  geom_sf_text(label = dist_perc_label, size = 4, color = "white") +
  coord_sf(datum = st_crs(26911)) +
  scale_fill_continuous(name = "Disturbance\nPercentage (%)") +
  labs(x = "Longitude", y = "Latitude") +
  ggrepel::geom_label_repel(data = watershed_perc[14, ],
                            aes(label = round(perc, digits = 1), geometry = geometry),
                            size = 4,
                            stat = "sf_coordinates",
                            box.padding = 4,
                            nudge.y = -25000,
                            nudge.x = 0,
                            colour = "black",
                            segment.colour = "black") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))


# Plot density figures in grid; adjust aspect ratio for PDF
cowplot::plot_grid(p3, p2,
                   rel_heights = c(1, 1),
                   align = "hv",
                   ncol = 1,
                   labels = c("A","B"),
                   label_size = 18,
                   label_fontfamily = "sans")

#################################################################################
### Plot showing location density for end of simulation (year) survival rates ###
#################################################################################

p4_male <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_male, 
                  aes(x = df_all3_male_Coords[, 1], y = df_all3_male_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat,
            labeller = labeller(
              bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , `>= 0.75 & < 0.95` = ""),
              bear_sex_age_class = c(`male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult Male")
            )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()) 

p4_female <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_female, 
                  aes(x = df_all3_female_Coords[, 1], y = df_all3_female_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , 
                                           `>= 0.75 & < 0.95` = "", `>= 0.95` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult Female", `female-sub-adult` = "Sub-adult Female",
                                      `female-adult-w-off` = "Adult Female \nwith offspring")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()) 


p4_both <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3, 
                  aes(x = df_all3_Coords[, 1], y = df_all3_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , 
                                           `>= 0.75 & < 0.95` = "", `>= 0.95` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult\nFemale", `female-sub-adult` = "Sub-adult\nFemale",
                                      `female-adult-w-off` = "Adult Female\nwith\noffspring", 
                                      `male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult\nMale")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  guides(fill = guide_legend(nrow = 2, title.position = "top"),
         alpha = guide_legend(title.position = "top")) +
  theme_bw() + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 15),
        strip.background.y = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom", legend.box = "horizontal")

############################################################################################
### Survival rate adjusted so that bear agents with BCI <= -1 gets a survival value of 0 ###
############################################################################################

# Male
df_all3_maleB <- df_all3_male
df_all3_maleB$bear_survival_rates_1 <- df_all3_maleB$bear_survival_rates
df_all3_maleB$bear_survival_rates_cat_1 <- as.character(df_all3_maleB$bear_survival_rates_cat)
adj_male_1_rows <- which(df_all3_maleB$bear_current_bci <= -1)
df_all3_maleB[adj_male_1_rows, "bear_survival_rates_1"] <- 0
df_all3_maleB[adj_male_1_rows, "bear_survival_rates_cat_1"] <- "< 0.25"
df_all3_maleB$bear_survival_rates_cat_1 <- as.factor(df_all3_maleB$bear_survival_rates_cat_1)


# Female
df_all3_femaleB <- df_all3_female
df_all3_femaleB$bear_survival_rates_1 <- df_all3_femaleB$bear_survival_rates
df_all3_femaleB$bear_survival_rates_cat_1 <- as.character(df_all3_femaleB$bear_survival_rates_cat)
adj_female_1_rows <- which(df_all3_femaleB$bear_current_bci <= -1)
df_all3_femaleB[adj_female_1_rows, "bear_survival_rates_1"] <- 0
df_all3_femaleB[adj_female_1_rows, "bear_survival_rates_cat_1"] <- "< 0.25"
df_all3_femaleB$bear_survival_rates_cat_1 <- as.factor(df_all3_femaleB$bear_survival_rates_cat_1)

# Adults and female adult with offspring (no sub-adults)
df_all3MF <- rbind(df_all3_maleB, df_all3_femaleB)
df_all3MF_Coords <- rbind(df_all3_male_Coords, df_all3_female_Coords)
#adj_MF_1_rows <- df_all3MF$bear_sex_age_class %in% c("female-adult","female-adult-w-off", "male-adult")
#df_all3MF <- df_all3MF[adj_MF_1_rows, ]
#df_all3MF_Coords <- df_all3MF_Coords[adj_MF_1_rows, ]

# Plot showing location density for end of simulation (year) survival rates, with survival rates
# adjusted to zero for bear agents with BCI <= -1
p9_both <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3MF, 
                  aes(x = df_all3MF_Coords[, 1], y = df_all3MF_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat_1,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat_1,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , 
                                           `>= 0.75 & < 0.95` = "", `>= 0.95` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult\nFemale", `female-sub-adult` = "Sub-adult\nFemale",
                                      `female-adult-w-off` = "Adult Female\nwith\noffspring", 
                                      `male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult\nMale")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  guides(fill = guide_legend(nrow = 2, title.position = "top"),
         alpha = guide_legend(title.position = "top")) +
  theme_bw() + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_text(face = "bold", size = 15),
        strip.background.y = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom", legend.box = "horizontal")



p9_MaleFemale <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3MF, 
                  aes(x = df_all3MF_Coords[, 1], y = df_all3MF_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat_1,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat_1,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , `>= 0.75 & < 0.95` = "",
                                           `>= 0.95` = ""),
               bear_sex_age_class = c(`male-adult` = "Adult Male", `female-adult` = "Adult Female",
                                      `female-adult-w-off` = "Adult Female\nwith Offspring")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.title  = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)) 



p5_male <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_maleB, 
                  aes(x = df_all3_male_Coords[, 1], y = df_all3_male_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat_1,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat_1,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , `>= 0.75 & < 0.95` = ""),
               bear_sex_age_class = c(`male-adult` = "Adult Male", `male-sub-adult` = "Sub-adult Male")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()) 


p6_female <- ggplot() + 
  geom_sf(data = watershed_boundaries, fill = NA) + 
  geom_sf() +
  stat_density_2d(data = df_all3_femaleB, 
                  aes(x = df_all3_female_Coords[, 1], y = df_all3_female_Coords[, 2], 
                      alpha = ..level.., fill = bear_survival_rates_cat_1,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_grid(bear_sex_age_class ~ bear_survival_rates_cat_1,
             labeller = labeller(
               bear_survival_rates_cat = c(`< 0.25` = "", `>= 0.25 & < 0.5` = "", `>= 0.5 & < 0.75` = "" , 
                                           `>= 0.75 & < 0.95` = "", `>= 0.95` = ""),
               bear_sex_age_class = c(`female-adult` = "Adult Female", `female-sub-adult` = "Sub-adult Female",
                                      `female-adult-w-off` = "Adult Female \nwith offspring")
             )) + 
  labs(x = "Longitude", y = "Latitude", fill = "Survival Rate", alpha = "Relative Density") +
  coord_sf(datum = st_crs(26911)) +
  theme_bw() + 
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank()) 

###################################################
### Plot weight gain from den exit to den entry ###
###################################################

agg_All <- aggregate(location.body.mass ~ run_number + tick + bear_sex_age_class, 
                       data = df_all2, mean)
agg_All$jul_date <- with(agg_All, floor(tick/24) + 105)
agg_mean_ci <- aggregate(location.body.mass ~ tick + jul_date + bear_sex_age_class,
                         data = agg_All, 
                         FUN = function(x) c(mean = mean(x), 
                                             lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                             upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x)))))
agg_mean_ci <- cbind(agg_mean_ci[, 1:3], as.data.frame(agg_mean_ci$location.body.mass))

label_names <- c(
  'female-adult'       = "Adult Female",
  'female-adult-w-off' = "Adult Female with Offspring",
  'female-sub-adult'   = "Sub-adult Female",
  'male-adult'         = "Adult Male",
  'male-sub-adult'     = "Sub-adult Male"
)

ggplot(data = agg_All, mapping = aes(x = jul_date, y = location.body.mass, 
                                  group = interaction(bear_sex_age_class, run_number))) +
  stat_smooth(geom ="line", alpha = 0.3, se = FALSE, color = "darkgray") + 
  geom_smooth(data = agg_mean_ci, aes(x = jul_date, y = mean, group = bear_sex_age_class), colour = "black",
              size = 1.3) +
  facet_wrap(~bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names)) +
  labs(x = "Julian Date", y = "Body Mass (kg)") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

####################################################
### Mass gain difference between spring and fall ###
####################################################

# Get values for den exit (tick 1) and den entry (tick 4414)
agg_All_gain_diff <- agg_All[which(agg_All$tick == 1 | agg_All$tick == 4414), ]

# Calculate mean and standard deviation
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}

ggplot(data = agg_All_gain_diff, mapping = aes(x = as.factor(jul_date), y = location.body.mass, 
                                               group = interaction(as.factor(jul_date), bear_sex_age_class))) +
  geom_violin(trim = FALSE, fill = "darkgray", alpha = 0.3) +
  stat_summary(fun.data = data_summary, color = "red") +
  facet_wrap(~bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names)) +
  labs(x = "Julian Date", y = "Body Mass (kg)") +
  scale_x_discrete(labels=c("105" = "Den Exit (105)", "288" = "Den Entry (288)")) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Same values as figure above, pooled from every run and every bear
with(agg_All_gain_diff, 
     aggregate(location.body.mass ~ jul_date + bear_sex_age_class, 
               data = agg_All_gain_diff, 
               FUN = function(x) c(mean = mean(x), 
                                   sd = sd(x),
                                   se = (sd(x)/sqrt(length(x))),
                                   min = min(x),
                                   max = max(x),
                                   lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                   upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x))))))

# With adult females (lone and with offspring) pooled together
agg_All_gain_diff2 <- agg_All_gain_diff
levels(agg_All_gain_diff2$bear_sex_age_class) <- c("female-adult", "female-adult", "female-sub-adult", "male-adult", "male-sub-adult")
with(agg_All_gain_diff2, 
     aggregate(location.body.mass ~ jul_date + bear_sex_age_class, 
               data = agg_All_gain_diff2, 
               FUN = function(x) c(mean = mean(x), 
                                   sd = sd(x),
                                   se = (sd(x)/sqrt(length(x))),
                                   min = min(x),
                                   max = max(x),
                                   lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                   upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x))))))

###########################
### Weight gain per day ###
###########################

# Day with lowest weight (typically Day152: June 1st to Day 166: June 15th depending on bear_sex_age_class)
dat1 <- agg_mean_ci[agg_mean_ci$mean %in% with(agg_mean_ci, tapply(mean, bear_sex_age_class, min)), ]
dat2 <- dat1[, c("jul_date", "bear_sex_age_class")]

# Get values for lowest point of mass (around tick 1134 [June 1st], tick 1473[June 15th]) and den entry (tick 4414)
agg_All_gain_day <- agg_All[agg_All$tick %in% c(dat1$tick, 4414), ]

agg_All_gain_day2 <- do.call(rbind, lapply(1:nrow(dat2), function(i) {
  t <- subset(agg_All_gain_day , bear_sex_age_class == dat2[i, 2] & jul_date != dat2[i, 1])
  t1 <- which(t$jul_date == 288)
  t2 <- t[t1,  c("location.body.mass", "jul_date") ]
  t3 <- cbind(t, t2)
  t3 <- t3[-t1, ]
  colnames(t3)[6:7] <- paste(colnames(t3)[6:7], ".entry", sep = "")
  t3$location.body.mass.diff <- with(t3, location.body.mass.entry - location.body.mass)
  t3$jul_date.diff <- with(t3, jul_date.entry - jul_date)
  t3$kg_day <- with(t3, location.body.mass.diff / jul_date.diff)
  return(t3)
  }))

# Summarize weight gain by jul_date (whichever appropriate) + bear_sex_age_class
# Must substract 288 from dat1 jul_date values to get approximately peak day for each sex_age_class
agg_All_gain_day3 <- with(agg_All_gain_day2, 
                          aggregate(kg_day ~ jul_date.diff + bear_sex_age_class, 
                                    data = agg_All_gain_day2, 
                                    FUN = function(x) c(mean = mean(x), 
                                                        lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                                        upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x))))))

#######################################################
### Body condition index from den exit to den entry ###
#######################################################

agg_All_BCI <- aggregate(location.current.bci ~ run_number + tick + bear_sex_age_class + bear_survive, 
                     data = df_all2, mean)
agg_All_BCI$jul_date <- with(agg_All_BCI, floor(tick/24) + 105)
agg_BCI_mean_ci <- aggregate(location.current.bci ~ tick + jul_date + bear_sex_age_class,
                             data = agg_All_BCI, 
                             FUN = function(x) c(mean = mean(x), 
                                                 lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                                 upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x)))))
agg_BCI_mean_ci <- cbind(agg_BCI_mean_ci[, 1:3], as.data.frame(agg_BCI_mean_ci$location.current.bci))


# Plot bci gain from den exit to den entry
ggplot(data = agg_All_BCI, mapping = aes(x = jul_date, y = location.current.bci, 
                                         group = interaction(bear_sex_age_class, run_number))) +
  stat_smooth(geom ="line", alpha = 0.3, se = FALSE, color = "darkgray") + 
  geom_smooth(data = agg_BCI_mean_ci, aes(x = jul_date, y = mean, group = bear_sex_age_class), colour = "black",
              size = 1.3) +
  facet_wrap(~bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names)) +
  labs(x = "Julian Date", y = "Body Condition Index (BCI)") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Day with lowest BCI for each sex-age-class
agg_BCI_mean_ci_zero_rm <- agg_BCI_mean_ci[- (which(agg_BCI_mean_ci$mean == 0)), ]
dat1_BCI <- agg_BCI_mean_ci_zero_rm[agg_BCI_mean_ci_zero_rm$mean %in% with(agg_BCI_mean_ci_zero_rm, 
                                                       tapply(mean, bear_sex_age_class, 
                                                              FUN = min)), ]

# Get values for den exit day + 1 (jul_date 106) and den entry (jul_date 288)
# Days chosen from dat1_BCI, so must change for different dataset
agg_All_BCI_diff <- agg_All_BCI[which(agg_All_BCI$jul_date == 106 | agg_All_BCI$jul_date == 288), ]
agg_All_BCI_diff <- rbind(
  agg_All_BCI_diff, 
  agg_All_BCI[which(agg_All_BCI$jul_date == 152 & agg_All_BCI$bear_sex_age_class == "female-adult"), ],
  agg_All_BCI[which(agg_All_BCI$jul_date == 166 & agg_All_BCI$bear_sex_age_class == "female-adult-w-off"), ],
  agg_All_BCI[which(agg_All_BCI$jul_date == 153 & agg_All_BCI$bear_sex_age_class == "female-sub-adult"), ],
  agg_All_BCI[which(agg_All_BCI$jul_date == 166 & agg_All_BCI$bear_sex_age_class == "male-adult"), ],
  agg_All_BCI[which(agg_All_BCI$jul_date == 155 & agg_All_BCI$bear_sex_age_class == "male-sub-adult"), ]
  )

# Aggregate data of den exit, lowest BCI entire day, and den entry do capture mean and confidence interval
agg_BCI_diff_mean_ci <- aggregate(location.current.bci ~ jul_date + bear_sex_age_class,
                             data = agg_All_BCI_diff, 
                             FUN = function(x) c(mean = mean(x), 
                                                 lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                                 upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x)))))

# Bears that survive      
#agg_All_BCI_diff_T <- agg_All_BCI_diff[which(agg_All_BCI_diff$bear_survive == "true"), ]

# Plot violin plot for den exit + 1 day (106), day with lowest BCI value for each sex-and-age class,
# and day entry (288)
ggplot(data = agg_All_BCI_diff, mapping = aes(x = as.factor(jul_date), y = location.current.bci, 
                                                group = interaction(as.factor(jul_date), bear_sex_age_class))) +
  geom_violin(trim = FALSE, fill = "darkgray", alpha = 0.3) +
  stat_summary(fun.data = data_summary, color = "red") +
  facet_wrap(~bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names)) +
  labs(x = "Julian Date", y = " Mean Body Condition Index (BCI)")  +
  scale_x_discrete(labels=c("106" = "Den\nExit\n+ 1 day\n(106)", "288" = "Den\nEntry\n(288)")) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

######################
### Total distance ###
######################

agg_All_dist <- aggregate(bear_total_dist ~ run_number + tick + bear_sex_age_class, 
                     data = df_all2, mean)
agg_All_dist$jul_date <- with(agg_All_dist, floor(tick/24) + 105)
#agg_All_dist_diff <- agg_All_dist[which(agg_All_dist$jul_date == 288), ]
agg_All_dist_diff <- agg_All_dist[which(agg_All_dist$tick == 4414), ]
agg_dist_mean_ci <- aggregate(bear_total_dist ~ bear_sex_age_class,
                             data = agg_All_dist_diff, 
                             FUN = function(x) c(mean = mean(x), 
                                                 lowerci = mean(x) - (qnorm(0.975) * (sd(x)/sqrt(length(x)))),
                                                 upperci = mean(x) + (qnorm(0.975) * (sd(x)/sqrt(length(x)))),
                                                 n = length(x), 
                                                 min = min(x),
                                                 max = max(x)))
##################################
### Total distance against bci ###
##################################

# Extract latitude column
df_all3_bci <- df_all3
df_all3_bci$lat <- sf::st_coordinates(df_all3_bci)[, 2]
df_all3_bci$long <- sf::st_coordinates(df_all3_bci)[, 1]

# Aggregate total dist
agg_All_distBci <- aggregate(bear_total_dist ~ run_number + location.who + bear_sex_age_class +
                               bear_cub + bear_yearling + bear_current_bci + bear_spring_body_mass, 
                          data = df_all3_bci, unique)
# Aggregate latitude
agg_All_distBciB <- aggregate(lat ~ run_number + location.who + bear_sex_age_class +
                                bear_cub + bear_yearling, 
                              data = df_all3_bci, mean)
# Aggregate longitude
agg_All_distBciB2 <- aggregate(long ~ run_number + location.who + bear_sex_age_class +
                                bear_cub + bear_yearling, 
                              data = df_all3_bci, mean)
# Merge lat and long data
agg_All_distBciC <- merge(agg_All_distBciB, agg_All_distBciB2, by.x = c("run_number", "location.who"),
                          by.y = c("run_number", "location.who"))

# Merge lat/long back with total distance
agg_All_distBciD <- merge(agg_All_distBci, agg_All_distBciC, by.x = c("run_number", "location.who"),
                          by.y = c("run_number", "location.who"))

# Rename columns
agg_All_distBciD <- agg_All_distBciD[, -c(9:11, 13:15)] 
colnames(agg_All_distBciD) <- c("run_number", "location.who", "bear_sex_age_class", "bear_cub", "bear_yearling",
                                "bear_current_bci", "bear_spring_body_mass", "bear_total_dist", "lat", "long")

# Group data into areas of low resource quality and areas of high resource quality based on mean 
# latitude and longitude positions
agg_All_distBciD$latlongcat <- with(agg_All_distBciD, ifelse(lat <= 5830000 & long < 540000, "Low", "High"))
agg_All_distBciD$latlongcat <- as.factor(agg_All_distBciD$latlongcat)


# Only for bear that have BCI > -1, those less are eliminated as they are dead
agg_All_distBci_1 <- agg_All_distBciD[-(which(agg_All_distBciD$bear_current_bci <= -1)), ]
#agg_All_distBci_1 <- agg_All_distBci_1[-(which(agg_All_distBci_1$bear_current_bci > 3)), ]
#agg_All_distBci_1 <- agg_All_distBci_1[(which(agg_All_distBci_1$bear_survival_rates_cat == ">= 0.75 & < 0.95")), ]

# Housekeeping to get females with cubs and females with yearlings into proper categories
agg_All_distBci_1[, c("bear_cub", "bear_yearling")] <- apply(agg_All_distBci_1[, c("bear_cub", "bear_yearling")],
                                                             2, function(x) as.logical(as.factor(x)))
agg_All_distBci_1$bear_sex_age_class2 <-  with(agg_All_distBci_1, ifelse(bear_cub, "female-adult-w-cub", 
                                                         as.character(bear_sex_age_class)))
agg_All_distBci_1$bear_sex_age_class2 <- with(agg_All_distBci_1, ifelse(bear_yearling, "female-adult-w-yrl", 
                                                                        bear_sex_age_class2))
agg_All_distBci_1$bear_sex_age_class2 <- as.factor(agg_All_distBci_1$bear_sex_age_class2)


# Updated labels
label_names2 <- c(
  'female-adult'       = "Adult Female",
  'female-adult-w-cub' = "Adult Female with Cub",
  'female-adult-w-yrl' = "Adult Female with Yearling",
  'female-sub-adult'   = "Sub-adult Female",
  'male-adult'         = "Adult Male",
  'male-sub-adult'     = "Sub-adult Male"
)

# Adult male multiple linear regression
df_bci_m <- agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "male-adult"), ]

# Scale data to generate standardized parameter estimates, and if needed, for model convergence
# Note we are also scaling the response data; use `as.numeric or c` because scale changes column class, so simplify
# to numeric vector when using lm
df_bci_m$bear_total_dist_s        <- as.numeric(scale(df_bci_m$bear_total_dist, center = TRUE, scale = TRUE))
df_bci_m$bear_spring_body_mass_s <- as.numeric(scale(df_bci_m$bear_spring_body_mass, center = TRUE, scale = TRUE))
df_bci_m$bear_current_bci_s      <- as.numeric(scale(df_bci_m$bear_current_bci, center = TRUE, scale = TRUE))

# Scale with attribute data
df_bci_m$bear_total_dist_s1       <- scale(df_bci_m$bear_total_dist, center = TRUE, scale = TRUE)
df_bci_m$bear_spring_body_mass_s1 <- scale(df_bci_m$bear_spring_body_mass, center = TRUE, scale = TRUE)
df_bci_m$bear_current_bci_s1      <- scale(df_bci_m$bear_current_bci, center = TRUE, scale = TRUE)

# Explore the data

# lm in geom_smooth by defaults fits individual intercepts and individual slopes for factor levels,
# so in effect fitting random intercepts and slopes or an ANCOVA
# Note we are not taking bear_spring_body_mass into consideration for visualization purposes
ggplot(data = df_bci_m, mapping = aes(x = bear_current_bci, y = bear_total_dist, 
                     group = interaction(bear_sex_age_class2, latlongcat))) +
  geom_point(aes(colour = latlongcat), alpha = 0.1) +
  geom_smooth(method = "lm", aes(colour = latlongcat)) +
  labs(x = "Body Condition Index", y = "Distance Traveled (km/year)")  +
  ylim(800, 2300) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Run the linear model; bear_spring_body_mass in control variable
#mod_bci_m <- lm(bear_total_dist ~ bear_spring_body_mass + bear_current_bci*latlongcat, data = df_bci_m)
mod_bci_m_adj <- lm(bear_total_dist_s ~ latlongcat + bear_spring_body_mass_s + 
                      bear_current_bci_s*latlongcat, data = df_bci_m)

# Check collinearity between predictor variables
performance::check_collinearity(mod_bci_m_adj)

# Create the density plot of residuals
plot(density(mod_bci_m_adj$residuals))

# Create Q-Q plot
qqnorm(mod_bci_m_adj$residuals)
qqline(mod_bci_m_adj$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# Create a fitted values vs residuals plot
plot(mod_bci_m_adj$residuals ~ mod_bci_m_adj$fitted.values)
lines(lowess(mod_bci_m_adj$fitted.values, mod_bci_m_adj$residuals), col = "blue")
#text(mod_bci_m$fitted.values, mod_bci_m$residuals, row.names(df_bci_m), cex = 0.6, pos = 4, col = "red")

# Interpreting the linear model
summary(mod_bci_m_adj)

# Results
library(broom)
broom::glance(mod_bci_m_adj)

library(sjstats)
sjstats::anova_stats(mod_bci_m_adj)
#sjstats::anova_stats(car::Anova(mod_bci_m2, type = 2)) # report type 2 because interaction term is not significant, 
                                                       # main factors are tested in light of one another, but not in light of the interaction term.
library(ggeffects)
ggpred_m_adj <- ggpredict(mod_bci_m_adj, terms = c("bear_current_bci_s", "latlongcat"))
plot(ggpred_m_adj, facet = FALSE)

# Prepare newdata for prediction
pred_m_adj <- with(df_bci_m, expand.grid(latlongcat = unique(latlongcat),
                                              bear_spring_body_mass_s = mean(bear_spring_body_mass_s), 
                                              bear_current_bci_s = seq(from = range(bear_current_bci_s)[1],
                                                                    to = range(bear_current_bci_s)[2],
                                                                    by = 0.05)))
# Prediction with predict (fit, se, lwr, upr), calculate lwr and upr, and rescale fits to original scale
fit_m_adj <- predict(mod_bci_m_adj, newdata = pred_m_adj, type = "response", se.fit = TRUE,
                     interval = "confidence", level = 0.95)
pred_m_adj$fit <- fit_m_adj$fit[, "fit"]
pred_m_adj$se  <- fit_m_adj$se.fit
pred_m_adj$lwr <- fit_m_adj$fit[, "lwr"]
pred_m_adj$upr <- fit_m_adj$fit[, "upr"]
pred_m_adj$lwr_1 <- with(pred_m_adj, fit + (qnorm(0.025) * se))
pred_m_adj$upr_1 <- with(pred_m_adj, fit + (qnorm(0.975) * se))
pred_m_adj$bear_spring_body_mass_u <- pred_m_adj$bear_spring_body_mass_s * attr(df_bci_m$bear_spring_body_mass_s1, 'scaled:scale') + 
                                      attr(df_bci_m$bear_spring_body_mass_s1, 'scaled:center')
pred_m_adj$bear_current_bci_u <- pred_m_adj$bear_current_bci_s * attr(df_bci_m$bear_current_bci_s1, 'scaled:scale') + 
                                 attr(df_bci_m$bear_current_bci_s1, 'scaled:center')
pred_m_adj$fit_u <- pred_m_adj$fit * attr(df_bci_m$bear_total_dist_s1, 'scaled:scale') + 
                    attr(df_bci_m$bear_total_dist_s1, 'scaled:center')
pred_m_adj$lwr_u  <- pred_m_adj$lwr * attr(df_bci_m$bear_total_dist_s1, 'scaled:scale') + 
                     attr(df_bci_m$bear_total_dist_s1, 'scaled:center')
pred_m_adj$upr_u  <- pred_m_adj$upr * attr(df_bci_m$bear_total_dist_s1, 'scaled:scale') + 
                     attr(df_bci_m$bear_total_dist_s1, 'scaled:center')
pred_m_adj$lwr_1_u  <- pred_m_adj$lwr_1 * attr(df_bci_m$bear_total_dist_s1, 'scaled:scale') + 
                       attr(df_bci_m$bear_total_dist_s1, 'scaled:center')
pred_m_adj$upr_1_u  <- pred_m_adj$upr_1 * attr(df_bci_m$bear_total_dist_s1, 'scaled:scale') + 
                       attr(df_bci_m$bear_total_dist_s1, 'scaled:center')
pred_m_adj$sex <- "Male"


# Scaled plot; compare to ggeffect::ggpredict
ggplot(data = pred_m_adj, aes(x = bear_current_bci_s, y = fit)) +
  geom_line(aes(colour = latlongcat)) + 
  geom_ribbon(aes(ymin = lwr_1, ymax = upr_1, fill = latlongcat), alpha = 0.2) +
  xlim(-3, 4) +
  ylim(-0.75, 1.5) +
  labs(x = "BCI", y = "Displacement (km/year)") +
  scale_fill_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) + 
  scale_colour_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) +
  theme_bw() 

# Plot rescaled to original scale
ggplot(data = pred_m_adj, aes(x = bear_current_bci_u, y = fit_u)) +
  geom_line(aes(colour = latlongcat)) + 
  geom_ribbon(aes(ymin = lwr_1_u, ymax = upr_1_u, fill = latlongcat), alpha = 0.2) +
  labs(x = "BCI", y = "Displacement (km/year)") +
  scale_fill_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) + 
  scale_colour_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.position = c(0.16, 0.89))

# Adult female multiple linear regression
df_bci_f <- agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "female-adult"), ]

# Scale data to generate standardized parameter estimates, and if needed, for model convergence
# Note we are also scaling the response data; use `as.numeric or c` because scale changes column class, so simplify
# to numeric vector when using lm
df_bci_f$bear_total_dist_s       <- as.numeric(scale(df_bci_f$bear_total_dist, center = TRUE, scale = TRUE))
df_bci_f$bear_spring_body_mass_s <- as.numeric(scale(df_bci_f$bear_spring_body_mass, center = TRUE, scale = TRUE))
df_bci_f$bear_current_bci_s      <- as.numeric(scale(df_bci_f$bear_current_bci, center = TRUE, scale = TRUE))

# Scale with attribute data
df_bci_f$bear_total_dist_s1       <- scale(df_bci_f$bear_total_dist, center = TRUE, scale = TRUE)
df_bci_f$bear_spring_body_mass_s1 <- scale(df_bci_f$bear_spring_body_mass, center = TRUE, scale = TRUE)
df_bci_f$bear_current_bci_s1      <- scale(df_bci_f$bear_current_bci, center = TRUE, scale = TRUE)

# Explore the data

# lm in geom_smooth by defaults fits individual intercepts and individual slopes for factor levels,
# so in effect fitting random intercepts and slopes or an ANCOVA
# Note we are not taking bear_spring_body_mass into consideration for visualization purposes
ggplot(data = df_bci_f, mapping = aes(x = bear_current_bci, y = bear_total_dist, 
                                      group = interaction(bear_sex_age_class2, latlongcat))) +
  geom_point(aes(colour = latlongcat), alpha = 0.1) +
  geom_smooth(method = "lm", aes(colour = latlongcat)) +
  labs(x = "Body Condition Index", y = "Distance Traveled (km/year)")  +
  ylim(800, 2300) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

# Run the linear model; bear_spring_body_mass in control variable
#mod_bci_f <- lm(bear_total_dist ~ bear_spring_body_mass + bear_current_bci*latlongcat, data = df_bci_f)
mod_bci_f_adj <- lm(bear_total_dist_s ~ latlongcat + bear_spring_body_mass_s + 
                      bear_current_bci_s*latlongcat, data = df_bci_f)

# Check collinearity between predictor variables
performance::check_collinearity(mod_bci_f_adj)

# Create the density plot of residuals
plot(density(mod_bci_f_adj$residuals))

# Create Q-Q plot
qqnorm(mod_bci_f_adj$residuals)
qqline(mod_bci_f_adj$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# Create a fitted values vs residuals plot
plot(mod_bci_f_adj$residuals ~ mod_bci_f_adj$fitted.values)
lines(lowess(mod_bci_f_adj$fitted.values, mod_bci_f_adj$residuals), col = "blue")
#text(mod_bci_f$fitted.values, mod_bci_f$residuals, row.names(df_bci_f), cex = 0.6, pos = 4, col = "red")

# Interpreting the linear model
summary(mod_bci_f_adj)

# Results
library(broom)
broom::glance(mod_bci_f_adj)

library(sjstats)
sjstats::anova_stats(mod_bci_f_adj)
#sjstats::anova_stats(car::Anova(mod_bci_f2, type = 2)) # report type 2 because interaction term is not significant, 
# main factors are tested in light of one another, but not in light of the interaction term.
library(ggeffects)
ggpred_f_adj <- ggpredict(mod_bci_f_adj, terms = c("bear_current_bci_s", "latlongcat"))
plot(ggpred_f_adj, facet = FALSE)

# Prepare newdata for prediction
pred_f_adj <- with(df_bci_f, expand.grid(latlongcat = unique(latlongcat),
                                         bear_spring_body_mass_s = mean(bear_spring_body_mass_s), 
                                         bear_current_bci_s = seq(from = range(bear_current_bci_s)[1],
                                                                  to = range(bear_current_bci_s)[2],
                                                                  by = 0.05)))
# Prediction with predict (fit, se, lwr, upr), calculate lwr and upr, and rescale fits to original scale
fit_f_adj <- predict(mod_bci_f_adj, newdata = pred_f_adj, type = "response", se.fit = TRUE,
                     interval = "confidence", level = 0.95)
pred_f_adj$fit <- fit_f_adj$fit[, "fit"]
pred_f_adj$se  <- fit_f_adj$se.fit
pred_f_adj$lwr <- fit_f_adj$fit[, "lwr"]
pred_f_adj$upr <- fit_f_adj$fit[, "upr"]
pred_f_adj$lwr_1 <- with(pred_f_adj, fit + (qnorm(0.025) * se))
pred_f_adj$upr_1 <- with(pred_f_adj, fit + (qnorm(0.975) * se))
pred_f_adj$bear_spring_body_mass_u <- pred_f_adj$bear_spring_body_mass_s * attr(df_bci_f$bear_spring_body_mass_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_spring_body_mass_s1, 'scaled:center')
pred_f_adj$bear_current_bci_u <- pred_f_adj$bear_current_bci_s * attr(df_bci_f$bear_current_bci_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_current_bci_s1, 'scaled:center')
pred_f_adj$fit_u <- pred_f_adj$fit * attr(df_bci_f$bear_total_dist_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_total_dist_s1, 'scaled:center')
pred_f_adj$lwr_u  <- pred_f_adj$lwr * attr(df_bci_f$bear_total_dist_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_total_dist_s1, 'scaled:center')
pred_f_adj$upr_u  <- pred_f_adj$upr * attr(df_bci_f$bear_total_dist_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_total_dist_s1, 'scaled:center')
pred_f_adj$lwr_1_u  <- pred_f_adj$lwr_1 * attr(df_bci_f$bear_total_dist_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_total_dist_s1, 'scaled:center')
pred_f_adj$upr_1_u  <- pred_f_adj$upr_1 * attr(df_bci_f$bear_total_dist_s1, 'scaled:scale') + 
  attr(df_bci_f$bear_total_dist_s1, 'scaled:center')
pred_f_adj$sex <- "Female"

# Scaled plot; compare to ggeffect::ggpredict
ggplot(data = pred_f_adj, aes(x = bear_current_bci_s, y = fit)) +
  geom_line(aes(colour = latlongcat)) + 
  geom_ribbon(aes(ymin = lwr_1, ymax = upr_1, fill = latlongcat), alpha = 0.2) +
  xlim(-3, 4) +
  ylim(-0.75, 1.5) +
  labs(x = "BCI", y = "Displacement (km/year)") +
  scale_fill_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) + 
  scale_colour_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) +
  theme_bw() 

# Plot rescaled to original scale
ggplot(data = pred_f_adj, aes(x = bear_current_bci_u, y = fit_u)) +
  geom_line(aes(colour = latlongcat)) + 
  geom_ribbon(aes(ymin = lwr_1_u, ymax = upr_1_u, fill = latlongcat), alpha = 0.2) +
  labs(x = "BCI", y = "Displacement (km/year)") +
  scale_fill_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) + 
  scale_colour_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.position = c(0.84, 0.90))

# Plot for male and female
pred_mf_adj <- rbind(pred_f_adj, pred_m_adj)
pred_mf_adj$sex <- as.factor(pred_mf_adj$sex)

# Plot rescaled to original scale
ggplot(data = pred_mf_adj, aes(x = bear_current_bci_u, y = fit_u)) +
  geom_line(aes(colour = latlongcat)) + 
  geom_ribbon(aes(ymin = lwr_1_u, ymax = upr_1_u, fill = latlongcat), alpha = 0.2) +
  labs(x = "BCI", y = "Displacement (km/year)") +
  scale_fill_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) + 
  scale_colour_discrete(name = "Resource Quality", labels = c("Rich", "Poor")) +
  facet_wrap(~ sex) +
  theme_bw() +
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.position = c(0.08, 0.95)) # adjust accordingly

# Old displacement (movement) vs bci

#sjstats::anova_stats(car::Anova(mod_bci_f2, type = 3)) # report type 3 because interaction is significant

# If the F-test of the interaction term is significant (as it is here), you can only report the result 
# of the interaction term from this output in your results section. You can’t interpret or report the 
# main effects from this output because they are not independent. To report the results of each of your 
# explanatory variables (BCI and resource quality area), you need to conduct and report the results of
# separate simple linear regressions for each group individually. To do that, you will need to subset 
# your data so that you can run a linear regression for each category of your categorical explanatory 
# variable separately (one for resource rich (high) and poor (low) areas).

# Get the data for the resource rich areas only
rich <- df_bci_f[which(df_bci_f$latlongcat == 'High'), ]
# Get the data for the resource poor areas only
poor <- df_bci_f[which(df_bci_f$latlongcat == 'Low'), ]


# Run the linear regression
mod_rich <- lm(bear_total_dist ~ bear_current_bci, data = rich)
mod_poor <- lm(bear_total_dist ~ bear_current_bci, data = poor)

# Create the density plot of residuals
plot(density(mod_rich$residuals))
plot(density(mod_poor$residuals))

# Create Q-Q plot
qqnorm(mod_rich$residuals)
qqline(mod_rich$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))
qqnorm(mod_poor$residuals)
qqline(mod_poor$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# Create a fitted values vs residuals plot
plot(mod_rich$residuals ~ mod_rich$fitted.values)
lines(lowess(mod_rich$fitted.values, mod_rich$residuals), col = "blue")
plot(mod_poor$residuals ~ mod_poor$fitted.values)
lines(lowess(mod_poor$fitted.values, mod_poor$residuals), col = "blue")


# Interpreting of linear regression
summary(mod_rich)
summary(mod_poor)
library(car)
car::Anova(mod_bci_f2, type = 3) # type 3 

# Results
library(broom)
broom::glance(mod_bci_f)
broom::glance(mod_bci_f2)

library(sjstats)
sjstats::anova_stats(mod_bci_f)


# Random intercept
mod_distBci_i <- glmmTMB::glmmTMB(bear_total_dist ~ bear_current_bci + 1|latlongcat,
                                  data = agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "male-adult"), ])
# Random slope and intercept
mod_distBci_s <- glmmTMB::glmmTMB(bear_total_dist ~ scale(bear_current_bci) + scale(bear_current_bci)|latlongcat, 
                  data = agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "male-adult"), ])
# Fixed effect model used Ancova type 111
mod_distBci_null <- lm(bear_total_dist ~ bear_current_bci*latlongcat, 
                       data = agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "female-sub-adult"), ])


ggplot(agg_All_distBci_1,
                          aes(y = bear_total_dist, x = bear_sex_age_class2, fill = latlongcat)) +
  #geom_bar(position = "dodge", stat = "identity") + 
  geom_boxplot() +
  ggsignif::geom_signif(stat = "identity", comparisons = list(c("high", "low")), 
              map_signif_level=TRUE)
  stat_summary(fun="mean", geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge",width=.8)

# Diagnostics
summary(mod_distBci_i)
summary(mod_distBci_s)
summary(mod_distBci_null)
performance::r2(mod_distBci_i)
performance::r2(mod_distBci_s)
performance::r2(mod_distBci_null)
anova(mod_distBci_s, mod_distBci_i, mod_distBci_null)
bbmle::AICtab(mod_distBci_null, mod_distBci_i, mod_distBci_s, base = TRUE)

library(DHARMa)
res_distBci_s <- DHARMa::simulateResiduals(mod_distBci_s)
plot(res_distBci_s)

# Predict, here using random intercept and slope model, as slopes do differ among groups
# See below when comparing to individual groups fitted with separate linear model
mod_distBci_p_i <- predict(mod_distBci_i)
mod_distBci_p_s <- predict(mod_distBci_s, se.fit = TRUE)


ggplot(data = cbind(agg_All_distBci_1, mod_distBci_p_s),
       aes(x = bear_current_bci, y = bear_total_dist)) + 
  #geom_point(fill = "darkgray", alpha = 0.2) +
  geom_line(aes(y = mod_distBci_p_s$fit, fill = bear_sex_age_class2)) +
  geom_ribbon(aes(ymin = mod_distBci_p_s$fit - 1.96*mod_distBci_p_s$se.fit,
                  ymax = mod_distBci_p_s$fit + 1.96*mod_distBci_p_s$se.fit, 
                  fill = bear_sex_age_class2), alpha = 0.2) +
  labs(x = "Body Condition Index", y = "Distance Traveled (km)", fill = "Sex-and-Age Class")  +
  scale_fill_discrete(labels = label_names2) +
  theme_bw()
#+ facet_wrap(.~ bear_sex_age_class2)



# Adult female
distBci_f <- agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "female-adult"), ]
mod_distBci_f <- lm(bear_total_dist ~ bear_current_bci + latlongcat - 1, data = distBci_f)
summary(mod_distBci_f)

# Adult female with cub
distBci_fc <- agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "female-adult-w-cub"), ]
mod_distBci_fc <- lm(bear_total_dist ~ bear_current_bci, data = distBci_fc)
summary(mod_distBci_fc)

# Adult male
distBci_m <- agg_All_distBci_1[which(agg_All_distBci_1$bear_sex_age_class2 == "male-adult"), ]
mod_distBci_m <- lm(bear_total_dist ~ bear_current_bci + latlongcat - 1, data = distBci_m)
summary(mod_distBci_m)

fixef(mod_distBci_s)
ranef(mod_distBci_s)


################
### Survival ###
################

agg_All_surv <- aggregate(bear_survival_rates ~ run_number + tick + bear_sex_age_class, 
                          data = df_all2, mean)
agg_All_surv$jul_date <- with(agg_All_surv, floor(tick/24) + 105)
agg_All_surv_diff <- agg_All_surv[which(agg_All_surv$jul_date == 288), ]
agg_All_surv_diff <- agg_All_surv[which(agg_All_surv$tick == 4414), ]
agg_surv_mean_ci <- aggregate(bear_survival_rates ~ bear_sex_age_class,
                              data = agg_All_surv_diff, 
                              FUN = function(x) c(mean = mean(x), 
                                                  lowerci = mean(x) - qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                                  upperci = mean(x) + qnorm(0.975) * (sd(x)/sqrt(length(x))),
                                                  n = length(x), 
                                                  min = min(x),
                                                  max = max(x)))

#############################
### Home Range Estimation ###
#############################

# Note that we estimate home range with 1 location (tick) per day (24 hours)
# Field results from Yellowhead have annual home ranges of ~ 1800 km2 and 700 km2 for adult males and females, 
# respectively (SARA)
# Our model results show annual home ranges ~ 3000 km2 and 2000 km2 for adult males and females, respectively
# Adding a submodel that follows gradients (i.e. turning/orienting towards previously sampled 
# or encountered areas based on memory of food resource quality) would probably shrink home range sizes 
# in the model. Centroid submodel either has bear agents concentrate around their centroid or get stuck on 
# world edge, so a memory submodel would be better


# Libraries
library(adehabitatHR) # Create mcp
library(sp)           # Getting data in proper coordinates for mcp in adehabitatHR
library(scales)       # Helps make polygons partly transparent using the alpha argument
library(sf)           # Spatial data in R, will superseed sp

# Remove geometry column for computation purpose
df_bci_hr <- as.data.frame(df_all3_female)
df_bci_hr <- df_bci_hr[, -29]
df_bci_hr$low.current.bci <- rep(0, nrow(df_bci_hr))

bci_hr <- aggregate(location.current.bci ~ run_number + location.who + bear_sex_age_class, 
                         data = df_bci_hr, max)

df_bci_hr2 <- lapply(1:nrow(bci_hr), function(i) {
  foo <- with(df_bci_hr, which(run_number == bci_hr[i, 1] & location.who == bci_hr[i, 2]))
  df_bci_hr[foo, "low.current.bci"] <- bci_hr[i, "location.current.bci"]
  return(df_bci_hr[foo, ])
  })

df_bci_hr3 <- do.call(rbind, df_bci_hr2)


# Include run number, location who (id), tick (season calculation), bear-age-class, bci cat,
# and x, y columns for making MCP's; and adjust row and column names
# Change for female, can use same object names
bear.sp_male <- cbind(df_bci_hr3[, c("run_number", "location.who", "tick", "bear_age_class",
                                       "low.current.bci", "bear_survive")], df_all3_female_Coords)
row.names(bear.sp_male) <- 1:nrow(bear.sp_male)
colnames(bear.sp_male)[2] <- "id.location.who.sur"

# Keep adult bears only or adult bears with off
bear.sp_male <- bear.sp_male[which(bear.sp_male$bear_age_class == "sub-adult"), ]

# Keep mesophagia and hyperphagia data (< June 15, which is ~ 1464 ticks) or whatever interval
# Here it's May 1st through September 30
bear.sp_male <- bear.sp_male[which(bear.sp_male$tick > 360 & bear.sp_male$tick < 4032),]

# Relevel bear_current_bci_names < 0 = A; >= 0 & , 2.5 = B; >= 2.5 & < 5 = C; > 5 = D
#levels(bear.sp_male$bear_current_bci_cat) <- c("A", "B", "C", "D")

# drop sf geometry to get dataframe needed for sp object
#bear.sp_male <- sf::st_drop_geometry(bear.sp_male)

# Split bear data by run number and id; drop dataframes with length zero (should be none)
bear.sp_male <- with(bear.sp_male, split(bear.sp_male, 
                                         list(run_number, id.location.who.sur, low.current.bci,
                                                            bear_survive), drop = TRUE, sep = "_"))

# add "id.run_num.location.who" based on run number and location.who by binding unique rows to each dataframe in list, 
# colname must be start with "id" for MCP
bear.sp_male <- Map(cbind, bear.sp_male, "id_runNum_locWho_bciCat_sur" = names(bear.sp_male))

# bind list into one dataframe for MCP
bear.sp_male <- do.call(rbind, bear.sp_male)
row.names(bear.sp_male) <- 1:nrow(bear.sp_male)

# Use only id.runNum.locWho, x and y columns for mcp
bear.sp_male <- bear.sp_male[, c("id_runNum_locWho_bciCat_sur", "X", "Y")]

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(bear.sp_male) <- c("X", "Y")

# Set the coordianate reference systems (CRS)
# epsg projection 26911 - nad83 / utm zone 11n
# https://spatialreference.org/ref/epsg/nad83-utm-zone-11n/
proj4string(bear.sp_male) <- CRS("+init=epsg:26911")

# Remove bear with points that are too low (maybe bear dies lack of food or out-of-bound issues?)
# Must adjust value if using all seasons or specific seasons
bear.sp_male_tab <- table(bear.sp_male$id_runNum_locWho_bciCat_sur)
bear.sp_male2 <- bear.sp_male[!(bear.sp_male$id_runNum_locWho_bciCat_sur %in% names(which(bear.sp_male_tab <= 130))), ]

# Calculate MCPs for each bear in km2
bear.mcp.male <- adehabitatHR::mcp(bear.sp_male2, percent = 95, unout = "km2")

mean(bear.mcp.male$area)
median(bear.mcp.male$area)
hist(bear.mcp.male$area)

# Bear under 2000 km2, looking for patterns with male bears
bear.mcp.test <- bear.mcp.male[which(bear.mcp.male@data$area < 2000), ]

hist(bear.mcp.test$area)
mean(bear.mcp.test$area)

# Plot example
ggplot(st_as_sf(bear.mcp.test[1:100, ]), aes(fill = id)) + geom_sf(alpha = 0.5) +
  scale_fill_discrete(name = "Animal id")

# Looking at mean home ranges sizes for different bci cat values, need to remove survival category
mean(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "A"), ]$area)
hist(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "A"), ]$area)
mean(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "B"), ]$area)
hist(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "B"), ]$area)
mean(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "C"), ]$area)
hist(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "C"), ]$area)
mean(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "D"), ]$area)
hist(bear.mcp.male[which(gsub("[^a-zA-Z]", "", bear.mcp.male$id) == "D"), ]$area)

bear.h <- bear.mcp.male@data
bear.h <- cbind(bear.h, data.frame(do.call(rbind, (strsplit(as.character(bear.h$id), "_", fixed = TRUE)))))
colnames(bear.h) <- c("id", "area", "run.num", "locWho", "bciCat", "sur")

with(bear.h, plot(area ~ bciCat))
mod_bear.h <- lm(area ~ as.numeric(bciCat), data = bear.h)
abline(mod_bear.h)

with(bear.h, aggregate(area ~ sur, FUN = mean))


# Home range estimated with kernel density estimator
# Using full dataset (i.e. every tick) may affect these results
kernel.ref <- adehabitatHR::kernelUD(bear.sp_male, h = "href")
kernel.poly <- adehabitatHR::getverticeshr(kernel.ref, percent = 95, unout = "km2") 


######################################
### Check if locations overlap map ###
######################################

check_points_example <- function(){
  
  pts <- st_sfc(st_multipoint(matrix(c(450000, 5888521, 494815.1, 5918922, 500815.1, 5888521, 440000, 5888521,
                                       495815.1, 5888521), 
                                     nrow = 5, ncol = 2, byrow = TRUE)), 
                crs = st_crs(watershed_boundaries))
  
  plot(watershed_boundaries$geometry)
  plot(pts, add = TRUE)
  t <- st_intersection(x = watershed_boundaries, y = pts)
  pts_dim <- dim(pts[[1]])
  int_dim <- dim(do.call(rbind, t$geometry))
  return(list(pts_dim = pts_dim, intersection_dim = int_dim))
  
}

plot(watershed_boundaries$geometry)
#plot(df_all3, add = TRUE)
inter_test <- st_intersection(x = watershed_boundaries, y = df_all3)
dim(df_all3)
dim(do.call(rbind, inter_test$geometry))

#--------------------------------------------------------------------------------------------------------------
###################################
### Regression Models and Plots ###
##################################

# Function to transform fraction with 0 and 1 to <1 and >0
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

# Function to backtransform transform01
backtransform.est <- function(x, n) {
  y <- (x * n - 0.5) / (n - 1)
  return(y)
}

# Logit transformation
logit <- function(x) {
  y <- log(x/(1 - x))
  return(y)
}

# Calculate Root mean squared error
RMSE <- function(x, y) {
  a <- sqrt(sum((y - x)^2) / length(x))
  return(a)
}

# Grab data on last tick for regression
regr_df <- df_all2[which(df_all2$tick == 4414), ]

# Adjust survival rates for bears with low BCI (-1, -2, -3)
regr_df$bear_survival_rates_0 <- regr_df$bear_survival_rates
regr_df[which(regr_df$bear_current_bci <= 0), "bear_survival_rates_0" ] <- 0
regr_df$bear_survival_rates_1 <- regr_df$bear_survival_rates
regr_df[which(regr_df$bear_current_bci <= -1), "bear_survival_rates_1" ] <- 0
regr_df$bear_survival_rates_2 <- regr_df$bear_survival_rates
regr_df[which(regr_df$bear_current_bci <= -2), "bear_survival_rates_2" ] <- 0
regr_df$bear_survival_rates_3 <- regr_df$bear_survival_rates
regr_df[which(regr_df$bear_current_bci <= -3), "bear_survival_rates_3" ] <- 0

# Grab data needed for regressions
regr_df1 <- regr_df[, c("run_number", "bear_sex_age_class", "bear_survival_rates", "bear_current_bci",
                      "bear_road_dens","bear_survival_rates_1", "bear_survival_rates_2", 
                      "bear_survival_rates_3", "bear_survival_rates_0")]

# Run number as factor
regr_df1$run_number <- as.factor(regr_df1$run_number)

# Transform data 0, 1 to > 0 and < 1 for plotting beta regressions
regr_df_beta <- cbind(regr_df1[c(1:2, 4:5)], apply(regr_df1[c(3, 6:9)], 2, transform01))

####################################
### Mixed effect beta regression###
####################################

# Random effects had no effect on model variance, so not used here, although kept coding

# Mixed effect model
beta_0_I <- glmmTMB(bear_survival_rates ~ bear_sex_age_class + bear_current_bci + (1|run_number),
                  family = beta_family(link = "logit"), data = regr_df_beta)


beta_0_S <- glmmTMB(bear_survival_rates ~ bear_sex_age_class + bear_current_bci + (bear_current_bci|run_number),
                  family = beta_family(link = "logit"), data = regr_df_beta)

# Fixed effects and random effects coefficients
glmmTMB::fixef(beta_0_I)
glmmTMB::ranef(beta_0_I)
glmmTMB::fixef(beta_0_S)
glmmTMB::ranef(beta_0_S)

# Calculate the intraclass correlation; doesn't work because random effect is too small here
performance::icc(beta_0_I)
performance::icc(beta_0_S)

# The first is called the marginal R2 and describes 
# the proportion of variance explained by the fixed factor(s) alone.
# The second is the conditional R2, which describes the proportion of 
# variance explained by both the fixed and random factors
performance::r2(beta_0_I)
performance::r2(beta_0_S)

# Fixed effect regressions, do hard to fit so use GAMs instead, see below

# Beta fit with transform/adjusted data with 0 > 0
beta_0_fix <- betareg(bear_survival_rates ~ bear_sex_age_class + bear_current_bci, 
                            data = regr_df_beta, link = "logit") # beta
beta_0_fix2 <- glmmTMB::glmmTMB(bear_survival_rates ~ bear_sex_age_class + bear_current_bci, 
                      data = regr_df_beta, family = beta_family(link = "logit")) # beta, still fixed effect, but using glmmTMB
logit_0_fix <- glm(logit(bear_survival_rates) ~ bear_sex_age_class + bear_current_bci,
                   data = regr_df_beta, family = "gaussian") # linear gaussian with logit transformation

# Uses unscaled zero data, i.e. 0 data stay as 0
betaInf_0_fix <- gamlss(bear_survival_rates ~ bear_sex_age_class + bear_current_bci,  
                        family = BEZI, data = regr_df1, trace = F) # zero-inflated beta
gaus_0_fix <- glm(bear_survival_rates ~ bear_sex_age_class + bear_current_bci, 
                  data = regr_df1, family = "gaussian") # linear gaussian
poly2_0_fix <- glm(bear_survival_rates ~ 1 + bear_sex_age_class + poly(bear_current_bci, 2), 
                  data = regr_df1, family = "gaussian") # 2nd order polynomial
poly3_0_fix <- glm(bear_survival_rates ~ bear_sex_age_class + poly(bear_current_bci, 3), 
                   data = regr_df1, family = "gaussian") # 3rd order polynomial

# Visual to see points
ggplot(regr_df1, aes(x = bear_current_bci, y = bear_survival_rates)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ bear_sex_age_class)

ggplot(regr_df1, aes(x = bear_road_dens, y = bear_survival_rates)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ bear_sex_age_class)
  
# Model Comparison
bbmle::AICtab(beta_0_fix, beta_0_fix2, logit_0_fix, betaInf_0_fix, gaus_0_fix, poly2_0_fix, poly3_0_fix)
performance::r2(beta_0_fix) # if fitting different random effect structures (e.g. intercept vs intercept + slope)
performance::check_collinearity(beta_0_fix) # Check for collinearity
performance::check_autocorrelation(beta_0_fix) # Check for autocorrelation
simOutBeta_0_fix <- DHARMa::simulateResiduals(fittedModel = beta_0_fix2, plot = TRUE) # really only for mixed models

################
### Fit GAMs ###
################

## Body Condition Index unadjusted

# Beta regression glm, though using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This more or less models a linear relationship with a slight curve
gam_glm_beta_0_fix <- mgcv::gam(bear_survival_rates ~ bear_sex_age_class + bear_current_bci,
                                family = betar(link = "logit"), data = regr_df_beta)
# Beta regression gam using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This is adding a basis function (smoother) that mimics a sigmoid function but with beta distribution instead
# of binomial distribution using a logistic regression
gam_beta_0_fix <- mgcv::gam(bear_survival_rates ~ bear_sex_age_class + s(bear_current_bci, bs = 'tp'), 
                       family = betar(link = "logit"), data = regr_df_beta) # thin plate regression spline

# Model Comparison
bbmle::AICtab(gam_glm_beta_0_fix, gam_beta_0_fix, weights = TRUE, delta = TRUE, 
           base = TRUE, logLik = TRUE, sort = TRUE)
summary(gam_glm_beta_0_fix)$sp.criterion # GCV
summary(gam_beta_0_fix)$sp.criterion # GCV
summary(gam_glm_beta_0_fix)$r.sq # Adjusted R-squared
summary(gam_beta_0_fix)$r.sq # Adjusted R-squared
anova(gam_glm_beta_0_fix, gam_beta_0_fix, test = "Chisq") # Does non-linear relationship of covariates improve model
performance::r2(gam_glm_beta_0_fix)  # Grabs Adjusted R-squared
performance::r2(gam_beta_0_fix)  # Grabs Adjusted R-squared
performance::check_collinearity(gam_glm_beta_0_fix) # Check for collinearity
performance::check_collinearity(gam_beta_0_fix) # Check for collinearity
performance::check_autocorrelation(gam_glm_beta_0_fix) # Check for autocorrelation
performance::check_autocorrelation(gam_beta_0_fix) # Check for autocorrelation

# Model Diagnostics
# Enhance visual check of [devtools::install_github("m-clark/visibly")]
visibly::plot_gam_check(gam_beta_0_fix)
mgcv::gam.check(gam_beta_0_fix, k.rep = 1000)

# Graphical Display, can use package "visreg"
# Replace gam_beta_0_fix below IF want to model with gam_glm_beta_0_fix
namesFactor <- levels(regr_df_beta$bear_sex_age_class)
predictData <- do.call(rbind,
                       lapply(1:length(namesFactor), function (x) {
                         testdata <- data.frame(bear_sex_age_class = rep(namesFactor[x], 
                                                                         length = length(seq(-4, 7, by = 0.01))), 
                                                bear_current_bci = seq(-4, 7, by = 0.01))
                         fits <- predict(gam_beta_0_fix, newdata = testdata, 
                                         type = 'response', se = TRUE)
                         predicts <- data.frame(testdata, fits ) %>% 
                           mutate(lower = fit - qnorm(0.975) * se.fit,
                                  upper = fit + qnorm(0.975) * se.fit)
                         predicts$bear_sex_age_class <- factor(predicts$bear_sex_age_class)
                         return(predicts)
                         })
                       )
# Predicted survival against body condition index
# Note, will use unadjusted values to plot points

label_names1 <- c(
  'female-adult'       = "Adult Female",
  'female-adult-w-off' = "Adult Female with Offspring",
  'female-sub-adult'   = "Sub-adult Female",
  'male-adult'         = "Adult Male",
  'male-sub-adult'     = "Sub-adult Male"
)

g_unadj <- ggplot() +
  geom_point(data = regr_df1, aes(x = bear_current_bci, y = bear_survival_rates), alpha = 0.1) +
  geom_ribbon(data = predictData, aes(x = bear_current_bci, y = fit, 
                                      ymin = lower, ymax = upper), fill = 'gray80') +
  geom_line(data = predictData, aes(x = bear_current_bci, y = fit), color ='black') + #'#00aaff'
  facet_wrap(~ bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names1)) +
  labs(x = "Body Condition Index", y = "Survival Rate") +
  theme_bw() + 
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank()) # remove to get x axis label

## Survival rate adjusted so that bear agents with BCI <= -1 gets a survival value of 0

# Beta regression glm, though using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This more or less models a linear relationship with a slight curve
gam_glm_beta_1_fix <- mgcv::gam(bear_survival_rates_1 ~ bear_sex_age_class + bear_current_bci,
                                family = betar(link = "logit"), data = regr_df_beta)
# Beta regression gam using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This is  adding a basis function (smoother) that mimics a polynomial but with beta distribution instead
# of gaussian distribution using a polynomial regression
gam_beta_1_fix <- mgcv::gam(bear_survival_rates_1 ~ bear_sex_age_class + s(bear_current_bci, bs = 'tp'), 
                            family = betar(link = "logit"), data = regr_df_beta) 

# Model Comparison
bbmle::AICtab(gam_glm_beta_1_fix, gam_beta_1_fix, weights = TRUE, delta = TRUE, 
              base = TRUE, logLik = TRUE, sort = TRUE) # AIC comparison
summary(gam_glm_beta_1_fix)$sp.criterion # GCV
summary(gam_beta_1_fix)$sp.criterion # GCV
summary(gam_glm_beta_1_fix)$r.sq # Adjusted R-squared
summary(gam_beta_1_fix)$r.sq # Adjusted R-squared
anova(gam_glm_beta_1_fix, gam_beta_1_fix, test = "Chisq") # Does non-linear relationship of covariates improve model
performance::r2(gam_glm_beta_1_fix)  # Grabs Adjusted R-squared
performance::r2(gam_beta_1_fix)  # Grabs Adjusted R-squared
performance::check_collinearity(gam_glm_beta_1_fix) # Check for collinearity
performance::check_collinearity(gam_beta_1_fix) # Check for collinearity
performance::check_autocorrelation(gam_glm_beta_1_fix) # Check for autocorrelation
performance::check_autocorrelation(gam_beta_1_fix) # Check for autocorrelation

# Model Diagnostics
# Enhance visual check of [devtools::install_github("m-clark/visibly")]
visibly::plot_gam_check(gam_beta_1_fix)
mgcv::gam.check(gam_beta_1_fix, k.rep = 1000)

# Graphical Display, can use package "visreg"
# Replace gam_beta_1_fix below IF want to model with gam_glm_beta_1_fix
namesFactor1 <- levels(regr_df_beta$bear_sex_age_class)
predictData1 <- do.call(rbind,
                       lapply(1:length(namesFactor1), function (x) {
                         testdata <- data.frame(bear_sex_age_class = rep(namesFactor1[x], 
                                                                         length = length(seq(-4, 7, by = 0.01))), 
                                                bear_current_bci = seq(-4, 7, by = 0.01))
                         fits <- predict(gam_beta_1_fix, newdata = testdata, 
                                         type = 'response', se = TRUE)
                         predicts <- data.frame(testdata, fits) %>% 
                           mutate(lower = fit - qnorm(0.975) * se.fit,
                                  upper = fit + qnorm(0.975) * se.fit)
                         predicts$bear_sex_age_class <- factor(predicts$bear_sex_age_class)
                         return(predicts)
                       })
)
  
# Predicted survival against body condition index
regr_df_beta_1 <- regr_df_beta
# backtransformed for plotting purposes
regr_df_beta_1$bear_survival_rates_1_backtransformed <- backtransform.est(regr_df_beta_1$bear_survival_rates_1, 
                                                                          nrow(regr_df_beta_1))
g_adj <- ggplot() +
  geom_point(data = regr_df_beta_1, aes(x = bear_current_bci, y = bear_survival_rates_1_backtransformed), 
             alpha = 0.1) +
  geom_ribbon(data = predictData1, aes(x = bear_current_bci, y = fit, 
                                      ymin = lower, ymax = upper), fill = 'gray80') +
  geom_line(data = predictData1, aes(x = bear_current_bci, y = fit), color ='black') + #'#00aaff'
  facet_wrap(~ bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names1)) +
  labs(x = "Body Condition Index", y = "Survival Rate") +
  theme_bw() + 
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

## Survival rate adjusted so that bear agents with BCI <= 0 gets a survival value of 0

# Beta regression glm, though using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This more or less models a linear relationship with a slight curve
gam_glm_beta_1_fix <- mgcv::gam(bear_survival_rates_0 ~ bear_sex_age_class + bear_current_bci,
                                family = betar(link = "logit"), data = regr_df_beta)
# Beta regression gam using mgcv package; bear_survival rates are transformed so that 0 are > 0
# This is  adding a basis function (smoother) that mimics a polynomial but with beta distribution instead
# of gaussian distribution using a polynomial regression
gam_beta_1_fix <- mgcv::gam(bear_survival_rates_0 ~ bear_sex_age_class + s(bear_current_bci, bs = 'tp'), 
                            family = betar(link = "logit"), data = regr_df_beta) 

# Model Comparison
bbmle::AICtab(gam_glm_beta_1_fix, gam_beta_1_fix, weights = TRUE, delta = TRUE, 
              base = TRUE, logLik = TRUE, sort = TRUE) # AIC comparison
summary(gam_glm_beta_1_fix)$sp.criterion # GCV
summary(gam_beta_1_fix)$sp.criterion # GCV
summary(gam_glm_beta_1_fix)$r.sq # Adjusted R-squared
summary(gam_beta_1_fix)$r.sq # Adjusted R-squared
anova(gam_glm_beta_1_fix, gam_beta_1_fix, test = "Chisq") # Does non-linear relationship of covariates improve model
performance::r2(gam_glm_beta_1_fix)  # Grabs Adjusted R-squared
performance::r2(gam_beta_1_fix)  # Grabs Adjusted R-squared
performance::check_collinearity(gam_glm_beta_1_fix) # Check for collinearity
performance::check_collinearity(gam_beta_1_fix) # Check for collinearity
performance::check_autocorrelation(gam_glm_beta_1_fix) # Check for autocorrelation
performance::check_autocorrelation(gam_beta_1_fix) # Check for autocorrelation

# Model Diagnostics
# Enhance visual check of [devtools::install_github("m-clark/visibly")]
visibly::plot_gam_check(gam_beta_1_fix)
mgcv::gam.check(gam_beta_1_fix, k.rep = 1000)

# Graphical Display, can use package "visreg"
# Replace gam_beta_1_fix below IF want to model with gam_glm_beta_1_fix
namesFactor1 <- levels(regr_df_beta$bear_sex_age_class)
predictData1 <- do.call(rbind,
                        lapply(1:length(namesFactor1), function (x) {
                          testdata <- data.frame(bear_sex_age_class = rep(namesFactor1[x], 
                                                                          length = length(seq(-4, 7, by = 0.01))), 
                                                 bear_current_bci = seq(-4, 7, by = 0.01))
                          fits <- predict(gam_beta_1_fix, newdata = testdata, 
                                          type = 'response', se = TRUE)
                          predicts <- data.frame(testdata, fits) %>% 
                            mutate(lower = fit - qnorm(0.975) * se.fit,
                                   upper = fit + qnorm(0.975) * se.fit)
                          predicts$bear_sex_age_class <- factor(predicts$bear_sex_age_class)
                          return(predicts)
                        })
)

# Predicted survival against body condition index
regr_df_beta_1 <- regr_df_beta
# backtransformed for plotting purposes
regr_df_beta_1$bear_survival_rates_1_backtransformed <- backtransform.est(regr_df_beta_1$bear_survival_rates_0, 
                                                                          nrow(regr_df_beta_1))
ggplot() +
  geom_point(data = regr_df_beta_1, aes(x = bear_current_bci, y = bear_survival_rates_1_backtransformed), 
             alpha = 0.1) +
  geom_ribbon(data = predictData1, aes(x = bear_current_bci, y = fit, 
                                       ymin = lower, ymax = upper), fill = 'gray80') +
  geom_line(data = predictData1, aes(x = bear_current_bci, y = fit), color ='black') + #'#00aaff'
  facet_wrap(~ bear_sex_age_class, labeller = labeller(bear_sex_age_class = label_names1)) +
  labs(x = "Body Condition Index", y = "Survival Rate") +
  theme_bw() + 
  theme(strip.text = element_text(face = "bold", size = 15),
        strip.background = element_rect(fill = "#CCCCFF"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))


# Plot unadjusted (surv. = 0 for any BCI) and adjusted (surv.= 0 with BCI <= -1)
library(cowplot)

cowplot::plot_grid(g_unadj, g_adj,
                   rel_heights = c(1, 1),
                   ncol = 1,
                   labels = c("A","B"),
                   label_size = 18,
                   label_fontfamily = "sans")

###########
### END ###
###########






ggplot(regr_df1, aes(x = bear_current_bci, y = bear_survival_rates)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "gam") +
  #stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  ylim(0, 1) + 
  labs(x = "Body Condition Index (BCI)", y = "Survival Rate", title = "Survival Rates [BCI < 1] = 0") +
  facet_wrap(~ bear_sex_age_class)

ggplot(survival, aes(x = bear_current_bci, y = bear_survival_rates_adj, color = bear_sex_age_class)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "gam") +
#stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  ylim(0, 1) + 
  facet_wrap(~ bear_sex_age_class)

ggplot(survival, aes(x = bear_road_dens, y = bear_survival_rates, linetype = bear_sex_age_class)) +
  geom_point()
 stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
 
ggplot(survival, aes(x = bear_current_bci_adj, y = bear_road_dens, color = bear_sex_age_class)) +
  geom_point(alpha = 0.1) +
  stat_smooth(method = "loess") +
  #stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x, 3, raw = TRUE)) +
  ylim(0, 7.5) + 
  facet_wrap(~ bear_sex_age_class)
 
  
#-------------------------------------------------------------------------------------------------------------------
###########
### END ###
###########

# Label for road density and disturbance percentage
road_dist_perc_label <- parse(text = paste(round(watershed_perc$km_km2.x, digits = 2), 
                                           "~km/km^2~\";\"~",
                                           round(watershed_perc$perc, digits = 2),
                                           "~\"%\""))

# Plot of road density and disturbance percentage labels
ggplot(data = watershed_perc) +
  geom_sf(aes(fill = km_km2.x)) +
  geom_sf_text(label = road_dist_perc_label) +
  coord_sf(datum = st_crs(26911))





 
 
 
 
 
 

  stat_density_2d(aes(alpha = ..level.., fill = bear_current_bci_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_wrap(~bear_sex + bear_current_bci_cat) +
  coord_sf(datum = st_crs(26911))

, aes(x = location.xcor, y = location.ycor)) +
  stat_density_2d(aes(alpha = ..level.., fill = bear_current_bci_cat,), 
                  geom = "polygon", bins = 10,
                  contour_var = "ndensity") + 
  facet_wrap(~bear_sex + bear_current_bci_cat) +
  coord_sf(datum = st_crs(26911))



ggplot(df_all[which(df_all$run_number == 4), ], 
       aes(x = location.xcor, y = location.ycor, fill = bear_current_bci)) +
  geom_point()




test$bear.id <- apply(test[, c("run_number", "location.who")], 1, paste, collapse = ".")

t <- test[which(test$run_number == 7), ]
t2 <- t[!is.na(t$location.xcor), ]

ggplot(test[which(test$run_number == 4), ], aes(x = location.xcor, 
                                                              y = location.ycor, fill = bear_current_bci)) +
  geom_point()


  stat_density_2d(
    geom = "polygon",
    aes(fill = ..level..),
    contour = FALSE
  ) + scale_fill_viridis_c()

ggplot(df_all3_male) + 
  geom_sf(data = watershed_boundaries, fill = NA) +
  geom_point(aes(x = df_all3_male_Coords[, 1], 
                 y = df_all3_male_Coords[, 2]),
             alpha = 0.1) +
  facet_wrap(~bear_sex_age_class)


  stat_density_2d(aes(x = df_all3_male_Coords[, 1], y = df_all3_male_Coords[, 2],
    geom = "polygon",
    aes(fill = ..level..),
    contour = FALSE)) +
  facet_wrap(~bear_sex_age_class)

  stat_density_2d(
    geom = "polygon",
    aes(fill = ..level..),
    contour = FALSE
  ) + scale_fill_viridis_c()

stat_density_2d(
  geom = "raster",
  aes(fill = after_stat(density)),
  contour = FALSE
) + scale_fill_viridis_c()





#  scale_fill_distiller(palette = "Spectral", direction = 1)

fill_rows <- function(x) {
  for(i in nrow(x)[-1]) if(is.na(test[, 7])) x[,7 ] <- x[i - 1, ]
  return(x)
}

library(tidyr)
tidyr::fill(test, colnames(test[, 7:ncol(test)]))
