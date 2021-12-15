
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
getPackages("ggplot2",   # Plotting
            "reshape2"  # AIC tab
)

# read in cleaned results and adjust file name accordingly
sim_results <- read.csv("output/BearIBMCleaned/bear_IBMv13_ResultsCleanedRv11.csv", 
                        header = TRUE, stringsAsFactors = FALSE)

# relevel to adult and sub-adult
sim_results[which(as.logical(sim_results$bear_cub)), "bear_age_class"] <- "adult_w_cub"
sim_results[which(as.logical(sim_results$bear_yearling)), "bear_age_class"] <- "adult_w_year"
sim_results$bear_age_class<- factor(sim_results$bear_age_class)
levels(sim_results$bear_age_class) <- c("adult", "adult_w_cy" , "adult_w_cy","adult", "sub-adult")

#################################
### Spring and Fall Body Mass ###
#################################

mass_df <- melt(sim_results, id.vars = c("bear_sex", "bear_age_class"), 
                measure.vars = c("bear_spring_body_mass", "bear_body_mass"),
                variable.name = "bear_mass_id",
                value.name = "bear_mass_value")

labels_p1 <- c("bear_spring_body_mass" = "Spring", "bear_body_mass" = "Fall")
labels_p11 <- c("Adult", "Adult w/ C/Y", "Sub-adult")

p1 <- ggplot(mass_df, aes(x = bear_age_class, y = bear_mass_value)) + 
  geom_boxplot(aes(fill = factor(bear_sex))) +
  scale_x_discrete(labels= labels_p11) +
  facet_wrap(~bear_mass_id, labeller = labeller(bear_mass_id = labels_p1)) +
  labs(x = "Age-class", y = "Body Mass (kg)") +
  guides(fill = guide_legend(title = "Sex")) 


###############################################
### Body Mass as a function of Road Density ###
###############################################

labels_p2 <- c("female" = "Female", "male" = "Male") 
sim_results$bear_road_dens_bin <- with(sim_results, 
                                       cut(bear_road_dens, seq(from = -0.05,
                                                               to = 3, 
                                                               by = 0.2)))

p2 <- ggplot(sim_results, aes(x = bear_road_dens, y = bear_body_mass)) +
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(y = "Body Mass (kg)", x = expression(Road~Density~(km/km^2))) +
  guides(col = guide_legend(title = "Age-class"))

sim_results_p2a <- sim_results 
sim_results_p2a$bear_road_dens <- round(sim_results_p2a$bear_road_dens, digits = 2)

#sim_results_agg <- with(sim_results, aggregate(bear_spring_body_mass ~ run_number + bear_sex + bear_age_class, FUN = mean))
# bin bear_body_mass within 0.01 interval of bear_road_dens
sim_results_p2a <- with(sim_results_p2a, aggregate(list(bear_body_mass), 
                                                  by = list(run_number, bear_sex, bear_age_class, bear_road_dens), 
                                                  FUN = mean))
colnames(sim_results_p2a) <- c("run_number", "bear_sex", "bear_age_class", "bear_road_dens", "bear_body_mass")


p2a <- ggplot(sim_results_p2a, aes(x = bear_road_dens, y = bear_body_mass)) + 
  geom_point(aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(y = "Body Mass (kg)", x = expression(Road~Density~(km/km^2))) +
  guides(col = guide_legend(title = "Sex"))

########################################################
### Bear Survival Rate as a function of Road Density ###
########################################################

labels_p3 <- c("female" = "Female", "male" = "Male") 

p3 <- ggplot(sim_results, aes(x = bear_road_dens, y = bear_survival_rates)) + 
  geom_point(aes(col = bear_age_class)) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p3)) +
  labs(y = "Bear Surivival Rate", x = expression(Road~Density~(km/km^2))) +
  guides(col = guide_legend(title = "Age-class"))

sim_results_p3a <- sim_results 
sim_results_p3a$bear_road_dens <- round(sim_results_p3a$bear_road_dens, digits = 2)

#sim_results_agg <- with(sim_results, aggregate(bear_spring_body_mass ~ run_number + bear_sex + bear_age_class, FUN = mean))
# bin bear_body_mass within 0.01 interval of bear_road_dens
sim_results_p3a <- with(sim_results_p3a, aggregate(list(bear_survival_rates), 
                                                   by = list(run_number, bear_sex, bear_age_class, bear_road_dens), FUN = mean))
colnames(sim_results_p3a) <- c("run_number", "bear_sex", "bear_age_class", "bear_road_dens", "bear_survival_rates")

p3a <- ggplot(sim_results_p3a, aes(x = bear_road_dens, y = bear_survival_rates)) + 
  geom_point(aes(col = bear_age_class)) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p3)) +
  labs(y = "Bear Surivival Rate", x = expression(Road~Density~(km/km^2))) +
  guides(col = guide_legend(title = "Age-class"))

##############################################################
### Bear Survival Percentage as a function of Road Density ###
##############################################################
sim_results_p4a <- sim_results
sim_results_p4a <- with(sim_results_p4a, aggregate(list(bear_road_dens), 
                                                   by = list(run_number, bear_sex, bear_age_class, bear_survive), FUN = mean))
colnames(sim_results_p4a) <- c("run_number", "bear_sex", "bear_age_class", "bear_survive", "bear_road_dens")

labels_p4 <- c("female" = "Female", "male" = "Male")
labels_p41 <- c("FALSE", "TRUE")


p4a <- ggplot(sim_results_p4a, aes(x = bear_survive, y = bear_road_dens)) + 
  geom_boxplot(aes(fill = factor(bear_age_class))) +
  scale_x_discrete(labels= labels_p41) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p4)) +
  labs(y = expression(Road~Density~(km/km^2)), x = "Bear Survive") +
  guides(fill = guide_legend(title = "Age-class"))

sim_results_p4b <- sim_results
sim_results_p4b <- with(sim_results_p4b, aggregate(list(bear_body_mass), 
                                                   by = list(run_number, bear_sex, bear_age_class, bear_survive), FUN = mean))
colnames(sim_results_p4b) <- c("run_number", "bear_sex", "bear_age_class", "bear_survive", "bear_body_mass")

p4b <- ggplot(sim_results_p4b, aes(x = bear_survive, y = bear_body_mass)) + 
  geom_boxplot(aes(fill = factor(bear_age_class))) +
  scale_x_discrete(labels= labels_p41) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p4)) +
  labs(y = "Body Mass (kg)", x = "Bear Survive") +
  guides(fill = guide_legend(title = "Age-class"))

sim_results_p4c <- sim_results
sim_results_p4c <- with(sim_results_p4c, aggregate(list(bear_current_bci), 
                                                   by = list(run_number, bear_sex, bear_age_class, bear_survive), FUN = mean))
colnames(sim_results_p4c) <- c("run_number", "bear_sex", "bear_age_class", "bear_survive", "bear_current_bci")

p4c <- ggplot(sim_results_p4c, aes(x = bear_survive, y = bear_current_bci)) + 
  geom_boxplot(aes(fill = factor(bear_age_class))) +
  scale_x_discrete(labels= labels_p41) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p4)) +
  labs(y = "Body COndition Index", x = "Bear Survive") +
  guides(fill = guide_legend(title = "Age-class"))

##########################################################
### Body Condition Index as a function of Road Density ###
##########################################################

p5 <- ggplot(sim_results, aes(x = bear_road_dens, y = bear_current_bci)) + 
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(x = expression(Road~Density~(km/km^2)), y = "Body Condition Index") +
  guides(col = guide_legend(title = "Age-class"))

###########################################################
### Survival Rate as a function of Body Condition Index ###
###########################################################

p6 <- ggplot(sim_results, aes(x = bear_current_bci, y = bear_survival_rates)) + 
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(x = "Body Condition Index", y = "Survival Rate") +
  guides(col = guide_legend(title = "Age-class")) 
 

p6a <- ggplot(sim_results, aes(x = bear_current_bci, y = bear_survival_rates)) + 
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(x = "Body Condition Index", y = "Survival Rate") +
  guides(col = guide_legend(title = "Age-class")) + 
  xlim(-5, 5)

#####################################################
### Distance Traveled as function of Road Density ###
#####################################################

p7 <- ggplot(sim_results, aes(x = bear_road_dens, y = bear_total_dist)) + 
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(x = expression(Road~Density~(km/km^2)), y = "Distance Traveled (km)") +
  guides(col = guide_legend(title = "Age-class")) 

##################################################
### Body Mass as function of Distance Traveled ###
##################################################

p8 <- ggplot(sim_results, aes(x = bear_total_dist, y = bear_body_mass)) + 
  geom_point(stat = "identity", aes(col = bear_age_class)) +
  stat_smooth(aes(col = bear_age_class), method = "lm", formula = y ~ x + I(x^2), size = 1) +
  facet_wrap(~bear_sex, labeller = labeller(bear_sex = labels_p2)) +
  labs(x = "Distance Traveled (km)", y = "Body Mass (kg)") +
  guides(col = guide_legend(title = "Age-class")) 

#####################
### Print Figures ###
#####################

l.figures <- mget(c("p1", "p2", "p3", "p4a", "p4b", "p4c", "p5", "p6", "p6a", "p7", "p8"))
pdf("figs/figs/bear_IBMv13_ResultsRv11.pdf", width = 15, height = 8)
invisible(lapply(l.figures, print))
dev.off()

