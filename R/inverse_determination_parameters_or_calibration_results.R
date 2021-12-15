## This script cleans the results of a single simulation purposely run to assess inverse determination of two key 
## parameters (i.e calibration). Calibration results documented in section 6 of the TRACE document (model output verification).

# Read in cleaned data
df_calib <- read.csv("output/BearIBMCleaned/bear_IBMv17_ResultsCleanedRv16.csv", 
                     header = TRUE, stringsAsFactors = FALSE)

# Create column to get percentage increase in bear weight between den entry and den exit  
df_calib$mass_perc <- with(df_calib, ((bear_body_mass - bear_spring_body_mass) / bear_spring_body_mass) * 100)

# Merge older adult with adult for both male and female
df_calib$bear_age_class2 <- factor(df_calib$bear_age_class)
levels(df_calib$bear_age_class2)[2] <- "adult"

# Merge sex grouping with age-class grouping
df_calib$bear_sex_age_class <- paste(df_calib$bear_sex, df_calib$bear_age_class2, sep = "-")

# Aggregate the data to assess two key parameters: 
# proportion of day active coefficient and minimum hourly energy

calib_mean1 <- with(df_calib, aggregate(mass_perc ~ active.coeff + min_energy_hourly + bear_sex_age_class, FUN = mean))

calib_mean2 <- with(df_calib, aggregate(mass_perc ~ active.coeff + bear_sex_age_class, FUN = mean))

calib_SE1 <- with(df_calib, aggregate(mass_perc ~ active.coeff + min_energy_hourly + bear_sex_age_class, 
                                   FUN = function(x) sd(x) / sqrt(length(x))))

calib_SE2 <- with(df_calib, aggregate(mass_perc ~ active.coeff + bear_sex_age_class, 
                                      FUN = function(x) sd(x) / sqrt(length(x))))

# Could use merge instead
calib_mean1_SE1 <- cbind(calib_mean1, calib_SE1$mass_perc)
