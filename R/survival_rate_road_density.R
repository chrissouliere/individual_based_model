# generates survival rate curves as function of road density for 
# adult males, sub-adult males, adult-females with cubs/yearling, adult-females with 2yrs or alone, and sub-adult females


am <- read.csv("../IBMData/rawR/Survival_RoadDensity/adult_males.csv", header = TRUE)
sub_m <- read.csv("../IBMData/rawR/Survival_RoadDensity/subadult_males.csv", header = TRUE)
af <- read.csv("../IBMData/rawR/Survival_RoadDensity/females_with_2yrs_or_alone.csv", header = TRUE)
af_cy <- read.csv("../IBMData/rawR/Survival_RoadDensity/females_with_cubs_yearlings.csv", header = TRUE)
sub_f <- read.csv("../IBMData/rawR/Survival_RoadDensity/subadult_females.csv", header = TRUE)

################
### adult male
################

mod_am   <- lm(survival_rate ~ road_density, data = am)
#second degree
mod_am2 <- lm(survival_rate ~ poly(road_density, 2, raw=TRUE), data = am)
#third degree
mod_am3 <- lm(survival_rate ~ poly(road_density, 3, raw=TRUE), data = am)
#fourth degree
mod_am4 <- lm(survival_rate ~ poly(road_density, 4, raw=TRUE), data = am)
#generate range of numbers 
xx <- seq(0, 4,length = 600)
yy <- seq(0, 1, length = 600)
plot(survival_rate ~ road_density, data = am, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Adult Male")
lines(xx, predict(mod_am, data.frame(survival_rate = yy, road_density = xx)), col="red")
lines(xx, predict(mod_am2, data.frame(survival_rate = yy, road_density = xx)), col="orange")
lines(xx, predict(mod_am3, data.frame(survival_rate = yy, road_density = xx)), col="blue")
lines(xx, predict(mod_am4, data.frame(survival_rate = yy, road_density = xx)), col="black")

mod_am3fun <- function (r) {
  y <- 0.946014 + (-0.042284 * r) + (-0.031795 * r ^ 2) + (-0.006340 * r ^ 3)
  return (y)
}

lines(seq(0,4,0.1), mod_am3fun (seq(0,4,0.1)), col = "blue") 


###################
### sub-adult male
###################

mod_sub_m   <- lm(survival_rate ~ road_density, data = sub_m)
#second degree
mod_sub_m2 <- lm(survival_rate ~ poly(road_density, 2, raw=TRUE), data = sub_m)
#third degree
mod_sub_m3 <- lm(survival_rate ~ poly(road_density, 3, raw=TRUE), data = sub_m)
#fourth degree
mod_sub_m4 <- lm(survival_rate ~ poly(road_density, 4, raw=TRUE), data = sub_m)
#generate range of numbers 
xx <- seq(0, 4,length = 600)
yy <- seq(0, 1, length = 600)
plot(survival_rate ~ road_density, data = sub_m, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Sub-adult Male")
lines(xx, predict(mod_sub_m, data.frame(survival_rate = yy, road_density = xx)), col="red")
lines(xx, predict(mod_sub_m2, data.frame(survival_rate = yy, road_density = xx)), col="orange")
lines(xx, predict(mod_sub_m3, data.frame(survival_rate = yy, road_density = xx)), col="blue")
lines(xx, predict(mod_sub_m4, data.frame(survival_rate = yy, road_density = xx)), col="black")

mod_sub_m3fun <- function (r) {
  y <- 0.942445 + (0.080035 * r) + (-0.353526 * r ^ 2) + (0.075524 * r ^ 3)
  return (y)
}

lines(seq(0,3.1,0.1), mod_sub_m3fun (seq(0,3.1,0.1)), col = "blue")

######################################################
### adult female with no cubs or with 2+ yearlings ###
######################################################

mod_af   <- lm(survival_rate ~ road_density, data = af)
#second degree
mod_af2 <- lm(survival_rate ~ poly(road_density, 2, raw=TRUE), data = af)
#third degree
mod_af3 <- lm(survival_rate ~ poly(road_density, 3, raw=TRUE), data = af)
#fourth degree
mod_af4 <- lm(survival_rate ~ poly(road_density, 4, raw=TRUE), data = af)
#generate range of numbers 
xx <- seq(0, 4,length = 600)
yy <- seq(0, 1, length = 600)
plot(survival_rate ~ road_density, data = af, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Lone adult female")
lines(xx, predict(mod_af, data.frame(survival_rate = yy, road_density = xx)), col="red")
lines(xx, predict(mod_af2, data.frame(survival_rate = yy, road_density = xx)), col="orange")
lines(xx, predict(mod_af3, data.frame(survival_rate = yy, road_density = xx)), col="blue")
lines(xx, predict(mod_af4, data.frame(survival_rate = yy, road_density = xx)), col="black")

mod_af2fun <- function (r) {
  y <- 0.984809 + (-0.009914 * r) + (-0.042663 * r ^ 2)
  return (y)
}

lines(seq(0,5,0.1), mod_af2fun (seq(0,5,0.1)), col = "blue") 

######################################################
### adult female with cubs or 1 year-old yearlings ###
######################################################

mod_af_cy   <- lm(survival_rate ~ road_density, data = af_cy)
#second degree
mod_af2_cy <- lm(survival_rate ~ poly(road_density, 2, raw=TRUE), data = af_cy)
#third degree
mod_af3_cy <- lm(survival_rate ~ poly(road_density, 3, raw=TRUE), data = af_cy)
#fourth degree
mod_af4_cy <- lm(survival_rate ~ poly(road_density, 4, raw=TRUE), data = af_cy)
#generate range of numbers 
xx <- seq(0, 4,length = 600)
yy <- seq(0, 1, length = 600)
plot(survival_rate ~ road_density, data = af_cy, ylim = c(0, 1), xlim = c(0, 10))
lines(xx, predict(mod_af_cy, data.frame(survival_rate = yy, road_density = xx)), col="red")
lines(xx, predict(mod_af2_cy, data.frame(survival_rate = yy, road_density = xx)), col="orange")
lines(xx, predict(mod_af3_cy, data.frame(survival_rate = yy, road_density = xx)), col="blue")
lines(xx, predict(mod_af4_cy, data.frame(survival_rate = yy, road_density = xx)), col="black")

mod_af3_cyfun <- function (r) {
  y <- 0.97954 + (-0.11163 * r) + (0.14217 * r ^ 2) + (-0.14685 * r ^ 3)
  return (y)
}

lines(seq(0,6,0.1), mod_af3_cyfun (seq(0,6,0.1)), col = "yellow") 

########################
### sub-adult female ###
########################

mod_sub_f   <- lm(survival_rate ~ road_density, data = sub_f)
#second degree
mod_sub_f2 <- lm(survival_rate ~ poly(road_density, 2, raw=TRUE), data = sub_f)
#third degree
mod_sub_f3 <- lm(survival_rate ~ poly(road_density, 3, raw=TRUE), data = sub_f)
#fourth degree
mod_sub_f4 <- lm(survival_rate ~ poly(road_density, 4, raw=TRUE), data = sub_f)
#generate range of numbers 
xx <- seq(0, 4,length = 600)
yy <- seq(0, 1, length = 600)
plot(survival_rate ~ road_density, data = sub_f, ylim = c(0, 1), xlim = c(0, 10))
lines(xx, predict(mod_sub_f, data.frame(survival_rate = yy, road_density = xx)), col="red")
lines(xx, predict(mod_sub_f2, data.frame(survival_rate = yy, road_density = xx)), col="orange")
lines(xx, predict(mod_sub_f3, data.frame(survival_rate = yy, road_density = xx)), col="blue")
lines(xx, predict(mod_sub_f4, data.frame(survival_rate = yy, road_density = xx)), col="black")

mod_sub_f3fun <- function (r) {
  y <- 0.96643 + (0.11602 * r) + (-0.21249 * r ^ 2) + (0.02300 * r ^ 3)
  return (y)
}

lines(seq(0,6,0.1), mod_sub_f3fun (seq(0,6,0.1)), col = "yellow")

# Plots for TRACE
par(mfrow = c(2, 3))
plot(survival_rate ~ road_density, data = am, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Adult Male", cex.lab = 1.2, cex.axis = 1.2)
lines(seq(0,4,0.1), mod_am3fun (seq(0,4,0.1)), col = "blue") 
plot(survival_rate ~ road_density, data = af, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Lone Adult Female", cex.lab = 1.2, cex.axis = 1.2)
lines(seq(0,5,0.1), mod_af2fun (seq(0,5,0.1)), col = "blue")
plot(survival_rate ~ road_density, data = sub_m, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Sub-adult Male", cex.lab = 1.2, cex.axis = 1.2)
lines(seq(0,3.1,0.1), mod_sub_m3fun (seq(0,3.1,0.1)), col = "blue")
plot(survival_rate ~ road_density, data = sub_f, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Sub-adult Female", cex.lab = 1.2, cex.axis = 1.2)
lines(seq(0,5,0.1), mod_sub_f3fun (seq(0,5,0.1)), col = "blue")
plot(survival_rate ~ road_density, data = af_cy, ylim = c(0, 1), xlim = c(0, 5),
     xlab = expression(Road~Density~(km/km^{2})), ylab = "Survival rate", pch = 19,
     main = "Adult Female with\n Cubs or Yearlings", cex.lab = 1.2, cex.axis = 1.2)
lines(seq(0,5,0.1), mod_af3_cyfun  (seq(0,5,0.1)), col = "blue")
