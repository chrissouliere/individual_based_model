# 1. organises road frequency crossing rate data in dataframe form
# 2. regresses/model road density (watershed_home_dens) against road frequency crossing rate 


# Read in road crossing rate data
df <- read.csv("../IBMData/rawR/randomwalk_road_crossing_rate_100000ticks_9960runs.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read.csv("../IBMData/rawR/randomwalk_road_crossing_rate_10000ticks_960runs.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read.csv("../IBMData/rawR/randomwalk_road_crossing_rate_4500ticks_450runs.csv", header = TRUE, stringsAsFactors = FALSE)
df <- read.csv("../IBMData/rawR/randomwalk_road_crossing_rate_4500ticks1000rep.csv", header = TRUE, stringsAsFactors = FALSE)
# this one is the one used for regressing road_freq crossing to road density
df <- read.csv("../IBMData/rawR/randomwalk_road_crossing_rate_4500ticks1000rep_randomstartloc2.csv", header = TRUE, stringsAsFactors = FALSE)

dat <- lapply(1:nrow(df), function(u) {
  
  foo <- df
  x <- foo[u, 5:length(foo)]
# replace "[" or ("|") "]" with "", then remove with noquote
  y <- lapply(1:length(x), function(i) {
  p <- list()
  p <- noquote(gsub("\\[|\\]", "", x[i]))
  p <-  as.numeric(unlist(strsplit(p, split = " ")))
  return(p)
  })

a <- foo[u, 1:4]
len <- length(unlist(y[1]))
a <- cbind(a[rep(seq_len(nrow(a)), each = len), ], y)
colnames(a)[5:length(foo)] <- colnames(foo[5:length(foo)])

return(a)

})

# this one is the one used for regressing road_freq crossing to road density
datf_1000rep_ranStarLoc <- do.call(rbind, dat) 
datf_1000rep <- do.call(rbind, dat)
datf_450 <- do.call(rbind, dat)
datf_960 <- do.call(rbind, dat)
datf_9960 <- do.call(rbind, dat)
with(datf_9960, plot(x = road_freq, y = watershed_home_dens, main = "Raw value for 9960 runs",
     xlim = c(0, 10000)))
model_1000rep_ranStarLoc <- lm(watershed_home_dens ~ road_freq, data = datf_1000rep_ranStarLoc)
model_1000rep <- lm(watershed_home_dens ~ road_freq, data = datf_1000rep)
model_450 <- lm(watershed_home_dens ~ road_freq, data = datf_450)
model_960 <- lm(watershed_home_dens ~ road_freq, data = datf_960)
model_9960 <- lm(watershed_home_dens ~ road_freq, data = datf_9960)
abline(model_1000rep_ranStarLoc, col = "blue", lwd = 4)
abline(model_1000rep, col = "blue")
abline(model_450, col = "blue", lty = 2)
abline(model_960, col = "blue", lty = 3)
abline(model_9960, col = "blue", lty = 4)

datf2_450 <- with(datf_450, aggregate(road_freq ~ tick_step + watershed_home_dens, FUN = mean))
datf2_960 <- with(datf_960, aggregate(road_freq ~ tick_step + watershed_home_dens, FUN = mean))
datf2_9960 <- with(datf_9960, aggregate(road_freq ~ tick_step + watershed_home_dens, FUN = mean))
with(datf2_9960, plot(x = road_freq, y = watershed_home_dens, main = "Mean value across each tick increment",
     xlim = c(0, 10000)))
model2_450 <- lm(watershed_home_dens ~ road_freq, data = datf2_450)
model2_960 <- lm(watershed_home_dens ~ road_freq, data = datf2_960)
model2_9960 <- lm(watershed_home_dens ~ road_freq, data = datf2_9960)
abline(model2_450, col = "red")
abline(model2_960, col = "red", lty = 2)
abline(model2_9960, col = "red", lty = 3)

# this one is the one used for regressing road_freq crossing to road density
datf3_1000rep_ranStarLoc <- with(datf_1000rep_ranStarLoc, aggregate(road_freq ~ tick_step + watershed_home_dens, FUN = mean))
datf3_1000rep <- with(datf_1000rep, aggregate(road_freq ~ tick_step + watershed_home_dens, FUN = mean))
datf3_450 <- with(datf2_450, aggregate(road_freq ~ watershed_home_dens, FUN = mean))
datf3_960 <- with(datf2_960, aggregate(road_freq ~ watershed_home_dens, FUN = mean))
datf3_9960 <- with(datf2_9960, aggregate(road_freq ~ watershed_home_dens, FUN = mean))
with(datf3_1000rep_ranStarLoc, plot(x = road_freq, y = watershed_home_dens, main = "Mean value across each tick increment and watershed_home_dens",
                         xlim = c(0, 1000), ylim = c(0,2)))
with(datf3_1000rep, plot(x = road_freq, y = watershed_home_dens, main = "Mean value across each tick increment and watershed_home_dens",
                     xlim = c(0, 1000), ylim = c(0,2)))
with(datf3_450, plot(x = road_freq, y = watershed_home_dens, main = "Mean value across each tick increment and watershed_home_dens",
                      xlim = c(0, 1000), ylim = c(0,2)))
with(datf3_9960, plot(x = road_freq, y = watershed_home_dens, main = "Mean value across each tick increment and watershed_home_dens",
                 xlim = c(0, 10000), ylim = c(0,10)))
# this one is the one used for regressing road_freq crossing to road density
model3_1000rep_ranStarLoc <- lm(watershed_home_dens ~ road_freq, data = datf3_1000rep_ranStarLoc)
model3_1000rep <- lm(watershed_home_dens ~ road_freq, data = datf3_1000rep)
model3_450 <- lm(watershed_home_dens ~ road_freq, data = datf3_450)
model3_960 <- lm(watershed_home_dens ~ road_freq, data = datf3_960)
model3_9960 <- lm(watershed_home_dens ~ road_freq, data = datf3_9960)
abline(model3_1000rep_ranStarLoc, col = "orange", lwd = 4)
abline(model3_1000rep, col = "orange")
abline(model3_450, col = "orange", lty = 2)
abline(model3_960, col = "orange", lty = 3)
abline(model3_9960, col = "orange", lty = 4)

fit_nls <- with(datf3, nls(road_freq ~ (watershed_home_dens ^ b), start = c(b = 5), trace = T))
coef(fit_nls)
lines(1:100, (1:100)*coef(fit_nls), col = "red")

# Figure for TRACE
library(ggplot2)
ggplot(data = datf3_1000rep_ranStarLoc, aes(x = road_freq, y = watershed_home_dens)) +
  geom_point() + 
  xlim(0, 750) +
  stat_smooth(method = "lm", fullrange = TRUE) +
  labs(x = "Road Crossing Frequency", y = expression(Road~Density~(km/km^{2}))) +
  theme_classic() +
  theme(text = element_text(size = 15), axis.text = element_text(size = 15))
