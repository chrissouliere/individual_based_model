
# Read in capture data
df <- read.csv("../IBMData/rawR/capture_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Columns include date of capture, day of year, day of birth, and age in years
df$POSIX_date <- as.POSIXct(df$eventDate, format = "%m/%d/%Y") # date of capture
df <- df[!is.na(df$POSIX_date),]
df$year <- as.factor(format(df$POSIX_date, format = "%Y"))
df$doy <- strftime(as.POSIXct(df$eventDate, format = "%m/%d/%Y"), format = "%j") # day of year
df$day_of_birth <- as.POSIXct(df$birthDate, format = "%m/%d/%Y") # day of birth
df$age <- difftime(df$POSIX_date, df$day_of_birth, units = "days")/365.25 # age in years adjusted for leap years (0.25)
attributes(df$age) <- NULL # remove unit "days"

# Day of year range April 1st (91) to June 15 (166) [spring only]; adjusted for leap years
IDD <- function(data, x, y) {
  
  a <- 91
  b <- 166
  
  # Leap year
  ly <- with(df, which(year == 2000| year == 2004 | year == 2008 | year == 2012 | year == 2016))
  ly2 <- as.integer(df[ly, "doy"])
  ly3 <- ly[which(ly2 >= a + 1 & ly2 <= b + 1)]
  
  # Normal year
  ny <- seq.int(nrow(df))[-ly]
  ny2  <- as.integer(df[ny, "doy" ])
  ny3 <- ny[which(ny2 >= a & ny2 <= b)]
  
  dat <- c(ly3, ny3)
  return(dat)  
}

bodyMass <- function (i) {
  
  ID <- IDD()
  df1 <- df[ID, c("sexCode", "weightActual", "lengthTotal", "eventDate", "POSIX_date", "year", "doy",
                  "birthDate", "day_of_birth", "age")]
  # Remove na's for age and weightActual + weight Actual < 1000 to take care of 9999 values +
  # ages greater or equal to 3 or all ages
  df2 <- with(df1, df1[!is.na(age & weightActual) & (weightActual < 1000) & (lengthTotal < 300)
                       & (eval(parse(text = with(df1, ifelse(i, "!age < 3", "age < 100"))))) , ])
  df3 <- cbind(seq.int(nrow(df2)), df2)
  colnames(df3)[1] <- "id"
   
  write_csv  <- ifelse(i, "../IBMData/rawR/bodyMassLength_adultOnly.csv",
                       "../IBMData/rawR/bodyMassLength_allAges.csv")
  
  write.csv(df3, write_csv)
  
}

bodyMass(TRUE) # adults only
bodyMass(FALSE) # all ages

# another way but with not adjustement for leap year
# df5 and df7 should be the same
# ID <- which(as.integer(df$doy) >= 92 & as.integer(df$doy) <= 166)
# df1 <- df[ID, c("sexCode", "weightActual", "eventDate", "POSIX_date", "year", "doy",
#                 "birthDate", "day_of_birth", "age")]
# df2 <- df1[!is.na(df1$age),]
# df3 <- df2[!is.na(df2$weightActual),]
# df4 <- df3[df3$weightActual < 1000,]
# df5 <- cbind(seq.int(nrow(df4)), df4)
# colnames(df5)[1] <- "id"
# 
# df6 <- with(df1, df1[!is.na(age & weightActual) & (weightActual < 1000) , ])
# df7 <- cbind(seq.int(nrow(df6)), df6)
# colnames(df7)[1] <- "id"

# adjust time include AM/PM format (%p)
#strptime(paste(df$eventDate, df$eventTime), format = "%m/%d/%Y %H:%M:%S %p")

###################################
### von Bertalanffy calculation ###
###################################

library(FSA)
library(nlstools)

# Read in body mass and length data for all ages
foo <- read.csv("../IBMData/processedR/bodyMassLength_allAges.csv", header = TRUE, stringsAsFactors = FALSE)
foo_m <- foo[with(foo, which(sexCode == "M")), c("age", "lengthTotal", "weightActual")]
foo_m <- foo_m[!with(foo_m, (age >= 3) & (lengthTotal <= 117 | weightActual < 40)), ] # remove length outlier
foo_f <- foo[with(foo, which(sexCode == "F")), c("age", "lengthTotal", "weightActual")]

# Plots showing length and mass by sex in spring (April 1st to June 15)
with(foo_m, plot(age, lengthTotal, main = "male length"))
with(foo_m, plot(age, weightActual, main = "male mass"))
with(foo_f, plot(age, lengthTotal, main = "female length"))
with(foo_f, plot(age, weightActual, main = "female mass"))

# minimum and maximum lengths and weight of 3 years or older
range(foo_m[foo_m$age >= 3, "lengthTotal"])
range(foo_f[foo_f$age >= 3, "lengthTotal"])
range(foo_m[foo_m$age >= 3, "weightActual"])
range(foo_f[foo_f$age >= 3, "weightActual"])

## Male Spring Body Length ##

# Starting values
svTypical_ml <- vbStarts(lengthTotal ~ age, data = foo_m)

# VGBM model
vbTypical <- lengthTotal ~ Linf * (1-exp(-K * (age - t0)))
fitTypical <- nls(vbTypical, data = foo_m, start = svTypical_ml)

# Visual and summary results of model fit
fitPlot(fitTypical, xlab = "Age", ylab = "Total Length (cm)", main = "male spring body length")
overview(fitTypical)

# Residual standard error is also known as residual standard deviation in the regression model OR
# standard error of the estimate; not to be confused with the standard errors that are associated
# with parameter estimates
res <- fitTypical$m$resid() # residuals(difference between observable estimate and the observable sample mean)
attributes(res) <- NULL
degFred <- length(res) - length(fitTypical$m$getPars()) # degree of freedom: n - p
ResSumSq <- sum(res ^ 2) # residual sum of squares
SD <- sqrt(ResSumSq/ degFred) # standard deviation

# Assumption checking: homoscedasticity, normally distributed errors, adequate model fit to data,
# no influential outlying points
par(mfrow = c(1, 2))
plot(fitted(fitTypical), residuals(fitTypical), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
hist(residuals(fitTypical), xlab = "Residuals", main = "")
residPlot(fitTypical) # from FSA package

## Bootstrapping ##

bootTypical <- nlsBoot(fitTypical, niter = 1000) # bootstrap object
summary(bootTypical) # parameter estimates and confidence intervals
bootTypical$estiboot # paramater estimates and SE
confint(bootTypical, plot = TRUE) # confidence intervals + plot
SD_boot <- sqrt(sum(bootTypical$rse ^ 2)/length(bootTypical$rse)) # shoud be similar to SD of fitTypical

# The p-value for testing the hypothesis that H0 : K = 0.5 versus HA : K < 0.5
# if p-value is less than alpha = 0.05, then we can conclude there is enough evidence to conclude that
# K is less than 0.5, otherwise, we can not. 
htest(bootTypical,"K",0.5,"less")

# Illustrates correlations between pairs of paramaters similar to correlation matrix found
# in overview(fitTypical)
plot(bootTypical)

# Predicted length with corresponding confidence intervals and prediction intervals 
# constructed from bootstrapped results
par(mfrow = c(2, 2))
ages2plot <- 0:25
fitPlot(fitTypical, xlab = "Age", ylab = "Total Length (cm)", xlim = range(ages2plot),
        main = "Male Spring Body Length") # "Fitted VBGM to male spring body length" VGBM refers to von Bertalanffy Growth Model
LCI <- UCI <- LPI <- UPI <- numeric(length(ages2plot))
ests <- bootTypical$coefboot
for (i in 1:length(ages2plot)) {
  pv <- ests[, "Linf"] * (1 - exp(- ests[, "K"] * (ages2plot[i] - ests[, "t0"])))
  UCI[i] <- quantile(pv, 0.975)
  LCI[i] <- quantile(pv, 0.025)
  UPI[i] <- quantile(pv + bootTypical$rse, 0.975)
  LPI[i] <- quantile(pv - bootTypical$rse, 0.025)
}
lines(UCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(LCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(UPI ~ ages2plot, type = "l", lty = 2, col = "red")
lines(LPI ~ ages2plot, type = "l", lty = 2, col = "red")
legend("bottomright", legend = c("95% bootstrap CIs", "95% bootstrap prediction bounds"), 
       col = c("blue", "red"), lty = c(2, 2))

## Female Spring Body Length ##

# Starting values
svTypical_fm <- vbStarts(lengthTotal ~ age, data = foo_f)

# VGBM model
vbTypical <- lengthTotal ~ Linf * (1-exp(-K * (age - t0)))
fitTypical <- nls(vbTypical, data = foo_f, start = svTypical_fm)

# Visual and summary results of model fit
fitPlot(fitTypical, xlab = "Age", ylab = "Total Length (cm)", main = "female spring body length")
overview(fitTypical)

# Residual standard error is also known as residual standard deviation in the regression model OR
# standard error of the estimate; not to be confused with the standard errors that are associated
# with parameter estimates
res <- fitTypical$m$resid() # residuals(difference between observable estimate and the observable sample mean)
attributes(res) <- NULL
degFred <- length(res) - length(fitTypical$m$getPars()) # degree of freedom: n - p
ResSumSq <- sum(res ^ 2) # residual sum of squares
SD <- sqrt(ResSumSq/ degFred) # standard deviation

# Assumption checking: homoscedasticity, normally distributed errors, adequate model fit to data,
# no influential outlying points
par(mfrow = c(1, 2))
plot(fitted(fitTypical), residuals(fitTypical), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
hist(residuals(fitTypical), xlab = "Residuals", main = "")
residPlot(fitTypical) # from FSA package

## Bootstrapping ##

bootTypical <- nlsBoot(fitTypical, niter = 1000) # bootstrap object
summary(bootTypical) # parameter estimates and confidence intervals
bootTypical$estiboot # paramater estimates and SE
confint(bootTypical, plot = TRUE) # confidence intervals + plot
SD_boot <- sqrt(sum(bootTypical$rse ^ 2)/length(bootTypical$rse)) # shoud be similar to SD of fitTypical

# The p-value for testing the hypothesis that H0 : K = 0.5 versus HA : K < 0.5
# if p-value is less than alpha = 0.05, then we can conclude there is enough evidence to conclude that
# K is less than 0.5, otherwise, we can not. 
htest(bootTypical,"K",0.5,"less")

# Illustrates correlations between pairs of paramaters similar to correlation matrix found
# in overview(fitTypical)
plot(bootTypical)

# Predicted length with corresponding confidence intervals and prediction intervals 
# constructed from bootstrapped results
par(mfrow = c(1, 1))
ages2plot <- 0:25
fitPlot(fitTypical, xlab = "Age", ylab = "Total Length (cm)", xlim = range(ages2plot),
        main = "Female Spring Body Length")
LCI <- UCI <- LPI <- UPI <- numeric(length(ages2plot))
ests <- bootTypical$coefboot
for (i in 1:length(ages2plot)) {
  pv <- ests[, "Linf"] * (1 - exp(- ests[, "K"] * (ages2plot[i] - ests[, "t0"])))
  UCI[i] <- quantile(pv, 0.975)
  LCI[i] <- quantile(pv, 0.025)
  UPI[i] <- quantile(pv + bootTypical$rse, 0.975)
  LPI[i] <- quantile(pv - bootTypical$rse, 0.025)
}
lines(UCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(LCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(UPI ~ ages2plot, type = "l", lty = 2, col = "red")
lines(LPI ~ ages2plot, type = "l", lty = 2, col = "red")
legend("bottomright", legend = c("95% bootstrap CIs", "95% bootstrap prediction bounds"), 
       col = c("blue", "red"), lty = c(2, 2))

## Male Spring Body Mass ##

# Starting values
svTypical_mm <- vbStarts(weightActual ~ age, data = foo_m)

# VGBM model
vbTypical <- weightActual ~ Linf * ((1-exp(-K * (age - t0))) ^ 3)
fitTypical <- nls(vbTypical, data = foo_m, start = svTypical_mm)

# Visual and summary results of model fit
fitPlot(fitTypical, xlab = "Age", ylab = "Total Mass (kg)", main = "male spring mass")
overview(fitTypical)

# Residual standard error is also known as residual standard deviation in the regression model OR
# standard error of the estimate; not to be confused with the standard errors that are associated
# with parameter estimates
res <- fitTypical$m$resid() # residuals(difference between observable estimate and the observable sample mean)
attributes(res) <- NULL
degFred <- length(res) - length(fitTypical$m$getPars()) # degree of freedom: n - p
ResSumSq <- sum(res ^ 2) # residual sum of squared
SD <- sqrt(ResSumSq/ degFred) # standard deviation


# Assumption checking: homoscedasticity, normally distributed errors, adequate model fit to data,
# no influential outlying points
par(mfrow = c(1, 2))
plot(fitted(fitTypical), residuals(fitTypical), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
hist(residuals(fitTypical), xlab = "Residuals", main = "")
residPlot(fitTypical) # from FSA package

## Bootstrapping ##

bootTypical <- nlsBoot(fitTypical, niter = 1000) # bootstrap object
summary(bootTypical) # parameter estimates and confidence intervals
bootTypical$estiboot # paramater estimates and SE
confint(bootTypical, plot = TRUE) # confidence intervals + plot
SD_boot <- sqrt(sum(bootTypical$rse ^ 2)/length(bootTypical$rse)) # shoud be similar to SD of fitTypical

# The p-value for testing the hypothesis that H0 : K = 0.5 versus HA : K < 0.5
# if p-value is less than alpha = 0.05, then we can conclude there is enough evidence to conclude that
# K is less than 0.5, otherwise, we can not. 
htest(bootTypical,"K",0.5,"less")

# Illustrates correlations between pairs of paramaters similar to correlation matrix found
# in overview(fitTypical)
plot(bootTypical)

# Predicted length with corresponding confidence intervals and prediction intervals 
# constructed from bootstrapped results
par(mfrow = c(1, 1))
ages2plot <- 0:25
fitPlot(fitTypical, xlab = "Age", ylab = "Total Mass (kg)", xlim = range(ages2plot),
        main = "Male Spring Body Mass")
LCI <- UCI <- LPI <- UPI <- numeric(length(ages2plot))
ests <- bootTypical$coefboot
for (i in 1:length(ages2plot)) {
  pv <- ests[, "Linf"] * ((1 - exp(- ests[, "K"] * (ages2plot[i] - ests[, "t0"]))) ^ 3)
  UCI[i] <- quantile(pv, 0.975)
  LCI[i] <- quantile(pv, 0.025)
  UPI[i] <- quantile(pv + bootTypical$rse, 0.975)
  LPI[i] <- quantile(pv - bootTypical$rse, 0.025)
}
lines(UCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(LCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(UPI ~ ages2plot, type = "l", lty = 2, col = "red")
lines(LPI ~ ages2plot, type = "l", lty = 2, col = "red")
legend("topright", legend = c("95% bootstrap CIs", "95% bootstrap prediction bounds"), 
       col = c("blue", "red"), lty = c(2, 2))

## Female Spring Body Mass ##

# Starting values
svTypical_fm <- vbStarts(weightActual ~ age, data = foo_f)

# VGBM model
vbTypical <- weightActual ~ Linf * ((1-exp(-K * (age - t0))) ^ 3)
fitTypical <- nls(vbTypical, data = foo_f, start = svTypical_fm)

# Visual and summary results of model fit
fitPlot(fitTypical, xlab = "Age", ylab = "Total Mass (kg)", main = "female spring mass")
overview(fitTypical)

# Residual standard error is also known as residual standard deviation in the regression model OR
# standard error of the estimate; not to be confused with the standard errors that are associated
# with parameter estimates
res <- fitTypical$m$resid() # residuals(difference between observable estimate and the observable sample mean)
attributes(res) <- NULL
degFred <- length(res) - length(fitTypical$m$getPars()) # degree of freedom: n - p
ResSumSq <- sum(res ^ 2) # residual sum of squares
SD <- sqrt(ResSumSq/ degFred) # standard deviation

# Assumption checking: homoscedasticity, normally distributed errors, adequate model fit to data,
# no influential outlying points
par(mfrow = c(1, 2))
plot(fitted(fitTypical), residuals(fitTypical), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
hist(residuals(fitTypical), xlab = "Residuals", main = "")
residPlot(fitTypical) # from FSA package

## Bootstrapping ##

bootTypical <- nlsBoot(fitTypical, niter = 1000) # bootstrap object
summary(bootTypical) # parameter estimates and confidence intervals
bootTypical$estiboot # paramater estimates and SE
confint(bootTypical, plot = TRUE) # confidence intervals + plot
SD_boot <- sqrt(sum(bootTypical$rse ^ 2)/length(bootTypical$rse)) # shoud be similar to SD of fitTypical

# The p-value for testing the hypothesis that H0 : K = 0.5 versus HA : K < 0.5
# if p-value is less than alpha = 0.05, then we can conclude there is enough evidence to conclude that
# K is less than 0.5, otherwise, we can not. 
htest(bootTypical,"K",0.5,"less")

# Illustrates correlations between pairs of paramaters similar to correlation matrix found
# in overview(fitTypical)
plot(bootTypical)

# Predicted length with corresponding confidence intervals and prediction intervals 
# constructed from bootstrapped results
par(mfrow = c(1,1))
ages2plot <- 0:25
fitPlot(fitTypical, xlab = "Age", ylab = "Total Mass (kg)", xlim = range(ages2plot),
        main = "Female Spring Body Mass")
LCI <- UCI <- LPI <- UPI <- numeric(length(ages2plot))
ests <- bootTypical$coefboot
for (i in 1:length(ages2plot)) {
  pv <- ests[, "Linf"] * ((1 - exp(- ests[, "K"] * (ages2plot[i] - ests[, "t0"]))) ^ 3)
  UCI[i] <- quantile(pv, 0.975) # For every i (age; x-axis) there is ~ 1000 model estimates generated 
  LCI[i] <- quantile(pv, 0.025) # from bootstrapping from which the CIs are generated using quantile.
  UPI[i] <- quantile(pv + bootTypical$rse, 0.975)
  LPI[i] <- quantile(pv - bootTypical$rse, 0.025)
}
lines(UCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(LCI ~ ages2plot, type = "l", lty = 2, col = "blue")
lines(UPI ~ ages2plot, type = "l", lty = 2, col = "red")
lines(LPI ~ ages2plot, type = "l", lty = 2, col = "red")
legend("bottomright", legend = c("95% bootstrap CIs", "95% bootstrap prediction bounds"), 
       col = c("blue", "red"), lty = c(2, 2))

