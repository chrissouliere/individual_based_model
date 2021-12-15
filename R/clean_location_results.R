## This script cleans IBM location results data to remove brackets produced
## in NetLogo

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
getPackages("ggplot2",  # Plotting
            "reshape2"  # AIC tab
)

# How many runs are you bringing in
run_num <- 100

# Create file names to read in
file_names <- sapply(seq_len(run_num), function(x) {
  paste("output/BearIBMResults/IBM/LocationResults/bear_IBMv22 Resultsv22/location_results_", x, ".csv", sep = "")
  })

# Read in file names and put into list                  
df <- lapply(seq_len(run_num), function(x) read.csv(file_names[x], header = TRUE, stringsAsFactors = FALSE))

# Removes brackets "[" from rows, transforms rows of each individual into a column
# and binds all individuals together for one run. Then binds all runs into a dataframe
unscramble_runs <- do.call(rbind, lapply(seq_len(run_num), function(z) {
  
  inn <- lapply(1:nrow(df[[z]]), function(u)  {

  # Removes brackets "[" for each row that has them
  # and separates the items of each row into 
  # individual elements, which are subsequently put into dataframe
  foo <- df[[z]]
  x <- foo[u, 3:length(foo)]
  run_number <- rep(foo[u, 1], length = 100) # 100 refers to number of unique individuals in simulation
  tick <- rep(foo[u, 2], length = 100) # ditto
  # replace "[" or ("|") "]" with "", then remove with noquote
  y <- cbind(run_number, tick,
             as.data.frame(do.call(cbind, lapply(1:length(x), function(i) {
               p <- list()
               p <- noquote(gsub("\\[|\\]", "", x[i]))
               p <- as.numeric(unlist(strsplit(p, split = " ")))
               return(p)
               }))))
  return(y)
  })

# Bind list within one run into a dataframe  
f <- do.call(rbind, inn)
colnames(f) <- c("run_number", "tick", "location-who", "location-xcor", "location-ycor", 
                 "location-spring-body-mass", "location-body-mass", "location-current-bci") 
return(f)
  
}))

# Write location results to file
write.csv(unscramble_runs, "output/BearIBMCleaned/bear_IBMv22_LocationCleanedRv22.csv")



