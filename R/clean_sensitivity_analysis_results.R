## This script cleans sensitivity analysis results data to remove brackets produced
## in NetLogo

# Read in sensitivity results file
sensi_results <- read.csv(file.path(getwd(), "sensitivity_results_1_250.csv"))

unscramble_runs <- function() {
  
  inn <- lapply(1:nrow(sensi_results), function(u)  {
  
    # Removes brackets "[" for each row that has them
    # and separates the items of each row into 
    # individual elements, which are subsequently put into dataframe
    foo <- sensi_results
    res <- foo[u, 9:length(foo)] # rows with brackets
    par <- apply(foo[u, 1:8], 2, function(x) rep(x, length = foo[u, "initial.num.bears"])) # parameter rows duplicated
    par <- cbind(data.frame(run.number = rep(u, length = foo[u, "initial.num.bears"])), par) # bind simulation run number 
    # replace "[" or ("|") "]" with "", then remove with noquote
    y <- cbind(par, # bind parameter dataframe to dataframe with brackets removed (output variables)
               as.data.frame(do.call(cbind, 
                                     lapply(1:length(res), function(i) {
                                       p <- list()
                                       p <- noquote(gsub("\\[|\\]", "", res[i]))
                                       p <- unlist(strsplit(p, split = " "))
                                       return(p)
                                       }))))
    
    y[, c(10, 11, 14:16, 18:21)] <- lapply(y[, c(10, 11, 14:16, 18:21)], as.numeric) # adjust class
    y[, c(12, 13, 17, 22, 23)] <- lapply(y[, c(12, 13, 17, 22, 23)], factor) # adjust class
    colnames(y) <- c(colnames(par), colnames(res))
    
    return(y)
    })
  
  f <- do.call(rbind, inn)
  return(f)
}

# Final sensitivity results
sensi_results_cleaned <- unscramble_runs()

# Write clean results to file
write.csv(sensi_results_cleaned, "sensitivity_results_cleaned_1_250.csv")





