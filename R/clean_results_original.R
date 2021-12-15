## This script cleans NetLogo Behavior Space data into long format

# column names, adjust as needed
c.names <- c("run_number", "results_on", "step", "ticks", "bear_who", "bear_age", "bear_sex", 
             "bear_age_class", "bear_spring_body_mass", "bear_body_mass", "bear_survival_rates", "bear_survive",         
             "bear_road_freq", "bear_road_dens", "bear_total_dist","bear_current_bci", "bear_cub", "bear_yearling")
# adjust file name 
df <- read.csv("output/BearIBMResults/R/bear_IBMv13_ResultsRv11.csv", 
               header = TRUE, stringsAsFactors = FALSE, col.names = c.names)


unscramble_runs <- lapply(1:nrow(df), function(u)  {
  # Removes brackets for each column that has them
  # and separates the items of each column into 
  # individual elements.
  
  foo <- df
  x <- foo[u, 5:length(foo)]
  # replace "[" or ("|") "]" with "", then remove with noquote
  y <- lapply(1:length(x), function(i) {
    p <- list()
    p <- noquote(gsub("\\[|\\]", "", x[i]))
    p <- unlist(strsplit(p, split = " "))
    return(p)
  })
  
})

reorder_runs <- function() {
  # Takes a nested list, test whether the elements
  # within the lower level of the list are numeric or
  # character, and replaces them according to what they are.
  # Followed by assembling the lower level list into a 
  # dataframe, and the binding the dataframes together.
  
  y <- unscramble_runs
  foo <- df
  
  # test numeric on each elements of nested list, and then return 
  # any of the vector of elements considered numeric (elements should all 
  # be of the same class, that why use any).
  my_fun <- function(x) {
    return(any(!is.na(as.numeric(x))))
  }
  
  # apply test numeric to each vector of elements in list and return
  # logical vector of length 1
  r1 <- suppressWarnings( # warnings from using as.numeric on letters
    lapply(unscramble_runs, function(x) {
      sapply(x, my_fun)
    })
  )
  
  # length of of lower level list, should be the same for every top-level list
  r2 <- r1[[1]]
  
  # loops through the top-level list (runs), then the vector elements
  # within each lower-level list, by cheching if the data should be
  # numeric or character
  for (i in 1:length(r1)) {
    for (j in 1:length(r2)) {
      if (r1[[i]][j]) {
        y[[i]][[j]] <- as.numeric(y[[i]][[j]])
      } else {
        y[[i]][[j]] <- as.character(y[[i]][[j]])
      }
    }
  }
  
  # for each top-level list, reorganize data into dataframe
  # by binding the first for rows of df (foo) with the manipulated rows
  f <- lapply(1:length(r1), function (z) {
    a <- foo[z, 1:4] # first four columns
    len <- unique(sapply(y[[z]], length))# length of runs, should be the exact same
    a <- cbind(a[rep(seq_len(nrow(a)), each = len), ], y[[z]])
    colnames(a)[5:length(foo)] <- colnames(foo[5:length(foo)])
    return(a)
  })
  
  # bind list of dataframes in single datafame
  f2 <- do.call(rbind, f)
  return(f2)
}

sim_r<- reorder_runs()
write.csv(sim_r, "output/BearIBMCleaned/bear_IBMv13_ResultsCleanedRv11.csv")

