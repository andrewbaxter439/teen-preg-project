
# First function - don't use parallel processing --------------------------

testSynthIterations_single <- function(yrs = 1990:1998,  # pre-intervention years
                                       pred = "pRate",  # predictor to iterate over grouping
                                       data = synthData_u20,
                                       ccodes = u_20_ccodes,  # control country identifiers
                                       n = 5,  # Number of groups to try out
                                       predictors = NULL,
                                       time.optimise = 1990:1998,
                                       ...) {
  
  require(gtools)
  require(dplyr)
  require(Synth)
  require(stringr)
  
  data <- data.frame(data)
  x <- 1:n
  
  # These steps work out all the different combinations of years in up to n groupings for the optimisation years specified
  combos <- combinations(length(x), length(yrs), x, repeats.allowed = TRUE)
  
  i <- 1
  
  for (i in 1:nrow(combos)){
    combos[i,] <- as.numeric(as.factor(as.character(combos[i,])))
  }
  
  combos <- unique(combos)
  
  
  # Set up empty vectors for parts to extract
  mspes <- c()
  sps <- c()
  
  
  # looping (with counter)
  i <- 1
  
  for (i in 1:nrow(combos)) {
    print(paste0("iteration ", i, "/", nrow(combos)))
    
    
    # puts together a list of the 1-5 groupings for the predictor variable iteration i
    special.preds <- list(
      a = list(pred, yrs = yrs[combos[i,] == 1], "mean"),
      b = list(pred, yrs = yrs[combos[i,] == 2], "mean"),
      c = list(pred, yrs = yrs[combos[i,] == 3], "mean"),
      d = list(pred, yrs = yrs[combos[i,] == 4], "mean"),
      e = list(pred, yrs = yrs[combos[i,] == 5], "mean")
    )
    
    
    # dataprep for iteration i
    dp <-   dataprep(
      foo = data.frame(data %>% filter(Year >= yrs[1])),
      predictors = predictors,
      special.predictors = special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)],  # filters the 5 groups for only those used
      time.predictors.prior = yrs,
      dependent = pred,
      unit.variable = "Code",
      unit.names.variable = "Country",
      time.variable = "Year",
      treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
      controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
      time.optimize.ssr = time.optimise,
      time.plot = yrs
    )
    
    
    # from Abadie - steps to get synthetic control, create 'md' dataframe of observed/control values across years
    so <- synth(dp, ...)
    synthC <- dp$Y0 %*% so$solution.w
    
    md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>%
      gather("Group", "Rate", -1)
    
    
    # pre-intervention mspe for iteration i
    mspes[i] <- so$loss.v
    
    # list of special predictors used in iteration i - so this can be extracted later and used
    sps[[i]] <-special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)]
    
    
    
  }
  
  # combines all iterations into a single table
  
  tb <- tibble(iteration = 1:nrow(combos),
               pattern = NA,
               mspe = mspes,
               sPred = sps,
               groups = NA)
  
  # pattern column created to show groupings of years
  i <- 1
  for (i in 1:nrow(combos)){
    tb$pattern[i] <- str_flatten(combos[i,], collapse = ", ")
    tb$groups[i] <- length(tb$sPred[[i]])
  }
  
  
  return(tb)
  
}


# second function - do with parallel processing - trickier to foll --------

testSynthIterations <- function(yrs = 1990:1998, 
                                pred = "pRate",
                                data = synthData_u20, 
                                ccodes = u_20_ccodes,
                                n = 5,
                                predictors = NULL,
                                time.optimise = 1990:1998,
                                dependent = pred,
                                ...) {
  
  require(gtools)
  require(dplyr)
  require(Synth)
  require(stringr)
  require(foreach)
  require(doParallel)
  require(tcltk)
  
  data <- data.frame(data)
  x <- 1:n
  
  combos <- combinations(length(x), length(yrs), x, repeats.allowed = TRUE)
  
  i <- 1
  
  for (i in 1:nrow(combos)){
    combos[i,] <- as.numeric(as.factor(as.character(combos[i,])))
  }
  
  combos <- unique(combos)
  
  mspes <- c()
  sps <- c()
  w <- c()
  v <- c()
  
  # This part sets up the multicore - using a subet of available cores
  
  cores <- detectCores() - 1
  
  if (cores>8) {cores <- 8}
  
  cl <- makeCluster(cores)
  
  registerDoParallel(cl)
  
  # key difference here, the `foreach` fuction does parallel, so needs structured differently
  
  i <- 1
  tb <- tibble()
  
  tb <- foreach (i=1:nrow(combos),
                 .packages = c("Synth", "dplyr", "purrr", "tidyr", "tcltk", "parallel"),
                 .combine = bind_rows) %dopar% {
                   
                   # display a progress bar for each processor (messy, but only way of keeping track!)
                   
                   if(!exists("pb")) pb <- tkProgressBar("Iterations", min = 1, max = nrow(combos))
                   setTkProgressBar(pb, i)
                   
                   special.preds <- list(
                     a = list(pred, yrs = yrs[combos[i,] == 1], "mean"),
                     b = list(pred, yrs = yrs[combos[i,] == 2], "mean"),
                     c = list(pred, yrs = yrs[combos[i,] == 3], "mean"),
                     d = list(pred, yrs = yrs[combos[i,] == 4], "mean"),
                     e = list(pred, yrs = yrs[combos[i,] == 5], "mean")
                   )
                   
                   dp <- dataprep(
                     foo = data.frame(data %>% filter(Year >= yrs[1])),
                     predictors = predictors,
                     special.predictors = special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)],
                     time.predictors.prior = yrs[1]:1998,
                     dependent = dependent,
                     unit.variable = "Code",
                     unit.names.variable = "Country",
                     time.variable = "Year",
                     treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
                     controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
                     time.optimize.ssr = time.optimise,
                     time.plot = yrs[1]:2013
                   )
                   
                   
                   
                   so <- synth(dp, ...)
                   st <- synth.tab(so, dp)
                   
                   synthC <- dp$Y0 %*% so$solution.w
                   
                   md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
                     gather("Group", "Rate", -1)
                   
                   mspe <- so$loss.v[1,1]
                   
                   # Extra element in this function - extract the weightings for each country and for each year grouping
                   w <- st$tab.w
                   v <- tibble(Pred = row.names(st$tab.v), v_weight = as.numeric(st$tab.v))
                   
                   sps <- special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)]
                   
                   
                   # Also extract a series of gaps
                   gaps <- md %>%
                     spread(Group, Rate) %>% 
                     mutate(iteration = i,
                            groups = length(sps),
                            Gap = Treated - Synthetic)
                   
                   
                   # close progress bat windows - doesn't always work!
                   if(i>=(nrow(combos)-8)) {close(pb)}
                   
                   tibble(iteration = i, 
                          pattern = NA,  
                          mspe, 
                          sPred = list(sps), 
                          w_weights = list(w),  # cool tip - use `list` to assign an entire dataframe as a variable in another dataframe!
                          v_weights = list(v),
                          gaps = list(gaps),
                          groups = NA)
                   
                 }
  
  
  i <- 1
  for (i in 1:nrow(combos)){
    tb$pattern[i] <- str_flatten(combos[i,], collapse = ", ")
    tb$groups[i] <- length(tb$sPred[[i]])
  }
  
  
  return(tb)
  registerDoSEQ()
}


# running the function - remember to assign the output to a dataframe!

it_u18_rateSp <- testSynthIterations(
  yrs = 1985:1998,
  pred = "rate",
  data = synthData_u18[,1:4],
  ccodes = u_18_ccodes,
  n = 4,
  predictors = NULL,
  time.optimise = 1985:1998
) %>%
  arrange(groups, mspe)


