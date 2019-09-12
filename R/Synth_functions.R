# Function to create synth plots as ggplot objects
# so = synth.res (output from `synth` function)
# dp = dataprep.res (output from `dataprep` function)

gg_synth <- function(dp = NULL, md = NULL, yr = 1999, post = FALSE, mspe = NULL, mspeOptim = FALSE) {
  require(dplyr)
  require(SPHSUgraphs)
  require(Synth)
  
  
  if ((is.null(dp)&&is.null(md))|(!is.null(dp)&&!is.null(md)))
    stop("Please enter either dataprep object or model")
  
  if (is.null(md)&&!is.null(dp)) {
    
  agegrp <- gsub("^.*(\\d{2}).*$","\\1", deparse(substitute(dp)))
  so <- synth(dp)
  synthC <- dp$Y0 %*% so$solution.w
  
  md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
    gather("Group", "Rate", -1)
  if (is.null(mspe) & mspeOptim){
  mspe <- so$loss.v[1]
  }
  } else {
    
  agegrp <- gsub("^.*(\\d{2}).*$","\\1", deparse(substitute(md)))
  }
  
  if(post){
    xmax = NA
  } else {
    xmax = yr
    md <- md %>% filter(Year<yr)
  }
  
  if(is.null(mspe) & mspeOptim) {
    stop("Please enter mspe for the synth output, or change mspeOptim to 'FALSE' to calculate from whole pre-intervention period.")
  }
  
  if(!is.null(mspe)){
  mspe <- signif(mspe, 3)
  } else {
  mspe <- pre_MSPE(md)
  }
  
  
  plot <-  ggplot(md, aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 2) +
    theme_sphsu_light() +
    ylab(paste0("Under-", agegrp, " rate (per 1,000 women)")) +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted") +
    scale_x_continuous(limits = c(NA, xmax)) +
    labs(subtitle = paste0("Pre-intervention MSPE = ", mspe))

  return(plot)
  
}

predvalues_synth <- function(dp, synth_outputs = TRUE, yr = 1999, ...) {
  require(dplyr)
  require(Synth)
  
so <- synth(dp, ...)
  synthC <- dp$Y0 %*% so$solution.w
  
  if (synth_outputs){
  so_name <- gsub("^[a-z]+_(.*$)", "so_\\1", deparse(substitute(dp)))
  st_name <- gsub("^[a-z]+_(.*$)", "st_\\1", deparse(substitute(dp)))
  print(paste("outputting", so_name, "and", st_name))
  st <- synth.tab(so, dp)
  
  assign(so_name, so, envir = .GlobalEnv)
  assign(st_name, st, envir = .GlobalEnv)
  }
  
  df <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
    gather("Group", "Rate", -1)
  
return(df)
  
}

printCoefficients <- function(md = NULL, model = NULL){
  
  if ((is.null(model)&&is.null(md))|(!is.null(model)&&!is.null(md)))
    stop("Please enter either lm model or data set ('md = ')")
  
  if (is.null(model)) {
    model <- md %>% filter(Year > 1999) %>% 
      mutate(Time = Year-1998) %>% 
      lm(Rate ~ Time + Group + Time*Group, data = .)
  }
  
  print("Coefficients:")
  coefs <- as_tibble(summary(model)$coefficients) %>%
    mutate(Coefficient = rownames(summary(model)$coefficients),
           Estimate = as.numeric(Estimate),
           SE = as.numeric(`Std. Error`),
           P = as.numeric(`Pr(>|t|)`)) %>% 
    select(Coefficient, Estimate, SE, P)
  
    print(coefs)

  print("Confidence intervals:")
  coefs %>% 
    transmute(Coefficient = Coefficient,
              Estimate = Estimate,
              LowerCI = Estimate - 1.96*SE,
              UpperCI = Estimate + 1.96*SE) %>% 
    print()
}

generatePlacebos <- function(data,
                             predictors = NULL,
                             special.predictors = NULL, 
                             time.optimize.ssr = 1990:1998,
                             time.plot = 1985:2013, 
                             dependent = "rate", 
                             ...) {

  data <- data.frame(data %>% 
                       filter(Country != "England and Wales"))
  
  ccodes <- data %>% 
    select(Code, Country) %>% 
    unique()
  
  placebos <- data.frame()
  
  for(i in 1:nrow(ccodes)){
  
    dp <- dataprep(
    data,
    predictors = predictors,
    predictors.op = "mean",
    special.predictors = special.predictors,
    time.predictors.prior = time.plot[1]:1998,
    dependent = dependent,
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = ccodes$Code[i],
    controls.identifier = ccodes$Code[-i],
    time.optimize.ssr = time.optimize.ssr,
    time.plot = time.plot
  )
    
    # md <- predvalues_synth(dp, FALSE, ...) 
    # mspe <- pre_MSPE(md)
    so <- synth(dp, ...)
    synthC <- dp$Y0 %*% so$solution.w
    
    md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
      gather("Group", "Rate", -1)
    
    mspe <- so$loss.v
    
    gaps <- md %>%
      spread(Group, Rate) %>% 
        mutate(Country = ccodes$Country[i],
               Gap = Treated - Synthetic,
               pre_mspe = mspe)
    
    placebos <- bind_rows(placebos, gaps)
}
    return(placebos)
}

pre_MSPE <- function (md){
  md %>%
    filter(Year < 1999) %>% 
    spread(Group, Rate) %>% 
    mutate(SPE = (Treated - Synthetic)**2) %>% 
    summarise(MSPE = mean(SPE)) %>% 
    pull() %>% 
    signif(3)
}

sPredText <- function(dp) {
  paste("Prediction periods:",
  map(dp$tag$special.predictors, pluck(2)) %>% 
    map(function(x) paste0(min(x), "-", max(x))) %>% 
    str_flatten(collapse = "; ")
  )
}

gg_gaps <- function(md, pl, dp = NULL, mspe_limit = NULL, title = FALSE, subtitle = FALSE) {
  
  
  if(is.null(mspe_limit)) {
    mspe_limit <- pre_MSPE(md)
  }
  
  p <- md %>% 
    spread(Group, Rate) %>% 
    mutate(Gap = Treated - Synthetic) %>% 
    ggplot(aes(Year, Gap)) +
    geom_segment(x = min(md$Year), xend = 2013, y = 0, yend = 0) +
    geom_line(data = pl %>% filter(pre_mspe < 5*mspe_limit), aes(group = Country), col = "grey") +
    geom_line(col = sphsu_cols("Thistle", names = FALSE), size = 2) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 1998.5, linetype = "dotted") +
    ylab("Gap = Treated - Synthetic Control")
  
  if(title){
    p <- p + labs(title = sPredText(dp_u20_sp))
  }
  
  if(subtitle) {
    p <- p + labs(subtitle = paste0("MSPE over optimisation period: ", signif(mspe_limit_u20_sp, 3), " (controls <5*MSPE)"))
  }
  
  p
  
}


interpolateAb <- function(country, data = synth_data_plus_ab){
  cdata_u20 <- data %>% 
    filter(Country == country, agegrp == "Under 20")
  
  meanProp <- cdata_u20 %>% mutate(prop = Abortions/sumBirths) %>% 
    filter(prop < mean(prop, na.rm = TRUE) + 2*sd(prop, na.rm = TRUE)) %>% 
    summarise(mean = mean(prop)) %>% pull()
  
  new_dat <- cdata_u20 %>% 
    filter(is.na(Abortions)) %>%
    mutate(Abortions = sumBirths * meanProp) %>% 
    full_join(cdata_u20 %>% filter(!is.na(Abortions)), by = c("Code", "Country", "Year", "Abortions", "agegrp", "sumBirths", "sumPops", "rate", "GDPperCap", "MF_ratio", "MobilePhones", "UrbanPop")) %>% 
    mutate(totalPregs = Abortions + sumBirths)
  
  data %>% 
    filter(Country == country, agegrp != "Under 20") %>% 
    bind_rows(new_dat,.) %>% 
    return()
  
}

testSynthIterations <- function(yrs = 1990:1998, 
                                pred = "pRate",
                                data = synthData_U20, 
                                ccodes = u_20_codes,
                                n = 5,
                                predictors = NULL,
                                time.optimise = 1990:1998,
                                ...) {
  
  require(gtools)
  require(dplyr)
  require(Synth)
  require(stringr)
  
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
  
  i <- 1
  
  for (i in 1:nrow(combos)) {
    print(paste0("iteration ", i, "/", nrow(combos)))
    
    special.preds <- list(
      a = list(pred, yrs = yrs[combos[i,] == 1], "mean"),
      b = list(pred, yrs = yrs[combos[i,] == 2], "mean"),
      c = list(pred, yrs = yrs[combos[i,] == 3], "mean"),
      d = list(pred, yrs = yrs[combos[i,] == 4], "mean"),
      e = list(pred, yrs = yrs[combos[i,] == 5], "mean")
    )
    
    dp <-   dataprep(
      foo = data.frame(data %>% filter(Year >= yrs[1])),
      predictors = predictors,
      special.predictors = special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)],
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
    
    # md <- predvalues_synth(dp, synth_outputs = FALSE, ...)
    
    so <- synth(dp, ...)
    synthC <- dp$Y0 %*% so$solution.w
    
    md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
      gather("Group", "Rate", -1)
    
    mspes[i] <- so$loss.v

    sps[[i]] <-special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)]
    
    
    
  }
  
  
  tb <- tibble(iteration = 1:nrow(combos),
               pattern = NA,
               mspe = mspes,
               sPred = sps,
               groups = NA)
  
  i <- 1
  for (i in 1:nrow(combos)){
    tb$pattern[i] <- str_flatten(combos[i,], collapse = ", ")  
    tb$groups[i] <- length(tb$sPred[[i]])
  }
  
  
  return(tb)
  
}

# Initial dataprep and synth ----------------------------------------

data <- synthData_u18
ccodes <- u_18_ccodes
grp <- "u_18_gdp"

synthPrep <- function(data, 
                      grp, 
                      dependent,
                      time.predictors.prior = NULL,
                      time.optimise.ssr = NULL,
                      time.plot = NULL,
                      predictors = NULL,
                      special.predictors = NULL,
                      ...){

  start <- min(data$Year)  
  

ccodes <- data %>% 
  select(Code, Country) %>% 
  unique()
  
  if(is.null(time.predictors.prior)){
    time.predictors.prior <- start:1998
  }
  
  if(is.null(time.optimise.ssr)){
    time.optimise.ssr <- start:1998
  }
  
  if(is.null(time.plot)){
    time.plot <- start:2013
  }
  
  dp <- dataprep(
    foo = data,
    dependent = dependent,
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    time.predictors.prior = time.predictors.prior,
    time.optimize.ssr = time.optimise.ssr,
    time.plot = time.plot,
    treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
    controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
    predictors = predictors,
    special.predictors = special.predictors,
    ...
  )
  
  so <- synth(dp, ...)
  st <- synth.tab(so, dp)
  
  synthC <- dp$Y0 %*% so$solution.w
  mspe_lim <- so$loss.v[1]
  

md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
  gather("Group", "Rate", -1)
  

  assign(paste0("dp_", grp), dp, envir = .GlobalEnv)
  assign(paste0("so_", grp), so, envir = .GlobalEnv)
  assign(paste0("st_", grp), st, envir = .GlobalEnv)
  assign(paste0("md_", grp), md, envir = .GlobalEnv)
  assign(paste0("mspe_limit_", grp), mspe_lim, envir = .GlobalEnv)
  
}

# synthPrep(synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp),
#           "u18_gdp",
#           dependent = "rate",
#           predictors = "GDPperCap",
#           special.predictors = sp_u18_gdp)