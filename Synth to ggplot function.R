# Function to create synth plots as ggplot objects
# so = synth.res (output from `synth` function)
# dp = dataprep.res (output from `dataprep` function)

gg_synth <- function(dp = NULL, md = NULL, yr = 1999, post = FALSE) {
  require(dplyr)
  require(SPHSUgraphs)
  require(Synth)
  
  if ((is.null(dp)&&is.null(md))|(!is.null(dp)&&!is.null(md)))
    stop("Please enter either dataprep object or model")
  
  if (is.null(md)&&!is.null(dp)) {
  so <- synth(dp)
  synthC <- dp$Y0 %*% so$solution.w
  
  md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
    gather("Group", "Rate", -1)
  
  }
  
  if(post){
    xmax = NA
  } else {
    xmax = yr
    md <- md %>% filter(Year<yr)
  }
  
  mspe <- signif(pre_MSPE(md), 3)
  
  plot <-  ggplot(md, aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted") +
    scale_x_continuous(limits = c(NA, xmax)) +
    labs(subtitle = paste0("MSPE = ", mspe))
  
  print(plot)
  return(plot)
  
}

predvalues_synth <- function(dp, synth_outputs = TRUE, yr = 1999) {
  require(dplyr)
  require(Synth)
  
so <- synth(dp)
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

generatePlacebos <- function(data, predictors = NULL, special.predictors = NULL, time.optimize.ssr = 1990:1998, time.plot = 1985:2013) {

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
    time.predictors.prior = 1985:1998,
    dependent = "rate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = ccodes$Code[i],
    controls.identifier = ccodes$Code[-i],
    time.optimize.ssr = time.optimize.ssr,
    time.plot = time.plot
  )
    
    md <- predvalues_synth(dp, FALSE) 
    mspe <- pre_MSPE(md)
    
    gaps <- md %>%
      spread(Group, Rate) %>% 
        mutate(Country = ccodes$Country[i],
               Gap = Treated - Synthetic,
               pre_mspe = mspe)
    
    placebos <- bind_rows(placebos, gaps)
}
    return(placebos)
}
  
  
  # calculate MSPE ---------------------------------------------------------------------------------------------

pre_MSPE <- function (md){
  md %>%
    filter(Year < 1999) %>% 
    spread(Group, Rate) %>% 
    mutate(SPE = (Treated - Synthetic)**2) %>% 
    summarise(MSPE = mean(SPE)) %>% 
    pull()
}

