# Function to create synth plots as ggplot objects
# so = synth.res (output from `synth` function)
# dp = dataprep.res (output from `dataprep` function)

gg_synth <- function(dp = NULL, md = NULL, yr = 1999) {
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
  
  plot <-  ggplot(md, aes(Year, Rate, col = Group, linetype = Group)) +
    geom_line(size = 1.5) +
    theme_sphsu_light() +
    ylab("Under-20 pregnancy rate (per 1,000 women)") +
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    scale_linetype_manual(name = "Data", values = c("Synthetic" = "dashed", "Treated" = "solid")) +
    scale_colour_manual(name = "Data", values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
    geom_vline(xintercept = 1998.5, linetype = "dotted")
  
  print(plot)
  return(plot)
  
}

predvalues_synth <- function(dp, yr = 1999) {
  require(dplyr)
  require(SPHSUgraphs)
  require(Synth)
  
so <- synth(dp)
  synthC <- dp$Y0 %*% so$solution.w
  
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
