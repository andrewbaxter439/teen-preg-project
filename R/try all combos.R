yrs <- 1985:1998
y1 <- 1990
y2 <- 1998
y1:y2
pred <- "pRate"
n = 5

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
    # sps[[i]] <- map(special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)], function (x) x[[2]])
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


# various tests ----------------------------------------------------------------------------------------------


U_18_best <- testSynthInterations(yrs = 1985:1998, pred = "rate", data = data.frame(synthData_U18 %>% filter(Year<2014)), n = 4)

U_18_best %>% 
  rowwise() %>% 
  mutate(yeargrps = factor(length(unique(str_split(pattern, ", ", simplify = TRUE)[1, ])))) %>% 
  ggplot(aes(iteration, mbpe, col = yeargrps)) + geom_point() + theme_minimal() + labs(title = "MSPE for each permutation of year predictors")

ggsave("graphs/Under 18 pregnancy rates - iterations of year permutations.png")

U_18_best %>% 
  arrange(mbpe)

testSynthInterations(n = 2)


# U-18 without NI/Scot controls ------------------------------------------------------------------------------

it_U18_noScotNI <- testSynthInterations(yrs = 1985:1998,
                                        pred = "rate", 
                                        data = data.frame(synthData_U18 %>% filter(Year<2014)), 
                                        ccodes = u_18_ccodes %>% filter(Country!="Scotland",
                                                                        Country!="Northern Ireland"), n = 5,
                                        Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)

it_U18_noScotNI %>% 
  mutate(groups = factor(groups)) %>% 
  ggplot(aes(iteration, mbpe, col = groups)) + 
  geom_point() +
  theme_minimal()

it_U18_noScotNI %>% 
  filter(groups == 4) %>% 
  arrange(mbpe) %>% 
  View()

sp_U18_noScotNI <- it_U18_noScotNI$sPred[it_U18_noScotNI$iteration==45][[1]]


