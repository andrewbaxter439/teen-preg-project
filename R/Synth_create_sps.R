source("R/Synth_functions.R")
source("R/Synth_data.R")
# Special predictors iterations ------------------------------------------------------------------------------


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


it_u18_gdp <- testSynthIterations(
  yrs = 1985:1998,
  pred = "rate",
  data = synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp),
  ccodes = u_18_ccodes %>% filter(!Country %in% exclude_u18_gdp),
  n = 4,
  predictors = "GDPperCap",
  time.optimise = 1985:1998
) %>% arrange(groups, mspe)


it_u18_all <- testSynthIterations(
  yrs = 1985:1998,
  pred = "rate",
  data = synthData_u18 %>% filter(!Country %in% exclude_u18_all),
  ccodes = u_18_ccodes %>% filter(!Country %in% exclude_u18_all),
  n = 4,
  predictors =c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
  time.optimise = 1985:1998
) %>% arrange(groups, mspe)

it_u18_gdp_1990 <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp_1990),
  ccodes = u_18_ccodes %>% filter(!Country %in% exclude_u18_gdp_1990),
  n = 4,
  predictors = "GDPperCap",
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)


it_u18_all_1990 <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = synthData_u18 %>% filter(!Country %in% exclude_u18_all_1990),
  ccodes = u_18_ccodes %>% filter(!Country %in% exclude_u18_all_1990),
  n = 4,
  predictors =c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)

it_u20_sp <-  testSynthIterations(
  yrs = 1990:1998,
  pred = "pRate",
  data = synthData_u20[,1:4],
  ccodes = u_20_ccodes,
  n = 4,
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)

it_u20_gdp <-  testSynthIterations(
  yrs = 1990:1998,
  pred = "pRate",
  data = synthData_u20[,c(1:4, 6)] %>% filter(!Country %in% exclude_u20_gdp),
  ccodes = u_20_ccodes %>% filter(!Country %in% exclude_u20_gdp),
  n = 4,
  predictors = "GDPperCap",
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)

it_u20_all <-  testSynthIterations(
  yrs = 1990:1998,
  pred = "pRate",
  data = synthData_u20[,-5] %>% filter(!Country %in% exclude_u20_all),
  ccodes = u_20_ccodes %>% filter(!Country %in% exclude_u20_all),
  n = 4,
  predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)


save(it_u18_rateSp, it_u18_gdp, it_u18_all, it_u20_sp, it_u20_gdp, it_u20_all, it_u18_all_1990, it_u18_gdp_1990, file = "Data/iterations.rdata")


# test iterations for best fits ------------------------------------------------------------------------------

it_u18_rateSp %>%  # iteration 220 best fit
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u18_gdp %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u18_all %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_sp %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_gdp %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_all %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

sp_u18_rateSp <- it_u18_rateSp$sPred[it_u18_rateSp$iteration == 220][[1]]
sp_u18_gdp <- it_u18_gdp$sPred[it_u18_gdp$iteration == 9][[1]]
sp_u18_all <- it_u18_all$sPred[it_u18_all$iteration == 9][[1]]
sp_u20_sp <- it_u20_sp$sPred[it_u20_sp$iteration == 18][[1]]
sp_u20_gdp <- it_u20_gdp$sPred[it_u20_gdp$iteration == 9][[1]]
sp_u20_all <- it_u20_all$sPred[it_u20_all$iteration == 9][[1]]

save(
  sp_u18_rateSp,
  sp_u18_gdp,
  sp_u18_all,
  sp_u20_sp,
  sp_u20_gdp,
  sp_u20_all,
  file = "Data/special_preds.rdata"
)

#  Country placebo data --------------------------------------------------------------------------------------

#u18_gdp and u18_all now use 1994 as start optimise year

pl_u18_rateSp  <- generatePlacebos(synthData_u18[, 1:4], special.predictors = sp_u18_rateSp, time.optimize.ssr = 1985:1998)
pl_u18_gdp     <- generatePlacebos(synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp), special.predictors = sp_u18_gdp, time.optimize.ssr = 1994:1998, predictors = "GDPperCap")
pl_u18_all     <- generatePlacebos(synthData_u18 %>% filter(!Country %in% exclude_u18_all) , special.predictors = sp_u18_all, time.optimize.ssr = 1994:1998, predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"))
pl_u20_sp      <- generatePlacebos(synthData_u20[, 1:4], special.predictors = sp_u20_sp , time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate")
pl_u20_gdp     <- generatePlacebos(synthData_u20[,c(1:4, 6)] %>% filter(!Country %in% exclude_u20_gdp), special.predictors = sp_u20_gdp, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate", predictors = "GDPperCap")
pl_u20_all     <- generatePlacebos(synthData_u20[,-5] %>% filter(!Country %in% exclude_u20_all), special.predictors = sp_u20_all, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate", predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"))

save(
pl_u18_rateSp,
pl_u18_gdp,
pl_u18_all,
pl_u20_sp,
pl_u20_gdp,
pl_u20_all,
file = "Data/placebos_country.rdata"
)



# Iterate through removing countries --------------------------------------


