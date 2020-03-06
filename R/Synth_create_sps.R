source("R/Synth_functions.R")
load('Data/synth_data_c.rdata')
# source("R/Synth_data.R")
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


it_u18_sp_1990 <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = synthData_u18[,1:4],
  ccodes = u_18_ccodes,
  n = 4,
  predictors = NULL,
  time.optimise = 1990:1998
) %>%
  arrange(groups, mspe)

it_u18_filt <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = synthData_u18_filt[,1:4],
  ccodes = u_18_ccodes_f,
  n = 4,
  predictors = NULL,
  time.optimise = 1990:1998
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


it_u18_gdp_1990 <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp_1990),
  ccodes = u_18_ccodes %>% filter(!Country %in% exclude_u18_gdp_1990),
  n = 4,
  predictors = "GDPperCap",
  time.optimise = 1990:1998
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

it_u20_filt <-  testSynthIterations(
  yrs = 1990:1998,
  pred = "pRate",
  data = synthData_u20_filt[,1:4],
  ccodes = u_20_ccodes_f,
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

it_u18_ns <- testSynthIterations(
  yrs = 1990:1998,
  pred = "rate",
  data = sd_noScot,
  ccodes = u_20_ccodes_f %>% filter(Country!="Scotland"),
  n=5,
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)

it_u20_ns <- testSynthIterations(
  yrs = 1990:1998,
  pred = "pRate",
  data = sd_noScot,
  ccodes = u_20_ccodes_f %>% filter(Country!="Scotland"),
  n=5,
  time.optimise = 1990:1998
) %>% arrange(groups, mspe)

save(it_u18_rateSp, it_u18_gdp, it_u18_all, it_u20_sp, it_u20_gdp, it_u20_all, it_u18_sp_1990, it_u18_all_1990, it_u18_gdp_1990, it_u18_ns, it_u20_ns, file = "Data/iterations.rdata")


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

it_u18_sp_1990 %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u18_gdp_1990 %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u18_all_1990 %>% 
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
sp_u20_gdp <- it_u20_gdp$sPred[it_u20_gdp$iteration == 18][[1]]
sp_u20_all <- it_u20_all$sPred[it_u20_all$iteration == 18][[1]]
sp_u18_sp_1990 <- it_u18_sp_1990$sPred[it_u18_sp_1990$iteration == 81][[1]]
sp_u18_gdp_1990 <- it_u18_gdp_1990$sPred[it_u18_gdp_1990$iteration == 3][[1]]
sp_u18_all_1990 <- it_u18_all_1990$sPred[it_u18_all_1990$iteration == 3][[1]]

save(
  sp_u18_rateSp,
  sp_u18_gdp,
  sp_u18_all,
  sp_u20_sp,
  sp_u20_gdp,
  sp_u20_all,
  sp_u18_sp_1990,
  sp_u18_gdp_1990,
  sp_u18_all_1990,
  file = "Data/special_preds.rdata"
)

it_u18_filt %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)

it_u20_filt %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)

it_u18_ns %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

it_u20_ns %>% 
  group_by(groups) %>% 
  top_n(3, -mspe)  

sp_u18_filt <- it_u18_filt$sPred[it_u18_filt$iteration == 26][[1]]
sp_u20_filt <- it_u20_filt$sPred[it_u20_filt$iteration == 69][[1]]
sp_u18_ns <- it_u18_ns$sPred[it_u18_filt$iteration == 26][[1]]
sp_u20_ns <- it_u20_ns$sPred[it_u20_filt$iteration == 69][[1]]

save(
  it_u18_filt,
  it_u20_filt,
  sp_u18_filt,
  sp_u20_filt,
  it_u18_ns,
  it_u20_ns,
  sp_u18_ns,
  sp_u20_ns,
  file = "Data/filtered_itsp.rdata"
)

#  Country placebo data --------------------------------------------------------------------------------------

#u18_gdp and u18_all now use 1990 as starting year and sps derived from no-predictor data

pl_u18_rateSp  <- generatePlacebos(synthData_u18[, 1:4], special.predictors = sp_u18_rateSp, time.optimize.ssr = 1985:1998)
pl_u18_sp_1990  <- generatePlacebos(synthData_u18[, 1:4], special.predictors = sp_u18_sp_1990, time.optimize.ssr = 1990:1998, time.plot = 1990:2013,)
pl_u18_gdp     <- generatePlacebos(synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp_1990), special.predictors = sp_u18_sp_1990, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, predictors = "GDPperCap")
# pl_u18_all     <- generatePlacebos(synthData_u18 %>% filter(!Country %in% exclude_u18_all_1990) , special.predictors = ssp_u18_sp_1990,time.optimize.ssr = 1990:1998, time.plot = 1990:2013, predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"))
pl_u20_sp      <- generatePlacebos(synthData_u20[, 1:4], special.predictors = sp_u20_sp, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate")
pl_u20_gdp     <- generatePlacebos(synthData_u20[,c(1:4, 6)] %>% filter(!Country %in% exclude_u20_gdp), special.predictors = sp_u20_gdp, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate", predictors = "GDPperCap")
# pl_u20_all     <- generatePlacebos(synthData_u20[,-5] %>% filter(!Country %in% exclude_u20_all), special.predictors = sp_u20_all, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate", predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"))

save(
pl_u18_rateSp,
pl_u18_gdp,
pl_u18_all,
pl_u20_sp,
pl_u20_gdp,
pl_u20_all,
file = "Data/placebos_country.rdata"
)

pl_u18_filt  <- generatePlacebos(synthData_u18_filt[, 1:4], special.predictors = sp_u18_filt, time.optimize.ssr = 1990:1998, time.plot = 1990:2013,)
pl_u20_filt  <- generatePlacebos(synthData_u20_filt[, 1:4], special.predictors = sp_u20_filt, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate")


pl_u18_ns  <- generatePlacebos(sd_noScot, special.predictors = sp_u18_filt, time.optimize.ssr = 1990:1998, time.plot = 1990:2013,)
pl_u20_ns  <- generatePlacebos(sd_noScot, special.predictors = sp_u20_filt, time.optimize.ssr = 1990:1998, time.plot = 1990:2013, dependent = "pRate")

pl_u18_all     <- generatePlacebos(sd_noScot,
                                   special.predictors = sp_u18_filt %>% 
                                     append(sp_u20_gdp) %>% 
                                     append(sp_u20_edu) %>% 
                                     append(sp_u20_mob) %>% 
                                     append(sp_u20_urb),
                                   time.optimize.ssr = 1990:1998, 
                                   time.plot = 1990:2013)

pl_u20_all     <- generatePlacebos(sd_noScot,
                                   special.predictors = sp_u20_filt %>% 
                                     append(sp_u20_gdp) %>% 
                                     append(sp_u20_edu) %>% 
                                     append(sp_u20_mob) %>% 
                                     append(sp_u20_urb),
                                   time.optimize.ssr = 1990:1998, 
                                   time.plot = 1990:2013,
                                   dependent = "pRate")

save(
  pl_u18_filt,
  pl_u20_filt,
  pl_u18_ns,
  pl_u20_ns,
  pl_u18_all,
  pl_u20_all,
  file = "Data/placebo_country_b.rdata"
)
# Iterate through removing countries --------------------------------------



iterateCountries <- function(data = synthData_u18[,1:4], ccodes = u_18_ccodes, n=4, start = 1985, pred = "rate", ...){
cc <- ccodes

it_c <- list()

for (i in 1:(nrow(ccodes)-2)) {
  
  
  it <- testSynthIterations(
    yrs = start:1998,
    pred = pred,
    data = data,
    ccodes = cc,
    n = n,
    predictors = NULL,
    time.optimise = start:1998,
    ...
  ) %>%
    arrange(groups, mspe)
  
 it_c[[i]] <- it %>% 
    select(iteration, w_weights, mspe) %>% 
    unnest(w_weights) %>% 
    top_n(1, -mspe) %>% 
    ungroup() %>% 
    top_n(1,w.weights) %>% 
    mutate(weight = paste0(w.weights*100, "%"),
           label = paste0(unit.names, ", ", weight, ", ", "MSPE = ", round(mspe, 3))) %>% 
    left_join(it %>%  select(iteration, gaps), by = "iteration")
  
  cc <- cc %>% filter(Code != it_c[[i]]$unit.numbers)
  
}

return(it_c)
}


itco_u18_sp <- iterateCountries(Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)

itco_u18_sp_1990 <- iterateCountries(data = synthData_u18[,1:4], 
                                     ccodes = u_18_ccodes,
                                     start = 1990,
                                     n = 4,
                                     Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)

itco_u20_sp <- iterateCountries(data = synthData_u20[,1:4], ccodes = u_20_ccodes, pred = "pRate", start = 1990,
                                n=4,
                                Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6)

save(itco_u18_sp, itco_u18_sp_1990, itco_u20_sp, file = "Data/iterating_rm_countries.rdata")


# filtered iterating countries --------------------------------------------

itco_u18_filt <- iterateCountries(synthData_u18_filt, u_18_ccodes_f, start = 1990, pred = "rate")
itco_u20_filt <- iterateCountries(synthData_u20_filt, u_20_ccodes_f, start = 1990, pred = "pRate")

save(
  itco_u18_filt,
  itco_u20_filt,
  file = "Data/iterating_rm_countries_filt.rdata"
)
