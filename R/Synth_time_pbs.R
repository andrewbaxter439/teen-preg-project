load('Data/synth_data.rdata') # outputted from 'Synth_data.R'
load('Data/special_preds.rdata')  # outputted from 'Synth_create_sps.R'
source('R/Synth_functions.R')

# Model 1 - u18 Sp -------------------------------------------------------------------------------------------

super_md_u18_sp <- tibble()

for (y in 1995:2005){
  sp <-   purrr::map(sp_u18_rateSp, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u18[,1:4],
    special.predictors = sp,
    predictors.op = "mean",
    time.predictors.prior = 1985:y,
    dependent = "rate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_18_ccodes$Code[u_18_ccodes$Country =="England and Wales"],
    controls.identifier = u_18_ccodes$Code[u_18_ccodes$Country !="England and Wales"],
    time.optimize.ssr = 1985:y,
    time.plot = 1985:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u18_sp <- bind_rows(super_md_u18_sp, md)
}

# Model 2 - u18 GDP as predictor -----------------------------------------------------------------------------


super_md_u18_gdp <- tibble()

for (y in 1995:2005){
  sp <-   purrr::map(sp_u18_gdp_1990, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u18[,1:5] %>% filter(!Country %in% exclude_u18_gdp_1990),
    special.predictors = sp,
    predictors = "GDPperCap",
    predictors.op = "mean",
    time.predictors.prior = 1990:y,
    dependent = "rate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_18_ccodes$Code[u_18_ccodes$Country =="England and Wales"],
    controls.identifier = u_18_ccodes %>% filter(!Country %in% exclude_u18_gdp_1990, Country != "England and Wales") %>% pull(Code),
    time.optimize.ssr = 1990:y,
    time.plot = 1990:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u18_gdp <- bind_rows(super_md_u18_gdp, md)
}


# Model 3: u18 all -------------------------------------------------------------------------------------------

super_md_u18_all <- tibble()

for (y in 1995:2005){
  
  sp <-   purrr::map(sp_u18_all_1990, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u18 %>% filter(!Country %in% exclude_u18_all_1990),
    special.predictors = sp,
    predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
    predictors.op = "mean",
    time.predictors.prior = 1990:y,
    dependent = "rate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_18_ccodes$Code[u_18_ccodes$Country =="England and Wales"],
    controls.identifier = u_18_ccodes %>% filter(!Country %in% exclude_u18_all_1990, Country != "England and Wales") %>% pull(Code),
    time.optimize.ssr = 1990:y,
    time.plot = 1990:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u18_all <- bind_rows(super_md_u18_all, md)
}


# Model 4: u20 Sp --------------------------------------------------------------------------------------------

super_md_u20_sp <- tibble()

for (y in 1995:2005){
  sp <-   purrr::map(sp_u20_sp, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u20[,1:4],
    special.predictors = sp,
    predictors.op = "mean",
    time.predictors.prior = 1990:y,
    dependent = "pRate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_20_ccodes$Code[u_20_ccodes$Country =="England and Wales"],
    controls.identifier = u_20_ccodes %>% filter(Country != "England and Wales") %>% pull(Code),
    time.optimize.ssr = 1990:y,
    time.plot = 1990:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u20_sp <- bind_rows(super_md_u20_sp, md)
}


# Model 5 - u20 gdp ------------------------------------------------------------------------------------------

super_md_u20_gdp <- tibble()

for (y in 1995:2005){
  
  sp <-   purrr::map(sp_u20_gdp, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u20[,c(1:4, 6)] %>% filter(!Country %in% exclude_u20_gdp),
    special.predictors = sp,
    predictors = "GDPperCap",
    predictors.op = "mean",
    time.predictors.prior = 1990:y,
    dependent = "pRate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_20_ccodes$Code[u_20_ccodes$Country =="England and Wales"],
    controls.identifier = u_20_ccodes %>% filter(!Country %in% exclude_u20_gdp, Country != "England and Wales") %>% pull(Code),
    time.optimize.ssr = 1990:y,
    time.plot = 1990:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u20_gdp <- bind_rows(super_md_u20_gdp, md)
}


# Model 6: u20 all -------------------------------------------------------------------------------------------

super_md_u20_all <- tibble()

for (y in 1995:2005){
  sp <-   purrr::map(sp_u20_all, 
                     function(x) list(var = 
                                        ifelse(min(x$yrs)<y,
                                               x[[1]],
                                               NA),
                                      yrs = 
                                        x$yrs[x$yrs<y],
                                      op = 
                                        ifelse(min(x$yrs)<y,
                                               x[[3]],
                                               NA))) %>% 
    purrr::modify_if(anyNA, function(x) NULL)
  
  dp <- dataprep(
    foo = synthData_u20[,-5] %>% filter(!Country %in% exclude_u20_all),
    special.predictors = sp,
    predictors = c("GDPperCap", "MobilePhones", "UrbanPop", "MF_ratio"),
    predictors.op = "mean",
    time.predictors.prior = 1990:y,
    dependent = "pRate",
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    treatment.identifier = u_20_ccodes$Code[u_20_ccodes$Country =="England and Wales"],
    controls.identifier = u_20_ccodes %>% filter(!Country %in% exclude_u20_all, Country != "England and Wales") %>% pull(Code),
    time.optimize.ssr = 1990:y,
    time.plot = 1990:2013
  )
  md <- predvalues_synth(dp, synth_outputs = FALSE, yr = y)
  md <- md %>% 
    mutate(IntYr = y,
           mspe = pre_MSPE(md))
  super_md_u20_all <- bind_rows(super_md_u20_all, md)
}


# output -----------------------------------------------------------------------------------------------------

save(
  super_md_u18_sp,
  super_md_u18_gdp,
  super_md_u18_all,
  super_md_u20_sp,
  super_md_u20_gdp,
  super_md_u20_all,
  file = "Data/time_placebos.rdata"
)

