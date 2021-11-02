library(tidyverse)
library(patchwork)
library(car)
library(nlme)

return_p_tables <- function() {
  load("Data/all_uk_rates_u.rdata")

  sim_once <-
    function(trend_diff = 0,
             data = u18_engscot,
             form = synth_pred ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare,
             mod = mod_scot,
             phi = scot_phi) {
      
      one_diff_mod <- mod$coefficients
      
      if(!str_detect(as.character(form)[[3]], "Trend2")) {
        one_diff_mod['Trend2'] <- 0
      }
      
      new_data <- data %>%
        mutate(resid = mod$residuals) %>%
        arrange(Country, Year) %>%
        group_by(Country) %>%
        mutate(
          new_resid = rnorm(25, sd = sd(resid)),
          lagged_resid = lag(new_resid),
          synth_pred = case_when(
            Country != "England" ~ Value,
            Year < 1999 ~ Value,
            TRUE ~ (one_diff_mod[1]) +
              Time * (one_diff_mod[2]) +
              England * (one_diff_mod[3]) +
              Cat1 * (one_diff_mod[4]) +
              Trend1 * (one_diff_mod[5]) +
              Cat1_Eng * (one_diff_mod[6]) +
              Trend1_Eng * trend_diff +
              Trend2 * one_diff_mod['Trend2'] +
              PillScare * (one_diff_mod['PillScare']) +
              new_resid +
              phi * lagged_resid
          )
        )
      
      new_mod <- gls(
        form,
        data = new_data,
        method = "ML",
        correlation = corARMA(
          p = 1,
          q = 0,
          form = ~ Time | England
        )
      )
      
      summary(new_mod)$tTable['Trend1_Eng', 'p-value']
    }
  
  
  # data --------------------------------------------------------------------
  
  
  under_18 <- all_UK_rates$`Under 18` %>%
    filter(Year >= 1992, Country != "England and Wales") %>%
    mutate(
      England = ifelse(Country == "England", 1, 0),
      Year = as.numeric(Year),
      Time = Year - min(Year) + 1,
      Cat1 = ifelse(Year < 1999, 0, 1),
      Cat2 = ifelse(Year <= 2008, 0, 1),
      Trend1 = ifelse(Cat1 == 0, 0, Year - 1998),
      Trend2 = ifelse(Cat2 == 0, 0, Year - 2008),
      PillScare = ifelse(Year >= 1996, 1, 0)
    ) %>%
    mutate_at(., colnames(.)[5:9], list(Eng = ~ . * England))
  
  u18_eng <-  filter(under_18, Country == "England")
  u18_scot <-  filter(under_18, Country == "Scotland")
  u18_wal <-  filter(under_18, Country == "Wales")
  u18_engscot <- filter(under_18, Country != "Wales")
  u18_engwal <-  filter(under_18, Country != "Scotland")
  
  
  # models ------------------------------------------------------------------
  
  mod_scot <- gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare,
    data = u18_engscot,
    method = "ML",
    correlation = corARMA(
      p = 1,
      q = 0,
      form = ~ Time | England
    )
  )
  
  scot_phi <-
    coef(mod_scot$modelStruct$corStruct, unconstrained = FALSE)
  
  mod_wal <- gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare,
    data = u18_engwal,
    method = "ML",
    correlation = corARMA(
      p = 1,
      q = 0,
      form = ~ Time | England
    )
  )
  
  wal_phi <-
    coef(mod_wal$modelStruct$corStruct, unconstrained = FALSE)
  
  mod_scot_08 <- gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = u18_engscot,
    method = "ML",
    correlation = corARMA(
      p = 1,
      q = 0,
      form = ~ Time | England
    )
  )
  
  scot_phi_08 <-
    coef(mod_scot_08$modelStruct$corStruct, unconstrained = FALSE)
  
  mod_wal_08 <- gls(
    Value ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare,
    data = u18_engwal,
    method = "ML",
    correlation = corARMA(
      p = 1,
      q = 0,
      form = ~ Time | England
    )
  )
  
  wal_phi_08 <-
    coef(mod_wal_08$modelStruct$corStruct, unconstrained = FALSE)
  
  # running synth -----------------------------------------------------------
  
  
  
  sim <- tribble(
    ~ iter,
    ~ comp,
    ~ diff,
    ~ data,
    ~ formula,
    ~ mod,
    ~ phi,
    1:10000, "scot_simple", 0, u18_engscot, synth_pred ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare, mod_scot, scot_phi,
    1:10000, "wal_simple", 0, u18_engwal, synth_pred ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + PillScare, mod_wal, wal_phi,
    1:10000, "scot_bump", 0, u18_engscot, synth_pred ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare, mod_scot_08, scot_phi_08, 
    1:10000, "wal_bump", 0, u18_engwal, synth_pred ~ Time + England + Cat1 + Trend1 + Cat1_Eng + Trend1_Eng + Trend2 + PillScare, mod_wal_08, wal_phi_08
  )
  
  sim <- bind_rows(sim, mutate(sim, diff = diff - 0.5)) %>%
    bind_rows(mutate(sim, diff = diff - 1))
  
  iterations <- sim %>%
    unnest(iter) %>%
    mutate(p = pmap_dbl(
      list(diff, data, formula, mod, phi),
      ~ sim_once(..1, ..2, ..3, ..4, ..5)
    )) %>%
    select(comp, diff, p)
  
  iterations
}

p_table <- return_p_tables()
