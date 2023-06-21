# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(plm)
library(stargazer)
library(purrr)
library(stringr)


# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSH.RData")
  dir.create("3.SFA_outputs")
  setwd("3.SFA_outputs")
  dir.create("CSH")
  setwd("CSH")
  dir.create("1. Portugal Frontier (operational)")
  setwd("1. Portugal Frontier (operational)")
  ## 1.1 Variables and Functional form ------------------------------------
    ### A. Select variables and test correlation ----------------------------
    CSH_0 = CSH |> select(hospital, year, operational, operational_ACSS, in_days, RO, azo, region, 
                          surg, app, urge, patient_dis, wait_scheduled_surg) |> 
      drop_na()
    
    # correlation testing RO
    corr_table = CSH_0 |> 
      select(operational, operational_ACSS, in_days, RO, surg, app, urge, 
             patient_dis, wait_scheduled_surg) |> 
      cor()
    
    # Discard volume variables that have high correlation 
    #(choosen the scale variable will capture the effects of the others)
    CSH_0 = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                          surg, wait_scheduled_surg) |> 
      filter(azo == 0) |>
      drop_na()
    
    ### B. Panel data -----------------------------------------------------
    CSH_panel = pdata.frame(CSH_0, c("hospital", "year"))
  ## 1.2 Define functional forms ------------------------------------------
  form = log(operational) ~ log(in_days)  + log(RO) # includes Dummy variable
  formt = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO)
  
  # Firm level inefficiencies models BC95
  # mean inefficiency of u_it is "determined" by factors z_it
  formz = log(operational) ~ log(in_days) +log(surg) | wait_scheduled_surg
  formtz = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) | RO + wait_scheduled_surg

  # FE model G05 (Greene)
  # time invariant individual heterogeneity is not separate from individual innefficieny
  form_r = log(operational) ~ log(in_days)  + log(RO) + factor(region) 
  formt_r = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + factor(region) 
  
# 2. Operational Cost function : Continent --------------------------------
  ## 2.1 Estimation: time invariant inefficiencies ------------------------
    ### A. Cobb-Douglas (form) ----------------------------------------------
    sfront = sfa( form,  
                  data = CSH_panel,
                  ineffDecrease = F, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
  
    ### B. Trans log (formt) -------------------------------------------------
    sfrontt = sfa( formt,  
                  data = CSH_panel,
                  ineffDecrease = F, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfrontt)
    
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontz = sfa( formz,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontz)
    
    ### D. Trans log with z_it (formtz) --------------------------------------
    sfronttz = sfa( formtz,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfronttz)
    
    ### E. Cobb-Douglas with FE by region (form_r) ----------------------------
    sfrontr = sfa( form_r,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfrontr)
    
    ### F. Trans Log with FE by region (form_r) ------------------------------
    sfronttr = sfa( formt_r,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfronttr)

# 3. Tables ---------------------------------------------------------------
  ### A. BC92 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfront))
  c2 = as.vector(coef(sfrontt))

  pv1 = as.vector(coef(summary(sfront))[,4])
  pv2 = as.vector(coef(summary(sfrontt))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfront, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfrontt, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]),3)

  ll1 = round(as.numeric(logLik(sfront, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfrontt, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSH_panel)
  lm2 = lm(formt, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            se = list(c1,c2),
            p = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{inpatient days}$", 
                                 "$(\\log \\text{inpatient days}) ^2$", 
                                  "$\\log$ Rate of Occupancy"))

  
  ### B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfronttz))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfronttz))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[9,1]), 3)
  g2 = round(as.vector(coef(summary(sfronttz, extraPar = TRUE))[9,1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttz, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)

  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(operational) ~ log(in_days) + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregressionz_table_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant", "$\\log \\text{inpatient days}$",
                                 "$(\\log \\text{inpatient days}) ^2$", 
                                 "Z Constant", "Rate of Occupancy", "Wait for Scheduled Surgeries"))
# 4. Azores Prediction: Continent Frontier ---------------------------------
  ## 4.1 Prediction Portugal Continent ---------------------------------------
  CSH_predict = CSH_panel |> select(hospital, year, operational)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = predict(sfront, newdata = CSH_panel),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfa" = "predict")
  prediction_sfat = data.frame(predict = predict(sfrontt, newdata = CSH_panel),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfat" = "predict")
  prediction_sfaz = data.frame(predict = predict(sfrontz, newdata = CSH_panel),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfaz" = "predict")
  prediction_sfatz = data.frame(predict = predict(sfronttz, newdata = CSH_panel),
                                 hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfatz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSH_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSH_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL))
  
  ## 4.2 Prediction Azores Island -----------------------------------------
  CSH_panel_a = CSH |> select(hospital, year, operational, cmvmc, in_days, RO, azo, region, 
                        surg, app, urge, patient_dis, wait_scheduled_surg) |> 
    filter(azo ==1) |> drop_na() |> pdata.frame(c("hospital", "year"))
  
  CSH_predict_a = CSH_panel_a |> select(hospital, year, operational) 

  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSH_panel_a)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel_a)))) |>
    separate("hospital", c("hospital", "year"),sep = "-")
  prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSH_panel_a)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel_a)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predictt" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSH_panel_a)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel_a)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predictz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSH_panel_a)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel_a)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predicttz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSH_predict_a, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSH_predict_a = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  # These differences are too high
  CSH_predict_a = CSH_predict_a |> mutate(difference = operational - predict, differencet = operational - predictt,
                                          differencez = operational - predictz, differencetz = operational - predicttz)
  
  ## 4.3 Efficiency Portugal Continent ------------------------------------
  # A. Efficiency tables
  efficiencies_sfa = data.frame(hospital = rownames(efficiencies(sfront, newdata = CSH_panel)),
                                efficiency = efficiencies(sfront, newdata = CSH_panel)) |>
    rename("efficiency_sfa" = "efficiency")
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSH_panel),
                                 hospital = rownames(efficiencies(sfrontt, newdata = CSH_panel))) |>
    rename("efficiency_sfat" = "efficiency")
  efficiencies_sfaz = data.frame(efficiency = efficiencies(sfrontz, newdata = CSH_panel),
                                 hospital = rownames(efficiencies(sfrontz, newdata = CSH_panel))) 
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSH_panel),
                                  hospital = rownames(efficiencies(sfronttz, newdata = CSH_panel))) |>
    rename("efficiencyt.2015" = "efficiency.2015", "efficiencyt.2016" = "efficiency.2016",
           "efficiencyt.2017" = "efficiency.2017", "efficiencyt.2018" = "efficiency.2018",
           "efficiencyt.2019" = "efficiency.2019", "efficiencyt.2020" = "efficiency.2020",
           "efficiencyt.2021" = "efficiency.2021")

  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSH_effi = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital"), 
                                    .init = NULL))
  
  # C. Descriptive 
  CSH_effi = CSH_effi |> mutate(mean_efficiencyz = rowMeans(CSH_effi[,4:10], na.rm = TRUE),
                                mean_efficiencytz = rowMeans(CSH_effi[,11:16], na.rm = TRUE))
  ## 4.4 Efficiency Azores Island -----------------------------------------
  
  # A. Efficiency tables
  efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSH_panel_a),
                                hospital = rownames(efficiencies(sfront, newdata = CSH_panel_a))) |>
    rename("efficiency_sfa" = "efficiency")
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSH_panel_a),
                                 hospital = rownames(efficiencies(sfrontt, newdata = CSH_panel_a))) |>
    rename("efficiency_sfat" = "efficiency")
  efficiencies_sfaz = data.frame(efficiency = efficiencies(sfrontz, newdata = CSH_panel_a),
                                 hospital = rownames(efficiencies(sfrontz, newdata = CSH_panel_a))) 
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSH_panel_a),
                                  hospital = rownames(efficiencies(sfronttz, newdata = CSH_panel_a))) |>
    rename("efficiencyt.2015" = "efficiency.2015", "efficiencyt.2016" = "efficiency.2016",
           "efficiencyt.2017" = "efficiency.2017", "efficiencyt.2018" = "efficiency.2018",
           "efficiencyt.2019" = "efficiency.2019", "efficiencyt.2020" = "efficiency.2020",
           "efficiencyt.2021" = "efficiency.2021")
  
  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSH_effi_a = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital"), 
                                    .init = NULL))
  # C. Descriptive 
  CSH_effi = CSH_effi |> mutate(mean_efficiencyz = rowMeans(CSH_effi[,4:10], na.rm = TRUE),
                                mean_efficiencytz = rowMeans(CSH_effi[,11:16], na.rm = TRUE))
  
  ## 4.5 Prediction Azores and Portugal ------------------------------------
  CSH_panel = CSH |> select(azo, hospital, year, operational, in_days, RO, azo, region, 
                        surg, urge, wait_scheduled_surg) |> 
    drop_na() |> pdata.frame(c("hospital", "year"))
  CSH_predict = CSH_panel |> select(hospital, year, operational,azo)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSH_panel)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfa" = "predict")
  prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSH_panel)),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfat" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSH_panel)),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfaz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSH_panel)),
                                hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfatz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSH_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSH_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  # These differences are too high
  CSH_predict = CSH_predict |> mutate(difference = operational - predict_sfa, differencet = operational - predict_sfat,
                                      differencez = operational - predict_sfaz, differencetz = operational - predict_sfatz,
                                      difference_perc_sfa = (operational - predict_sfa)/predict_sfa,
                                      difference_perc_sfat = (operational - predict_sfat)/predict_sfat,
                                      difference_perc_sfaz = (operational - predict_sfaz)/predict_sfaz,
                                      difference_perc_sfatz = (operational - predict_sfatz)/predict_sfatz)
  write.csv(CSH_predict, "CSH_predict.csv", row.names = FALSE)
  
  ## 4.6 Efficiency Azores and Portugal ------------------------------------
  # A. Efficiency tables
  efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSH_panel),
                                hospital = rownames(efficiencies(sfront, newdata = CSH_panel))) |>
    rename("efficiency_sfa" = "efficiency")
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSH_panel),
                                 hospital = rownames(efficiencies(sfrontt, newdata = CSH_panel))) |>
    rename("efficiency_sfat" = "efficiency")
  
  efficiencies_sfaz = data.frame(hospital = rownames(efficiencies(sfrontz, newdata = CSH_panel)),
                                 efficiency = efficiencies(sfrontz, newdata = CSH_panel)) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021"),
                 names_to = "year", values_to = "efficiency_sfa_z") |>
    mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
  
  
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSH_panel),
                                  hospital = rownames(efficiencies(sfronttz, newdata = CSH_panel))) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021"),
                 names_to = "year", values_to = "efficiency_sfa_tz") |>
    mutate( year = str_remove(year, ".*\\."))
  
  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSH_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                       by = c("hospital" = "hospital", "year" = "year"))
  CSH_effi = bind_rows(CSH_effi, full_join(efficiencies_sfa,efficiencies_sfat), 
                       by = c("hospital" = "hospital"))
  
  write.csv(CSH_effi, "CSH_effi.csv", row.names = FALSE)
  
# 5. Data ------------------------------------------------------------------
rm(list = setdiff(ls(), c("CSH", "sfrontz")))
setwd("..")
  dir.create("2. Frontier w Azores Dummy (operational)")
  setwd("2. Frontier w Azores Dummy (operational)")
  ### A. Select variables and test correlation ----------------------------
  CSH_panel = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                         surg, wait_scheduled_surg) |> 
    drop_na() |>
    pdata.frame(c("hospital", "year"))

  ## 5.1 Define functional forms ------------------------------------------
  form = log(operational) ~ log(in_days) + log(RO) + azo
  formt = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + azo
  
  # Firm level inefficiencies models BC95
  # mean inefficiency of u_it is "determined" by factors z_it
  formz = log(operational) ~ log(in_days) + log(surg) + azo| wait_scheduled_surg
  formtz = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + azo| RO + wait_scheduled_surg
  
# 6. Operational Cost function: Azores Dummy ------------------------------
  #(choosen the scale variable will capture the effects of the others)
  CSH_0 = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                        surg, urge, wait_scheduled_surg) |> 
    drop_na()
  
  ## 6.1 Estimation: time invariant inefficiencies ------------------------
  ### A. Cobb-Douglas (form) -----------------------------------------------
  sfrontd = sfa( form,  
                data = CSH_panel,
                ineffDecrease = F, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfrontd, extraPar = TRUE)
  
  ### B. Trans log (formt) -------------------------------------------------
  sfronttd = sfa( formt,  
                 data = CSH_panel,
                 ineffDecrease = F, # FALSE for cost function and TRUE for production
                 truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                 timeEffect = FALSE, # time is allowed to have an effect on efficiency
                 printIter = 1 )
  summary(sfronttd)
  
  ### C. Cobb-Douglas with z_it (formz) -------------------------------------
  sfrontzd = sfa( formz,  
                 data = CSH_panel,
                 ineffDecrease = F, # FALSE for cost function and TRUE for production
                 truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                 timeEffect = FALSE, # time is allowed to have an effect on efficiency
                 printIter = 1 )
  summary(sfrontzd)
  summary(sfrontz)
  
  ### D. Trans log with z_it (formtz) --------------------------------------
  sfronttzd = sfa( formtz,  
                  data = CSH_panel,
                  ineffDecrease = F, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
  summary(sfronttzd)
  
# 7. Tables ---------------------------------------------------------------
  ### A. BC92 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontd))
  c2 = as.vector(coef(sfronttd))
  
  pv1 = as.vector(coef(summary(sfrontd))[,4])
  pv2 = as.vector(coef(summary(sfronttd))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontd, extraPar = TRUE))[nrow(coef(summary(sfrontd, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfronttd, extraPar = TRUE))[nrow(coef(summary(sfrontd, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontd, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttd, which = "mle")),3)
  
  me1 = round(summary(sfrontzd)$efficMean,3)
  me2 = round(summary(sfronttzd)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSH_panel)
  lm2 = lm(formt, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            se = list(c1,c2),
            p = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_1_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{inpatient days}$", 
                                 "$(\\log \\text{inpatient days}) ^2$", 
                                 "$\\log$ Rate of Occupancy", "Azores"))
  
  ### B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontzd))
  c2 = as.vector(coef(sfronttzd))
  
  pv1 = as.vector(coef(summary(sfrontzd))[,4])
  pv2 = as.vector(coef(summary(sfronttzd))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontzd, extraPar = TRUE))[9,1]), 3)
  g2 = round(as.vector(coef(summary(sfronttzd, extraPar = TRUE))[9,1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttzd, which = "mle")),3)
  
  me1 = round(summary(sfrontzd)$efficMean,3)
  me2 = round(summary(sfronttzd)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(operational) ~ log(in_days) + azo + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days)  + I((log(in_days)^2)/2) + azo + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_2_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant", "$\\log \\text{inpatient days}$",
                                 "$(\\log \\text{inpatient days}) ^2$", "Azores",
                                 "Z Constant", "Rate of Occupancy", "Wait for Scheduled Surgeries"))

  ### B. BC95: w/ and wthout/ dummy ---------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfrontzd))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfrontzd))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[6,1]), 3)
  g2 = round(as.vector(coef(summary(sfrontzd, extraPar = TRUE))[7,1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfrontzd)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(operational) ~ log(in_days) + RO + wait_scheduled_surg, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days) + log(surg) + azo + RO + wait_scheduled_surg, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_dummy_table_3_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant", "$\\log \\text{inpatient days}$","$\\log \\text{surgeries}$",
                                "Azores",
                                 "Z Constant", "Wait for Scheduled Surgeries"))

# 8. Azores Prediction: Dummy Frontier -------------------------------------
  ## 8.1 Prediction --------------------------------------------------------
  CSH_predict = CSH_panel |> select(hospital, year, operational)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSH_panel)),
                              hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfa" = "predict")
  prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSH_panel)),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfat" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSH_panel)),
                               hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfaz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSH_panel)),
                                hospital = rownames(data.frame(predict(sfront, newdata = CSH_panel)))) |>
    separate("hospital", c("hospital", "year"),sep = "-") |>
    rename("predict_sfatz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSH_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSH_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  # These differences are too high
  CSH_predict = CSH_predict |> mutate(difference = operational - predict_sfa, differencet = operational - predict_sfat,
                                          differencez = operational - predict_sfaz, differencetz = operational - predict_sfatz,
                                      difference_perc_sfa = (operational - predict_sfa)/predict_sfa,
                                      difference_perc_sfat = (operational - predict_sfat)/predict_sfat,
                                      difference_perc_sfaz = (operational - predict_sfaz)/predict_sfaz,
                                      difference_perc_sfatz = (operational - predict_sfatz)/predict_sfatz)
  write.csv(CSH_predict, "CSH_predict.csv", row.names = FALSE)
  
  CSH_predict_summary = CSH_predict |> group_by(hospital) |>
    summarise(operational = mean(operational), predict_sfa = mean(predict_sfa), difference = mean(difference),
              differencez = mean(differencez), difference_perc_sfa = mean(difference_perc_sfa),
              difference_perc_sfaz = mean(difference_perc_sfa))
  write.csv(CSH_predict_summary, "CSH_predict_summary.csv", row.names = FALSE)
  ## 8.2 Efficiency --------------------------------------------------------
  # A. Efficiency tables
  efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSH_panel),
                                hospital = rownames(efficiencies(sfront, newdata = CSH_panel))) |>
    rename("efficiency_sfa" = "efficiency")
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSH_panel),
                                 hospital = rownames(efficiencies(sfrontt, newdata = CSH_panel))) |>
    rename("efficiency_sfat" = "efficiency")
  
  efficiencies_sfaz = data.frame(hospital = rownames(efficiencies(sfrontz, newdata = CSH_panel)),
                                 efficiency = efficiencies(sfrontz, newdata = CSH_panel)) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_z") |>
     mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
    
  
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSH_panel),
                                  hospital = rownames(efficiencies(sfronttz, newdata = CSH_panel))) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_tz") |>
    mutate( year = str_remove(year, ".*\\."))
  
  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSH_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                       by = c("hospital" = "hospital", "year" = "year"))
  CSH_effi = bind_rows(CSH_effi, full_join(efficiencies_sfa,efficiencies_sfat), 
                       by = c("hospital" = "hospital"))
  
  write.csv(CSH_effi, "CSH_effi.csv", row.names = FALSE)
  
#### STOP HERE
# STOPPED HEREE -----------
# 1. Data -----------------------------------------------------------------
rm(list = setdiff(ls(), "CSH"))
  setwd("..")
  dir.create("3. Portugal Frontier (staff)")
  setwd("3. Portugal Frontier (staff)")
  ## 1.1 Variables and Functional form ------------------------------------
    ### A. Select variables and test correlation ----------------------------
    CSH_0 = CSH |> select(hospital, year, operational, cmvmc, in_days, RO, azo, region, 
                          surg, app, urge, patient_dis, wait_scheduled_surg) |> 
      drop_na()
    
    # correlation testing RO
    corr_table = CSH_0 |> 
      select(operational, cmvmc, in_days, RO, surg, app, urge, 
             patient_dis, wait_scheduled_surg) |> 
      cor()
    
    # Discard volume variables that have high correlation 
    #(choosen the scale variable will capture the effects of the others)
    CSH_0 = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                          wait_scheduled_surg) |> 
      filter(azo == 0) |>
      drop_na()
    
    ### B. Panel data -----------------------------------------------------
    CSH_panel = pdata.frame(CSH_0, c("hospital", "year"))
  ## 1.2 Define functional forms ------------------------------------------
    form = log(operational) ~ log(in_days) + log(surg) + log(urge) + log(RO) # includes Dummy variable
    formt = log(operational) ~ log(in_days) + log(surg) + I((log(in_days)^2)/2) + I((log(surg)^2)/2) + log(urge) + log(RO)
    
    # Firm level inefficiencies models BC95
    # mean inefficiency of u_it is "determined" by factors z_it
    formz = log(operational) ~ log(in_days) + log(surg) + log(urge) | RO + wait_scheduled_surg
    formtz = log(operational) ~ log(in_days) + log(surg) + log(urge) + I((log(in_days)^2)/2) + I((log(surg)^2)/2) | RO + wait_scheduled_surg
    
    # FE model G05 (Greene)
    # time invariant individual heterogeneity is not separate from individual innefficieny
    form_r = log(operational) ~ log(in_days) + log(surg) + log(urge) + log(RO) + factor(region) 
    formt_r = log(operational) ~ log(in_days) + log(surg) + I((log(in_days)^2)/2) + I((log(surg)^2)/2) + log(urge) + log(RO) + factor(region) 
    
# 2. Operational Cost function : Continent --------------------------------
  ## 2.1 Estimation: time invariant inefficiencies ------------------------
    ### A. Cobb-Douglas (form) ----------------------------------------------
    sfront = sfa( form,  
                  data = CSH_panel,
                  ineffDecrease = F, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    ### B. Trans log (formt) -------------------------------------------------
    sfrontt = sfa( formt,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontt)
    
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontz = sfa( formz,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontz)
    
    ### D. Trans log with z_it (formtz) --------------------------------------
    sfronttz = sfa( formtz,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfronttz)
    
    ### E. Cobb-Douglas with FE by region (form_r) ----------------------------
    sfrontr = sfa( form_r,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontr)
    
    ### F. Trans Log with FE by region (form_r) ------------------------------
    sfronttr = sfa( formt_r,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfronttr)
    
# 3. Tables ---------------------------------------------------------------
  ### A. BC92 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfront))
  c2 = as.vector(coef(sfrontt))
  
  pv1 = as.vector(coef(summary(sfront))[,4])
  pv2 = as.vector(coef(summary(sfrontt))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfront, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfrontt, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfront, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfrontt, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSH_panel)
  lm2 = lm(formt, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            se = list(c1,c2),
            p = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{inpatient days}$", "$\\log$ surgeries",
                                 "$(\\log \\text{inpatient days}) ^2$","$(\\log \\text{surgeries}) ^2$", 
                                 "$\\log$ urgencies", "$\\log$ Rate of Occupancy"))
  
  summary(sfront)
  ### B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfronttz))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfronttz))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[9,1]), 3)
  g2 = round(as.vector(coef(summary(sfronttz, extraPar = TRUE))[9,1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttz, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(operational) ~ log(in_days)  + azo + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + azo + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Operational Costs"),
            title = "Stochastic Frontier: Hospital Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregressionz_table_CSH.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant", "$\\log \\text{inpatient days}$", "$\\log$ surgeries",
                                 "$\\log$ urgencies", "$(\\log \\text{inpatient days}) ^2$", 
                                 "$\\log \\text{surgeries} ^2$", 
                                 "Z Constant", "Rate of Occupancy", "Wait for Scheduled Surgeries"))
# §. Tests ----------------------------------------------------------------
warnings()  
setwd("../../..")
###### END #####
rm(list=ls())
