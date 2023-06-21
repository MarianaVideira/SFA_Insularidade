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

# PORTUGAL -----------------------------------------------------------------
# 1. Data and Functional forms ----------------------------------------
load("0.DataBase/CSP.RData")
dir.create("3.SFA_outputs")
setwd("3.SFA_outputs")
dir.create("CSP")
setwd("CSP")
dir.create("1. Portugal Frontier (staff)")
setwd("1. Portugal Frontier (staff)")
    ### A. Select variables and test correlation ----------------------------
    CSP_1 = CSP |> select(id, unit, year, azo, staff, app, app_dome, app_remote, 
                          n_polos, prop_nofam_MF, prop_fem, prop_age_0_4,
                          prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
      drop_na()
    
    corr_table = CSP_1 |> 
      select(staff, app, app_dome, app_remote, n_polos, prop_nofam_MF, prop_fem,
             prop_age_0_4, prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
      cor()
    
    # Discard volume variables that have high correlation 
    #(choosen the scale variable will capture the effects of the others)
    CSP_0 = CSP |> select(id, year, unit, azo, staff, app, n_polos, prop_nofam_MF, 
                          prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                          prop_age_65_74, prop_age_75_hig) |> 
      filter(azo == 0) |>
      drop_na()
    ### B. Panel data -----------------------------------------------------
    CSP_panel = pdata.frame(CSP_0, c("unit", "year"))
  
  ## 1.2 Define functional forms ------------------------------------------
  form = log(staff) ~ log(app) + log(prop_age_45_64)+ log(prop_age_75_hig)
  formt = log(staff) ~ log(app) + I((log(app)^2)/2) + log(prop_age_45_64)+ log(prop_age_75_hig) 
  
  # Firm level inefficiencies models BC95
  # mean inefficiency of u_it is "determined" by factors z_it
  formz = log(staff) ~ log(app) + log(n_polos) | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
  formtz = log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem
  
# 2. Staff Cost Function: Cross section -----------------------------------
  ## 2.1 Estimation Portugal Continent 2016-2018 ------------------------------------
    ### A. Cobb-Douglas (form) ------------------------------------
    sfront16 = sfa( form,  
                    data = filter(CSP_0, year == 2016),
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfront16, extraPar = TRUE)
    
    sfront17 = sfa( form,  
                    data = filter(CSP_0, year == 2017),
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfront17, extraPar = TRUE)
    
    sfront18 = sfa( form,  
                    data = filter(CSP_0, year == 2018),
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfront18, extraPar = TRUE)
    ### B. Trans log (formt) -------------------------------------------------
    sfront = sfa( formt,  
                  data = filter(CSP_0, year == 2016),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    sfront = sfa( formt,  
                  data = filter(CSP_0, year == 2017),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    sfront = sfa( formt,  
                  data = filter(CSP_0, year == 2018),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
  
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
    sfront = sfa( formz,  
                  data = filter(CSP_0, year == 2016),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    sfront = sfa( formz,  
                  data = filter(CSP_0, year == 2017),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    sfront = sfa( formz,  
                  data = filter(CSP_0, year == 2018),
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
  

  ## 2.2 Residuals Check: Portugal Continent ------------------------------
    ### A. Cobb-Douglas 2016-2018 (form) ----------------------------------
    # Create a data frame with predicted values and residuals
    residuals_df16 = data.frame(predicted = fitted(sfront16) , residuals = residuals(sfront16))
    residuals_df17 = data.frame(predicted = fitted(sfront17) , residuals = residuals(sfront17))
    residuals_df18 = data.frame(predicted = fitted(sfront18) , residuals = residuals(sfront18))
    
    # Create a scatter plot of predicted values vs. residuals 2016
    p16 = ggplot(residuals_df16, aes(x = predicted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot Cobb-Douglas: 2016")
    p16
    
    # Create a density plot of the predicted values
    d16 = ggplot(residuals_df16, aes(x = predicted)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(x = "Predicted Values", y = "Density", title = "Density Plot of residuals Cobb-Douglas: 2016")
    d16 
    
    # Create a scatter plot of predicted values vs. residuals 2018
    p17 = ggplot(residuals_df17, aes(x = predicted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot Cobb-Douglas: 2017")
    p17
    
    # Create a density plot of the predicted values
    d17 = ggplot(residuals_df17, aes(x = predicted)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(x = "Predicted Values", y = "Density", title = "Density Plot of residuals Cobb-Douglas: 2017")
    d17 
    
    # Create a scatter plot of predicted values vs. residuals 2018
    p18 = ggplot(residuals_df18, aes(x = predicted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      labs(x = "Predicted Values", y = "Residuals", title = "Residual Plot Cobb-Douglas: 2018")
    p18
    
    # Create a density plot of the predicted values
    d18 = ggplot(residuals_df18, aes(x = residuals)) +
      geom_density(fill = "blue", alpha = 0.5) +
      labs(x = "Predicted Values", y = "Density", title = "Density Plot of residuals Cobb-Douglas: 2018")
    d18
    # Create a scatter plot of predicted values vs. residuals
    
# 3. Operational Cost function: Panel Data --------------------------------
  ## 3.3 Estimation: time invariant inefficiencies ------------------------
    ### A. Cobb-Douglas (form) ----------------------------------------------
    sfront = sfa( form,  
                  data = CSP_panel,
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
  
    ### B. Trans log (formt) -------------------------------------------------
    sfrontt = sfa( formt,  
                   data = CSP_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontt,extraPar = TRUE)
    class(summary(sfrontt))
  
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontz = sfa( formz,  
                   data = CSP_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontz, extraPar = TRUE)
  
    ### D. Trans log with z_it (formtz) --------------------------------------
    sfronttz = sfa( formtz,  
                    data = CSP_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfronttz, extraPar = TRUE)
  
## 4. Tables for Panel Data ------------------------------------------------
  ## A. BC92 ---------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfront))
  c2 = as.vector(coef(sfrontt))

  
  pv1 = as.vector(coef(summary(sfront))[,4])
  pv2 = as.vector(coef(summary(sfrontt))[,4])

  # save gamma parameter, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfront, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfrontt, extraPar = TRUE))[nrow(coef(summary(sfront, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfront, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfrontt, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)

  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSP_panel)
  lm2 = lm(formt, data = CSP_panel)

  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            se = list(pv1,pv2),
            p = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 4, float.env = "table",
            dep.var.labels=c("log Staff Costs"),
            title = "Stochastic Frontier: Primary Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_CSP.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{Appointments}$", "$(\\log \\text{Appointments}) ^2$", 
                                 "$\\log$ proportion ages 45-64", "$\\log$ proportion ages 75+"))
  summary(sfront)
  
  ## B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfronttz))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfronttz))[,4])
  
  # save gamma parameter, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[nrow(coef(summary(sfrontz, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfronttz, extraPar = TRUE))[nrow(coef(summary(sfrontz, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttz, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfronttz)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(staff) ~ log(app) + log(n_polos) + log(prop_age_45_64) + prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem, data = CSP_panel)
  lm2 = lm(log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) + log(prop_age_45_64) + prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem, data = CSP_panel)
  
  summary(sfrontz)
  summary(sfronttz)
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("log Staff Costs"),
            title = "Stochastic Frontier: Primary Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregressionz_table_CSP.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{Appointments}$","$(\\log \\text{Appointments}) ^2$", 
                                 "$\\log$ nº of Polos", "Z Constant","proportion without FD",
                                 "proportion ages 45-64", "proportion ages 75+",
                                 "proportion ages 05-14", "proportion ages 00-04", 
                                 "proportion female"))

  
# 4. Azores Prediction: Continent Frontier ---------------------------------
  ## 4.1 Prediction Portugal Continent ---------------------------------------
  CSP_predict = CSP_panel |> select(unit, year, staff)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = predict(sfront, newdata = CSP_panel),
                              unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfa" = "predict")
  prediction_sfat = data.frame(predict = predict(sfrontt, newdata = CSP_panel),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfat" = "predict")
  prediction_sfaz = data.frame(predict = predict(sfrontz, newdata = CSP_panel),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfaz" = "predict")
  prediction_sfatz = data.frame(predict = predict(sfronttz, newdata = CSP_panel),
                                unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfatz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSP_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSP_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("unit" = "unit",
                                                 "year" = "year"), 
                                    .init = NULL))
  ## 4.2 Prediction Azores Island -----------------------------------------
  CSP_panel_a = CSP |> select(id, year, unit, azo, staff, app, n_polos, prop_nofam_MF, 
                        prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                        prop_age_65_74, prop_age_75_hig) |> 
    filter(azo == 0) |> drop_na() |> pdata.frame(c("unit", "year"))
  
  CSP_predict_a = CSP_panel_a |> select(unit, year, staff) 
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSP_panel_a)),
                              unit = rownames(data.frame(predict(sfront, newdata = CSP_panel_a)))) |>
    separate("unit", c("unit", "year"),sep = "-")
  prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSP_panel_a)),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel_a)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predictt" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSP_panel_a)),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel_a)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predictz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSP_panel_a)),
                                unit = rownames(data.frame(predict(sfront, newdata = CSP_panel_a)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predicttz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSP_predict_a, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSP_predict_a = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("unit" = "unit",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  # These differences are too high
  CSP_predict_a = CSP_predict_a |> mutate(difference = staff - predict, differencet = staff - predictt,
                                          differencez = staff - predictz, differencetz = staff - predicttz)
  ## 4.5 Prediction Azores and Portugal ------------------------------------
  CSP_panel = CSP |> select(id, year, unit, azo, staff, app, n_polos, prop_nofam_MF, 
                              prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                              prop_age_65_74, prop_age_75_hig) |> 
    drop_na() |> pdata.frame(c("unit", "year"))
  
  CSP_predict = CSP_panel |> select(unit, year, staff,azo)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSP_panel)),
                              unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-")
  prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSP_panel)),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predictt" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSP_panel)),
                               unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predictz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSP_panel)),
                                unit = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predicttz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSP_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSP_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("unit" = "unit",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  # These differences are too high
  CSP_predict = CSP_predict |> mutate(difference = staff - predict, differencet = staff - predictt,
                                      differencez = staff - predictz, differencetz = staff - predicttz,
                                      difference_perc_sfa = (staff - predict)/predict,
                                      difference_perc_sfat = (staff - predictt)/predictt,
                                      difference_perc_sfaz = (staff - predictz)/predictz,
                                      difference_perc_sfatz = (staff - predicttz)/predicttz)
  write.csv(CSP_predict, "CSP_predict.csv", row.names = FALSE)
  
  ## 4.6 Efficiency Azores and Portugal ------------------------------------
  # A. Efficiency tables
  efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSP_panel),
                                unit = rownames(efficiencies(sfront, newdata = CSP_panel))) |>
    rename("efficiency_sfa" = "efficiency")
  
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSP_panel),
                                 unit = rownames(efficiencies(sfrontt, newdata = CSP_panel))) |>
    rename("efficiency_sfat" = "efficiency")
  
  efficiencies_sfaz = data.frame(unit = rownames(efficiencies(sfrontz, newdata = CSP_panel)),
                                 efficiency = efficiencies(sfrontz, newdata = CSP_panel)) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_z") |>
    mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
  
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSP_panel),
                                  unit = rownames(efficiencies(sfronttz, newdata = CSP_panel))) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_tz") |>
    mutate( year = str_remove(year, ".*\\."))
  
  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSP_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                       by = c("unit" = "unit", "year" = "year"))
  CSP_effi = bind_rows(CSP_effi, full_join(efficiencies_sfa,efficiencies_sfat), 
                       by = c("unit" = "unit"))
  
  write.csv(CSP_effi, "CSP_effi.csv", row.names = FALSE)
  
  # Efficiencies summary with SFA Z
  CSP_effi_sum = data.frame(efficiency = efficiencies(sfronttz, newdata = CSP_panel),
                       unit = rownames(efficiencies(sfronttz, newdata = CSP_panel))) |>
    pivot_longer(c("efficiency.2015", "efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_tz") |> drop_na() |> 
    mutate( year = str_remove(year, ".*\\.")) |> group_by(unit) |> summarise(mean_ef = mean(efficiency_sfa_tz))
  
  write.csv(CSP_effi_sum, "CSP_effi_sum.csv", row.names = FALSE)
# PORTUGAL AND AZORES -----------------------------------------------------
# 6. Data and Functional forms ----------------------------------------
  rm(list = setdiff(ls(), c("CSP", "sfrontz")))
  setwd("..")
  dir.create("2. Frontier w Azores Dummy (staff)")
  setwd("2. Frontier w Azores Dummy (staff)")    
  ### A. Select variables and test correlation ----------------------------
  CSP_0 = CSP |> select(id, unit, year, azo, staff, app, app_dome, app_remote, 
                        n_polos, prop_nofam_MF, prop_fem, prop_age_0_4,
                        prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
    drop_na()
  
  ### B. Panel data -----------------------------------------------------
  CSP_panel = pdata.frame(CSP_0, c("unit", "year"))
  
  ## 6.2 Define functional forms ------------------------------------------
  form = log(staff) ~ log(app) + log(prop_age_45_64)+ log(prop_age_75_hig) + azo
  formt = log(staff) ~ log(app) + I((log(app)^2)/2) + log(prop_age_45_64)+ log(prop_age_75_hig) +azo
  
  # Firm level inefficiencies models BC95
  # mean inefficiency of u_it is "determined" by factors z_it
  formz = log(staff) ~ log(app)  + log(n_polos) + azo | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
  formtz = log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) +azo | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig  + prop_age_0_4 
  
# 7. Operational Cost function: Panel Data --------------------------------
  ## 7.3 Estimation: time invariant inefficiencies ------------------------
    ### A. Cobb-Douglas (form) ----------------------------------------------
    sfrontd = sfa( form,  
                  data = CSP_panel,
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfrontd, extraPar = TRUE)
    
    ### B. Trans log (formt) -------------------------------------------------
    sfronttd = sfa( formt,  
                   data = CSP_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfronttd,extraPar = TRUE)
    class(summary(sfronttd))
    
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontzd = sfa( formz,  
                   data = CSP_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontzd, extraPar = TRUE)
    
    ### D. Trans log with z_it (formtz) --------------------------------------
    sfronttzd = sfa( formtz,  
                    data = CSP_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfronttzd, extraPar = TRUE)
    
## 8. Tables for Panel Data ------------------------------------------------
  ## A. BC92 ---------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontd))
  c2 = as.vector(coef(sfronttd))
  
  
  pv1 = as.vector(coef(summary(sfrontd))[,4])
  pv2 = as.vector(coef(summary(sfronttd))[,4])
  
  # save gamma parameter, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontd, extraPar = TRUE))[nrow(coef(summary(sfrontd, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfronttd, extraPar = TRUE))[nrow(coef(summary(sfrontd, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontd, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttd, which = "mle")),3)
  
  me1 = round(summary(sfrontzd)$efficMean,3)
  me2 = round(summary(sfronttzd)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSP_panel)
  lm2 = lm(formt, data = CSP_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            se = list(pv1,pv2),
            p = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 4, float.env = "table",
            dep.var.labels=c("log Staff Costs"),
            title = "Stochastic Frontier: Primary Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table_CSP.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{Appointments}$", "$(\\log \\text{Appointments}) ^2$", 
                                 "$\\log$ proportion ages 45-64", "$\\log$ proportion ages 75+", "Azores"))

  ## B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontzd))
  c2 = as.vector(coef(sfronttzd))
  
  pv1 = as.vector(coef(summary(sfrontzd))[,4])
  pv2 = as.vector(coef(summary(sfronttzd))[,4])
  
  # save gamma parameter, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontzd, extraPar = TRUE))[nrow(coef(summary(sfrontzd, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfronttzd, extraPar = TRUE))[nrow(coef(summary(sfrontzd, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttzd, which = "mle")),3)
  
  me1 = round(summary(sfrontzd)$efficMean,3)
  me2 = round(summary(sfronttzd)$efficMean,3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(staff) ~ log(app) + log(n_polos) + azo+ log(prop_age_45_64) + prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_0_4 , data = CSP_panel)
  lm2 = lm(log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) +azo + log(prop_age_45_64) + prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_0_4 , data = CSP_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("log Staff Costs"),
            title = "Stochastic Frontier: Primary Healthcare",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregressionz_table_CSP.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant","$\\log \\text{Appointments}$","$(\\log \\text{Appointments}) ^2$", 
                                 "$\\log$ nº of Polos", "Azores","Z Constant","proportion without FD",
                                 "proportion ages 45-64", "proportion ages 75+",
                                 "proportion ages 00-04"))
  
  ## C. BC95: dummy --------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfrontzd))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfrontzd))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[12,1]), 3)
  g2 = round(as.vector(coef(summary(sfrontzd, extraPar = TRUE))[10,1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
  
  me1 = round(summary(sfrontz)$efficMean,3)
  me2 = round(summary(sfrontzd)$efficMean,3)
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(staff) ~ log(app) + log(n_polos) + prop_fem +prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)
  lm2 = lm(log(staff) ~ log(app) + log(n_polos) + azo + prop_fem + prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            se = list(pv1,pv2),
            t.auto = FALSE,
            p.auto = FALSE,
            intercept.bottom = FALSE,
            digits = 3, float.env = "table",
            dep.var.labels=c("$\\log$ Staff Costs"),
            title = "Stochastic Frontier: Primary Healthcare",
            column.labels = c("Continent", "Continent and Azores"),
            model.numbers = TRUE,
            out = "SFAregression_dummy_table_3_CSP.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)),
            covariate.labels = c("Constant", "$\\log \\text{appointments}$","$\\log \\text{number of polos}$",
                                 "Azores",
                                 "Z Constant", "Proportion without FD", "Proportion ages 75+", "Proportion ages 0-4"))

# 9. Azores Prediction: Dummy Frontier -------------------------------------
  ## 9.1 Prediction --------------------------------------------------------
  CSP_predict = CSP_panel |> select(unit, year, staff, azo)
  
  # A. prediction tables
  prediction_sfa = data.frame(predict = exp(predict(sfrontd, newdata = CSP_panel)),
                              unit = rownames(data.frame(predict(sfrontd, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfa" = "predict")
  prediction_sfat = data.frame(predict = exp(predict(sfronttd, newdata = CSP_panel)),
                               unit = rownames(data.frame(predict(sfronttd, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfat" = "predict")
  prediction_sfaz = data.frame(predict = exp(predict(sfrontzd, newdata = CSP_panel)),
                               unit = rownames(data.frame(predict(sfronttd, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfaz" = "predict")
  prediction_sfatz = data.frame(predict = exp(predict(sfronttzd, newdata = CSP_panel)),
                                unit = rownames(data.frame(predict(sfronttzd, newdata = CSP_panel)))) |>
    separate("unit", c("unit", "year"),sep = "-") |>
    rename("predict_sfatz" = "predict")
  
  # B. Join all data frames 
  list_df = list(CSP_predict, prediction_sfa,
                 prediction_sfat, prediction_sfaz, prediction_sfatz)
  
  CSP_predict = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("unit" = "unit",
                                                 "year" = "year"), 
                                    .init = NULL))
  # C. Differences
  CSP_predict = CSP_predict |> mutate(difference = staff - predict_sfa, differencet = staff - predict_sfat,
                                      differencez = staff - predict_sfaz, differencetz = staff - predict_sfatz,
                                      difference_perc_sfa = (staff - predict_sfa)/predict_sfa,
                                      difference_perc_sfat = (staff - predict_sfat)/predict_sfat,
                                      difference_perc_sfaz = (staff - predict_sfaz)/predict_sfaz,
                                      difference_perc_sfatz = (staff - predict_sfatz)/predict_sfatz)
  write.csv(CSP_predict, "CSP_predict.csv", row.names = FALSE)
  
  CSP_predict_summary = CSP_predict |> group_by(unit) |>
    summarise( staff = mean(staff), predict_sfa = mean(predict_sfa), difference = mean(difference),
              differencez = mean(differencez), difference_perc_sfa = differencez/mean(predict_sfa),
              difference_perc_sfaz = differencez/mean(predict_sfaz))
  write.csv(CSP_predict_summary, "CSP_predict_summary.csv", row.names = FALSE)
  
  ## 9.2 Efficiency --------------------------------------------------------
  # A. Efficiency tables
  # SFA
  efficiencies_sfa = data.frame(efficiency = efficiencies(sfrontd, newdata = CSP_panel),
                                unit = rownames(efficiencies(sfrontd, newdata = CSP_panel))) |>
    rename("efficiency_sfa" = "efficiency")
  # SFA Trans
  efficiencies_sfat = data.frame(efficiency = efficiencies(sfronttd, newdata = CSP_panel),
                                 unit = rownames(efficiencies(sfronttd, newdata = CSP_panel))) |>
    rename("efficiency_sfat" = "efficiency")
  # SFA Z
  efficiencies_sfaz = data.frame(unit = rownames(efficiencies(sfrontzd, newdata = CSP_panel)),
                                 efficiency = efficiencies(sfrontzd, newdata = CSP_panel)) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_z") |>
    mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
  
  # SFA TRANS Z
  efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttzd, newdata = CSP_panel),
                                  unit = rownames(efficiencies(sfronttzd, newdata = CSP_panel))) |>
    pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                   "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                 names_to = "year", values_to = "efficiency_sfa_tz") |>
    mutate( year = str_remove(year, ".*\\."))
  
  # B. Join all efficiency tables
  list_df = list(efficiencies_sfa, efficiencies_sfat,
                 efficiencies_sfaz, efficiencies_sfatz)
  
  CSP_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                       by = c("unit" = "unit", "year" = "year"))
  CSP_effi = bind_rows(CSP_effi, full_join(efficiencies_sfa,efficiencies_sfat), by = c("unit" = "unit"))
  
  write.csv(CSP_effi, "CSP_effi.csv", row.names = FALSE)
  
# §. Tests --------------------------------------------------------------
warnings()  
setwd("../../..")
###### END #####
rm(list=ls())
