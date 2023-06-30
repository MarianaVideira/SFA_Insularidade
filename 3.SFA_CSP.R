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

# I. SFA Portugal Continent for Operational Costs -------------------------
  ## 1. Data and Functional forms -----------------------------------------
  load("0.DataBase/CSP.RData")
  dir.create("3.SFA_outputs")
  setwd("3.SFA_outputs")
  dir.create("CSP")
  setwd("CSP")
  dir.create("1. Portugal Frontier (staff)")
  setwd("1. Portugal Frontier (staff)")
  
    ### A. Select variables and test correlation ----------------------------
  CSP_1 = CSP |> select(aces, year, azo, staff, app, 
                        n_polos, prop_nofam_MF, prop_fem, prop_age_0_4,
                        prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
    drop_na()
  
  corr_table = CSP_1 |> 
    select(staff, app, n_polos, prop_nofam_MF, prop_fem,
           prop_age_0_4, prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
    cor()
  
  # Discard volume variables that have high correlation 
  #(choosen the scale variable will capture the effects of the others)
  CSP_0 = CSP |> select(aces, year, azo, staff, app, n_polos, prop_nofam_MF, 
                        prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                        prop_age_65_74, prop_age_75_hig) |> 
    filter(azo == 0) |>
    drop_na()
      
    ### B. Panel data -----------------------------------------------------
    CSP_panel = pdata.frame(CSP_0, c("aces", "year"))
    
    ### C. Define functional forms ------------------------------------------
    form = log(staff) ~ log(app) + log(prop_age_45_64)+ log(prop_age_75_hig)
    formt = log(staff) ~ log(app) + I((log(app)^2)/2) + log(prop_age_45_64)+ log(prop_age_75_hig) 
    
    # Firm level inefficiencies models BC95
    # mean inefficiency of u_it is "determined" by factors z_it
    formz = log(staff) ~ log(app) + log(n_polos) | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
    formtz = log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem
    
  ## 2. Staff Cost Function: Cross section --------------------------------
    ### 2.1 Estimation Portugal Continent 2016-2018 -----------------------
      #### A. Cobb-Douglas (form) ------------------------------------
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
      
      #### B. Trans log (formt) -------------------------------------------------
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
    
      #### C. Cobb-Douglas with z_it (formz) -------------------------------------
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
    
  
    ## 2.2 Residuals Check: Portugal Continent ----------------------------
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
      
  ## 3. Operational Cost function: Panel Data -----------------------------
    ### 3.3 Estimation: time invariant inefficiencies ---------------------
      #### A. Cobb-Douglas (form) ----------------------------------------------
      sfront = sfa( form,  
                    data = CSP_panel,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
      summary(sfront, extraPar = TRUE)
    
      #### B. Trans log (formt) -------------------------------------------------
      sfrontt = sfa( formt,  
                     data = CSP_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfrontt,extraPar = TRUE)
      class(summary(sfrontt))
    
      #### C. Cobb-Douglas with z_it (formz) -------------------------------------
      sfrontz = sfa( formz,  
                     data = CSP_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfrontz, extraPar = TRUE)
    
      #### D. Trans log with z_it (formtz) --------------------------------------
      sfronttz = sfa( formtz,  
                      data = CSP_panel,
                      ineffDecrease = F, # FALSE for cost function and TRUE for production
                      truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                      timeEffect = FALSE, # time is allowed to have an effect on efficiency
                      printIter = 1 )
      summary(sfronttz, extraPar = TRUE)
    
  ### 4. Tables for Panel Data --------------------------------------------
    ### A. BC92 ---------------------------------------------------------------
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
    
    ### B. BC95 -------------------------------------------------------------
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
  
    
  ## 5. Azores Prediction: Continent Frontier -----------------------------
    ## 5.1 Prediction Portugal Continent ----------------------------------
    CSP_predict = CSP_panel |> select(aces, year, staff)
    
    # A. prediction tables
    # SFA
    prediction_sfa = data.frame(predict = predict(sfront, newdata = CSP_panel),
                                aces = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfa" = "predict")
    
    # Trans
    prediction_sfat = data.frame(predict = predict(sfrontt, newdata = CSP_panel),
                                 aces = rownames(data.frame(predict(sfrontt, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfat" = "predict")
    # SFAZ
    prediction_sfaz = data.frame(predict = predict(sfrontz, newdata = CSP_panel),
                                 aces = rownames(data.frame(predict(sfrontz, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfaz" = "predict")
    
    # SFAZ Trans
    prediction_sfatz = data.frame(predict = predict(sfronttz, newdata = CSP_panel),
                                  aces = rownames(data.frame(predict(sfronttz, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfatz" = "predict")
    
    # B. Join all data frames 
    list_df = list(CSP_predict, prediction_sfa,
                   prediction_sfat, prediction_sfaz, prediction_sfatz)
    
    CSP_predict = list_df |> 
      reduce(function(x, y) full_join(x, y, by = c("aces" = "aces",
                                                   "year" = "year"), 
                                      .init = NULL))
    
    ## 5.2 Prediction Azores Island -----------------------------------------
    CSP_panel_a = CSP |> select(aces, year, azo, staff, app, n_polos, prop_nofam_MF, 
                          prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                          prop_age_65_74, prop_age_75_hig) |> 
      filter(azo == 0) |> drop_na() |> pdata.frame(c("aces", "year"))
    
    CSP_predict_a = CSP_panel_a |> select(aces, year, staff) 
    
    # A. prediction tables
    # SFA
    prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSP_panel_a)),
                                aces = rownames(data.frame(predict(sfront, newdata = CSP_panel_a)))) |>
      separate("aces", c("aces", "year"),sep = "-")
    
    # Trans
    prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSP_panel_a)),
                                 aces = rownames(data.frame(predict(sfrontt, newdata = CSP_panel_a)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predictt" = "predict")
    
    # SFAZ 
    prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSP_panel_a)),
                                 aces = rownames(data.frame(predict(sfrontz, newdata = CSP_panel_a)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predictz" = "predict")
    
    # SFAZ Trans
    prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSP_panel_a)),
                                  aces = rownames(data.frame(predict(sfronttz, newdata = CSP_panel_a)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predicttz" = "predict")
    
    # B. Join all data frames 
    list_df = list(CSP_predict_a, prediction_sfa,
                   prediction_sfat, prediction_sfaz, prediction_sfatz)
    
    CSP_predict_a = list_df |> 
      reduce(function(x, y) full_join(x, y, by = c("aces" = "aces",
                                                   "year" = "year"), 
                                      .init = NULL))
    # C. Differences
    # These differences are too high
    CSP_predict_a = CSP_predict_a |> mutate(difference = staff - predict, differencet = staff - predictt,
                                            differencez = staff - predictz, differencetz = staff - predicttz)
    
    ## 5.5 Prediction Azores and Portugal ------------------------------------
    CSP_panel = CSP |> select(aces, year, azo, staff, app, n_polos, prop_nofam_MF, 
                                prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                                prop_age_65_74, prop_age_75_hig) |> 
      drop_na() |> pdata.frame(c("aces", "year"))
    
    CSP_predict = CSP_panel |> select(aces, year, staff,azo)
    
    # A. prediction tables
    prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSP_panel)),
                                aces = rownames(data.frame(predict(sfront, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-")
    prediction_sfat = data.frame(predict = exp(predict(sfrontt, newdata = CSP_panel)),
                                 aces = rownames(data.frame(predict(sfrontt, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predictt" = "predict")
    prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSP_panel)),
                                 aces = rownames(data.frame(predict(sfrontz, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predictz" = "predict")
    prediction_sfatz = data.frame(predict = exp(predict(sfronttz, newdata = CSP_panel)),
                                  aces = rownames(data.frame(predict(sfronttz, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predicttz" = "predict")
    
    # B. Join all data frames 
    list_df = list(CSP_predict, prediction_sfa,
                   prediction_sfat, prediction_sfaz, prediction_sfatz)
    
    CSP_predict = list_df |> 
      reduce(function(x, y) full_join(x, y, by = c("aces" = "aces",
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
    
    ## 5.6 Efficiency Azores and Portugal ------------------------------------
    # A. Efficiency tables
    efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSP_panel),
                                  aces = rownames(efficiencies(sfront, newdata = CSP_panel))) |>
      rename("efficiency_sfa" = "efficiency")
    
    efficiencies_sfat = data.frame(efficiency = efficiencies(sfrontt, newdata = CSP_panel),
                                   aces = rownames(efficiencies(sfrontt, newdata = CSP_panel))) |>
      rename("efficiency_sfat" = "efficiency")
    
    efficiencies_sfaz = data.frame(aces = rownames(efficiencies(sfrontz, newdata = CSP_panel)),
                                   efficiency = efficiencies(sfrontz, newdata = CSP_panel)) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_z") |>
      mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
    
    efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttz, newdata = CSP_panel),
                                    aces = rownames(efficiencies(sfronttz, newdata = CSP_panel))) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_tz") |>
      mutate( year = str_remove(year, ".*\\."))
    
    # B. Join all efficiency tables
    list_df = list(efficiencies_sfa, efficiencies_sfat,
                   efficiencies_sfaz, efficiencies_sfatz)
    
    CSP_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                         by = c("aces" = "aces", "year" = "year"))
    CSP_effi = bind_rows(CSP_effi, full_join(efficiencies_sfa,efficiencies_sfat), 
                         by = c("aces" = "aces"))
    
    write.csv(CSP_effi, "CSP_effi.csv", row.names = FALSE)
    
    # Efficiencies summary with SFA Z
    CSP_effi_sum = data.frame(efficiency = efficiencies(sfronttz, newdata = CSP_panel),
                         aces = rownames(efficiencies(sfronttz, newdata = CSP_panel))) |>
      pivot_longer(c("efficiency.2015", "efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_tz") |> drop_na() |> 
      mutate( year = str_remove(year, ".*\\.")) |> group_by(aces) |> summarise(mean_ef = mean(efficiency_sfa_tz))
    
    write.csv(CSP_effi_sum, "CSP_effi_sum.csv", row.names = FALSE)
# II.  SFA Portugal & Islands for Operational Costs -----------------------
  ## 1. Data and Functional forms ----------------------------------------
    rm(list = setdiff(ls(), c("CSP", "sfrontz")))
    setwd("..")
    dir.create("2. Frontier w Azores Dummy (staff)")
    setwd("2. Frontier w Azores Dummy (staff)")    
    ### A. Select variables and test correlation ----------------------------
    CSP_0 = CSP |> select(aces, year, azo, staff, app,
                          n_polos, prop_nofam_MF, prop_fem, prop_age_0_4,
                          prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
      drop_na()
    
    ### B. Panel data -----------------------------------------------------
    CSP_panel = pdata.frame(CSP_0, c("aces", "year"))
    
    ### C. Define functional forms ------------------------------------------
    # form = log(staff) ~ log(app) + log(n_polos) + azo + log(prop_age_45_64)+ log(prop_age_75_hig) Number os polos is not significant
    form = log(staff) ~ log(app) + log(n_polos) + azo 
    formt = log(staff) ~ log(app) + I((log(app)^2)/2) + n_polos + azo 
    
    # Firm level inefficiencies models BC95
    # mean inefficiency of u_it is "determined" by factors z_it
    # formz = log(staff) ~ log(app)  + log(n_polos) + azo | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
    formz = log(staff) ~ log(app)  + log(n_polos) + azo | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
    formtz = log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) +azo | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig  + prop_age_0_4 
    
  ## 2. Operational Cost function: Panel Data --------------------------------
    ### 2.3 Estimation: time invariant inefficiencies ------------------------
      #### A. Cobb-Douglas (form) ----------------------------------------------
      sfrontd = sfa( form,  
                    data = CSP_panel,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
      summary(sfrontd, extraPar = TRUE)
      
      #### B. Trans log (formt) -------------------------------------------------
      sfronttd = sfa( formt,  
                     data = CSP_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfronttd,extraPar = TRUE)
      class(summary(sfronttd))
      
      #### C. Cobb-Douglas with z_it (formz) -------------------------------------
      sfrontzd = sfa( formz,  
                     data = CSP_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfrontzd, extraPar = TRUE)
      
      #### D. Trans log with z_it (formtz) --------------------------------------
      sfronttzd = sfa( formtz,  
                      data = CSP_panel,
                      ineffDecrease = F, # FALSE for cost function and TRUE for production
                      truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                      timeEffect = FALSE, # time is allowed to have an effect on efficiency
                      printIter = 1 )
      summary(sfronttzd, extraPar = TRUE)
      
  ## 3. Tables for Panel Data ---------------------------------------------
    ### A. BC92 ---------------------------------------------------------------
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
                                   "$\\log$ nº of polos", "Azores"))
  
    ### B. BC95 -------------------------------------------------------------
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
                                  "$\\log$ nº of polos", "Azores","Z Constant","proportion without FD",
                                   "proportion ages 45-64", "proportion ages 75+",
                                   "proportion ages 00-04"))
    
    ### C. BC95: dummy --------------------------------------------------------
    # Creative approach to export SFA tables (since normal packages don't support format)
    # save the coefficients and p-v as vectors:
    c1 = as.vector(coef(sfrontz))
    c2 = as.vector(coef(sfrontzd))
    
    pv1 = as.vector(coef(summary(sfrontz))[,4])
    pv2 = as.vector(coef(summary(sfrontzd))[,4])
    
    # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
    g1 = round(as.vector(coef(summary(sfrontd))[nrow(coef(summary(sfrontd))),1]), 3)
    g2 = round(as.vector(coef(summary(sfrontzd))[nrow(coef(summary(sfrontzd))),1]),3)

    nrow(coef(summary(sfrontzd)))
    ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
    ll2 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
    
    me1 = round(summary(sfrontz)$efficMean,3)
    me2 = round(summary(sfrontzd)$efficMean,3)
    #run the same specifications using a linear model 
    # (remember to change the "|" before the z variables to a "+" in the linear model):
    lm1 = lm(log(staff) ~ log(app) + log(n_polos) + log(prop_age_45_64) + prop_fem +prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)
    lm2 = lm(log(staff) ~ log(app) + log(n_polos) + azo + log(prop_age_45_64) + prop_fem + prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)
    
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
              covariate.labels = c("Constant", "$\\log \\text{appointments}$", "$\\log$ nº of polos",
                                   "Azores",
                                   "Z Constant", "Proportion without FD", "Proportion ages 75+", "Proportion ages 0-4"))
  
    ### D. BC92/95 --------------------------------------------------------
    # Creative approach to export SFA tables (since normal packages don't support format)
    # save the coefficients and p-v as vectors:
    c1 = as.vector(coef(sfrontd))
    c2 = as.vector(coef(sfrontzd))
    
    pv1 = as.vector(coef(summary(sfrontd))[,4])
    pv2 = as.vector(coef(summary(sfrontzd))[,4])
    
    # save gamma parameter, log-likelihood and mean efficiency
    g1 = round(as.vector(coef(summary(sfrontd))[nrow(coef(summary(sfrontd))),1]), 3)
    g2 = round(as.vector(coef(summary(sfrontzd))[nrow(coef(summary(sfrontzd))),1]),3)
    
    ll1 = round(as.numeric(logLik(sfrontd, which = "mle")),3)
    ll2 = round(as.numeric(logLik(sfrontzd, which = "mle")),3)
    
    me1 = round(summary(sfrontd)$efficMean,3)
    me2 = round(summary(sfrontzd)$efficMean,3)
    
    #run the same specifications using a linear model 
    # (remember to change the "|" before the z variables to a "+" in the linear model):
    lm1 = lm(form, data = CSP_panel)
    lm2 = lm(log(staff) ~ log(app) + log(n_polos) + azo + log(prop_age_45_64)  + prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)
    summary(sfrontzd)
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
              covariate.labels = c("Constant","$\\log \\text{Appointments}$", "$\\log \\text{nº of polos}$",
                                   "Azores","Z Constant","proportion without FD",
                                   "proportion ages 75+", "proportion ages 00-04"))
    
  ## 4. Azores Prediction: Dummy Frontier ---------------------------------
    ### 4.1 Prediction ----------------------------------------------------
    CSP_predict = CSP_panel |> select(aces, year, staff, azo, app, azo)
    
    # A. prediction tables
    prediction_sfa = data.frame(predict = exp(predict(sfrontd, newdata = CSP_panel)),
                                aces = rownames(data.frame(predict(sfrontd, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfa" = "predict")
    prediction_sfat = data.frame(predict = exp(predict(sfronttd, newdata = CSP_panel)),
                                 aces = rownames(data.frame(predict(sfronttd, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfat" = "predict")
    prediction_sfaz = data.frame(predict = exp(predict(sfrontzd, newdata = CSP_panel)),
                                 aces = rownames(data.frame(predict(sfronttd, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfaz" = "predict")
    prediction_sfatz = data.frame(predict = exp(predict(sfronttzd, newdata = CSP_panel)),
                                  aces = rownames(data.frame(predict(sfronttzd, newdata = CSP_panel)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfatz" = "predict")
    
    # B. Join all data frames 
    list_df = list(CSP_predict, prediction_sfa,
                   prediction_sfat, prediction_sfaz, prediction_sfatz)
    
    CSP_predict = list_df |> 
      reduce(function(x, y) full_join(x, y, by = c("aces" = "aces",
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
    
    CSP_predict_summary = CSP_predict |> group_by(aces) |>
      summarise( staff = mean(staff), predict_sfa = mean(predict_sfa), difference = mean(difference),
                 differencez = mean(differencez), difference_perc_sfa = differencez/mean(predict_sfa),
                 difference_perc_sfaz = differencez/mean(predict_sfaz))
    write.csv(CSP_predict_summary, "CSP_predict_summary.csv", row.names = FALSE)
    
    ### 4.2 Efficiency ----------------------------------------------------
    # A. Efficiency tables
    # SFA
    efficiencies_sfa = data.frame(efficiency = efficiencies(sfrontd, newdata = CSP_panel),
                                  aces = rownames(efficiencies(sfrontd, newdata = CSP_panel))) |>
      rename("efficiency_sfa" = "efficiency")
    # SFA Trans
    efficiencies_sfat = data.frame(efficiency = efficiencies(sfronttd, newdata = CSP_panel),
                                   aces = rownames(efficiencies(sfronttd, newdata = CSP_panel))) |>
      rename("efficiency_sfat" = "efficiency")
    # SFA Z
    efficiencies_sfaz = data.frame(aces = rownames(efficiencies(sfrontzd, newdata = CSP_panel)),
                                   efficiency = efficiencies(sfrontzd, newdata = CSP_panel)) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_z") |>
      mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
    
    # SFA TRANS Z
    efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttzd, newdata = CSP_panel),
                                    aces = rownames(efficiencies(sfronttzd, newdata = CSP_panel))) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_tz") |>
      mutate( year = str_remove(year, ".*\\."))
    
    # B. Join all efficiency tables
    list_df = list(efficiencies_sfa, efficiencies_sfat,
                   efficiencies_sfaz, efficiencies_sfatz)
    
    CSP_effi = full_join(efficiencies_sfaz, efficiencies_sfatz, 
                         by = c("aces" = "aces", "year" = "year"))
    CSP_effi = bind_rows(CSP_effi, full_join(efficiencies_sfa,efficiencies_sfat), by = c("aces" = "aces"))
    
    write.csv(CSP_effi, "CSP_effi.csv", row.names = FALSE)
    
    ### 4.3 Over cost Estimation ------------------------------------------
    CSP_predict = full_join(CSP_predict, efficiencies_sfa, by = "aces")
    CSP_predict = full_join(CSP_predict, efficiencies_sfaz, by = c("aces","year"))
    
    # A. Mean efficiency Portugal and Azores
    eff_0 = CSP_predict |> filter(azo == 0) |> summarise(d = mean(efficiency_sfa)) |> 
      pull(d) 
    effz_0 = CSP_predict |> filter(azo == 0) |> summarise(d = mean(efficiency_sfa_z)) |> 
      pull(d)
    
    innef_P = 1- eff_0
    innefz_P = 1-effz_0
    
    # B. Tidy the Data frame
    CSP_predict_1 = CSP_predict |>
      filter(azo == 1) |>
      select(year, aces, staff, predict_sfa, predict_sfaz, difference, differencez,
             efficiency_sfa, efficiency_sfa_z)
    
    # C. Table with over costs
    CSP_predict_1 = CSP_predict |>
      mutate(Inef_Cost_Cont = innef_P * predict_sfa, 
             Inef_Cost_Cont_z = innefz_P * predict_sfaz,
             Innef_Cost_Isl = (1-efficiency_sfa)*predict_sfa,
             Innef_Cost_Isl_z = (1-efficiency_sfa_z)*predict_sfaz,
             overcost =  Innef_Cost_Isl- Inef_Cost_Cont,
             overcostz = Innef_Cost_Isl_z - Inef_Cost_Cont_z) # There is an issue with this method: efficiency açores is higher -> can't calculate overcost
    
  ## 5. Graph -------------------------------------------------------------
    
    ggplot(data = filter(CSP_predict, year == 2018), aes(x = app, y = staff, color = azo)) +
      geom_point() +  # Data points
      geom_line(data = filter(CSP_predict, azo == 0, year == 2018), aes(y = predict_sfaz), linetype = "dashed", color = "blue") +
      geom_line(data = filter(CSP_predict, azo == 1, year == 2018), aes(y = predict_sfaz), linetype = "dashed", color = "pink") +
      labs(x = "Consultas", y = "Custos com Pessoal", title = "Fronteira Estocástica", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2018." ) +
      scale_color_manual(values = c("1" = "lightpink", "0" = "lightgreen"),
                         labels = c("Portugal Continental", "Açores"),
                         name = "")
    ggsave(filename = "CDgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
    
# III.  SFA Portugal & Azores Units for Operational ACSS ------------------
  ## 1. Data and Functional forms ----------------------------------------
    rm(list = setdiff(ls(), c("CSP")))
    setwd("..")
    dir.create("3. Portugal Frontier and Overcost Islands (staff)")
    setwd("3. Portugal Frontier and Overcost Islands (staff)")    
    ### A. Select variables and test correlation ----------------------------
    CSP_panel = CSP |> 
      filter(azo== 0) |>
      select(aces, year, azo, staff, app, n_polos, prop_nofam_MF, 
             prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
             prop_age_65_74, prop_age_75_hig) |>
      drop_na() 
    
    # Double-check correlation
    corr_table = CSP_panel |> 
      select(staff, app, n_polos, prop_nofam_MF, prop_fem,
             prop_age_0_4, prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
      cor()
    
    ### B. Panel data -----------------------------------------------------
    CSP_panel = pdata.frame(CSP_panel, c("aces", "year"))
    
    ### C. Define functional forms ------------------------------------------
    form = log(staff) ~ log(app) + log(n_polos)  
    formz = log(staff) ~ log(app)  + log(n_polos) | prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 
    
  ## 2. Operational Cost function: Panel Data --------------------------------
    ### 2.3 Estimation: time invariant inefficiencies ------------------------
    #### A. Cobb-Douglas (form) ----------------------------------------------
    sfront = sfa( form,  
                   data = CSP_panel,
                   ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    #### B. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontz = sfa( formz,  
                    data = CSP_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
    summary(sfrontz, extraPar = TRUE)
    
  ## 3. Tables for Panel Data ---------------------------------------------
    ### D. BC92/95 --------------------------------------------------------
    # Creative approach to export SFA tables (since normal packages don't support format)
    # save the coefficients and p-v as vectors:
    c1 = as.vector(coef(sfront))
    c2 = as.vector(coef(sfrontz))
    
    pv1 = as.vector(coef(summary(sfront))[,4])
    pv2 = as.vector(coef(summary(sfrontz))[,4])
    
    # save gamma parameter, log-likelihood and mean efficiency
    g1 = round(as.vector(coef(summary(sfront))[nrow(coef(summary(sfront))),1]), 3)
    g2 = round(as.vector(coef(summary(sfrontz))[nrow(coef(summary(sfrontz))),1]),3)
    
    ll1 = round(as.numeric(logLik(sfront, which = "mle")),3)
    ll2 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
    
    me1 = round(summary(sfront)$efficMean,3)
    me2 = round(summary(sfrontz)$efficMean,3)
    
    #run the same specifications using a linear model 
    # (remember to change the "|" before the z variables to a "+" in the linear model):
    lm1 = lm(form, data = CSP_panel)
    lm2 = lm(log(staff) ~ log(app) + log(n_polos) + log(prop_age_45_64)  + prop_nofam_MF + prop_age_75_hig  + prop_age_0_4 , data = CSP_panel)

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
              covariate.labels = c("Constant","$\\log \\text{Appointments}$", "$\\log \\text{nº of polos}$",
                                   "Z Constant","proportion without FD",
                                   "proportion ages 75+", "proportion ages 00-04"))
    
  ## 4. Azores Prediction: Dummy Frontier ---------------------------------
    ### 4.1 Prediction ----------------------------------------------------
    CSP_predict = CSP |> pdata.frame( c("aces", "year"))
    
    # A. prediction tables
    # SFA 
    prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSP_predict)),
                                aces = rownames(data.frame(predict(sfront, newdata = CSP_predict)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfa" = "predict")
    # SFA Z
    prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSP_predict)),
                                 aces = rownames(data.frame(predict(sfrontz, newdata = CSP_predict)))) |>
      separate("aces", c("aces", "year"),sep = "-") |>
      rename("predict_sfaz" = "predict")
    
    # B. Join all data frames 
    list_df = list(CSP_predict, prediction_sfa, prediction_sfaz)
    
    CSP_predict = list_df |> 
      reduce(function(x, y) full_join(x, y, by = c("aces" = "aces",
                                                   "year" = "year"), 
                                      .init = NULL))
    
    # C. Differences
    CSP_predict = CSP_predict |> mutate(difference = staff - predict_sfa, differencez = staff - predict_sfaz, 
                                        difference_perc_sfa = (staff - predict_sfa)/predict_sfa,
                                        difference_perc_sfaz = (staff - predict_sfaz)/predict_sfaz)
    
    
    CSP_predict_summary = CSP_predict |> group_by(aces) |>
      summarise( staff = mean(staff), predict_sfa = mean(predict_sfa), difference = mean(difference),
                 differencez = mean(differencez), difference_perc_sfa = differencez/mean(predict_sfa),
                 difference_perc_sfaz = differencez/mean(predict_sfaz))
    write.csv(CSP_predict_summary, "CSP_predict_summary.csv", row.names = FALSE)
    
    ### 4.2 Efficiency ----------------------------------------------------
    # A. Efficiency tables
    # SFA
    efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSP_predict),
                                  aces = rownames(efficiencies(sfront, newdata = CSP_predict))) |>
      rename("efficiency_sfa" = "efficiency")
    
    # SFA Z
    efficiencies_sfaz = data.frame(aces = rownames(efficiencies(sfrontz, newdata = CSP_predict)),
                                   efficiency = efficiencies(sfrontz, newdata = CSP_predict)) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_z") |>
      mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
    
    # B. Join all efficiency tables
    CSP_predict = full_join(CSP_predict, efficiencies_sfaz, 
                            by = c("aces" = "aces", "year" = "year"))
    CSP_predict = full_join(CSP_predict,efficiencies_sfa, by = c("aces" = "aces"))
    
    ### 4.3 Over cost Estimation ------------------------------------------
    
    # A. Mean efficiency Portugal and Azores
    innef_P = 1-round(summary(sfront)$efficMean,3)
    innefz_P = 1-round(summary(sfrontz)$efficMean,3)
    
    # B. Table with over costs
    CSP_predict= CSP_predict |>
        mutate(Inef_Cost_Cont = innef_P * predict_sfa, 
               Inef_Cost_Cont_z = innefz_P * predict_sfaz,
               Innef_Cost_Isl = (1-efficiency_sfa)*predict_sfa,
               Innef_Cost_Isl_z = (1-efficiency_sfa_z)*predict_sfaz,
               overcost =  Innef_Cost_Isl- Inef_Cost_Cont,
               overcostz = Innef_Cost_Isl_z - Inef_Cost_Cont_z)
    
        Azo_CSP_predict = CSP_predict |> filter(azo == 1) |>
        group_by(year, aces) |>
        summarise( staff = sum(staff), 
                  Inef_Cost_Cont = sum (Inef_Cost_Cont), Inef_Cost_Cont_z = sum(Inef_Cost_Cont_z),
                  Innef_Cost_Isl = sum(Innef_Cost_Isl), Innef_Cost_Isl_z = sum(Innef_Cost_Isl_z),
                  overcost = sum(overcost), overcostz = sum(overcostz)) |>
          distinct()
      
    
    write.csv(Azo_CSP_predict, "CSP_predict.csv", row.names = FALSE)
    # Save the summary table as a PDF file
    pdf(paste0("CSH_predict_overcost_azores.pdf"), width = 20, height = 25)
    grid.table(Azo_CSP_predict, rows = NULL)
    dev.off()
    
  ## 5. Graph -------------------------------------------------------------
    CSP_predict = CSP_predict |> mutate(new_predict = predict_sfa*me1)
    CSP_predict = CSP_predict |> mutate(new_predict_2 = predict_sfa*efficiency_sfa)
    CSP_predict = CSP_predict |> mutate(pred_overcost = predict_sfa + overcost)
    
    ggplot(data = filter(CSP_predict, year == 2018), aes(x = app, y = staff, color = azo)) +
      geom_point(alpha = 0.5) +  # Data points
      xlim(0, 2.6e+05) +
      ylim(0, 1.5e+07) +
      geom_line(data = filter(CSP_predict, azo == 0, year == 2018), aes(y = predict_sfa), linetype = "dashed", color = "lightgreen") +
      geom_point(data = filter(CSP_predict, azo == 1, year == 2018), aes(x = app, y = pred_overcost), color = "red", shape = 4, size = 3) +
      geom_segment(data = filter(CSP_predict, azo == 1, year == 2018), aes(x = app, y = predict_sfa, xend = app, yend = pred_overcost), color = "red", linetype = "dotted") +
      geom_segment(data = filter(CSP_predict, azo == 1, year == 2018), aes(x = app, y = pred_overcost, xend = app, yend = staff), color = "blue", linetype = "dotted") +
      geom_point(data = filter(CSP_predict, azo == 1, year == 2018), aes(x = app, y = predict_sfa), color = "red") +
      labs(x = "Consultas", y = "Custos com Pessoal", title = "Fronteira Estocástica", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2018." ) +
      scale_color_manual(values = c("1" = "lightblue", "0" = "lightgreen"),
                         labels = c("Portugal Continental", "Açores"),
                         name = "")
      
    ggsave(filename = "CDgraph_eff.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
    
    # write prediction table
    CSP_predict = CSP_predict |> select(aces, year, staff, predict_sfa, predict_sfaz, difference, differencez, 
                                        difference_perc_sfa, difference_perc_sfaz, efficiency_sfa,
                                        efficiency_sfa_z)
    write.csv(CSP_predict, "CSP_pred_eff.csv", row.names = FALSE)
    # Save the summary table as a PDF file
    pdf(paste0("CSP_predict_overcost.pdf"), width = 10, height = 4)
    grid.table(CSP_predict, rows = NULL)
    dev.off()
# IV. Tests -----------------------------------------------------------------
warnings()  
setwd("../../..")
###### END #####
rm(list=ls())
