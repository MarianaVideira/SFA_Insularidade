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
library(gridExtra)

# I. SFA Portugal Continent for Operational Costs -------------------------
  ## 1. Data --------------------------------------------------------------
  load("0.DataBase/CSH.RData")
    dir.create("3.SFA_outputs")
    setwd("3.SFA_outputs")
    dir.create("CSH")
    setwd("CSH")
    dir.create("1. Portugal Frontier (operational)")
    setwd("1. Portugal Frontier (operational)")
    ### A. Select variables and test correlation --------------------------
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
    ### C. Define functional forms ----------------------------------------
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
    
  ## 2. Operational Cost function : Continent -----------------------------
    ### 2.1 Estimation: time invariant inefficiencies ------------------------
      #### A. Cobb-Douglas (form) -------------------------------------------
      sfront = sfa( form,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
      summary(sfront, extraPar = TRUE)
    
      #### B. Trans log (formt) ---------------------------------------------
      sfrontt = sfa( formt,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
      summary(sfrontt)
      
      #### C. Cobb-Douglas with z_it (formz) --------------------------------
      sfrontz = sfa( formz,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfrontz)
      
      #### D. Trans log with z_it (formtz) ----------------------------------
      sfronttz = sfa( formtz,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfronttz)
      
      #### E. Cobb-Douglas with FE by region (form_r) -----------------------
      sfrontr = sfa( form_r,  
                      data = CSH_panel,
                      ineffDecrease = F, # FALSE for cost function and TRUE for production
                      truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                      timeEffect = FALSE, # time is allowed to have an effect on efficiency
                      printIter = 1 )
      summary(sfrontr)
      
      #### F. Trans Log with FE by region (form_r) --------------------------
      sfronttr = sfa( formt_r,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfronttr)
  
  ## 3. Tables ------------------------------------------------------------
    #### A. BC92 ------------------------------------------------------------
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
  
    
    #### B. BC95 ------------------------------------------------------------
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
    
  ## 4. Azores Prediction: Continent Frontier -----------------------------
    ### 4.1 Prediction Portugal Continent -----------------------------------
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
    
    ### 4.2 Prediction Azores Island ----------------------------------------
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
    
    ### 4.3 Efficiency Portugal Continent -----------------------------------
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
    
    ### 4.4 Efficiency Azores Island ----------------------------------------
    
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
    
    ### 4.5 Prediction Azores and Portugal ----------------------------------
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
    
    ### 4.6 Efficiency Azores and Portugal ----------------------------------
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
    
  
# II.  SFA Portugal & Islands for Operational Costs -----------------------
  ## 1. Data --------------------------------------------------------------
  rm(list = setdiff(ls(), c("CSH", "sfrontz")))
  setwd("..")
    dir.create("2. Frontier w Azores Dummy (operational)")
    setwd("2. Frontier w Azores Dummy (operational)")
      #### A. Select variables and test correlation -------------------------
      CSH_panel = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                             surg, wait_scheduled_surg) |> 
        drop_na() |>
        pdata.frame(c("hospital", "year"))
    
    ## 1.1 Define functional forms ----------------------------------------
    form = log(operational) ~ log(in_days) + log(RO) + azo
    formt = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + azo
    
    # Firm level inefficiencies models BC95
    # mean inefficiency of u_it is "determined" by factors z_it
    formz = log(operational) ~ log(in_days) + log(surg) + azo| wait_scheduled_surg
    formtz = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + azo| RO + wait_scheduled_surg
    
  ## 2. Operational Cost function: Azores Dummy ---------------------------
    #(choosen the scale variable will capture the effects of the others)
    CSH_0 = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                          surg, urge, wait_scheduled_surg) |> 
      drop_na()
    
    ### 2.1 Estimation: time invariant inefficiencies --------------------
      #### A. Cobb-Douglas (form) ----------------------------------------
      sfrontd = sfa( form,  
                    data = CSH_panel,
                    ineffDecrease = F, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
      summary(sfrontd, extraPar = TRUE)
      
      #### B. Trans log (formt) ------------------------------------------
      sfronttd = sfa( formt,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfronttd)
      
      #### C. Cobb-Douglas with z_it (formz) ------------------------------
      sfrontzd = sfa( formz,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfrontzd)
      summary(sfrontz)
      
      #### D. Trans log with z_it (formtz) --------------------------------
      sfronttzd = sfa( formtz,  
                      data = CSH_panel,
                      ineffDecrease = F, # FALSE for cost function and TRUE for production
                      truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                      timeEffect = FALSE, # time is allowed to have an effect on efficiency
                      printIter = 1 )
      summary(sfronttzd)
      
  ## 3. Tables ------------------------------------------------------------
    ### A. BC92 -----------------------------------------------------------
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
    
    ### B. BC95 -----------------------------------------------------------
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
  
    ### B. BC95: w/ and wthout/ dummy -------------------------------------
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
  
  ## 4. Azores Prediction: Dummy Frontier ---------------------------------
    ### 4.1 Prediction ----------------------------------------------------
    CSH_predict = CSH_panel |> select(hospital, year, operational, azo, in_days)
    
    # A. prediction tables
    # SFA
    prediction_sfa = data.frame(predict = exp(predict(sfrontd, newdata = CSH_panel)),
                                hospital = rownames(data.frame(predict(sfrontd, newdata = CSH_panel)))) |>
      separate("hospital", c("hospital", "year"),sep = "-") |>
      rename("predict_sfa" = "predict")
    
    # SFA Trans
    prediction_sfat = data.frame(predict = exp(predict(sfronttd, newdata = CSH_panel)),
                                 hospital = rownames(data.frame(predict(sfrontd, newdata = CSH_panel)))) |>
      separate("hospital", c("hospital", "year"),sep = "-") |>
      rename("predict_sfat" = "predict")
    
    # SFA Z
    prediction_sfaz = data.frame(predict = exp(predict(sfrontzd, newdata = CSH_panel)),
                                 hospital = rownames(data.frame(predict(sfronttzd, newdata = CSH_panel)))) |>
      separate("hospital", c("hospital", "year"),sep = "-") |>
      rename("predict_sfaz" = "predict")
    
    # SFA Trans Z
    prediction_sfatz = data.frame(predict = exp(predict(sfronttzd, newdata = CSH_panel)),
                                  hospital = rownames(data.frame(predict(sfronttzd, newdata = CSH_panel)))) |>
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
    
  
    ### 4.2 Efficiency ----------------------------------------------------
    # A. Efficiency tables
    # SFA 
    efficiencies_sfa = data.frame(efficiency = efficiencies(sfrontd, newdata = CSH_panel),
                                  hospital = rownames(efficiencies(sfrontd, newdata = CSH_panel))) |>
      rename("efficiency_sfa" = "efficiency")
    
    # SFA Trans
    efficiencies_sfat = data.frame(efficiency = efficiencies(sfronttd, newdata = CSH_panel),
                                   hospital = rownames(efficiencies(sfronttd, newdata = CSH_panel))) |>
      rename("efficiency_sfat" = "efficiency")
    
    # SFA Z
    efficiencies_sfaz = data.frame(hospital = rownames(efficiencies(sfrontzd, newdata = CSH_panel)),
                                   efficiency = efficiencies(sfrontzd, newdata = CSH_panel)) |>
      pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                     "efficiency.2019", "efficiency.2020", "efficiency.2021", "efficiency.2022"),
                   names_to = "year", values_to = "efficiency_sfa_z") |>
       mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
      
    # SFA Trans
    efficiencies_sfatz = data.frame(efficiency = efficiencies(sfronttzd, newdata = CSH_panel),
                                    hospital = rownames(efficiencies(sfronttzd, newdata = CSH_panel))) |>
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
# III.  SFA Portugal & Azores H. Center for Operational ACSS --------------
  ## 1. Data --------------------------------------------------------------
    rm(list = setdiff(ls(), c("CSH")))
    setwd("..")
    dir.create("3. Frontier w Azores Hospital Centre (operational ACSS)")
    setwd("3. Frontier w Azores Hospital Centre (operational ACSS)")
    ### A. Select variables and test correlation --------------------------
      hosp_centre = CSH |> filter(azo == 1) |> 
        group_by(year) |>
        summarise(operational_ACSS = sum(operational_ACSS), in_days = sum(in_days), RO = mean(RO), 
                  surg = sum(surg), wait_scheduled_surg = sum(wait_scheduled_surg), 
                  aging = mean(aging), fem = mean(fem), den = sum(den),
                  pop = mean(pop)) |>
      distinct(year, .keep_all = TRUE) |>
      mutate(n_polos = 3, azo = 1, hospital = "hospital_azores") |>
      mutate(azo = as.factor(azo), year = as.numeric(year)) 

      # Panel for SFA analysis
      CSH_panel = CSH |> 
        filter(azo== 0) |>
        select(hospital, year,operational_ACSS, in_days, azo,
               surg, wait_scheduled_surg, n_polos, den, fem, aging) |>
        drop_na() 
        
      CSH_panel = pdata.frame(CSH_panel, c("hospital", "year"))
    
      #CSH_panel = bind_rows(CSH_panel, hosp_centre)
      
      corr_table = CSH_panel |> 
        select(operational_ACSS, in_days,
               surg, wait_scheduled_surg, n_polos, den, fem, aging) |> 
        cor()
      
    ### B Define functional forms -----------------------------------------
    form = log(operational_ACSS) ~ log(in_days) + log(surg) + log(wait_scheduled_surg) + n_polos
    formz = log(operational_ACSS) ~ log(in_days)  + log(surg) |  fem + den + aging + wait_scheduled_surg

  ## 2. Operational Cost function -----------------------------------------
    ### 2.1 Estimation: time invariant inefficiencies ---------------------
      #### A. Cobb-Douglas (form) ----------------------------------------
      sfront = sfa( form,  
                     data = CSH_panel,
                     ineffDecrease = F, # FALSE for cost function and TRUE for production
                     truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                     timeEffect = FALSE, # time is allowed to have an effect on efficiency
                     printIter = 1 )
      summary(sfront, extraPar = TRUE)
      
      #### B. Cobb-Douglas with z_it (formz) ------------------------------
      sfrontz = sfa( formz,  
                      data = CSH_panel,
                      ineffDecrease = F, # FALSE for cost function and TRUE for production
                      truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                      timeEffect = FALSE, # time is allowed to have an effect on efficiency
                      printIter = 1 )
      summary(sfrontz)
      
  ## 3. Table -------------------------------------------------------------
    # BC 92/95
    # Creative approach to export SFA tables (since normal packages don't support format)
    # save the coefficients and p-v as vectors:
    c1 = as.vector(coef(sfront))
    c2 = as.vector(coef(sfrontz))
    
    pv1 = as.vector(coef(summary(sfront))[,4])
    pv2 = as.vector(coef(summary(sfrontz))[,4])
    
    # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
    g1 = round(as.vector(coef(summary(sfront, extraPar = TRUE))[nrow(coef(summary(sfront))),1]), 3)
    g2 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[nrow(coef(summary(sfrontz))),1]),3)
     
    ll1 = round(as.numeric(logLik(sfront, which = "mle")),3)
    ll2 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
    
    me1 = round(summary(sfront)$efficMean,3)
    me2 = round(summary(sfrontz)$efficMean,3)
    
    #run the same specifications using a linear model 
    # (remember to change the "|" before the z variables to a "+" in the linear model):
    lm1 = lm(form, data = CSH_panel)
    lm2 = lm(log(operational_ACSS) ~ log(in_days)  + log(surg) + log(fem) + fem + den + aging + wait_scheduled_surg, data = CSH_panel)
    
    #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
    stargazer(lm1, lm2,
              coef = list(c1,c2), 
              se = list(pv1,pv2),
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
                                   "$\\log \\text{surgeries}$", "$\\log \\text{wait secheduled surgeries}$", "nº of hospitals",
                                   "$\\log$ Z Constant", "Proportion female", "Population density",
                                   "Aging index"))
    
  ## 4. Azores Prediction: Dummy Frontier ---------------------------------
    ### 4.1 Prediction HC -------------------------------------------------
    CSH_predict = bind_rows(hosp_centre, filter(CSH, azo == 0)) |> pdata.frame(c("hospital", "year"))
    
    CSH_predict = CSH_predict |> mutate( ifelse(hospital == "hospital_azores", 1, azo)) |>
      mutate(azo = as.factor(azo))
    
    hosp_centre = pdata.frame(hosp_centre, c("year","hospital"))
      #### A. prediction tables Azores --------------------------------------
      # SFA
      prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = hosp_centre)),
                                  year = rownames(data.frame(predict(sfront, newdata = hosp_centre))),
                                  hospital = "hospital_azores" ) |>
        mutate( year = str_remove(year, "-.*")) |>
        rename("predict_sfa" = "predict")
      
      # SFA Z
      prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = hosp_centre)),
                                   year = rownames(data.frame(predict(sfrontz, newdata = hosp_centre))),
                                   hospital = "hospital_azores" ) |>
        mutate( year = str_remove(year, "-.*")) |>
        rename("predict_sfaz" = "predict")
      
      #### B. Join all data frames Azores -----------------------------------
      list_df = list(hosp_centre,prediction_sfa, prediction_sfaz)
      
      CSH_predict_Azor = list_df |> 
        reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                     "year" = "year"), 
                                        .init = NULL))
      
      #### C. Prediction tables whole sample --------------------------------
      # SFA
      prediction_sfa = data.frame(predict = exp(predict(sfront, newdata = CSH_predict)),
                                  hospital = rownames(data.frame(predict(sfront, newdata = CSH_predict)))) |>
        separate("hospital", c("hospital", "year"),sep = "-") |>
        rename("predict_sfa" = "predict")
      # SFA Z
      prediction_sfaz = data.frame(predict = exp(predict(sfrontz, newdata = CSH_predict)),
                                   hospital = rownames(data.frame(predict(sfrontz, newdata = CSH_predict)))) |>
        separate("hospital", c("hospital", "year"),sep = "-") |>
        rename("predict_sfaz" = "predict")
      
      #### D. Join all data frames  ------------------------------------------
      list_df = list(CSH_predict,prediction_sfa, prediction_sfaz)
      
      CSH_predict = list_df |> 
        reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                     "year" = "year"), 
                                        .init = NULL))
      
      #### E. Differences Azores --------------------------------------------
      # These differences are too high
      CSH_predict_Azor = CSH_predict_Azor |> mutate(difference = operational_ACSS - predict_sfa, differencez = operational_ACSS - predict_sfaz,
                                          difference_perc_sfa = (operational_ACSS - predict_sfa)/predict_sfa,
                                          difference_perc_sfaz = (operational_ACSS - predict_sfaz)/predict_sfaz)
      write.csv(CSH_predict, "CSH_predict.csv", row.names = FALSE)
      
      CSH_predict_summary = CSH_predict_Azor |> group_by(hospital) |>
        summarise(operational_ACSS = mean(operational_ACSS), predict_sfa = mean(predict_sfa), difference = mean(difference),
                  differencez = mean(differencez), difference_perc_sfa = mean(difference_perc_sfa),
                  difference_perc_sfaz = mean(difference_perc_sfa))
      write.csv(CSH_predict_summary, "CSH_predict_summary.csv", row.names = FALSE)
      
    ### 4.2 Efficiency HC -------------------------------------------------
      #### A. Efficiency tables Azores ------------------------------------
      # SFA 
      efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = hosp_centre),
                                    year = rownames(efficiencies(sfront, newdata = hosp_centre)),
                                    hospital = "hospital_azores" ) |>
        rename("efficiency_sfa" = "efficiency")
      
      # SFA Z
      efficiencies_sfaz = data.frame(year = rownames(efficiencies(sfrontz, newdata = hosp_centre)),
                                     efficiency = efficiencies(sfrontz, newdata = hosp_centre),
                                     hospital = "hospital_azores") |>
        rename("efficiency_sfaz" = "efficiency")
  
      #### B. Join all efficiency tables Azores ---------------------------
      list_df = list(CSH_predict_Azor, efficiencies_sfa, efficiencies_sfaz)
      
      CSH_predict_Azor = list_df |> reduce(function(x,y) full_join(x, y, by = c("hospital" = "hospital", "year" = "year"),
                                 .init = NULL)) 
      
      write.csv(CSH_predict_Azor, "CSH_effi_pred_Azor.csv", row.names = FALSE)
      
      #### C. Efficiency tables whole DB ------------------------------------
      # SFA
      efficiencies_sfa = data.frame(efficiency = efficiencies(sfront, newdata = CSH_predict),
                                    hospital = rownames(efficiencies(sfront, newdata = CSH_predict))) |>
        rename("efficiency_sfa" = "efficiency")
  
      # SFA Z
      efficiencies_sfaz = data.frame(hospital = rownames(efficiencies(sfrontz, newdata = CSH_predict)),
                                     efficiency = efficiencies(sfrontz, newdata = CSH_predict)) |>
        pivot_longer(c("efficiency.2015","efficiency.2016", "efficiency.2017", "efficiency.2018",
                       "efficiency.2019", "efficiency.2020", "efficiency.2021"),
                     names_to = "year", values_to = "efficiency_sfa_z") |>
        mutate( year = str_remove(year, ".*\\.")) # .(any character) *(zero or more occurences) \\. (".")
      
      #### D. Join all efficiency tables (whole sample) -------------------
      CSH_predict = full_join(CSH_predict, efficiencies_sfaz, by = c("hospital" = "hospital", "year" = "year"))
      CSH_predict = full_join(CSH_predict, efficiencies_sfa, by = c("hospital" = "hospital"))
      
    ### 4.3 Over cost Estimation -----------------------------------------
    # A. Mean efficiency Portugal and Azores
    innef_P = 1-round(summary(sfront)$efficMean,3)
    innefz_P = 1-round(summary(sfrontz)$efficMean,3)
    
    # B. Table with overcosts
    CSH_predict_Azor = CSH_predict_Azor |>
      mutate(Inef_Cost_Cont = innef_P * predict_sfa, 
             Inef_Cost_Cont_z = innefz_P * predict_sfaz,
             Innef_Cost_Isl = (1-efficiency_sfa)*predict_sfa,
             Innef_Cost_Isl_z = (1-efficiency_sfaz)*predict_sfaz,
             overcost =  Innef_Cost_Isl- Inef_Cost_Cont,
             overcostz = Innef_Cost_Isl_z - Inef_Cost_Cont_z,
             mean_overcost = overcost/pop) |>
      select(hospital, year, operational_ACSS, predict_sfa, predict_sfaz, efficiency_sfa,
             efficiency_sfaz, Inef_Cost_Cont, Inef_Cost_Cont_z, Innef_Cost_Isl,
             Innef_Cost_Isl_z, overcost, overcostz, mean_overcost)
    
    # C. Save to directory 
    write.csv(CSH_predict_Azor, "CSH_predict_overcost_Azores.csv", row.names = FALSE)
    # Save the summary table as a PDF file
    pdf(paste0("CSH_predict_overcost_azores.pdf"), width = 20, height = 8)
    grid.table(CSH_predict_Azor, rows = NULL)
    dev.off()
    
    # B. Table with overcosts
    CSH_predict = CSH_predict |>
      mutate(Inef_Cost_Cont = innef_P * predict_sfa, 
             Inef_Cost_Cont_z = innefz_P * predict_sfaz,
             Innef_Cost_Isl = (1-efficiency_sfa)*predict_sfa,
             Innef_Cost_Isl_z = (1-efficiency_sfa_z)*predict_sfaz,
             overcost =  Innef_Cost_Isl- Inef_Cost_Cont,
             overcostz = Innef_Cost_Isl_z - Inef_Cost_Cont_z)

  ## 5. Graphic Representation --------------------------------------------
    ### A. Database for graph
    CSH_predict = CSH_predict |> mutate(pred_overcost = predict_sfa + overcost)
    d = CSH_predict |> select(azo, predict_sfa, predict_sfa, pred_overcost, overcostz, overcost)
    
    ggplot(data = filter(CSH_predict, year == 2018), aes(x = in_days, y = operational_ACSS, color = azo)) +
      geom_point(alpha = 0.7) +
      xlim(0, 2e+06) +
      ylim(0, 2e+09) +
      geom_line(data = filter(CSH_predict, azo == 0), aes(y = predict_sfa), linetype = "dashed", color = "lightgreen") +
      geom_point(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = pred_overcost), color = "red", shape = 4, size = 3) +
      geom_segment(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = predict_sfa, xend = in_days, yend = pred_overcost), color = "red", linetype = "dotted") +
      geom_segment(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = pred_overcost, xend = in_days, yend = operational_ACSS), color = "blue", linetype = "dotted") +
      geom_point(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = predict_sfa), color = "pink") +
      labs(x = "Consultas", y = "Custos com Pessoal", title = "Fronteira Estocástica", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2018." ) +
      scale_color_manual(values = c("1" = "lightblue", "0" = "lightgreen"),
                         labels = c("Açores", "Portugal Continental"),
                         name = "")
    ggsave(filename = "CDgraph_eff_pred.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
    
    
    # ZOOM in
    ggplot(data = filter(CSH_predict, year == 2018), aes(x = in_days, y = operational_ACSS, color = azo)) +
      geom_point(alpha = 0.7) +
      xlim(0, 500000) +
      ylim(0, 2.5e+08) +
      #geom_line(data = filter(CSH_predict, azo == 0), aes(y = predict_sfa), linetype = "dashed", color = "lightgreen") +
      geom_point(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = pred_overcost), color = "red", shape = 4, size = 3) +
      geom_segment(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = predict_sfa, xend = in_days, yend = pred_overcost), color = "red", linetype = "dotted") +
      geom_segment(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = pred_overcost, xend = in_days, yend = operational_ACSS), color = "blue", linetype = "dotted") +
      geom_point(data = filter(CSH_predict, azo == 1, year == 2018), aes(x = in_days, y = predict_sfa), color = "pink") +
      labs(x = "Consultas", y = "Custos com Pessoal", title = "Fronteira Estocástica", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2018." ) +
      scale_color_manual(values = c("1" = "lightblue", "0" = "lightgreen"),
                         labels = c("Açores", "Portugal Continental"),
                         name = "")
    ggsave(filename = "CDgraph_eff_pred_zoom.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
  
# IV. SFA Portugal Continent for Staff Costs ------------------------------
  ## 1. Data -----------------------------------------------------------------
  rm(list = setdiff(ls(), "CSH"))
    setwd("..")
    dir.create("4. Portugal Frontier (staff)")
    setwd("4. Portugal Frontier (staff)")
  
    ### B. Select variables and test correlation --------------------------
      CSH_0 = CSH |> select(hospital, year, staff, in_days, RO, azo, region, 
                            surg, app, urge, beds, patient_dis, wait_scheduled_surg) |> 
        drop_na()
      
      # correlation testing RO
      corr_table = CSH_0 |> 
        select(staff, in_days, RO,beds, surg, app, urge, 
               patient_dis, wait_scheduled_surg) |> 
        cor()
      
      # Discard volume variables that have high correlation 
      #(choosen the scale variable will capture the effects of the others)
      CSH_0 = CSH |> select(hospital, year, staff, in_days, RO, azo, region, surg,
                            n_polos, fem, den, aging, wait_scheduled_surg) |> 
        filter(azo == 0) |>
        drop_na()
      
    ### A. Variables and Functional form ------------------------------------
    form = log(staff) ~ log(in_days) +  log(surg) + log(wait_scheduled_surg) + n_polos
    formz = log(staff) ~ log(in_days)  + log(surg) |  fem + den + aging + wait_scheduled_surg
    
    ### C. Panel data -----------------------------------------------------
    CSH_panel = pdata.frame(CSH_0, c("hospital", "year"))
  ## 2. Operational Cost function : Continent -----------------------------
  ## 2.1 Estimation: time invariant inefficiencies ------------------------
    ### A. Cobb-Douglas (form) ----------------------------------------------
    sfront = sfa( form,  
                  data = CSH_panel,
                  ineffDecrease = F, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
    summary(sfront, extraPar = TRUE)
    
    ### B. Cobb-Douglas with z_it (formz) -------------------------------------
    sfrontz = sfa( formz,  
                   data = CSH_panel,
                   ineffDecrease = F, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = FALSE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
    summary(sfrontz)
    
  ## 3. Tables ------------------------------------------------------------
## STOP HERE COMPLETE TABLES ----
# V. Tests ----------------------------------------------------------------
warnings()  
setwd("../../..")
###### END #####
rm(list=ls())
