# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(plm)
library(stargazer)

# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSH.RData")

# 2. Operational Cost function --------------------------------------------

  ## 2.1 Variables and Functional form ------------------------------------

  ### A. Select variables and test correlation ----------------------------
  CSH_1 = CSH |> select(hospital, year, operational, cmvmc, in_days, RO, azo, region, 
                        surg, app, urge, patient_dis, wait_scheduled_surg) |> 
    drop_na()

  # correlation testing RO
  corr_table = CSH_1 |> 
    select(operational, cmvmc, in_days, RO, surg, app, urge, 
           patient_dis, wait_scheduled_surg) |> 
    cor()

  # Discard volume variables that have high correlation 
  #(choosen the scale variable will capture the effects of the others)
  CSH_1 = CSH |> select(hospital, year, operational, in_days, RO, azo, region, 
                        surg, urge, wait_scheduled_surg) |> 
    drop_na()
  
  ### B. Panel data -------------------------------------------------------
  CSH_panel = pdata.frame(CSH_1, c("hospital", "year"))
# STOP HERE_--------
  ## 2.2 Define functional forms ------------------------------------------
  form = log(operational) ~ log(in_days) + log(RO) + azo # includes Dummy variable
  formt = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + azo
  
  # Firm level inefficiencies models BC95
  # mean inefficiency of u_it is "determined" by factors z_it
  formz = log(operational) ~ I(log(in_days)) + azo | RO
  formtz = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + azo | RO
    
  # FE model G05 (Greene)
  # time invariant individual heterogeneity is not separate from individual innefficieny
  form_r = log(operational) ~ I(log(in_days)) + log(RO) + factor(region) 
  formt_r = log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + log(RO) + factor(region) 

  ## 2.3 Estimation: time invariant inefficiencies ------------------------
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
  class(summary(sfrontt))
  
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
  
  ## 2.? Tables -----------------------------------------------------------
  ### A. BC92 ----------------------------------------------------------------
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
  
  me1 = round(mean(efficiencies(sfront)),3)
  me2 = round(mean(efficiencies(sfrontt)),3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(form, data = CSH_panel)
  lm2 = lm(formt, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            digits = 3, float.env = "table",
            dep.var.labels=c("log Operational Costs"),
            title = "Regression Results",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)))

  ## STOP HERE: double check coeffs -----
  ### B. BC95 -------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 = as.vector(coef(sfrontz))
  c2 = as.vector(coef(sfronttz))
  
  pv1 = as.vector(coef(summary(sfrontz))[,4])
  pv2 = as.vector(coef(summary(sfronttz))[,4])
  
  # save gamma parameter, nº of time periods, log-likelihood and mean efficiency
  g1 = round(as.vector(coef(summary(sfrontz, extraPar = TRUE))[nrow(coef(summary(sfrontz, extraPar = TRUE))),1]), 3)
  g2 = round(as.vector(coef(summary(sfronttz, extraPar = TRUE))[nrow(coef(summary(sfrontz, extraPar = TRUE))),1]),3)
  
  ll1 = round(as.numeric(logLik(sfrontz, which = "mle")),3)
  ll2 = round(as.numeric(logLik(sfronttz, which = "mle")),3)
  
  me1 = round(mean(efficiencies(sfrontz)),3)
  me2 = round(mean(efficiencies(sfronttz)),3)
  
  #run the same specifications using a linear model 
  # (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 = lm(log(operational) ~ I(log(in_days)) + azo + RO, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days) + I((log(in_days)^2)/2) + azo | RO, data = CSH_panel)
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm2,
            coef = list(c1,c2), 
            p = list(pv1,pv2),
            digits = 3, float.env = "table",
            dep.var.labels=c("log Operational Costs"),
            title = "Regression Results",
            column.labels = c("Cobb-Douglas", "Translog"),
            model.numbers = TRUE,
            out = "SFAregression_table.tex",
            omit.stat = c("rsq", "adj.rsq", "ser", "f"),
            add.lines = list(c("Gamma", g1, g2), 
                             c("Log-likelihood value", ll1,ll2),
                             c("Mean efficiency", me1, me2)))

  ### C. G05 --------------------------------------------------------------


  

  # §. Tests ----------------------------------------------------------------
warnings()     
###### END #####
rm(list=ls())
