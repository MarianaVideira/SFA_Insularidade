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
  dir.create("3.SFA_outputs")
  setwd("3.SFA_outputs")
  dir.create("CSH")
  setwd("CSH")
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
                          surg, urge, wait_scheduled_surg) |> 
      filter(azo == 0) |>
      drop_na()
    
    ### B. Panel data -------------------------------------------------------
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
  
# 2. Operational Cost function --------------------------------------------
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
  lm1 = lm(log(operational) ~ log(in_days) + log(surg) + log(urge) + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  lm2 = lm(log(operational) ~ log(in_days) + log(surg) + log(urge) + I((log(in_days)^2)/2) + I((log(surg)^2)/2) + log(RO) + RO + wait_scheduled_surg, data = CSH_panel)
  
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

  ### C. G05 --------------------------------------------------------------
## STOPED HERE -------

  # §. Tests --------------------------------------------------------------
warnings()  
setwd("../..")
###### END #####
rm(list=ls())
