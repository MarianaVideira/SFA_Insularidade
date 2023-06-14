# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(plm)
library(stargazer)
# log(n_polos) + log(prop_age_45_64)+ log(prop_age_75_hig) + log(prop_age_65_74) + log(prop_age_5_14) + log(prop_age_0_4) + log(prop_fem)

# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSP.RData")

## 1.1 Data and Functional forms ------------------------------------
  ### A. Select variables and test correlation ----------------------------
  CSP_1 = CSP |> select(id, nome, year, azo, staff, app, app_dome, app_remote, 
                        n_polos, prop_nofam_MF, prop_fem, prop_age_0_4,
                        prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
    drop_na()
  
  corr_table = CSP_1 |> 
    select(staff, app, app_dome, app_remote, n_polos, prop_nofam_MF, prop_fem,
           prop_age_0_4, prop_age_5_14, prop_age_45_64, prop_age_65_74, prop_age_75_hig) |> 
    cor()
  
  # Discard volume variables that have high correlation 
  #(choosen the scale variable will capture the effects of the others)
  CSP_1 = CSP |> select(id, year, nome, azo, staff, app, n_polos, prop_nofam_MF, 
                        prop_fem, prop_age_0_4,prop_age_5_14, prop_age_45_64, 
                        prop_age_65_74, prop_age_75_hig) |> 
    filter(azo == 0) |>
    drop_na()

  ### B. Cross section --------------------------------------------------------
  CSP_2016 = CSP_1 |> filter(year == 2016)
  CSP_2017 = CSP_1 |> filter(year == 2017)
  CSP_2018 = CSP_1 |> filter(year == 2018)
  
  ### C. Panel data -------------------------------------------------------
  CSP_panel = pdata.frame(CSP_1, c("nome", "year"))

## 1.2 Define functional forms ------------------------------------------
form = log(staff) ~ log(app) + log(prop_age_45_64)+ log(prop_age_75_hig)
formt = log(staff) ~ log(app) + I((log(app)^2)/2) + log(prop_age_45_64)+ log(prop_age_75_hig) 

# Firm level inefficiencies models BC95
# mean inefficiency of u_it is "determined" by factors z_it
formz = log(staff) ~ log(app) + + log(n_polos) | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem
formtz = log(staff) ~ log(app) + I((log(app)^2)/2) + log(n_polos) | prop_nofam_MF + prop_age_45_64+ prop_age_75_hig + prop_age_5_14 + prop_age_0_4 + prop_fem

# FE model G05 (Greene)
# time invariant individual heterogeneity is not separate from individual innefficieny
form_r = log(staff) ~ log(app) + log(prop_nofam_MF) + factor(id) 
formt_r = log(staff) ~ log(app) + I((log(app)^2)/2) + log(prop_nofam_MF) + factor(id) 
# The models do mot work with these variables
  
# 2. Staff Cost Function: Cross section -----------------------------------
  ## 2.1 Estimation -------------------------------------------------------
    ### A. Cobb-Douglas 2016-2018 (form) ------------------------------------
  sfront16 = sfa( form,  
                  data = CSP_2016,
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
  summary(sfront16, extraPar = TRUE)
  
  sfront17 = sfa( form,  
                  data = CSP_2017,
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
  summary(sfront17, extraPar = TRUE)
  
  sfront18 = sfa( form,  
                  data = CSP_2018,
                  ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                  truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                  timeEffect = FALSE, # time is allowed to have an effect on efficiency
                  printIter = 1 )
  summary(sfront18, extraPar = TRUE)
    ### B. Trans log (formt) -------------------------------------------------
  sfront = sfa( formt,  
                data = CSP_2016,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  
  sfront = sfa( formt,  
                data = CSP_2017,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  
  sfront = sfa( formt,  
                data = CSP_2018,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  
    ### C. Cobb-Douglas with z_it (formz) -------------------------------------
  sfront = sfa( formz,  
                data = CSP_2016,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  
  sfront = sfa( formz,  
                data = CSP_2017,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  
  sfront = sfa( formz,  
                data = CSP_2018,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront, extraPar = TRUE)
  

  ## 2.1 Residuals Check --------------------------------------------------
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

#### STOP HERE ###_----------------
## 3.? Tables -----------------------------------------------------------
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

# §. Tests --------------------------------------------------------------
warnings()     
###### END #####
rm(list=ls())
