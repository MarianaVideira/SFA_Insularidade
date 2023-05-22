# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(plm)
library(ggplot2)
library(stargazer)

# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("SFAestimates")
setwd("SFAestimates")

# Other things to check ----------
# function for translog elasticities
# Check the lrtest.frontier likelihood ratio test as well and resettestFrontier RESET test for Stochastic Frontier Models
# 1. Data -----------------------------------------------------------------
  ## 1.1. Panel data frame----------------------------------------------------
  hospanel = pdata.frame(hospital, c("hospital", "year"))
  
  ## 1.2. Define functional forms --------------------------------------------
  form = log(operational) ~ I(log(app)) + I(log(in_days)) + I(log(urge)) + I(log(RO))
  formquad = operational ~ app + in_days + urge + RO + I(app^2) + I(in_days^2) + I(urge^2) + I(RO^2)+ I(urge*app) + I(app*in_days) + I(urge*in_days) + I(urge*RO) + I(RO*app) + I(RO*in_days) # quadractic form no interactions
  formtrans = log(operational) ~ I(log(app)) + I(log(in_days)) + I(log(urge)) + I(log(RO)) + I((log(app)^2)/2) + I((log(in_days)^2)/2) + I((log(urge)^2)/2) + I((log(RO)^2)/2)  # translof following Azevedo 2014
  # No interaction term for now
  
  ## 1.3. Define functional forms with | Z factors-----------------------------
  # (TBA)
  # Note, when adding functional form BC95 go check again the frontier package guide (function "attr")

# 2. Model Estimation (Cross section) --------------------------------------
  ## 2.1 Logarithm (Cobb-Doug) ------------------------------------------------
  # Cross section estimation of the average cost per patient frontier, following
  # ALS77 specification: yi= a + sum(\beta_j x_{j,i}) + v_i -u_i (half-normal dist)

  # A. Cost frontier Estimation
  sfront = sfa( form,  
                data = hospital,
                ineffDecrease = F, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  
  # B. Coefficients
  # summary estimation
  summary(sfront)
  # extract table of coefficients
  cost_coef_table = coef(summary(sfront), which = "mle", extraPar = T) # returns coefficients and p-value
  cost_coef = coef(sfront, which = "mle", extraPar = T) # only returns coefficients
  

  # C. Individual inefficiencies
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  innef = 1 - efficiencies(sfront)
  summary(innef)
  innef_table = summary(innef)
  
  hospital$innef_log = 1 - efficiencies(sfront, asInData = TRUE)
  
  # D. Fitted and predicted values (There is also a predict function)
  fit = fitted(sfront, asInData = TRUE)
  res = residuals(sfront, asInData = TRUE)
  

  ## 2.2 Quadratic specifications --------------------------------------------
  
  # A. Cost frontier Estimation: no interaction term
  sfrontq = sfa( formquad,  
                data = hospital,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )

  # B. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(sfrontq)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(sfrontq), which = "mle", extraPar = T)
  cost_coef = coef(sfrontq, which = "mle", extraPar = T) # only returns coefficients
  
  # C. Individual inefficiencies by function 
  # no convergence

  ## 2.3 Translog specifications --------------------------------------------
  
  # A. Cost frontier Estimation: no interaction term
  sfrontt = sfa(formtrans,  
                data = hospital,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  
  # B. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(sfrontt)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(sfrontt), which = "mle", extraPar = T)
  cost_coef = coef(sfrontt, which = "mle", extraPar = T) # only returns coefficients
  
  # C. Individual inefficiencies by function 
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  in_costf = 1 - efficiencies(sfrontt)
  summary(in_costf)
  innef_table = summary(in_costf)
  
  hospital$innef_trans = 1 - efficiencies(sfrontt, asInData = TRUE)
  

# 3. Tables ---------------------------------------------------------------
  # Creative approach to export SFA tables (since normal packages don't support format)
  # save the coefficients and p-v as vectors:
  c1 <- as.vector(coef(sfront))
  c2 <- as.vector(coef(sfrontq))
  c3 <- as.vector(coef(sfrontt))
  
  pv1 <- as.vector(coef(summary(sfront))[,4])
  pv2 <- as.vector(coef(summary(sfrontq))[,4])
  pv3 <- as.vector(coef(summary(sfrontt))[,4])
  
  #Then run the same specifications using a linear model (remember to change the "|" before the z variables to a "+" in the linear model):
  lm1 <- lm(form, data = hospital)
  lm2 <- lm(formquad, data = hospital)
  lm3 <- lm(formtrans, data = hospital)
  #lm2 <- lm(log(y2) ~ log(x3) + log(x4) + z1 + z2, data = data)
  
  
  #Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
  stargazer(lm1, lm3,
            coef = list(c1,c3), 
            se = list(pv1,pv3),
            digits = 2, float.env = "table",
            title = "Regression Results",
            column.labels = c("Cobb-Douglas", "Translog"),
            out = "SFAregression_table.tex")


# §. Tests ----------------------------------------------------------------

    
  # OTHER OPTION: 
  library(knitr)
  library(kableExtra)
  install.packages("kableExtra")
  
  # Convert matrix to a data frame
  my_df <- as.data.frame(cost_coef_table)
  class(cost_coef_table)
  # Print the matrix as a LaTeX table using kable and kableExtra
  kable(my_df, format = "latex") 
  
  # OTHER OPTION
  library(xtable)
  my_table = xtable(cost_coef_table)
  print(my_table, file = "table.tex")

  # other option: https://r-forge.r-project.org/forum/forum.php?thread_id=33779&forum_id=908&group_id=275
  
##### END #####
rm(form, formquad, formtrans,
   c1, c2, c3, 
   se1, se2, se3, 
   lm1, lm2, lm3,
   latex_table,
   cost_table_coef, cost_coef,
   sfront, sfrontq, sfrontt, fit, res,
   innef_table, innef)

setwd("..") 
# remove var from tests
rm(my_df, my_table, p_values, latexModels, coefficients)
