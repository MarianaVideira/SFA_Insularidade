# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(plm)
library(ggplot2)
library(stargazer)

# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("SFAestimates_simp")
setwd("SFAestimates_simp")

# 1. Data -----------------------------------------------------------------
hospitals_all2020 = hospitals_all |> filter(year == 2020)

## 1.1. Panel data frame----------------------------------------------------
hospanel = pdata.frame(hospitals_all, c("hospital", "year"))

## 1.2. Define functional forms --------------------------------------------
form = log(operational) ~ I(log(in_days))
formquad = operational ~ in_days +in_days*in_days
formtrans = log(operational) ~ log(in_days) + I((log(in_days)^2)/2)

# 2. Model Estimation (Cross section) --------------------------------------
## 2.1 Logarithm (Cobb-Doug) ------------------------------------------------
# Cross section estimation of the average cost per patient frontier, following
# ALS77 specification: yi= a + sum(\beta_j x_{j,i}) + v_i -u_i (half-normal dist)

# A. Cost frontier Estimation
sfront = sfa( form,  
              data = hospitals_all2020,
              ineffDecrease = F, # FALSE for cost function and TRUE for production
              truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
              timeEffect = FALSE, # time is allowed to have an effect on efficiency
              printIter = 1 )
summary(sfront)
## 2.2 Quadratic specifications --------------------------------------------

# A. Cost frontier Estimation: no interaction term
sfrontq = sfa( formquad,  
               data = hospitals_all2020,
               ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
               truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
               timeEffect = FALSE, # time is allowed to have an effect on efficiency
               printIter = 1 )
summary(sfrontq)
## 2.3 Translog specifications --------------------------------------------

# A. Cost frontier Estimation: no interaction term
sfrontt = sfa(formtrans,  
              data = hospitals_all2020,
              ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
              truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
              timeEffect = FALSE, # time is allowed to have an effect on efficiency
              printIter = 1 )
summary(sfrontt)
# 2.4 Predition with Main -------------------------------------------------
# A. Cost frontier Estimation
sfrontpr = sfa( form,  
              data = hospital,
              ineffDecrease = F, # FALSE for cost function and TRUE for production
              truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
              timeEffect = FALSE, # time is allowed to have an effect on efficiency
              printIter = 1 )

# B. Coefficients
# summary estimation
summary(sfrontpr)
hospitals_aco$predic = predict(sfrontpr, newdata = hospitals_aco, asInData = TRUE )
# hospitals_aco$predic_eff = efficiencies(sfront, newdata = hospitals_aco, asInData = TRUE ) Does not working

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
lm2 <- lm(formquad, data = hospitals_all)
lm3 <- lm(formtrans, data = hospitals_all)
#lm2 <- lm(log(y2) ~ log(x3) + log(x4) + z1 + z2, data = data)


#Finally, use stargazer on the linear models, and change the coefficients and the standard errors reported using the "coef" and "se" arguments:
stargazer(lm1, lm3,
          coef = list(c1,c3), 
          se = list(pv1,pv3),
          digits = 2, float.env = "table",
          title = "Regression Results",
          column.labels = c("Cobb-Douglas", "Translog"),
          out = "SFAregression_table_simp.tex")

# 4. Frontier Graph -------------------------------------------------------
## 4.1 Cob ----------------------------------------------------------------

# Define the estimated parameters
intercept = coef(summary(sfront))[1,1]
in_days_coeff = coef(summary(sfront))[2,1]

# Compute the predicted values using the estimated frontier
hospitals_all2020$log_operational = log(hospitals_all2020$operational)
hospitals_all2020$log_in_days = log(hospitals_all2020$in_days)
hospitals_all2020$predicted = fitted(sfront)

# Plot the graph
ggplot(data = hospitals_all2020, aes(x = log_in_days, y = log_operational, color = main)) +
  geom_point() +  # Data points
  geom_line(aes(y = predicted), linetype = "dashed", color = "blue") +  # Stochastic frontier
  labs(x = "in_days", y = "operational") +  # Axis labels
  scale_color_manual(values = c("0" = "red", "1" = "green")) 


## 4.2 Quad ---------------------------------------------------------------
# Define the estimated parameters
intercept = coef(summary(sfrontq))[1,1]
in_days_coeff = coef(summary(sfrontq))[2,1]
in_days_coeff2 = coef(summary(sfrontq))[3,1]

# Compute the predicted values using the estimated frontier
hospitals_all2020$predicted = fitted(sfrontq)

# Plot the graph
ggplot(data = hospitals_all2020, aes(x = in_days, y = operational, color = main)) +
  geom_point() +  # Data points
  geom_line(aes(y = predicted), linetype = "dashed", color = "blue") +  # Stochastic frontier
  labs(x = "in_days", y = "operational") +  # Axis labels
  scale_color_manual(values = c("0" = "red", "1" = "green")) 

## 4.2 Trans ---------------------------------------------------------------
# Define the estimated parameters
intercept = coef(summary(sfrontt))[1,1]
in_days_coeff = coef(summary(sfrontt))[2,1]
in_days_coeff2 = coef(summary(sfrontt))[3,1]

# Compute the predicted values using the estimated frontier
hospitals_all2020$predicted = fitted(sfrontt)

# Plot the graph
ggplot(data = hospitals_all2020, aes(x = log_in_days, y = log_operational, color = main)) +
  geom_point() +  # Data points
  geom_line(aes(y = predicted), linetype = "dashed", color = "blue") +
  labs(x = "Number of days (inpatients)", y = "Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Translogorithmic cost function for the year 2020." ) +
  scale_color_manual(values = c("0" = "lightpink", "1" = "lightgreen"),
                     labels = c("Açores", "Portugal Mainland"),
                     name = "") +  # Color mapping for 'main' variable and legend labels
  theme_bw() +  # Use a white and black theme
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0)) 

ggsave(filename = "transgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# 4.4 Efficiencies --------------------------------------------------------

# Extract efficiencies from the model object
hospitals_all2020$efficiencies = efficiencies(sfrontt)

# Plot the distribution of efficiencies
ggplot(data = hospitals_all2020, aes(x = efficiencies)) +
  geom_density(fill = "lightblue", color = "lightblue", alpha = 0.3) +
  geom_vline(data = subset(hospitals_all2020, main == 0), aes(xintercept = efficiencies), color = "lightpink", linetype = "dashed", size = 1) +
  labs(x = "Efficiency", y = "Density", title = "Distribution of Efficiencies") +
  theme_bw()

ggsave(filename = "effigraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# §. Tests ----------------------------------------------------------------

##### END #####
rm(list=setdiff(ls(), c("hospital", "hospital2", "hospitals_all")))

setwd("..") 
