# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSH.RData")
CSH = CSH |> mutate(avg_operational = operational/in_days)
CSH_2019 = CSH |> select(operational, year, hospital, azo, in_days) |> filter(year == 2019)
CSH_2019_0 = CSH_2019 |> filter(azo == 0) |> drop_na()
CSH_2019_1 = CSH_2019 |> filter(azo == 1) |> drop_na()

  ## 1.1. Define functional forms --------------------------------------------
  form = log(operational) ~ I(log(in_days)) 
  formquad = operational ~ in_days +in_days*in_days
  formtrans = log(operational) ~ log(in_days) + I((log(in_days)^2)/2)

  ## 1.2. Define Avg. functional forms --------------------------------------

# 2. Model Estimation (Cross section) --------------------------------------
  # Create a folder for Descriptive statistics plots and set as new working directory
  dir.create("outputs")
  setwd("outputs")
  dir.create("4.SFAestimates_simp")
  setwd("4.SFAestimates_simp")
  dir.create("CSH")
  setwd("CSH")
  
  ## 2.1 Logarithm (Cobb-Doug) ------------------------------------------------
  # Cross section estimation of the average cost per patient frontier, following
  # ALS77 specification: yi= a + sum(\beta_j x_{j,i}) + v_i -u_i (half-normal dist)
  sfront = sfa( form,  
                data = CSH_2019_0,
                ineffDecrease = F, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront)
  
  ## 2.2 Quadratic specifications --------------------------------------------
  sfrontq = sfa( formquad,  
                 data = CSH_2019_0,
                 ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                 truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                 timeEffect = FALSE, # time is allowed to have an effect on efficiency
                 printIter = 1 )
  summary(sfrontq)
  ## 2.3 Translog specifications --------------------------------------------
  sfrontt = sfa(formtrans,  
                data = CSH_2019_0,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfrontt)
  
# 3. Prediction with Main -------------------------------------------------
  # Predict Costs (azo = 1)
  predic = predict(sfront, newdata = subset(CSH_2019, azo == 1), asInData = TRUE )
  CSH_2019_1$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfront, newdata = subset(CSH_2019, azo == 1), asInData = TRUE)
  CSH_2019_1$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (azo = 0)
  Prediction = fitted(sfront)
  CSH_2019_0 = CSH_2019_0 |> mutate(Prediction = fitted(sfront), 
                             Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
 

  # 4. Frontier Graph -------------------------------------------------------
  ## 4.1 Cob Douglas: Mainland ----------------------------------------------
  CSH_2019$log_operational = log(CSH_2019$operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  CSH_2019$azo = as.factor(CSH_2019$azo)
  
  
  avg_azo_0 <- mean(CSH_2019$log_operational[CSH_2019$azo == 0])
  avg_in_days_azo_0 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 0])
  
  # Plot the graph
  ggplot(data = filter(CSH_2019, azo == 0), aes(x = log_in_days, y = log_operational, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_azo_0, y = avg_azo_0), color = "pink", size = 3, shape = 8) +
    #geom_point(data = CSH_2019[CSH_2019$azo == 0, ], aes(x = log_in_days, y = Prediction), color = "pink", size = 3, shape = 5) +
    labs(x = "Log Number of days (inpatients)", y = "Log Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightpink"),
                       labels = c("Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  ggsave(filename = "CDgraph_main.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

  ## 4.1 Cob Douglas: Mainland & Azores -----------------------------------
  CSH_2019$log_operational = log(CSH_2019$operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  CSH_2019$azo = as.factor(CSH_2019$azo)
  
  
  avg_azo_0 <- mean(CSH_2019$log_operational[CSH_2019$azo == 0])
  avg_azo_1 <- mean(CSH_2019$log_operational[CSH_2019$azo == 1])
  avg_in_days_azo_0 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 0])
  avg_in_days_azo_1 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 1]) 
  
  distance_azo_1 <- CSH_2019$Prediction[CSH_2019$azo == 1] - CSH_2019$log_operational[CSH_2019$azo == 1]
  
  # Create a data frame with hospital names and values
  CSH_2019_1$Difference = exp(abs(distance_azo_1))
  CSH_2019_1_table = CSH_2019_1 |> mutate(pred_ex = exp(Prediction), Difference = operational - pred_ex) |> 
    select(Difference, hospital, year)
  xtable_obj = xtable(CSH_2019_1_table)
  print(xtable_obj, file = "CSP_2019_1_table.tex", include.rownames = FALSE, type = "latex")
  cat("Table saved as", "CSP_2019_1_table.tex", "\n")
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_azo_1, y = avg_azo_1), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_in_days_azo_0, y = avg_azo_0), color = "lightgreen", size = 3, shape = 8)  +
    geom_point(data = CSH_2019[CSH_2019$azo == 1, ], aes(x = log_in_days, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSH_2019[CSH_2019$azo == 1, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Number of days (inpatients)", y = "Log Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2020." ) +
    scale_color_manual(values = c("1" = "lightpink", "0" = "lightgreen"),
                       labels = c("Portugal Mainland", "Açores"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  ggsave(filename = "CDgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
 ### STOP ###---- 
  ## 4.2 Trans ---------------------------------------------------------------
  # Predict Costs (main = 0)
  predic = predict(sfrontt, newdata = subset(CSH_2019, azo == 0), asInData = TRUE )
  CSH_2019_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfrontt, newdata = subset(CSH_2019, azo == 0), asInData = TRUE)
  CSH_2019_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (azo = 1)
  CSH_2019_1 = CSH_2019_1 |> mutate(Prediction = fitted(sfront), 
                                    Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
  CSH_2019$azo = as.factor(CSH_2019$azo)
  CSH_2019$log_operational = log(CSH_2019$operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  
  avg_azo_0 <- mean(CSH_2019$log_operational[CSH_2019$azo == 0])
  avg_azo_1 <- mean(CSH_2019$log_operational[CSH_2019$azo == 1])
  avg_in_days_azo_0 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 0])
  avg_in_days_azo_1 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 1]) 
  
  distance_azo_0 <- CSH_2019$Prediction[CSH_2019$azo == 0] - CSH_2019$log_operational[CSH_2019$azo == 0]
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_azo_0, y = avg_azo_0), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_in_days_azo_1, y = avg_azo_1), color = "lightgreen", size = 3, shape = 8)  +
    geom_point(data = CSH_2019[CSH_2019$azo == 0, ], aes(x = log_in_days, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSH_2019[CSH_2019$azo == 0, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Number of days (inpatients)", y = "Log Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Translog cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightpink", "1" = "lightgreen"),
                       labels = c("Açores", "Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  
  ggsave(filename = "transgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
 # STOPPED HERE!!!!! ##### 
  ## 4.2 Avg. CD ---------------------------------------------------------------
  # Predict Costs (azo = 1)
  predic = predict(asfront, newdata = subset(CSH_2019, azo == 0), asInData = TRUE )
  CSH_2019_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(asfrontt, newdata = subset(CSH_2019, azo == 0), asInData = TRUE)
  CSH_2019_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (azo = 1)
  CSH_2019_1 = CSH_2019_1 |> mutate(Prediction = fitted(asfront), 
                                    Prediction_Innef = 1-efficiencies(asfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
  CSH_2019$log_operational = log(CSH_2019$avg_operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  CSH_2019$azo = as.factor(CSH_2019$azo)
  
  avg_azo_0 <- mean(CSH_2019$log_operational[CSH_2019$azo == 0])
  avg_azo_1 <- mean(CSH_2019$log_operational[CSH_2019$azo == 1])
  avg_in_days_azo_0 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 0])
  avg_in_days_azo_1 <- mean(CSH_2019$log_in_days[CSH_2019$azo == 1]) 
  
  distance_azo_0 <- CSH_2019$Prediction[CSH_2019$azo == 0] - CSH_2019$log_operational[CSH_2019$azo == 0]
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_azo_0, y = avg_azo_0), color = "red", size = 3) +
    geom_point(aes(x = avg_in_days_azo_1, y = avg_azo_1), color = "green", size = 3)  +
    geom_point(data = CSH_2019[CSH_2019$azo == 0, ], aes(x = log_in_days, y = Prediction), color = "orange", size = 3) +
    geom_segment(data = CSH_2019[CSH_2019$azo == 0, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Number of days (inpatients)", y = "Log Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Translogorithmic cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightpink", "1" = "lightgreen"),
                       labels = c("Açores", "Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  
  ggsave(filename = "AVGtransgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# 5 Efficiencies ---------------------------------------------------------

# Extract efficiencies from the model object
hospitals_all2020$efficiencies = efficiencies(sfrontt)

# Plot the distribution of efficiencies
ggplot(data = hospitals_all2020, aes(x = efficiencies)) +
  geom_density(fill = "lightblue", color = "lightblue", alpha = 0.3) +
  geom_vline(data = subset(hospitals_all2020, azo == 0), aes(xintercept = efficiencies), color = "lightpink", linetype = "dashed", size = 1) +
  labs(x = "Efficiency", y = "Density", title = "Distribution of Efficiencies") +
  theme_bw()

ggsave(filename = "effigraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

# §. Tests ----------------------------------------------------------------
warnings() # check models convergence

##### END #####
rm(list=ls())

setwd("../../..") 

# 
# Define the estimated parameters
#intercept = coef(summary(sfront))[1,1]
#in_days_coeff = coef(summary(sfront))[2,1]

#intercept = coef(summary(sfrontt))[1,1]
#in_days_coeff = coef(summary(sfrontt))[2,1]
#in_days_coeff2 = coef(summary(sfrontt))[3,1]
