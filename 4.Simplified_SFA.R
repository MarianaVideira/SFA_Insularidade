# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSH.RData")
CSH = CSH |> mutate(avg_operational = operational/in_days)
CSH_2019 = CSH |> filter(year == 2019)
CSH_2019_0 = CSH |> filter(year == 2019, main == 0)
CSH_2019_1 = CSH |> filter(year == 2019, main == 1) |> drop_na()

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
                data = CSH_2019_1,
                ineffDecrease = F, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront)
  
  ## 2.2 Quadratic specifications --------------------------------------------
  sfrontq = sfa( formquad,  
                 data = CSH_2019_1,
                 ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                 truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                 timeEffect = FALSE, # time is allowed to have an effect on efficiency
                 printIter = 1 )
  summary(sfrontq)
  ## 2.3 Translog specifications --------------------------------------------
  sfrontt = sfa(formtrans,  
                data = CSH_2019_1,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfrontt)
  
  ## 2.1 Avg Logarithm (Cobb-Doug) -----------------------------------------------
  
  ## 2.2 Avg Quadratic specifications --------------------------------------------

# 3. Prediction with Main -------------------------------------------------
  # Predict Costs (main = 0)
  predic = predict(sfront, newdata = subset(CSH_2019, main == 0), asInData = TRUE )
  CSH_2019_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfront, newdata = subset(CSH_2019, main == 0), asInData = TRUE)
  CSH_2019_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (main = 1)
  CSH_2019_1 = CSH_2019_1 |> mutate(Prediction = fitted(sfront), 
                             Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
 

  # 4. Frontier Graph -------------------------------------------------------
  ## 4.1 Cob ----------------------------------------------------------------
  CSH_2019$log_operational = log(CSH_2019$operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  CSH_2019$main = as.factor(CSH_2019$main)
  
  
  avg_main_0 <- mean(CSH_2019$log_operational[CSH_2019$main == 0])
  avg_main_1 <- mean(CSH_2019$log_operational[CSH_2019$main == 1])
  avg_in_days_main_0 <- mean(CSH_2019$log_in_days[CSH_2019$main == 0])
  avg_in_days_main_1 <- mean(CSH_2019$log_in_days[CSH_2019$main == 1]) 
  
  distance_main_0 <- CSH_2019$Prediction[CSH_2019$main == 0] - CSH_2019$log_operational[CSH_2019$main == 0]
  
  # Create a data frame with hospital names and values
  CSH_2019_0$Difference = exp(abs(distance_main_0))
  CSH_2019_0_table = CSH_2019_0 |> mutate(pred_ex = exp(Prediction), Difference = operational - pred_ex) |> 
    select(Difference, hospital, year)
  xtable_obj = xtable(CSH_2019_0_table)
  print(xtable_obj, file = "CSP_2019_0_table.tex", include.rownames = FALSE, type = "latex")
  cat("Table saved as", "CSP_2019_0_table.tex", "\n")
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = main)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_main_0, y = avg_main_0), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_in_days_main_1, y = avg_main_1), color = "lightgreen", size = 3, shape = 8)  +
    geom_point(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Number of days (inpatients)", y = "Log Operational costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightpink", "1" = "lightgreen"),
                       labels = c("Açores", "Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  ggsave(filename = "CDgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

  ## 4.2 Trans ---------------------------------------------------------------
  # Predict Costs (main = 0)
  predic = predict(sfrontt, newdata = subset(CSH_2019, main == 0), asInData = TRUE )
  CSH_2019_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfrontt, newdata = subset(CSH_2019, main == 0), asInData = TRUE)
  CSH_2019_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (main = 1)
  CSH_2019_1 = CSH_2019_1 |> mutate(Prediction = fitted(sfront), 
                                    Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
  CSH_2019$main = as.factor(CSH_2019$main)
  CSH_2019$log_operational = log(CSH_2019$operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  
  avg_main_0 <- mean(CSH_2019$log_operational[CSH_2019$main == 0])
  avg_main_1 <- mean(CSH_2019$log_operational[CSH_2019$main == 1])
  avg_in_days_main_0 <- mean(CSH_2019$log_in_days[CSH_2019$main == 0])
  avg_in_days_main_1 <- mean(CSH_2019$log_in_days[CSH_2019$main == 1]) 
  
  distance_main_0 <- CSH_2019$Prediction[CSH_2019$main == 0] - CSH_2019$log_operational[CSH_2019$main == 0]
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = main)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_main_0, y = avg_main_0), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_in_days_main_1, y = avg_main_1), color = "lightgreen", size = 3, shape = 8)  +
    geom_point(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
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
  # Predict Costs (main = 0)
  predic = predict(asfront, newdata = subset(CSH_2019, main == 0), asInData = TRUE )
  CSH_2019_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(asfrontt, newdata = subset(CSH_2019, main == 0), asInData = TRUE)
  CSH_2019_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (main = 1)
  CSH_2019_1 = CSH_2019_1 |> mutate(Prediction = fitted(asfront), 
                                    Prediction_Innef = 1-efficiencies(asfront, asInData = TRUE))
  CSH_2019 = bind_rows(CSH_2019_1, CSH_2019_0)
  CSH_2019$log_operational = log(CSH_2019$avg_operational)
  CSH_2019$log_in_days = log(CSH_2019$in_days)
  CSH_2019$main = as.factor(CSH_2019$main)
  
  avg_main_0 <- mean(CSH_2019$log_operational[CSH_2019$main == 0])
  avg_main_1 <- mean(CSH_2019$log_operational[CSH_2019$main == 1])
  avg_in_days_main_0 <- mean(CSH_2019$log_in_days[CSH_2019$main == 0])
  avg_in_days_main_1 <- mean(CSH_2019$log_in_days[CSH_2019$main == 1]) 
  
  distance_main_0 <- CSH_2019$Prediction[CSH_2019$main == 0] - CSH_2019$log_operational[CSH_2019$main == 0]
  
  # Plot the graph
  ggplot(data = CSH_2019, aes(x = log_in_days, y = log_operational, color = main)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_in_days_main_0, y = avg_main_0), color = "red", size = 3) +
    geom_point(aes(x = avg_in_days_main_1, y = avg_main_1), color = "green", size = 3)  +
    geom_point(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = Prediction), color = "orange", size = 3) +
    geom_segment(data = CSH_2019[CSH_2019$main == 0, ], aes(x = log_in_days, y = log_operational, xend = log_in_days, yend = Prediction), color = "black", linetype = "dotted") +
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
  geom_vline(data = subset(hospitals_all2020, main == 0), aes(xintercept = efficiencies), color = "lightpink", linetype = "dashed", size = 1) +
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
