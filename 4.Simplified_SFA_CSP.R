# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)

# 1. Data -----------------------------------------------------------------
load("0.DataBase/CSP.RData")
CSP_2018 = CSP |> filter(year == 2018)
CSP_2018_0 = CSP |> filter(year == 2018, azo == 0) |> drop_na()
CSP_2018_1 = CSP |> filter(year == 2018, azo == 1) 

  ## 1.1. Define functional forms --------------------------------------------
  form = log(staff) ~ I(log(app))
  formquad = staff ~ app +app*app
  formtrans = log(staff) ~ log(app) + I((log(app)^2)/2)

# 2. Model Estimation (Cross section) --------------------------------------
  # Create a folder for Descriptive statistics plots and set as new working directory
  dir.create("outputs")
  setwd("outputs")
  dir.create("4.SFAestimates_simp")
  setwd("4.SFAestimates_simp")
  dir.create("CSP")
  setwd("CSP")
  
  ## 2.1 Logarithm (Cobb-Doug) ------------------------------------------------
  # Cross section estimation of the average cost per patient frontier, following
  # ALS77 specification: yi= a + sum(\beta_j x_{j,i}) + v_i -u_i (half-normal dist)
  sfront = sfa( form,  
                data = CSP_2018_0,
                ineffDecrease = F, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfront)
  
  ## 2.2 Quadratic specifications --------------------------------------------
  sfrontq = sfa( formquad,  
                 data = CSP_2018_0,
                 ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                 truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                 timeEffect = FALSE, # time is allowed to have an effect on efficiency
                 printIter = 1 )
  summary(sfrontq)
  ## 2.3 Translog specifications --------------------------------------------
  sfrontt = sfa(formtrans,  
                data = CSP_2018_0,
                ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                timeEffect = FALSE, # time is allowed to have an effect on efficiency
                printIter = 1 )
  summary(sfrontt)
  

# 3. Prediction with Main -------------------------------------------------
  # Predict Costs (main = 0)
  predic = predict(sfront, newdata = subset(CSP_2018, azo == 1), asInData = TRUE )
  CSP_2018_1$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfront, newdata = subset(CSP_2018, azo == 1), asInData = TRUE)
  CSP_2018_1$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (main = 1)
  CSP_2018_0 = CSP_2018_0 |> mutate(Prediction = fitted(sfront), 
                             Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSP_2018 = bind_rows(CSP_2018_1, CSP_2018_0)
 

  # 4. Frontier Graph -------------------------------------------------------
  ## 4.1 Cob-Douglas: Main & Azores -----------------------------------------
  CSP_2018$log_staff = log(CSP_2018$staff)
  CSP_2018$log_app = log(CSP_2018$app)
  CSP_2018$azo = as.factor(CSP_2018$azo)
  
  avg_azo_0 <- mean(CSP_2018$log_staff[CSP_2018$azo == 0])
  avg_azo_1 <- mean(CSP_2018$log_staff[CSP_2018$azo == 1])
  avg_app_azo_0 <- mean(CSP_2018$log_app[CSP_2018$azo == 0])
  avg_app_azo_1 <- mean(CSP_2018$log_app[CSP_2018$azo == 1]) 
  
  distance_azo_1 <- CSP_2018$Prediction[CSP_2018$azo == 1] - CSP_2018$log_staff[CSP_2018$azo == 1]
  
  # Create a data frame with hospital names and values
  CSP_2018_1$Difference = exp(abs(distance_azo_1))
  CSP_2018_1_table = CSP_2018_1 |> mutate(pred_ex = exp(Prediction), Difference = staff - pred_ex) |> 
    select(Difference, nome, year)
  xtable_obj = xtable(CSP_2018_1_table)
  print(xtable_obj, file = "CSP_2018_1_table.tex", include.rownames = FALSE, type = "latex")
  cat("Table saved as", "CSP_2018_1_table.tex", "\n")
  
  # Plot the graph
  ggplot(data = CSP_2018, aes(x = log_app, y = log_staff, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_app_azo_1, y = avg_azo_1), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_app_azo_0, y = avg_azo_0), color = "yellow", size = 3, shape = 8)  +
    geom_point(data = CSP_2018[CSP_2018$azo == 1, ], aes(x = log_app, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSP_2018[CSP_2018$azo == 1, ], aes(x = log_app, y = log_staff, xend = log_app, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Appointments", y = "Log staff costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2020." ) +
    scale_color_manual(values = c("1" = "lightpink", "0" = "lightgreen"),
                       labels = c("Portugal Mainland", "Açores"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  ggsave(filename = "CDgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

  ## 4.1 Cob-Douglas: Main  -----------------------------------------------
  # Plot the graph
  ggplot(data = filter(CSP_2018, azo == 0) , aes(x = log_app, y = log_staff, color = azo)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_app_azo_0, y = avg_azo_0), color = "yellow", size = 3, shape = 8)  +
    labs(x = "Log Appointments", y = "Log staff costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Cobb Douglas cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightgreen"),
                       labels = c("Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  ggsave(filename = "CDgraph_main.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
#STOPPPED HEREEE -----------------  
  ## 4.2 Trans ---------------------------------------------------------------
  # Predict Costs (main = 0)
  predic = predict(sfrontt, newdata = subset(CSP_2018, main == 0), asInData = TRUE )
  CSP_2018_0$Prediction = predic
  
  # Predict efficiency
  predic = 1-efficiencies(sfrontt, newdata = subset(CSP_2018, main == 0), asInData = TRUE)
  CSP_2018_0$Prediction_Innef = predic
  
  # Compute the Fitted values using the estimated frontier (main = 1)
  CSP_2018_1 = CSP_2018_1 |> mutate(Prediction = fitted(sfront), 
                                    Prediction_Innef = 1-efficiencies(sfront, asInData = TRUE))
  CSP_2018 = bind_rows(CSP_2018_1, CSP_2018_0)
  CSP_2018$main = as.factor(CSP_2018$main)
  CSP_2018$log_staff = log(CSP_2018$staff)
  CSP_2018$log_app = log(CSP_2018$app)
  
  avg_main_0 <- mean(CSP_2018$log_staff[CSP_2018$main == 0])
  avg_main_1 <- mean(CSP_2018$log_staff[CSP_2018$main == 1])
  avg_app_main_0 <- mean(CSP_2018$log_app[CSP_2018$main == 0])
  avg_app_main_1 <- mean(CSP_2018$log_app[CSP_2018$main == 1]) 
  
  distance_main_0 <- CSP_2018$Prediction[CSP_2018$main == 0] - CSP_2018$log_staff[CSP_2018$main == 0]
  
  # Plot the graph
  ggplot(data = CSP_2018, aes(x = log_app, y = log_staff, color = main)) +
    geom_point() +  # Data points
    geom_line(aes(y = Prediction), linetype = "dashed", color = "blue") +
    geom_point(aes(x = avg_app_main_0, y = avg_main_0), color = "pink", size = 3, shape = 8) +
    geom_point(aes(x = avg_app_main_1, y = avg_main_1), color = "lightgreen", size = 3, shape = 8)  +
    geom_point(data = CSP_2018[CSP_2018$main == 0, ], aes(x = log_app, y = Prediction), color = "pink", size = 3, shape = 5) +
    geom_segment(data = CSP_2018[CSP_2018$main == 0, ], aes(x = log_app, y = log_staff, xend = log_app, yend = Prediction), color = "black", linetype = "dotted") +
    labs(x = "Log Number of days (inpatients)", y = "Log staff costs", title = "Stochastic Cost Frontier", caption = "Note: This graphic depict a Translog cost function for the year 2020." ) +
    scale_color_manual(values = c("0" = "lightpink", "1" = "lightgreen"),
                       labels = c("Açores", "Portugal Mainland"),
                       name = "") +  # Color mapping for 'main' variable and legend labels
    theme_bw() +  # Use a white and black theme
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0)) 
  
  ggsave(filename = "transgraph.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
  
# §. Tests ----------------------------------------------------------------
warnings() # check models convergence

##### END #####
rm(list=ls())

setwd("../../..") 
