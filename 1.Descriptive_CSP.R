# 0. Packages and Data -----------------------------------------------------
library(xtable)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(xtable)
library(ggpubr)
library(viridis)

# load data
load("0.DataBase/CSP.RData")

# II. Summary Tables ------------------------------------------------------
# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("1.descriptive_stats")
setwd("1.descriptive_stats")
dir.create("CSP")
setwd("CSP")
dir.create("summary_tables")
setwd("summary_tables")
dir.create("1.per_year")
setwd("1.per_year")
## 1. Summary tables per year -------------------------------------------
### 1.1 Costs -------------------------------------------------------------

# A. Create summary tables for each year using loop
# Create an empty list to store the summary tables
summary_list <- list()

# Loop through each year from 2015 to 2020
for (year in 2016:2018) {
  
  # Filter the data for the current year
  filtered_data = CSP |>
    filter(year == year)
  
  # Calculate the summary statistics for variables DP
  summary_table = filtered_data |>
    summarise(variable = "staff costs",
              mean = mean(staff, na.rm = TRUE),
              sd = sd(staff, na.rm = TRUE),
              obs = n(),
              min = min(staff, na.rm = TRUE),
              max = max(staff, na.rm = TRUE),
              q25 = quantile(staff, 0.25, na.rm = TRUE),
              q75 = quantile(staff, 0.75, na.rm = TRUE))
  
  # Add the summary table to the list
  summary_list[[year - 2015]] = summary_table
  
  # Save each summary table as a PDF file
  pdf(paste0("costs_summary_table_acp", year, ".pdf"), width = 10, height = 4)
  grid.table(summary_table, rows = NULL)
  dev.off()
  
}

# B. Latex tables
dir.create("latex")
setwd("latex")

# Loop for Latex tables
for (i in 1:length(summary_list)) {
  summary_table_latex = xtable(summary_list[[i]], caption = paste("Summary Table costs", i+2014))
  
  # save file in working directory
  file_name = paste0("summary_table_costs", i+2014,".tex")
  file_path = file.path(getwd(), file_name)
  
  print(summary_table_latex, file = file_path, include.rownames = FALSE, caption.placement = "top")
}
setwd("..")
### 1.3 Volume -------------------------------------------------------------

# Create an empty list to store the summary tables
summary_list <- list()

# Loop through each year from 2015 to 2020
for (year in 2015:2020) {
  
  # Filter the data for the current year
  filtered_data <- CSP |>
    filter(year == year)
  
  # Calculate the summary statistics for variables DP
  app_summary <- filtered_data |>
    summarise(variable = "appointments",
              mean = mean(app, na.rm = TRUE),
              sd = sd(app, na.rm = TRUE),
              obs = n(),
              min = min(app, na.rm = TRUE),
              max = max(app, na.rm = TRUE),
              q25 = quantile(app, 0.25, na.rm = TRUE),
              q75 = quantile(app, 0.75, na.rm = TRUE))
  
  app_rem_summary <- filtered_data |>
    summarise(variable = "remote appointments",
              mean = mean(app_remote, na.rm = TRUE),
              sd = sd(app_remote, na.rm = TRUE),
              obs = n(),
              min = min(app_remote, na.rm = TRUE),
              max = max(app_remote, na.rm = TRUE),
              q25 = quantile(app_remote, 0.25, na.rm = TRUE),
              q75 = quantile(app_remote, 0.75, na.rm = TRUE))
  
  app_dome_summary <- filtered_data |>
    summarise(variable = "mean_wait_summary",
              mean = mean(app_dome, na.rm = TRUE),
              sd = sd(app_dome, na.rm = TRUE),
              obs = n(),
              min = min(app_dome, na.rm = TRUE),
              max = max(app_dome, na.rm = TRUE),
              q25 = quantile(app_dome, 0.25, na.rm = TRUE),
              q75 = quantile(app_dome, 0.75, na.rm = TRUE))
  
  inscritos_summary <- filtered_data |>
    summarise(variable = "scheduled_days",
              mean = mean(inscritos, na.rm = TRUE),
              sd = sd(inscritos, na.rm = TRUE),
              obs = n(),
              min = min(inscritos, na.rm = TRUE),
              max = max(inscritos, na.rm = TRUE),
              q25 = quantile(inscritos, 0.25, na.rm = TRUE),
              q75 = quantile(inscritos, 0.75, na.rm = TRUE))
  
  
  
  # Combine the summary tables into a single table
  summary_table <- bind_rows(inscritos_summary, app_dome_summary, app_rem_summary,
                             app_summary)
  
  # Add the summary table to the list
  summary_list[[year - 2014]] <- summary_table
  
  
  # Save the summary table as a PDF file
  pdf(paste0("vol_summary_table_", year, ".pdf"), width = 10, height = 4)
  grid.table(summary_table, rows = NULL)
  dev.off()
  
}

# B. Latex tables
setwd("latex")

# Loop for Latex tables
for (i in 1:length(summary_list)) {
  summary_table_latex = xtable(summary_list[[i]], caption = paste("Summary Table average costs per patient", i+2014))
  
  # save file in working directory
  file_name = paste0("summary_table_volume", i+2014,".tex")
  file_path = file.path(getwd(), file_name)
  
  print(summary_table_latex, file = file_path, include.rownames = FALSE, caption.placement = "top")
}

setwd("../..")

## 2. Summary tables per Azores/Madeira ---------------------------------
dir.create("2.Portugal_Islands")
setwd("2.Portugal_Islands")

# A. Create summary tables for each year using loop
# Create an empty list to store the summary tables
summary_list = list()
CSP$azo <- as.factor(CSP$azo)
for (azo_level in levels(CSP$azo)) {
  
  # Filter the data for the current year
  filtered_data = filter(CSP, azo == azo_level)
  
  # Calculate the summary statistics for variables
  summary_table = filtered_data |>
    summarise(variable = "staff costs",
              mean = mean(staff, na.rm = TRUE),
              sd = sd(staff, na.rm = TRUE),
              obs = n(),
              min = min(staff, na.rm = TRUE),
              max = max(staff, na.rm = TRUE),
              q25 = quantile(staff, 0.25, na.rm = TRUE),
              q75 = quantile(staff, 0.75, na.rm = TRUE))
  
  avg_staff_table = filtered_data |>
    summarise(variable = "staff costs per population",
              mean = mean(avg_staff, na.rm = TRUE),
              sd = sd(avg_staff, na.rm = TRUE),
              obs = n(),
              min = min(avg_staff, na.rm = TRUE),
              max = max(avg_staff, na.rm = TRUE),
              q25 = quantile(avg_staff, 0.25, na.rm = TRUE),
              q75 = quantile(avg_staff, 0.75, na.rm = TRUE))
  
  app_summary <- filtered_data |>
    summarise(variable = "appointments",
              mean = mean(app, na.rm = TRUE),
              sd = sd(app, na.rm = TRUE),
              obs = n(),
              min = min(app, na.rm = TRUE),
              max = max(app, na.rm = TRUE),
              q25 = quantile(app, 0.25, na.rm = TRUE),
              q75 = quantile(app, 0.75, na.rm = TRUE))
  
  app_rem_summary <- filtered_data |>
    summarise(variable = "remote appointments",
              mean = mean(app_remote, na.rm = TRUE),
              sd = sd(app_remote, na.rm = TRUE),
              obs = n(),
              min = min(app_remote, na.rm = TRUE),
              max = max(app_remote, na.rm = TRUE),
              q25 = quantile(app_remote, 0.25, na.rm = TRUE),
              q75 = quantile(app_remote, 0.75, na.rm = TRUE))
  
  app_dome_summary <- filtered_data |>
    summarise(variable = "home appointments",
              mean = mean(app_dome, na.rm = TRUE),
              sd = sd(app_dome, na.rm = TRUE),
              obs = n(),
              min = min(app_dome, na.rm = TRUE),
              max = max(app_dome, na.rm = TRUE),
              q25 = quantile(app_dome, 0.25, na.rm = TRUE),
              q75 = quantile(app_dome, 0.75, na.rm = TRUE))
  
  inscritos_summary <- filtered_data |>
    summarise(variable = "number of citizens",
              mean = mean(inscritos, na.rm = TRUE),
              sd = sd(inscritos, na.rm = TRUE),
              obs = n(),
              min = min(inscritos, na.rm = TRUE),
              max = max(inscritos, na.rm = TRUE),
              q25 = quantile(inscritos, 0.25, na.rm = TRUE),
              q75 = quantile(inscritos, 0.75, na.rm = TRUE))
  
  MD_summary = filtered_data |>
    summarise(variable = "proportion without FD",
              mean = mean(prop_nofam_MF, na.rm = TRUE),
              sd = sd(prop_nofam_MF, na.rm = TRUE),
              obs = n(),
              min = min(prop_nofam_MF, na.rm = TRUE),
              max = max(prop_nofam_MF, na.rm = TRUE),
              q25 = quantile(prop_nofam_MF, 0.25, na.rm = TRUE),
              q75 = quantile(prop_nofam_MF, 0.75, na.rm = TRUE))
  
  age_75_summary = filtered_data |>
    summarise(variable = "proportion with 75more",
              mean = mean(prop_age_75_hig, na.rm = TRUE),
              sd = sd(prop_age_75_hig, na.rm = TRUE),
              obs = n(),
              min = min(prop_age_75_hig, na.rm = TRUE),
              max = max(prop_age_75_hig, na.rm = TRUE),
              q25 = quantile(prop_age_75_hig, 0.25, na.rm = TRUE),
              q75 = quantile(prop_age_75_hig, 0.75, na.rm = TRUE))
  
  age_0_4_summary = filtered_data |>
    summarise(variable = "proportion 00-04",
              mean = mean(prop_age_0_4, na.rm = TRUE),
              sd = sd(prop_age_0_4, na.rm = TRUE),
              obs = n(),
              min = min(prop_age_0_4, na.rm = TRUE),
              max = max(prop_age_0_4, na.rm = TRUE),
              q25 = quantile(prop_age_0_4, 0.25, na.rm = TRUE),
              q75 = quantile(prop_age_0_4, 0.75, na.rm = TRUE))
  
  npolos_summary = filtered_data |>
    summarise(variable = "number of centers",
              mean = mean(n_polos, na.rm = TRUE),
              sd = sd(n_polos, na.rm = TRUE),
              obs = n(),
              min = min(n_polos, na.rm = TRUE),
              max = max(n_polos, na.rm = TRUE),
              q25 = quantile(n_polos, 0.25, na.rm = TRUE),
              q75 = quantile(n_polos, 0.75, na.rm = TRUE))
  
  fem_summary = filtered_data |>
    summarise(variable = "fem prop",
              mean = mean(prop_fem, na.rm = TRUE),
              sd = sd(prop_fem, na.rm = TRUE),
              obs = n(),
              min = min(prop_fem, na.rm = TRUE),
              max = max(prop_fem, na.rm = TRUE),
              q25 = quantile(prop_fem, 0.25, na.rm = TRUE),
              q75 = quantile(prop_fem, 0.75, na.rm = TRUE))
  
  
  # Combine the summary tables into a single table
  summary_table = bind_rows(summary_table, avg_staff_table, inscritos_summary, app_dome_summary, 
                            app_rem_summary, app_summary, fem_summary, npolos_summary, age_0_4_summary, 
                            age_75_summary, MD_summary)
  
  # Add the summary table to the list
  summary_list[[as.integer(azo_level) + 1]] = summary_table
  
  # Save each summary table as a PDF file
  pdf(paste0("summary_table_", as.integer(azo_level), ".pdf"), width = 12, height = 6)
  grid.table(summary_table, rows = NULL)
  dev.off()

}

# B. Latex tables
dir.create("latex")
setwd("latex")

# Loop for Latex tables
for (i in 1:length(summary_list)) {
  if (i == 1) {
    caption = "Summary table Azores"
  } else if (i == 0) {
    caption = "Summary table Portugal_Continent"
  }
  
  summary_table_latex = xtable(summary_list[[i]], caption = caption)
  
  # save file in working directory
  file_name = paste0("summary_table", i,".tex")
  file_path = file.path(getwd(), file_name)
  
  
  print(summary_table_latex, file = file_path, include.rownames = FALSE, caption.placement = "top")
  
  # write as a csv
  write.csv(summary_list[[i]], file = paste0("summary_table_", i, ".csv"))
}

# ---------------- END ------------------------
# Go back to project's working directory
setwd("../../../../..")
rm(list = ls())
