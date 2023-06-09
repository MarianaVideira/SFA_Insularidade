# 0. Packages and Data -----------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(xtable)
library(ggpubr)
library(viridis)

# load data
load("0.DataBase/CSH.RData")
CSH_2019_0 = CSH |> filter(year == 2019, main == 0)
CSH_2019_1 = CSH |> filter(year == 2019, main == 1) |> drop_na()
# Review from here!! ----------- 


# 1. Plots: Volume Surgeries and costs -------------------------------------
# Create a folder for Descriptive statistics plots and set as new working directory
setwd("outputs")
dir.create("1.descriptive_stats")
setwd("1.descriptive_stats")
dir.create("plots")
setwd("plots")

# function that generates a plot for a given data set
generate_plot <- function(data,volume,cost, title, filter_year = NULL, xlabel = NULL, ylabel = NULL) {
  if(!is.null(filter_year)){
    data = data |> filter(year == filter_year)
  }
  
  data |> 
    select(year, region, {{volume}}, {{cost}}) |> 
    drop_na() 
  
  p = ggplot(data, aes(x = {{volume}}, y = {{cost}}, color = region)) +
    geom_point(alpha = 0.6) +
    ggtitle(title) +
    #scale_color_brewer(palette = "Accent") + 
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(family = "Helvetica"), #"Times-Roman", "Helvetica", "Courier"
          plot.margin = unit(c(.5,.5,.5,.5), "cm"),
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10))+
    labs(color = "Region")
  
  if(!is.null(xlabel)){
    p = p + xlab(xlabel)
  }
  
  if(!is.null(ylabel)){
    p = p + ylab(ylabel)
  }
  
   return(p)
}
  # 1.1 Surgeries and operational costs -------------------------------------
  
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(all, surg_total, c_op_acp, "Costs per patient", ylabel = "Operational cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(all, surg_total, c_op_acp, 
                          "Costs per patient: year 2021", filter_year = 2021, ylabel = "Operational cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(all, surg_total, c_op, "Total costs", ylabel = "Operational costs", xlabel = "Surgeries")
    plot4 = generate_plot(all, surg_total, c_op, "Total costs: year 2021", filter_year = 2021, "Operational costs", xlabel = "Surgeries")
    
    # C. Plot Grid 
    # create legend
    #grid = grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, top = "Operational costs per number of surgeries")
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")

  # 1.2 Surgeries and staff costs -------------------------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(all, surg_total, c_pess_acp, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(all, surg_total, c_pess_acp, "Costs per patient: year 2021", filter_year = 2021 , ylabel = "Operational cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(all, surg_total, c_pess, "Total costs", ylabel = "Staff costs", xlabel = "Surgeries")
    plot4 = generate_plot(all, surg_total, c_pess, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff cost", xlabel = "Surgeries")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_pess.pdf", grid,limitsize = F, width = 22, height = 20,units = "cm")
  
  # 1.3 Surgeries and staff adjusted costs ----------------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(all, surg_total, c_pess_a_acp, "Costs per patient",  ylabel = "Staff adjusted cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(all, surg_total, c_pess_a_acp, "Costs per patient: year 2021", ylabel = "Staff adjusted cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(all, surg_total, c_pess_a, "Total costs", ylabel = "Staff adjusted costs", xlabel = "Surgeries")
    plot4 = generate_plot(all, surg_total, c_pess_a, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Surgeries")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
  
# 2. Plots: Volume Appointments and costs ---------------------------------
  # 2.1 Appointments and operational costs -------------------------------------
  
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, app, c_op_acp, "Costs per patient", ylabel = "Operational cost per patient", xlabel = "Appointments")
  plot2 = generate_plot(all, app, c_op_acp, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Operational cost per patient", xlabel = "Appointments")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, app, c_op, "Total costs", ylabel = "Operational costs", xlabel = "Appointments")
  plot4 = generate_plot(all, app, c_op, "Total costs: year 2021", filter_year = 2021, ylabel = "Operational costs", xlabel = "Appointments")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("app_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
  
  # 2.2 Appointments and staff costs -------------------------------------------
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, app, c_pess_acp, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Appointments")
  plot2 = generate_plot(all, app, c_pess_acp, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Staff cost per patient", xlabel = "Appointments")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, app, c_pess, "Total costs", ylabel = "Staff costs", xlabel = "Appointments")
  plot4 = generate_plot(all, app, c_pess, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff costs", xlabel = "Appointments")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("app_pess.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
  
  # 2.3 Appointments and staff adjusted costs ----------------------------------
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, app, c_pess_a_acp, "Adjusted costs per patient", ylabel = "Staff adjusted cost per patient", xlabel = "Appointments")
  plot2 = generate_plot(all, app, c_pess_a_acp, "Adjusted costs per patient: year 2021", filter_year = 2021, ylabel = "Staff adjusted cost per patient", xlabel = "Appointments")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, app, c_pess_a, "Total adjusted costs", ylabel = "Staff adjusted costs", xlabel = "Appointments")
  plot4 = generate_plot(all, app, c_pess_a, "Total adjusted costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Appointments")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("app_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
  
  
# 3. Plots: Volume Inpatient admissions (in days) and costs ---------------------------------
  # 3.1 Admissions and operational costs -------------------------------------
  
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, stay, c_op_acp, "Costs per patient", ylabel = "Operational cost per patient", xlabel = "Admissions")
  plot2 = generate_plot(all, stay, c_op_acp, "Costs per patient: year 2021" , filter_year = 2021, ylabel = "Operational cost per patient", xlabel = "Admissions")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, stay, c_op, "Total costs", ylabel = "Operational costs", xlabel = "Admissions")
  plot4 = generate_plot(all, stay, c_op, "Total costs: year 2021", filter_year = 2021, ylabel = "Operational costs", xlabel = "Admissions")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("stay_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")

  # 3.2 Admissions and staff costs -------------------------------------------
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, stay, c_pess_acp, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Admissions")
  plot2 = generate_plot(all, stay, c_pess_acp, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Staff cost per patient", xlabel = "Admissions")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, stay, c_pess, "Total costs", ylabel = "Staff costs", xlabel = "Admissions")
  plot4 = generate_plot(all, stay, c_pess, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff costs", xlabel = "Admissions")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("stay_pess.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
  
  # 3.3 Admissions and staff adjusted costs ----------------------------------
  # A. Plot combined years and most recent (complete) year: Average Cost per patient
  plot1 = generate_plot(all, stay, c_pess_a_acp, "Adjusted costs per patient", ylabel = "Staff adjusted cost per patient", xlabel = "Admissions")
  plot2 = generate_plot(all, stay, c_pess_a_acp, "Adjusted costs per patient: year 2021", filter_year = 2021, ylabel = "Staff adjusted cost per patient", xlabel = "Admissions")
  
  # B. Plot combined years and most recent (complete) year: Total costs
  # Arrange the plots in a grid
  plot3 = generate_plot(all, stay, c_pess_a, "Total adjusted costs", ylabel = "Staff adjusted costs", xlabel = "Admissions")
  plot4 = generate_plot(all, stay, c_pess_a, "Total adjusted costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Admissions")
  
  # C. Plot Grid 
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("stay_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")

# 4. Other plots ----------------------------------------------------------
  dir.create("others")
  setwd("others")
  # 4.1 Surgeries and operational costs -------------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op_acp, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_op_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_op_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_op)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_op.pdf", grid_all)
  
  # 4.2 Surgeries and staff costs -------------------------------------------
  # A. Plot with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_acp, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_pess_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_pess_acp.pdf", grid_all)
  
  # B. Plot with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_pess)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_pess.pdf", grid_all)
  
  # 4.3 Surgeries and staff adjusted costs ----------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a_acp, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_pess_a_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_pess_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a, surg_total) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(surg_total, c_pess_a)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_surg_pess.pdf", grid_all)
  
  
  # 4.4 Appointments and operational costs -------------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op_acp, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_op_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_op_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_op)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_op.pdf", grid_all)
  
  # 4.5 Appointments and staff costs -------------------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_acp, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_pess_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_pess_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_pess)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_pess.pdf", grid_all)
  
  # 4.6 Appointments and staff adjusted costs ----------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a_acp, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_pess_a_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_pess_a_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a, app) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(app, c_pess_a)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_app_a_pess.pdf", grid_all)
  
  
  # 4.7 Admissions and operational costs -------------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op_acp, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_op_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_op_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_op, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_op)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all = grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_op.pdf", grid_all)
  
  # 4.8 Admissions and staff costs -------------------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_acp, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_pess_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_pess_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_pess)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_pess.pdf", grid_all)
  
  # 4.9 Admissions and staff adjusted costs ----------------------------------
  # A. Plot grid with all years for costs per patient
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a_acp, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_pess_a_acp)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_pess_a_acp.pdf", grid_all)
  
  # B. Plot grid with all years for total costs
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |> select(year, region, c_pess_a, stay) |>
      filter(year == year)
    
    # Create a ggplot object with the filtered data
    current_plot <- ggplot(filtered_data, aes(stay, c_pess_a)) +
      geom_point() +
      ggtitle(paste("Year:", year)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Add the current plot to the list
    plot_list[[year - 2014]] <- current_plot
    
  }
  
  # Arrange the plots in a grid and save plot as pdf
  grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
  ggsave("grid_stay_a_pess.pdf", grid_all)
  
# Clear space -------------------------------------------------------------
rm(plot1, plot2, plot3, plot4, plot_list, grid_all, grid, 
   filtered_data, current_plot, year)
setwd("../..")

# 5. Summary tables -------------------------------------------------------
# create new directory for summary tables
dir.create("sum_tables")
setwd("sum_tables")

  # 5.1. Costs per patient --------------------------------------------------
  
  # A. Create summary tables for each year using loop
  # Create an empty list to store the summary tables
  summary_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |>
      filter(year == year)
    
    # Calculate the summary statistics for variables DP
    op_p_summary <- filtered_data |>
      summarise(variable = "c_op_acp",
                mean = mean(c_op_acp, na.rm = TRUE),
                sd = sd(c_op_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_op_acp, na.rm = TRUE),
                max = max(c_op_acp, na.rm = TRUE),
                q25 = quantile(c_op_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_op_acp, 0.75, na.rm = TRUE))
    
    pess_acp_summary <- filtered_data |>
      summarise(variable = "c_pess_acp",
                mean = mean(c_pess_acp, na.rm = TRUE),
                sd = sd(c_pess_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_pess_acp, na.rm = TRUE),
                max = max(c_pess_acp, na.rm = TRUE),
                q25 = quantile(c_pess_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_pess_acp, 0.75, na.rm = TRUE))
    
    pess_a_acp_summary <- filtered_data |>
      summarise(variable = "c_pess_a_acp",
                mean = mean(c_pess_a_acp, na.rm = TRUE),
                sd = sd(c_pess_a_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_pess_a_acp, na.rm = TRUE),
                max = max(c_pess_a_acp, na.rm = TRUE),
                q25 = quantile(c_pess_a_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_pess_a_acp, 0.75, na.rm = TRUE))
    
    med_p_summary <- filtered_data |>
      summarise(variable = "c_med_acp",
                mean = mean(c_med_acp, na.rm = TRUE),
                sd = sd(c_med_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_med_acp, na.rm = TRUE),
                max = max(c_med_acp, na.rm = TRUE),
                q25 = quantile(c_med_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_med_acp, 0.75, na.rm = TRUE))
    
    mat_p_summary <- filtered_data |>
      summarise(variable = "c_mat_acp",
                mean = mean(c_mat_acp, na.rm = TRUE),
                sd = sd(c_mat_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_mat_acp, na.rm = TRUE),
                max = max(c_mat_acp, na.rm = TRUE),
                q25 = quantile(c_mat_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_mat_acp, 0.75, na.rm = TRUE))
    
    farm_p_summary <- filtered_data |>
      summarise(variable = "c_farm_acp",
                mean = mean(c_farm_acp, na.rm = TRUE),
                sd = sd(c_farm_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_farm_acp, na.rm = TRUE),
                max = max(c_farm_acp, na.rm = TRUE),
                q25 = quantile(c_farm_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_farm_acp, 0.75, na.rm = TRUE))
    
    fse_p_summary <- filtered_data |>
      summarise(variable = "c_fse_acp",
                mean = mean(c_fse_acp, na.rm = TRUE),
                sd = sd(c_fse_acp, na.rm = TRUE),
                obs = n(),
                min = min(c_fse_acp, na.rm = TRUE),
                max = max(c_fse_acp, na.rm = TRUE),
                q25 = quantile(c_fse_acp, 0.25, na.rm = TRUE),
                q75 = quantile(c_fse_acp, 0.75, na.rm = TRUE))

    # Combine the summary tables into a single table
    summary_table = bind_rows(op_p_summary, pess_acp_summary, pess_a_acp_summary, farm_p_summary, 
                               mat_p_summary, med_p_summary, fse_p_summary)
    
    # Add the summary table to the list
    summary_list[[year - 2014]] = summary_table
    
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
    summary_table_latex = xtable(summary_list[[i]], caption = paste("Summary Table average costs per patient", i+2014))
    
    # save file in working directory
    file_name = paste0("summary_table_costs_acp_", i+2014,".tex")
    file_path = file.path(getwd(), file_name)
    
    print(summary_table_latex, file = file_path, include.rownames = FALSE, caption.placement = "top")
  }

  setwd("..")

  # 5.2 Costs ---------------------------------------------------------------
  
  # A. Create summary tables for each year using loop
  # Create an empty list to store the summary tables
  summary_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |>
      filter(year == year)
    
    # Calculate the summary statistics for variables DP
    op_summary <- filtered_data |>
      summarise(variable = "c_op",
                mean = mean(c_op, na.rm = TRUE),
                sd = sd(c_op, na.rm = TRUE),
                obs = n(),
                min = min(c_op, na.rm = TRUE),
                max = max(c_op, na.rm = TRUE),
                q25 = quantile(c_op, 0.25, na.rm = TRUE),
                q75 = quantile(c_op, 0.75, na.rm = TRUE))
    
    pess_summary <- filtered_data |>
      summarise(variable = "c_pess",
                mean = mean(c_pess, na.rm = TRUE),
                sd = sd(c_pess, na.rm = TRUE),
                obs = n(),
                min = min(c_pess, na.rm = TRUE),
                max = max(c_pess, na.rm = TRUE),
                q25 = quantile(c_pess, 0.25, na.rm = TRUE),
                q75 = quantile(c_pess, 0.75, na.rm = TRUE))
    
    pess_a_summary <- filtered_data |>
      summarise(variable = "c_pess_a",
                mean = mean(c_pess_a, na.rm = TRUE),
                sd = sd(c_pess_a, na.rm = TRUE),
                obs = n(),
                min = min(c_pess_a, na.rm = TRUE),
                max = max(c_pess_a, na.rm = TRUE),
                q25 = quantile(c_pess_a, 0.25, na.rm = TRUE),
                q75 = quantile(c_pess_a, 0.75, na.rm = TRUE))
    
    med_summary <- filtered_data |>
      summarise(variable = "c_med",
                mean = mean(c_med, na.rm = TRUE),
                sd = sd(c_med, na.rm = TRUE),
                obs = n(),
                min = min(c_med, na.rm = TRUE),
                max = max(c_med, na.rm = TRUE),
                q25 = quantile(c_med, 0.25, na.rm = TRUE),
                q75 = quantile(c_med, 0.75, na.rm = TRUE))
    
    mat_summary <- filtered_data |>
      summarise(variable = "c_mat",
                mean = mean(c_mat, na.rm = TRUE),
                sd = sd(c_mat, na.rm = TRUE),
                obs = n(),
                min = min(c_mat, na.rm = TRUE),
                max = max(c_mat, na.rm = TRUE),
                q25 = quantile(c_mat, 0.25, na.rm = TRUE),
                q75 = quantile(c_mat, 0.75, na.rm = TRUE))
    
    farm_summary <- filtered_data |>
      summarise(variable = "c_farm",
                mean = mean(c_farm, na.rm = TRUE),
                sd = sd(c_farm, na.rm = TRUE),
                obs = n(),
                min = min(c_farm, na.rm = TRUE),
                max = max(c_farm, na.rm = TRUE),
                q25 = quantile(c_farm, 0.25, na.rm = TRUE),
                q75 = quantile(c_farm, 0.75, na.rm = TRUE))
    
    fse_summary <- filtered_data |>
      summarise(variable = "c_fse",
                mean = mean(c_fse, na.rm = TRUE),
                sd = sd(c_fse, na.rm = TRUE),
                obs = n(),
                min = min(c_fse, na.rm = TRUE),
                max = max(c_fse, na.rm = TRUE),
                q25 = quantile(c_fse, 0.25, na.rm = TRUE),
                q75 = quantile(c_fse, 0.75, na.rm = TRUE))
    
    # Combine the summary tables into a single table
    summary_table = bind_rows(op_summary, pess_summary, pess_a_summary, farm_summary, 
                              mat_summary, med_summary, fse_summary)
    
    # Add the summary table to the list
    summary_list[[year - 2014]] = summary_table
    
    # Save each summary table as a PDF file
    pdf(paste0("costs_summary_table_", year, ".pdf"), width = 10, height = 4)
    grid.table(summary_table, rows = NULL)
    dev.off()
    
  }
  
  # B. Latex tables
  setwd("latex")
  
  # Loop for Latex tables
  for (i in 1:length(summary_list)) {
    summary_table_latex = xtable(summary_list[[i]], caption = paste("Summary Table average costs per patient", i+2014))
    
    # save file in working directory
    file_name = paste0("summary_table_costs", i+2014,".tex")
    file_path = file.path(getwd(), file_name)
    
    print(summary_table_latex, file = file_path, include.rownames = FALSE, caption.placement = "top")
  }
  
  setwd("..")
  # 5.3 Volume -------------------------------------------------------------
  
  # Create an empty list to store the summary tables
  summary_list <- list()
  
  # Loop through each year from 2015 to 2020
  for (year in 2015:2020) {
    
    # Filter the data for the current year
    filtered_data <- all |>
      filter(year == year)
    
    # Calculate the summary statistics for variables DP
    beds_summary <- filtered_data |>
      summarise(variable = "beds",
                mean = mean(beds, na.rm = TRUE),
                sd = sd(beds, na.rm = TRUE),
                obs = n(),
                min = min(beds, na.rm = TRUE),
                max = max(beds, na.rm = TRUE),
                q25 = quantile(beds, 0.25, na.rm = TRUE),
                q75 = quantile(beds, 0.75, na.rm = TRUE))
    
    
    mean_days_surg_summary <- filtered_data |>
      summarise(variable = "mean_days_surg_summary",
                mean = mean(mean_days_surg, na.rm = TRUE),
                sd = sd(mean_days_surg, na.rm = TRUE),
                obs = n(),
                min = min(mean_days_surg, na.rm = TRUE),
                max = max(mean_days_surg, na.rm = TRUE),
                q25 = quantile(mean_days_surg, 0.25, na.rm = TRUE),
                q75 = quantile(mean_days_surg, 0.75, na.rm = TRUE))
    
    scheduled_sur_summary <- filtered_data |>
      summarise(variable = "scheduled_days",
                mean = mean(scheduled_sur, na.rm = TRUE),
                sd = sd(scheduled_sur, na.rm = TRUE),
                obs = n(),
                min = min(scheduled_sur, na.rm = TRUE),
                max = max(scheduled_sur, na.rm = TRUE),
                q25 = quantile(scheduled_sur, 0.25, na.rm = TRUE),
                q75 = quantile(scheduled_sur, 0.75, na.rm = TRUE))
    
    
    mean_wait_summary <- filtered_data |>
      summarise(variable = "mean_wait",
                mean = mean(mean_wait, na.rm = TRUE),
                sd = sd(mean_wait, na.rm = TRUE),
                obs = n(),
                min = min(mean_wait, na.rm = TRUE),
                max = max(mean_wait, na.rm = TRUE),
                q25 = quantile(mean_wait, 0.25, na.rm = TRUE),
                q75 = quantile(mean_wait, 0.75, na.rm = TRUE))
    
    app_summary <- filtered_data |>
      summarise(variable = "app",
                mean = mean(app, na.rm = TRUE),
                sd = sd(app, na.rm = TRUE),
                obs = n(),
                min = min(app, na.rm = TRUE),
                max = max(app, na.rm = TRUE),
                q25 = quantile(app, 0.25, na.rm = TRUE),
                q75 = quantile(app, 0.75, na.rm = TRUE))
    
    f_app_summary <- filtered_data |>
      summarise(variable = "f_app",
                mean = mean(f_app, na.rm = TRUE),
                sd = sd(f_app, na.rm = TRUE),
                obs = n(),
                min = min(f_app, na.rm = TRUE),
                max = max(f_app, na.rm = TRUE),
                q25 = quantile(f_app, 0.25, na.rm = TRUE),
                q75 = quantile(f_app, 0.75, na.rm = TRUE))
    
    discharge_summary <- filtered_data |>
      summarise(variable = "discharge",
                mean = mean(discharge, na.rm = TRUE),
                sd = sd(discharge, na.rm = TRUE),
                obs = n(),
                min = min(discharge, na.rm = TRUE),
                max = max(discharge, na.rm = TRUE),
                q25 = quantile(discharge, 0.25, na.rm = TRUE),
                q75 = quantile(discharge, 0.75, na.rm = TRUE))
    
    
    stay_summary <- filtered_data |>
      summarise(variable = "stay",
                mean = mean(stay, na.rm = TRUE),
                sd = sd(stay, na.rm = TRUE),
                obs = n(),
                min = min(stay, na.rm = TRUE),
                max = max(stay, na.rm = TRUE),
                q25 = quantile(stay, 0.25, na.rm = TRUE),
                q75 = quantile(stay, 0.75, na.rm = TRUE))
    
    surg_p_summary <- filtered_data |>
      summarise(variable = "surg_p",
                mean = mean(surg_p, na.rm = TRUE),
                sd = sd(surg_p, na.rm = TRUE),
                obs = n(),
                min = min(surg_p, na.rm = TRUE),
                max = max(surg_p, na.rm = TRUE),
                q25 = quantile(surg_p, 0.25, na.rm = TRUE),
                q75 = quantile(surg_p, 0.75, na.rm = TRUE))
    
    surg_u_summary <- filtered_data |>
      summarise(variable = "surg_u",
                mean = mean(surg_u, na.rm = TRUE),
                sd = sd(surg_u, na.rm = TRUE),
                obs = n(),
                min = min(surg_u, na.rm = TRUE),
                max = max(surg_u, na.rm = TRUE),
                q25 = quantile(surg_u, 0.25, na.rm = TRUE),
                q75 = quantile(surg_u, 0.75, na.rm = TRUE))
    
    surg_a_summary <- filtered_data |>
      summarise(variable = "surg_a",
                mean = mean(surg_a, na.rm = TRUE),
                sd = sd(surg_a, na.rm = TRUE),
                obs = n(),
                min = min(surg_a, na.rm = TRUE),
                max = max(surg_a, na.rm = TRUE),
                q25 = quantile(surg_a, 0.25, na.rm = TRUE),
                q75 = quantile(surg_a, 0.75, na.rm = TRUE))
    
    surg_c_summary <- filtered_data |>
      summarise(variable = "surg_c",
                mean = mean(surg_c, na.rm = TRUE),
                sd = sd(surg_c, na.rm = TRUE),
                obs = n(),
                min = min(surg_c, na.rm = TRUE),
                max = max(surg_c, na.rm = TRUE),
                q25 = quantile(surg_c, 0.25, na.rm = TRUE),
                q75 = quantile(surg_c, 0.75, na.rm = TRUE))
    
    surg_total_summary <- filtered_data |>
      summarise(variable = "surg_total",
                mean = mean(surg_total, na.rm = TRUE),
                sd = sd(surg_total, na.rm = TRUE),
                obs = n(),
                min = min(surg_total, na.rm = TRUE),
                max = max(surg_total, na.rm = TRUE),
                q25 = quantile(surg_total, 0.25, na.rm = TRUE),
                q75 = quantile(surg_total, 0.75, na.rm = TRUE))
    
    
    # Combine the summary tables into a single table
    summary_table <- bind_rows(surg_total_summary, surg_c_summary, surg_a_summary,
                               surg_u_summary, surg_p_summary, stay_summary,
                               discharge_summary, f_app_summary, app_summary,
                               mean_wait_summary, scheduled_sur_summary, 
                               mean_days_surg_summary, beds_summary)
    
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


# Clear space -------------------------------------------------------------
  rm(list=setdiff(ls(), c("all", "all_balanced")))
  
# Go back to project's working directory
setwd("../../..")
  
# ---------------- END ------------------------
