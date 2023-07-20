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
load("0.DataBase/pop.RData")
# I. Scatter Plots (reference year) ----------------------------------------

# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("1.descriptive_stats")
setwd("1.descriptive_stats")
dir.create("CSH")
setwd("CSH")
dir.create("plots")
setwd("plots")

# function that generates a plot for a given data set
generate_plot <- function(data,volume,cost, title, filter_year = NULL, xlabel = NULL, ylabel = NULL) {
  if(!is.null(filter_year)){
    data = data |> filter(year == filter_year)}
    
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
  
  ## 1. Plots: Volume Surgeries and costs ------------------------------------
    ### 1.1 Surgeries and operational costs --------------------------------
    
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, surg, avg_operational, "Costs per patient", ylabel = "CMVMC cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(CSH, surg, avg_operational, 
                          "Costs per patient: year 2021", filter_year = 2021, ylabel = "CMVMC cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, surg, operational, "Total costs", ylabel = "CMVMC costs", xlabel = "Surgeries")
    plot4 = generate_plot(CSH, surg, operational, "Total costs: year 2021", filter_year = 2021, "CMVMC costs", xlabel = "Surgeries")
    
    # C. Plot Grid 
    # create legend
    #grid = grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, top = "Operational costs per number of surgeries")
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    ### 1.2 Surgeries and staff costs --------------------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, surg, avg_staff, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(CSH, surg, avg_staff, "Costs per patient: year 2021", filter_year = 2021 , ylabel = "Operational cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, surg, staff, "Total costs", ylabel = "Staff costs", xlabel = "Surgeries")
    plot4 = generate_plot(CSH, surg, staff, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff cost", xlabel = "Surgeries")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_pess.pdf", grid,limitsize = F, width = 22, height = 20,units = "cm")
    
    ### 1.3 Surgeries and staff adjusted costs -----------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, surg, avg_staff_adjusted, "Costs per patient",  ylabel = "Staff adjusted cost per patient", xlabel = "Surgeries")
    plot2 = generate_plot(CSH, surg, avg_staff_adjusted, "Costs per patient: year 2021", ylabel = "Staff adjusted cost per patient", xlabel = "Surgeries")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, surg, staff_adjusted, "Total costs", ylabel = "Staff adjusted costs", xlabel = "Surgeries")
    plot4 = generate_plot(CSH, surg, staff_adjusted, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Surgeries")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("surg_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    
  ## 2. Plots: Volume Appointments and costs -------------------------------
    ### 2.1 Appointments and operational costs -----------------------------
    
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, app, avg_operational, "Costs per patient", ylabel = "Operational cost per patient", xlabel = "Appointments")
    plot2 = generate_plot(CSH, app, avg_operational, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Operational cost per patient", xlabel = "Appointments")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, app, operational, "Total costs", ylabel = "Operational costs", xlabel = "Appointments")
    plot4 = generate_plot(CSH, app, operational, "Total costs: year 2021", filter_year = 2021, ylabel = "Operational costs", xlabel = "Appointments")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("app_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    ### 2.2 Appointments and staff costs -----------------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, app, avg_staff, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Appointments")
    plot2 = generate_plot(CSH, app, avg_staff, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Staff cost per patient", xlabel = "Appointments")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, app, staff, "Total costs", ylabel = "Staff costs", xlabel = "Appointments")
    plot4 = generate_plot(CSH, app, staff, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff costs", xlabel = "Appointments")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("app_pess.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    ### 2.3 Appointments and staff adjusted costs --------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, app, avg_staff_adjusted, "Adjusted costs per patient", ylabel = "Staff adjusted cost per patient", xlabel = "Appointments")
    plot2 = generate_plot(CSH, app, avg_staff_adjusted, "Adjusted costs per patient: year 2021", filter_year = 2021, ylabel = "Staff adjusted cost per patient", xlabel = "Appointments")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, app, staff_adjusted, "Total adjusted costs", ylabel = "Staff adjusted costs", xlabel = "Appointments")
    plot4 = generate_plot(CSH, app, staff_adjusted, "Total adjusted costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Appointments")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("app_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    
  ## 3. Plots: Volume Inpatient days and costs -----------------------------
    ### 3.1 In_days and operational costs ----------------------------------
    
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, in_days, avg_operational, "Costs per patient", ylabel = "Operational cost per patient", xlabel = "Inpatient days")
    plot2 = generate_plot(CSH, in_days, avg_operational, "Costs per patient: year 2021" , filter_year = 2021, ylabel = "Operational cost per patient", xlabel = "Admissions")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, in_days, operational, "Total costs", ylabel = "Operational costs", xlabel = "Inpatient days")
    plot4 = generate_plot(CSH, in_days, operational, "Total costs: year 2021", filter_year = 2021, ylabel = "Operational costs", xlabel = "Admissions")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("stay_op.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    ### 3.2 Admissions and staff adjusted costs ----------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, in_days, avg_staff_adjusted, "Costs per patient", ylabel = "Staff cost per patient", xlabel = "Admissions")
    plot2 = generate_plot(CSH, in_days, avg_staff_adjusted, "Costs per patient: year 2021", filter_year = 2021, ylabel = "Staff cost per patient", xlabel = "Admissions")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, in_days, staff_adjusted, "Total costs", ylabel = "Staff costs", xlabel = "Admissions")
    plot4 = generate_plot(CSH, in_days, staff_adjusted, "Total costs: year 2021", filter_year = 2021, ylabel = "Staff costs", xlabel = "Admissions")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("stay_pess.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
    ### 3.3 Admissions and staff costs -------------------------------------
    # A. Plot combined years and most recent (complete) year: Average Cost per patient
    plot1 = generate_plot(CSH, in_days, avg_staff, "Adjusted costs per patient", ylabel = "Staff adjusted cost per patient", xlabel = "Admissions")
    plot2 = generate_plot(CSH, in_days, avg_staff, "Adjusted costs per patient: year 2021", filter_year = 2021, ylabel = "Staff adjusted cost per patient", xlabel = "Admissions")
    
    # B. Plot combined years and most recent (complete) year: Total costs
    # Arrange the plots in a grid
    plot3 = generate_plot(CSH, in_days, staff, "Total adjusted costs", ylabel = "Staff adjusted costs", xlabel = "Admissions")
    plot4 = generate_plot(CSH, in_days, staff, "Total adjusted costs: year 2021", filter_year = 2021, ylabel = "Staff adjusted costs", xlabel = "Admissions")
    
    # C. Plot Grid 
    grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
    ggsave("stay_pess_a.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
    
  ## 4. Other plots --------------------------------------------------------
  dir.create("others")
  setwd("others")
    ### 4.1 Surgeries and operational costs --------------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_operational, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(surg, avg_operational)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
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
      filtered_data <- CSH |> select(year, region, operational, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(surg, operational)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] <- current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_surg_op.pdf", grid_all)
    
    ### 4.2 Surgeries and staff costs --------------------------------------
    # A. Plot with all years for costs per patient
    # Create an empty list to store the plots
    plot_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data <- CSH |> select(year, region, avg_staff, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(surg, avg_staff)) +
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
      filtered_data <- CSH |> select(year, region, staff, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(surg, staff)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] <- current_plot
      
    }
    
    # Arrange the plots in a and save plot as pdf
    grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_surg_pess.pdf", grid_all)
    
    ### 4.3 Surgeries and staff adjusted costs -----------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data= CSH |> select(year, region, avg_staff_adjusted, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(surg, avg_staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_surg_pess_acp.pdf", grid_all)
    
    # B. Plot grid with all years for total costs
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, staff_adjusted, surg) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(surg, staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_surg_pess.pdf", grid_all)
    
    
    ### 4.4 Appointments and operational costs -----------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_operational, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(app, avg_operational)) +
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
      filtered_data = CSH |> select(year, region, operational, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(app, operational)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_app_op.pdf", grid_all)
    
    ### 4.5 Appointments and staff costs -----------------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_staff, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(app, avg_staff)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
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
      filtered_data = CSH |> select(year, region, staff, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(app, staff)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] <- current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_app_pess.pdf", grid_all)
    
    ### 4.6 Appointments and staff adjusted costs --------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_staff_adjusted, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(app, avg_staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_app_pess_a_acp.pdf", grid_all)
    
    # B. Plot grid with all years for total costs
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, staff_adjusted, app) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(app, staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_app_a_pess.pdf", grid_all)
    
    
    ### 4.7 In_days and operational costs ----------------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_operational, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot <- ggplot(filtered_data, aes(in_days, avg_operational)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
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
      filtered_data = CSH |> select(year, region, operational, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(in_days, operational)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_stay_op.pdf", grid_all)
    
    ### 4.8 In_days and staff costs ----------------------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list = list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_staff, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(in_days, avg_staff)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_stay_pess_acp.pdf", grid_all)
    
    # B. Plot grid with all years for total costs
    # Create an empty list to store the plots
    plot_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, staff, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(in_days, staff)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] <- current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all <- grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_stay_pess.pdf", grid_all)
    
    ### 4.9 Admissions and staff adjusted costs ---------------------------
    # A. Plot grid with all years for costs per patient
    # Create an empty list to store the plots
    plot_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, avg_staff_adjusted, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(in_days, avg_staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_stay_pess_a_acp.pdf", grid_all)
    
    # B. Plot grid with all years for total costs
    # Create an empty list to store the plots
    plot_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |> select(year, region, staff_adjusted, in_days) |>
        filter(year == year)
      
      # Create a ggplot object with the filtered data
      current_plot = ggplot(filtered_data, aes(in_days, staff_adjusted)) +
        geom_point() +
        ggtitle(paste("Year:", year)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Add the current plot to the list
      plot_list[[year - 2014]] = current_plot
      
    }
    
    # Arrange the plots in a grid and save plot as pdf
    grid_all = grid.arrange(grobs = plot_list, ncol = 3)
    ggsave("grid_stay_a_pess.pdf", grid_all)
    
# Clear space -------------------------------------------------------------
rm(list = setdiff(ls(), c("CSH")))
setwd("../..")

# II. Summary Tables ------------------------------------------------------
dir.create("summary_tables")
setwd("summary_tables")
dir.create("1.per_year")
setwd("1.per_year")
  ## 1. Summary tables per year -------------------------------------------
    ### 1.1 Costs per patient ---------------------------------------------
    
    # A. Create summary tables for each year using loop
    # Create an empty list to store the summary tables
    summary_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |>
        filter(year == year)
      
      # Calculate the summary statistics for variables DP
      op_p_summary = filtered_data |>
        summarise(variable = "average operational costs",
                  mean = mean(avg_operational, na.rm = TRUE),
                  sd = sd(avg_operational, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_operational, na.rm = TRUE),
                  max = max(avg_operational, na.rm = TRUE),
                  q25 = quantile(avg_operational, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_operational, 0.75, na.rm = TRUE))
      
      pess_acp_summary = filtered_data |>
        summarise(variable = "average staff costs",
                  mean = mean(avg_staff, na.rm = TRUE),
                  sd = sd(avg_staff, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_staff, na.rm = TRUE),
                  max = max(avg_staff, na.rm = TRUE),
                  q25 = quantile(avg_staff, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_staff, 0.75, na.rm = TRUE))
      
      pess_a_acp_summary = filtered_data |>
        summarise(variable = "avgerage staff adjusted costs",
                  mean = mean(avg_staff_adjusted, na.rm = TRUE),
                  sd = sd(avg_staff_adjusted, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_staff_adjusted, na.rm = TRUE),
                  max = max(avg_staff_adjusted, na.rm = TRUE),
                  q25 = quantile(avg_staff_adjusted, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_staff_adjusted, 0.75, na.rm = TRUE))
      
      med_p_summary <- filtered_data |>
        summarise(variable = "average medicine costs",
                  mean = mean(avg_medicine, na.rm = TRUE),
                  sd = sd(avg_medicine, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_medicine, na.rm = TRUE),
                  max = max(avg_medicine, na.rm = TRUE),
                  q25 = quantile(avg_medicine, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_medicine, 0.75, na.rm = TRUE))
      
      mat_p_summary <- filtered_data |>
        summarise(variable = "average materials costs",
                  mean = mean(avg_materials, na.rm = TRUE),
                  sd = sd(avg_materials, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_materials, na.rm = TRUE),
                  max = max(avg_materials, na.rm = TRUE),
                  q25 = quantile(avg_materials, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_materials, 0.75, na.rm = TRUE))
      
      farm_p_summary <- filtered_data |>
        summarise(variable = "average pharma costs",
                  mean = mean(avg_pharma, na.rm = TRUE),
                  sd = sd(avg_pharma, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_pharma, na.rm = TRUE),
                  max = max(avg_pharma, na.rm = TRUE),
                  q25 = quantile(avg_pharma, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_pharma, 0.75, na.rm = TRUE))
      
      fse_p_summary <- filtered_data |>
        summarise(variable = "average fse",
                  mean = mean(avg_fse, na.rm = TRUE),
                  sd = sd(avg_fse, na.rm = TRUE),
                  obs = n(),
                  min = min(avg_fse, na.rm = TRUE),
                  max = max(avg_fse, na.rm = TRUE),
                  q25 = quantile(avg_fse, 0.25, na.rm = TRUE),
                  q75 = quantile(avg_fse, 0.75, na.rm = TRUE))
  
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
  
    ### 1.2 Costs ---------------------------------------------------------
    
    # A. Create summary tables for each year using loop
    # Create an empty list to store the summary tables
    summary_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data = CSH |>
        filter(year == year)
      
      # Calculate the summary statistics for variables DP
      op_summary <- filtered_data |>
        summarise(variable = "operational costs",
                  mean = mean(operational, na.rm = TRUE),
                  sd = sd(operational, na.rm = TRUE),
                  obs = n(),
                  min = min(operational, na.rm = TRUE),
                  max = max(operational, na.rm = TRUE),
                  q25 = quantile(operational, 0.25, na.rm = TRUE),
                  q75 = quantile(operational, 0.75, na.rm = TRUE))
      
      pess_summary <- filtered_data |>
        summarise(variable = "staff costs",
                  mean = mean(staff, na.rm = TRUE),
                  sd = sd(staff, na.rm = TRUE),
                  obs = n(),
                  min = min(staff, na.rm = TRUE),
                  max = max(staff, na.rm = TRUE),
                  q25 = quantile(staff, 0.25, na.rm = TRUE),
                  q75 = quantile(staff, 0.75, na.rm = TRUE))
      
      pess_a_summary = filtered_data |>
        summarise(variable = "staff adjusted costs",
                  mean = mean(staff_adjusted, na.rm = TRUE),
                  sd = sd(staff_adjusted, na.rm = TRUE),
                  obs = n(),
                  min = min(staff_adjusted, na.rm = TRUE),
                  max = max(staff_adjusted, na.rm = TRUE),
                  q25 = quantile(staff_adjusted, 0.25, na.rm = TRUE),
                  q75 = quantile(staff_adjusted, 0.75, na.rm = TRUE))
      
      med_summary = filtered_data |>
        summarise(variable = "medicine costs",
                  mean = mean(medicine, na.rm = TRUE),
                  sd = sd(medicine, na.rm = TRUE),
                  obs = n(),
                  min = min(medicine, na.rm = TRUE),
                  max = max(medicine, na.rm = TRUE),
                  q25 = quantile(medicine, 0.25, na.rm = TRUE),
                  q75 = quantile(medicine, 0.75, na.rm = TRUE))
      
      mat_summary <- filtered_data |>
        summarise(variable = "material costs",
                  mean = mean(materials, na.rm = TRUE),
                  sd = sd(materials, na.rm = TRUE),
                  obs = n(),
                  min = min(materials, na.rm = TRUE),
                  max = max(materials, na.rm = TRUE),
                  q25 = quantile(materials, 0.25, na.rm = TRUE),
                  q75 = quantile(materials, 0.75, na.rm = TRUE))
      
      farm_summary <- filtered_data |>
        summarise(variable = "pharmaceutical costs",
                  mean = mean(pharma, na.rm = TRUE),
                  sd = sd(pharma, na.rm = TRUE),
                  obs = n(),
                  min = min(pharma, na.rm = TRUE),
                  max = max(pharma, na.rm = TRUE),
                  q25 = quantile(pharma, 0.25, na.rm = TRUE),
                  q75 = quantile(pharma, 0.75, na.rm = TRUE))
      
      fse_summary <- filtered_data |>
        summarise(variable = "fse costs",
                  mean = mean(fse, na.rm = TRUE),
                  sd = sd(fse, na.rm = TRUE),
                  obs = n(),
                  min = min(fse, na.rm = TRUE),
                  max = max(fse, na.rm = TRUE),
                  q25 = quantile(fse, 0.25, na.rm = TRUE),
                  q75 = quantile(fse, 0.75, na.rm = TRUE))
      
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
    ### 1.3 Volume -------------------------------------------------------------
    
    # Create an empty list to store the summary tables
    summary_list <- list()
    
    # Loop through each year from 2015 to 2020
    for (year in 2015:2020) {
      
      # Filter the data for the current year
      filtered_data <- CSH |>
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
      
      beds_summary <- filtered_data |>
        summarise(variable = "RO",
                  mean = mean(RO, na.rm = TRUE),
                  sd = sd(RO, na.rm = TRUE),
                  obs = n(),
                  min = min(RO, na.rm = TRUE),
                  max = max(RO, na.rm = TRUE),
                  q25 = quantile(RO, 0.25, na.rm = TRUE),
                  q75 = quantile(RO, 0.75, na.rm = TRUE))
      
      mean_days_surg_summary <- filtered_data |>
        summarise(variable = "mean_wait_summary",
                  mean = mean(mean_wait, na.rm = TRUE),
                  sd = sd(mean_wait, na.rm = TRUE),
                  obs = n(),
                  min = min(mean_wait, na.rm = TRUE),
                  max = max(mean_wait, na.rm = TRUE),
                  q25 = quantile(mean_wait, 0.25, na.rm = TRUE),
                  q75 = quantile(mean_wait, 0.75, na.rm = TRUE))
      
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
      
      discharge_summary <- filtered_data |>
        summarise(variable = "patient discharge",
                  mean = mean(patient_dis, na.rm = TRUE),
                  sd = sd(patient_dis, na.rm = TRUE),
                  obs = n(),
                  min = min(patient_dis, na.rm = TRUE),
                  max = max(patient_dis, na.rm = TRUE),
                  q25 = quantile(patient_dis, 0.25, na.rm = TRUE),
                  q75 = quantile(patient_dis, 0.75, na.rm = TRUE))
      
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
        summarise(variable = "surgerie",
                  mean = mean(surg, na.rm = TRUE),
                  sd = sd(surg, na.rm = TRUE),
                  obs = n(),
                  min = min(surg, na.rm = TRUE),
                  max = max(surg, na.rm = TRUE),
                  q25 = quantile(surg, 0.25, na.rm = TRUE),
                  q75 = quantile(surg, 0.75, na.rm = TRUE))
      
      
      # Combine the summary tables into a single table
      summary_table <- bind_rows(surg_total_summary, surg_c_summary, surg_a_summary,
                                 surg_u_summary, surg_p_summary,
                                 discharge_summary, app_summary,
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
  
    setwd("../..")
  
  ## 2. Summary tables per Azores/Madeira ---------------------------------
  dir.create("2.Portugal_Islands")
  setwd("2.Portugal_Islands")
  
  # A. Create summary tables for each year using loop
  # Create an empty list to store the summary tables
  summary_list = list()
  CSH$azo = as.factor(CSH$azo)
  
  
  for (azo_level in levels(CSH$azo)) {
    
    # Filter the data for the current year
    filtered_data <- CSH |>
      filter(azo == azo_level)
    
    # Calculate the summary statistics for variables Costs
    operational_summary = filtered_data |>
      summarise(variable = "Operational Costs",
                mean = mean(operational, na.rm = TRUE),
                sd = sd(operational, na.rm = TRUE),
                min = min(operational, na.rm = TRUE),
                max = max(operational, na.rm = TRUE),
                q25 = quantile(operational, 0.25, na.rm = TRUE),
                q75 = quantile(operational, 0.75, na.rm = TRUE))
    
    operational_ACSS_summary = filtered_data |>
      summarise(variable = "Operational ACSS Costs",
                mean = mean(operational_ACSS, na.rm = TRUE),
                sd = sd(operational_ACSS, na.rm = TRUE),
                min = min(operational_ACSS, na.rm = TRUE),
                max = max(operational_ACSS, na.rm = TRUE),
                q25 = quantile(operational_ACSS, 0.25, na.rm = TRUE),
                q75 = quantile(operational_ACSS, 0.75, na.rm = TRUE))
    
    staff_summary = filtered_data |>
      summarise(variable = "Staff Costs",
                mean = mean(staff, na.rm = TRUE),
                sd = sd(staff, na.rm = TRUE),
                min = min(staff, na.rm = TRUE),
                max = max(staff, na.rm = TRUE),
                q25 = quantile(staff, 0.25, na.rm = TRUE),
                q75 = quantile(staff, 0.75, na.rm = TRUE))
    
    staff_adj_summary = filtered_data |>
      summarise(variable = "Staff Adjusted Costs",
                mean = mean(staff_adjusted, na.rm = TRUE),
                sd = sd(staff_adjusted, na.rm = TRUE),
                min = min(staff_adjusted, na.rm = TRUE),
                max = max(staff_adjusted, na.rm = TRUE),
                q25 = quantile(staff_adjusted, 0.25, na.rm = TRUE),
                q75 = quantile(staff_adjusted, 0.75, na.rm = TRUE))
    
    pharma_summary = filtered_data |>
      summarise(variable = "Pharmaceuticals Cost",
                mean = mean(pharma, na.rm = TRUE),
                sd = sd(pharma, na.rm = TRUE),
                min = min(pharma, na.rm = TRUE),
                max = max(pharma, na.rm = TRUE),
                q25 = quantile(pharma, 0.25, na.rm = TRUE),
                q75 = quantile(pharma, 0.75, na.rm = TRUE))
    
    medicine_summary = filtered_data |>
      summarise(variable = "Medicine Cost",
                mean = mean(medicine, na.rm = TRUE),
                sd = sd(medicine, na.rm = TRUE),
                min = min(medicine, na.rm = TRUE),
                max = max(medicine, na.rm = TRUE),
                q25 = quantile(medicine, 0.25, na.rm = TRUE),
                q75 = quantile(medicine, 0.75, na.rm = TRUE))
    
    fse_summary = filtered_data |>
      summarise(variable = "FSE Costs",
                mean = mean(fse, na.rm = TRUE),
                sd = sd(fse, na.rm = TRUE),
                min = min(fse, na.rm = TRUE),
                max = max(fse, na.rm = TRUE),
                q25 = quantile(fse, 0.25, na.rm = TRUE),
                q75 = quantile(fse, 0.75, na.rm = TRUE))
    
    materials_summary = filtered_data |>
      summarise(variable = "Materials Costs",
                mean = mean(materials, na.rm = TRUE),
                sd = sd(materials, na.rm = TRUE),
                min = min(materials, na.rm = TRUE),
                max = max(materials, na.rm = TRUE),
                q25 = quantile(materials, 0.25, na.rm = TRUE),
                q75 = quantile(materials, 0.75, na.rm = TRUE))
    
    # Calculate the summary statistics for variables Volume
    in_days_summary = filtered_data |>
      summarise(variable = "Inpatient days",
                mean = mean(in_days, na.rm = TRUE),
                sd = sd(in_days, na.rm = TRUE),
                min = min(in_days, na.rm = TRUE),
                max = max(in_days, na.rm = TRUE),
                q25 = quantile(in_days, 0.25, na.rm = TRUE),
                q75 = quantile(in_days, 0.75, na.rm = TRUE))
    
    surge_summary = filtered_data |>
      summarise(variable = "Surgeries",
                mean = mean(surg, na.rm = TRUE),
                sd = sd(surg, na.rm = TRUE),
                min = min(surg, na.rm = TRUE),
                max = max(surg, na.rm = TRUE),
                q25 = quantile(surg, 0.25, na.rm = TRUE),
                q75 = quantile(surg, 0.75, na.rm = TRUE))
    
    app_summary = filtered_data |>
      summarise(variable = "Appointments",
                mean = mean(app, na.rm = TRUE),
                sd = sd(app, na.rm = TRUE),
                min = min(app, na.rm = TRUE),
                max = max(app, na.rm = TRUE),
                q25 = quantile(app, 0.25, na.rm = TRUE),
                q75 = quantile(app, 0.75, na.rm = TRUE))
    
    urge_summary = filtered_data |>
      summarise(variable = "Urgencies",
                mean = mean(urge, na.rm = TRUE),
                sd = sd(urge, na.rm = TRUE),
                min = min(urge, na.rm = TRUE),
                max = max(urge, na.rm = TRUE),
                q25 = quantile(urge, 0.25, na.rm = TRUE),
                q75 = quantile(urge, 0.75, na.rm = TRUE))
    
    RO_summary = filtered_data |>
      summarise(variable = "Rate of Occupancy",
                mean = mean(RO, na.rm = TRUE),
                sd = sd(RO, na.rm = TRUE),
                min = min(RO, na.rm = TRUE),
                max = max(RO, na.rm = TRUE),
                q25 = quantile(RO, 0.25, na.rm = TRUE),
                q75 = quantile(RO, 0.75, na.rm = TRUE))
    
    beds_summary = filtered_data |>
      summarise(variable = "Beds",
                mean = mean(beds, na.rm = TRUE),
                sd = sd(beds, na.rm = TRUE),
                min = min(beds, na.rm = TRUE),
                max = max(beds, na.rm = TRUE),
                q25 = quantile(beds, 0.25, na.rm = TRUE),
                q75 = quantile(beds, 0.75, na.rm = TRUE))
    
    wait_days_summary = filtered_data |>
      summarise(variable = "Mean Wait for Surgeries",
                mean = mean(mean_wait, na.rm = TRUE),
                sd = sd(mean_wait, na.rm = TRUE),
                min = min(mean_wait, na.rm = TRUE),
                max = max(mean_wait, na.rm = TRUE),
                q25 = quantile(mean_wait, 0.25, na.rm = TRUE),
                q75 = quantile(mean_wait, 0.75, na.rm = TRUE))
    
    beds_summary = filtered_data |>
      summarise(variable = "Beds",
                mean = mean(beds, na.rm = TRUE),
                sd = sd(beds, na.rm = TRUE),
                min = min(beds, na.rm = TRUE),
                max = max(beds, na.rm = TRUE),
                q25 = quantile(beds, 0.25, na.rm = TRUE),
                q75 = quantile(beds, 0.75, na.rm = TRUE))
    
    fem_summary = filtered_data |>
      summarise(variable = "prop fem",
                mean = mean(fem, na.rm = TRUE),
                sd = sd(fem, na.rm = TRUE),
                min = min(fem, na.rm = TRUE),
                max = max(fem, na.rm = TRUE),
                q25 = quantile(fem, 0.25, na.rm = TRUE),
                q75 = quantile(fem, 0.75, na.rm = TRUE))
    
    den_summary = filtered_data |>
      summarise(variable = "den pop",
                mean = mean(den, na.rm = TRUE),
                sd = sd(den, na.rm = TRUE),
                min = min(den, na.rm = TRUE),
                max = max(den, na.rm = TRUE),
                q25 = quantile(den, 0.25, na.rm = TRUE),
                q75 = quantile(den, 0.75, na.rm = TRUE))
    
    aging_summary = filtered_data |>
      summarise(variable = "aging index",
                mean = mean(aging, na.rm = TRUE),
                sd = sd(aging, na.rm = TRUE),
                min = min(aging, na.rm = TRUE),
                max = max(aging, na.rm = TRUE),
                q25 = quantile(aging, 0.25, na.rm = TRUE),
                q75 = quantile(aging, 0.75, na.rm = TRUE))
    
    npolos_summary = filtered_data |>
      summarise(variable = "n_polos",
                mean = mean(n_polos, na.rm = TRUE),
                sd = sd(n_polos, na.rm = TRUE),
                min = min(n_polos, na.rm = TRUE),
                max = max(n_polos, na.rm = TRUE),
                q25 = quantile(n_polos, 0.25, na.rm = TRUE),
                q75 = quantile(n_polos, 0.75, na.rm = TRUE))
    
    ## Average Tables 
    pop = pop |> group_by(year) |>
      summarise(pop =sum(pop,na.rm = TRUE))
    
    sum_table = CSH |>
      group_by(year) |>
      summarise(variable = "avg_operational ACSS",
                operational_ACSS =sum(operational_ACSS,na.rm = TRUE))
    
    sum_table = full_join(sum_table,pop,by= "year") 
    sum_table = sum_table |> mutate(avg_operational_ACSS = operational_ACSS/pop) |>
      select(-c(pop, operational_ACSS)) |>
      summarise(mean = mean(avg_operational_ACSS, na.rm = TRUE),
                sd = sd(avg_operational_ACSS, na.rm = TRUE),
                min = min(avg_operational_ACSS, na.rm = TRUE),
                max = max(avg_operational_ACSS, na.rm = TRUE),
                q25 = quantile(avg_operational_ACSS, 0.25, na.rm = TRUE),
                q75 = quantile(avg_operational_ACSS, 0.75, na.rm = TRUE))
    
    
    
    # Combine the summary tables into a single table
    summary_table = bind_rows(operational_summary, operational_ACSS_summary, 
                              staff_adj_summary, staff_summary, staff_pop, pharma_summary,
                              medicine_summary, materials_summary, fse_summary, 
                              app_summary, surge_summary, in_days_summary,
                              urge_summary,RO_summary ,beds_summary, wait_days_summary,
                              npolos_summary, aging_summary, den_summary, fem_summary,
                              beds_summary, sum_table, sum_table2)
    
    # Add the summary table to the list
    summary_list[[as.integer(azo_level)+1]] = summary_table
    
    # Save each summary table as a PDF file
    pdf(paste0("summary_table_", as.integer(azo_level), ".pdf"), width = 12, height = 6)
    grid.table(summary_table, rows = NULL)
    dev.off()
    
  }
  
  pop = pop |> group_by(year) |>
    summarise(pop =sum(pop,na.rm = TRUE))

  sum_table = CSH |>
    group_by(year) |>
    summarise(variable = "avg_operational ACSS",
              operational_ACSS =sum(operational_ACSS,na.rm = TRUE))
  
  sum_table = full_join(sum_table,pop,by= "year") 
  sum_table = sum_table |> mutate(avg_operational_ACSS = operational_ACSS/pop) |>
    select(-c(pop, operational_ACSS)) |>
    summarise(mean = mean(avg_operational_ACSS, na.rm = TRUE),
              sd = sd(avg_operational_ACSS, na.rm = TRUE),
              min = min(avg_operational_ACSS, na.rm = TRUE),
              max = max(avg_operational_ACSS, na.rm = TRUE),
              q25 = quantile(avg_operational_ACSS, 0.25, na.rm = TRUE),
              q75 = quantile(avg_operational_ACSS, 0.75, na.rm = TRUE))
  
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
    
    # and as csv file
    write.csv(summary_list[[i]], file = paste0("summary_table_", i, ".csv"))
  }
  
# ---------------- END ------------------------
# Go back to project's working directory
setwd("../../../../..")
rm(list = ls())
  