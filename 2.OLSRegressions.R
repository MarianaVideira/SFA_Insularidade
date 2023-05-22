# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(plm)
library(ggplot2)
library(stargazer)
library(ggpubr)
library(gridExtra)
library(texreg)
library(tinytex)

# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("OLSregression")
setwd("OLSregression")

# 1. Regressions OLS --------------------------------------------------------

  # 1.1 Operational costs ~ surg --------------------------------------------
  alle = all |> dplyr::select(c_op_acp, surg_total, hospital, year, region) |> #select columns of interest
    drop_na() 
  
  reg = lm(log(c_op_acp) ~ I(log(surg_total)), data = alle)
  summary(reg)
  
  # 1.2 Operational costs ~ app --------------------------------------------
  alle = all |> dplyr::select(c_op_acp, app, hospital, year, region) |> #select columns of interest
    drop_na() 
  
  reg2 = lm(log(c_op_acp) ~ I(log(app)), data = alle)
  summary(reg)
  
  # 1.3 Staff costs ~ app ---------------------------------------------
  alle = all |> dplyr::select(c_pess_acp, surg_total, hospital, year, region) |> #select columns of interest
    drop_na() 
  
  reg3 = lm(log(c_pess_acp) ~ I(log(surg_total)), data = alle)
  summary(reg)
  
  
  # 1.4 Staff costs ~ app ---------------------------------------------
  alle = all |> dplyr::select(c_pess_acp, app, hospital, year, region) |> #select columns of interest
    drop_na() 

  reg4 = lm(log(c_pess_acp) ~ I(log(app)), data = alle)
  summary(reg)
  

  # 1.5 Regression Table ----------------------------------------------------

  latex_table = stargazer(reg, reg2, reg3, reg4,
                          type = "latex", # output in LaTeX format
                          title = "Regression Results", # table title
                          header = FALSE, # don't include column headers
                          dep.var.caption = "Dependent Variable", # caption for dependent variable column
                          dep.var.labels.include = T, # don't include variable labels in dependent variable column
                          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"), # labels for the regression models
                          model.names = FALSE, # don't include regression model names
                          digits = 3, # number of digits to display in the table
                          out = "OLSregression_table.tex") # save the table as a LaTeX file

  # 2. Plots ----------------------------------------------------------------
generate_plot <- function(data,volume,cost, title, filter_year = NULL, xlabel = NULL, ylabel = NULL, reg = F) {
    if(!is.null(filter_year)){
      data = data |> filter(year == filter_year)
    }
    
    data |> 
      select(year, region, {{volume}}, {{cost}}) |> 
      drop_na() 
    
    p = ggplot(data, aes(x = log({{volume}}), y = log({{cost}}) )) +
      geom_point(aes(color = region),alpha = 0.6) +
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
    
    if(reg == T){
      p = p + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) # add regression line
    }
    return(p)
}
  
  plot1 = generate_plot(all, surg_total, c_op_acp, "Operational Costs per patient", ylabel = "Log Operational cost per patient", xlabel = "Log Surgeries", reg = T)
  plot2 = generate_plot(all, surg_total, c_pess_acp, "Operational Costs per patient", ylabel = "Log Staff cost per patient", xlabel = "Log Surgeries", reg = T)
  plot3 = generate_plot(all, app, c_op_acp, "Staff Costs per patient", ylabel = "Log Operational cost per patient", xlabel = "Log Appointments", reg = T)
  plot4 = generate_plot(all, app, c_pess_acp, "Staff Costs per patient", ylabel = "Log Staff cost per patient", xlabel = "Log Appointments", reg = T)
  
  grid = ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
  ggsave("app_pess.png", grid, limitsize = F, width = 22, height = 20,units = "cm")
  ggsave("app_pess.pdf", grid, limitsize = F, width = 22, height = 20,units = "cm")
  