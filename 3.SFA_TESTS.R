# 0. Imports --------------------------------------------------------------
library(frontier)
library(dplyr)
library(tidyr)
library(plm)
library(xtable)
library(tools)
library(ggplot2)
library(stargazer)

# Create a folder for Descriptive statistics plots and set as new working directory
dir.create("SFAestimates")
setwd("SFAestimates")

# 1. Data -----------------------------------------------------------------
  # A. Balanced Data
  alle = all |> dplyr::select(c_op, surg_total, hospital, year, app, region, beds) |> #select columns of interest
    drop_na() 
  
  # B. Panel Data 
  # Construct panel dataframe for time-variant
  allPanel = pdata.frame(alle, c("hospital", "year"))

# 2. Model Estimation (Cross section) -------------------------------------

  # A. Define functional forms for different models (Following Azevedo 2014)
  form = log(c_op_acp) ~ I(log(surg_total+ app)) + I(log(beds))
  formquad = c_op_acp ~ I(surg_total) + I(app) + I(surg_total^2) + I(app^2) 
  formquadi = c_op_acp ~ I(surg_total) + I(app) + I(surg_total^2) + I(app^2) + I(app*surg_total)
  
  # TEST ------
  form = log(c_pess_acp) ~ I(log(surg_total)) + I(log(app))
  form = log(c_pess_acp) ~ I(log(surg_total)) #+ I(log(app))
  form = log(c_op) ~ log(surg_total) #+ app
  formquad = c_pess_acp ~ I(surg_total) + I(app) + I(surg_total^2) + I(app^2) 
  formquadi = c_pess_acp ~ I(surg_total) + I(app) + I(surg_total^2) + I(app^2) + I(app*surg_total)
  formqtranslog = log(c_pess_acp) ~ log(surg_total) + log(app) + I(0.5*log(surg_total)^2) + I(0.5*log(app)^2) #+ I(log(app)*log(surg_total))
  formtr = log(c_pess_acp) ~ log(surg_total) + log(app) + factor(region)
  formth = log(c_pess_acp) ~ log(surg_total) + log(app) + factor(hospital)
  

  # The I() function acts to convert the argument to "as.is", 
  # i.e. what you expect. So I(x^2) would return a vector of values raised to the
  # second power.
  
  # The ^ and : operators are used to construct interactions so  x = x^2 = x^3 rather than becoming perhaps expected mathematical powers. (A variable interacting with itself is just the same variable.) If you had typed (x+y)^2 the R interpreter would have produced (for its own good internal use), not a mathematical: x^2 +2xy +y^2 , but rather a symbolic: x + y +x:y where x:y is an interaction term without its main effects. (The ^ gives you both main effects and interactions.)
# DOUBLE CHECK MODELS IN LITERATURE ------  
  # Multicolinearity without interaction: 
  # Formulas with factoring region: factors for hospital and region
  formtr = log(c_op_acp) ~ log(surg_total) + log(app) + factor(region)
  formtrqi = c_op_acp ~ I(surg_total) + I(app) + I(surg_total^2) + I(app^2) + I(app*surg_total) + factor(region)
  formth = log(c_op_acp) ~ log(surg_total) + log(app) + factor(hospital)
  
  # Formula approach by BC95
  #formz = log(c_op) ~ log(surg_total) + log(app) | # double check
  # add factors
  
  # 2.1 Logarithmic specification  ---------------------------------------------
  # Cross section estimation of the average cost per patient frontier, following
  # ALS77 specification: yi= a + sum(\beta_j x_{j,i}) + v_i -u_i (half-normal dist)
    
  # A. Cost frontier Estimation
  sfront = sfa( form,  
                data = alle,
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
  
  # C. Individual Inefficiency by JLMS
  # epsilon ( \epsilon_i = v_i - u_i) from fitted values
  fitcost = fitted(sfront, asInData = T)
  ei = log(alle$c_op_acp) - fitcost
  
  # E(u_i | e_i)
  us2 = (cost_coef[["sigmaU"]])^2
  vs2 = (cost_coef[["sigmaV"]])^2
  sigmastar = sqrt((us2*vs2)/(vs2+us2))
  ustari = (-us2*ei)/(vs2+us2)
  ucost_f = ((sigmastar*dnorm(ustari/sigmastar))/(pnorm(ustari/sigmastar)))+ ustari
  in_costf = 1 - exp(-ucost_f)
             
  summary(in_costf)
  
  # D. Individual inefficiencies by function 
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  in_costf2 = 1 - efficiencies(sfront)
  summary(in_costf2)
  innef_table = summary(in_costf2)
  
  # E. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                     caption = "Average cost per patient frontier")
  
  file_name <- "1costf.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # Inefficiencies table
  innef_table = xtable(innef_table, 
                       caption = "Summary Inneficiency")
  
  file_name <- "1eff.tex"
  file_path <- file.path(getwd(), file_name)
  print(innef_table, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  rm(sfront, cost_coef, cost_f, cost_coef_table, 
     form, formlquadi, formquad, formquadi, formth, formtr,
     ei, fitcost, in_costf, sigmastar, ucost_f, us2, ustari, vs2, innef_table, in_costf2,
     file_name, file_path, table_latex, table_latex2)

  # 2.2 Quadratic specifications --------------------------------------------
  
  # A. Cost frontier Estimation: no interaction term
  sfront = sfa( formquad,  
                    data = alle,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
  
  # B. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(sfront)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(sfront), which = "mle", extraPar = T)
  cost_coef = coef(sfront, which = "mle", extraPar = T) # only returns coefficients
  
  # C. Individual inefficiencies by function 
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  in_costf2 = 1 - efficiencies(sfront)
  summary(in_costf2)
  innef_table = summary(in_costf2)
  
  # D. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                       caption = "Average cost per patient frontier (levels)")

  file_name <- "2cost_quad.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # Inefficiencies table
  innef_table = xtable(innef_table, 
                       caption = "Summary Inneficiency (levels)")
  
  file_name <- "2eff.tex"
  file_path <- file.path(getwd(), file_name)
  print(innef_table, file = file_path, include.rownames = TRUE, caption.placement = "top")
# STOP HERE ------
  # E. Cost frontier Estimation: interaction term
  sfront = sfa(formquadi,  
               data = alle,
               ineffDecrease = F, # FALSE for cost function and TRUE for production
               truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
               timeEffect = FALSE, # time is allowed to have an effect on efficiency
               printIter = 1 )
  
  # F. Coefficients (quadratic_int no interaction term)
  # summary estimation
  summary(sfront)
  # extract table of coefficients
  cost_table_coef = coef(summary(quadratic_interaction), which = "mle", extraPar = T)
  
  # G. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                       caption = "Average cost per patient frontier")
  
  file_name <- "3cost_quad_i.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  # INCLUDE INNEF --------
  # 2.3 Transcendental logarithmic function -------------------------------
  # A. Cost frontier Estimation
  Translog_mod = sfa(formqtranslog, 
                     data = alle,
                     ineffDecrease = FALSE, 
                     truncNorm = FALSE,
                     timeEffect = FALSE)
  
  # B. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(Translog_mod)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(Translog_mod), which = "mle", extraPar = T)
  
  # C. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                       caption = "Average cost per patient frontier")
  
  table_latex2 = stargazer(cost_coef_table, 
                           title = "Average cost per patient frontier",
                           align = TRUE,
                           dep.var.labels = c("Average costs per patient"),
                           covariate.labels = c("", "Estimates"))
  
  file_name <- "cost_transl.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex2, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # 2.4 FE: factorization with regions and hospital ---------------------
  # True fixed effects model, adapted from Greene (2005) G05 to cross section
  
  # A.  Cost frontier Estimation: region
  costreg = sfa(formtr, 
                 data = alle,
                 ineffDecrease = FALSE, 
                 truncNorm = FALSE,
                 timeEffect = FALSE)
  
  # B. Coefficients factors
  # summary estimation
  summary(costreg)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(costreg), which = "mle", extraPar = T)
  
  # C. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                       caption = "Average cost per patient frontier")
  
  table_latex2 = stargazer(cost_coef_table, 
                           title = "Average cost per patient frontier",
                           align = TRUE,
                           dep.var.labels = c("Average costs per patient"),
                           covariate.labels = c("", "Estimates"))
  
  file_name <- "cost_cd.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex2, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # D. Individual inefficiencies by function 
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  in_costf2 = 1 - efficiencies(costreg)
  summary(in_costf2)
  
  
  # E.  Cost frontier Estimation: hospital
  costhos = sfa(formth, 
                data = alle,
                ineffDecrease = FALSE, 
                truncNorm = FALSE,
                timeEffect = FALSE)
  
  
  # F. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(costhos)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(costhos), which = "mle", extraPar = T)
  
  # G. Exporting Results 
  table_latex = xtable(cost_coef_table, 
                       caption = "Average cost per patient frontier")
  
  table_latex2 = stargazer(cost_coef_table, 
                           title = "Average cost per patient frontier",
                           align = TRUE,
                           dep.var.labels = c("Average costs per patient"),
                           covariate.labels = c("", "Estimates"))
  
  file_name <- "cost_cd.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex2, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # H. Individual inefficiencies by function 
  # EFF_i = E(Y_i* | U_i, X_i)/ E(Y_i* | U_i = 0, X_i)
  #(generalization from Jondrow 1982 (JLMS) and Bat. Coel. 1992 (BC92))
  in_costf2 = 1 - efficiencies(costhos)
  summary(in_costf2)

# ---------- STOP HERE-------
  
# 3. Model Estimation (Panel data) -------------------------------------
  # Models estimated trough MLE with half normal 
  
  # 3.1 Normal specification  --------------------------------------------
  # A. Cobb-douglas Cost frontier Estimation
  cost_cdoug_P = sfa(form,  
                    data = allPanel,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = TRUE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
  
  # B. Coefficients
  # summary estimation
  summary(cost_cdoug_P)
  # extract table of coefficients
  cost_table_coef_P = coef(summary(cost_cdoug_P), which = "mle", extraPar = T)
  
  
  
  # C. Exporting Results 
  table_latex = xtable(cost_table_coef_P, 
                       caption = "Cost frontier with observation-specific efficiencies")
  
  file_name <- "cost_cd.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  
  # 3.2 Quadratic specification ---------------------------------------------
  # A. Without interaction term
  quadratic_P = sfa( formquad,  
                   data = allPanel,
                   ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                   truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                   timeEffect = TRUE, # time is allowed to have an effect on efficiency
                   printIter = 1 )
  
  # B. With interaction term
  quadratic_interaction_P = sfa(formquadint,  
                              data = allPanel,
                              ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                              truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                              timeEffect = FALSE, # time is allowed to have an effect on efficiency
                              printIter = 1 )
  
  # C. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(quadratic_P)
  # extract table of coefficients
  cost_table_coef = coef(summary(quadratic_P), which = "mle", extraPar = T)
  
  # D. Coefficients (quadratic_int no interaction term)
  # summary estimation
  summary(quadratic_interaction_P)
  # extract table of coefficients
  cost_table_coef = coef(summary(quadratic_interaction_P), which = "mle", extraPar = T)
  
  # 3.3 Transcendental logarithmic function -------------------------------
  # A. TL Estimation
  Translog_mod_P = sfa(formqtranslog, 
                     data = allPanel,
                     ineffDecrease = FALSE, 
                     truncNorm = FALSE,
                     timeEffect = TRUE)
  
  # B. Coefficients (quadratic no interaction term)
  # summary estimation
  summary(Translog_mod_P)
  
  # extract table of coefficients
  cost_table_coef = coef(summary(Translog_mod_P), which = "mle", extraPar = T)
  
  # REVIEW 2.2 Specification with factoring --------------------------------------
  
  # A. Cobb-douglas Cost frontier Estimation
  cost_cdoug = sfa( formt,  
                    data = alle,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
  
  
  # B. Coefficients
  cost_table_coef = coef(summary(cost_cdoug), which = "mle", extraPar = T)
  
  # summary estimation
  summary(cost_cdoug)
  
  # C. Exporting Results 
  table_latex = xtable(cost_table_coef, 
                       caption = "Cost frontier with observation-specific efficiencies")
  
  file_name <- "cost_cd_t.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  # 2.3 Specification with factoring and quadratic ------------------------
  
  # A. Cobb-douglas Cost frontier Estimation
  cost_cdoug = sfa( formq,  
                    data = alle,
                    ineffDecrease = FALSE, # FALSE for cost function and TRUE for production
                    truncNorm = FALSE, # FALSE -> errors have half normal distribution TRUE -> truncated distribution (mu parameter is added)
                    timeEffect = FALSE, # time is allowed to have an effect on efficiency
                    printIter = 1 )
  
  
  # B. Coefficients
  cost_table_coef = coef(summary(cost_cdoug), which = "mle", extraPar = T)
  
  # summary estimation
  summary(cost_cdoug)
  
  # C. Exporting Results 
  table_latex = xtable(cost_table_coef, 
                       caption = "Cost frontier with observation-specific efficiencies")
  
  file_name <- "cost_cd_q.tex"
  file_path <- file.path(getwd(), file_name)
  print(table_latex, file = file_path, include.rownames = TRUE, caption.placement = "top")
  
  #
# Clear data --------------------------------------------------------------
rm(alle, allPanel, 
   form, formq, formt, formz)
setwd("..")

############ END ###############

# Notes ------------------------------------------------------------------
# dplyr:: to suggest that the select function is from dplyr package. 
# it has a conflict with MASS function. "MASS" is required by ‘gamlss.dist’


# a) to remove variables with almost no values
# checking the number of variables in each factor 
# count the number of observations per level of the factor variable "hospital"
#obs_per_level <- table(all$hospital)

# identify the levels with fewer than 5 observations
#levels_to_drop <- names(obs_per_level[obs_per_level < 5])

# remove the levels with fewer than 5 observations using droplevels()
#allsub <- all[!(all$hospital %in% levels_to_drop), ]
#table(allsub$hospital)

# b) stargazer for tables
# library(stargazer)
# METHOD 1: Stargazer
#Export to latex
#latex_tab <- stargazer(cost_cdcoef, tittle ="Summary Table",summary = F,align = TRUE)
#cat(latex_tab, file = "summary_table.tex")

# *Test phase* 2.1.2 Individual inefficiency by JLMS ----------------------------------
# Double check
#cost_coef = coef(cost_cdoug, which = "mle", extraPar = T)

# Fitted values epsilon
#fcost_cd <- fitted(cost_cdoug, asInData = T) # rever função OLS VS SFA
#ei = log(alle$c_op) - fcost_cd

# E(ui | ei)
#us2 = (cost_coef[["SigmaU"]])^2
#vs2 = (cost_coef[["SigmaV"]])^2
#sigmastar = sqrt((vs2*us2)/(vs2+us2))
#ustari = (-us2*ei)/(vs2+us2)
#ucost = ((sigmastar*dnorm(ustari/sigmastar))/(pnorm(ustari/sigmastar))) +ustari
#incost = 1 - exp(-ucost)

#summary(incost)

# *Test phase* Graphical Representation --------------------------------------------
# Create data frame with real costs and fitted values
df <- data.frame(c_op = alle$c_op, fitted = fcost_cd, region = alle$region)

# Create scatter plot with regression line / review this graph
p <- ggplot(df, aes(x = fitted, y = log(c_op))) +
  geom_point(aes(color = region)) +
  geom_smooth(method = "lm") +
  xlab("Real Costs") +
  ylab("Fitted Values")# +
#scale_color_manual(values = c("red", "blue", "green")) # Specify color scheme

ggsave("my_plot.pdf", plot = p, width = 8, height = 6)

# Export to pdf
# Open a PDF file for writing
#pdf("my_table.pdf")

# Print the table to the PDF file
#print(table_latex,
#     include.rownames = FALSE, 
#      include.colnames = TRUE, 
#     floating = FALSE, 
#    hline.after = c(-1, 0, nrow(cost_table_coef)),
#   tabular.environment = "longtable")

# Close the PDF file
#dev.off()

# Note ----
# If argument formula of sfa is a (usual) one-part formula (or argument zNames of 
# frontier is NULL), an ‘Error Components Frontier’ (ECF, see Battese and Coelli 1992)
# is estimated. If argument formula is a two-part formula (or zNames is not NULL), an 
#‘Efficiency Effects Frontier’ (EEF, see Battese and Coelli 1995) is estimated
# https://rdrr.io/cran/frontier/man/frontier.html

# graph: GAM-SFA
fcost_cd = fitted(cost_cdoug, asInData = T)
plot3 = generate_plot(all2, surg_total, c_op, "Total costs")


# Create data frame with real costs and fitted values
df <- data.frame(app = alle$app, c_op = alle$c_op, surg_total = alle$surg_total, fitted = fcost_cd, region = alle$region)

# Create scatter plot with regression line / review this graph
p = ggplot(df) + # , aes(x = fitted, y = log(c_op))
  geom_point(aes(x = log(surg_total), y = log(c_op), color = region)) +
  geom_line(aes(x = log(surg_total), y = -0.277621 * log(surg_total) + 1.343485*log(app))) +
  #geom_smooth(method = "lm") +
  xlab("Total surgeries") +
  ylab("Operational costs")

ggsave("my_plot.pdf", plot = p, width = 8, height = 6)


# Other Functional forms --------------------------------------------------

# Almost perfect multicolinearity
formquad = c_op_acp ~ log(surg_total) + log(app) + I(log(surg_total^2)) + I(log(app^2)) + I(log(app*surg_total))
formquadint = c_op_acp ~ surg_total + app + I(surg_total^2) + I(app^2) + I(app*surg_total)
formqtranslog = log(c_op_acp) ~ log(surg_total) + log(app) + I(0.5*log(surg_total)^2) + I(0.5*log(app)^2) + I(log(app)*log(surg_total))
formlquadi = log(c_op_acp) ~ surg_total + app + I(surg_total^2) + I(app^2) + I(app*surg_total)
# STargazer -------------
# Not working properly
table_latex2 = stargazer(cost_coef_table, 
                         title = "Average cost per patient frontier",
                         align = TRUE,
                         dep.var.labels = c("Average costs per patient"),
                         covariate.labels = c("", "Estimates"))