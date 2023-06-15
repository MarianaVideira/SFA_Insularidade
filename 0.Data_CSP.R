# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)


# 0. Data import ----------------------------------------------------------

csp = read_dta("data/CSP/csp_dados_continente_acores.dta") # for semicolon separated values


# 1. Tidy Data ------------------------------------------------------------
## 1.1 Operational Costs -------------------------------------------------

CSP = csp |>
  rename("staff" = "Custo_pessoal", "app" = "Prod_cons_pres", "app_remote" = "Prod_cons_rem",
         "app_dome" = "Prod_cons_dom", "n_polos" = "Atendim_polos") |> # n of clinics (polos) to recieve patients, proxy for geographical dispersion of health services
  mutate(azo = ifelse(nome %in% c("USI Santa Maria", "USI S. Miguel", 
                                  "USI Terceira", "USI Graciosa", 
                                  "USI S. Jorge", "USI Pico", 
                                  "USI Faial", "USI Flores", "USI Corvo"), 1, 0)) |>
  filter((azo == 1) | (azo == 0 & inscritos >= quantile(inscritos, 0.05) & inscritos <= quantile(inscritos, 0.95))) |>
  mutate(prop_nofam_MF = 1 - (inscritos_MF/inscritos), # proportion of citizens without family doctor, proxy for illness incidennce due to less prevention.  
         prop_fem = inscritos_fem/ inscritos, # proportion of female citizens, proxy for a higher demand of healthcare services *why? 
         prop_age_0_4 = inscritos_00_04/inscritos, 
         # proportion of citizens by age, proxy for demand of health services and ilness incidence.
         prop_age_5_14 = inscritos_05_14/ inscritos,
         prop_age_45_64 = inscritos_45_64/ inscritos, 
         prop_age_65_74 = inscritos_65_74/ inscritos,
         prop_age_75_hig = inscritos_75m/ inscritos) |>
  mutate(across(c(azo,id, nome), as.factor),
         across(c(year, staff, app, app_dome, app_remote, n_polos, prop_nofam_MF, prop_fem), as.numeric)) |>
  select(id, year, nome,azo,  staff, app, app_remote, app_dome, n_polos, prop_nofam_MF, 
       prop_fem, prop_age_0_4, prop_age_5_14, prop_age_45_64, prop_age_65_74, 
       prop_age_75_hig) |>
  filter(nome != "UCSP Sete Rios")

save(CSP, file= "0.DataBase/CSP.RData")

# §. Exporting Data -------------------------------------------------------

###### END #####
rm(list=ls())
