# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)

# 0. Data import ----------------------------------------------------------
# A. Data for continent
csh_2020 = read_sav("data/CSH/INE/BD_Hospitais_2020_Difusao.sav")
csh_2021 = read_dta("data/CSH/INE/hospitais_2021_INE.dta")
csh_2019 = read_dta("data/CSH/INE/BD_Hospitais_2019_Difusao.dta")
csh_2018 = read_dta("data/CSH/INE/BD_Hospitais_2018_Difusao.dta")
csh_2017 = read_sav("data/CSH/INE/BD_Hospitais_2017_Difusao.sav")
csh_2016 = read_sav("data/CSH/INE/BD_Hospitais_2016_Difusao.sav")
csh_2015 = read_sav("data/CSH/INE/BD_Hospitais_2015_Difusao.sav")

# 1. Tidy data ------------------------------------------------------------

# Function
rename_columns = function(data, column_map) {
  renamed_data = rename(data, !!!column_map)
  selected_data = renamed_data[names(column_map)]
  return(selected_data)
}


column_map = list("year" = "Ano", "id" = "NORDEM", "region" = "NUTS2", "entity_type" = "ENT_COD",
    "used_capacity" = "B1000","n_surgery_rooms" = "B3100n","n_birth_rooms" = "B3200N",
    "staff_total" = "C10001","staff_doctors" = "C20001","staff_spec_doctors" = "C21001",
    "staff_nurses" = "C30001","staff_spec_nurses" = "C31001",
    "in_days" = "D010004","inpatients" = "D010001","births" = "E10001",
    "app" = "F10001","surg" = "G10001","urge" = "H3000",
    "operational" = "J30000","patient_transp" = "J40700")

# Rename and select from raw databases
csh_2021 = rename_columns(csh_2021, column_map)
csh_2020 = rename_columns(csh_2020, column_map)
csh_2019 = rename_columns(csh_2019, column_map)

# Column map for years 2015 - 2018

column_map = list("year" = "Ano", "id" = "NORDEM", "region" = "NUTS2", "entity_type" = "ENT_COD",
                  "used_capacity" = "B1000","n_surgery_rooms" = "B3100n","n_birth_rooms" = "B3200n",
                  "staff_total" = "C10001","staff_doctors" = "C20001","staff_spec_doctors" = "C21001",
                  "staff_nurses" = "C30001","staff_spec_nurses" = "C31001",
                  "in_days" = "D010004","inpatients" = "D010001","births" = "E10001",
                  "app" = "F10001","surg" = "G10001","urge" = "H10001")
# Urge might be a slightly different variable, should double check annual evolution.
# There are no costs for these years
csh_2018 = rename_columns(csh_2018, column_map) |> mutate(year = as.numeric(year), region = as.factor(region))
csh_2017 = rename_columns(csh_2017, column_map) |> mutate(year = as.numeric(year), region = as.factor(region))
csh_2016 = rename_columns(csh_2016, column_map) |> mutate(region = as.factor(region))
csh_2015 = rename_columns(csh_2015, column_map) |> mutate(region = as.factor(region))


# 2. Joining Tables -------------------------------------------------------

CSH_INE = bind_rows(csh_2020, csh_2019, csh_2021) |> 
  mutate(year = as.numeric(year), region = as.factor(region))

CSH_INE = bind_rows(CSH_INE, csh_2018, csh_2017, csh_2016, csh_2015) 

# 3. Exporting Data -------------------------------------------------------
save(CSH_INE, file= "0.DataBase/CSH_INE.RData")

# ---------------- END ------------------------
rm(list=ls())
