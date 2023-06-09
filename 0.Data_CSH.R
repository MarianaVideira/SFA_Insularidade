# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)


# 0. Data import ----------------------------------------------------------
# A. Data for continent
hospitals_fin = read_csv2("data/CSH/ACSS/agregados-economico-financeiros.csv") # for semicolon separated values
hospitals_ur = read_csv2("data/CSH/ACSS/atendimentos-por-tipo-de-urgencia-hospitalar-link.csv") # urgent appointments
hospitals_in = read_csv2("data/CSH/ACSS/atividade-de-internamento-hospitalar.csv") # inpatient days
hospitals_OR = read_csv2("data/CSH/ACSS/ocupacao-do-internamento.csv") # Occupancy rate
hospitals_app = read_csv2("data/CSH/ACSS/01_sica_evolucao-mensal-das-consultas-medicas-hospitalares.csv") # appointments
hospitals_beds = read_csv2("data/CSH/ACSS/lotacao-praticada-por-tipo-de-cama.csv")
hospitals_surg = read_csv2("data/CSH/ACSS/intervencoes-cirurgicas.csv") # 

  # Benchmarking Website (Cost variables)
  c_operational = read_csv("data/CSH/Costs/custos_op.csv")
  c_staff = read_csv("data/CSH/Costs/custos_pess.csv")
  c_staff_adjusted = read_csv("data/CSH/Costs/custos_pess_aj.csv")
  c_pharma = read_csv("data/CSH/Costs/custos_farm.csv")
  c_medicine = read_csv("data/CSH/Costs/custos_med.csv")
  c_materials = read_csv("data/CSH/Costs/custos_mat.csv")
  c_fse = read_csv("data/CSH/Costs/custos_fse.csv")

# B. Data for Açores
hospitals_aco = read.csv("data/CSH/Açores/Açores.csv") 

# C. Data for Madeira
# divide RO for 100
# mutate year as numeric because I need it for the aggregation of data
# 1. Tidy Data ------------------------------------------------------------
  ## 1.1 Operational Costs -------------------------------------------------

  hospitals_fin = hospitals_fin |>
    separate("Período", c("year", "month"), "-") |>
    separate("Localização Geográfica", c("geo_lat", "geo_lon"), ",") |>
    rename("region" = "Região", "hospital" = "Entidade",
                   "operational_ACSS" = "Gastos Operacionais") |>
    mutate(year = as.numeric(year), month = as.numeric(month), 
           operational_ACSS = abs(as.numeric(operational_ACSS))) |>
    select(c(operational_ACSS, region, year, month, hospital)) |>
    drop_na() |>
    group_by(hospital, year, region) |>
    filter(n() == 12) |>
    summarise(operational_ACSS = sum(operational_ACSS)) |>
    ungroup() 
  
  ## 1.2 Operational Costs (Benchmarking) ------------------------------------
  c_operational = c_operational |>
    rename("hospital" = "Instituição", "year" = "Ano") |> 
    select(-c(Grupo)) |>
    mutate(year = as.numeric(year), operational = abs(cost_op)) |>
    drop_na() |>
    group_by(hospital, year) |>
    filter(n() == 12) |>
    summarise( operational = sum(operational), dp = sum(dp_op)) |>
    mutate(avg_operational = operational/dp) |>
    ungroup() #|> select(-dp) # I should keeo dp in the database. 

  ## 1.3 Staff Costs --------------------------------------------------------

  c_staff_adjusted = c_staff_adjusted |>
    rename("hospital" = "Instituição", "year" = "Ano") |>
    mutate(year = as.numeric(year), staff_adjusted = abs(c_pess_a)) |>
    select(-c(Grupo)) |>
    drop_na() |>
    group_by(hospital, year) |>
    filter(n() == 12) |>
    summarise(staff_adjusted = sum(staff_adjusted), dp = sum(dp_pess_a)) |>
    mutate(avg_staff_adjusted = staff_adjusted/dp) |>
    ungroup() |> select(-dp)

  c_staff = c_staff |>
    rename("hospital" = "Instituição", "year" = "Ano") |>
    mutate(year = as.numeric(year), staff = abs(c_pess)) |>
    select(-c(Grupo)) |>
    drop_na() |>
    group_by(hospital, year) |>
    filter(n() == 12) |>
    summarise(staff = sum(staff), dp = sum(dp_pess)) |>
    mutate(avg_staff = staff/dp) |>
    ungroup() |> select(-dp)
  
  ## 1.4 Other Costs --------------------------------------------------------
    ### A. Materials --------------------------------------------------------
    c_materials = c_materials |>
      rename("hospital" = "Instituição", "year" = "Ano") |>
      mutate(year = as.numeric(year), materials = abs(costs_mat)) |>
      select(-c(Grupo)) |>
      drop_na() |>
      group_by(hospital, year) |>
      filter(n() == 12) |>
      summarise(materials = sum(materials), dp = sum(dp_mat)) |>
      mutate(avg_materials = materials/dp) |>
      ungroup() |> select(-dp)

    ### B. Pharmaceuticals --------------------------------------------------
  
    c_pharma = c_pharma |>
      rename("hospital" = "Instituição", "year" = "Ano") |>
      mutate(year = as.numeric(year), pharma = abs(costs_farm)) |>
      select(-c(Grupo)) |>
      drop_na() |>
      group_by(hospital, year) |>
      filter(n() == 12) |>
      summarise(pharma = sum(pharma), dp = sum(dp_farm)) |>
      mutate(avg_pharma = pharma/dp) |>
      ungroup() |> select(-dp)


    ### C. Medicine --------------------------------------------------------
   
    c_medicine = c_medicine |>
      rename("hospital" = "Instituição", "year" = "Ano") |>
      mutate(year = as.numeric(year), medicine = abs(costs_med)) |>
      select(-c(Grupo)) |>
      drop_na() |>
      group_by(hospital, year) |>
      filter(n() == 12) |>
      summarise( medicine = sum(costs_med), dp = sum(dp_med)) |>
      mutate(avg_medicine = medicine/dp) |>
      ungroup() |> select(-dp)
  
    ### D. FSE ------------------------------------------------------------------
    c_fse <- c_fse |>
      rename("hospital" = "Instituição", "year" = "Ano") |>
      mutate(year = as.numeric(year), fse = abs(costs_fse)) |>
      select(-c(Grupo)) |>
      drop_na() |>
      group_by(hospital, year) |>
      filter(n() == 12) |>
      summarise(fse = sum(fse), dp = sum(dp_fse)) |>
      mutate(avg_fse = fse/dp) |>
      ungroup() |> select(-dp)
  
  ## 1.5 Inputs (Staff prices) ---------------------------------------------
  # missing data
  ## 1.6 Outputs -----------------------------------------------------------
    ### A. Patient discharges ----------------------------------------------
    hospitals_in = hospitals_in |>
      separate("Período", c("year", "month"), "-") |>
      rename("region" = "Região", "hospital" = "Instituição", 
             "types_beds" = "Tipo de Especialidade",
             "patient_dis" = "Doentes Saídos") |>
      mutate(year = as.numeric(year), month = as.numeric(month), 
             patient_dis = as.numeric(patient_dis)) |>
      select(c(types_beds, patient_dis, region, year, month, hospital)) |>
      drop_na() |>
      group_by(hospital, year, region, types_beds)  |>
      filter(n() == 12) |>
      summarise(patient_dis = sum(patient_dis)) |>
      pivot_wider(names_from = types_beds, values_from = patient_dis) |>
      rename( "d1" = "Especialidade Cirurgica", 
              "d2" = "Especialidade Médica",
              "d3" = "Outras Camas") |>
      mutate( patient_dis = d1 + d2 + d3) |>
      select(-c(d1, d2, d3)) |>
      ungroup()

    ### B. Inpatient days and Rate of Occupancy -----------------------------
    hospitals_OR = hospitals_OR |>
        separate("Período", c("year", "month"), "-") |>
        rename("region" = "Região", "hospital" = "Instituição",
               "in_days" = "Nº Dias de Internamento", 
               "R_ocuppancy" = "Taxa Anual de Ocupação em Internamento") |>
        mutate(year = as.numeric(year), month = as.numeric(month), 
               in_days = as.numeric(in_days),
               R_ocuppancy = R_ocuppancy/100) |>
        drop_na() |>
        group_by(hospital, year, region) |>
        filter(n() == 12) |>
        summarise( R_ocuppancy = mean(R_ocuppancy), in_days = sum(in_days)) |>
        ungroup()
      
    ### C. Urgent care ------------------------------------------------------
    hospitals_ur = hospitals_ur |>
      select(-c("Localização Geográfica")) |>
      separate("Período", c("year", "month"), "-") |>
      rename("region" = "Região", "hospital" = "Instituição",
             "urge" = "Total Urgências") |>
      mutate(year = as.numeric(year), month = as.numeric(month),
             urge = as.numeric(urge)) |>
      drop_na() |>
      group_by(hospital, year, region)  |>
      filter(n() == 12) |>
      summarise(urge = sum(urge)) |>
      ungroup()
    
    ### D. Appointments (Outpatients)---------------------------------------
    hospitals_app = hospitals_app |>
      rename("region" = "Região", "hospital" = "Instituição",
             "app" = "Nº Consultas Médicas Total") |>  
      separate("Período", c("year", "month"), "-")|>
      mutate(year = as.numeric(year), month = as.numeric(month),
             app = as.numeric(app)) |>
      drop_na() |>
      group_by(hospital, year, region) |>
      filter(n() == 12) |>
      summarise( app = sum(app)) |>
      ungroup()
    

    ### E. Beds ------------------------------------------------------------
    hospitals_beds = hospitals_beds |>
      select(-c("Localização Geográfica")) |>
      separate("Período", c("year", "month"), "-") |>
      mutate(year = as.numeric(year), month = as.numeric(month))|>
      rename("region" = "Região", "hospital" = "Instituição", "num_beds" = "Lotação",
             "types_beds" = "Tipo de Camas",) |>
      drop_na() |>
      group_by(hospital, year, region, types_beds)  |>
      filter(n() == 12) |>
      summarise(  beds = mean(num_beds)) |>
      pivot_wider(names_from = types_beds, values_from = beds) |>
      select(-c("Camas Neutras")) |>
      drop_na() |>
      rename( "cir" = "Camas Cirúrgicas",
              "med" = "Camas Médicas",
              "out" = "Outras Camas") |>
      mutate( beds = cir + med + out) |>
      select(-c(cir, med, out)) |>
      ungroup()

    ### F. Surgeries --------------------------------------------------------
    hospitals_surg = hospitals_surg |>
      rename("region" = "Região", "hospital" = "Instituição",
             "surg_p" = "Nº Intervenções Cirúrgicas Programadas",
             "surg_c" = "Nº Intervenções Cirúrgicas Convencionais",
             "surg_a" = "Nº Intervenções Cirúrgicas de Ambulatório",
             "surg_u" = "Nº Intervenções Cirúrgicas Urgentes") |>   
      separate("Período", c("year", "month"), "-") |>
      mutate(year = as.numeric(year), month = as.numeric(month)) |>
      drop_na() |>
      group_by(hospital, year, region) |>
      filter(n() == 12) |>
      summarise( surg_p = sum(surg_p), 
                 surg_c = sum(surg_c), 
                 surg_a = sum(surg_a),
                 surg_u = sum(surg_u)) |>
      mutate( surg = surg_p + surg_u) |> # total surgeries
      ungroup()
    
  ## 1.8 Check missing months -------------------------------------------------
    check_missing_months <- function(data) {
      missing_months <- data |>
        drop_na() |>  # Check for each of the variables if needed
        group_by(hospital, year) |>
        summarise(m_months = 12 - n()) |>
        filter(m_months > 0)
      
      return(missing_months)
    }
  
  check_missing_months(hospitals_ur)

# 2. Joining data frames --------------------------------------------------

  # Join all data frames in list
  list_df = list(hospitals_fin, hospitals_ur, hospitals_in, hospitals_OR, hospitals_app,
                 hospitals_beds, hospitals_surg)
  CSH = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year",
                                                 "region" = "region"), 
                                    .init = NULL)) |> 
    filter(year > 2014) 

  list_df = list(CSH, c_operational, c_staff, c_staff_adjusted,
                 c_pharma, c_medicine, c_fse, c_materials)
  
  CSH = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL)) |> 
    filter(year > 2014)  |> mutate(azo = 0)
  
  # C. Join data Açores and mainland 
  hospitals_aco$azo = 1 
  CSH = bind_rows(CSH, hospitals_aco)
  CSH$azo = factor(CSH$azo)
  
  # D. Create new Rate of Occupancy
  CSH = CSH |> mutate(RO = in_days/ (beds*30.4375*12), region = as.factor(region))

# 3. Exporting Data -------------------------------------------------------
save(CSH, file= "0.DataBase/CSH.RData")


###### END #####
rm(list=ls())
  