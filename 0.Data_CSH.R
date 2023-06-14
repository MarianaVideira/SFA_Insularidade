# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)

# 0. Data import ----------------------------------------------------------
# A. Data for continent
hospitals_fin = read_csv2("data/CSH/1. ACSS/agregados-economico-financeiros.csv") # for semicolon separated values
hospitals_ur = read_csv2("data/CSH/1. ACSS/atendimentos-em-urgencia-triagem-manchester.csv")
hospitals_in = read_csv2("data/CSH/1. ACSS/atividade-de-internamento-hospitalar.csv") # inpatient days
hospitals_OR = read_csv2("data/CSH/1. ACSS/ocupacao-do-internamento.csv") # Occupancy rate
hospitals_app = read_csv2("data/CSH/1. ACSS/01_sica_evolucao-mensal-das-consultas-medicas-hospitalares.csv") # appointments
hospitals_beds = read_csv2("data/CSH/1. ACSS/lotacao-praticada-por-tipo-de-cama.csv")
hospitals_surg = read_csv2("data/CSH/1. ACSS/intervencoes-cirurgicas.csv") # 
hospitals_wait = read_csv2("data/CSH/1. ACSS/demora-media-antes-da-cirurgia.csv") 

  # Benchmarking Website (Cost variables)
  c_cmvmc = read_csv("data/CSH/2. Benchmarking/custos_op.csv")
  c_staff = read_csv("data/CSH/2. Benchmarking/custos_pess.csv")
  c_staff_adjusted = read_csv("data/CSH/2. Benchmarking/custos_pess_aj.csv")
  c_pharma = read_csv("data/CSH/2. Benchmarking/custos_farm.csv")
  c_medicine = read_csv("data/CSH/2. Benchmarking/custos_med.csv")
  c_materials = read_csv("data/CSH/2. Benchmarking/custos_mat.csv")
  c_fse = read_csv("data/CSH/2. Benchmarking/custos_fse.csv")

# B. Data for Demographic Characteristics
gdhs = read_dta("data/CSH/5. GDH/GDH2014_st13.dta") 
  
gdhs_test = gdhs |> sample_n(3000)

# C. Data for Açores
hospitals_aco = read.csv("data/CSH/3. Açores/Açores.csv") 

# D. Data for Madeira

# 1. Tidy Data ------------------------------------------------------------
  ## 1.1 Operational Costs -------------------------------------------------

  hospitals_fin = hospitals_fin |>
    separate("Período", c("year", "month"), "-") |>
    separate("Localização Geográfica", c("geo_lat", "geo_lon"), ",") |>
    rename("region" = "Região", "hospital" = "Entidade",
                   "operational" = "Gastos Operacionais") |>
    mutate(year = as.numeric(year), month = as.numeric(month), 
           operational = abs(as.numeric(operational))) |>
    select(c(operational, region, year, month, hospital)) |>
    drop_na() |>
    group_by(hospital, year, region) |>
    filter(n() == 12) |>
    summarise(operational = sum(operational)) |>
    ungroup() 

  ## 1.2 Operational Costs (Benchmarking) ------------------------------------
  c_cmvmc = c_cmvmc |>
    rename("hospital" = "Instituição", "year" = "Ano") |> 
    select(-c(Grupo)) |>
    mutate(year = as.numeric(year), cmvmc = abs(cost_op)) |>
    drop_na() |>
    group_by(hospital, year) |>
    filter(n() == 12) |>
    summarise( cmvmc = sum(cmvmc), dp = sum(dp_op)) |>
    mutate(avg_cmvmc = cmvmc/dp) |>
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
  
  ## 1.5 Waiting times for surgeries ---------------------------------------
  hospitals_wait = hospitals_wait |>
    select(-c("Localização Geográfica")) |>
    separate("Período", c("year", "month"), "-") |>
    rename("region" = "Região", "hospital" = "Instituição", 
           "wait_scheduled_surg" = "Nº Dias até cirurgia em episódios de GDH  cirúrgicos programados",
           "num_surgeries_scheduled" = "Nº de Episódios em GDH cirúrgicos de internamento programados - com exclusões",
           "mean_wait" = "Demora Média Antes da Cirurgia") |>
    mutate(year = as.numeric(year), month = as.numeric(month), 
           mean_wait = as.numeric(mean_wait)) |>
    drop_na() |>
    group_by(hospital, year, region)  |>
    filter(n() == 12) |>
    summarise( wait_scheduled_surg = mean(wait_scheduled_surg),
               scheduled_sur = sum(num_surgeries_scheduled),
               mean_wait = mean(mean_wait)) |>
    ungroup()
  
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
             "n1" = "Nº Atendimentos em Urgência SU Triagem Manchester -Vermelha", 
             "n2" = "Nº Atendimentos em Urgência SU Triagem Manchester -Laranja",
             "n3" = "Nº Atendimentos em Urgência SU Triagem Manchester -Amarela",
             "n4" = "Nº Atendimentos em Urgência SU Triagem Manchester -Verde", 
             "n5" = "Nº Atendimentos em Urgência SU Triagem Manchester -Azul",
             "n6" = "Nº Atendimentos em Urgência SU Triagem Manchester -Branca",
             "n7" = "Nº Atendimentos s\\ Triagem Manchester") |>
      mutate(year = as.numeric(year), month = as.numeric(month),
             urge = n1 + n2 + n3 + n4 + n5 + n6 + n7) |>
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

## 2.1 Rename hospitals ----------------------------------------------------
# function
  rename_hospitals <- function(data) {
    data = data |> mutate(hospital = ifelse(hospital == "Centro Hospitalar de Lisboa - Zona Ocidental, EPE",
                                            "Centro Hospitalar de Lisboa Ocidental, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Psiquiátrico de Lisboa, SPA",
                                            "Centro Hospitalar Psiquiátrico de Lisboa", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Universitário do Algarve, E.P.E",
                                            "Centro Hospitalar Universitário do Algarve, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Póvoa Varzim / Vila do Conde, EPE",
                                            "Centro Hospitalar de Póvoa de Varzim/Vila do Conde, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Universitário do Algarve, EPE",
                                            "Centro Hospitalar Universitário do Algarve,EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Universitário do Algarve, E.P.E",
                                            "Centro Hospitalar Universitário do Algarve,EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Vila Nova Gaia/Espinho, EPE", 
                                            "Centro Hospitalar Vila Nova de Gaia/Espinho, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Arcebispo João Crisóstomo - Cantanhede", 
                                            "Hospital Arcebispo João Crisóstomo", hospital),
                          hospital = ifelse(hospital == "Hospital da Senhora da Oliveira Guimarães, EPE", 
                                            "Hospital da Senhora da Oliveira, Guimarães, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Distrital Figueira da Foz, EPE", 
                                            "Hospital Distrital da Figueira da Foz, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Distrital Santarém, EPE", 
                                            "Hospital Distrital de Santarém, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Dr. Francisco Zagalo - Ovar", 
                                            "Hospital Dr. Francisco Zagalo", hospital),
                          hospital = ifelse(hospital == "Hospital Garcia de Orta, EPE - Almada", 
                                            "Hospital Garcia de Orta, EPE", hospital),
                          hospital = ifelse(hospital == "Instituto de Oftalmologia Dr. Gama Pinto", 
                                            "Instituto Gama Pinto", hospital),
                          hospital = ifelse(hospital == "Instituto Português Oncologia de Coimbra, EPE", 
                                            "Instituto Português de Oncologia de Coimbra, EPE", hospital),
                          hospital = ifelse(hospital == "Instituto Português Oncologia de Lisboa, EPE", 
                                            "Instituto Português de Oncologia de Lisboa, EPE", hospital),
                          hospital = ifelse(hospital == "Instituto Português Oncologia do Porto, EPE", 
                                            "Instituto Português de Oncologia do Porto, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                                            "Hospital de Braga", hospital),
                          hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                                            "Hospital de Braga", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Barreiro - Montijo, EPE", 
                                            "Centro Hospitalar Barreiro/Montijo, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Oeste, EPE", 
                                            "Centro Hospitalar do Oeste", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Oeste, SPA", 
                                            "Centro Hospitalar do Oeste", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Universitário de Lisboa Central, EPE", 
                                            "Centro Hospitalar Universitário Lisboa Central, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Universitário Lisboa Norte, EPE", 
                                            "Centro Hospitalar Universitário de Lisboa Norte, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar de Entre o Douro e Vouga, EPE", 
                                            "Centro Hospitalar Entre Douro e Vouga, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Médio Ave, EPE", 
                                            "Centro Hospitalar do Médio Ave, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Tâmega e Sousa, EPE", 
                                            "Centro Hospitalar Tâmega e Sousa, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar de Póvoa de Varzim/Vila do Conde, EPE", 
                                            "Centro Hospitalar Póvoa de Varzim/Vila do Conde, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Medicina de Reabilitação da Região Centro Rovisco Pais", 
                                            "Hospital Rovisco Pais", hospital),
                          hospital = ifelse(hospital == "Hospital Fernando Fonseca, EPE", 
                                            "Hospital Professor Doutor Fernando Fonseca, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                            "Hospital de Vila Franca de Xira", hospital),
                          hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                            "Hospital de Vila Franca de Xira", hospital),
                          hospital = ifelse(hospital == "Hospital Santa Maria Maior, EPE", 
                                            "Hospital Distrital S.Maria Maior, EPE - Barcelos", hospital),
                          hospital = ifelse(hospital == "Hospital de Loures, EPE", 
                                            "Hospital de Loures", hospital),
                          hospital = ifelse(hospital == "Hospital de Loures, PPP", 
                                            "Hospital de Loures", hospital))
    
    return(data)
  }

# rename variables
  hospitals_fin = rename_hospitals(hospitals_fin)
  hospitals_ur = rename_hospitals(hospitals_ur)
  hospitals_in = rename_hospitals(hospitals_in)
  hospitals_OR = rename_hospitals(hospitals_OR)
  hospitals_app = rename_hospitals(hospitals_app)
  hospitals_beds = rename_hospitals(hospitals_beds)
  hospitals_surg = rename_hospitals(hospitals_surg)
  hospitals_wait = rename_hospitals(hospitals_wait)
  c_cmvmc = rename_hospitals(c_cmvmc)
  c_staff = rename_hospitals(c_staff)
  c_staff_adjusted = rename_hospitals(c_staff_adjusted)
  c_pharma = rename_hospitals(c_pharma)
  c_medicine = rename_hospitals(c_medicine)
  c_fse = rename_hospitals(c_fse)
  c_materials = rename_hospitals(c_materials)
  
## 2.2 Joining DB -------------------------------------------------------------

  # Join all data frames from ACSS in list
  list_df = list(hospitals_fin, hospitals_ur, hospitals_in, hospitals_OR, hospitals_app,
                 hospitals_beds, hospitals_surg, hospitals_wait)
  
  CSH = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year",
                                                 "region" = "region"), 
                                    .init = NULL)) |> 
    filter(year > 2014) 
  
  # join all databases from benchmarking
  list_df = list(CSH, c_cmvmc, c_staff, c_staff_adjusted,
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
write.csv(CSH, file = "filename.csv")
  

###### END #####
rm(list=ls())
  