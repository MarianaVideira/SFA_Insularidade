# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)
library(gridExtra)
library(readxl)
library(haven)
library(stringr)

# 0. Data import ----------------------------------------------------------
# A. Data for continent
hospitals_fin = read_csv("data/CSH/1. ACSS/operacionais.csv") # for semicolon separated values
hospitals_ur = read_csv2("data/CSH/1. ACSS/atendimentos-em-urgencia-triagem-manchester.csv")
hospitals_in = read_csv2("data/CSH/1. ACSS/atividade-de-internamento-hospitalar.csv") # inpatient days
hospitals_OR = read_csv2("data/CSH/1. ACSS/ocupacao-do-internamento.csv") # Occupancy rate
hospitals_app = read_csv2("data/CSH/1. ACSS/01_sica_evolucao-mensal-das-consultas-medicas-hospitalares.csv") # appointments
hospitals_beds = read_csv2("data/CSH/1. ACSS/lotacao-praticada-por-tipo-de-cama.csv")
hospitals_surg = read_csv2("data/CSH/1. ACSS/intervencoes-cirurgicas.csv") # 
hospitals_wait = read_csv2("data/CSH/1. ACSS/demora-media-antes-da-cirurgia.csv") 

  # Benchmarking Website (Cost variables)
  c_operational = read_csv("data/CSH/2. Benchmarking/custos_op.csv")
  c_staff = read_csv("data/CSH/2. Benchmarking/custos_pess.csv")
  c_staff_adjusted = read_csv("data/CSH/2. Benchmarking/custos_pess_aj.csv")
  c_pharma = read_csv("data/CSH/2. Benchmarking/custos_farm.csv")
  c_medicine = read_csv("data/CSH/2. Benchmarking/custos_med.csv")
  c_materials = read_csv("data/CSH/2. Benchmarking/custos_mat.csv")
  c_fse = read_csv("data/CSH/2. Benchmarking/custos_fse.csv")
  n_polos = read_excel("data/CSH/6. Others/n_polos.xlsx") # key for nº of hospitals
  
  # Control Variables and Demographic characteristics
  municipios = read_dta("data/CSH/5. GDH/correspondencia_hosp_concelho_16.dta") # key for municipality and hospital
  key_mun = read_excel("data/CSH/5. GDH/keys/key_mun.xlsx")
  key_hosp = read_excel("data/CSH/5. GDH/keys/key_hosp.xlsx")
  key_region = read_excel("data/CSH/5. GDH/keys/key_region.xlsx")
  characteristics = read_csv("data/CSH/7. Characterization/characteristics.csv")
  pop = read_excel("data/CSH/7. Characterization/pop.xlsx")

# C. Data for Açores and Madeira
hospitals_aco = read.csv("data/CSH/3. Açores/Açores.csv") 

# D. Data for Madeira
hospitals_mad = read_csv("data/CSH/8. Madeira/mad.csv")

# 1. Tidy Data ------------------------------------------------------------
  ## 1.1 Operational Costs -------------------------------------------------
  hospitals_fin = hospitals_fin |>
    pivot_longer(!c(hospital,case_mix), names_to = "year", values_to = "operational_ACSS") |>
    mutate(year = as.numeric(year))

  ## 1.2 Operational Costs (Benchmarking) ------------------------------------
  c_operational = c_operational|>
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
      mutate(hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                               "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                               "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                               "Hospital de Braga", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                               "Hospital de Braga", hospital)) |>
      drop_na() |>
      group_by(hospital, year, region, types_beds)  |>
      #filter(n() == 12) |>
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
               R_ocuppancy = R_ocuppancy/100,
               hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                        "Hospital de Vila Franca de Xira", hospital),
               hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                        "Hospital de Vila Franca de Xira", hospital),
               hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                                 "Hospital de Braga", hospital),
               hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                                 "Hospital de Braga", hospital)) |>
        filter(month == 12) |>
        select(year, hospital, region, in_days, R_ocuppancy)

      
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
             urge = rowSums(across(c(n1, n2, n3, n4, n5, n6, n7)), na.rm = TRUE) ,
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                               "Hospital de Braga", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                               "Hospital de Braga", hospital)) |>
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
             app = as.numeric(app),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                               "Hospital de Braga", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                               "Hospital de Braga", hospital)) |>
      drop_na() |>
      filter(month == 12) |>
      select(year, app, hospital, region)
      
    ### E. Beds ------------------------------------------------------------
    hospitals_beds = hospitals_beds |>
      select(-c("Localização Geográfica")) |>
      separate("Período", c("year", "month"), "-") |>
      mutate(year = as.numeric(year), month = as.numeric(month))|>
      rename("region" = "Região", "hospital" = "Instituição", "num_beds" = "Lotação",
             "types_beds" = "Tipo de Camas") |>
      mutate(hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                               "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                               "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                               "Hospital de Braga", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                               "Hospital de Braga", hospital)) |>
      drop_na() |>
      group_by(hospital, year, region)  |>
      summarise(  beds = mean(num_beds,na.rm = TRUE)) 

    ### F. Surgeries --------------------------------------------------------
    hospitals_surg = hospitals_surg |>
      rename("region" = "Região", "hospital" = "Instituição",
             "surg_p" = "Nº Intervenções Cirúrgicas Programadas",
             "surg_c" = "Nº Intervenções Cirúrgicas Convencionais",
             "surg_a" = "Nº Intervenções Cirúrgicas de Ambulatório",
             "surg_u" = "Nº Intervenções Cirúrgicas Urgentes") |>   
      separate("Período", c("year", "month"), "-") |>
      mutate(year = as.numeric(year), month = as.numeric(month),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                      "Hospital de Vila Franca de Xira", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, EPE", 
                               "Hospital de Braga", hospital),
             hospital = ifelse(hospital == "Hospital de Braga, PPP", 
                               "Hospital de Braga", hospital)) |>
      filter(month == 12) |>
      mutate( surg = rowSums(across(c(surg_p, surg_u)), na.rm = TRUE),
              prop_surg_a = surg_a/surg) |> # total surgeries
      select(year, hospital, region, surg, prop_surg_a)
      
    
    ## 1.7 Characteristics - Municipality ----------------------------------
    key_mun = full_join(key_mun,key_region, by ="distrito") |> select(-distrito_nome) |>
      drop_na()
  
    # Arrange municipality key and join
    key_mun = key_mun |>
      mutate(across(c(distrito, concelho), ~as.character(.))) |>
      mutate(distrito = ifelse(nchar(distrito) == 1, str_pad(distrito, width = 2, side = "left", pad = "0"), distrito),
             concelho = ifelse(nchar(concelho) == 1, str_pad(concelho, width = 2, side = "left", pad = "0"), concelho)) |>
      mutate(distrito_concelho = paste0(distrito, concelho)) |>
      select(-c(distrito, concelho))
    
    municipios =  full_join(municipios, key_mun, by = c("distrito_concelho"))
    
    # Join with hospital key
    municipios = full_join(municipios, key_hosp, by = "hosp_id") |> 
      mutate( distrito_concelho = ifelse(hosp_id == 'HSEI' & prop_utentes > 0.5 , '3001', distrito_concelho),
              distrito_concelho = ifelse(hosp_id == 'HSEI' & prop_utentes > 0.2 & prop_utentes < 0.5 , '3002', distrito_concelho)) |>
      drop_na(hosp_id, total) |>
      rename("municipio" = "DES_DCF") |>
      select(-c(d, distrito, concelho))
    
    write.csv(municipios, file = "mun.csv")
    
    # join with characteristics
    characteristics = full_join(municipios, characteristics, by = c("municipio")) 
    write.csv(characteristics, file = "car.csv")
    
    characteristics =  characteristics |>
      drop_na(total) |> 
      filter(prop_utentes > 0.2) |>
      mutate(den = prop_utentes*den, fem = prop_utentes*prop_fem, aging = prop_utentes*aging,
             desemp = prop_utentes*desemp, wage = prop_utentes*wage) |>
      group_by(hospital,hosp_id, year) |>
      summarise(den = mean(den), fem = mean(fem), aging = mean(aging), desemp = mean(desemp),
                wage = mean(wage))
    
    char_aco = characteristics |> filter(hosp_id %in% c("HDES", "HORT", "HSEI")) |>
      group_by(year) |> 
      summarise(den = mean(den), fem = mean(fem), aging = mean(aging), desemp = mean(desemp),
                wage = mean(wage), hospital = "Hospital Açores", hosp_id = "HCRA") 
    
    characteristics = characteristics |> filter(!(hosp_id %in% c("HDES", "HORT", "HSEI")))
    characteristics = bind_rows(characteristics, char_aco) 
    
    ## 1.8 Characteristics - Population ------------------------------------
    
    pop = pop |>
      pivot_longer(!c(hospital, cod_hosp), names_to = "year", values_to = "pop") |>
      mutate(year = as.numeric(year))
    
    ## 1.9 Check missing months -------------------------------------------------
    check_missing_months <- function(data) {
      missing_months <- data |>
        drop_na() |>  # Check for each of the variables if needed
        group_by(hospital, year) |>
        summarise(m_months = 12 - n()) |>
        filter(m_months > 0)
      
      return(missing_months)
    }
  
  check_missing_months(hospitals_ur)

# 2. Joining data frames ---------------------------------------------------
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
                          hospital = ifelse(hospital == "Centro Hospitalar de Vila Nova de Gaia/Espinho, EPE", 
                                            "Centro Hospitalar Vila Nova de Gaia/Espinho, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Arcebispo João Crisóstomo - Cantanhede", 
                                            "Hospital Arcebispo João Crisóstomo", hospital),
                          hospital = ifelse(hospital == "Hospital Arcebispo João Crisostomo - Cantanhede", 
                                            "Hospital Arcebispo João Crisóstomo", hospital),
                          hospital = ifelse(hospital == "Hospital da Senhora da Oliveira Guimarães, EPE", 
                                            "Hospital da Senhora da Oliveira, Guimarães, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Senhora da Oliveira, E.P.E. - Guimarães", 
                                            "Hospital da Senhora da Oliveira, Guimarães, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Distrital Figueira da Foz, EPE", 
                                            "Hospital Distrital da Figueira da Foz, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Distrital Santarém, EPE", 
                                            "Hospital Distrital de Santarém, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Dr. Francisco Zagalo - Ovar", 
                                            "Hospital Dr. Francisco Zagalo", hospital),
                          hospital = ifelse(hospital == "Hospital Garcia de Orta, EPE - Almada", 
                                            "Hospital Garcia de Orta, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Garcia de Orta, E.P.E. - Almada", 
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
                          hospital = ifelse(hospital == "Centro Hospitalar Barreiro/Montijo, E.P.E.", 
                                            "Centro Hospitalar Barreiro/Montijo, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Barreiro Montijo, EPE", 
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
                          hospital = ifelse(hospital == "Hospital Professor Dr. Fernando da Fonseca, E.P.E.", 
                                            "Hospital Professor Doutor Fernando Fonseca, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, PPP", 
                                            "Hospital de Vila Franca de Xira", hospital),
                          hospital = ifelse(hospital == "Hospital de Vila Franca de Xira, EPE", 
                                            "Hospital de Vila Franca de Xira", hospital),
                          hospital = ifelse(hospital == "Hospital Santa Maria Maior, EPE", 
                                            "Hospital Distrital S.Maria Maior, EPE - Barcelos", hospital),
                          hospital = ifelse(hospital == "Hospital Santa Maria Maior, E.P.E. - Barcelos", 
                                            "Hospital Distrital S.Maria Maior, EPE - Barcelos", hospital),
                          hospital = ifelse(hospital == "Hospital de Loures, EPE", 
                                            "Hospital de Loures", hospital),
                          hospital = ifelse(hospital == "Hospital de Loures, PPP", 
                                            "Hospital de Loures", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar de Leiria-Pombal, E.P.E.", 
                                            "Centro Hospitalar de Leiria, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Distrital de Santarém, E.P.E.", 
                                            "Hospital Distrital de Santarém, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital do Espírito Santo - Évora, E.P.E.", 
                                            "Hospital Espírito Santo de Évora, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde da Guarda, E.P.E.", 
                                            "Unidade Local de Saúde da Guarda, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde de Castelo Branco, E.P.E.", 
                                            "Unidade Local de Saúde de Castelo Branco, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde de Matosinhos, E.P.E.", 
                                            "Unidade Local de Saúde de Matosinhos, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde do Baixo Alentejo, E.P.E.", 
                                            "Unidade Local de Saúde do Baixo Alentejo, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde do Litoral Alentejano, E.P.E.", 
                                            "Unidade Local de Saúde do Litoral Alentejano, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde do Norte Alentejano E. P. E.", 
                                            "Unidade Local de Saúde do Norte Alentejano, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde Nordeste, E.P.E.", 
                                            "Unidade Local de Saúde do Nordeste, EPE", hospital),
                          hospital = ifelse(hospital == "Unidade Local de Saúde Nordeste, E.P.E.", 
                                            "Unidade Local de Saúde do Nordeste, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Setúbal, E.P.E", 
                                            "Centro Hospitalar de Setúbal, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar de Tras-os-Montes e Alto Douro, E.P.E.", 
                                            "Centro Hospitalar Trás-os-Montes e Alto Douro, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar de Trás-os-Montes e Alto Douro, EPE", 
                                            "Centro Hospitalar Trás-os-Montes e Alto Douro, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Baixo Vouga, E.P.E.", 
                                            "Centro Hospitalar do Baixo Vouga, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Médio Ave, E.P.E.", 
                                            "Centro Hospitalar do Médio Ave, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Lisboa Ocidental, E.P.E.", 
                                            "Centro Hospitalar de Lisboa Ocidental, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar e Universitário de Coimbra, E.P.E.", 
                                            "Centro Hospitalar e Universitário de Coimbra, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Tâmega e Sousa, E.P.E.", 
                                            "Centro Hospitalar Tâmega e Sousa, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Tondela-Viseu, E.P.E.", 
                                            "Centro Hospitalar Tondela-Viseu, EPE", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar Tondela Viseu, EPE", 
                                            "Centro Hospitalar Tondela-Viseu, EPE", hospital),
                          hospital = ifelse(hospital == "HPP Hospital de Cascais, Dr. José de Almeida", 
                                            "Hospital de Cascais", hospital),
                          hospital = ifelse(hospital == "Hospital de Cascais, PPP", 
                                            "Hospital de Cascais", hospital),
                          hospital = ifelse(hospital == "Hospital Beatriz Ângelo - Loures", 
                                            "Hospital de Loures", hospital),
                          hospital = ifelse(hospital == "Centro Hospitalar do Médio Tejo, EPE", 
                                            "Centro Hospitalar Médio Tejo, EPE", hospital),
                          hospital = ifelse(hospital == "Hospital Espírito Santo de Évora, EPE", 
                                            "Hospital do Espírito Santo de Évora, EPE", hospital))
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
  c_operational = rename_hospitals(c_operational)
  c_staff = rename_hospitals(c_staff)
  c_staff_adjusted = rename_hospitals(c_staff_adjusted)
  c_pharma = rename_hospitals(c_pharma)
  c_medicine = rename_hospitals(c_medicine)
  c_fse = rename_hospitals(c_fse)
  c_materials = rename_hospitals(c_materials)
  characteristics = rename_hospitals(characteristics)
  pop = rename_hospitals(pop)
  
## 2.2 Joining DB ---------------------------------------------------------
  ### A. Joining tables for Continent ---------------------------------------
  # Join all data frames from ACSS in list
  list_df = list(hospitals_ur, hospitals_in, hospitals_OR, hospitals_app,
                 hospitals_beds, hospitals_surg, hospitals_wait)
  
  CSH = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year",
                                                 "region" = "region"), 
                                    .init = NULL)) |> 
    filter(year > 2014) 
  
  # join all databases from benchmarking
  list_df = list(CSH, c_operational, c_staff, c_staff_adjusted,
                 c_pharma, c_medicine, c_fse, c_materials, hospitals_fin, pop)
  CSH = list_df |> 
    reduce(function(x, y) full_join(x, y, by = c("hospital" = "hospital",
                                                 "year" = "year"), 
                                    .init = NULL)) |> 
    filter(year > 2014)  |> mutate(azo = 0, mad = 0)
    
  ### B. Joining tables from Azores ----------------------------------------
  hospitals_aco = hospitals_aco |> 
    group_by(year) |>
    summarise(app = sum(app,na.rm = TRUE), surg = sum(surg, na.rm = TRUE), 
              in_days = sum(in_days, na.rm = TRUE), patient_dis = sum(patient_dis),
              urge = sum(urge), day_app = sum(day_app), operational = sum(operational), cmvmc = sum(cmvmc), 
              staff = sum(staff), beds = sum(beds), pharma = sum(pharma), materials = sum(materials), 
              fse = sum(fse), wait_scheduled_surg = mean(wait_scheduled_surg), wait_app = mean(wait_app, na.rm = TRUE),
              case_mix = mean(case_mix)) |>
    ungroup() |>
    mutate(azo = 1 , mad = 0, operational_ACSS = operational,
           azo = as.factor(azo), mad = as.factor(mad),
           region = "Região de Saúde dos Açores",
           hospital = "Hospital Açores")
  
  hospitals_aco = hospitals_aco |> inner_join(pop, by = c("hospital", "year")) 
  
  CSH = CSH |> filter(cod_hosp != 46, cod_hosp != 47) |>
    mutate(azo = as.factor(azo),
           mad = as.factor(mad))
  CSH = bind_rows(CSH, hospitals_aco)
  
  ### C. Joining tables from Madeira ---------------------------------------
  hospitals_mad = hospitals_mad |>
    mutate(region = "Região de Saúde da Madeira",
           mad = 1 , azo = 0, operational_ACSS = operational,
           mad = as.factor(mad), azo = as.factor(azo))

  CSH = bind_rows(CSH, hospitals_mad)
  
  ### D. Join polos, characteristics ---------------------------------------
  CSH = full_join(CSH, characteristics, by = c("hospital", "year"))
  CSH = full_join(CSH, n_polos, by = "hospital")
  
  ### E. Create new Rate of Occupancy --------------------------------------
  CSH = CSH |> mutate(RO = in_days/ (beds*30.4375*12)) |>
    filter(!str_detect(hospital, "^Instituto Português de Oncologia"),
           !str_detect(hospital, "Centro Hospitalar Psiquiátrico de Lisboa")) |>
    mutate(region = case_when(
      hospital == "Hospital Madeira" ~ "Região de Saúde da Madeira",
      TRUE ~ region )) |>
    mutate(id_region_NUTSII = case_when(
            region == "Região de Saúde do Algarve" ~ "PT15",
            region == "Região de Saúde do Alentejo" ~ "PT18",
            region == "Região de Saúde LVT" ~ "PT17",
            region == "Região de Saúde do Centro" ~ "PT16",
            region == "Região de Saúde Norte" ~ "PT11",
            region == "Região de Saúde dos Açores" ~ "PT20",
            region == "Região de Saúde da Madeira" ~ "PT30",
            TRUE ~ NA_character_)) |>
    drop_na(hospital, year, cod_hosp) |>
    select(-c(day_app, wait_app, cmvmc, dp, avg_operational, avg_fse, avg_staff,
              avg_staff_adjusted, avg_pharma, avg_medicine, medicine, pharma, materials,
              operational, staff_adjusted))
  
# 3. Exporting Data -------------------------------------------------------
save(CSH, file= "0.DataBase/CSH.RData")
write.csv(CSH, file = "0.DataBase/csh.csv")

###### END #####
rm(list=ls())
  
