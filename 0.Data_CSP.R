# Packages ---------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)


# 0. Data import ----------------------------------------------------------
  # Portugal Continent
  csp = read_dta("data/CSP/csp_dados_continente_acores.dta")
  unidades = read_dta("data/CSP/unidades_aces.dta")
  aces = read_csv("data/CSP/Dados_ACES.csv")
  
  # Madeira
  prod_mad = read_dta("data/CSP/02.madeira/producao_csp.dta")
  ins_mad = read_dta("data/CSP/02.madeira/inscritos_csp.dta")
  c_mad = read_dta("data/CSP/02.madeira/custos_csp.dta")
  polos_mad = read_excel("data/CSP/02.madeira/N_polos.xlsx")

# 1. Tidy Data ------------------------------------------------------------
## 1.1 Portugal Database ------------------------------------------------

unidades = unidades |> rename( "cod_aces" = "cod_uo", "aces1" = "aces" )

CSP = aces |>
  rename("staff" = "Custo_pessoal",
         "inscritos" = "qtd_inscritos_ativos", "inscritos_sMF" = "qtd_inscritos_ativos_s_mdf", 
         "app" = "qtd_cons_med_12m", "app_enf" = "qtd_cons_enf_12m", "inscritos_fem" = "qtd_genero_femin") |> # n of clinics (polos) to recieve patients, proxy for geographical dispersion of health services
  mutate(azo = 0, mad = 0, 
         across(c(azo, mad,cod_aces), as.factor)) |>  
  mutate(prop_nofam_MF = inscritos_sMF/inscritos, # proportion of citizens without family doctor, proxy for illness incidence due to less prevention.  
         prop_fem = inscritos_fem/ inscritos, # proportion of female citizens, proxy for a higher demand of healthcare services *why? 
         #prop_age_0_4 = inscritos_00_04/inscritos, # proportion of citizens by age, proxy for demand of health services and illness incidence.
         #prop_age_5_14 = inscritos_05_14/ inscritos,
         #prop_age_45_64 = inscritos_45_64/ inscritos, 
         #prop_age_65_hig = (inscritos_75m + inscritos_65_74)/ inscritos,
         avg_staff = staff/inscritos) |>
  select(cod_aces, aces, year ,azo, mad,  staff, app, prop_nofam_MF, 
         prop_fem, inscritos, app_enf, nSAP,nUCC, n_polos, n_centros_saude, avg_staff) #, #prop_age_0_4,prop_age_65_hig, inscritos, app_remote, app_dome, avg_staff, n_polos)
  

ages = csp |> rename("staff" = "Custo_pessoal", "app" = "Prod_cons_pres", "app_remote" = "Prod_cons_rem",
                     "unit" = "nome", "app_dome" = "Prod_cons_dom", "n_polos" = "Atendim_polos") |> # n of clinics (polos) to recieve patients, proxy for geographical dispersion of health services
  mutate(azo = ifelse(unit %in% c("USI Santa Maria", "USI S. Miguel", 
                                  "USI Terceira", "USI Graciosa", 
                                  "USI S. Jorge", "USI Pico", 
                                  "USI Faial", "USI Flores", "USI Corvo"), 1, 0)) |>
  mutate(across(c(azo, cod_aces), as.factor)) |>
  filter(azo == 0) |>
  group_by(aces, year, cod_aces) |>
  summarise(inscritos_00_04 = sum(inscritos_00_04, na.rm = T), inscritos_05_14 = sum(inscritos_05_14, na.rm = T), 
            inscritos_45_64 = sum(inscritos_45_64, na.rm = T),inscritos_65_74 = sum(inscritos_65_74, na.rm = T), 
            inscritos_75m = sum(inscritos_75m, na.rm = T)) |>
  ungroup()
  
CSP = full_join(ages, CSP, by = c("year", "cod_aces")) |>
  rename("aces" = "aces.x") |> select(-"aces.y") |>
  mutate(prop_age_0_4 = inscritos_00_04/inscritos, # proportion of citizens by age, proxy for demand of health services and illness incidence.
         prop_age_5_14 = inscritos_05_14/ inscritos,
         prop_age_45_64 = inscritos_45_64/ inscritos, 
         prop_age_65_hig = (inscritos_75m + inscritos_65_74)/ inscritos) 

## 1.2 Açores Database ----------------------------------------------------
csp_aco = csp |>
  rename("staff" = "Custo_pessoal", "app" = "Prod_cons_pres", "app_remote" = "Prod_cons_rem",
         "unit" = "nome", "app_dome" = "Prod_cons_dom", "n_polos" = "Atendim_polos") |> # n of clinics (polos) to recieve patients, proxy for geographical dispersion of health services
  mutate(azo = ifelse(unit %in% c("USI Santa Maria", "USI S. Miguel", 
                                  "USI Terceira", "USI Graciosa", 
                                  "USI S. Jorge", "USI Pico", 
                                  "USI Faial", "USI Flores", "USI Corvo"), 1, 0),
         mad = 0, 
         aces = ifelse(azo == 1, unit, aces),
         cod_aces = ifelse(unit == "USI Corvo", 60001, cod_aces),
         cod_aces = ifelse(unit == "USI Faial", 60002, cod_aces),
         cod_aces = ifelse(unit == "USI Flores", 60003, cod_aces),
         cod_aces = ifelse(unit == "USI Graciosa", 60004, cod_aces),
         cod_aces = ifelse(unit == "USI Pico", 60005, cod_aces),
         cod_aces = ifelse(unit == "USI S. Jorge", 60006, cod_aces),
         cod_aces = ifelse(unit == "USI S. Miguel", 60007, cod_aces),
         cod_aces = ifelse(unit == "USI Santa Maria", 60008, cod_aces),
         cod_aces = ifelse(unit == "USI Terceira", 60009, cod_aces)) |>  
  mutate(across(c(azo,mad, cod_aces, aces), as.factor),
         across(c(year, staff, app, app_dome, app_remote, n_polos), as.numeric)) |>
  filter(azo == 1) |>
  mutate(prop_nofam_MF = 1 - (inscritos_MF/inscritos), # proportion of citizens without family doctor, proxy for illness incidence due to less prevention.  
         prop_fem = inscritos_fem/ inscritos, # proportion of female citizens, proxy for a higher demand of healthcare services *why? 
         prop_age_0_4 = inscritos_00_04/inscritos, # proportion of citizens by age, proxy for demand of health services and illness incidence.
         prop_age_5_14 = inscritos_05_14/ inscritos,
         prop_age_45_64 = inscritos_45_64/ inscritos, 
         prop_age_65_hig = (inscritos_75m + inscritos_65_74)/ inscritos,
         avg_staff = staff/inscritos) |>
  select(cod_aces, aces, year ,azo, mad,  staff, app, prop_nofam_MF, 
         prop_fem, prop_age_0_4,prop_age_65_hig, inscritos, avg_staff, n_polos)
  
CSP = bind_rows(csp_aco, CSP) |> 
  select(-c("inscritos_00_04","inscritos_05_14", "inscritos_45_64", "inscritos_75m", "inscritos_65_74"))
  
## 1.3 Madeira Database ---------------------------------------------------
# Tidy data production
csp_mad = prod_mad |>
  rename("year" = "ano", "cs" = "CentrosdeSaúde") |>
  mutate( csp = case_when(Concelho == "Bom Jesus" ~ "01 FUNCHAL",
          Concelho == "São Roque" ~ "01 FUNCHAL",
          Concelho == "Monte" ~ "01 FUNCHAL",
          Concelho == "Santo António" ~ "01 FUNCHAL",
          Concelho == "Nazaré" ~ "01 FUNCHAL",
          Concelho == "Calheta" ~ "02 CALHETA",
          Concelho == "CS Câmara de Lobos" ~ "03 CÂMARA DE LOBOS",
          Concelho == "Ponta do Sol" ~ "04 PONTA DO SOL",
          Concelho == "Porto Moniz" ~ "05 PORTO MONIZ",
          Concelho == "Ribeira Brava" ~ "06 RIBEIRA BRAVA",
          Concelho == "São Vicente" ~ "07 SÃO VICENTE",
          Concelho == "Machico" ~ "08 MACHICO",
          Concelho == "CS Santa Cruz" ~ "09 SANTA CRUZ",
          Concelho == "Santana" ~ "10 SANTANA",
          Concelho == "Porto Santo" ~ "11 PORTO SANTO"),
          app = prod21 + prod20 + prod17,
          app_enf = prod1 + prod2 + prod3 + prod11,
          app_pres = prod21,
          app_pres_enf = prod3) |>
  select(app, app_enf, app_pres, app_pres_enf, year, csp) |>
  group_by(year, csp) |>
  summarise(app = sum(app), app_pres = sum(app_pres), app_pres_enf = sum(app_pres_enf),
            app_enf = sum(app_enf))

# Tidy data costs 
c_mad2 = c_mad |>
  rename("staff" = "custo83", "year" = "ano") |> 
  select(staff, csp, year) 

# Tidy data inscritos 
inscritos = ins_mad |> 
  mutate(idade = str_replace_all(idade, "\\s|-", "")) |>
  filter(!str_detect(idade, "^Média")) |>
  mutate(csp = case_when(concelho == "Nazaré" & cs == "CS Câmara de Lobos" ~ "03 CÂMARA DE LOBOS",
                         concelho == "Nazaré" & cs == "CS Funchal Zona II" ~ "01 FUNCHAL",
                         concelho == "Nazaré" & cs == "CS Santa Cruz" ~ "09 SANTA CRUZ",
                         concelho == "São Roque" ~ "01 FUNCHAL",                 
                         concelho == "Monte" ~ "01 FUNCHAL",                 
                         concelho == "Bom Jesus" ~ "01 FUNCHAL",                 
                         concelho == "Santo António" ~ "01 FUNCHAL",                 
                         concelho == "Calheta" ~ "02 CALHETA",
                         concelho == "Ponta do Sol" ~ "04 PONTA DO SOL",
                         concelho == "Porto Moniz" ~ "05 PORTO MONIZ",
                         concelho == "Ribeira Brava" ~ "06 RIBEIRA BRAVA",
                         concelho == "São Vicente" ~ "07 SÃO VICENTE",
                         concelho == "Machico" ~ "08 MACHICO",
                         concelho == "Santana" ~ "10 SANTANA",
                         concelho == "Porto Santo" ~ "11 PORTO SANTO")) |>
  select(-concelho) |>
  group_by(ano, csp, idade, genero)|>
  summarize(yinscritos = sum(yinscritos), yinscritos_sem_MGF = sum(yinscritos_sem_MGF)) |>
  ungroup() |>
  pivot_wider(names_from = idade, values_from = c(yinscritos,yinscritos_sem_MGF)) |>
  mutate(age_65_hig = rowSums(across(c(yinscritos_100104anos, yinscritos_105emaisanos,
                                       yinscritos_7579anos, yinscritos_8084anos,
                                       yinscritos_8589anos, yinscritos_9094anos,
                                       yinscritos_9599anos, yinscritos_6569anos,
                                       yinscritos_7074anos)), na.rm = TRUE)) |>
  select(c(age_65_hig, yinscritos_04anos, yinscritos_TotalAno, 
           yinscritos_sem_MGF_TotalAno, genero, ano, csp)) |>
  pivot_wider(names_from = genero, values_from = c(yinscritos_sem_MGF_TotalAno, 
                                                   yinscritos_TotalAno, age_65_hig, yinscritos_04anos)) |>
  ungroup() |>
  mutate(inscritos = yinscritos_TotalAno_F + yinscritos_TotalAno_M,
         prop_age_0_4 = (yinscritos_04anos_F + yinscritos_04anos_M)/inscritos,
         prop_age_65_hig = (age_65_hig_F + age_65_hig_M)/inscritos,
         prop_nofam_MF = (yinscritos_sem_MGF_TotalAno_F + yinscritos_sem_MGF_TotalAno_M)/inscritos,
         prop_fem = yinscritos_TotalAno_F/inscritos) |>
  rename("year" = "ano") |>
  select(inscritos, prop_age_0_4, prop_age_65_hig, prop_fem, prop_nofam_MF, year, csp)

#Join Madeira Data-Frames
csp_mad = full_join(inscritos, csp_mad, by = c("csp", "year")) 

csp_mad = full_join(csp_mad, c_mad2, by = c("csp", "year")) 

csp_mad = csp_mad |> rename("aces" = "csp") |>	
  mutate( azo = 0, 
          mad = 1, 
          cod_aces = 0, 
          cod_aces = ifelse(aces == "01 FUNCHAL", 70001, cod_aces),
          cod_aces = ifelse(aces == "02 CALHETA", 70002, cod_aces),
          cod_aces = ifelse(aces == "03 CÂMARA DE LOBOS", 70003, cod_aces),
          cod_aces = ifelse(aces == "04 PONTA DO SOL", 70004, cod_aces),
          cod_aces = ifelse(aces == "05 PORTO MONIZ", 70005, cod_aces),
          cod_aces = ifelse(aces == "06 RIBEIRA BRAVA", 70006, cod_aces),
          cod_aces = ifelse(aces == "07 SÃO VICENTE", 70007, cod_aces),
          cod_aces = ifelse(aces == "08 MACHICO", 70008, cod_aces),
          cod_aces = ifelse(aces == "09 SANTA CRUZ", 70009, cod_aces),
          cod_aces = ifelse(aces == "10 SANTANA", 70010, cod_aces),
          cod_aces = ifelse(aces == "11 PORTO SANTO", 70011, cod_aces),
          avg_staff = staff/ inscritos,
          across(c(azo, mad, aces, cod_aces), as.factor))

csp_mad = full_join(csp_mad, polos_mad, by = c("aces", "year")) 
# 2. Join Databases ------------------------------------------------------- 

CSP = bind_rows(CSP, csp_mad) |> select(-c(prop_age_5_14, prop_age_45_64))

# 3. Exporting Data -------------------------------------------------------
save(CSP, file= "0.DataBase/CSP.RData")
write.csv(CSP, file = "0.DataBase/csp.csv") 

###### END ######
rm(list=ls())
