# Packages ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)


# 0. Data import ----------------------------------------------------------
# Portugal & Azores
csp = read_dta("data/CSP/csp_dados_continente_acores.dta") # for semicolon separated values
unidades = read_dta("data/CSP/unidades_aces.dta")

# Madeira
prod_mad = read_dta("data/CSP/madeira/producao_csp.dta")
ins_mad = read_dta("data/CSP/madeira/inscritos_csp.dta")
c_mad = read_dta("data/CSP/madeira/custos_csp.dta")
polos_mad = read_excel("data/CSP/madeira/N_polos.xlsx")
write.csv(ins_mad, file = "filename2.csv") 

# 1. Tidy Data ------------------------------------------------------------
## 1.1 Portugal Database and Azores ---------------------------------------

unidades = unidades |> rename( "cod_aces" = "cod_uo", "aces1" = "aces" )

CSP = csp |>
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
  filter((azo == 1) | (azo == 0 & inscritos >= quantile(inscritos, 0.05) & inscritos <= quantile(inscritos, 0.95))) |>
  mutate(across(c(azo,mad, id, unit, aces), as.factor),
         across(c(year, staff, app, app_dome, app_remote, n_polos), as.numeric)) |>
  filter(unit != "UCSP Sete Rios") |>
  group_by(aces, year,azo, mad, cod_aces) |>
  summarise(staff = sum(staff), app = sum(app), inscritos_MF = sum(inscritos_MF),
            inscritos_fem = sum(inscritos_fem), inscritos_00_04 = sum(inscritos_00_04), 
            inscritos_05_14 = sum(inscritos_05_14), inscritos_45_64 = sum(inscritos_45_64),
            inscritos_65_74 = sum(inscritos_65_74), inscritos_75m = sum(inscritos_75m),
            inscritos = sum(inscritos),
            n_polos = sum(n_polos), n_units = n_distinct(cod_uf),
            app_remote = sum(app_remote), app_dome = sum(app_dome)) |>
    mutate(prop_nofam_MF = 1 - (inscritos_MF/inscritos), # proportion of citizens without family doctor, proxy for illness incidence due to less prevention.  
           prop_fem = inscritos_fem/ inscritos, # proportion of female citizens, proxy for a higher demand of healthcare services *why? 
           prop_age_0_4 = inscritos_00_04/inscritos, # proportion of citizens by age, proxy for demand of health services and illness incidence.
           prop_age_5_14 = inscritos_05_14/ inscritos,
           prop_age_45_64 = inscritos_45_64/ inscritos, 
           prop_age_65_hig = (inscritos_75m + inscritos_65_74)/ inscritos,
           avg_staff = staff/inscritos) |>
  select(cod_aces, aces, year ,azo, mad,  staff, app, n_polos, prop_nofam_MF, 
         prop_fem, prop_age_0_4,prop_age_65_hig, inscritos, app_remote, app_dome, avg_staff) |>
  ungroup() 

# join Database with ACES units
CSP = full_join(CSP, unidades, by= c("cod_aces", "year"))

CSP =CSP |> 
  select(-c(aces1, nUSFB, nUSFA, nURAP, nUCSP, nECCI)) #|> filter(azo == 0) |> drop_na()

## 1.2 Madeira Database ---------------------------------------------------
# Tidy data production
csp_mad = prod_mad |>
  rename("app" = "prod21", "app_remote" = "prod20", "app_dome" = "prod17",
         "year" = "ano", "cs" = "CentrosdeSaúde") |>
  select(app, year, app_dome, app_remote, cs)

# Tidy data costs 
c_mad2 = c_mad |>
  rename("staff" = "custo83", "cs" = "csp", "year" = "ano") |>
  select(staff, cs, year) |>
  mutate(cs = ifelse(cs == "01 FUNCHAL", "CS Funchal", cs),
         cs = ifelse(cs == "02 CALHETA", "CS Zona Oeste", cs),
         cs = ifelse(cs == "03 CÂMARA DE LOBOS", "CS Câmara de Lobos", cs),
         cs = ifelse(cs == "04 PONTA DO SOL", "CS Zona Oeste", cs),
         cs = ifelse(cs == "05 PORTO MONIZ", "CS Zona Oeste", cs),
         cs = ifelse(cs == "06 RIBEIRA BRAVA", "CS Zona Oeste", cs),
         cs = ifelse(cs == "07 SÃO VICENTE", "CS Zona Oeste", cs),
         cs = ifelse(cs == "08 MACHICO", "CS Zona Leste", cs),
         cs = ifelse(cs == "09 SANTA CRUZ", "CS Santa Cruz", cs),
         cs = ifelse(cs == "10 SANTANA", "CS Zona Leste", cs),
         cs = ifelse(cs == "11 PORTO SANTO", "CS Dr Francisco Rodrigues Jardim", cs))

# Tidy data inscritos 
inscritos = ins_mad |> 
  mutate(idade = str_replace_all(idade, "\\s|-", "")) |>
  filter(!str_detect(idade, "^Média")) |>
  select(-concelho) |>
  group_by(ano, cs, idade, genero)|>
  summarize(yinscritos = sum(yinscritos), yinscritos_sem_MGF = sum(yinscritos_sem_MGF)) |>
  ungroup() |>
  pivot_wider(names_from = idade, values_from = c(yinscritos,yinscritos_sem_MGF)) |>
  mutate(age_65_hig = rowSums(across(c(yinscritos_100104anos, yinscritos_105emaisanos,
                                       yinscritos_7579anos, yinscritos_8084anos,
                                       yinscritos_8589anos, yinscritos_9094anos,
                                       yinscritos_9599anos, yinscritos_6569anos,
                                       yinscritos_7074anos)), na.rm = TRUE)) |>
  select(c(age_65_hig, yinscritos_04anos, yinscritos_TotalAno, 
           yinscritos_sem_MGF_TotalAno, genero, ano, cs)) |>
  pivot_wider(names_from = genero, values_from = c(yinscritos_sem_MGF_TotalAno, 
                                                   yinscritos_TotalAno, age_65_hig, yinscritos_04anos)) |>
  ungroup() |>
  mutate(inscritos = yinscritos_TotalAno_F + yinscritos_TotalAno_M,
         prop_age_0_4 = (yinscritos_04anos_F + yinscritos_04anos_M)/inscritos,
         prop_age_65_hig = (age_65_hig_F + age_65_hig_M)/inscritos,
         prop_nofam_MF = (yinscritos_sem_MGF_TotalAno_F + yinscritos_sem_MGF_TotalAno_M)/inscritos,
         prop_fem = yinscritos_TotalAno_F/inscritos) |>
  rename("year" = "ano") |>
  select(inscritos, prop_age_0_4, prop_age_65_hig, prop_fem, prop_nofam_MF, year, cs)

#Join Madeira DataFrames
csp_mad = full_join(inscritos, csp_mad, by = c("cs", "year")) 

csp_mad = csp_mad |>
  mutate(cs = ifelse(cs == "CS Funchal Zona I", "CS Funchal", cs),
         cs = ifelse(cs == "CS Funchal Zona II", "CS Funchal", cs)) |>
  group_by(year, cs) |>
  summarise(inscritos = sum(inscritos), prop_age_0_4 = mean(prop_age_0_4),
            prop_fem = mean(prop_fem), app = sum(app), prop_nofam_MF = mean(prop_nofam_MF), 
            prop_age_65_hig = mean(prop_age_65_hig), app_dome = sum(app_dome),
            app_remote= sum(app_remote))

csp_mad = full_join(csp_mad, c_mad2, by = c("cs", "year")) 

csp_mad = csp_mad |> rename("aces" = "cs") |>	
  mutate( azo = 0, 
          mad = 1, 
          azo = as.factor(azo),
          mad = as.factor(mad),
          cod_aces = 0, 
          cod_aces = ifelse(aces == "CS Funchal", 70001, cod_aces),
          cod_aces = ifelse(aces == "CS Zona Oeste", 70002, cod_aces),
          cod_aces = ifelse(aces == "CS Câmara de Lobos", 70003, cod_aces),
          cod_aces = ifelse(aces == "CS Zona Leste", 70004, cod_aces),
          cod_aces = ifelse(aces == "CS Santa Cruz", 70005, cod_aces),
          cod_aces = ifelse(aces == "CS Dr Francisco Rodrigues Jardim", 70001, cod_aces),
          avg_staff = staff/ inscritos)
csp_mad = full_join(csp_mad, polos_mad, by = c("aces", "year")) 
# 2. Join Databases ------------------------------------------------------- 

CSP = bind_rows(CSP, csp_mad)

# 3. Exporting Data -------------------------------------------------------
save(CSP, file= "0.DataBase/CSP.RData")
write.csv(CSP, file = "0.DataBase/csp.csv") 

###### END ######
rm(list=ls())
