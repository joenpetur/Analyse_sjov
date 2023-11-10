
Sys.setlocale(locale="Danish")

library(readxl)
library(tidyverse)
library(lubridate)
library(highcharter)
library(lubridate)

fra_date <- as.Date("2023-03-01")
til_date <- as.Date("2023-06-01")

afd_chef <- c("TRM Claus Andersen", "TRM Flemming Schiller", "TRM Thomas Jørgensen", "TRM Kirstine F. Hindsberger")

# Read in the Excel file
data <- read_excel("data/Joen1.xlsx",
                   .name_repair = ~str_to_upper(.x) |> 
                     str_replace_all(c(" "="_")) |> 
                     str_remove("\\:")) |> 
  mutate(GODKENDER_NAVN = str_squish(GODKENDER_NAVN),
         GODKENDER_ENHED =str_squish(GODKENDER_ENHED),
         HANDLING_DATO = as.Date(HANDLING_DATO))
 
med_arb_flere_kt <- read_xlsx("data/HR_personer_i_flere_kt.xlsx") |> 
  mutate(FRA_DATO_1 = if_else(as.numeric(FRA_DATO)<40000,
                              fra_date,
                              as.Date(as.double(FRA_DATO), origin = "1899-12-30")),
         TIL_DATO_1 = if_else(TIL_DATO=="Nu", 
                              til_date,
                              as.Date(as.double(TIL_DATO), origin = "1899-12-30")),
         .keep = "unused") |>
  filter(!is.na(FRA_DATO_1) & FRA_DATO_1<TIL_DATO_1) |>
  select(-KOMMENTAR) |> 
  rename_with(~str_remove(.x, "_1")) |> 
  bind_rows(tribble(~ANSVARLIG, ~ANSVARLIG_ENHED, ~FRA_DATO, ~TIL_DATO,
                 "TRM Maria Bundgaard Bonabi", "Kollektiv Trafikkontoret", as.Date("2022-01-01"), as.Date("2023-04-16"), 
                 "TRM Maria Bundgaard Bonabi", "Bane- og Luftfartskontoret", as.Date("2023-04-17"), as.Date("2023-08-01")))

med_arb <- data |> 
  filter(GODKENDER_ENHED!="Nye brugere") |> 
  distinct(GODKENDER_NAVN, GODKENDER_ENHED, HANDLING_DATO) |> 
  summarise(MIN_HANDLING = min(HANDLING_DATO),
            MAX_HANDLING = max(HANDLING_DATO),
            .by = c("GODKENDER_NAVN", "GODKENDER_ENHED")
  ) |> 
  mutate(GODKENDER_ENHED = case_when(GODKENDER_ENHED=="Klima- og Analysekontoret"~"Center for Klima- og Analyse",
                                     GODKENDER_ENHED=="Budget- og Bygningsenheden"~"Budgetenheden",
                                     GODKENDER_ENHED=="Lovkontoret"~"Retskontoret",
                                     T~GODKENDER_ENHED),
         GODKENDER_NAVN = case_when(GODKENDER_NAVN== "TRM Lisa Pontoppidan Chahil" ~"Færdselskontoret",
                                    T~GODKENDER_NAVN) |>
           str_squish()) |> 
  filter(MAX_HANDLING>fra_date) |> 
  add_count(GODKENDER_NAVN, name = "ANTAL_KT")

  
med_m_fl_kt <- med_arb |>
  filter(ANTAL_KT>1) |> 
  distinct(GODKENDER_NAVN, ANTAL_KT) |>
  left_join(med_arb_flere_kt |> select(-ANTAL_KT), by = join_by(GODKENDER_NAVN==ANSVARLIG)) |> 
  # filter(!is.na(ANSVARLIG_ENHED)) |> 
  rename(GODKENDER_ENHED=ANSVARLIG_ENHED)
  
  
datoer <- seq.Date(fra_date, til_date, by = "days") |> 
  enframe(name=NULL, value = "DATO")

# med_arb_enhed <- read_xlsx("data/Medarb_Enhed.xlsx", 
#                            .name_repair = ~str_replace_all(.x, " ", "_") |> 
#                              str_to_upper()) |> 
#   filter(ANSVARLIG %in% med_arb_2022 & !ANSVARLIG_ENHED %in% c("Nye brugere", "Ligestillingsafdelingen")) |> 
#   mutate(ANSVARLIG_ENHED = case_when(ANSVARLIG_ENHED=="Klima- og Analysekontoret"~"Center for Klima- og Analyse",
#                                      ANSVARLIG_ENHED=="Budget- og Bygningsenheden"~"Budgetenheden",
#                                      ANSVARLIG_ENHED=="Lovkontoret"~"Retskontoret",
#                                      T~ANSVARLIG_ENHED),
#          ANSVARLIG = str_squish(ANSVARLIG)) |> 
#   distinct() 


slet_person <- c("TRM Natascha Parnow", "TRM Natascha Kirstine Larsen", 
                 "TRM Tina Persson", "TRM Peter Fløj-Jensen")


# write.csv2(x = med_m_fl_kt, file = "personer_i_flere_kt.csv")

  
start_slut_dato <- med_arb |> 
  filter(ANTAL_KT==1) |> 
  mutate(GODKENDER_NAVN = str_squish(GODKENDER_NAVN)) |> 
  reframe(START_DATO = MIN_HANDLING |> floor_date(unit = "month"),
          SLUT_DATO =MAX_HANDLING |> ceiling_date(unit = "month"),
          .by =c("GODKENDER_NAVN"))

med_arb_enhed_m_dato <- med_arb |>  
  filter(ANTAL_KT==1) |> 
  filter(!GODKENDER_NAVN %in% slet_person) |> 
  anti_join(med_m_fl_kt, by = "GODKENDER_NAVN") |> 
  mutate(ANTAL_KT=1,
         FRA_DATO = fra_date,
         TIL_DATO = til_date) |> 
  mutate(GODKENDER_ENHED = case_when(GODKENDER_ENHED %in% c("IT- og Serviceenheden", "Selskabs- og Koncernstyringsenheden",
                                                            "Budgetenheden", "HR-enheden", "Økonomistyringsenheden") ~"CØHRK",
                                     T ~GODKENDER_ENHED)) |> 
  left_join(start_slut_dato |> select(GODKENDER_NAVN, START_DATO, SLUT_DATO), join_by(GODKENDER_NAVN) ) |> 
  mutate(FRA_DATO = pmax(FRA_DATO, START_DATO ),
         TIL_DATO = pmin(TIL_DATO, SLUT_DATO))


 
dage_pr_kt <- med_arb_enhed_m_dato |> 
  bind_rows(med_m_fl_kt) |> 
  mutate(GODKENDER_NAVN = str_squish(GODKENDER_NAVN)) |> 
  distinct() |> 
  filter(FRA_DATO>=fra_date & FRA_DATO<=til_date |
           TIL_DATO>=fra_date & TIL_DATO<=til_date) |>
#Ret til og fra daoter
  mutate(FRA_DATO_1 = pmax(fra_date, FRA_DATO),
         TIL_DATO_1 = pmin(til_date, TIL_DATO),
         ANTAL_DAGE = difftime(TIL_DATO_1, FRA_DATO_1, units = "days")+1) |> 
  mutate(GODKENDER_ENHED = case_when(GODKENDER_ENHED %in% c("IT- og Serviceenheden", "Selskabs- og Koncernstyringsenheden",
                                                            "Budgetenheden", "HR-enheden", "Økonomistyringsenheden") ~"CØHRK",
                                     T ~GODKENDER_ENHED)) |> 
  reframe(ANTAL_DAGE = sum(ANTAL_DAGE),
          ANTAL_FULD_MED = as.numeric(ANTAL_DAGE/as.double(til_date-fra_date)) |> round(2),
          .by = "GODKENDER_ENHED")
#   
# data |> 
#   filter(year(HANDLING_DATO)==2022) |> 
#   left_join(med_arb_enhed_m_dato |> 
#               bind_rows(med_arb_flere_kt), 
#             by = join_by(GODKENDER_NAVN==ANSVARLIG, HANDLING_DATO >=FRA_DATO, HANDLING_DATO <=TIL_DATO)) |> 
#   # filter(!is.na(ANSVARLIG_ENHED), !ANSVARLIG_ENHED %in% c("Direktionen", "Minister- og Ledelsessekretariatet")) |> 
#   reframe(ANTAL = n(),
#           .by = c("GODKENDER_NAVN", "ANSVARLIG_ENHED", "HANDLING")) |> view()
  # 

til_piv |> 
  count(GODKENDER_ENHED)

til_piv <- data |> 
  filter(HANDLING_DATO>=fra_date & HANDLING_DATO<til_date) |> 
  select(-GODKENDER_ENHED ) |> 
  left_join(med_arb_enhed_m_dato |> 
              bind_rows(med_arb_flere_kt), 
            by = join_by(GODKENDER_NAVN, HANDLING_DATO >=FRA_DATO, HANDLING_DATO <=TIL_DATO)) |> 
  left_join(dage_pr_kt,by = join_by(GODKENDER_ENHED)) |> 
  filter(!is.na(GODKENDER_ENHED), !GODKENDER_ENHED %in% c("Direktionen", "Minister- og Ledelsessekretariatet")) |> 
  group_by(across(c("GODKENDER_ENHED", "HANDLING", "HANDLING_DATO", "ANTAL_FULD_MED"))) |> 
  summarise(ANTAL = n(),
            .groups = "drop") |> 
  group_by(GODKENDER_ENHED, HANDLING) |> 
  mutate(AKK_ANTAL = cumsum(ANTAL),
         AKK_ANTAL_V = AKK_ANTAL/ANTAL_FULD_MED,
         # AKK_ANTAL_V = ifelse(GODKENDER_ENHED=="Færdselskontoret", AKK_ANTAL_V*12,AKK_ANTAL_V)
         AKK_ANTAL_V = ifelse(GODKENDER_ENHED=="Bane- og Luftfartskontoret", AKK_ANTAL_V*1.065,AKK_ANTAL_V),
         AKK_ANTAL_V = ifelse(GODKENDER_ENHED=="Færdselskontoret", AKK_ANTAL_V*1.45,AKK_ANTAL_V)  |> round(1)              
         # AKK_ANTAL_V = ifelse(HANDLING_DATO>=as.Date("2023-05-25") & GODKENDER_ENHED=="Bane- og Luftfartskontoret", 
         #                      58.2,AKK_ANTAL_V),
         # AKK_ANTAL_V = ifelse(GODKENDER_ENHED=="Færdselskontoret", AKK_ANTAL_V*1.5,AKK_ANTAL_V),  
         ) |>
  ungroup() 

slet_dato <- c("2023-04-06", "2023-04-07", "2023-04-08", "2023-04-09", "2023-04-10", "2023-05-18",
               "2023-05-19", "2023-05-20", "2023-05-21")

kontorer_godk_bcr <- datoer |> 
  left_join(til_piv |> 
              filter(HANDLING=="Godkendt") |> 
              pivot_wider(id_cols = "HANDLING_DATO", names_from = "GODKENDER_ENHED", values_from = "AKK_ANTAL_V") |> 
              arrange(HANDLING_DATO),
            by = join_by(DATO==HANDLING_DATO)) |> 
  mutate(across(where(is.integer), ~ifelse(DATO==fra_date, 0, .x)),
         # across(-"DATO", ~fill(.x, .direction = "down")),
         across(-"DATO", ~(fill(cur_data(), .x, .direction = 'down')$.x))) |> 
  filter(!DATO %in% slet_dato) |> 
  add_row(tibble(DATO =as.Date("2023-02-28"))) |> 
  arrange(DATO)


write_csv2(kontorer_godk_bcr, "data/bcr_kontor_godk.csv")

kontorer_retur_bcr <- datoer |> 
  left_join(til_piv |> 
              filter(HANDLING=="Returneret") |> 
              pivot_wider(id_cols = "HANDLING_DATO", names_from = "GODKENDER_ENHED", values_from = "AKK_ANTAL_V") |> 
              arrange(HANDLING_DATO),
            by = join_by(DATO==HANDLING_DATO))  |> 
  mutate(across(where(is.integer), ~ifelse(DATO==fra_date, 0, .x)),
         # across(-"DATO", ~fill(.x, .direction = "down")),
         across(-"DATO", ~(fill(cur_data(), .x, .direction = 'down')$.x)))

write.csv2(kontorer_retur_bcr, "data/bcr_kontor_retur.csv")
