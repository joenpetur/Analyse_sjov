

install.packages("gsheet")

library(gsheet)
library(tidyverse)
library(knitr)
library(kableExtra)
library(janitor)
library(lubridate)

gsheet2tbl('https://docs.google.com/spreadsheets/d/1wVlhhg6yD_7bAkz5K90OjxiXr2s72HaXLLAPjbhrLr0/edit?fbclid=IwAR1YNkGpZxN1_u9Yy0b4GcwSzVUgc6KKa0b2L1vIThcP-VuGrL8ZZputLFk#gid=0')

data_liste <- list("2016"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=1642163847",
                   "2017"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=185231049",
                   "2018"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=0",
                   "2019"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=590637056",
                   "2020"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=2068331676",
                   "2021"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=310931621",
                   "2022"="https://docs.google.com/spreadsheets/d/13l23QHC7fZluj-qHOgIwbUT3G4sqqmLWFo-VK0S5ZGg/edit#gid=879613812"
)

data <- map2_dfr(names(data_liste), data_liste, 
    ~gsheet2tbl(.y) |> mutate(AAR=.x))

til_ber <- data |> 
  # filter(str_detect(type, "Motor|Udg", negate = TRUE)) |> 
  filter(str_detect(type, "Inrigger|Coastal")) |> 
  mutate(type_end = word(type),
    antal_saedder= ifelse(str_detect(type, "Inrigger|Coastal 4x"), parse_number(type)+1, parse_number(type)),
    .keep = "unused")


aar_type <- til_ber |> 
  group_by(AAR, type_end) |> 
  summarise(antal_baade = n(),
            antal_ture = sum(antal_ture), 
            antal_km = sum(antal_km), 
            .groups = "drop_last") |>
  mutate(a_antal_baade = antal_baade/sum(antal_baade)*100,
    a_antal_ture = antal_ture/sum(antal_ture)*100,
    a_antal_km = antal_km/sum(antal_km)*100)
  

aar_type |> count(AAR, wt = antal_ture)

til_ber |> 
  mutate(pr_person_km = antal_km*antal_saedder) |> 
  group_by(AAR, type_end) |> 
  summarise(pr_person_km = sum(pr_person_km), 
            .groups = "drop_last") |>
  mutate(a_pr_person_km = pr_person_km/sum(pr_person_km)*100)

         