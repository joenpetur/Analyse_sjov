


library(haven)
library(rvest)
library(xlsx)
library(tidyverse)


flekstrafik <- 175322


mydata <- readr::read_csv2("https://www.tu2023.dk/meta/meta_sps.php", 
                           locale = locale(encoding = "latin1")) |> 
  rename(col=1)


data <- mydata |> 
  mutate(til_split = str_detect(col, "add value labels "),
         col = str_remove(col, "(?<=\\<).+(?=\\>)") |> 
           str_remove_all("<>|add value labels ") |> 
           str_squish(),
         ant_tab = cumsum(til_split)) |> 
  select(col, ant_tab) |> 
  filter(ant_tab!=0)

liste_data <- split(data, f = data$ant_tab) 

liste_navne <- map_dfr(liste_data, ~.x |> 
      filter(dplyr::row_number()==1)) |> pull("col")


liste_excel <- map2_dfr(liste_data,liste_navne, ~.x |> 
      filter(dplyr::row_number()>1) |> 
       mutate(variabel = .y,
              kode = str_extract(col, "^[^\\s]+"),
              tekst = str_extract(col, "\\s.+") |> str_remove_all("'") |> str_squish(),
              .keep = "used") |> 
        filter(!is.na(kode)) |> 
        select(-col)) |> 
  distinct()

liste_excel |> janitor::get_dupes() |> view()

write.xlsx2(liste_excel, "TU_variable.xlsx", append = TRUE, overwrite = T)
