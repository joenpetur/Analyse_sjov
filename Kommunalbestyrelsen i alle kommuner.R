


library(rvest)
library(tidyverse)


url_samlet <- "https://www.kmdvalg.dk/kv/2021/KMDValgKV.html"

sider <- read_html(url_samlet) %>%
    html_elements(".list-group-item.valg-status-final") |>
    (\(y) bind_cols(html_attr(y, "href") |> enframe(name = NULL),
      html_text(y) |> str_trim() |> enframe(name = NULL)))() |> 
    rename(HTML_SIDE=1, KOMMUNE=2) |> 
  mutate(HTML_SIDE = str_replace_all(HTML_SIDE, "^K", "KM"))
    

hent_kandidater <- function(x){
  
  url <- str_glue("https://www.kmdvalg.dk/kv/2021/{x}")
  
  rows <- url %>%
    read_html() %>%
    html_nodes(".row.table-like-row") |> 
    html_text2() 
  
  data <- map(rows, ~ strsplit(.x, "\n")[[1]] |> (\(y) y[which(y!="\r")])() |> enframe() |> 
                pivot_wider(values_from=value, names_from=name)) |> 
    bind_rows()
  
  colnames(data) <- data[1, ] 
  
  filter(data, row_number()>1)
}

sider
end_data <- map2(sider$HTML_SIDE, sider$KOMMUNE, ~hent_kandidater(.x) |> mutate(KOMMUNE = .y))
  
 
end_data_1 <- end_data |>
  bind_rows() |> 
  mutate(Kandidatliste_1 = case_when(Kandidatliste=="FSF - Socialistisk Folkeparti"~"FSF",
                                     T ~str_extract(Kandidatliste, "\\w{1}")),
         Kandidatliste_2 = case_when(Kandidatliste=="FSF - Socialistisk Folkeparti"~"Socialistisk Folkeparti",
                                     T ~Kandidatliste |> str_remove_all(str_c("^", Kandidatliste_1)) |> 
                                       word(sep = ",|-"))) 
  