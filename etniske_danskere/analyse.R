
library(statbank)
library(tidyverse)
library(lubridate)
library(highcharter)
library(TRMvisual)
library(scales)
library(ggthemes)
library(zoo)

tbl_dst("FOLK1C", "da")$HERKOMST


pop_data <- tbl_dst("FOLK1C", "da") |> 
  filter(`OMRÅDE`=="000", `KØN`=="TOT", ALDER =="IALT", IELAND=="0000", HERKOMST!="TOT", substr(TID, 1, 4)>2018) |> 
  select(- OMRÅDE, -KØN, -ALDER, -IELAND) |> 
  use_labels() |> 
  collect()


tbl_dst("FOLK1C", "da")$HERKOMST

data_til_fig <- pop_data |> 
  group_by(HERKOMST) |> 
  mutate(MAX_INDHOLD = max(INDHOLD),
         DATE = as.Date(as.yearqtr(TID, format = "%YK%q")),
         MAX_DATE = ifelse(INDHOLD==MAX_INDHOLD, 1, NA) )  |> 
  fill(MAX_DATE, .direction = "down") |> 
  mutate(DIFF = ifelse(MAX_DATE==1, INDHOLD - MAX_INDHOLD, 0),
         HERKOMST = factor(HERKOMST, levels = c("Personer med dansk oprindelse", "Indvandrere", "Efterkommere"))) |> 
  ungroup()


udv_datoer <- data_til_fig |> filter(!is.na(DIFF)) |> pull(DATE) |> unique()

data_til_fig_1 <- data_til_fig |> 
  filter(DATE %in% udv_datoer) |> 
  mutate(DIFF = INDHOLD - (ifelse(DATE==min(DATE), INDHOLD,0) |> max()),
         .by = "HERKOMST") |> 
  select(-MAX_DATE)



data_til_fig_1 |> 
  hchart("line", hcaes(x=DATE, y =DIFF, group = HERKOMST), marker = FALSE,  opposite=TRUE)

format_dates <- function(x) {
  months <- strftime(x, format = "%b")              # Abbreviated name of the month.
  years <- lubridate::year(x)  
  str_c(months, str_sub(years, 3,4))
  
}


breaks <- data_til_fig_1$DATE %>% 
  unique() %>% 
  (\(y) seq.Date(min(y), max(y), by = "3 month"))()

data_til_fig_1 |> 
  ggplot() +
  geom_line(aes(x=DATE, y =DIFF, color = HERKOMST), size=1) +
  geom_hline(yintercept = 0, color = "black", size = 1, linetype="dashed") +
  ggtitle("Befolkningsudvikling i herkomstgrupper\nsiden 1. oktober 2021") +
  scale_x_date(name = "",
               breaks = breaks,          # Date labels for each month.
               minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
               expand = expansion(add = 0),  # Add 15 days to the x-axis on the left and on the right.
               labels = format_dates, position = "top") + 
  scale_y_continuous(name = "Antal i tusind", labels = label_number(scale = 1/1000), limits = c(-20000, 85000),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  theme_foundation(base_size = 12, base_family = "sans") + 
  theme(line = element_line(linetype = 1, colour = "black"), 
        rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"], linetype = 0, 
                            colour = NA), text = element_text(colour = "black"), 
        title = element_text(family = "mono", size = rel(1)), 
        axis.title = element_text(size = 12, face = "bold"), 
        # axis.title.x = element_text(margin = margin(t=)),
        axis.text = element_text(face = "bold", 
                                 size = rel(1)), 
        axis.text.x = element_text(colour = NULL
                                   # margin = 
                                   # margin = margin(40,)
                                   # vjust = 10
                                   ), 
        axis.text.y = element_text(colour = NULL),
        axis.ticks = element_line(colour = NULL), axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = NULL),
        axis.line = element_line(), axis.line.y = element_blank(),
        legend.background = element_rect(), legend.position = "top", legend.direction = "horizontal",
        legend.box = "vertical", legend.title = element_blank(),
        panel.grid = element_line(colour = NULL, linetype = 3), panel.grid.major = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0, face = "bold"),
        plot.margin = unit(c(1, 1, 1, 1), "lines"), strip.background = element_rect()
        )

ggsave("udv_befolkning_her.png")


