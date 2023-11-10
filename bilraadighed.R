


library(statbank)
library(tidyverse)
library(lubridate)
library(highcharter)
library(TRMvisual)
library(scales)
library(ggthemes)
library(zoo)

tbl_dst("BIL800", "da")$OMRÅDE 


pop_data <- tbl_dst("BIL800", "da") |> 
  filter(RAADMOENS %in% c("10000", "10200", "10210"), as.numeric(`OMRÅDE`)<15, `OMRÅDE`!="000", TID>2013) |> 
  use_labels() |> 
  collect() |> 
  rename(OMRAADE=`OMRÅDE`)

pop_data_1 <- pop_data |> 
  # mutate(RAADMOENS = str_to_upper(RAADMOENS) |> str_replace_all(c(" "="_", "FAMILIER_"=""))) |> 
  # pivot_wider(names_from = "RAADMOENS", values_from = "INDHOLD") |> 
  mutate(across(-c("TID"), ~.x-lag(.x), .names = "UDV_{.col}" ),
         across(everything(), ~str_remove_all(.x, "FAMILIER_") ),
         .by = c("OMRAADE", "RAADMOENS")) 

data_til_fig <- pop_data_1 |> 
  filter(TID %in% c(2023), RAADMOENS!="Familier i alt")  |> 
  mutate(RAADMOENS = str_remove_all(RAADMOENS, "Familier | i alt") |>
           str_to_sentence(),
         UDV_INDHOLD = as.numeric(UDV_INDHOLD),
         OMRAADE = str_remove(OMRAADE, "Landsdel "))
  

data_til_fig_1 <- data_til_fig |> 
  # filter(OMRAADE=="Landsdel Byen København") |> 
  hchart('column', hcaes(x=OMRAADE, y =UDV_INDHOLD, group = RAADMOENS))


fig_familie_bilraadighed <- data_til_fig |> 
  filter(TID==2023) |> 
  ggplot() +
  geom_bar(aes(x=OMRAADE, y =UDV_INDHOLD, fill = RAADMOENS), stat = "identity", position = "dodge",
           width = 0.7) +
  # facet_wrap(~TID) +
  scale_x_discrete(name = "",
                     guide = guide_axis(angle = 45)) + 
  scale_y_continuous(name = "Antal", limits = c(-3000, 9000),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_foundation(base_size = 12, base_family = "sans") + 
  theme(line = element_line(linetype = 1, colour = "black"), 
        rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"], linetype = 0, 
                            colour = NA), text = element_text(colour = "black"), 
        title = element_text(family = "mono", size = rel(2)), 
        axis.title = element_text(size = 12, face = "bold"), 
        # axis.title.x = element_text(margin = margin(t=)),
        axis.text = element_text(face = "bold", 
                                 size = rel(1)), 
        axis.text.x = element_text(colour = NULL
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

ggsave("udv_bilraadighed_familier.png")


