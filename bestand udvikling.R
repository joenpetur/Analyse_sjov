

library(statbank)
library(tidyverse)
library(lubridate)
library(highcharter)
library(TRMvisual)
library(scales)
library(ggthemes)



tbl_dst("BIL54")$BRUG

drivmiddel_data <- tbl_dst("BIL54", "da") |> 
  filter(BRUG=="1000", BILTYPE=="4000100001", `OMRÅDE`=="000", substr(TID, 1, 4)>2018) |> 
  select(-BILTYPE, -BRUG) |> 
  use_labels() |> 
  collect()

august_data <- drivmiddel_data |> 
  filter(TID>"2020M06") |> 
  group_by(DRIV) |> 
  mutate(MAX_INDHOLD = max(INDHOLD),
         DATE = as.Date(str_c(word(TID, sep = "M"),"-", word(TID, 2, sep = "M"), "-01")) %m+% months(1),
         MAX_DATE = ifelse(INDHOLD==MAX_INDHOLD, 1, NA),
         ) |> fill(MAX_DATE, .direction = "down") |> 
  mutate(DIFF = ifelse(MAX_DATE==1, INDHOLD - MAX_INDHOLD, 0),
         DRIV = ifelse(DRIV=="Drivmidler i alt", "Biler i alt", DRIV) |> 
           factor(levels = c("Diesel", "Benzin", "Biler i alt"))) |>
  filter(DRIV %in% c("Diesel", "Benzin", "Biler i alt")) |> 
  ungroup()



august_data |> 
  hchart("line", hcaes(x=DATE, y =DIFF, group = DRIV), marker = FALSE,  opposite=TRUE)

format_dates <- function(x) {
  months <- strftime(x, format = "%b")              # Abbreviated name of the month.
  years <- lubridate::year(x)  
  str_c(months, str_sub(years, 3,4))

}


breaks <- august_data$DATE %>% 
  unique() %>% 
  (\(y) seq.Date(min(y) %m+% months(1), max(y)+1, by = "6 month"))()

august_data |> 
  ggplot() +
  geom_line(aes(x=DATE, y =DIFF, color = DRIV), size=1) +
  scale_x_date(name = "",
               breaks = breaks,          # Date labels for each month.
               minor_breaks = NULL,              # No additional labels in-between `date_breaks`.
               expand = expansion(add = 0),  # Add 15 days to the x-axis on the left and on the right.
               labels = format_dates, position = "top") + 
  scale_y_continuous(name = "Antal i tusind", labels = label_number(scale = 1/1000), limits = c(-100000, 0),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  theme_foundation(base_size = 12, base_family = "sans") + 
  theme(line = element_line(linetype = 1, colour = "black"), 
        rect = element_rect(fill = ggthemes::ggthemes_data$wsj$bg["brown"], linetype = 0, 
                            colour = NA), text = element_text(colour = "black"), 
        title = element_text(family = "mono", size = rel(2)), 
        axis.title = element_text(size = 12, face = "bold"), 
        # axis.title.x = element_text(margin = margin(t=)),
        axis.text = element_text(face = "bold", 
                                 size = rel(1)), 
        axis.text.x = element_text(colour = NULL), 
        axis.text.y = element_text(colour = NULL), 
        axis.ticks = element_line(colour = NULL), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_line(colour = NULL), 
        axis.line = element_line(), axis.line.y = element_blank(), 
        legend.background = element_rect(), legend.position = "top", legend.direction = "horizontal",
        legend.box = "vertical", legend.title = element_blank(),
        panel.grid = element_line(colour = NULL, linetype = 3), panel.grid.major = element_line(colour = "black"), 
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust = 0, face = "bold"), 
        plot.margin = unit(c(1, 1, 1, 1), "lines"), strip.background = element_rect())

ggsave("udv_bestand")
  

