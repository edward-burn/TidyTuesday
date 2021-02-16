library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(ggrepel)

freed_slaves <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv') 

freed_slaves<-freed_slaves %>% 
  mutate(seg=
      ifelse(Year==1790, 98,
      ifelse(Year==1800, 93,
      ifelse(Year==1810, 91,
      ifelse(Year==1820, 91.5,
      ifelse(Year==1830, 91.5,
      ifelse(Year==1840, 92,
      ifelse(Year==1850, 94,
      ifelse(Year==1860, 97,
      ifelse(Year==1870, 90,NA)))))))))) %>% 
  mutate(seg1=
      ifelse(Year %in% c(1820,1830), NA,  seg))

freed_slaves<-freed_slaves %>% 
  mutate(Free.perc=paste0(Free, "%"))

freed_slaves %>%
  ggplot() +
  geom_area_pattern(
    aes(Year, 100),
    fill          = "#006600",
    pattern       = 'plasma',
    pattern_alpha = 0.5,
    colour        = 'white'
  ) +
  geom_area_pattern(
    aes(Year, Slave),
    fill          = "black",
    pattern       = 'plasma',
    pattern_alpha = 0.25,
    colour        = 'black'
  ) +
  geom_text_repel(
    aes(Year, 100, label = Year),
    nudge_x      = 0,
    nudge_y      = 2.5,
    segment.size = 0,
    box.padding = 0,
    force = 0,
    segment.alpha = 0,
    size = 4.5,
    fontface = "bold"
  ) +
  geom_text_repel(
    aes(Year, seg, label = Free.perc),
    nudge_x      = 0,
    nudge_y      = -2.5,
    segment.size = 0,
    box.padding = 0,
    force = 0,
    segment.alpha = 0,
    size = 4.5,
    fontface = "bold"
  ) +
  geom_segment(mapping = aes(
    x = Year,
    xend = Year,
    y = seg1,
    yend = 100
  ),
  size = 0.5) +
  geom_line(aes(x, y), data = data.frame(x = 1790:1870, y = 100)) +
  annotate(
    'text',
    x = 1825,
    y = 95,
    label = "FREE - LIBRE",
    size = 7,
    colour = "black",
    fontface = "bold"
  ) +
  annotate(
    'text',
    x = 1825,
    y = 60,
    label = "SLAVES",
    size = 7,
    colour = "white",
    fontface = "bold"
  ) +
  annotate(
    'text',
    x = 1825,
    y = 55,
    label = "ESCLAVES",
    size = 7,
    colour = "white",
    fontface = "bold"
  ) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0.01, 0.05, 0.12, 0.05)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "#f5e5c9",
      colour = "#f5e5c9",
      linetype = NULL
    )
  ) 


#save
  #width=7
  #height=9


