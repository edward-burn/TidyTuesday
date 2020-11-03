# packages ----
library(dplyr)
library(ggplot2)
library(scales)

# data ----
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

#prices to gp
#current exchange rate is 0.21
ikea$price<-ikea$price*0.21

# plot -----
ikea %>%
  filter(category %in% c("Beds", "Bookcases & shelving units", "Chairs",
                      "Chests of drawers & drawer units",
                      "Outdoor furniture","Sofas & armchairs",
                     "Tables & desks", "Wardrobes" ) ) %>%
  mutate(category=ifelse(category=="Bookcases & shelving units",
                         "Bookcases", category)) %>%
  mutate(category=ifelse(category=="Chests of drawers & drawer units",
                         "Chests of drawers", category)) %>%
  ggplot(aes(x=price, y=1 ))+
  facet_grid(category~ ., switch="y")+
  geom_jitter(shape=21, fill="grey", size=3.5, alpha=0.75, stroke=1.2)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect(fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0),
        plot.title = element_text(size=22,face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size=12,face="bold"),
        plot.caption.position = "plot")+
  scale_x_continuous(labels = function(x) paste0("£",comma(x)))+
  ggtitle("A brief tour of IKEA prices")+
  labs(caption = "Each point is one product.")

ggsave("Ed_Ikea_Prices.png", dpi=300,
       width=11, height=7)
