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
  ggplot(aes( x=1, y=price))+
  facet_wrap(category~ ., ncol = 2)+
  geom_jitter(shape=21, fill="grey", size=3.5, alpha=0.75, stroke=1.2)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect(fill="#f7f7f7"))+
  scale_y_continuous(labels = function(x) paste0("£",comma(x)))+
  ggtitle("A brief tour of IKEA prices")

ggsave("EdBIkeaPrices.png", dpi=300,
       width=7, height=11)
