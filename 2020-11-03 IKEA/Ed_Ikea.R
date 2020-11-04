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
  geom_jitter(shape=21, fill="blue",
              colour="black",
              size=3.5, alpha=0.95, stroke=2.5)+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.text=element_text(size=14,face="bold", colour="black"),
        strip.text = element_text(size=16, face="bold"),
        strip.background = element_rect(fill="#FCE205",size = 2, colour="black"),
        strip.text.y.left = element_text(angle = 0,hjust = 0.1),
        plot.title = element_text(size=28,face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size=12,face="bold"),
        plot.caption.position = "plot",
          panel.background = element_rect(fill = "#FCE205",
                                          colour = "yellow",
                                          size = 2, linetype = "solid"),
          panel.border = element_rect(color = "black", fill = NA, size = 2),
          panel.grid.major.x = element_line(size = 5, linetype = 'solid',
                                          colour = "white"),
        panel.grid.major.y = element_line(size = 3, linetype = 'solid',
                                          colour = "white"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#FFC30B") )+
  scale_x_continuous(labels = function(x) paste0("£",comma(x)), expand = c(0.01, 0.01))+
  ggtitle("A brief tour of IKEA prices")+
  labs(caption = "Each point is one product.\nPrices based on current exhange rate\nfrom Saudi Riyals to Pound Sterling.")

ggsave("Ed_Ikea_Prices.png", dpi=300,
       width=16, height=9)
