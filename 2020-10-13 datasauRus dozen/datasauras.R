
# packages -----
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)


# Ansccombe -----
data(anscombe)
# to long
anscombe<- rbind(
anscombe[,c(1,5)] %>% 
  rename(x=x1,
         y=y1) %>% 
  mutate(dataset=1),
anscombe[,c(2,6)] %>% 
  rename(x=x2,
         y=y2) %>% 
  mutate(dataset=2),
anscombe[,c(3,7)] %>% 
  rename(x=x3,
         y=y3) %>% 
  mutate(dataset=3),
anscombe[,c(4,8)] %>% 
  rename(x=x4,
         y=y4) %>% 
  mutate(dataset=4))

#colours for the plot
anscombe.colours<- c("#1b9e77", "#d95f02","#7570b3","#e7298a")

# mean,sd, and cor
anscombe %>% 
  group_by(dataset) %>% 
  summarise(mean(x),
            sd(x),
            mean(y),
            sd(y),
            cor(x, y))


anscombe.name<-"X Mean : 9\n\nX SD : 3.32\n\n
.........
\n\nY Mean : 7.5\n\nY SD : 2.03\n\n.........
\n\nCorr. : 0.82
"

anscombe %>% 
  mutate(name={{anscombe.name}}) %>% 
  #ggplot
  ggplot()+
  facet_grid(name~ ., switch="y")+
  geom_point(aes(x, y, 
                 fill=as.character(dataset)),
             shape=21,
               size=5)+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"),
        plot.title=element_text(size=24),
        plot.subtitle=element_text(size=14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.position = "none") +
  scale_fill_manual(values=anscombe.colours)+
  scale_x_continuous(limits = c(2,20),
                     breaks = c(2,11,20))+
  scale_y_continuous(limits = c(2,14),
                     breaks = c(2,8,14), 
                     position = "right")+
  xlab("")+ylab("")+
  ggtitle("Anscombe´s quartet")+ 
  labs(subtitle = "Datasets share summary statistics, but are oh-so different") +
  annotate(geom="text", x=17.7, y=3, label="#TidyTuesday", size=4)+
  annotate(geom="text", x=17.5, y=2.35, label="@EdBurn1", size=4)+
  # gganimate
  transition_states(dataset,
                    transition_length = 2,
                    state_length = 5)+
  enter_fade() +
  exit_fly(x_loc = 2, y_loc = 14) +
  exit_recolour(fill = 'white')

anim_save("anscombe.gif")

# Datasauras ----
datasaurus <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-13/datasaurus.csv')

datasaurus.colours<- c("#984ea3", "#377eb8","#4daf4a","#e41a1c")

datasaurus %>% 
  group_by(dataset) %>% 
  summarise(mean(x),
            sd(x),
            mean(y),
            sd(y),
            cor(x, y))


datasaurus.name<-"X Mean : 54.3\n\nX SD : 16.8\n\n
.........
\n\nY Mean : 47.8\n\nY SD : 26.9\n\n.........
\n\nCorr. : -0.06
"

datasaurus %>% 
  filter(dataset %in% c("away", "bullseye", "star","dino"))%>% 
  mutate(ttime=
    ifelse(dataset=="away",1,
    ifelse(dataset=="bullseye",2,
    ifelse(dataset=="star",3,
    ifelse(dataset=="dino",4,NA )))))  %>% 
  mutate(name={{datasaurus.name}}) %>% 
  ggplot()+
  facet_grid(name~ ., switch="y")+
  geom_point(aes(x, y, 
                 fill=dataset),
             shape=21,
               size=3.5)+
  theme_bw()+
  theme(panel.spacing = unit(1, "lines"),
        plot.title=element_text(size=24),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.position = "none") +
  scale_fill_manual(values=datasaurus.colours)+
  scale_x_continuous(limits = c(0,100),
                     breaks = c(0,50,100),)+
  scale_y_continuous(limits = c(-5,100),
                     breaks = c(0,50,100), 
                     position = "right")+
  xlab("")+ylab("")+
  labs(subtitle = "Datasets share summary statistics, but are oh-so different") +
  ggtitle("Datasaurus")+ 
  annotate(geom="text", x=80, y=-2.65, label="#TidyTuesday @EdBurn1", size=4)+
  # gganimate
  transition_states(ttime,
                    transition_length = 2,
                    state_length = 5)+
  enter_fade() +
  exit_fly(x_loc = 100, y_loc = 100) +
  exit_recolour(fill = 'white')

anim_save("datasauRus.gif")




