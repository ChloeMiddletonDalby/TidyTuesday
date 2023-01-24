# Chloe
# Tidy tuesday - 2022-08-02
# Spotted frogs
# Source : https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-08-02


rm(list = ls())
pacman::p_load(tidyverse, expss, lubridate, gganimate, ggmap, gifski) 

frog <- read.csv("00-frogs.csv")


# Clean the data
frog <- frog %>% 
  
  mutate(
    SurveyDate = as.Date(SurveyDate, format = "%m/%d/%Y", origin = "1899-30-12"),
    frogID = as.character(round(ifelse(Frequency > 165, (Frequency-165)*1000, (Frequency-164)*1000), 0)),
    week = floor_date(SurveyDate, "week"),
    Female = as.character(Female)

  ) %>%

  apply_labels(Site = "location",
                Subsite = "location second level",
                HabType = "location third level",
                SurveyDate = "date",
                Ordinal = "Ordinal day from January 1, 2018 on which telemetry data were collected",
                Frequency = "Unique transmitter frequency associated with each individual frog",
                UTME_83 = "UTM coordinates",
                UTMN_83 = "UTM coordinates",
                Interval = "integer",
                Female = "gender (binary)",
                Water = "water",
                Type = "water type",
                Structure = "structure",
                Substrate = "substrate",
                Beaver = "beaver",
                Detection = "detection type", 
                frogID = "Frog ID") 


myLocation <- c(left = -121.83, bottom =  43.76, 
                right =  -121.76, top = 43.815)
myMap <- get_stamenmap(bbox=myLocation, crop=TRUE, 
                       color="bw", maptype = "watercolor")

frogPlot <- ggmap(myMap) +
  geom_point(aes(x=longitude, y=latitude, color=frogID, shape=Female), 
             data = frog,
             alpha = .5, size = 3, show.legend = FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())
        
        

frogPlot 


frogAnim <- frogPlot + transition_time(frog$week) +
  labs(title = "day: {frame_time}") + 
  shadow_wake(wake_length = 1)

animate(frogAnim, renderer = gifski_renderer(), height = 500, width = 500, fps = 30, duration = 10,
        end_pause = 60, res = 100)
anim_save("graph.gif")

(frog$SurveyDate) %>%  clipr::write_clip()
