# Analyzing Artists development




#----------------------------------------------------------------------------------------
# Set up
#----------------------------------------------------------------------------------------
#install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)
library(dplyr)
#devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
library(scales)
library(ggplot2)
library(ggiraphExtra)
library(ggRadar)

Sys.setenv(SPOTIFY_CLIENT_ID = '531568209321426ba50d24e911b23c80')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd4307d3c67c74598bed9d7d80f4db35b')

access_token <- get_spotify_access_token()






#----------------------------------------------------------------------------------------
# Kendrick Lamar
#----------------------------------------------------------------------------------------

kendrick <- get_artist_audio_features('kendrick lamar')

# get data ready for radar chart
# make a vector of the features we want to plot
radar_features <- c("album_name", "danceability", "energy", "loudness",
                    "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")

kendrick_radar <- kendrick[,radar_features]

kendrick_radar_agg <- aggregate(kendrick_radar, by=list(kendrick_radar$album_name), FUN = 'mean')
kendrick_radar_agg <- select(kendrick_radar_agg, -2)
kendrick_radar_agg <- kendrick_radar_agg[-c(3, 5), ]
kendrick_radar_agg$Group.1[1]="Black Panther"

kendrick_radar_agg %>%
  mutate(across(c(2:9), as.numeric)) %>%
  mutate_each(funs(rescale), -Group.1) %>%
  ggradar()+
  scale_color_manual(values=c("grey3", "red4", "grey32", "gray1",
                              "goldenrod4", "mistyrose4", "darkolivegreen"))+
  theme(legend.position = "right")


# need to improve this! make the polygons filled, I can't do this with ggradar
# but I can try with ggplot geom plygon and radar shape


