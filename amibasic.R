# Am I basic? 

# ---------------------------------------------------------------------------------------------------------------
# Set up: 
# --------------------------------------------------------------------------------------------------------------

# documentation
## https://www.rcharlie.com/spotifyr/
# https://developer.spotify.com/documentation/general/guides/authorization/app-settings/
# developer dashboard: https://developer.spotify.com/dashboard/applications/531568209321426ba50d24e911b23c80

# Set up
#install.packages('spotifyr')
library(spotifyr)
#library(tidyverse)
#library(knitr)
library(ggplot2)
library(ggrepel)
library(peRReo)


# Palettes: 
pal1=latin_palette("badbunny1",50,type="continuous")
pal2=latin_palette("rosalia",20,type="continuous")
pal3=latin_palette("beckyg",type="continuous")

# Log in spotify

Sys.setenv(SPOTIFY_CLIENT_ID = '531568209321426ba50d24e911b23c80')
# aradillo: 7b39df5b18064e31b5943ed996bc5e00
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd4307d3c67c74598bed9d7d80f4db35b')
# aradillo: d5298cf1e3af42879bdf10696852a37b
access_token <- get_spotify_access_token()


# ---------------------------------------------------------------------------------------------------------------
# Artist
# --------------------------------------------------------------------------------------------------------------

# Retrieve data: 

# Top 20
my_top_20<-as.data.frame(get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 20))
hist(my_top_20$popularity)

# Top 50: 
my_top_50 <- as.data.frame(get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 50))
hist(my_top_50$popularity)

# Top 50 recent: 
my_top_50_recent <- as.data.frame(get_my_top_artists_or_tracks(type = 'artists', time_range = 'short_term', limit = 50))
hist(my_top_50_recent$popularity)



# plot boring but beautiful histogram
ggplot(my_top_20, aes(x = popularity))+
  geom_histogram(binwidth = 1)

# 20: plot scatter to see correlation 
my_top_20$rank = as.numeric(row.names(my_top_20))

ggplot(my_top_20, aes(y = rank, x = popularity, size = followers.total))+
  geom_point()+ 
  scale_y_reverse()



# 50: plot scatter to see correlation 
my_top_50$rank = as.numeric(row.names(my_top_50))

ggplot(my_top_50, aes(y = rank, x = popularity, label = name, color = rank))+
  geom_point(aes(size = followers.total),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.5)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



  # 50 recent: plot scatter to see correlation 
my_top_50_recent$rank = as.numeric(row.names(my_top_50_recent))

ggplot(my_top_50_recent, aes(y = rank, x = popularity, size = followers.total, label = name, color = rank))+
  geom_point()+ 
  #geom_text(aes(label=name), size=3)+
  geom_label_repel()+
  scale_y_reverse()+ 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.5)+
  scale_color_gradientn(colors=rev(pal2))+
  theme(legend.position = "none")+ 
  theme_classic()




# ---------------------------------------------------------------------------------------------------------------
# Tracks
# ---------------------------------------------------------------------------------------------------------------

# Top 50: 
my_top_50_tracks <- as.data.frame(get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 50))
hist(my_top_50_tracks$popularity)

my_top_50_tracks$rank = as.numeric(row.names(my_top_50_tracks))

ggplot(my_top_50_tracks, aes(y = rank, x = popularity, label = name, color = 1-rank))+
  geom_point(aes(),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the song is")+
  ylab("How high is this song on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.5)+
  scale_color_gradientn(colors=rev(pal3))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# Explicit?
ggplot(my_top_50_tracks, aes(y = rank, x = popularity, label = name, color = explicit))+
  geom_point(aes(),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.5)+
  scale_color_manual(values=c("black", "red"))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# get the features of the tracks to see what's up in my brain
# ERROR Handle overflow:top_50_tracks_features <- get_track_audio_features(my_top_50_tracks)

my_top_20_tracks <- as.data.frame(get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 20))
# ERROR Handle overflow: 
top_20_tracks_features <- get_track_audio_features(my_top_20_tracks)

my_top_10_tracks <- as.data.frame(get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 10))
# Error 502: top_10_tracks_features <- get_track_audio_features(my_top_10_tracks)

my_top_5_tracks <- as.data.frame(get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 5))
# Error 502: top_5_tracks_features <- get_track_audio_features(my_top_5_tracks)

