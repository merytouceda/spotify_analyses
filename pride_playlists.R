# Spotify Pride playlists analysis

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
library(ggrepel)

Sys.setenv(SPOTIFY_CLIENT_ID = '531568209321426ba50d24e911b23c80')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd4307d3c67c74598bed9d7d80f4db35b')

access_token <- get_spotify_access_token()

#----------------------------------------------------------------------------------------
# 
#----------------------------------------------------------------------------------------


# URIS
# Out now: spotify:playlist:37i9dQZF1DX3LyU0mhfqgP
# Pride classics: spotify:playlist:37i9dQZF1DX59HcpGmPXYR
# Pride beyond borders: spotify:playlist:37i9dQZF1DX5gUQYmrehre
# Trascend: spotify:playlist:37i9dQZF1DWViyN2b86Qnu
# Club resistence: spotify:playlist:37i9dQZF1DWTLrNDPW5co2
# Latin Pride: spotify:playlist:37i9dQZF1DX2lenzaqw2vs
# Alternative pride: spotify:playlist:37i9dQZF1DWTMR78LDoAZC
# Pioneers: spotify:playlist:37i9dQZF1DWZREeDGwM6aE
# Black, Queer, & Proud: spotify:playlist:37i9dQZF1DWYdV3Fs5eWjC
# Queer As Folk: spotify:playlist:37i9dQZF1DX5TMFhaZc9ov
# Disco Fever: spotify:playlist:37i9dQZF1DX2GKumqRIZ7g
# Work it, Own it: spotify:playlist:37i9dQZF1DXbmCTvLQy5AO
# Queer composers: spotify:playlist:37i9dQZF1DX75gMjYMWCsk
# Fierce: spotify:playlist:37i9dQZF1DWZZCoBbLu26P
# Spotify Singles, Pride: spotify:playlist:37i9dQZF1DXcaEXdyjLdxm
# ROY G BIV: spotify:playlist:37i9dQZF1DX6PiP84oEbKp

# Keep copying them, make big database, make boxplots to explore the data. 

playlist_uris <- c('37i9dQZF1DX3LyU0mhfqgP','37i9dQZF1DX59HcpGmPXYR', '37i9dQZF1DX5gUQYmrehre', 
                   '37i9dQZF1DWViyN2b86Qnu', '37i9dQZF1DWTLrNDPW5co2', '37i9dQZF1DX2lenzaqw2vs', 
                   '37i9dQZF1DWTMR78LDoAZC', '37i9dQZF1DWZREeDGwM6aE', '37i9dQZF1DWYdV3Fs5eWjC', 
                   '37i9dQZF1DX5TMFhaZc9ov','37i9dQZF1DX2GKumqRIZ7g', '37i9dQZF1DXbmCTvLQy5AO', 
                   '37i9dQZF1DX75gMjYMWCsk','37i9dQZF1DWZZCoBbLu26P','37i9dQZF1DXcaEXdyjLdxm', 
                   '37i9dQZF1DX6PiP84oEbKp')
pride_playlists<- get_playlist_audio_features(username = 'spotify', playlist_uris = playlist_uris)

# extract only the unique values of song in playlist
pride_playlists<- pride_playlists %>%
  distinct(playlist_name, track.name, .keep_all = TRUE)



palette <- c('royalblue1', 'orange', 'steelblue1', 'gold', 'pink', 'purple', 
             'orangered2', 'orangered4', 'green1', 'violetred1', 'seagreen4', 'gray', 
             'snow1', 'darkorange3', 'plum3', 'darksalmon')

# Exploratory boxplots
ggplot(pride_playlists, aes(x = reorder(playlist_name, -danceability), y = danceability))+
  geom_boxplot(aes(fill=playlist_name), alpha = 0.4)+
  geom_jitter(aes(color=playlist_name), alpha=0.6)+
  scale_fill_manual(guide = "none", values = palette)+
  scale_color_manual(guide = "none", values = palette)+
  xlab("")+
  ylab("Danceability")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(size = 14),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(color = 'black'), 
        axis.line.y = element_line(color = 'black'))


# PCA
# Do an ordination of the playlists based on the properties
# properties: columns 6-9, 11-16
# remove column 10: mode
pride_playlists <- pride_playlists[,-pride_playlists$mode]
#properties: columns 6-15

pride_pca <- prcomp(pride_playlists[,c(6:15)], center = TRUE, scale = TRUE)
library(ggbiplot)
ggbiplot(pride_pca, groups = pride_playlists$playlist_name)+
  scale_color_manual(values = palette)

# La mayor diferencia entre las playlists esta entre el acousticness e instrumentalness the la lista de clasica y las demas. 
# Pero entre las demas no hay mucha diferencia. Se hacen todas una pelota, sobre todo las mas populares. 
# Eso quiere decir que las listas no son tan diferentes en mood. 
# Entonces en que son diferentes? Como encuentro la variable que las diferencia?

# PCA without Queer Composers playlist

palette2 <- c('royalblue1', 'orange', 'steelblue1', 'gold', 'pink', 'purple', 
             'orangered2', 'orangered4', 'green1', 'violetred1', 'seagreen4', 
             'snow1', 'darkorange3', 'plum3', 'darksalmon')
pride_playlists_noncomposers <- pride_playlists[!(pride_playlists$playlist_name == "Queer Composers"),]
pride_pca_noncomposers <- prcomp(pride_playlists_noncomposers[,c(6:15)], center = TRUE, scale = TRUE)
ggbiplot(pride_pca_noncomposers, groups = pride_playlists_noncomposers$playlist_name)+
  scale_color_manual(values = palette2)
## ----------------------------
# Popularity Analysius
## ----------------------------
# analyze what are the playlists with more popular songs
popular <- pride_playlists[pride_playlists$track.popularity != 0,] # remove songs with 0 propularity
# There is some problem with the song "Into You" by Ariana Grande, it appears as a popularity 3, impossible, the song has 
# ##### reproductions. 
# Also, when looking into the playlist from the spotify app,the song doesn't appear. 
# I am removing it. 
popular <- popular[!(popular$track.name =="Into You"), ]

ggplot(popular, aes(x = reorder(playlist_name, -track.popularity), y = track.popularity, color=playlist_name, label = track.name ))+
  geom_label_repel()+
  geom_boxplot(aes(fill=playlist_name), alpha = 0.4)+
  geom_jitter(aes(color=playlist_name), alpha=0.6)+
  scale_fill_manual(guide = "none",values = palette)+
  scale_color_manual(guide = "none",values = palette)+
  xlab("")+
  ylab("Track Popularity")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
               axis.title.y=element_text(size = 14),
               panel.background = element_rect(fill = 'white'),
               axis.line.x = element_line(color = 'black'), 
               axis.line.y = element_line(color = 'black'))
  
# Improvement: make labels only label the most popular songs
# obtained like this: 
sortedpopular <- popular[order(-popular$track.popularity),]
mostpopularsongs <- head(sortedpopular, n = 10)




# IDEA 2: analyze how many songs by rupaul in each playlist. 

i = 0
rupaul <- data.frame(matrix(ncol = 61, nrow = 0))
for (i in 1:nrow(pride_playlists)){
  if (pride_playlists$track.artists[[i]][[3]] == "RuPaul") {
    print(pride_playlists$track.artists[[i]][[3]])
    #rbind(rupaul, pride_playlists[pride_playlists$track.artists[[i]][[3]],])
    
    }
}

# not worth it, the number of rupauls is too low!!

# STOP THIS IDEA HERE

## ----------------------------
# Explicit
## ----------------------------
# see what proportion of explicit songs each playlist has

ggplot(popular, aes(x = reorder(playlist_name, -track.popularity), y = track.popularity))+
  #geom_label_repel()+
  geom_boxplot(alpha = 0.4)+
  geom_jitter(aes(color=track.explicit), alpha=0.6)+
  #scale_fill_manual(guide = "none",values = palette)+
  scale_color_manual(values = c("black", "red"))+
  #scale_color_manual(guide = "none",values = palette)+
  xlab("")+
  ylab("Track Popularity")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(size = 14),
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(color = 'black'), 
        axis.line.y = element_line(color = 'black'))

metadata<- pride_playlists %>%
  group_by(playlist_name) %>% 
  summarise(explicitproportion = sum(track.explicit)) 

metadata$numbersongs <- count(pride_playlists,playlist_name)$n


ggplot(metadata, aes(x = reorder(playlist_name, -explicitproportion/numbersongs), y = explicitproportion/numbersongs))+ 
  geom_col(aes(fill= playlist_name))+ 
  scale_fill_manual(guide = "none",values = palette)+
  xlab("")+
  ylab("Proportion of playlist that are explicit songs")+
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        axis.title.y=element_text(size = 14),  
        panel.background = element_rect(fill = 'white'),
        axis.line.x = element_line(color = 'black'), 
        axis.line.y = element_line(color = 'black'))


