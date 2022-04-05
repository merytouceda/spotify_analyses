# Playing around: 

# ---------------------------------------------------------------------------------------------------------------
# Palettes 
# ---------------------------------------------------------------------------------------------------------------

# https://github.com/jbgb13/peRReo
library(peRReo)
# https://github.com/jakelawlor/PNWColors/
library(PNWColors)
# https://github.com/BlakeRMills/MetBrewer
library(MetBrewer)





# ---------------------------------------------------------------------------------------------------------------
# Spotify: 
# --------------------------------------------------------------------------------------------------------------

# documentation
## https://www.rcharlie.com/spotifyr/
# https://developer.spotify.com/documentation/general/guides/authorization/app-settings/
# developer dashboard: https://developer.spotify.com/dashboard/applications/531568209321426ba50d24e911b23c80

# Set up
#install.packages('spotifyr')
library(spotifyr)
library(tidyverse)
library(knitr)
library(dplyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '531568209321426ba50d24e911b23c80')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd4307d3c67c74598bed9d7d80f4db35b')

access_token <- get_spotify_access_token()


# Finding beatle's most played cords
beatles <- get_artist_audio_features('the beatles')
beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()


# Get your most recently played tracks
library(lubridate)

get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()
yes


# all time favorite artists
get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 20) %>% 
  select(name, genres, popularity) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()




# favorite tracks at the moment
get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()


# What is next with spotifyR: 

# Hirearchical clustering of David Bowie: https://twitter.com/WireMonkey/status/1009915034246565891?s=20&t=epagwVqyLnafDEG5TaY4lw
# More tutorials and stuff here: https://www.rdocumentation.org/packages/spotifyr/versions/2.1.1



# retrieving my playlists

my_id <- 'ergumenijilda'
my_plists <- get_user_playlists(my_id)
barackobama_plists <- get_user_playlists('barackobama')

my_plists_madebyme <- my_plists %>%
  filter(owner.display_name == my_id) 

my_plists_madebyme_tracks <- get_playlist_tracks(my_plists_madebyme)

features <- get_track_audio_features(tracks)





kendrick <- get_artist_audio_features('kendrick lamar')




