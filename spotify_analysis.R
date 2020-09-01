library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '1c9782ff428146068c6b53aeb10e21fc')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '89c11fe033fa47e59137fa8f555f12a0')

access_token <- get_spotify_access_token()
lana <- get_artist_audio_features('lana del rey')

library(tidyverse)
library(knitr)

lana %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

lana %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(15) %>% 
  kable()

