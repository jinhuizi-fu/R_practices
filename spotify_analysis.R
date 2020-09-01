#------------------------------------------------#
#-- Spotify analysis                           --#
#-- Last Update Date: 8/31/2020                --#
#------------------------------------------------#

library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)

Sys.setenv(SPOTIFY_CLIENT_ID = '1c9782ff428146068c6b53aeb10e21fc')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '89c11fe033fa47e59137fa8f555f12a0')
access_token <- get_spotify_access_token()

# get track features of artist Lana Del Rey
lana <- get_artist_audio_features('lana del rey')

# most used key
lana %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

# happiest song
lana %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(15) %>% 
  kable()


#---- Personal Streaming Data Exploration ------#

# last 5 songs played
get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

# my playlists
my_plists <- get_user_playlists('user_id')





# my all time favourite artists
get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

# my all time favourite tracks
get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

# all favourites
all_my_fav_tracks <-
  # This is somehow tough to read, but I lOVE PIPELINE!
  # FIRST we send get_my_saved_tracks request, set include_meta_info to TRUE, will return the number of all tracks in total. After that, we request 50 items per time, therefore, the looptime should be 50 divide the length of tracks.
  # Not all of us is lucky man, so if the number of tracks can't be divided exactly, we make one more time offset.
  ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  # Generate a sequence by looptimes.
  seq() %>%
  # PlZ remember, the offset is start from zero, so we minus 1 from the offset. And everytime, we jump 50 items, because we have already request 50 items every time.
  # Every loop, we get a data.frame with 50 rows, once the loop finished, we got a list with looptime piece data.frame, with reduce(rbind), we can merge all the data.frames as one.
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind) %>%
  # For saving time, we can save the data as rds, this is not required, but it can take things back, once we make some mistakes.
  write_rds('raw_all_my_fav_tracks.rds')
# Let's check the structure of our tracks.
glimpse(all_my_fav_tracks)


# first loved song!
all_my_fav_tracks %>%
  mutate(added_at = ymd_hms(added_at)) %>%
  arrange(added_at) %>%
  head(100, wt = added_at) %>%
  select(track.name,added_at)  %>%
  kable()
# my god, my first loved song is "put your records on"!!

artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  # I don't think we need Urls in further analyses, id (unique mark of artist) and name are selected here.
  select(id, name)

track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)

track_num_artist  %>%
  kable()

# For numerical variables, sometimes for simplifying problems, cut them into fractions is a good idea. Here, we go further, we fill the column plot with different color to represent different frequency group.
track_num_artist %>%
  mutate(
    freq = case_when(
      n > 100 ~ 'More than 100 tracks',
      between(n, 50, 99) ~ '50~99 tracks',
      between(n, 20, 49) ~ '20~49 tracks',
      TRUE ~ 'Less than 20 tracks'
    )
  ) %>%
  # To avoid mess up the order of frequency group, I always suggest to convert the category variables as factor variables, with built-in order, levels.
  mutate(freq = factor(
    freq,
    levels = c(
      'More than 100 tracks',
      '50~99 tracks',
      '20~49 tracks',
      'Less than 20 tracks'
    )
  )) %>%
  ggplot(mapping = aes(
    x = reorder(name, -n),
    y = n,
    fill = freq
  )) +
  geom_col() +
  labs(fill = NULL,title = 'Who is My Favorite Artist',caption = 'data from spotify via spotiyr') +
  xlab('Artist') +
  ylab('Tracks Number') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -60),
        axis.title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        plot.caption = element_text(hjust = 1,face = 'bold.italic'))



key_country <- tracks2%>%
  select(playlist_name, key)%>%
  group_by(playlist_name, key)%>%
  mutate(n=n())%>%
  unique()%>%
  group_by(key)%>%
  mutate(total=sum(n))%>%
  mutate(percent=round((n/total)*100))

head(key_country, 10)


