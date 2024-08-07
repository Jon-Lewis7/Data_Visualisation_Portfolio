#tidying data assessment
install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(dplyr)
# -------------------------------------------------------------------------

#importing data
spotify_db <- read.csv("Spotify_Messy_210935475.txt", header = TRUE, sep= "\t")                                       #specifies first row as headers and separates headers based on the tab space between them
View(spotify_db)

# -------------------------------------------------------------------------

#each variable must have a column
spotify_db <- spotify_db %>% 
 separate_wider_delim("danceability.energy","_", names=c("danceability","energy"))                                    #danceability and energy separated into their own columns

# -------------------------------------------------------------------------

#each observation must have its own cell (tidily)
spotify_db <- spotify_db %>% 
  pivot_longer((cols = 22:25), names_to =c("genre"),values_to = "subgenre", values_drop_na = TRUE)                    #generation of genre and subgenre from columns containing only variables

# -------------------------------------------------------------------------

#fixing letters added to a number column
spotify_db <- spotify_db %>% mutate("mode"=as.numeric(gsub("[^0-9]","",mode)))                                        #mutation of mode so that it only includes numeric values

clean_num <- function(x) {as.numeric(gsub("[^0-9]","",x))}                                                            #the same use as above but in function format.
clean_num(spotify_db$mode)                                                                                            #function to clean my data, specifying the variable mode

# -------------------------------------------------------------------------

#fixing column names
spotify_db <- spotify_db %>% rename_at("ZZtrack_name89", ~ "track_name")                                              #renaming a specific column

# -------------------------------------------------------------------------

#fixing observation errors
spotify_db <- spotify_db %>% mutate("track_album_release_date" = gsub("0.5-","2005-",track_album_release_date)) %>%   #substituting wrong values
  mutate("track_album_release_date" = gsub("22005","2005",track_album_release_date))                                                            
# -------------------------------------------------------------------------

#searching for and fixing errors in artist names
spotify_db <- spotify_db %>% mutate(track_artist=as.factor(track_artist))                                             #factorising track artist

levels(spotify_db$track_artist)                                                                                       #allows easy viewing to search for artists that could've been misspelt

spotify_db <- spotify_db %>%                                  
  mutate(track_artist = gsub("Shakra",
                        "Shakira",
                        track_artist)) %>%
  mutate(track_artist = as.factor(track_artist))                                                                      #changing of "shakra" to "shakira"

spotify_db <- spotify_db %>% 
  mutate(track_artist = gsub("The Fourty Owls",
                        "The Four Owls",
                        track_artist)) %>%
  mutate(track_artist = as.factor(track_artist))                                                                      #changing of "the fourty owls" to "the four owls"

# -------------------------------------------------------------------------

#factorisation for future analysis
spotify_db <- spotify_db %>% mutate(playlist_name=as.factor(playlist_name))                                           #factorising playlists

View(spotify_db)

# -------------------------------------------------------------------------

#extracting tidy table as a text file
write.table(spotify_db, "Spotify_Messy_210935475_Worked.txt",                                                         #function to extract data as a table
            sep="\t",
            col.names = T,
            row.names = F,
            quote = F)