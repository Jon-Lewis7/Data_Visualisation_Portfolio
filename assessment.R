#JONATHAN LEWIS 210935475 CODE
#required packages
install.packages("scales")
install.packages("tibble")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("remotes")
install.packages("viridis")
install.packages("patchwork")
remotes::install_github("ricardo-bion/ggradar")
source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r")
library(scales)
library(remotes)
library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggradar)
library(viridis)
library(patchwork)
##########################################################################################################################################################
#downloading of data (https://data.world/vizzup/the-greatest-hip-hop-songs-of-all-time-by-bbc-poll)(https://data.world/sarahlovesdata/profanity-in-hip-hop)
spotify_songs <- tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'))
polls <- read.csv("https://query.data.world/s/hs6ncbe3p2ifla557hke6b7yheypbv?dws=00000", header=TRUE, stringsAsFactors=FALSE);
rankings <- read.csv("https://query.data.world/s/yfsts5vuraphykqtlm7gyamu5xjkux?dws=00000", header=TRUE, stringsAsFactors=FALSE);
GET("https://query.data.world/s/lhksmckn25g7t3tobgtks7payluret?dws=00000", write_disk(tf <- tempfile(fileext = ".xlsx")))
spotifyrap <- spotify_songs
###########################################################################################################################################################
#viewing spotify before working
View(spotify_songs)
###########################################################################################################################################################
#creating genre then subgenre subsets
#isolating rap from spotify
spotify_rap <- spotify_songs %>%                #making a dataframe with only rap        
  filter(playlist_genre == "rap")
View(spotify_rap)

#isolating hiphop from spotify
spotify_hiphop <- spotify_songs %>%                #making a df with only hiphop
  filter(playlist_subgenre == "hip hop")        #only filtering for the subgenre
View(spotify_hiphop)

#isolating southern hip hop from spotify
spotify_southernhiphop <- spotify_songs %>%
  filter(playlist_subgenre == "southern hip hop")

#isolating gangster rap from spotify
spotify_gangsterrap <- spotify_songs %>%
  filter(playlist_subgenre == "gangster rap")

#isolating trap from spoify
spotify_trap <- spotify_songs %>%
  filter(playlist_subgenre == "trap")

###########################################################################################################################################################
#factorisation
spotifysubgenre <- as.factor(spotify_songs$playlist_subgenre) 
spotifygenre <- as.factor(spotify_songs$playlist_genre)
###########################################################################################################################################################
#finding the means for each rap subgenre musical theme
##key,liveness excluded

mean(spotify_gangsterrap$danceability) #finding the means of each variable within subgenre 
################################################################################
#Radialplot for all subgenres of hiphop
#assigning objects with our data in the correct formating

var.names <- c("danceability", "energy", "loudness", "mode",       #assigning musical themes 
               "speechiness", "acousticness", "instrumentalness", 
               "valence")
var.order = seq(1:8)
values.a <- c(0.7196278,0.5659399,0.8626,0.4924357,0.1891239,0.308059,0.2188507,0.5092567) #assigning musical theme values

values.b <- c(0.7146806,0.6809224,0.8849,0.5808955,0.2015109,0.1174091,0.02248069,0.5541826) #average measurements for each theme

values.c <- c(0.7240254,0.6885494,0.8906,0.5240055,0.247118, 0.1529019,0.01505105,0.504932)

values.d <- c(0.7154051,0.6555755,0.8912,0.4709527,0.1448631,0.2162179,0.06797813,0.437307)

values.e <- rep(0, 8)                                                                      
group.names <- c("Hip Hop", "Southern Hip Hop", "Gangster Rap", "Trap") #assigning subgenres

# creating ggplot2 plotting frame

df1.a <- data.frame(matrix(c(rep(group.names[1], 8), var.names), nrow = 8, ncol = 2), #specifying all above factors and matrix layout
                    var.order = var.order, value = values.a)
df1.b <- data.frame(matrix(c(rep(group.names[2], 8), var.names), nrow = 8, ncol = 2), #df for each subgenre before combining
                    var.order = var.order, value = values.b)
df1.c <- data.frame(matrix(c(rep(group.names[3], 8), var.names), nrow = 8, ncol = 2), 
                    var.order = var.order, value = values.c)
df1.d <- data.frame(matrix(c(rep(group.names[4], 8), var.names), nrow = 8, ncol = 2), 
                    var.order = var.order, value = values.d)
df1 <- rbind(df1.a, df1.b, df1.c, df1.d)                                              #combining dataframes
colnames(df1) <- c("group", "variable.name", "variable.order", "variable.value")      #assigning column names to an object
df1

#creating radialplot plotting frame

m2 <- matrix(c(values.a, values.b,values.c,values.d), nrow = 4, ncol = 8, byrow = TRUE) #assigning the matrix values
group.names <- c(group.names[1:4])                                                      #specifying group name order
df2 <- data.frame(group = group.names, m2)                                              #new matrix with colnames and values
colnames(df2)[2:9] <- var.names                                                         #selecting the columns for our plot
print(df2) 

#radial plot using the function CreateRadialPlot
source("http://pcwww.liv.ac.uk/~william/Geodemographic%20Classifiability/func%20CreateRadialPlot.r") #<- please run
meansradial <- CreateRadialPlot(df2,  plot.extent.x = 1.65, plot.extent.y = 1.5,            #edditing plot borders
                                grid.min = 0.01, grid.max = 1, grid.mid = 0.5,            #specifying radarplot borders and scale
                                centre.y = 0, label.centre.y = FALSE,                     #specifying value at the centre off
                                label.gridline.min = FALSE, gridline.mid.colour = "grey", #disabling gridline min + adding colour mid
                                gridline.max.colour = "black", gridline.max.linetype = 1, #gridline max specifics
                                axis.label.size = 2,                                      #editing axis labels
                                group.line.width = 0.6, group.point.size = 3,             #editing width of geoms
                                background.circle.colour = "White") +                     #changing background colour
  labs(title = "Musical Themes Across Rap") +                                             #assigning plot title
  scale_colour_viridis_d() +                                                              #adding colourblind colour scheme
  labs(colour ='Rap Subgenres') +                                                         #changing legend title
  theme(plot.title = element_text(family="Times",face="bold"),                            #plot title specifcs
        legend.position = "left")                                                         #legend positioning
meansradial                       
################################################################################
#single radarplot (as above)
values_single <- c(0.7196278,0.5659399,0.8628,0.4924357,0.1891239,0.308059,0.2188507,0.5092567)
dfnew <- rbind(values_single)
colnamesnew(dfnew) <- c("group", "variable.name", "variable.order", "variable.value")

group.names <- c("Hip Hop")   #only utilising hiphop

m3 <- matrix(c(values_single), nrow = 1, ncol = 8, byrow = TRUE) #different matrix specifics
group.names <- c(group.names[1:1])
df2new <- data.frame(group = group.names, m3)
colnames(df2new)[2:9] <- var.names
print(df2new) 
?theme
hiphopradial <- CreateRadialPlot(df2new,
  plot.extent.x = 3, plot.extent.y = 1.5,                                             #different axis values for patchwork
  grid.min = 0.01, grid.max = 1, grid.mid = 0.5,
  centre.y = 0, label.centre.y = FALSE,
  label.gridline.min = FALSE, gridline.mid.colour = "grey",
  gridline.max.colour = "black", gridline.max.linetype = 1,
  axis.label.size = 2,
  group.line.width = 0.6, group.point.size = 3,
  background.circle.colour = "White") +
  labs(title = "Musical Themes in Hip Hop") +
  theme(plot.title = element_text(family="Times",face="bold")) +
  scale_color_manual(values = "#009698")                          #manually added viridis colour matching rap colourscheme
hiphopradial
################################################################################
#formatting for line graph Maryama's code
# Convert track_album_release_date to date format
spotifyrap$track_album_release_date <- as.Date(spotifyrap$track_album_release_date, format = "%Y-%m-%d")

# Extract year from the date
spotifyrap$year <- format(spotifyrap$track_album_release_date, "%Y")

spotifyrap<-spotifyrap[complete.cases(spotifyrap[,24]), ]             #excludes NAs from years
################################################################################
#scatter graph examining the relationship between musical themes over time
Pop_Loud <- ggplot(data = spotifyrap,
  mapping = aes(x = year, y = loudness, colour = track_popularity)) +
  geom_point() +                                                      #plotting as points
  labs(colour ='Song Popularity',                                     #legend title
  title = 'How Loudness is Maintained in Rap Across Years') +         
  xlab("Year of Release") +                                           
  ylab("Loudness (dB)") +
  scale_color_viridis() +                                             #applies colourblind scheme
  theme_bw() +                                                        #black and white bg settings
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            #angle and position of x legend
        plot.title = element_text(family="Times",face="bold", hjust = 0.5)) #title format
plot(Pop_Loud)
################################################################################
#patchworking
titletheme <- theme(plot.title = element_text(family="Times",face="bold",size=16))

RapThemes.png<-( meansradial + hiphopradial) / Pop_Loud +                            #two radar on top and scatter below
plot_annotation(tag_levels = "a",  title = "                     Exploring the Key Factors Driving the Popularity of Rap and Hip-Hop 
                                                      The Top Factor's Prevalence Over Time", theme = titletheme)
RapThemes.png

#saving
ggsave("RapThemesPlotNew8.png")





