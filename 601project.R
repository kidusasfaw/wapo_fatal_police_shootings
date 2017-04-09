library(ggplot2)
library(ggmap)
library(maps)
library(plyr)
library(tidyr)
library(rjson)
library(jsonlite)
library(readr)
df.all <- read.table("~/Documents/Data/601/Project/gvdb-aggregated-db/Events.tsv",
                     sep="\t", header=TRUE, fill=TRUE)

mystring <- read_file("~/Documents/Data/601/Project/gvdb-aggregated-db/Events.tsv")
# json
check <- fromJSON(mystring)

df.fatal <- read.csv("~/Documents/Data/601/Project/data-police-shootings-master/fatal-police-shootings-data.csv")

# get lonlat data and frequency
df.fatal <- unite(data=df.fatal, city.state, c(city, state), sep = " ", remove = FALSE) # create variable city.state for better accuracy
df.loc <- as.data.frame(table(df.fatal$city.state)) # get freq
names(df.loc)[1] <- 'city.state'
lonlat <- geocode(as.character(df.loc$city.state), source = 'dsk') # get latitude and longitude
df.loc <- na.omit(cbind(df.loc, lonlat)) # remove NA

saveRDS(df.loc, "~/Documents/Grad_school/Winter_2017/Stats 601/Project/df.loc.RDS") # save df.loc because it takes forever to pull data from google
df.loc <- readRDS("~/Documents/Grad_school/Winter_2017/Stats 601/Project/df.loc.RDS")

US <- map_data("state") # get US map data

# plot
ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=df.loc, inherit.aes=F, aes(x=lon, y=lat, size=Freq), colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 
  
map <- get_map(location=c(lon = -98.35, lat = 39.50), zoom = 4, source="google",maptype="roadmap",crop=FALSE)
ggmap(map, legend = "none") + 
  geom_point(aes(x = lon, y = lat), data = df.loc, alpha = .7, color = "darkgreen", size = (testdata$Total.Conversions)/5000)



# create distance matrix for visualization, use Gower distance
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
df.na <- df.fatal[rowSums(is.na(df.fatal)) > 0,] # see rows with missing values

drop <- c('name', 'date', 'city.state', 'city', 'state')
df.fatal.clean <- df.fatal[ , !(names(df.fatal) %in% drop)]








