library(ggmap)
library(maps)
library(ggplot2)
library(plyr)
library(tidyr)
df.all <- read.table("~/Documents/Data/601/Project/gvdb-aggregated-db/Events.tsv", 
                     sep="\t", header=TRUE, fill=TRUE)

df.fatal <- read.csv("~/Documents/Data/601/Project/data-police-shootings-master/fatal-police-shootings-data.csv")

# get lonlat data and frequency
df.fatal <- unite(data=df.fatal, city.state, c(city, state), sep = " ", remove = FALSE) # create variable city.state for better accuracy
df.loc <- as.data.frame(table(df.fatal$city.state)) # get freq
names(df.loc)[1] <- 'city.state'
lonlat <- geocode(df.loc$city.state) # get latitude and longitude from google maps
df.loc <- na.omit(cbind(df.loc, lonlat)) # remove NA

saveRDS(df.loc, "~/Documents/Grad_school/Winter_2017/Stats 601/Project/df.loc.RDS")
df.loc <- readRDS("~/Documents/Grad_school/Winter_2017/Stats 601/Project/df.loc.RDS")

US <- map_data("state") # get US map data

ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=df.loc, inherit.aes=F, aes(x=lon, y=lat, size=freq), colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 
  


