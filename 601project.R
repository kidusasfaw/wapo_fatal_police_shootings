library(ggmap)
library(maps)
library(ggplot2)
library(plyr)
df.all <- read.table("~/Documents/Data/601/Project/gvdb-aggregated-db/Events.tsv", 
                     sep="\t", header=TRUE, fill=TRUE)

df.fatal <- read.csv("~/Documents/Data/601/Project/data-police-shootings-master/fatal-police-shootings-data.csv")

df.loc <- readRDS("~/Documents/Grad_school/Winter_2017/Stats 601/Project/df.loc.RDS")

US <- map_data("state")

ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=check, inherit.aes=F, aes(x=lon, y=lat), size=.5, colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 
  

check = subset(df.loc, df.loc$lon < -50 & df.loc$lat < 50)
count = count(df.fatal$city)
df.loc <- cbind(df.loc, count)