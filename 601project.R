library(ggplot2)
library(ggmap)
library(maps)
library(cluster)
library(Rtsne)
library(plyr)
library(dplyr)
library(tidyr)

df.fatal <- read.csv("~/Documents/Data/601/Project/data-police-shootings-master/fatal-police-shootings-data.csv")

# get lonlat data and frequency for map
df.fatal <- unite(data=df.fatal, city.state, c(city, state), sep = " ", remove = FALSE) # create variable city.state for better accuracy
df.loc <- as.data.frame(table(df.fatal$city.state)) # get freq
names(df.loc)[1] <- 'city.state'
lonlat <- geocode(as.character(df.loc$city.state), source = 'dsk') # get latitude and longitude
df.loc <- na.omit(cbind(df.loc, lonlat)) # remove NA
saveRDS(df.loc, "~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.loc.RDS") # save df.loc if use google maps
# df.loc <- readRDS("~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.loc.RDS") to load

# plot using white US
US <- map_data("state") # get US map data, white map
ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=df.loc, inherit.aes=F, aes(x=lon, y=lat, size=Freq), colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 

# plot using google map 
# devtools::install_github("hadley/ggplot2@v2.2.0") need old version of ggplot2 for google maps
df.loc$city.state <- as.character(df.loc$city.state)

# separate noncontiguous states
df.loc$city.state <- as.character(df.loc$city.state) # change city.state to character to use grep
hawaii <- df.loc[grepl("HI$",df.loc$city.state),]
alaska <- df.loc[grepl("AK$",df.loc$city.state),]

# US
map <- get_map(location=c(lon = -98.35, lat = 39.70), zoom = 4, source="google",maptype="roadmap",crop=FALSE)
ggmap(map, legend = "none") + 
  geom_point(aes(x = lon, y = lat, size=Freq), data = df.loc, alpha = .7, color = "darkblue") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
# Alaska
map <- get_map(location = "alaska", zoom = 4)
ggmap(map, legend = "none") + 
  geom_point(aes(x = lon, y = lat, size=Freq), data = alaska, alpha = .7, color = "darkblue") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())
# Hawaii
map <- get_map(location = "hawaii", zoom = 7)
ggmap(map, legend = "none") + 
  geom_point(aes(x = lon, y = lat, size=Freq), data = hawaii, alpha = .7, color = "darkblue") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

# Clusterting

# create df with lat and lon
lonlat <- geocode(as.character(df.fatal$city.state), source = 'dsk')
df.location <- cbind(df.fatal, latlon)
saveRDS(df.location, "~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.location.RDS")

# Use this data for df.fatal
df.fatal <- readRDS("~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.location.RDS")
df.na <- df.fatal[rowSums(is.na(df.fatal)) > 0,] # see rows with missing values
df.fatal <- na.omit(df.fatal) # few NA, so just won't use

# create new variable minorty, 0 = white, 1 = black, 2 = other
df.fatal$minority <- rep(0, nrow(df.fatal))
df.fatal$minority[df.fatal$race =='B'] <- 1
df.fatal$minority[df.fatal$race !='B' & df.fatal$race != 'W'] <- 2
df.fatal$minority <- factor(df.fatal$minority)

# create distance matrix for visualization, use Gower distance since mostly categorical data
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/ 
drop <- c('name', 'date', 'city.state', 'city', 'state', 'race') 
df.fatal.clean <- df.fatal[ , !(names(df.fatal) %in% drop)]

gower_dist <- daisy(df.fatal.clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Sanity check
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
df.fatal.clean[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
df.fatal.clean[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# looks like K = 2
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- df.fatal.clean %>% dplyr::select(-id) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
# Looks to be clustered by threat level and race

# Look at metoids
df.fatal.clean[pam_fit$medoids, ]

# plot, dimension reduction using tSNE, t-distributed stochastic neighborhood embedding
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

# too many variables of interest, but still good to look at for us
tsne_data <-  data.frame(cluster = factor(pam_fit$clustering), df.fatal.clean, tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=threat_level))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=manner_of_death))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=signs_of_mental_illness))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=body_camera))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=minority))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=threat_level))

# MDS

fit <- cmdscale(gower_dist, k=2) # k is the number of dim
fit # view results

# plot solution 
fit <- as.data.frame(fit)
ggplot() + geom_point(data=fit, aes(fit[,1], fit[,2]))
fit <- cbind(fit, df.fatal)
ggplot() + geom_point(data=fit, aes(x=V1, y=V2, color=minority))


