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

US <- map_data("state") # get US map data, white map

# devtools::install_github("hadley/ggplot2@v2.2.0") need old version for google maps
df.loc$city.state <- as.character(df.loc$city.state)
hawaii <- df.loc[grepl("HI$",df.loc$city.state),]
alaska <- df.loc[grepl("AK$",df.loc$city.state),]


# plot using white US
ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=df.loc, inherit.aes=F, aes(x=lon, y=lat, size=Freq), colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 

# plot using google map 
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
#Hawaii
map <- get_map(location = "hawaii", zoom = 7)
ggmap(map, legend = "none") + 
  geom_point(aes(x = lon, y = lat, size=Freq), data = hawaii, alpha = .7, color = "darkblue") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

# get lonlat
all.lon.lat <- geocode(as.character(df.fatal$city.state), source = 'dsk')
df.location <- cbind(df.fatal, all.lon.lat)
saveRDS(df.location, "~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.location.RDS")

saveRDS(df.loc, "~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.loc.RDS") # save df.loc because it takes forever to pull data from google
df.loc <- readRDS("~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.loc.RDS")
df.loc <- cbind(df.loc, unique(df.fatal, df.fatal$state))

# Use this data for df.fatal
df.fatal <- readRDS("~/Documents/Grad_school/Winter_2017/Stats_601/Project/df.location.RDS")
df.fatal <- na.omit(df.fatal)


# create distance matrix for visualization, use Gower distance
# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
df.na <- df.fatal[rowSums(is.na(df.fatal)) > 0,] # see rows with missing values

drop <- c('name', 'date', 'city.state', 'city', 'state')
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

# looks like K = 2, but lets also check K=7
pam_fit2 <- pam(gower_dist, diss = TRUE, k = 2)
pam_fit6 <- pam(gower_dist, diss = TRUE, k = 6)

pam_results <- df.fatal.clean %>% dplyr::select(-id) %>%
  mutate(cluster = pam_fit2$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
# Looks to be clustered by threat level and race

# Look at metoids
df.fatal.clean[pam_fit2$medoids, ]
df.fatal.clean[pam_fit7$medoids, ]

# plot, dimension reduction using tSNE, t-distributed stochastic neighborhood embedding
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

# too many variables of interest, but still good to look at for us
tsne_data <-  data.frame(cluster = factor(pam_fit2$clustering), df.fatal.clean, tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=threat_level))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=manner_of_death))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=signs_of_mental_illness))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=body_camera))


tsne_data <-  data.frame(cluster = factor(pam_fit7$clustering), df.fatal.clean, tsne_data)
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=race))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=threat_level))

# MDS

fit <- cmdscale(gower_dist, k=2) # k is the number of dim
fit # view results

# plot solution 
fit <- as.data.frame(fit)
ggplot() + geom_point(data=fit, aes(fit[,1], fit[,2]))
fit <- cbind(fit, df.fatal)
ggplot() + geom_point(data=fit, aes(x=V1, y=V2, color=race))


