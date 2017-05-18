library(ggplot2)
library(ggmap)
library(maps)
library(cluster)
library(Rtsne)
library(plyr)
library(dplyr)
library(tidyr)

### Kidus

#### Laura

df.fatal <- read.csv("~/filepath/fatal-police-shootings-data.csv")

############### Plot Maps

# Get lonlat data and frequency for map
df.fatal <- unite(data=df.fatal, city.state, c(city, state), sep = " ", remove = FALSE) # create variable city.state for better accuracy
df.loc <- as.data.frame(table(df.fatal$city.state)) # get freq
names(df.loc)[1] <- 'city.state'
lonlat <- geocode(as.character(df.loc$city.state), source = 'dsk') # get latitude and longitude
df.loc <- na.omit(cbind(df.loc, lonlat)) # remove NA
saveRDS(df.loc, "~/filepath/df.loc.RDS") # save df.loc if use google maps since it takes so long
# df.loc <- readRDS("~/filepath/df.loc.RDS") to load

# Plot using white US map without Alaska or Hawaii
US <- map_data("state") # get US map data, white map
ggplot(data=US, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black") +
  xlim(-160, 60) + ylim(25,75) +
  geom_point(data=df.loc, inherit.aes=F, aes(x=lon, y=lat, size=Freq), colour="blue",  alpha=.8) +
  coord_cartesian(xlim = c(-130, -50), ylim=c(20,55)) 

# Plot using google map 
# devtools::install_github("hadley/ggplot2@v2.2.0") need old version of ggplot2 to use google maps
df.loc$city.state <- as.character(df.loc$city.state)

# Separate noncontiguous states
df.loc$city.state <- as.character(df.loc$city.state) # change city.state to character to use grep
hawaii <- df.loc[grepl("HI$",df.loc$city.state),]
alaska <- df.loc[grepl("AK$",df.loc$city.state),]

# US contiguous
map <- get_map(location=c(lon = -96.35, lat = 39.70), zoom = 4, source="google",crop=TRUE)
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

############### Clusterting

# Create df with lat and lon
lonlat <- geocode(as.character(df.fatal$city.state), source = 'dsk')
df.location <- cbind(df.fatal, latlon)
saveRDS(df.location, "~/filepath/df.location.RDS") 

# Use this data for df.fatal
df.fatal <- readRDS("~/filepath/df.location.RDS")
df.na <- df.fatal[rowSums(is.na(df.fatal)) > 0,] # see rows with missing values
df.fatal <- na.omit(df.fatal) # few NA, so just won't use them

# Create new variable minority
df.fatal$minority <- 'white'
df.fatal$minority[df.fatal$race =='B'] <- 'black'
df.fatal$minority[df.fatal$race =='H'] <- 'hispanic'
df.fatal$minority[df.fatal$race !='B' & df.fatal$race != 'W' & df.fatal$race != 'H'] <- 'other'
df.fatal$minority <- factor(df.fatal$minority)

# Create distance matrix for visualization, use Gower distance since mostly categorical data
drop <- c('name', 'date', 'city.state', 'city', 'state', 'race') 
df.fatal.clean <- df.fatal[ , !(names(df.fatal) %in% drop)]

gower_dist <- daisy(df.fatal.clean[, -1], metric = "gower")

# Check Gower dist works
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

# Looks like K = 2 is best
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

pam_results <- df.fatal.clean %>% dplyr::select(-id) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

# Look at numerics of clusters
pam_results$the_summary
# Looks to be clustered by threat level, race, and armed

# Look at medoids
df.fatal.clean[pam_fit$medoids, ]

############### Dimension reduction
# tSNE, t-distributed stochastic neighborhood embedding
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

# Lets look at some variables in 2D to see if anything separates well
tsne_data <-  data.frame(cluster = factor(pam_fit$clustering), df.fatal.clean, tsne_data, race=df.fatal$race)
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=threat_level))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=manner_of_death))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=signs_of_mental_illness))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=body_camera))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=minority))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color=race))

# Can compare with MDS
# MDS
fit <- cmdscale(gower_dist, k=2) # k is the number of dim
fit # view results

# Plot MDS without coloring groups  
fit <- as.data.frame(fit)
ggplot() + geom_point(data=fit, aes(fit[,1], fit[,2]))
# Plot MDS, color by cluster
fit <- cbind(fit, df.fatal, cluster = factor(pam_fit$clustering))
ggplot() + geom_point(data=fit, aes(x=V1, y=V2, color=cluster))

### Zoe

### Ed
ps.data = readRDS(file.choose())
set.seed(414)
library(randomForest)

###################### Functions ############################

# Loss Function
loss = function(y, f.hat, c){
  a = sum(y == "True" & f.hat == "False")
  b = sum(y == "False" & f.hat == "True")
  
  out = a*c + b
  return(out)
}

# Performs the Random Forest. Returns the confusion matrix, 
# the class error rates, and the value of the loss function
rf = function(X.train, Y.train, X.test, Y.test, thres, c = 9) {
  rf = randomForest(X.train, Y.train)
  pred.rf = predict(rf, X.test, type = "prob")
  t.num = which(colnames(pred.rf) == "True")
  pred.table = unname(ifelse(pred.rf[,t.num] > thres, "True", "False"))
  pred.table = cbind(pred.table,as.character(Y.test))
  
  out = table(pred.table[,1],pred.table[,2])
  false.err = 1-sum(pred.table[,1] == "False" & pred.table[,2] == "False")/sum(pred.table[,2] == "False")
  true.err = 1-sum(pred.table[,1] == "True" & pred.table[,2] == "True")/sum(pred.table[,2] == "True")
  
  loss = loss(y = Y.test, f.hat = pred.table[,1], c)
  
  return(list(out,false.err,true.err,loss))
}

# Creates k folds. Used for the CV function
folds = function(n, k){
  size = round(n/k)
  out = list()
  start = 0
  indices = sample(1:n,n,replace = FALSE)
  for(i in 1:k) {
    if(i < k) out[[i]] = indices[(start+1):(start+size)]
    else out[[i]] = indices[(start+1):n]
    start = start+size
  }
  return(out)
}

# Up Sample the minority class
up = function(X.train, Y.train){
  bcam.t = which(Y.train == "True")
  bcam.f = which(Y.train == "False")
  
  up.t = sample(bcam.t,length(bcam.f),replace = TRUE)
  up.f = sample(bcam.f,length(bcam.f),replace = FALSE)
  Y.up = Y.train[c(up.t,up.f)]
  X.up = X.train[c(up.t,up.f),]
  
  return(list(X.up,Y.up))
}

# Down Sample the majority class
down = function(X.train, Y.train){
  bcam.t = which(Y.train == "True")
  bcam.f = which(Y.train == "False")
  
  down.t = sample(bcam.t,length(bcam.t),replace = FALSE)
  down.f = sample(bcam.f,length(bcam.t),replace = FALSE)
  Y.down = Y.train[c(down.t,down.f)]
  X.down = X.train[c(down.t,down.f),]
  
  return(list(X.down,Y.down))
}

# Perform k-fold cross validation r times. Returns the loss 
# along with the error rate for the "true" class
cv.rf = function(X, Y, thres, c = 9, k = 5, r = 1,
                 sample = c("none","up","down")){
  loss = 0
  true.err = 0
  for(j in 1:r) {
    n = length(Y)
    sets = folds(n, k)
    for(i in 1:k) {
      X.train = X[unlist(sets[-i]),]
      Y.train = Y[unlist(sets[-i])]
      X.test = X[sets[[i]],]
      Y.test = Y[sets[[i]]]
      
      if(sample == "up") {
        temp = up(X.train,Y.train)
        X.train = temp[[1]]
        Y.train = temp[[2]]
      } 
      if(sample == "down") {
        temp = down(X.train,Y.train)
        X.train = temp[[1]]
        Y.train = temp[[2]]
      }
      
      results = rf(X.train, Y.train, X.test, Y.test, thres, c)
      loss = loss + results[[4]]
      true.err = true.err + results[[3]]
    }
  }
  return(list(loss/(k*r),(true.err)/(k*r)))
}

###################### Data Cleaning ##############################
Y = as.factor(ps.data$body_camera)
X = as.data.frame(ps.data[,c(4:14,16,17)])

Y = Y[-c(1935,2050)]
X = X[-c(1935,2050),]

# omit missing data
Y = Y[complete.cases(X)]
X = X[complete.cases(X),]

# subset "Armed"
X$armed = as.character(X$armed)
gun = grep("gun", X$armed)
X$armed[gun] = "gun"
other = which(X$armed != "gun" & 
                X$armed != "unarmed" & 
                X$armed != "undetermined" &
                X$armed != "knife" &
                X$armed != "vehicle" &
                X$armed != "toy weapon")
X$armed[other] = "other"
X$armed = as.factor(X$armed)

# remove city/state variables
X = X[-c(6:8)]

# create a training/test set
testset = function(X, Y, size = 500) {
  draws = sample(0:length(Y),length(Y),replace = FALSE)
  test = draws[1:size]
  train = draws[(size+1):length(Y)]
  
  Y.test = Y[test]
  X.test = X[test,]
  Y.train = Y[train]
  X.train = X[train,]
  return(list(X.train,Y.train,X.test,Y.test))
}

test = testset(X, Y, 500)
X.train = test[[1]]
Y.train = test[[2]]
X.test = test[[3]]
Y.test = test[[4]]

#################### Data Analysis ############################

rf.train = randomForest(X.train,Y.train)

# construct the oversampled training set
bcam.t = which(Y.train == "True")
bcam.f = which(Y.train == "False")

up.t = sample(bcam.t,length(bcam.f),replace = TRUE)
up.f = sample(bcam.f,length(bcam.f),replace = FALSE)
Y.up = Y.train[c(up.t,up.f)]
X.up = X.train[c(up.t,up.f),]

# fit random forest and calculate cv error
rf.up = randomForest(X.up,Y.up)
cv.rf(X.train,Y.train,.5,sample = "up")

# same as above for undersampled training set
down.t = sample(bcam.t,length(bcam.t),replace = FALSE)
down.f = sample(bcam.f,length(bcam.t),replace = FALSE)
Y.down = Y.train[c(down.t,down.f)]
X.down = X.train[c(down.t,down.f),]

rf.down = randomForest(X.down,Y.down)
cv.rf(X.train,Y.train,.5,sample = "down")

# For regular training set, search for optimal k
thresholds = c(0.001, 0.01, 0.1, .25, .5)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9, sample = "none")[[1]]))
}

thresholds = 0.1+0.01*(0:10)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9, sample = "none")[[1]]))
}

rf(X.train,Y.train,X.test,Y.test,.10)
rf(X.train,Y.train,X.train,Y.train,.1)

# For oversampled training set, search for optimal k
thresholds = c(0.001, 0.01, 0.1, .25, .5)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9,sample = "up")[[1]]))
}

thresholds = 0.1+0.01*(0:10)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9, sample = "up")[[1]]))
}

rf(X.up,Y.up,X.test,Y.test,.17)
rf(X.up,Y.up,X.up,Y.up,.17)

# For undersampled training set, search for optimal k
thresholds = c(0.001, 0.01, 0.1, .25, .5)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9, sample = "down")[[1]]))
}

thresholds = 0.4+0.01*(0:10)

for(x in thresholds){
  print(c(x,cv.rf(X.train,Y.train,x, r = 5, c = 9, sample = "down")[[1]]))
}

rf(X.down,Y.down,X.test,Y.test,.5)
rf(X.down,Y.down,X.down,Y.down,.5)



