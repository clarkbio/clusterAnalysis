library(stats)
library(ggdendro)
library(ggplot2)
library(cluster)

## Read in distance matrices data
dat <- read.csv("/home/user/R/clusterMatrix.csv", header = TRUE, row.names=1)

## Conversion into matrices for dist()
datMat <- as.matrix(dat)
distMat <- as.dist(datMat)

## Creation of distance matrices using dist() and the method parameter
## Available methods for dist(): 'euclidean' ; 'maximum' ; 'manhattan' ; 'canberra' ; 'binary' ; 'minkowski'
## e.x.: euclDist = dist(distMat, method = 'euclidean', p = 1)
euclDist = dist(distMat, method = 'maximum', p = 1)

###### Creation of clusters by strategy ######
### Cluster function: available methods for hclust(): "ward" ; "single" ; "complete" ; "average" ; "mcquitty" ; "median" ; "centroid"
## e.x.: hc <- hclust(distanceMatrix , method = "ward")
### Produce simple dendrogram 
## ggdendrogram(hc.ward, rotate = TRUE, size = 10) 
### Followed by ggplot parameters for tidiness/etc

##### 1 ##########
hc.centroid <- hclust(euclDist, method = 'centroid')
ggdendrogram(hc.centroid, rotate = TRUE, size = 10)
dhc.centroid <- as.dendrogram(hc.centroid)
dendro.centroid <- dendro_data(dhc.centroid, type = "rectangle")
#
p.centroid <- ggplot(segment(dendro.centroid)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend) , colour = "darkred", lwd = 1.75) +
  geom_text(data=dendro.centroid$labels, aes(x=x, y=y, label=label) , hjust = 1.3, size = 6) +
  ggtitle('Strategy #1: Centroid (unweighted), Centroid') + 
  theme(plot.title = element_text(size=16, face="bold", vjust=1.5)) + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + 
  labs(x="", y="") + 
  coord_flip(ylim=c(-.35,6.50), xlim=c(.75,5.25)) + 
  scale_y_continuous(breaks=seq(0,6,1))
  # 
#
p.centroid
##################

##### 2 ##########
hc.median <- hclust(euclDist, method = 'median')
ggdendrogram(hc.median, rotate = TRUE, size = 10)
dhc.median <- as.dendrogram(hc.median)
dendro.median <- dendro_data(dhc.median, type = "rectangle")
#
p.median <- ggplot(segment(dendro.median)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend) , colour = "forestgreen", lwd = 1.75) +
  coord_flip() + 
  geom_text(data=dendro.median$labels, aes(x=x, y=y, label=label) , hjust = 1.3, size = 6) +
  ggtitle('Strategy #2: Centroid (weighted), Median') + 
  theme(plot.title = element_text(size=16, face="bold", vjust=1.5)) + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + 
  labs(x="", y="") + 
  coord_flip(ylim=c(-.35,6.50), xlim=c(.75,5.25)) + 
  scale_y_continuous(breaks=seq(0,6,1))
#
p.median
##################

##### 3 ##########
hc.average <- hclust(euclDist, method = 'average')
ggdendrogram(hc.average, rotate = TRUE, size = 10)
dhc.average <- as.dendrogram(hc.average)
dendro.average <- dendro_data(dhc.average, type = "rectangle")
#
p.average <- ggplot(segment(dendro.average)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend) , colour = "firebrick1", lwd = 1.75) +
  coord_flip() + 
  geom_text(data=dendro.average$labels, aes(x=x, y=y, label=label) , hjust = 1.3, size = 6) +
  ggtitle('Strategy #3: Group-Mean Paired-Grouping') + 
  theme(plot.title = element_text(size=16, face="bold", vjust=1.5)) + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + 
  labs(x="", y="") + 
  coord_flip(ylim=c(-.35,6.50), xlim=c(.75,5.25)) + 
  scale_y_continuous(breaks=seq(0,6,1))
#
p.average
##################

##### 4 ##########
hc.flexible <- agnes(x = euclDist, method = 'flexible', par.method = 0.625)
ggdendrogram(hc.flexible, rotate = TRUE, size = 10)
dhc.flexible <- as.dendrogram(hc.flexible)
dendro.flexible <- dendro_data(dhc.flexible, type = "rectangle")
#
p.flexible <- ggplot(segment(dendro.flexible)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend) , colour = "deepskyblue4", lwd = 1.75) +
  coord_flip() + 
  geom_text(data=dendro.flexible$labels, aes(x=x, y=y, label=label) , hjust = 1.3, size = 6) +
  ggtitle('Strategy #4: Flexible') + 
  theme(plot.title = element_text(size=16, face="bold", vjust=1.5)) + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + 
  labs(x="", y="") + 
  coord_flip(ylim=c(-.35,6.50), xlim=c(.75,5.25)) + 
  scale_y_continuous(breaks=seq(0,6,1)) +
  scale_x_reverse()
#
p.flexible
##################

##### 5 ##########
hc.ward <- hclust(euclDist, method = 'ward')
ggdendrogram(hc.ward, rotate = TRUE, size = 10)
dhc.ward <- as.dendrogram(hc.ward)
dendro.ward <- dendro_data(dhc.ward, type = "rectangle")
#
p.ward <- ggplot(segment(dendro.ward)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend) , colour = "deepskyblue3", lwd = 1.75) +
  coord_flip() + 
  geom_text(data=dendro.ward$labels, aes(x=x, y=y, label=label) , hjust = 1.3, size = 6) +
  ggtitle('Strategy #5: Ward\'s Method') + 
  theme(plot.title = element_text(size=16, face="bold", vjust=1.5)) + 
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + 
  labs(x="", y="") + 
  coord_flip(ylim=c(-.35,6.50), xlim=c(.75,5.25)) + 
  scale_y_continuous(breaks=seq(0,6,1))

p.ward
##############

## Saving .png's of each plot
ggsave(p.centroid, file="strat1.png", scale=2)
ggsave(p.median, file="strat2.png", scale=2)
ggsave(p.average, file="strat3.png", scale=2)
ggsave(p.flexible, file="strat4.png", scale=2)
ggsave(p.ward, file="strat5.png", scale=2)




## ggplot reference ##

# p <- ggplot(segment(dendro.ward), main = "Ward Technique") +
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#   coord_flip()

# p <- ggplot(segment(ddata)) +
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#   coord_flip() +
#   scale_y_reverse(expand = c(0.2, 0))

## Optional arguments for ggplot from ggdendrogram
# p + 
#   coord_flip() +
#   theme_dendro()

### Ref ###
## http://www.statmethods.net/advgraphs/ggplot2.html
## http://www.statmethods.net/advstats/cluster.html
## https://cran.r-project.org/web/packages/cluster/cluster.pdf
## http://www.r-tutor.com/gpu-computing/clustering/distance-matrix
## https://stat.ethz.ch/R-manual/R-devel/library/cluster/html/agnes.html
## http://www.talkstats.com/showthread.php/32875-Using-flexible-beta-within-hclust()
## http://www.davidzeleny.net/anadat-r/doku.php/en:hier-agglom
## https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html
### ### ###
