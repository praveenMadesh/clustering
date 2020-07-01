library(readr)
airlines<-read.csv(file.choose())
View(airlines)
airlines <- airlines[,-1]
View(airlines)
norm <- scale(airlines[1:11])
View(norm)
plot(norm)

km_air<- kmeans(norm,4)
km_air

library(animation)
km_air <- kmeans.ani(norm,3)
str(km_air)
km_air$cluster
km_air$withinss
km_air$centers
wss <- (nrow(norm)-1)*sum(apply(norm,2,var))
wss
for(i in 1:11) wss[i]=sum(kmeans(norm,centers = 1)$withinss)
plot(1:11,wss,type = "b",xlab = "number of cluster",ylab="wuth group sum of squres")
title(sub = "k-means clustering Scree-plot")
############$$$$$$$$$########
library(readr)
airlines<-read.csv(file.choose())
View(airlines)
airlines <- airlines[,-1]
View(airlines)
airlines <- scale(airlines)
d <- dist(airlines,method="euclidean")
fit <- hclust(d,method="complete")
plot(fit)
plot(fit,hang = 1)
rect.hclust(fit,k=8,border = "red")
gorup <- cutree(fit,k=8)
