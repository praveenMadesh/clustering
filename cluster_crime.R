####
##preforming clustering fot the given crime data
library(readr)
crime_data<-read.csv(file.choose())
View(crime_data)

crime_data1<-crime_data[,2:5]
View(crime_data1)
summary(crime_data1)##measure of centeral tendency mean,median

#measure of dispersion
var(crime_data1$Murder)
sd(crime_data1$Murder)
var(crime_data1$Assault)
sd(crime_data1$Assault)
var(crime_data1$UrbanPop)
sd(crime_data1$UrbanPop)
var(crime_data1$Rape)
sd(crime_data1$Rape)

#measure of skewness
library(moments)
skewness(crime_data1$Murder)
skewness(crime_data1$Assault)
skewness(crime_data1$UrbanPop)
skewness(crime_data1$Rape)
#measure of kurtosis
kurtosis(crime_data1$Murder)
kurtosis(crime_data1$Assault)
kurtosis(crime_data1$UrbanPop)
kurtosis(crime_data1$Rape)

##
plot(crime_data1)##column1 and column2 Direction of the data is +ve,
##and data distributed normally 
cor(crime_data1)### correlation is 0.80 which is strong 
attach(crime_data)

####scatter plot
plot(Murder~Rape,crime_data1)### +ve direction, slightly strong and linear relation 
with(crime_data,text(Murder~Rape,labels=state,pos=4,cex=.3))
#normalizing data
norm <- scale(crime_data1)
plot(norm)

####calculating euclidean distance
distance <- dist(norm,method="euclidean")
print(distance,digits = 3)

####Dendogram with complete linkage
fit.c<-hclust(distance,method="complete")
plot(fit.c,hang = -1)###labels = crime_data$state)
rect.hclust(fit.c,k=4,border = "orange")

group <- cutree(fit.c,k=5)
ratio<-as.matrix(group)
final<-data.frame(crime_data,ratio)
View(final)

write.csv(final,file="final.csv",row.names = F)
aggregate(crime_data[,-1] , by=list(final$ratio),mean)

