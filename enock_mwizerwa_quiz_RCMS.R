rm(list=ls()) ## this command clears the history
data<- read.csv("My Data.csv") ## importing the data
View(data)
new_data<-data[-1]
View(new_data)

# i Cluster Analysis
new_data_scaled <- scale(new_data) ## standardizing the variables to make mean=0 and std=1
new_data_scaled.dist <- dist(new_data_scaled) # distance computation with year as observation
new_data_scaled.distT <- dist(t(new_data_scaled)) # distance computation with year as variables
new_data_scaled.distT
new_data_scaled

ward= hclust(new_data_scaled.distT,method='ward.D2')#clustering using ward algorithm
plot(ward, main = 'Wards Method',xlab =" ", sub ="")
rect.hclust(ward, k =3, border = 'red') ## selecting three clusters
 # ii PCA Analysis
install.packages('psych')
require(psych)

pc2 <- principal(new_data_scaled, nfactors = 2, rotate = "varimax") 
pc2
scores<- as.data.frame(pc2$scores) 
scores


# iii. plotting some of the scores
par(mfrow = c(1,2))
plot(scores$RC1~rownames(scores),type ="o", xlab = 'year', ylab='PF1', main='Rotated Component 1')
plot(scores$RC2~rownames(scores),type ="o", xlab = 'year', ylab='PF2', main='Rotated Component 2')

# iv Auto correlogram 
prep <- ts(scores$RC1, start = c(1960,1), end = c(2019,12), frequency = 12)
temp <- ts(scores$RC2, start = c(1960,1), end = c(2019,12), frequency = 12)
prep
temp
forecast::Acf(prep, lag.max = 36)
# V cross Correlation

PF1<-prep
PF2<-temp
forecast::Ccf(PF1,PF2)
dev.off()
# spectral Analysis 
library(forecast)
library(TSA)
library(biwavelet)

p1 <- periodogram(PF1)
period_Prep <- 1/p1$freq
plot(period_Prep, p1$spec, type = 'b', xlim = c(2,37), xlab = 'period', ylab = 'periodogram',
     main = 'PF1')
#VII wavelet morlet
timelong=seq(1960, 2020-1/12, 1/12)  # indexing
wavelet=wt(cbind(timelong,PF1),dj=0.1,mother="morlet",max.scale=16) #wavelet
par(oma=c(0, 0, 0, 1), mar=c(5, 4, 4, 5) + 0.1)
xx=plot(wavelet,  plot.cb = TRUE,lwd.coi = 1,col.coi="white",alpha.coi=0.5,col.sig="black",
        lwd.sig = 2, ncol = 768, tol = 0.95, plot.phase=F,ylab="Period(Year)", xlab = "Time(Year)",
        main='PF1')
