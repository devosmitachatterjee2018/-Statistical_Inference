### MVE155 Statistical Inference
### Assignment 1
### Devosmita Chatterjee

setwd("C:/Users/Acer/Desktop/MVE155/Assignments")#set current directory
#getwd()
data=read.csv("cancer.txt", quote='\'')#read file
colnames(data)=c("BreastCancer_Mortality","AdultWhiteFemale_Population")#column names
data
mydata=tibble::rowid_to_column(data, "Counties")
#str(data)
#dim(data)
#names(data)
mydata

#65a.
#min(data$BreastCancer_Mortality)#min value
#max(data$BreastCancer_Mortality)#max value
hist(data$BreastCancer_Mortality, col="red", xlim=c(0, 400), xlab="BreastCancer_Mortality", main="Histogram of the population values for cancer mortality")#histogram of the population values for cancer mortality

#65b.
mean(data$BreastCancer_Mortality)#population mean
sum(data$BreastCancer_Mortality)#total cancer mortality

var(data$BreastCancer_Mortality)#population variance
sd(data$BreastCancer_Mortality)#standard deviation

#65c. sampling distribution of the mean of a sample of 25 observations of cancer mortality.??
set.seed(1)
samplemeans = numeric(10000)
for (i in 1:10000){
  samplemeans[i] = mean(sample(data$BreastCancer_Mortality, 25, replace=FALSE))
}
hist(samplemeans, freq=FALSE, col="red", xlab="Sample_Means", main='Sampling Distribution of the Mean')
lines(density(samplemeans), col = "green", lwd = 2)

#65d.
set.seed(1) # set the seed
data25=sample(data$BreastCancer_Mortality, 25, replace=FALSE)  
#data25 # select a random subset of your full dataset
xbar25=mean(data25)#mean of the simple random sample of size 25
xbar25
sum25=sum(data25)#total cancer mortality of the simple random sample of size 25
sum25

#65e.
var(data25)#population variance of the simple random sample of size 25
sdx25=sd(data25)
sdx25#standard deviation of the simple random sample of size 25

#65f.
n25=25
N=301
sdxbar25 = sqrt(var(data25)/n25*(1-n25/N))
z = qnorm(1 - 0.05/2)#z(alpha/2) = z(0.05) 
#z
Imean25=c(xbar25 - z * sdxbar25, xbar25 + z * sdxbar25)#95% confidence intervals for the population mean of the simple random sample of size 25
Imean25
Itotal25=c(301 * xbar25 - z * 301 * sdxbar25, 301 * xbar25 + z * 301 * sdxbar25)#95% confidence intervals for the population total of the simple random sample of size 25
Itotal25

#65g.
set.seed(1) # set the seed
data100=sample(data$BreastCancer_Mortality, 100, replace=FALSE)  
#data100 # select a random subset of your full dataset


xbar100=mean(data100)#mean of the simple random sample of size 100
xbar100
sum100=sum(data100)#total cancer mortality of the simple random sample of size 100
sum100

var(data100)#population variance of the simple random sample of size 100
sdx100=sd(data100)
sdx100#standard deviation of the simple random sample of size 100

n100=100
N=301
sdxbar100 = sqrt(var(data100)/n100*(1-n100/N))
Imean100=c(xbar100 - z * sdxbar100, xbar100 + z * sdxbar100)#95% confidence intervals for the population mean of the simple random sample of size 100
Imean100
Itotal100=c(301*xbar100 - z * 301 * sdxbar100, 301*xbar100 + z * 301 * sdxbar100)#95% confidence intervals for the population total of the simple random sample of size 100
Itotal100

#65l.
















min(mydata$Counties)
max(mydata$Counties)
(301-1)/4
1+75
77+74
152+74
227+74

mydata$Counties[(mydata$Counties >= 1) & (mydata$Counties <= 76)]="s1"
mydata$Counties[(mydata$Counties >= 77) & (mydata$Counties <= 151)]="s2"
mydata$Counties[(mydata$Counties >= 152) & (mydata$Counties <= 226)]="s3"
mydata$Counties[(mydata$Counties >= 227) & (mydata$Counties <= 301)]="s4"
mydata
set.seed(1)
sample(mydata$AdultWhiteFemale_Population[(mydata$Counties >= 1) & (mydata$Counties <= 76)], 6, replace=FALSE)
set.seed(1)

sample(mydata$AdultWhiteFemale_Population[77<mydata$Counties<151], 6, replace=FALSE)
sample(mydata$AdultWhiteFemale_Population[(mydata$Counties >= 227) & (mydata$Counties <= 301)], 6, replace=FALSE)


table(s$Stratum,s$Prob)
tapply(s$mean(s$AdultWhiteFemale_Population), s$prob, mean)







mydata$Counties= sample(c("AA", "BB", "CC", "DD"), 6, replace = FALSE)

stratified(mydata$Counties, c("AA", "BB", "CC", "DD"), 6, replace = FALSE)





mydata$Counties_stratum[mydata$Counties >= 1 & mydata$Counties <= 76]="1-76"
mydata$Counties_stratum[mydata$Counties >= 77 & mydata$Counties <= 151]="77-151"
mydata$Counties_stratum[mydata$Counties >= 152 & mydata$Counties <= 226]="152-226"
mydata$Counties_stratum[mydata$Counties >= 227 & mydata$Counties <= 301]="227-301"
mydata