### MVE155 Statistical Inference
### Assignment 1
### Devosmita Chatterjee

setwd("C:/Users/Acer/Desktop/MVE155/Assignment1")#set current directory
#getwd()
data=read.csv("cancer.txt", quote='\'')#read file
colnames(data)=c("BreastCancer_Mortality","AdultWhiteFemale_Population")#column names
#data
mydata=tibble::rowid_to_column(data, "Counties")#working data
#str(data)
#dim(data)
#names(data)
#mydata

#65a.
#min(mydata$BreastCancer_Mortality)
#max(mydata$BreastCancer_Mortality)
hist(mydata$BreastCancer_Mortality, col="red", xlab="BreastCancer_Mortality", main="Histogram of the population values for cancer mortality")#histogram of the population values for cancer mortality

#65b.
pop_mu=mean(mydata$BreastCancer_Mortality)
pop_mu#population mean
pop_tot=sum(mydata$BreastCancer_Mortality)
pop_tot#total cancer mortality

N=301
pop_var=var(mydata$BreastCancer_Mortality)*(N-1)/N
pop_var#population variance
pop_sd=sqrt(pop_var)
pop_sd#standard deviation

#65c. 
set.seed(1)
sample_means = numeric(10000)
for (i in 1:10000){
  sample_means[i] = mean(sample(mydata$BreastCancer_Mortality, 25, replace=FALSE))
}
hist(sample_means, freq=FALSE, col="red", ylim=c(0, 0.05), xlab="Sample_Means", main='Sampling Distribution of the Mean')#sampling distribution of the mean of a sample of 25 observations of cancer mortality
lines(density(sample_means), col = "blue", lwd = 2)

#65d.
set.seed(1) # set the seed
data25=sample(mydata$BreastCancer_Mortality, 25, replace=FALSE)
#data25#draw a simple random sample of size 25
xbar25=mean(data25)
xbar25#mean of the simple random sample of size 25
T25=N*mean(data25)
T25#total cancer mortality of the simple random sample of size 25

#65e.
var25=var(data25)
var25#population variance of the simple random sample of size 25
sdx25=sd(data25)
sdx25#standard deviation of the simple random sample of size 25

#65f.
n25=25
N=301
sdxbar25 = sqrt((var25/n25)*(1-n25/N))
#sdxbar25
z = qnorm(1 - 0.05/2)#z(alpha/2) = z(0.05) 
#z
Imean25=c(xbar25 - z * sdxbar25, xbar25 + z * sdxbar25)
Imean25#95% confidence intervals for the population mean of the simple random sample of size 25
Itotal25=c(N * xbar25 - z * N * sdxbar25, N * xbar25 + z * N * sdxbar25)
Itotal25#95% confidence intervals for the population total of the simple random sample of size 25

pop_mu
pop_tot
(pop_mu>=Imean25[1])&(pop_mu<=Imean25[2])
(pop_tot>=Itotal25[1])&(pop_tot<=Itotal25[2])

#65g.
set.seed(1) # set the seed
data100=sample(mydata$BreastCancer_Mortality, 100, replace=FALSE)
#data100#draw a simple random sample of size 100

xbar100=mean(data100)
xbar100#mean of the simple random sample of size 100
T100=N*mean(data100)
T100#total cancer mortality of the simple random sample of size 100

var100=var(data100)
var100#population variance of the simple random sample of size 100
sdx100=sd(data100)
sdx100#standard deviation of the simple random sample of size 100

n100=100
N=301
sdxbar100 = sqrt((var100/n100)*(1-n100/N))
z = qnorm(1 - 0.05/2)#z(alpha/2) = z(0.05) 
Imean100=c(xbar100 - z * sdxbar100, xbar100 + z * sdxbar100)#95% confidence intervals for the population mean of the simple random sample of size 100
Imean100
Itotal100=c(301*xbar100 - z * 301 * sdxbar100, 301*xbar100 + z * 301 * sdxbar100)#95% confidence intervals for the population total of the simple random sample of size 100
Itotal100

pop_mu
pop_tot
(pop_mu>=Imean100[1])&(pop_mu<=Imean100[2])
(pop_tot>=Itotal100[1])&(pop_tot<=Itotal100[2])

#65l.
#Stratify the counties into four strata by population size and randomly sample six observations from each stratum
N=301
k=4

set.seed(1)
ran_stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 76], 6, replace = FALSE)
ran_stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 151], 6, replace = FALSE)
ran_stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 152 & mydata$Counties <= 226], 6, replace = FALSE)
ran_stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 227 & mydata$Counties <= 301], 6, replace = FALSE)
ran_stratum <- list(ran_stratum1, ran_stratum2, ran_stratum3, ran_stratum4)
ran_stratum

ran_N4=c()
ran_N4[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 76])
ran_N4[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 151])
ran_N4[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 152 & mydata$Counties <= 226])
ran_N4[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 227 & mydata$Counties <= 301])
#ran_N4

ran_omega=c()
ran_omega[1]=ran_N4[1]/N
ran_omega[2]=ran_N4[2]/N
ran_omega[3]=ran_N4[3]/N
ran_omega[4]=ran_N4[4]/N
#ran_omega

ran_mean=c()
for(i in 1:k){
  ran_mean[length(ran_mean)+1] <- mean(ran_stratum[[i]])
}
#ran_mean

ran_meanbar = t(ran_omega)%*%ran_mean
ran_meanbar#population mean

ran_totmeanbar=N*ran_meanbar
ran_totmeanbar#total mortality

#65m.
N=301
k=4
n=128

set.seed(1)
stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 76], 32, replace = FALSE)
stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 151], 32, replace = FALSE)
stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 152 & mydata$Counties <= 226], 32, replace = FALSE)
stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 227 & mydata$Counties <= 301], 32, replace = FALSE)
stratum <- list(stratum1, stratum2, stratum3, stratum4)

N4=c()
N4[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 76])
N4[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 151])
N4[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 152 & mydata$Counties <= 226])
N4[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 227 & mydata$Counties <= 301])
#N4

omega=c()
omega[1]=N4[1]/N
omega[2]=N4[2]/N
omega[3]=N4[3]/N
omega[4]=N4[4]/N
#omega

mean=c()
for(i in 1:k){
  mean[length(mean)+1] <- mean(stratum[[i]])
}
#mean

meanbar = t(omega)%*%mean
#meanbar

sd=c()
for(i in 1:k){
  sd[length(sd)+1] <- sd(stratum[[i]])
}
#sd

sdbar = t(omega)%*%sd
#sdbar

optalloc=c()
for(i in 1:k){
  optalloc[length(optalloc)+1] <- n*omega[i]*sd[i]/sdbar
}
optalloc#optimal allocation

proalloc=c()
for(i in 1:k){
  proalloc[length(proalloc)+1] <- n*omega[i]
}
proalloc#proportional allocation

sdsqbar = t(omega)%*%sd^2
#sdsqbar

total=0
for (i in 1:k) {
  total<-total+omega[i]*(mean[i]-meanbar)^2
}
#total

varx=(sdsqbar+total)/n
varx_so=sdbar^2/n
varx_sp=sdsqbar/n
varx#variance of the estimate of the population mean obtained using simple random sampling
varx_sp#variance of the estimate of the population mean obtained using proportional allocation
varx_so#variance of the estimate of the population mean obtained using optimal allocation

#65n.
############################Strata=8###########################
N=301
k=8
n=128

set.seed(1)
stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 38], 16, replace = FALSE)
stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 39 & mydata$Counties <= 76], 16, replace = FALSE)
stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 114], 16, replace = FALSE)
stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 115 & mydata$Counties <= 152], 16, replace = FALSE)
stratum5=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 153 & mydata$Counties <= 190], 16, replace = FALSE)
stratum6=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 227], 16, replace = FALSE)
stratum7=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 228 & mydata$Counties <= 264], 16, replace = FALSE)
stratum8=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 265 & mydata$Counties <= 301], 16, replace = FALSE)
stratum <- list(stratum1, stratum2, stratum3, stratum4, stratum5, stratum6, stratum7, stratum8)

N8=c()
N8[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 38])
N8[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 39 & mydata$Counties <= 76])
N8[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 114])
N8[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 115 & mydata$Counties <= 152])
N8[5]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 153 & mydata$Counties <= 190])
N8[6]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 227])
N8[7]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 228 & mydata$Counties <= 264])
N8[8]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 265 & mydata$Counties <= 301])
#N8

omega=c()
omega[1]=N8[1]/N
omega[2]=N8[2]/N
omega[3]=N8[3]/N
omega[4]=N8[4]/N
omega[5]=N8[5]/N
omega[6]=N8[6]/N
omega[7]=N8[7]/N
omega[8]=N8[8]/N
#omega

mean=c()
for(i in 1:k){
  mean[length(mean)+1] <- mean(stratum[[i]])
}
#mean

meanbar = t(omega)%*%mean
#meanbar

sd=c()
for(i in 1:k){
  sd[length(sd)+1] <- sd(stratum[[i]])
}
#sd

sdbar = t(omega)%*%sd
#sdbar

sdsqbar = t(omega)%*%sd^2
#sdsqbar

total=0
for (i in 1:k) {
  total<-total+omega[i]*(mean[i]-meanbar)^2
}
#total

varx=(sdsqbar+total)/n
varx_so=sdbar^2/n
varx_sp=sdsqbar/n
varx#variance of the estimate of the population mean obtained using simple random sampling
varx_sp#variance of the estimate of the population mean obtained using proportional allocation
varx_so#variance of the estimate of the population mean obtained using optimal allocation



############################Strata=16###########################
N=301
k=16
n=128

set.seed(1)
stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 19], 8, replace = FALSE)
stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 20 & mydata$Counties <= 38], 8, replace = FALSE)
stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 39 & mydata$Counties <= 57], 8, replace = FALSE)
stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 58 & mydata$Counties <= 76], 8, replace = FALSE)
stratum5=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 95], 8, replace = FALSE)
stratum6=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 96 & mydata$Counties <= 114], 8, replace = FALSE)
stratum7=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 115 & mydata$Counties <= 133], 8, replace = FALSE)
stratum8=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 134 & mydata$Counties <= 152], 8, replace = FALSE)
stratum9=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 153 & mydata$Counties <= 171], 8, replace = FALSE)
stratum10=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 172 & mydata$Counties <= 190], 8, replace = FALSE)
stratum11=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 209], 8, replace = FALSE)
stratum12=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 210 & mydata$Counties <= 228], 8, replace = FALSE)
stratum13=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 229 & mydata$Counties <= 247], 8, replace = FALSE)
stratum14=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 248 & mydata$Counties <= 265], 8, replace = FALSE)
stratum15=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 283], 8, replace = FALSE)
stratum16=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 284 & mydata$Counties <= 301], 8, replace = FALSE)
stratum <- list(stratum1, stratum2, stratum3, stratum4, stratum5, stratum6, stratum7, stratum8, stratum9, stratum10, stratum11, stratum12, stratum13, stratum14, stratum15, stratum16)

N16=c()
N16[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 19])
N16[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 20 & mydata$Counties <= 38])
N16[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 39 & mydata$Counties <= 57])
N16[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 58 & mydata$Counties <= 76])
N16[5]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 77 & mydata$Counties <= 95])
N16[6]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 96 & mydata$Counties <= 114])
N16[7]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 115 & mydata$Counties <= 133])
N16[8]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 134 & mydata$Counties <= 152])
N16[9]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 153 & mydata$Counties <= 171])
N16[10]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 172 & mydata$Counties <= 190])
N16[11]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 209])
N16[12]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 210 & mydata$Counties <= 228])
N16[13]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 229 & mydata$Counties <= 247])
N16[14]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 248 & mydata$Counties <= 265])
N16[15]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 283])
N16[16]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 284 & mydata$Counties <= 301])
#N16

omega=c()
omega[1]=N16[1]/N
omega[2]=N16[2]/N
omega[3]=N16[3]/N
omega[4]=N16[4]/N
omega[5]=N16[5]/N
omega[6]=N16[6]/N
omega[7]=N16[7]/N
omega[8]=N16[8]/N
omega[9]=N16[9]/N
omega[10]=N16[10]/N
omega[11]=N16[11]/N
omega[12]=N16[12]/N
omega[13]=N16[13]/N
omega[14]=N16[14]/N
omega[15]=N16[15]/N
omega[16]=N16[16]/N
#omega

mean=c()
for(i in 1:k){
  mean[length(mean)+1] <- mean(stratum[[i]])
}
#mean

meanbar = t(omega)%*%mean
#meanbar

sd=c()
for(i in 1:k){
  sd[length(sd)+1] <- sd(stratum[[i]])
}
#sd

sdbar = t(omega)%*%sd
#sdbar

sdsqbar = t(omega)%*%sd^2
#sdsqbar

total=0
for (i in 1:k) {
  total<-total+omega[i]*(mean[i]-meanbar)^2
}
#total

varx=(sdsqbar+total)/n
varx_so=sdbar^2/n
varx_sp=sdsqbar/n
varx#variance of the estimate of the population mean obtained using simple random sampling
varx_sp#variance of the estimate of the population mean obtained using proportional allocation
varx_so#variance of the estimate of the population mean obtained using optimal allocation



############################Strata=32###########################
N=301
k=32
n=128

set.seed(1)
stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 10], 4, replace = FALSE)
stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 11 & mydata$Counties <= 20], 4, replace = FALSE)
stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 21 & mydata$Counties <= 30], 4, replace = FALSE)
stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 31 & mydata$Counties <= 40], 4, replace = FALSE)
stratum5=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 41 & mydata$Counties <= 50], 4, replace = FALSE)
stratum6=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 51 & mydata$Counties <= 60], 4, replace = FALSE)
stratum7=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 61 & mydata$Counties <= 70], 4, replace = FALSE)
stratum8=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 71 & mydata$Counties <= 80], 4, replace = FALSE)
stratum9=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 81 & mydata$Counties <= 90], 4, replace = FALSE)
stratum10=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 91 & mydata$Counties <= 100], 4, replace = FALSE)
stratum11=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 101 & mydata$Counties <= 110], 4, replace = FALSE)
stratum12=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 111 & mydata$Counties <= 120], 4, replace = FALSE)
stratum13=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 121 & mydata$Counties <= 130], 4, replace = FALSE)
stratum14=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 131 & mydata$Counties <= 139], 4, replace = FALSE)
stratum15=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 140 & mydata$Counties <= 148], 4, replace = FALSE)
stratum16=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 149 & mydata$Counties <= 157], 4, replace = FALSE)
stratum17=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 158 & mydata$Counties <= 166], 4, replace = FALSE)
stratum18=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 167 & mydata$Counties <= 175], 4, replace = FALSE)
stratum19=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 176 & mydata$Counties <= 184], 4, replace = FALSE)
stratum20=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 185 & mydata$Counties <= 193], 4, replace = FALSE)
stratum21=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 194 & mydata$Counties <= 202], 4, replace = FALSE)
stratum22=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 203 & mydata$Counties <= 211], 4, replace = FALSE)
stratum23=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 212 & mydata$Counties <= 220], 4, replace = FALSE)
stratum24=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 221 & mydata$Counties <= 229], 4, replace = FALSE)
stratum25=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 230& mydata$Counties <= 238], 4, replace = FALSE)
stratum26=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 239 & mydata$Counties <= 247], 4, replace = FALSE)
stratum27=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 248 & mydata$Counties <= 256], 4, replace = FALSE)
stratum28=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 257 & mydata$Counties <= 265], 4, replace = FALSE)
stratum29=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 274], 4, replace = FALSE)
stratum30=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 275 & mydata$Counties <= 283], 4, replace = FALSE)
stratum31=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 284 & mydata$Counties <= 292], 4, replace = FALSE)
stratum32=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 293 & mydata$Counties <= 301], 4, replace = FALSE)
stratum <- list(stratum1, stratum2, stratum3, stratum4, stratum5, stratum6, stratum7, stratum8, stratum9, stratum10, stratum11, stratum12, stratum13, stratum14, stratum15, stratum16, stratum17, stratum18, stratum19, stratum20, stratum21, stratum22, stratum23, stratum24, stratum25, stratum26, stratum27, stratum28, stratum29, stratum30, stratum31, stratum32)

N32=c()
N32[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 10])
N32[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 11 & mydata$Counties <= 20])
N32[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 21 & mydata$Counties <= 30])
N32[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 31 & mydata$Counties <= 40])
N32[5]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 41 & mydata$Counties <= 50])
N32[6]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 51 & mydata$Counties <= 60])
N32[7]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 61 & mydata$Counties <= 70])
N32[8]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 71 & mydata$Counties <= 80])
N32[9]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 81 & mydata$Counties <= 90])
N32[10]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 91 & mydata$Counties <= 100])
N32[11]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 101 & mydata$Counties <= 110])
N32[12]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 111 & mydata$Counties <= 120])
N32[13]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 121 & mydata$Counties <= 130])
N32[14]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 131 & mydata$Counties <= 139])
N32[15]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 140 & mydata$Counties <= 148])
N32[16]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 149 & mydata$Counties <= 157])
N32[17]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 158 & mydata$Counties <= 166])
N32[18]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 167 & mydata$Counties <= 175])
N32[19]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 176 & mydata$Counties <= 184])
N32[20]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 185 & mydata$Counties <= 193])
N32[21]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 194 & mydata$Counties <= 202])
N32[22]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 203 & mydata$Counties <= 211])
N32[23]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 212 & mydata$Counties <= 220])
N32[24]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 221 & mydata$Counties <= 229])
N32[25]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 230& mydata$Counties <= 238])
N32[26]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 239 & mydata$Counties <= 247])
N32[27]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 248 & mydata$Counties <= 256])
N32[28]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 257 & mydata$Counties <= 265])
N32[29]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 274])
N32[30]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 275 & mydata$Counties <= 283])
N32[31]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 284 & mydata$Counties <= 292])
N32[32]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 293 & mydata$Counties <= 301])
#N32

omega=c()
omega[1]=N32[1]/N
omega[2]=N32[2]/N
omega[3]=N32[3]/N
omega[4]=N32[4]/N
omega[5]=N32[5]/N
omega[6]=N32[6]/N
omega[7]=N32[7]/N
omega[8]=N32[8]/N
omega[9]=N32[9]/N
omega[10]=N32[10]/N
omega[11]=N32[11]/N
omega[12]=N32[12]/N
omega[13]=N32[13]/N
omega[14]=N32[14]/N
omega[15]=N32[15]/N
omega[16]=N32[16]/N
omega[17]=N32[17]/N
omega[18]=N32[18]/N
omega[19]=N32[19]/N
omega[20]=N32[20]/N
omega[21]=N32[21]/N
omega[22]=N32[22]/N
omega[23]=N32[23]/N
omega[24]=N32[24]/N
omega[25]=N32[25]/N
omega[26]=N32[26]/N
omega[27]=N32[27]/N
omega[28]=N32[28]/N
omega[29]=N32[29]/N
omega[30]=N32[30]/N
omega[31]=N32[31]/N
omega[32]=N32[32]/N
#omega

mean=c()
for(i in 1:k){
  mean[length(mean)+1] <- mean(stratum[[i]])
}
#mean

meanbar = t(omega)%*%mean
#meanbar

sd=c()
for(i in 1:k){
  sd[length(sd)+1] <- sd(stratum[[i]])
}
#sd

sdbar = t(omega)%*%sd
#sdbar

sdsqbar = t(omega)%*%sd^2
#sdsqbar

total=0
for (i in 1:k) {
  total<-total+omega[i]*(mean[i]-meanbar)^2
}
#total

varx=(sdsqbar+total)/n
varx_so=sdbar^2/n
varx_sp=sdsqbar/n
varx#variance of the estimate of the population mean obtained using simple random sampling
varx_sp#variance of the estimate of the population mean obtained using proportional allocation
varx_so#variance of the estimate of the population mean obtained using optimal allocation





############################Strata=64###########################
N=301
k=64
n=128

set.seed(1)
stratum1=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 5], 2, replace = FALSE)
stratum2=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 6 & mydata$Counties <= 10], 2, replace = FALSE)
stratum3=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 11 & mydata$Counties <= 15], 2, replace = FALSE)
stratum4=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 16 & mydata$Counties <= 20], 2, replace = FALSE)
stratum5=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 21 & mydata$Counties <= 25], 2, replace = FALSE)
stratum6=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 26 & mydata$Counties <= 30], 2, replace = FALSE)
stratum7=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 31 & mydata$Counties <= 35], 2, replace = FALSE)
stratum8=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 36 & mydata$Counties <= 40], 2, replace = FALSE)
stratum9=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 41 & mydata$Counties <= 45], 2, replace = FALSE)
stratum10=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 46 & mydata$Counties <= 50], 2, replace = FALSE)
stratum11=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 51 & mydata$Counties <= 55], 2, replace = FALSE)
stratum12=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 56 & mydata$Counties <= 60], 2, replace = FALSE)
stratum13=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 61 & mydata$Counties <= 65], 2, replace = FALSE)
stratum14=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 66 & mydata$Counties <= 70], 2, replace = FALSE)
stratum15=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 71 & mydata$Counties <= 75], 2, replace = FALSE)
stratum16=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 76 & mydata$Counties <= 80], 2, replace = FALSE)
stratum17=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 81 & mydata$Counties <= 85], 2, replace = FALSE)
stratum18=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 86 & mydata$Counties <= 90], 2, replace = FALSE)
stratum19=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 91 & mydata$Counties <= 95], 2, replace = FALSE)
stratum20=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 96 & mydata$Counties <= 100], 2, replace = FALSE)
stratum21=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 101 & mydata$Counties <= 105], 2, replace = FALSE)
stratum22=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 106 & mydata$Counties <= 110], 2, replace = FALSE)
stratum23=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 111 & mydata$Counties <= 115], 2, replace = FALSE)
stratum24=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 116 & mydata$Counties <= 120], 2, replace = FALSE)
stratum25=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 121& mydata$Counties <= 125], 2, replace = FALSE)
stratum26=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 126 & mydata$Counties <= 130], 2, replace = FALSE)
stratum27=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 131 & mydata$Counties <= 135], 2, replace = FALSE)
stratum28=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 136 & mydata$Counties <= 140], 2, replace = FALSE)
stratum29=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 141 & mydata$Counties <= 145], 2, replace = FALSE)
stratum30=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 146 & mydata$Counties <= 150], 2, replace = FALSE)
stratum31=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 151 & mydata$Counties <= 155], 2, replace = FALSE)
stratum32=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 156 & mydata$Counties <= 160], 2, replace = FALSE)
stratum33=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 161 & mydata$Counties <= 165], 2, replace = FALSE)
stratum34=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 166 & mydata$Counties <= 170], 2, replace = FALSE)
stratum35=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 171 & mydata$Counties <= 175], 2, replace = FALSE)
stratum36=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 176 & mydata$Counties <= 180], 2, replace = FALSE)
stratum37=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 181 & mydata$Counties <= 185], 2, replace = FALSE)
stratum38=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 186 & mydata$Counties <= 190], 2, replace = FALSE)
stratum39=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 195], 2, replace = FALSE)
stratum40=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 196 & mydata$Counties <= 200], 2, replace = FALSE)
stratum41=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 201 & mydata$Counties <= 205], 2, replace = FALSE)
stratum42=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 206 & mydata$Counties <= 210], 2, replace = FALSE)
stratum43=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 211 & mydata$Counties <= 215], 2, replace = FALSE)
stratum44=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 216 & mydata$Counties <= 220], 2, replace = FALSE)
stratum45=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 221 & mydata$Counties <= 225], 2, replace = FALSE)
stratum46=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 226 & mydata$Counties <= 229], 2, replace = FALSE)
stratum47=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 230 & mydata$Counties <= 233], 2, replace = FALSE)
stratum48=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 234 & mydata$Counties <= 237], 2, replace = FALSE)
stratum49=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 238 & mydata$Counties <= 241], 2, replace = FALSE)
stratum50=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 242 & mydata$Counties <= 245], 2, replace = FALSE)
stratum51=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 246 & mydata$Counties <= 249], 2, replace = FALSE)
stratum52=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 250 & mydata$Counties <= 253], 2, replace = FALSE)
stratum53=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 254 & mydata$Counties <= 257], 2, replace = FALSE)
stratum54=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 258 & mydata$Counties <= 261], 2, replace = FALSE)
stratum55=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 262 & mydata$Counties <= 265], 2, replace = FALSE)
stratum56=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 269], 2, replace = FALSE)
stratum57=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 270& mydata$Counties <=  273], 2, replace = FALSE)
stratum58=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 274 & mydata$Counties <= 277], 2, replace = FALSE)
stratum59=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 278 & mydata$Counties <= 281], 2, replace = FALSE)
stratum60=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 282 & mydata$Counties <= 285], 2, replace = FALSE)
stratum61=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 286 & mydata$Counties <= 289], 2, replace = FALSE)
stratum62=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 290 & mydata$Counties <= 293], 2, replace = FALSE)
stratum63=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 294 & mydata$Counties <= 297], 2, replace = FALSE)
stratum64=sample(mydata$BreastCancer_Mortality[mydata$Counties >= 298 & mydata$Counties <= 301], 2, replace = FALSE)
stratum <- list(stratum1, stratum2, stratum3, stratum4, stratum5, stratum6, stratum7, stratum8, stratum9, stratum10, stratum11, stratum12, stratum13, stratum14, stratum15, stratum16, stratum17, stratum18, stratum19, stratum20, stratum21, stratum22, stratum23, stratum24, stratum25, stratum26, stratum27, stratum28, stratum29, stratum30, stratum31, stratum32, stratum33, stratum34, stratum35, stratum36, stratum37, stratum38, stratum39, stratum40, stratum41, stratum42, stratum43, stratum44, stratum45, stratum46, stratum47, stratum48, stratum49, stratum50, stratum51, stratum52, stratum53, stratum54, stratum55, stratum56, stratum57, stratum58, stratum59, stratum60, stratum61, stratum62, stratum63, stratum64)


N64=c()
N64[1]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 1 & mydata$Counties <= 5])
N64[2]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 6 & mydata$Counties <= 10])
N64[3]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 11 & mydata$Counties <= 15])
N64[4]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 16 & mydata$Counties <= 20])
N64[5]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 21 & mydata$Counties <= 25])
N64[6]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 26 & mydata$Counties <= 30])
N64[7]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 31 & mydata$Counties <= 35])
N64[8]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 36 & mydata$Counties <= 40])
N64[9]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 41 & mydata$Counties <= 45])
N64[10]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 46 & mydata$Counties <= 50])
N64[11]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 51 & mydata$Counties <= 55])
N64[12]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 56 & mydata$Counties <= 60])
N64[13]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 61 & mydata$Counties <= 65])
N64[14]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 66 & mydata$Counties <= 70])
N64[15]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 71 & mydata$Counties <= 75])
N64[16]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 76 & mydata$Counties <= 80])
N64[17]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 81 & mydata$Counties <= 85])
N64[18]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 86 & mydata$Counties <= 90])
N64[19]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 91 & mydata$Counties <= 95])
N64[20]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 96 & mydata$Counties <= 100])
N64[21]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 101 & mydata$Counties <= 105])
N64[22]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 106 & mydata$Counties <= 110])
N64[23]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 111 & mydata$Counties <= 115])
N64[24]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 116 & mydata$Counties <= 120])
N64[25]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 121& mydata$Counties <= 125])
N64[26]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 126 & mydata$Counties <= 130])
N64[27]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 131 & mydata$Counties <= 135])
N64[28]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 136 & mydata$Counties <= 140])
N64[29]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 141 & mydata$Counties <= 145])
N64[30]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 146 & mydata$Counties <= 150])
N64[31]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 151 & mydata$Counties <= 155])
N64[32]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 156 & mydata$Counties <= 160])
N64[33]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 161 & mydata$Counties <= 165])
N64[34]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 166 & mydata$Counties <= 170])
N64[35]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 171 & mydata$Counties <= 175])
N64[36]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 176 & mydata$Counties <= 180])
N64[37]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 181 & mydata$Counties <= 185])
N64[38]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 186 & mydata$Counties <= 190])
N64[39]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 191 & mydata$Counties <= 195])
N64[40]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 196 & mydata$Counties <= 200])
N64[41]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 201 & mydata$Counties <= 205])
N64[42]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 206 & mydata$Counties <= 210])
N64[43]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 211 & mydata$Counties <= 215])
N64[44]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 216 & mydata$Counties <= 220])
N64[45]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 221 & mydata$Counties <= 225])
N64[46]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 226 & mydata$Counties <= 229])
N64[47]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 230 & mydata$Counties <= 233])
N64[48]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 234 & mydata$Counties <= 237])
N64[49]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 238 & mydata$Counties <= 241])
N64[50]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 242 & mydata$Counties <= 245])
N64[51]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 246 & mydata$Counties <= 249])
N64[52]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 250 & mydata$Counties <= 253])
N64[53]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 254 & mydata$Counties <= 257])
N64[54]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 258 & mydata$Counties <= 261])
N64[55]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 262 & mydata$Counties <= 265])
N64[56]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 266 & mydata$Counties <= 269])
N64[57]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 270& mydata$Counties <=  273])
N64[58]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 274 & mydata$Counties <= 277])
N64[59]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 278 & mydata$Counties <= 281])
N64[60]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 282 & mydata$Counties <= 285])
N64[61]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 286 & mydata$Counties <= 289])
N64[62]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 290 & mydata$Counties <= 293])
N64[63]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 294 & mydata$Counties <= 297])
N64[64]=length(mydata$BreastCancer_Mortality[mydata$Counties >= 298 & mydata$Counties <= 301])
#N64

omega=c()
omega[1]=N64[1]/N
omega[2]=N64[2]/N
omega[3]=N64[3]/N
omega[4]=N64[4]/N
omega[5]=N64[5]/N
omega[6]=N64[6]/N
omega[7]=N64[7]/N
omega[8]=N64[8]/N
omega[9]=N64[9]/N
omega[10]=N64[10]/N
omega[11]=N64[11]/N
omega[12]=N64[12]/N
omega[13]=N64[13]/N
omega[14]=N64[14]/N
omega[15]=N64[15]/N
omega[16]=N64[16]/N
omega[17]=N64[17]/N
omega[18]=N64[18]/N
omega[19]=N64[19]/N
omega[20]=N64[20]/N
omega[21]=N64[21]/N
omega[22]=N64[22]/N
omega[23]=N64[23]/N
omega[24]=N64[24]/N
omega[25]=N64[25]/N
omega[26]=N64[26]/N
omega[27]=N64[27]/N
omega[28]=N64[28]/N
omega[29]=N64[29]/N
omega[30]=N64[30]/N
omega[31]=N64[31]/N
omega[32]=N64[32]/N
omega[33]=N64[33]/N
omega[34]=N64[34]/N
omega[35]=N64[35]/N
omega[36]=N64[36]/N
omega[37]=N64[37]/N
omega[38]=N64[38]/N
omega[39]=N64[39]/N
omega[40]=N64[40]/N
omega[41]=N64[41]/N
omega[42]=N64[42]/N
omega[43]=N64[43]/N
omega[44]=N64[44]/N
omega[45]=N64[45]/N
omega[46]=N64[46]/N
omega[47]=N64[47]/N
omega[48]=N64[48]/N
omega[49]=N64[49]/N
omega[50]=N64[50]/N
omega[51]=N64[51]/N
omega[52]=N64[52]/N
omega[53]=N64[53]/N
omega[54]=N64[54]/N
omega[55]=N64[55]/N
omega[56]=N64[56]/N
omega[57]=N64[57]/N
omega[58]=N64[58]/N
omega[59]=N64[59]/N
omega[60]=N64[60]/N
omega[61]=N64[61]/N
omega[62]=N64[62]/N
omega[63]=N64[63]/N
omega[64]=N64[64]/N
#omega

mean=c()
for(i in 1:k){
  mean[length(mean)+1] <- mean(stratum[[i]])
}
#mean

meanbar = t(omega)%*%mean
#meanbar

sd=c()
for(i in 1:k){
  sd[length(sd)+1] <- sd(stratum[[i]])
}
#sd

sdbar = t(omega)%*%sd
#sdbar

sdsqbar = t(omega)%*%sd^2
#sdsqbar

total=0
for (i in 1:k) {
  total<-total+omega[i]*(mean[i]-meanbar)^2
}
#total

varx=(sdsqbar+total)/n
varx_so=sdbar^2/n
varx_sp=sdsqbar/n
varx#variance of the estimate of the population mean obtained using simple random sampling
varx_sp#variance of the estimate of the population mean obtained using proportional allocation
varx_so#variance of the estimate of the population mean obtained using optimal allocation

