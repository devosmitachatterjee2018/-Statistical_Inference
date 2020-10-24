### MVE155 Statistical Inference
### Assignment 2
### Devosmita Chatterjee

setwd("C:/Users/Acer/Desktop/MVE155/Assignment2")#set current directory
getwd()
mydata=read.csv("whales.txt", quote='\'')#read file
colnames(mydata)=c("t")#column name
#mydata
#str(mydata)

#46a.
#Histogram of the 210 values of t_i
hist(mydata$t, breaks=20, probability = T, plot=T, col="red", xlab=expression('t'[i]), main=expression('Histogram of the 210 values of '*'t'[i]))

#46b.
N=length(mydata$t)           
X_bar=mean(mydata$t)
sigma_sq=mean(mydata$t^2) - (mean(mydata$t))^2
alpha_mme=X_bar^2/sigma_sq
lambda_mme=X_bar/sigma_sq
#Fit the parameters of the gamma distribution by the method of moments
alpha_mme
lambda_mme

#46c.
library(fitdistrplus)
para_mle=fitdist(mydata$t, "gamma", method="mle", start = list(shape = 0.7991741, rate = 1.318771))
#para_mle
alpha_mle=para_mle$estimate["shape"]
lambda_mle=para_mle$estimate["rate"]
#Fit the parameters of the gamma distribution by maximum likelihood
alpha_mle
lambda_mle

#46d.
x=seq(0,5,0.01)
y_mme=dgamma(x,alpha_mme,lambda_mme)
y_mle=dgamma(x,alpha_mle,lambda_mle)
#Plot the two gamma densities on top of the histogram
hist(mydata$t, breaks=20, probability = T,plot=T, col="red", xlab=expression('t'[i]), main=expression('Histogram of the 210 values of '*'t'[i]))
lines( sort(x) , y_mme, col = "blue" , lty = 2 , lwd = 2 )
lines( sort(x) , y_mle, col = "green" ,lwd = 2 )
legend("topright", legend=c("MME", "MLE"),
       col=c("blue", "green"), lty=2:1, lwd = 2, cex=1)

#46e.
n=3000
B=1000
y = matrix(0,B,n)

lambda_boot_mme=numeric(B)
alpha_boot_mme=numeric(B)

set.seed(1)
for(i in 1:B){
  y[i,] = rgamma(n,alpha_mme,lambda_mme)
  x_bar = mean(y[i,])
  sigma_sq=mean(y[i,]^2) - (mean(y[i,]))^2
  alpha_boot_mme[i] = x_bar^2/sigma_sq
  lambda_boot_mme[i] = x_bar/sigma_sq
}
#Estimate the sampling distributions and the standard errors of the parameters fit by the method of moments by using the bootstrap
hist(alpha_boot_mme, breaks=15, probability = T, plot=T, col="red") 
alpha_boot_mme_mean=mean(alpha_boot_mme)
alpha_boot_mme_mean
alpha_boot_mme_var=((B-1)/B)*var(alpha_boot_mme)
alpha_boot_mme_sd=sqrt(alpha_boot_mme_var)
alpha_boot_mme_sd

hist(lambda_boot_mme, breaks=15, probability = T, plot=T, col="red") 
lambda_boot_mme_mean=mean(lambda_boot_mme)
lambda_boot_mme_mean
lambda_boot_mme_var=((B-1)/B)*var(lambda_boot_mme)
lambda_boot_mme_sd=sqrt(lambda_boot_mme_var)
lambda_boot_mme_sd

#46f.
n=3000
B=1000
y = matrix(0,B,n)

lambda_boot_mle=numeric(B)
alpha_boot_mle=numeric(B)

set.seed(1)
for(i in 1:B){
  y[i,] = rgamma(n, alpha_mle, lambda_mle)
  para_boot_mle=fitdist(y[i,], "gamma", method="mle", start = list(shape = alpha_boot_mme[i], rate = lambda_boot_mme[i]))
  alpha_boot_mle[i]=para_boot_mle$estimate["shape"]
  lambda_boot_mle[i]=para_boot_mle$estimate["rate"]
}
#Estimate the sampling distributions and the standard errors of the parameters fit by maximum likelihood by using the bootstrap
hist(alpha_boot_mle, breaks=15, probability = T, plot=T, col="red") 
alpha_boot_mle_mean=mean(alpha_boot_mle)
alpha_boot_mle_mean
alpha_boot_mle_var=((B-1)/B)*var(alpha_boot_mle)
alpha_boot_mle_sd=sqrt(alpha_boot_mle_var)
alpha_boot_mle_sd

hist(lambda_boot_mle, breaks=15, probability = T, plot=T, col="red") 
lambda_boot_mle_mean=mean(lambda_boot_mle)
lambda_boot_mle_mean
lambda_boot_mle_var=((B-1)/B)*var(lambda_boot_mle)
lambda_boot_mle_sd=sqrt(lambda_boot_mle_var)
lambda_boot_mle_sd

#46g.
#Approximate confidence intervals for the parameters estimated by maximum likelihood
I_alpha_boot_mle.L=alpha_boot_mle_mean-1.96*alpha_boot_mle_sd
I_alpha_boot_mle.L
I_alpha_boot_mle.U=alpha_boot_mle_mean+1.96*alpha_boot_mle_sd
I_alpha_boot_mle.U

I_lambda_boot_mle.L=lambda_boot_mle_mean-1.96*lambda_boot_mle_sd
I_lambda_boot_mle.L
I_lambda_boot_mle.U=lambda_boot_mle_mean+1.96*lambda_boot_mle_sd
I_lambda_boot_mle.U

#Approximate confidence intervals for the parameters estimated by the method of moments
I_alpha_boot_mme.L=alpha_boot_mme_mean-1.96*alpha_boot_mme_sd
I_alpha_boot_mme.L
I_alpha_boot_mme.U=alpha_boot_mme_mean+1.96*alpha_boot_mme_sd
I_alpha_boot_mme.U

I_lambda_boot_mme.L=lambda_boot_mme_mean-1.96*lambda_boot_mme_sd
I_lambda_boot_mme.L
I_lambda_boot_mme.U=lambda_boot_mme_mean+1.96*lambda_boot_mme_sd
I_lambda_boot_mme.U