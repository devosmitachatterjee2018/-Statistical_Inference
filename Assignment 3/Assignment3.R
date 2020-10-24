### MVE155 Statistical Inference
### Assignment 2
### Devosmita Chatterjee

setwd("C:/Users/Acer/Desktop/MVE155/Assignment3")#set current directory
getwd()
mydata=read.csv("fruitfly.txt", quote='\'')
mydata
dim(mydata)

library(reshape2)
mydata$females = as.factor(mydata$females)
mydata$type = as.factor(mydata$type)
type = rep('NA', dim(mydata)[1])
type[mydata$type==0] = 'pregnant'
type[mydata$type==1] = 'virgin'
mydata$type=type
mydata$type = as.factor(mydata$type)

#32a.
library(doBy)
summaryBy(lifespan ~ females + type, data=mydata, FUN = summary)#summary statistics for lifespan in each group 
boxplot(lifespan ~ females + type, data=mydata, main="Boxplots of lifespan")# data in parallel boxplots

#32b.
library(doBy)
summaryBy(sleep ~ females + type, data=mydata, FUN = summary)#summary statistics for sleep in each group 
boxplot(sleep ~ females + type, data=mydata, main="Boxplots of sleep")# data in parallel boxplots

#32c.
plot(mydata$thorax, mydata$lifespan, type='p', pch=20, xlab='thorax', ylab='lifespan', main="Scatterplot of lifespan versus thorax length")
grid()#scatterplot of lifespan versus thorax length

summary(lm(lifespan ~ thorax, data=mydata))#Is thorax length predictive of lifespan?

boxplot(thorax ~ females + type, data=mydata, main="Boxplots of thorax")# did the randomization balance thorax length between the groups?

#32d.
fit = aov(lifespan ~ females + type, data=mydata)
summary(fit)#F test to test for differences in longevity between the groups

interaction.plot(mydata$females, mydata$type, mydata$lifespan, main='Interaction between type and females' )

TukeyHSD(fit)#Tukey's method

#Bonferroni method to compare all pairs of means
sp = sd(mydata$lifespan) # pooled standard deviation
group_means = aggregate(lifespan ~ type, data=mydata, mean)
so = order(group_means$lifespan, decreasing=TRUE)
group_means = group_means[so,] 
group_means

alpha = 0.1
I = 3 # the number of groups
k = choose(I, 2) # the number of pairs for comparisons

gp_NA = mydata[mydata$type =='NA', ]$lifespan
gp_pregnant = mydata[mydata$type =='pregnant', ]$lifespan
gp_virgin = mydata[mydata$type =='virgin', ]$lifespan

alpha_bonferroni = alpha/k # the reduced alpha

t.test(gp_NA, gp_pregnant, conf.level=1-alpha_bonferroni)
t.test(gp_NA, gp_virgin, conf.level=1-alpha_bonferroni)
t.test(gp_pregnant, gp_virgin, conf.level=1-alpha_bonferroni)

#32e.
kruskal.test(lifespan ~ type, data=mydata) #Kruskal-Wallis test

#32f.
library(doBy)
summaryBy(sleep ~ type, data=mydata, FUN = summary)#summary statistics for sleep in each group 
boxplot(sleep ~ type, data=mydata, main="Sleep")# data in parallel boxplots

