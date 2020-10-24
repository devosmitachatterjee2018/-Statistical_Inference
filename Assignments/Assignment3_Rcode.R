library(reshape2)
setwd("C:/Users/Acer/Desktop/MVE155/Assignment1")#set current directory
getwd()
DF = read.csv('fruitfly.txt', quote='\'')
dim(DF)
DF$females = as.factor(DF$females)
DF$type = as.factor(DF$type)
type = rep('NA', dim(DF)[1])
type[DF$type==0] = 'pregnant'
type[DF$type==1] = 'virgin'
DF$type = type
DF$type = as.factor(DF$type)

# Part (a):
#
print('number of samples in each group')
print(aggregate(lifespan ~ females + type, data=DF, length))
print('Mean of lifespan in each group')
print(aggregate(lifespan ~ females + type, data=DF, mean))
#postscript("../../WriteUp/Graphics/Chapter12/prob_32_boxplot_lifespan.eps", onefile=FALSE, horizontal=FALSE)
boxplot(lifespan ~ females + type, data=DF)
#dev.off()

# Part (b):
#
print('Mean of sleep in each group')
print(aggregate(sleep ~ females + type, data=DF, mean))
#postscript("../../WriteUp/Graphics/Chapter12/prob_32_boxplot_sleep.eps", onefile=FALSE, horizontal=FALSE)
boxplot(sleep ~ females + type, data=DF)
#dev.off()

fit = aov(lifespan ~ females * type, data=DF)
print(summary(fit))

fit = aov(lifespan ~ females + type, data=DF)
print(summary(fit))


#postscript("../../WriteUp/Graphics/Chapter12/prob_32_interactionplot_sleep.eps", onefile= FALSE, horizontal=FALSE)
interaction.plot(DF$females, DF$type, DF$sleep, type='l', main='Interaction between type and females' )
#dev.off()

# Part (c):
#
#postscript("../../WriteUp/Graphics/Chapter12/prob_32_lifespan_vs_thorax.eps", onefile= FALSE, horizontal=FALSE)
plot(DF$thorax, DF$lifespan, type='p', pch=19, xlab='thorax', ylab='lifespan')
grid()
#dev.off()

print(summary(lm(lifespan ~ thorax, data=DF)))

# Did the experiment properly balance thorax between the groups:
#
fit = aov(thorax ~ females + type, data=DF)
print(summary(fit))

#postscript("../../WriteUp/Graphics/Chapter12/prob_32_boxplot_thorax.eps", onefile=FALSE, horizontal=FALSE)
boxplot(thorax ~ females + type, data=DF)
#dev.off()

# Part (d):
#
fit = aov(lifespan ~ females * type, data=DF)
print(summary(fit))

fit = aov(lifespan ~ females + type, data=DF)
print(summary(fit))

#postscript("../../WriteUp/Graphics/Chapter12/prob_32_interactionplot_lifespan.eps", onefile= FALSE, horizontal=FALSE)
interaction.plot(DF$females, DF$type, DF$lifespan, type='l', main='Interaction between type and females' )
#dev.off()

# A Tukey comparison of means test:
#
print(TukeyHSD(fit))

par(las=2)
par(mar=c(5, 8, 4, 2))
plot(TukeyHSD(fit))

# Implement the Bonferroni computed confidence intervals (Section 11.4.8 EPage 458)
#
sp = sd(DF$lifespan) # the pooled standard deviation
group_means = aggregate(lifespan ~ type, data=DF, mean)
so = order(group_means$lifespan, decreasing=TRUE)
group_means = group_means[so,] # now the means are decreasing
print(group_means)

alpha = 0.1
I = 3 # the number of groups
k = choose(I, 2) # the number of comparisons

gp_NA = DF[DF$type =='NA', ]$lifespan
gp_pregnant = DF[DF$type =='pregnant', ]$lifespan
gp_virgin = DF[DF$type =='virgin', ]$lifespan

alpha_bonferroni = alpha/k # the reduced alpha

t.test(gp_NA, gp_pregnant, conf.level=1-alpha_bonferroni)
t.test(gp_NA, gp_virgin, conf.level=1-alpha_bonferroni)
t.test(gp_pregnant, gp_virgin, conf.level=1-alpha_bonferroni)

# Part (e): A nonparametric test for one-way ANOVA:
#
print(kruskal.test(lifespan ~ type, data=DF))