#problem1
# parametric procedure
library(MASS)
data(chickwts)
meatmeal=chickwts[chickwts[,2]=="meatmeal",1]
casein=chickwts[chickwts[,2]=="casein",1]
shapiro.test(meatmeal)
shapiro.test(casein)
var.test(meatmeal,casein)
t.test(meatmeal,casein,var.equal=T)
#non-parametric
wilcox.test(meatmeal,casein)

#c)re-sampling procedure
set.seed(1)
casein.bar= casein + mean(meatmeal) - mean(casein)
z.obs=(mean(meatmeal)-mean(casein))/sqrt(var(meatmeal)/length(meatmeal)+var(casein)/length(casein))
z.temp=c()
for(i in 1:1000){
  meatmeal.temp=sample(meatmeal,length(meatmeal),replace=T)
  casein.temp=sample(casein.bar,length(casein),replace=T)
  z.temp[i]=(mean(meatmeal.temp)-mean(casein.temp))/sqrt(var(meatmeal.temp)/length(meatmeal)+var(casein.temp)/length(casein))
}
p.value=sum(abs(z.temp)>=abs(z.obs))/1000
p.value


#problem2
soybean=chickwts[chickwts[,2]=="soybean",1]
sunflower=chickwts[chickwts[,2]=="sunflower",1]
library(bootstrap)
median.soy=bootstrap(soybean,1000,median)
median.sun=bootstrap(sunflower,1000,median)
quantile(abs(median.soy$thetastar-median.sun$thetastar),c(0.025,0.975))

#2
var.soy= bootstrap(soybean,1000,var)
var.sun = bootstrap(sunflower,1000,var)
quantile(var.soy$thetastar/var.sun$thetastar,c(0.025,0.975))


#3
shapiro.test(soybean)
shapiro.test(sunflower)
var.test(soybean,sunflower)$conf.int

#Problem 3
n=length(meatmeal)+length(soybean) # total number
low.m=sum(meatmeal<258) 
low.s=sum(soybean<258)
p=(low.m+low.s)/n
min(p*length(meatmeal),(1-p)*length(meatmeal),p*length(soybean),(1-p)*length(soybean))

prop.test(c(low.m,low.s),c(length(meatmeal),length(soybean)))
prop.test(c(low.m,low.s),c(length(meatmeal),length(soybean)))$conf.int