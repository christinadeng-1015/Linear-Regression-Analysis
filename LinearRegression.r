# ---------> complete and run the following code for this assignment <-------
##
R code for STA302 or STA1001H1F assignment 1
# copyright by Christina (Qi) Deng
# date: Oct. 04, 2016
#
## Load in the data set
brain = read.table("/Users/christinadeng/Desktop/Fall 2016/STA 302/Assignemnt /Assignment 1/BrainData.csv",sep=" ",## create an indicator for high-IQ (value =1) and low-IQ (value=0)
highIQ = ifelse(brain$FSIQ>=130,1, 0)
## Q1: t-test on MRI count between high- and low IQ groups
# subsets by high and low intelligence group (IG)
highIG_dat <- subset(brain, highIQ==1)
lowIG_dat <- subset(brain, highIQ==0)
# mean MRI count between the high and low IG.
t.test(highIG_dat$MRIcount, lowIG_dat$MRIcount);
## Q2: correlation analysis
# cor.test() : missing value is suppressed, default setting:
# mu = 0, alternative = c("two.sided"), paired = FALSE,var.equal = FALSE
# - find correlation between MRI count and 3 IQ variables
cor.test(brain[,2], brain[,7])
cor.test(brain[,3], brain[,7])
cor.test(brain[,4], brain[,7])
# - find correlation between MRI count and 3 IQ variables in high-IQ group
cor.test(highIG_dat[,2], highIG_dat[,7])
cor.test(highIG_dat[,3], highIG_dat[,7])
cor.test(highIG_dat[,4], highIG_dat[,7])
# - find correlation between MRI count and 3 IQ variables in low-IQ group
cor.test(lowIG_dat[,2], lowIG_dat[,7])
cor.test(lowIG_dat[,3], lowIG_dat[,7])
cor.test(lowIG_dat[,4], lowIG_dat[,7])
## Q4:
# - Scatterplot of PIQ vs MRI count
plot(brain$MRIcount, brain$PIQ, main ="PIQ vs MRI count", xlab="MRIcount", ylab = "PIQ");
abline(lm(brain$PIQ~brain$MRIcount));
# - find R-square, b0, b1, MSE and p-value for b1 in high-IQ group
smart <- lm(highIG_dat$PIQ~highIG_dat$MRIcount)
summary(smart)
(summary(smart)$sigma)^2
# - find R-square, b0, b1, MSE and p-value for b1 in low-IQ group
nonsmart <- lm(lowIG_dat$PIQ~lowIG_dat$MRIcount)
summary (nonsmart)
(summary(nonsmart)$sigma)^2

# ---------> complete and run the following code for this assignment <-------
###
R code for STA302 or STA1001H1F assignment 2
# copyright by Christina Deng
# date: Nov. 6, 2016
## Load in the data set
a2 = read.table("/Users/christinadeng/Desktop/Fall 2016/STA 302/Assignemnt /Assignment 2/DataA2.txt",header=T)
## Q1: fit a linear model to FEV on age
mod1 <- lm(a2$fev~a2$age)
## ==> Q1(a) produce the scatter plot (FEV vs Age) and the residual plot with fitted value
# plot the scatter plot and residual plot in one panel
par(mfrow=c(1,2))
# make a scatter plot of the data and add regression line
plot(a2$age,a2$fev, type="p",col="blue",pch=21, main="FEV vs age",
xlab = "age", ylab = "FEV")
abline(mod1,col="red",lty=2)
# to get the residual plot vs fitted value
plot(mod1,which=1)
##==> Q1(b): boxcox transformation
library(MASS)
# get the boxcox plot
boxcox(a2$fev~a2$age)
# find exact value of lambda
a=boxcox(a2$fev~a2$age, lambda=seq(-2, 4, 0.01))
Elambda= a$x[which.max(a$y)]
Elambda
## Q2
# fit a linear model to log(FEV) on age
m2 <- lm(log(fev) ~ age, data = a2)
# plot the scatter plot and residual plot in one panel
par(mfrow=c(1,2))
# make a scatter plot of the data and add regression line
plot(a2$age,log(a2$fev), type="p",col="blue",pch=21, main="log(FEV) vs age", xlab = "age", ylab = "log(FEV)")
abline(m2,col="red",lty=2)
# make a residual plot vs fitted value of mod 2 (m2)
plot(m2, which=1)
# get information on its slope and intercept
summary(m2)
# find 95% CI in untransformed scale
i <- predict(m2, interval = "confidence", newdata = data.frame(age = c(8, 17, 21)))
exp(i)
# find 95% PI in untransformed scale
j <- predict(m2,interval = "prediction", newdata = data.frame(age = c(8, 17, 21)))
exp(j)



# ---------> complete and run the following code for this assignment <-------
###
R code for STA302 or STA1001H1F assignment 3
# copyright by Christina Deng
# date: Nov. 30, 2016
## Load in the data set
a3 <- read.table("/Users/christinadeng/Desktop/Fall 2016/STA 302/Assignemnt /Assignment 3/a3data.txt",sep="",header=str(a3)
is.factor(a3$gender) # TRUE
is.factor(a3$smoke) # FALSE
a3$smoke = as.factor(a3$smoke) # convert smoke to a factor variable
is.factor(a3$smoke) # TRUE
a3$logPlasma <- log(a3$plasma)
head(a3)
## Q1:
# get the p-value of these pairs of variables
Find_Pvalue <- function(x, y, ...) {
horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
vertical <- (par("usr")[3] + par("usr")[4]) / 2;
text(horizontal, vertical, format(cor.test(x,y)$p.value, digits=2))
}
pairs(a3[, c(1,4,6:11,13)], main = "P value plot", pch = 21,
bg = c("pink","yellow","purple"), upper.panel=Find_Pvalue)
# get the correlation of these pairs of variables
Find_correlation <- function(x, y, ...) {
horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
vertical <- (par("usr")[3] + par("usr")[4]) / 2;
text(horizontal, vertical, format(abs(cor(x,y)), digits=2))
}
pairs(a3[, c(1,4,6:11,13)], main = "Correlations Plot", pch = 21,
bg = c("pink","yellow","purple"), upper.panel=Find_correlation)
# Strong correlation: Calories & Fat (0.9); Calories & CHL(0.67); Fat & CHL(0.71)
# Moderate correlation: Calories & Fiber (0.5); Fiber & DBC (0.48)
## Q2
# Fit the three regression equations
mod1 <- lm(logPlasma ~ Calories, data = a3)
mod2 <- lm(logPlasma ~ Calories + fat, data = a3)
mod3 <- lm(logPlasma ~ Calories + QI, data = a3)
summary(mod1) #p-value:0.2011; coeff of Calories: -8.586e-05
summary(mod2) #p-value:0.002872; coeff of Calories: 0.0003506
summary(mod3) #p-value:-8.252e-05; coeff of Calories: 9.192e-07
## Q3:
# fit the complete modeal
commod <- lm(logPlasma ~ age + factor(gender) + factor(smoke) + QI + factor(Vitamin) + Calories + fat + fiber + alcohol summary(commod)
## Q4
# no predictor in the model
nullmod <- lm(logPlasma ~ 1, data = a3)
# with all predictors in the model
fullmod <- lm(logPlasma ~ age + factor(gender) + factor(smoke) + QI + factor(Vitamin) + Calories + fat + fiber + alcohol # stepwise method: apply both directions method
bothways = step(nullmod ,scope=list(lower=formula(nullmod),upper=formula(fullmod)), direction="both")
formula(bothways)