#2015 Risk Terrain Modeling
setwd("D:/Penn/2015-2016/Fall/590-GIS/Homework/HW10_Risk_Terrain_Modeling/Data")
risk2 <- read.csv("RTM.txt")

library(QuantPsyc)
library(boot)
library(MASS)
library(ggplot2)

#to take logs
#first create a new data frame without the count field
riskNew <- risk2[,5:9]
write.csv(riskNew,"D:/Penn/2015-2016/Fall/590-GIS/Homework/HW10_Risk_Terrain_Modeling/Data/risknew.csv")
#Then import the new log+1 file, name it as riskNew, then do the following

#the below line loops through riskNew and takes the log of each variable and creates a new data frame
risk3 <- as.data.frame(lapply(riskNew, function(x) log(x)))
#was the log transformation complete?
head(risk3)
head(risk2)
#change the column headings of risk3 to show that they're logs
colnames(risk3) <- paste("log", colnames(risk3), sep = "")
head(risk3)
#now join your new log variables back to the original risk2 data frame
risk4 <- cbind(risk2, risk3)
head(risk4)
#finally get rid of any '-Inf' that might have been created by taking log of 0
risk4 <- do.call(data.frame,lapply(risk4, function(x) replace(x, is.infinite(x),0)))

#now run your regressions
#get rid of scientific notation in R which will help us better understand our regression coefficients
options(scipen=999)

#Let's take a look at the distribution of burglaries
hist(risk4$Count_)#name of this distribution:Poisson Distribution. Dont use OLS regression on this type
#of count distribution
hist(log(risk4$Count_))

#how many 0s do we have?
#the below two lines dont work in this homework, it works in the in class task
nrow(risk[ which(risk$burgRate == 0), ])
nrow(risk[ which(risk$burgRate == 0), ])  / nrow(risk)

#select out the columns we need
risk5 <- risk4[,c(4,10:14)]

#estimate
reg <- glm(Count_ ~ ., family = "poisson", data=risk5)
summary(reg)

#the below two lines makes no sense in this homework, it works in the in class task
exp(-4.088e-04 * 100)
1 - exp(-4.088e-04 * 100)
#for every 100 ft farther from a abandoned building, the number of crimes will fall by 4%. It is on
#average for the whole Chicago, it differs in different neighborhoods

#lets look for all the coefficients
t <- 1 - exp(reg$coefficients * 100)
t

#what do the fitted values tell us?
range(exp(reg$fitted.values))
#" The highest risk cells have an expected rate of burglary that is x times higher
#than a cell with a value of 1.0"

#create some standardized coefficients which scales the coefficients so that you can see which are
#the most important. only relatviely, coz they are only for this model, it's conditional to other
#predictors in the model
standardized <- as.data.frame(lm.beta(reg))
#add the name of the variable
standardized$variable <- row.names(standardized)
colnames(standardized)[1] <- "std_coef"

#plot using ggplot
c <- ggplot(standardized, aes(x=variable, y=std_coef, fill=variable)) + geom_bar(stat="identity")
c

#do it again by taking the absolute value of the std coefficient
c2 <- ggplot(standardized, aes(x=variable, y=abs(std_coef), fill=variable)) + geom_bar(stat="identity")
c2

#now I want to reorder the bars
#create a new absolute value std coeff variable
standardized$absCoef <- abs(standardized$std_coef)
#now plot with the reorder command
c3 <- ggplot(standardized, aes(x=reorder(variable,-absCoef), y=absCoef, fill=variable)) + geom_bar(stat="identity")
c3
