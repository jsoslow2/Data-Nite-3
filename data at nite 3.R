

knitr::opts_chunk$set(echo = TRUE, results = "hide")
if(!require('pacman')) {
  install.packages('pacman')
}
pacman::p_load(ISLR, leaps, car, tidyverse, GGally, reshape2)

#Create Variable for the table in ISLR
Hitters <- Hitters

#quick look at dimensions
dim(Hitters)

#Variable names
names(Hitters)

#Find abnormality in data 
str(Hitters)
summary(Hitters)

#See if there are any NAs
sum(is.na(Hitters))
Hitters

#We have 59 missing values. (Note that some missing data might not be coded as NA)  
apply(Hitters, 2, function(x) any(is.na(x)))  # apply function by columns: column-wise missing 
apply(Hitters, 1, function(x) any(is.na(x)))  # apply function by rows: row-wise missing

#It looks like all our missing values are for the Salary variable.
sum(is.na(Hitters$Salary))

#The players with NAs
NaHitters <- Hitters[is.na(Hitters$Salary), ]

#No clear reason why salary is missing


#In this case we will omit these players
data.comp <- na.omit(Hitters)


#We keep 263 players out of the original 322
dim(data.comp)



#Lets now take a quick look at the pairwise relationship.  We might spot some proper transformations for predicted or predictor variables.

#First, we will use dplyr to filter the non-numeric columns.  Then we will feed the output into our ggpairs function to produce the paired scatter-plots.  **WARNING: Running the pairwise scatter plots will take a while if you have many variables.**


data.comp %>%
select_if(is.numeric) %>%
ggpairs()


#Lets save the numeric columns for future use.

name.num <- sapply(data.comp, is.numeric)


#It's hard to recognize anything in this graph!  Let's try it with fewer variables.
data.comp %>%
  select_if(is.numeric) %>%
  select(Salary, AtBat, Hits, HmRun, Runs, RBI) %>%
  ggpairs()  # base pair-wise scatter plots


#Once again we can display the correlation table through a heat-map. 
#Lets now use a correlation heat-map.
data.comp %>%
select_if(is.numeric) %>%
qplot(x = Var1,
y = Var2,
data = melt(cor(
data.comp %>%
select_if(is.numeric))),
fill = value,
geom = "tile") +
xlab("") +
ylab("") +
guides(fill = guide_legend(title = "Correlation")) +
theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Histogram of Salary
ggplot(data.comp, aes(x = Salary)) +
  geom_histogram(binwidth = 40) +
  ggtitle("Histogram of Salary") +
  ylab("Frequency")

#Salary's distribution is not normal


#Log Salary to create a better prediction
data1 <- cbind(log(data.comp$Salary), data.comp)

#Change name of column
colnames(data1)[1] <- "logSalary"


#Density Plot of Log Salary
ggplot(data1, aes(x = logSalary)) +
  geom_density() +
  ggtitle("Density Plot of logSalary") +
  ylab("Frequency")

#Log Salary is much more normal. Ready for prediction

#Remove Salary for prediction
data2 <- data1[, -20]


#Run Huge Model
fit.all <- lm(logSalary ~., data = data2)
summary(fit.all)  

#The huge model is accurate, but not interpretable. There are way too many variables.

#Prepare Data for Lasso
Y <- data2[, 1] # extract Y
X.fl <- model.matrix(logSalary~., data=data2)[, -1]

#Run Lasso regression
fit.fl.lambda <- glmnet(X.fl, Y, alpha=1, lambda = 10) 

#Find Variables of Lasso Regression
fit.fl.cv <- cv.glmnet(X.fl, Y, alpha=1, nfolds=10 ) 
coef.min <- coef(fit.fl.cv, s="lambda.1se")  #s=c("lambda.1se","lambda.min") or lambda value
coef.min <- coef.min[which(coef.min !=0),]   # get the non=zero coefficients
coef.min  # the set of predictors chosen


#Plug variables into a regression
var.min <- rownames(as.matrix(coef.min)) # output the names
lm.input <- as.formula(paste("logSalary", "~", paste(var.min[-1], collapse = "+"))) # prepare for lm fomulae
lm.input

#Run Regression
fit.min.lm <- lm(lm.input, data=data1)
summary(fit.min.lm) 


#Test Accuracy
accuracyframe <- data1
accuracyframe$predictions <- predict(fit.min.lm, data1)
summary(lm(accuracyframe$predictions ~ accuracyframe$logSalary))
ggplot(accuracyframe, aes(logSalary, predictions)) + geom_point(aes(colour = CRBI))

#Find Most Underpaid and Overpaid players
accuracyframe$predictedSalary <- exp(accuracyframe$predictions)
accuracyframe$residuals <- accuracyframe$predictedSalary - accuracyframe$Salary
hist(accuracyframe$residuals)
accuracyframe <- accuracyframe[order(accuracyframe$residuals),] 

head(accuracyframe, n = 15)
#Mike Schmidt is the most overpaid player

tail(accuracyframe, n = 15)
#Pete Rose is the most underpaid player


plot(fit.min.lm,1)
plot(fit.min.lm,2)
