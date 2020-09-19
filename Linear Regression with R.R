install.packages("Metrics")
install.packages("fastDummies")
install.packages("corrr")
install.packages("HH")

library(Metrics)# metrics for regression
library(fastDummies)# for dummy variable
library(corrr)# to check correlation
library(HH)# find VIF

df <-read.csv("Sales of Orange at Park and Beach.csv",sep = ",")

head(df)
str(df)

dum <- dummy_cols(df$Location,remove_first_dummy = TRUE)
dum
df_new<-cbind(df,dum)
df_new
is.na.data.frame(df_new) #check for NA in dataframe.
#line 21 of price has a True, which means missing data
mean(df_new$Price,na.rm = TRUE) #mean of the price, excluding na.

#insert the mean value to the NA place
df_new$Price[is.na(df_new$Price)] <- 0.35
df_new

df_new<-df_new[-c(1:2, 8)] #remove Date, Location and .data
df_new
#  use this code.Pairwise will skip NA obs & use rows with data
dfcor<-cor(df_new,use = "pairwise.complete.obs")
dfcor

set.seed(50)

#get training and test data
split_data=sample(1:nrow(df_new),0.7*nrow(df_new))
#70% under train
train = df_new[split_data,]
#30% under test
test = df_new[-split_data,]

head(train)
head(test)

df_new.lm<-lm(Qty~., data = train)
summary(df_new.lm)

df_new.lm<-lm(Qty~Orange+Price, data = train)
summary(df_new.lm)

names(df_new.lm) # to check objects in the linear model (lm)
coef(df_new.lm) # Estimate coefficient of the linear model
confint(df_new.lm) # confidence interval to check values fall in between
fitted(df_new.lm) #predicated value
residuals(df_new.lm) #observed - predicted
formula(df_new.lm)
anova(df_new.lm)
vif(df_new.lm)#check for multicollinearity, should not more than 10

prediction <-c("Orange","Price")
predict_result <- predict.lm(df_new.lm, test[,prediction], type="response")
test['predicted_Qty'] <- predict_result
test

#plot graph
plot(df_new.lm)
residual.plots(df_new.lm)# to see residual plots
plot(formula(df_new.lm),data = df_new)# to see regress line
