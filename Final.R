library(glmnet)
library(pls)
library(dplyr)
library(tidyverse)
library(ROCR)
library(caret)
library(ggplot2)
library(visreg)
library(leaps)
library(MASS)
library(Metrics)
#Table 1
df <- read.csv2('E:/STK-MAT2011/Data/prepared_data.csv', header = T)
df$Y <- ifelse(df$FINAL_CLAIM!=0, 1, 0) #claim paid if != 0 and not paid if = 0
df <- df[df$FINAL_CLAIM>=0, ]
length(df$Y[df$Y==0])
length(df$Y[df$Y==1])
length(df$Y[df$Y==0])/length(df$Y) #Proportion of payed claims to claims made
#mean age for final claim != 0 and = 0
mean(as.numeric(df$AGE_AT_TIME[df$Y==1]))
mean(as.numeric(df$AGE_AT_TIME[df$Y==0]))
mean(as.numeric(df$AGE_AT_TIME))
#driver gender proportion final claim != 0 and = 0
count(df$GENDER_DRIVER[df$Y==1&df$GENDER_DRIVER=='M'])$freq
count(df$GENDER_DRIVER[df$Y==0&df$GENDER_DRIVER=='M'])$freq
count(df$GENDER_DRIVER[df$Y==1&df$GENDER_DRIVER=='K'])$freq
count(df$GENDER_DRIVER[df$Y==0&df$GENDER_DRIVER=='K'])$freq
count(df$GENDER_DRIVER)[2,2]#total observed females
count(df$GENDER_DRIVER)[3,2]#total observed males
#policy holder gender proportion for final claim != 0 and = 0
count(df$GENDER_POLICY_HOLDER[df$Y==1&df$GENDER_POLICY_HOLDER=='M'])$freq
count(df$GENDER_POLICY_HOLDER[df$Y==0&df$GENDER_POLICY_HOLDER=='M'])$freq
count(df$GENDER_POLICY_HOLDER[df$Y==1&df$GENDER_POLICY_HOLDER=='K'])$freq
count(df$GENDER_POLICY_HOLDER[df$Y==0&df$GENDER_POLICY_HOLDER=='K'])$freq
count(df$GENDER_POLICY_HOLDER)[1,2]#total observed females
count(df$GENDER_POLICY_HOLDER)[2,2]#total observed males
#Children under 18 for final claim != 0 and = 0
count(df$CHILDREN_U_18[df$Y==0&df$CHILDREN_U_18==1])$freq
count(df$CHILDREN_U_18[df$Y==1&df$CHILDREN_U_18==1])$freq
count(df$CHILDREN_U_18[df$Y==0&df$CHILDREN_U_18==0])$freq
count(df$CHILDREN_U_18[df$Y==1&df$CHILDREN_U_18==0])$freq
count(df$CHILDREN_U_18)[2,2]#total observed children
count(df$CHILDREN_U_18)[1,2]#total observed no children
#Reporting lag for final claim != 0 and = 0
mean(as.numeric(df$REPORTING_LAG[df$Y==1]))
mean(as.numeric(df$REPORTING_LAG[df$Y==0]))
mean(as.numeric(df$REPORTING_LAG))
#Number of parties for final claim != 0 and = 0
count(df$NUM_PARTIES[df$Y==0&df$NUM_PARTIES==1])$freq
count(df$NUM_PARTIES[df$Y==1&df$NUM_PARTIES==1])$freq
sum(count(df$NUM_PARTIES[df$Y==0&df$NUM_PARTIES>1])$freq)
sum(count(df$NUM_PARTIES[df$Y==1&df$NUM_PARTIES>1])$freq)
count(df$NUM_PARTIES)[1,2]#total observed one party
sum(count(df$NUM_PARTIES)[-1,2])#total observed several parties
##################################################################################
#Figure 1
par(mfrow=c(2,2))
barplot(prop.table(table(GENDER_POLICY_HOLDER)), xlab = 'Gender', ylab = 'Density', main = 'Gender Policy Holder', names.arg=c("Female", "Male"))
barplot(prop.table(table(CHILDREN_U_18)), xlab = 'Children amount', ylab = 'Density', main = 'Children under 18', names.arg=c("Zero children", "One or more children"))
barplot(prop.table(table(df[df$REPORTING_LAG<100, ]$REPORTING_LAG)), xlab = 'Days', ylab = 'Density', main = 'Reporting lag')
barplot(prop.table(table(AGE_AT_TIME)), xlab = 'Age', ylab = 'Density', main = 'Age at time')
##################################################################################
#figure 2
hist(df[df$FINAL_CLAIM>0, ]$FINAL_CLAIM,col="grey", pch=20, cex=4, freq = TRUE, breaks = seq(min(FINAL_CLAIM), max(FINAL_CLAIM),length.out = 100), xlim=c(0,100000),  ylim = c(0,80000), xlab = 'Claim size', ylab = 'Frequency of claims', main = 'Final claims')
grid()
hist(df[df$FINAL_CLAIM>0, ]$FINAL_CLAIM,col="grey", pch=20, cex=4, add = TRUE, freq = TRUE, breaks = seq(min(FINAL_CLAIM), max(FINAL_CLAIM),length.out = 100), xlim=c(0,100000),  ylim = c(0,80000), xlab = 'Claim size', ylab = 'Frequency of claims', main = 'Final claims')
##################################################################################
#figure 3
z <- seq(-7,7,0.01)
sigmoid <- function(z){
  1/(1+exp(-z))
}
plot(z, sigmoid(z), type = 'l', lty = 'dashed', col = 'red', lwd = 2, ylab = 'Sigmoid(z)', xlab = 'z', main = 'Sigmoid function')
grid()
legend(-5,0.9, legend = c('sigmoid'), lty = 'dashed', col = c('red'), lwd = 2, text.font = 4, bg = 'lightblue')
##################################################################################
#misclassification
options(max.print = 1000)
#reset
df <- read.csv2('E:/STK-MAT2011/Data/prepared_data.csv', header = T)
##################################################################################
#Clean up data
df <- df[,!names(df)%in% c("Index")]
df$Y <- ifelse(df$FINAL_CLAIM!=0, 1, 0) #claim paid if != 0 and not paid if = 0
df <- df[df$FINAL_CLAIM>=0, ] #remove values less than 0 for final claim
df <- df[,!names(df)%in% c("FINAL_CLAIM")]
df$GENDER_DRIVER <- as.factor(df$GENDER_DRIVER)
df$AGE_AT_TIME <- as.integer(df$AGE_AT_TIME)
df$GENDER_DRIVER <- as.factor(df$GENDER_DRIVER)
df$COUNTY_NUMBER_POLICY_HOLDER <- as.factor(df$COUNTY_NUMBER_POLICY_HOLDER)
df$GENDER_POLICY_HOLDER <- as.factor(df$GENDER_POLICY_HOLDER)
df$CATEGORY_CODE <- as.factor(df$CATEGORY_CODE)
df$INJURY_CODE <- as.factor(df$INJURY_CODE)
df$ACCIDENT_CODE <- as.factor(df$ACCIDENT_CODE)
df$INCIDENT_MONTH <- as.factor(df$INCIDENT_MONTH)
df$INCIDENT_YEAR <- as.factor(df$INCIDENT_YEAR)
df$INCIDENT_DAY <- as.factor(df$INCIDENT_DAY)
df$TIME_CODE <- as.factor(df$TIME_CODE)
df$COUNTERPART_CODE <- as.factor(df$COUNTERPART_CODE)
df$CHILDREN_U_18 <- as.factor(df$CHILDREN_U_18)
REPORT_LAG <- as.numeric(df$REPORTING_LAG)
df <- df %>% 
  mutate(REPORT_LAG = case_when(
    .$REPORTING_LAG >= 0 & .$REPORTING_LAG <= 7 ~ "1 week",
    .$REPORTING_LAG > 7 & .$REPORTING_LAG <= 14 ~ "2 weeks",
    .$REPORTING_LAG > 14 & .$REPORTING_LAG <= 21 ~ "3 weeks",
    .$REPORTING_LAG > 21 & .$REPORTING_LAG <= 28 ~ "4 weeks",
    .$REPORTING_LAG > 28 & .$REPORTING_LAG <= 60 ~ "2 months",
    .$REPORTING_LAG > 60 ~ "2 months +",
    
    TRUE ~ "other"
  )
  )
df <- df[,!names(df) %in% c("REPORTING_LAG")]
df$REPORT_LAG <- as.factor(df$REPORT_LAG)
#these cause too many factors and are too computational heavy
df <- df[,!names(df) %in% c("MUNICIPALITY_NUMBER_POLICY_HOLDER")]
df <- df[,!names(df) %in% c("MUNICIPALITY_NUMBER_ACCIDENT")]
##################################################################################
#Train and test
smp_size <- floor(0.70 * nrow(df))
set.seed(2)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind,]
test <- df[-train_ind,]
##################################################################################
#Glmnet
fit <- glm(Y~CATEGORY_CODE + INJURY_CODE + ACCIDENT_CODE + NUM_PARTIES + 
             GENDER_DRIVER + AGE_AT_TIME + COUNTERPART_CODE + COUNTY_NUMBER_POLICY_HOLDER + 
             REPORT_LAG, family = 'binomial', data = train)
summary(fit)
##################################################################################
#figure 4
visreg(fit, "AGE_AT_TIME", gg = TRUE, scale = 'response') + 
  labs(title = "Probability of payment respective to age",
       y = "Prob(payment)", x = "Age at time")
##################################################################################
#continuing
pred_test <- predict(fit, newdata = test, type = 'response')
prob_test <- 1/(exp(-pred_test) + 1)
cutoff <- seq(0.4,0.7,by=0.0001)
accuracy <- NULL
for (i in cutoff){
  new_value <- 1 - sum(abs(test$Y != ifelse(1/(exp(-pred_test) + 1) < i, 0, 1)))/nrow(test)
  accuracy <- c(accuracy, new_value)
}
accuracy <- cbind(accuracy, cutoff)
accuracy <- as.data.frame(accuracy)
max_cutoff <- accuracy[which.max(accuracy$accuracy),]$cutoff
max_accuracy <- accuracy[which.max(accuracy$accuracy),]$accuracy
#figure 5
par(mfrow=c(2,2))
plot(accuracy$cutoff, accuracy$accuracy, cex = 0.1, col = 'blue',
     xlab = 'Cutoff', ylab = 'Accuracy')
abline(v = max_cutoff, lty = 'dashed', col = c('darkblue'))
abline(h = max_accuracy, lty = 'dashed', col = c('darkblue'))
#legend(0.66,0.86, legend = c('Accuracy to cutoff', 'Max accuracy'), col = c('blue', 'darkblue'), lty = c('solid', 'dashed'), lwd = 1, text.font = 4, bg = 'lightblue')

ROCR.simple <- as.data.frame(prob_test)
ROCR.simple <- cbind(ROCR.simple, test$Y)

colnames(ROCR.simple) <- c('predictions', 'labels')

pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, 'tpr', 'fpr')
plot(perf, avg = 'threshold', colorize = TRUE, lwd = 3)
abline(0, 1, lty='dashed')

perf1 <- performance(pred, 'prec', 'rec')
plot(perf1, avg = 'threshold', colorize = TRUE, lwd = 3)

perf2 <- performance(pred, 'sens', 'spec')
plot(perf2, avg = 'threshold', colorize = TRUE, lwd = 3)
##################################################################################
#confusion matrix
#table 3

y_test <- ifelse(prob_test < max_cutoff, 0, 1)
test_misclassification <- sum(abs(test$Y != y_test))
accuracy <- confusionMatrix(as.factor(test$Y), reference = as.factor(y_test))$overall[1]
print(accuracy)
confusionMatrix(as.factor(test$Y), reference = as.factor(y_test))
specs <- confusionMatrix(as.factor(test$Y), reference = as.factor(y_test))
specs$byClass[1:2]
##################################################################################
#table 4
#Glmnet
#Train and test
smp_size <- floor(0.70 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind,]
test <- df[-train_ind,]
train_Y <- train$Y
train <- train[,!names(train)%in% c("Y")]
test_Y <- test$Y
test <- test[,!names(test)%in% c("Y")]
train <- model.matrix(~., data = train)
test <- model.matrix(~., data = test)
t <- seq(-8, -2, by = 1)

cv_lasso_fit <- cv.glmnet(x = train, y = train_Y, alpha = 1, nfolds = 48, lambda = exp(t),
                          standarize = TRUE, family = 'binomial')
cv_ridge_fit <- cv.glmnet(x = train, y = train_Y, alpha = 0, nfolds = 48, lambda = exp(t),
                          standarize = TRUE, family = 'binomial')

glm_lasso <- glmnet(x = train, y = train_Y, alpha = 1, lambda = cv_lasso_fit$lambda.min,
                    standarize = TRUE, family = 'binomial')
glm_ridge <- glmnet(x = train, y = train_Y, alpha = 0, lambda = cv_ridge_fit$lambda.min,
                    standarize = TRUE, family = 'binomial')

lasso_pred_test <- predict(glm_lasso, newx = test)
ridge_pred_test <- predict(glm_ridge, newx = test)

prob_lasso_test <- 1/(exp(-lasso_pred_test) + 1)
prob_ridge_test <- 1/(exp(-ridge_pred_test) + 1)

y_lasso_test <- ifelse(prob_lasso_test < 0.6156, 0, 1)
y_ridge_test <- ifelse(prob_ridge_test < 0.6156, 0, 1)

test_misclassification_lasso <- sum(abs(test_Y != y_lasso_test))
test_misclassification_ridge <- sum(abs(test_Y != y_ridge_test))

1 - test_misclassification_lasso/nrow(test)
1 - test_misclassification_ridge/nrow(test)

#confusionMatrix(as.factor(test_Y), reference = as.factor(y_lasso_test))
ok <- confusionMatrix(as.factor(test_Y), reference = as.factor(y_lasso_test))
ok$byClass
#confusionMatrix(as.factor(test_Y), reference = as.factor(y_ridge_test))
ok <- confusionMatrix(as.factor(test_Y), reference = as.factor(y_ridge_test))
ok$byClass
##################################################################################
#table 5 and 6
df <- read.csv2('E:/STK-MAT2011/Data/prepared_data.csv', header = T)
df <- df[,!names(df)%in% c("Index")]
#df <- df[,!names(df)%in% c("MUNICIPALITY_NUMBER_POLICY_HOLDER")]
#kun interessert i positive verdier for final claim
df <- df[df$FINAL_CLAIM>0, ]
#sjekk om det er NA verdier eller negative verdier for final claim
df[df$final_claim<0, ]
df[is.na(df$FINAL_CLAIM)=='TRUE',]
#sørger for at syntax er riktig for ulike variabler
df$GENDER_DRIVER <- as.factor(df$GENDER_DRIVER)
df$AGE_AT_TIME <- as.integer(df$AGE_AT_TIME)
df$GENDER_DRIVER <- as.factor(df$GENDER_DRIVER)
df$COUNTY_NUMBER_POLICY_HOLDER <- as.factor(df$COUNTY_NUMBER_POLICY_HOLDER)
df$MUNICIPALITY_NUMBER_POLICY_HOLDER <- as.factor(df$MUNICIPALITY_NUMBER_POLICY_HOLDER)
df$GENDER_POLICY_HOLDER <- as.factor(df$GENDER_POLICY_HOLDER)
df$CATEGORY_CODE <- as.factor(df$CATEGORY_CODE)
df$INJURY_CODE <- as.factor(df$INJURY_CODE)
df$ACCIDENT_CODE <- as.factor(df$ACCIDENT_CODE)
df$INCIDENT_MONTH <- as.factor(df$INCIDENT_MONTH)
df$INCIDENT_YEAR <- as.factor(df$INCIDENT_YEAR)
df$INCIDENT_DAY <- as.factor(df$INCIDENT_DAY)
df$REPORTING_LAG <- as.integer(df$REPORTING_LAG)
df$MUNICIPALITY_NUMBER_ACCIDENT <- as.factor(df$MUNICIPALITY_NUMBER_ACCIDENT)
df$TIME_CODE <- as.factor(df$TIME_CODE)
df$COUNTERPART_CODE <- as.factor(df$COUNTERPART_CODE)
REPORT_LAG <- as.numeric(df$REPORTING_LAG)
df <- df %>% 
  mutate(REPORT_LAG = case_when(
    .$REPORTING_LAG >= 0 & .$REPORTING_LAG <= 7 ~ "1 week",
    .$REPORTING_LAG > 7 & .$REPORTING_LAG <= 14 ~ "2 weeks",
    .$REPORTING_LAG > 14 & .$REPORTING_LAG <= 21 ~ "3 weeks",
    .$REPORTING_LAG > 21 & .$REPORTING_LAG <= 28 ~ "4 weeks",
    .$REPORTING_LAG > 28 & .$REPORTING_LAG <= 60 ~ "2 months",
    .$REPORTING_LAG > 60 ~ "2 months +",
    TRUE ~ "other"
  )
  )
df$REPORT_LAG <- as.factor(df$REPORT_LAG)
#these cause too many factors and are too computational heavy
df <- df[,!names(df) %in% c("REPORTING_LAG")]
df <- df[,!names(df) %in% c("MUNICIPALITY_NUMBER_POLICY_HOLDER")]
df <- df[,!names(df) %in% c("MUNICIPALITY_NUMBER_ACCIDENT")]
df <- df[,!names(df) %in% c("INCIDENT_YEAR")]
df <- df[,!names(df) %in% c("INCIDENT_DAY")]
df <- df[,!names(df) %in% c("INCIDENT_MONTH")]
##################################################################################
smp_size <- floor(0.70 * nrow(df))
set.seed(2)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind,]
test <- df[-train_ind,]
#offset log(numclaims)?
#gamma
#fit <- glm(FINAL_CLAIM~., data = df, family = gaussian)
#stepwise1 <- stepAIC(fit, direction = 'backward')

fit_gamma <- glm(FINAL_CLAIM~CATEGORY_CODE + INJURY_CODE + ACCIDENT_CODE + NUM_PARTIES + 
                   GENDER_DRIVER + TIME_CODE + COUNTERPART_CODE + GENDER_POLICY_HOLDER + 
                   COUNTY_NUMBER_POLICY_HOLDER + CHILDREN_U_18 + REPORT_LAG, data = train, family = Gamma(link='log'))
test_gamma <- test
test_gamma$pred <- predict(fit_gamma, newdata = test, type = 'response')
#gaussian
fit_gauss <- glm(FINAL_CLAIM~CATEGORY_CODE + INJURY_CODE + ACCIDENT_CODE + NUM_PARTIES + 
                   GENDER_DRIVER + TIME_CODE + COUNTERPART_CODE + GENDER_POLICY_HOLDER + 
                   COUNTY_NUMBER_POLICY_HOLDER + CHILDREN_U_18 + REPORT_LAG, data = train, family = gaussian)
test_gauss <- test
test_gauss$pred <- predict(fit_gauss, newdata = test, type = 'response')
#rmse
RMSE_gamma <- sqrt(mean((test_gamma$pred - test$FINAL_CLAIM)^2))
RMSE_gauss <- sqrt(mean((test_gauss$pred - test$FINAL_CLAIM)^2))
print(c(RMSE_gamma, RMSE_gauss))
print(c(mean(test_gamma$pred-test$FINAL_CLAIM), mean(test_gauss$pred-test$FINAL_CLAIM)))
mean(test$FINAL_CLAIM)
summary(fit_gamma)
summary(fit_gauss)
print(c(mean(test_gamma$pred), mean(test_gauss$pred)))
