library(dplyr)
library(ROSE)
library(car)
library(ROCR)
library(ggplot2)
library(caret)



train = read.csv('product_train.csv',stringsAsFactors = FALSE)

str(train)

sum(is.na(train))

table(dtrain$went_on_backorder)

dtrain <- ovun.sample(went_on_backorder~., data=train,p=0.5, seed=1,method="over")$data


table(dtrain$potential_issue)

dtrain=dtrain%>%
  mutate(potential_issue=as.numeric(potential_issue=="Yes"))


table(dtrain$deck_risk)

dtrain=dtrain%>%
  mutate(deck_risk=as.numeric(deck_risk=="Yes"))


table(dtrain$oe_constraint)

dtrain=dtrain%>%
  mutate(oe_constraint=as.numeric(oe_constraint=="Yes"))


table(dtrain$ppap_risk)

dtrain=dtrain%>%
  mutate(ppap_risk=as.numeric(ppap_risk=="Yes"))


table(dtrain$stop_auto_buy)

dtrain=dtrain%>%
  mutate(stop_auto_buy=as.numeric(stop_auto_buy=="Yes"))


table(dtrain$rev_stop)

dtrain=dtrain%>%
  mutate(rev_stop=as.numeric(rev_stop=="Yes"))


table(dtrain$went_on_backorder)

dtrain=dtrain%>%
  mutate(went_on_backorder=as.numeric(went_on_backorder=="Yes"))


sum(is.na(dtrain))

str(dtrain)

boxplot(dtrain$national_inv)
summary(dtrain$national_inv)
ul <- 22 + 1.5*(22 - 1)
dtrain$national_inv <- ifelse(dtrain$national_inv > ul, ul, dtrain$national_inv)
boxplot(dtrain$national_inv)
ll <- 1 - 1.5*(22-1)
dtrain$national_inv <- ifelse(dtrain$national_inv < ll, ll, dtrain$national_inv)
boxplot(dtrain$national_inv)


boxplot(dtrain$lead_time)
summary(dtrain$lead_time)
ul <- 8 + 1.5*(8 - 2)
dtrain$lead_time <- ifelse(dtrain$lead_time > ul, ul, dtrain$lead_time)
boxplot(dtrain$lead_time)


boxplot(dtrain$in_transit_qty)
summary(dtrain$in_transit_qty)


boxplot(dtrain$forecast_3_month)
summary(dtrain$forecast_3_month)
ul <- 24 + 1.5*(24 - 0)
dtrain$forecast_3_month <- ifelse(dtrain$forecast_3_month > ul, ul, dtrain$forecast_3_month)
boxplot(dtrain$forecast_3_month)


boxplot(dtrain$forecast_6_month)
summary(dtrain$forecast_6_month)
ul <- 42 + 1.5*(42 - 0)
dtrain$forecast_6_month <- ifelse(dtrain$forecast_6_month > ul, ul, dtrain$forecast_6_month)
boxplot(dtrain$forecast_6_month)


boxplot(dtrain$forecast_9_month)
summary(dtrain$forecast_9_month)
ul <- 60 + 1.5*(60 - 0)
dtrain$forecast_9_month <- ifelse(dtrain$forecast_9_month > ul, ul, dtrain$forecast_9_month)
boxplot(dtrain$forecast_9_month)


boxplot(dtrain$sales_1_month)
summary(dtrain$sales_1_month)
ul <- 7 + 1.5*(7 - 0)
dtrain$sales_1_month <- ifelse(dtrain$sales_1_month > ul, ul, dtrain$sales_1_month)
boxplot(dtrain$sales_1_month)


boxplot(dtrain$sales_3_month)
summary(dtrain$sales_3_month)
ul <- 21 + 1.5*(21 - 0)
dtrain$sales_3_month <- ifelse(dtrain$sales_3_month > ul, ul, dtrain$sales_3_month)
boxplot(dtrain$sales_3_month)


boxplot(dtrain$sales_6_month)
summary(dtrain$sales_6_month)
ul <- 38 + 1.5*(38 - 1)
dtrain$sales_6_month <- ifelse(dtrain$sales_6_month > ul, ul, dtrain$sales_6_month)
boxplot(dtrain$sales_6_month)


boxplot(dtrain$sales_9_month)
summary(dtrain$sales_9_month)
ul <- 55 + 1.5*(55 - 1)
dtrain$sales_9_month <- ifelse(dtrain$sales_9_month > ul, ul, dtrain$sales_9_month)
boxplot(dtrain$sales_9_month)


boxplot(dtrain$min_bank)
summary(dtrain$min_bank)
ul <- 3 + 1.5*(3 - 0)
dtrain$min_bank <- ifelse(dtrain$min_bank > ul, ul, dtrain$min_bank)
boxplot(dtrain$min_bank)




for_vif <- lm(went_on_backorder~.-sales_6_month-forecast_6_month-sales_3_month-perf_12_month_avg-forecast_9_month-sales_9_month-pieces_past_due
              ,data = dtrain)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

fit=glm(went_on_backorder~.-sales_6_month-forecast_6_month-sales_3_month-perf_12_month_avg-forecast_9_month-sales_9_month
        -pieces_past_due-oe_constraint,family= "binomial",data = dtrain)

fit=step(fit)

summary(fit)

formula(fit)

y_pred = predict(fit,newdata=dtrain,type="response")
ggplot(dtrain,aes(x=y_pred,y=went_on_backorder,color=factor(went_on_backorder)))+geom_point()+geom_jitter()


ROCRPred <- prediction(y_pred, dtrain$went_on_backorder)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(.1, by = 0.1))
abline(a=0, b=1)

res <- predict(fit, dtrain, type = "response")

PredictedValue <- res>.5

head(PredictedValue)
pv <- as.numeric(PredictedValue)

head(pv)
pv <- as.factor(pv)
dtrain$went_on_backorder <- as.factor(dtrain$went_on_backorder)
confusionMatrix(pv, dtrain$went_on_backorder)










####test data ##############

dtest= read.csv('product_test.csv',stringsAsFactors = FALSE)




dtest=dtest%>%
  mutate(potential_issue=as.numeric(potential_issue=="Yes"))



dtest=dtest%>%
  mutate(deck_risk=as.numeric(deck_risk=="Yes"))



dtest=dtest%>%
  mutate(oe_constraint=as.numeric(oe_constraint=="Yes"))


dtest=dtest%>%
  mutate(ppap_risk=as.numeric(ppap_risk=="Yes"))


dtest=dtest%>%
  mutate(stop_auto_buy=as.numeric(stop_auto_buy=="Yes"))


dtest=dtest%>%
  mutate(rev_stop=as.numeric(rev_stop=="Yes"))



sum(is.na(dtest))

str(dtest)

res <- predict(fit, dtest, type = "response")

PredictedValue <- res>.5
pv <- as.numeric(PredictedValue)
head(pv)
table(pv)



mydata = data.frame("sku"=dtest$sku,"went_on_backorder"=pv)


write.csv(mydata, file = "Prediction.csv")
