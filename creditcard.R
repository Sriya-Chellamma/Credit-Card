setwd("C:/Users/Ramachandran/Desktop/Tableau Docs- BBL/credit card")
library(readr)
cc=read_csv("creditcard.csv")
summary(cc)
library(ggplot2)
library(tidymodels)
library(CaTools)
library(baguette)
library(xgboost)
library(corrplot)

cc1=initial_split(cc,prop = .70,strata = Class)
traincc=training(cc1)
testcc=testing(cc1)
table(traincc$Class)
table(testcc$Class)
199029/(199029+335)
85286/(85286+157)

ggplot(traincc,aes(traincc$Time))+geom_histogram()
ggplot(traincc,aes(traincc$Class))+geom_bar()
ggplot(traincc,aes(traincc$Amount))+geom_histogram()
corrplot(cor(traincc),method = 'number')
summary(cc$Time)
boxplot(cc$Time)
cc$Time1=cc$Time/3600
summary(cc$Time1)
ggplot(cc,aes(cc$Class,cc$Time1))+geom_bar(stat = "identity")
cc %>% filter(Class=="1") %>% group_by(cc)

##Model Building
m1=glm(Class~.,family = binomial(),traincc)
m1
summary(m1)
m2=glm(Class~V27+V21+V20+V14+V10+V8+V4,family = binomial(),traincc)
summary(m2)
P1=predict(m2,testcc,type="response")
table(P1,testcc$Class)
head(P1)
pr1=ifelse(P1>0.5,1,0)
head(pr1)
table(pr1,testcc$Class)
traincc$Class=as.factor(traincc$Class)
testcc$Class=as.factor(testcc$Class)

##Decision Tree
cctreespec=decision_tree() %>% set_engine("rpart") %>% set_mode("classification")
tm1=cctreespec %>% fit(Class~.,traincc)
tm1
tp1=predict(tm1,testcc,type = "class")
head(tp1)
tp2=tp1 %>% mutate(true_class=testcc$Class)
head(tp2)
tp3=predict(tm1,testcc,type="prob")
head(tp3)
tp4=tp3 %>% mutate(true_class=testcc$Class)
head(tp4)
roc_curve(tp4,estimate=.pred_0,truth=testcc$Class) %>% autoplot()
auc=roc_auc(tp4,estimate=.pred_0,truth = testcc$Class)
auc
conf_mat(tp4,estimate = .pred_class, true_class)
accuracy(p4,estimate = .pred_class, true_class)

tm2=cctreespec %>% fit(Class~V27+V21+V20+V14+V10+V8+V4,traincc)
tp21=predict(tm2,testcc,type = "class")
head(tp21)
tp22=tp21 %>% mutate(true_class=testcc$Class)
head(tp22)
tp23=predict(tm2,testcc,type="prob")
head(tp23)
tp24=tp23 %>% mutate(true_class=testcc$Class)
head(tp24)
roc_curve(tp24,estimate=.pred_0,truth=testcc$Class) %>% autoplot()
auc2=roc_auc(tp24,estimate=.pred_0,truth = testcc$Class)
auc

tm3=cctreespec %>% fit(Class~V17+V12+V16+V10+V11+V18+V14,traincc)
tp31=predict(tm3,testcc,type = "class")
head(tp31)
tp32=tp31 %>% mutate(true_class=testcc$Class)
head(tp32)
tp33=predict(tm3,testcc,type="prob")
head(tp33)
tp34=tp33 %>% mutate(true_class=testcc$Class)
head(tp34)
roc_curve(tp34,estimate=.pred_0,truth=testcc$Class) %>% autoplot()
auc3=roc_auc(tp34,estimate=.pred_0,truth = testcc$Class)
auc

## Random Forest
ccforestspec=bag_tree() %>% set_mode("classification") %>% 
  set_engine("rpart",times=20)
ccforestspec

cfm1=fit(ccforestspec,Class~.,traincc)
cfm1
predcc=predict(cfm1,testcc,type="prob") %>% bind_cols(testcc)
cfp1=predict(cfm1,testcc,type="class")
head(cfp1)
table(cfp1,testcc$Class)
roc_curve(predcc,estimate=.pred_0,truth = Class)
roc_auc(predcc,estimate = .pred_0,truth = Class)

cfm2=fit(ccforestspec,Class~V27+V21+V20+V14+V10+V8+V4,traincc)
cfm2
predcc2=predict(cfm2,testcc,type="prob") %>% bind_cols(testcc)
cfp2=predict(cfm2,testcc,type="class")
head(cfp2)
table(cfp2,testcc$Class)
roc_curve(predcc2,estimate=.pred_0,truth = Class)
roc_auc(predcc2,estimate = .pred_0,truth = Class)

cfm3=fit(ccforestspec,Class~V17+V12+V16+V10+V11+V18+V14,traincc)
cfm3
predcc3=predict(cfm3,testcc,type="prob") %>% bind_cols(testcc)
cfp3=predict(cfm3,testcc,type="class")
head(cfp3)
table(cfp3,testcc$Class)
roc_curve(predcc3,estimate=.pred_0,truth = Class)
roc_auc(predcc3,estimate = .pred_0,truth = Class)

##Boosting
boostspec= boost_tree()%>% set_mode("classification")%>%  set_engine("xgboost")

bm1=fit(boostspec,Class~.,traincc)
bm1
bp1=predict(bm1,testcc,type="prob") %>% bind_cols(testcc)
roc_curve(bp1,estimate = .pred_0,truth = Class) %>% autoplot()
roc_auc(bp1,estimate = .pred_0, truth = Class)


bm2=fit(boostspec,Class~V27+V21+V20+V14+V10+V8+V4,traincc)
bm2
bp2=predict(bm2,testcc,type="prob") %>% bind_cols(testcc)
roc_curve(bp2,estimate = .pred_0,truth = Class) %>% autoplot()
roc_auc(bp2,estimate = .pred_0, truth = Class)

bm3=fit(boostspec,Class~V17+V12+V16+V10+V11+V18+V14,traincc)
bm3
bp3=predict(bm3,testcc,type="prob") %>% bind_cols(testcc)
roc_curve(bp3,estimate = .pred_0,truth = Class) %>% autoplot()
roc_auc(bp3,estimate = .pred_0, truth = Class)


