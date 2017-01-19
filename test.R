install.packages("systemfit");
install.packages('ROCR');
install.packages('foreign');
library(foreign)
library(systemfit)
library(ROCR);
setwd('/Users/fiona/RWorkspace/traffic');
#read data
data <- read.csv('testData1.csv');
crashData <- data[data$Crash == 1,];
write.csv(crashData, file = "test2.csv", row.names = FALSE);
crashData <- read.csv('test2.csv')
#get train data and test data
set.seed(22);
prctSample=0.8;
numRow=nrow(data);
rowIndex=sort(sample(numRow,numRow*prctSample));
trainCrash=data[rowIndex,];
#crashData=trainCrash$Crash == 1;
#trainType=trainCrash[crashData,]
test=data[-rowIndex,];

#GLM model crash/no-crahs
r <- Crash~SSD1+SSC1+ASC1+SFC1+TFC1+ASU2;
mglmCrash <- glm(r,family = binomial(link = "logit"),data = trainCrash);

#test GLM
testCrash=test$Crash;
pglmCrash <- predict(mglmCrash,newdata=test,type='response');
predCrash <- prediction(pglmCrash, testCrash);

perfCrash <- performance(predCrash, "tpr", "fpr" );
aucCrash <- performance(predCrash, "auc" );
plot(perfCrash);
aucCrash;

test2 <- test[pglmCrash > 0.45,];

#SUR model
r1 <- Crash1~SSD1+SSC1+ASC1+SFC1+TFC1+ASU2;
r2 <- Crash2~SSD1+SSC1+ASC1+SFC1+TFC1+ASU2;
fitsur <- systemfit(list(followreg = r1, sidereg = r2), 'SUR', data=crashData);
summary(fitsur);

#GLM model
#mglmfolw <- glm(r1,family=binomial(link="logit"),data=train);
#mglmside <- glm(r2,family=binomial(link="logit"),data=train);

#ROC
#SUR test and ROC
pSUR<-predict(fitsur,newdata=test2,type='response');
crash1=test2$Crash1;
crash2=test2$Crash2;
predfollowing <- prediction(pSUR$followreg.pred, crash1);
predside <- prediction(pSUR$sidereg.pred, crash2);
perffol1 <- performance(predfollowing, "tpr", "fpr" );
perfside1 <- performance(predside, "tpr", "fpr");
aucfol1 <- performance(predfollowing, "auc" );
aucside1 <- performance(predside, "auc" );

#GLM test and ROC
#pglmfolw <- predict(mglmfolw,newdata=test,type='response');
#pglmside <- predict(mglmside,newdata=test,type='response');

#predfollowing2 <- prediction(pglmfolw, crash1);
#predside2 <- prediction(pglmside, crash2);
#perffol2 <- performance(predfollowing2, "tpr", "fpr" );
#perfside2 <- performance(predside2, "tpr", "fpr");
#aucfol2 <- performance(predfollowing2, "auc" );
#aucside2 <- performance(predside2, "auc" )

plot(perffol1);
#SUR following AUC=0.6216216
aucfol1;
plot(perfside1);
#SUR side AUC=0.7536765
aucside1;

plot(perffol2);
#GLM following AUC=0.6
aucfol2;
plot(perfside2);
#GLM side AUC=0.8088235
aucside2
