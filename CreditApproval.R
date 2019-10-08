#Using the CreditApproval.csv data set to fit a classification model in order to predict
#whether a given applicant is approved for a credit card (see CreditApproval.txt for an explanation 
#of the data columns). Explore the data graphically and apply logistic regression using various 
#subsets of the predictors. Describe my findings.
# In the exploratory analysis,
# I will try to get an idea what useful variables might be, although here I don't know
# what the variables actually mean. By running the regression with all the variables
# you find out which ones are statistically significant. Finally, I can evaluate
# the performance by measuring the classification error on a testset.
#approval=read.csv("CreditApproval.csv",na.strings="?")
rm(list = ls())
CreditApproval <- read.csv("~/Downloads/CreditApproval.csv")
View(CreditApproval)
approval1=na.omit(CreditApproval)
head(approval1)
str(approval1)
credit.no=subset(approval1,X.=="+")
credit.yes=subset(approval1,X.=="-")
plot(credit.no$X0,credit.no$X1.25,col="blue")
points(credit.yes$X0,credit.yes$X1.25,col="red")
plot(approval1$X.,approval1$X0)
plot(approval1$X.,approval1$X1.25)

credit.lreg=glm(X. ~ X0+X1.25+X0.1+X00202+X30.83,family=binomial,data=approval1)
summary(credit.lreg)
credit.lreg=glm(X. ~ X1.25+X0.1,family=binomial,data=approval1)
summary(credit.lreg)

testindex=sample(1:652,300)
credit.train=approval1[-testindex,]
credit.test=approval1[testindex,]
credit.lreg=glm(X. ~ X1.25+X0.1,family=binomial,data=credit.train)
testprob=predict(credit.lreg,credit.test,type="response")
testpred=rep(0,300)
testpred[testprob>0.5]=1
testval=rep(0,300)
testval[credit.test$X.=="+"]=1
table(testval,testpred)

        #testpred
#testval   0   1
      # 0 139  12
      # 1  85  64