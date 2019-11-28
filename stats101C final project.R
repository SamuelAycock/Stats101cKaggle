training <- read.csv('train.csv')
test <- read.csv('test.csv')

y <- training$HTWins
y <- ifelse(y == "Yes",T,F)

x <- training[,-c(1,2,3,4,5,6,8,which(duplicated(t(training[1,]))))]

createsol <- function(name,model){
  probs <- predict(model,newdata = test)
  answers <- cbind(test[,1],ifelse(probs > 0.5,"Yes","No"))
  colnames(answers) <- c("id","HTWins")
  write.csv(answers,name,row.names = F)
}



#submission1 first 0 effort GLM with all variables. test accuracy = 0.655 ranked -50 percentile
model1 <- glm(y~., data = x)
summary(model1)
createsol("submission1.csv",model1)

#submission2. only points allowed/scored (cheese didnt work) accuract 0.6009

model2 <- glm(y~VT.TA.pts+VT.TS.pts,data=x)

createsol("subimssion2.csv",model2)
