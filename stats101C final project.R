training <- read.csv('train.csv')
test <- read.csv('test.csv')

y <- training$HTWins
y <- ifelse(y == "Yes",T,F)

x <- training[,-c(1,2,3,4,5,6,8,which(duplicated(t(training[1,]))))]
testx <- test[,-c(1:7,which(duplicated(t(test[1,]))))]
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

#submission2. only points allowed/scored (cheese didnt work) accuraty 0.6009

model2 <- glm(y~VT.TA.pts+VT.TS.pts,data=x)

createsol("subimssion2.csv",model2)

#submission3 accuracy ~0.58
out <-ifelse((test$VT.TS.pts - test$VT.TA.pts) > 0,"No","Yes")
out <- cbind(test$id,out)
colnames(out) <- c('id','HTWins')
write.csv(out,"submission3.csv",row.names = F)

#submission4 accuracy ~0.58
out <-ifelse(((test$VT.TS.fgm*2 + test$VT.TS.tpm*3) - (test$HT.TS.tpm * 3 + test$HT.TS.fgm * 2)) > 0,"No","Yes")
out <- cbind(test$id,out)
colnames(out) <- c('id','HTWins')
write.csv(out,"submission4.csv",row.names = F)

#bigSTDmat
big <- as.matrix(rbind(matrix(x),matrix(testx)))
library(tidyr)
std <- x %>% apply(2,function(x){(x - mean(x))/sd(x)})

trainSTD <- bigstd[1:9520,]
testSTD <- bigstd[-c(1:9520),]


#submission5 RIDGE 0.618
library(glmnet)
library(tidyr)
lambdas <- 10^seq(3, -2, by = -.1)
model5 <- cv.glmnet(trainSTD,y,alpha = 0, lambda = lambdas)

minl <- model5$lambda.min

outmodel <- glmnet(x2,y2,alpha = 0, lambda = minl,family = 'binomial')
probs <- predict(outmodel,newx = data.matrix(testSTD))
answers <- cbind(test[,1],ifelse(probs > 0.5,"Yes","No"))
colnames(answers) <- c("id","HTWins")
write.csv(answers,"submission5.csv",row.names = F)

#submission6 LASSO 0.625
model6 <- cv.glmnet(trainSTD,y,alpha = 1, lambda = lambdas)

minl <- model6$lambda.min

outmodel <- glmnet(x2,y2,alpha = 1, lambda = minl,family = 'binomial')
probs <- predict(outmodel,newx = data.matrix(testSTD))
answers <- cbind(test[,1],ifelse(probs > 0.5,"Yes","No"))
colnames(answers) <- c("id","HTWins")
write.csv(answers,"submission6-2.csv",row.names = F)

#submission7 MIXED NOT SUBMITTED
model7 <- cv.glmnet(trainSTD,y,alpha = 0.2, lambda = lambdas)

minl <- model7$lambda.min

outmodel <- glmnet(x2,y2,alpha = 0, lambda = minl,family = 'binomial')
probs <- predict(outmodel,newx = data.matrix(testSTD))
answers <- cbind(test[,1],ifelse(probs > 0.5,"Yes","No"))
colnames(answers) <- c("id","HTWins")
write.csv(answers,"submission6.csv",row.names = F)

#submission 8 PCA then Knn 0.5

trainPC <- prcomp(big[1:9520,],center = T,scale = T)
probs <- predict(trainPC,newdata = big[-c(1:9520),])
library(class)
ans <- knn(trainPC$x,probs,y)
answers <- cbind(test[,1],ifelse(as.logical(ans),"Yes","No"))
colnames(answers) <- c("id","HTWins")
write.csv(answers,"submission8.csv",row.names = F)

#submission 9
library(caret)
ind <- sample(1:9520,size = 1000,replace = F)
train <- data.frame(y = as.factor(y),x)
tetsting <- x[ind,]

fitcontrol <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
mod9 <- train(y~.,data = train,method = 'knn')

#submission10 boosted classification trees (LONG COMP TIME) acc 0.66
mod10 <- train(y~.,data = train,method = 'ada')
ans <- cbind(test[,1],ifelse(ans == "TRUE","Yes","No"))
colnames(ans) <- c("id","HTWins")
write.csv(ans,"submission10.csv",row.names = F)

#submission11 ORFlog NOT SUBMITTED

mod11 <- train(y~.,data = train,method = 'ORFlog')
ans <- predict(mod11,newdata = testx)
ans <- cbind(test[,1],ifelse(ans == "TRUE","Yes","No"))
colnames(ans) <- c("id","HTWins")
write.csv(ans,"submission11.csv",row.names = F)


#winrates

for(i in names(summary(training$VT))){
  cat(i,sum(training$HT == i & training$HTWins == "Yes")/sum(training$HT == i),'\n')
}
