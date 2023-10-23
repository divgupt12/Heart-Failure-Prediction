library("data.table")
library(ggplot2)
library(data.table)
library(corrplot)
library(lattice)
library(caret)
library(MASS)
library(leaps)
library(RColorBrewer)
library(DataExplorer)
library(caret)
library(dplyr)
library(randomForest )
library(faux)
library(rpart)
library(caTools)
library(rpart.plot)
library(InformationValue)
library(ISLR)

setwd("/Users/mac/Desktop/A6003")
df <- fread("heart_failure_clinical_records_dataset.csv", stringsAsFactors = T)

# Data pre-processing
# Converting integers to factors 
df$anaemia <- as.factor(df$anaemia)
df$diabetes <- as.factor(df$diabetes)
df$high_blood_pressure <- as.factor(df$high_blood_pressure)
df$sex <- as.factor(df$sex)
df$smoking <- as.factor(df$smoking)
df$DEATH_EVENT <- as.factor(df$DEATH_EVENT)

# Boxplot
x1 <- df$age
x2 <- df$creatinine_phosphokinase
x3 <- df$ejection_fraction	
x4 <- df$platelets	
x5 <- df$serum_creatinine	
x6 <- df$serum_sodium	
x7 <- df$time
y <- df$DEATH_EVENT

myColors <- ifelse(levels(df$DEATH_EVENT)=="Survival" , rgb(0.3,0.6,0.8,0.5) , 
                   ifelse(levels(df$DEATH_EVENT)=="Death", rgb(0.7,0.2,0.3,0.6),
                          "grey90" ) )
data <- data.frame(x,y)

boxplot(x7~y, data, main = "time vs DEATH_EVENT",ylab = "",xlab = "",col=myColors) 

boxplot(x6~y, data, main = "time vs serum_sodium",ylab = "",xlab = "",col=myColors) 

boxplot(x5~y, data, main = "time vs serum_creatinine	",ylab = "",xlab = "",col=myColors) 

boxplot(x4~y, data, main = "time vs platelets	",ylab = "",xlab = "",col=myColors) 

boxplot(x3~y, data, main = "time vs ejection_fraction	",ylab = "",xlab = "",col=myColors) 

boxplot(x2~y, data, main = "time vs creatinine_phosphokinase",ylab = "",xlab = "",col=myColors) 

boxplot(x1~y, data, main = "time vs age",ylab = "",xlab = "",col=myColors) 

# Stacked bar chart

ggplot(df, 
       aes(x = DEATH_EVENT, 
           fill = smoking)) + 
  geom_bar(position = "stack")+
  labs(title="smoking vs DEATH_EVENT \n", x="") +
  scale_color_manual(labels = c("N", "Y"))+theme(plot.title = element_text(hjust = 0.5))

ggplot(df, 
       aes(x = DEATH_EVENT, 
           fill = sex)) + 
  geom_bar(position = "stack")+
  labs(title="sex vs DEATH_EVENT \n", x="") +
  scale_color_manual(labels = c("N", "Y"))+theme(plot.title = element_text(hjust = 0.5))

ggplot(df, 
       aes(x = DEATH_EVENT, 
           fill = anaemia)) + 
  geom_bar(position = "stack")+
  labs(title="anaemia vs DEATH_EVENT \n", x="") +
  scale_color_manual(labels = c("N", "Y"))+theme(plot.title = element_text(hjust = 0.5))

# Correlation plot
df2 = fread("heart_failure_clinical_records_dataset.csv")
df2[] <- lapply(df2, as.numeric)
corrplot(cor(df2),type = "upper", order = "hclust", 
   tl.col = "black")

# Violin chart
df$Death <- ifelse(df$DEATH_EVENT == 1 | df$DEATH_EVENT == 2, "Dead", "Alive")

p<-ggplot(df, aes(x=serum_creatinine, y=Death,fill = "Status")) +geom_violin()+ coord_flip()
p + geom_boxplot(width = .2)+ scale_fill_manual(values = c("#BCE4D8", "#49A4B9"))+ theme_classic() 

ggplot(df, aes(x=serum_sodium, y=Death,fill = "Status")) +geom_violin()+ coord_flip() +
  geom_boxplot(width = .2)+ scale_fill_manual(values = c("#BCE4D8", "#49A4B9"))+ theme_classic() 


p<-ggplot(df, aes(x=ejection_fraction, y=Death,fill = "Status")) +geom_violin()+ coord_flip()
p + geom_boxplot(width = .2)+ scale_fill_manual(values = c("#BCE5E9", "#49A4B10"))+ theme_classic() 

p<-ggplot(df, aes(x=age, y=Death,fill = "Status")) +geom_violin()+ coord_flip()
p + geom_boxplot(width = .2)+ scale_fill_manual(values = c("#BCE5F9", "#49A4B10"))+ theme_classic()

# Distribution plot with normal range stated
ggplot(df, aes(x=serum_sodium))+ geom_histogram(binwidth=1, colour="white", fill="lightsalmon", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..))),colour="coral", fill="coral", alpha=0.3)+
  geom_vline(xintercept = 135, linetype="dashed")+geom_vline(xintercept = 145, linetype="dashed")+ annotate("text", x=130, y=20, label="Abnormal 27.76", size=2.5, color="dark red") + annotate("text", x=140, y=20, label="Normal 63.21%", color="dark green")+  annotate("text", x=147, y=20, label="Abnormal 9.03%", size=2.5, color="dark red")+labs(title="Serum Sodium") + theme_minimal(base_size = 8)

ggplot(df, aes(x=serum_creatinine))+ geom_histogram(binwidth=0.2, colour="white", fill="goldenrod", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*0.2))),colour="moccasin", fill="moccasin", alpha=0.3)+
  geom_vline(xintercept = 0.59, linetype="dashed")+geom_vline(xintercept = 1.35, linetype="dashed")+ annotate("text", x=0.05, y=20, label="Abnormal", size=2.5, color="dark red") + annotate("text", x=0.05, y=15, label="0.33%", size=2.5, color="dark red")+
  annotate("text", x=0.98, y=20, size=2.5,label="Normal", color="dark green")+annotate("text", x=0.98, y=15, size=2.5,label="72.58%", color="dark green") +  annotate("text", x=2.5, y=20, label="Abnormal", size=2.5, color="dark red")+annotate("text", x=2.5, y=15, label="27.09%", size=2.5, color="dark red")+labs(title="Serum Creatinine") + theme_minimal(base_size = 8)

ggplot(df, aes(x=age))+ geom_histogram(binwidth=5, colour="white", fill="darkseagreen2", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*5))),colour="darkgreen", fill="darkgreen", alpha=0.3)+ scale_x_continuous(breaks=seq(40,100,10))+geom_vline(xintercept = 55, linetype="dashed")+geom_vline(xintercept = 70, linetype="dashed")+ annotate("text", x=45, y=45, label="Age <55 31.77%", size=2.5, color="dark green") + annotate("text", x=61, y=45, label="55 <= Age <= 70 50.84%", size=2.5, color="dark red")+annotate("text", x=80, y=45, label="Age >= 70 17.39%", size=2.5, color="dark green") +labs(title="Age Distribution") + theme_minimal(base_size = 8)

# Losgistic Regession Model
clinical_records = fread("heart_failure_clinical_records_dataset.csv", stringsAsFactors = T)
colnames(clinical_records ) = c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","y")

# set train and test datasets
set.seed(1)
train_ = sample(c(TRUE,FALSE), nrow(clinical_records), replace=TRUE, prob=c(0.7,0.3))
train_set = clinical_records[train_,]
test_set = clinical_records[!train_,]

glm.control(epsilon = 1e-8, maxit = 200, trace = FALSE)
glm = glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12,family = binomial(link = "logit"),data = train_set)
logit.step = step(glm, direction = c("both"))
summary(glm)
summary(logit.step)

# related parameters
coef(logit.step)
exp(coef(logit.step))
confint(logit.step)

# make predictions(based on "logit.step")
train_x = train_set[,1:12]
train_y = train_set$y
train_p = predict(logit.step, train_x, type='response')
train_p_class <- ifelse(train_p > 0.5, 1, 0)

test_x = test_set[,1:12]
test_y = test_set$y
test_p = predict(logit.step, test_x, type='response')
test_p_class = ifelse(test_p > 0.5, 1, 0)

# make predictions(based on "glm")
train_x = train_set[,1:12]
train_y = train_set$y
train_p2 = predict(glm, train_x, type='response')
train_p2_class <- ifelse(train_p > 0.5, 1, 0)

test_x = test_set[,1:12]
test_y = test_set$y
test_p2 = predict(glm, test_x, type='response')
test_p2_class = ifelse(test_p > 0.5, 1, 0)

#
library(caret)
value_and_pred = table(test_p_class, test_y)
confusionMatrix(value_and_pred, positive = "1")
confusionMatrix(as.factor(test_p_class), as.factor(test_y),
                positive = "1", mode = "everything")

# ROC plot and make comparison on two models(logit.step and glm)
library(pROC)
roc_train_step = roc(train_y ~ train_p, data=train_set)
roc_train_step
roc_test_step = roc(test_y ~ test_p, data=test_set)
roc_test_step

roc_train_glm = roc(train_y ~ train_p2, data=train_set)
roc_train_glm
roc_test_glm = roc(test_y ~ test_p2, data=test_set)
roc_test_glm
# logit.step shows a high performance

roc.test(roc_train_step,roc_test_step)
plot(roc_train_step,col="red")
plot(roc_test_step,col="blue")

# print in the same chart
plot(roc_train_step, col="red")
plot.roc(roc_test_step, add=TRUE, col="blue")

# CART model
df$Death <- NULL
de0 <- df[DEATH_EVENT == "0"]
de1 <- df[DEATH_EVENT == "1"]

m <- rpart(DEATH_EVENT ~ ., data = df, method = "class", cp = 0)
printcp(m, digits = 3)
plotcp(m)
plot(m)
text(m, digits = 3)

CVerror.cap <- m$cptable[which.min(m$cptable[,"xerror"]), "xerror"] + m$cptable[which.min(m$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (m$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

cp.opt = ifelse(i > 1, sqrt(m$cptable[i,1] * m$cptable[i-1,1]), 1)

m.best <- prune(m, cp = cp.opt)
printcp(m.best, digits = 3)

m.best$variable.importance
m.best.scaledVarImpt <- round(100*m.best$variable.importance/sum(m.best$variable.importance))
m.best.scaledVarImpt

#As per variable importance, time is most important, so spliting data on time
set.seed(2)
train <- sample.split(Y= df$time, SplitRatio = 0.7)
trainset <- df[train == T]
testset <- df[train == F]
trainset
cart.max <- rpart(DEATH_EVENT ~ . , data=trainset,  method = "class", cp=0)
cart.opt <- prune(cart.max, cp = cp.opt)

#if data is split on Y variable only
set.seed(2)
train <- sample.split(Y= df$DEATH_EVENT, SplitRatio = 0.7)
trainset <- df[train == T]
testset <- df[train == F]
trainset
cart.max <- rpart(DEATH_EVENT ~ . , data=trainset,  method = "class", cp=0)
cart.opt <- prune(cart.max, cp = cp.opt)

cart.yhat <- predict(cart.max, newdata = testset, type = "class")
cart.yhat
table1 <- table(Pred = cart.yhat, Actual = testset$DEATH_EVENT, deparse.level=2)
table1
round(prop.table(table1, margin = 2),3) #87% predicted death and will die
Acc <-(table1[1]+table1[4])/sum(table1)
Acc
recall<-table1[4]/(table1[4]+table1[2])
recall
# If split on DEATH_EVENT, accuracy is 86%
# If split on time, accuracy is 89%

# Feature Selection for random forest model
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 6, # number of repeats
                      number = 5) # number of folds

#select x
set.seed(1950)
x <- df[,1:12]
as.data.frame(x)
# Target variable
y <- df$DEATH_EVENT

#Split train and test as 80% and 20%
Train <- createDataPartition(y, p = .80, list = FALSE)[,1]
x_train <- x[ Train, ]
x_test  <- x[-Train, ]
y_train <- y[ Train]
y_test  <- y[-Train]
result <- rfe(x = x_train, y = y_train, metrics = "Accuracy",sizes = c(1:13),rfeControl = control)
predictors(result)

#Plot
ggplot(data = result, metric = "Accuracy") + theme_bw()
ggplot(data = result, metric = "Kappa") + theme_bw()
# The result shows that using 3 features will give the best result and using 6 features ranks as second

# Random forest
# Split train and test data using seed 600
set.seed(600)
ind <- sample(2,nrow(df),replace=TRUE,prob=c(0.8,0.2))
train <- df[ind==1,]
test <- df[ind==2,]

# Oversmaple the data using SMOTE, making the number of death event balanced
library(ROSE) 
table(train$DEATH_EVENT)
train <- ovun.sample(DEATH_EVENT ~., data = train, method = "over",N = 326, seed = 1)$data
prop.table(table(train$DEATH_EVENT))

set.seed(1000)
B <- c(25, 25, 25, 100, 100, 100, 500, 500, 500)
m <- ncol(df)-1
RSF <- rep.int(c(1, floor(sqrt(m)), m), times=3)
OOB.error <- seq(1:9)

library(randomForest)
for (i in 1:length(B)) {
  m.RF <- randomForest(DEATH_EVENT ~ . , data = train,
                       mtry = RSF[i],
                       ntree = B[i],
                       na.action = na.omit)
  OOB.error[i] <- m.RF$err.rate[m.RF$ntree, 1]
}

results <- data.frame(B, RSF, OOB.error)
## trying different seeds, OOB error is relatively low for B = 500, RSF = 3.
## these are default values in randomForest() function.

m.RF.final <- randomForest(DEATH_EVENT ~ . , data = train, na.action = na.omit, importance = T,proximity=TRUE)
pred<-predict(m.RF.final,newdata=test)  

table <- table(pred,test$DEATH_EVENT) 
accuracy = (table[1]+table[4])/sum(table)
# Accuracy of the prediction moedel is 87.93%
recall = table[4]/(table[4]+table[2])
# Recall rate is 86.67%, meaning 86.67% of people that we predicted as death event=1 will die due to heart failure
importance <- importance(m.RF.final)
varImpPlot(m.RF.final, type = 1)
importance <- as.data.frame(importance)
importance <- importance[order(importance$MeanDecreaseAccuracy,decreasing = T),]

# Trying different feature selection
set.seed(2000)
feature_select <- rownames(importance)[1:6]
# Recall and accuracy reach their maximum when 6 feature are selected
test_top <- subset(test, select=c(feature_select, 'DEATH_EVENT'))
train_top <- subset(train, select=c(feature_select, 'DEATH_EVENT'))
RF.top <- randomForest(DEATH_EVENT ~ . , data = train_top, na.action = na.omit, importance = T,proximity=TRUE)
pred_top<-predict(RF.top,newdata=test_top)
compare_test <- table(pred_top,test_top$DEATH_EVENT, dnn = c('Predicted','Actual'))
compare_test
recall = compare_test[4]/(compare_test[4]+compare_test[2])
recall
accuracy = (compare_test[1]+compare_test[4])/sum(compare_test)
accuracy

# Calculate the AUC of model
pre_rf = as.numeric(pred_top)
pre_rf <- ifelse(pre_rf > 1, 1, 0)
test_rf = as.numeric(test_top$DEATH_EVENT)
test_rf <- ifelse(test_rf > 1, 1, 0)
roc_rf = roc(test_rf ~ pre_rf, dataset=train_top)
roc_rf

# Plot ROC curve
plot(roc_rf,col="blue")
