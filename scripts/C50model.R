library(rpart)
library(rpart.plot)
library(entropy)
library(C50)
library(gmodels)

### Load data
df <- read.csv("./data/bank-full.csv", header = TRUE, sep = ";")
dfcat <- df
# dfcat <- df[,-c(1)]

### Exploratory data
hist(dfcat$age, col = "light blue", xlab = "Age", freq = FALSE)
str(dfcat)
table(dfcat$education)
table(dfcat$default)
table(dfcat$housing)
table(dfcat$month)
summary(dfcat$duration)

### Data preparation – creating random training and test datasets
set.seed(123)
# train_sample <- sample(45211, 36168)
train_sample <- sample(45211, 40689)

df_train <- dfcat[train_sample,]
df_test <- dfcat[-train_sample,]

prop.table(table(df_train$y))

### training two models on the data
rpart <- rpart(y ~ ., data = df_train, method = "class", 
               control = rpart.control(minsplit = 1), parms = list(split = "Information"))

cmodel <- C5.0(df_train[-17], df_train$y)

### summary of models training
rpart
cmodel
summary(cmodel)

### Evaluate model performance
cmodel_pred <- predict(cmodel, df_test)
rpart_pred <- predict(rpart, df_test)

### Cross table validation
CrossTable(df_test$y, cmodel_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### improving model performance
cmodel_boost10 <- C5.0(df_train[-17], df_train$y,
                       trials = 10)

cmodel_boost20 <- C5.0(df_train[-17], df_train$y,
                       trials = 20)

summary(cmodel_boost10)
summary(cmodel_boost20)
summary(cmodel)

cmodel_pred10 <- predict(cmodel_boost10, df_test)
cmodel_pred20 <- predict(cmodel_boost20, df_test)

CrossTable(df_test$y, cmodel_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

CrossTable(df_test$y, cmodel_pred20,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

CrossTable(df_test$y, cmodel_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

confusion.matrix <- prop.table(table(cmodel_pred, df_test$y))
accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 
accuracy

confusion.matrix <- prop.table(table(cmodel_pred10, df_test$y))
accuracy10 <- confusion.matrix[1,1] + confusion.matrix[2,2] 
accuracy10

confusion.matrix <- prop.table(table(cmodel_pred20, df_test$y))
accuracy20 <- confusion.matrix[1,1] + confusion.matrix[2,2] 
accuracy20

table(accuracy, accuracy10, accuracy20)
