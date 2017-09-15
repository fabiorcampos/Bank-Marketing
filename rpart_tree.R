library(rpart)
library(rpart.plot)
library(entropy)

### load data
df <- read.csv("./data/bank.csv", header = TRUE, sep = ";")

### Cleaning dispensable variables
dfcat <- df[,-c(9:12)]
dfcat <- dfcat[,-c(6,9:11)]

### Exploratory analysis
hist(dfcat$age, col = "light blue", xlab = "Age", freq = FALSE)

### Entropy 
freqs <- table(dfcat$job)/length(dfcat$job)
entropy.empirical(freqs, unit="log2")

### Create the data train and test
set.seed(1923)
trainIndex <- createDataPartition(dfcat$y, p = .75, 
                                  list = FALSE, 
                                  times = 1)

head(trainIndex)

ytrain <- dfcat[ trainIndex,]
yTest  <- dfcat[-trainIndex,]

### Create model
dtree <- rpart(y ~ ., data = ytrain, method = "class", 
               control = rpart.control(minsplit = 1), parms = list(split = "Information"))

### Predict 
y_estimated <- predict(dtree, yTest, "class")

### Plot decision Tree
plot <- rpart.plot(dtree)
