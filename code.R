require(randomForest)
require(caret)

# Load the train and test dataset
train.df <- read.csv(file="data/train.csv",header = T)
train.df$label <- as.factor(train.df$label)
div <- sample(1:nrow(train.df), 0.8*nrow(train.df))
train <- train.df[div,]
test <- train.df[-div,]
test.df <- read.csv(file="data/test.csv",header = T)

set.seed(1111)
model.rf <- randomForest(label~., data=train, ntree=500, mtry=5)

qplot(data = model.rf, x = 500, y = model.rf$err.rate, color = model.rf$mtry, group = model.rf$mtry, geom = "line")

rotate <- function(x) t(apply(x, 2, rev))
m = rotate(matrix(unlist(train.df[3,-1]),nrow = 28,byrow = T))
image(m,col=grey.colors(255))

pred.rf <- predict(model.rf, test)

mean(test$label == pred.rf)

pred.test <- predict(model.rf, test.df)

test.df$label <- pred.test
write.csv(test.df$label, "pred.csv")


