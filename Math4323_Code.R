library("class")

facebookLikes <- read.delim("~/Data Science 4322/Live_20210128.txt", stringsAsFactors = TRUE)
head(facebookLikes)
facebookLikes <- facebookLikes[,-12]
facebookLikes <- facebookLikes[,-12]
facebookLikes <- facebookLikes[,-12]
facebookLikes <- facebookLikes[,-12]
str(facebookLikes)
summary(facebookLikes)
set.seed(2)
train = sample(1:dim(facebookLikes)[1], dim(facebookLikes)[1] *0.8)
testing = -train
library(e1071)
set.seed(2)

tune.out = tune(METHOD = svm, train.x = status_type~., data = facebookLikes[train,], kernel = "linear", ranges = list(cost=c(0.1,1,10)))
summary(tune.out)
svmfit = svm(status_type~., data = facebookLikes[train,], kernerl = "linear", cost = 10)

set.seed(2)
tune.out1 = tune(svm, status_type~., data = facebookLikes[train,], kernel = "polynomial", ranges = list(cost=c(0.1, 1, 10), degree=c(0,1,2,3)))
summary(tune.out1)
svmfit1 = svm(status_type~., data = facebookLikes[train,], kernel = "polynomial", degree = 1, cost = 10)

set.seed(2)
tune.out2 = tune(svm, status_type~., data = facebookLikes[train,], kernel = "radial", ranges = list(cost=c(0.1, 1, 10), gamma=c(0.5, 1, 2)))
summary(tune.out2)
svmfit2 = svm(status_type~., data = facebookLikes[train,], kernel = "radial", gamma = 0.5, cost = 1)

mean(predict(svmfit) != facebookLikes[train, 'status_type'])
mean(predict(svmfit1) != facebookLikes[train, 'status_type'])
mean(predict(svmfit2) != facebookLikes[train, 'status_type'])

mean(predict(svmfit) != facebookLikes[testing, 'status_type'])
mean(predict(svmfit1) != facebookLikes[testing, 'status_type'])
mean(predict(svmfit2) != facebookLikes[testing, 'status_type'])

summary(svmfit)


set.seed(2)
train <- sample(1:length(facebookLikes$status_id), length(facebookLikes$status_id)*.8)

x.train <- facebookLikes[train, -2]
x.test <- facebookLikes[testing, -2]

y.train <- facebookLikes$status_type[train]
y.test <- facebookLikes$status_type[-train]

knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 1)
mean(knn.pred != y.test)

knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 3)
mean(knn.pred != y.test)

knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 5)
mean(knn.pred != y.test)

knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 7)
mean(knn.pred != y.test)

knn.pred <- knn(train = x.train, test = x.test, cl = y.train, k = 10)
mean(knn.pred != y.test)

knn.pred <- knn(train = facebookLikes[,-2], test = facebookLikes[,-2], cl = facebookLikes$status_type, k = 1)
mean(knn.pred != facebookLikes$status_type)
