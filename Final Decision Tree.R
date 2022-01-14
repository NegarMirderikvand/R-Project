library(ISLR)
library(tree)

data(package="ISLR")
carseats<-Carseats

names(carseats)
str(carseats)
head(carseats)

High = ifelse(carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)
carseats$High = factor(carseats$High)

set.seed(84)
train=sample(1:nrow(carseats), 250)

#apply Decision Tree
tree.carseats = tree(High~.-Sales, carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)

#Predict DT on Test Data set
tree.pred = predict(tree.carseats, carseats[-train,], type="class")
#Confusion Matrix
with(carseats[-train,], table(tree.pred, High))

#cross validation for pruning
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats
plot(cv.carseats)


#prune the tree
prune.carseats = prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty=0)

#predict with pruned tree
tree.pred = predict(prune.carseats, carseats[-train,], type="class")
with(carseats[-train,], table(tree.pred, High))





