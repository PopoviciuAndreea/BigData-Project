library(rpart)
library(rpart.plot)
library(tidyverse)
library(rsample)
library(caret)
library(partykit)
library(readr)
library(dplyr)
library(pROC)


milknew <- read_csv("milknew.csv")
milknew <- milknew %>%  
  mutate(
    Taste = factor (Taste),
    Odor = factor (Odor),
    Fat = factor (Fat),
    Turbidity =factor (Turbidity),
    Grade = factor(Grade)
  )

set.seed(123)
milknew_split <- initial_split(milknew, prop = 0.7, strata = "Grade")
milknew_train <- training(milknew_split)
milknew_test <- testing(milknew_split)
table(milknew_train$Grade)
table(milknew_test$Grade)

set.seed(123)

m1 = rpart(
  formula = Grade ~. ,
  data = milknew_train,
  method = "class",
  control = list(cp = 0)
)
m1
summary(m1) 
rpart.plot(m1)

pred_m1 <- predict(m1, newdata = milknew_test, target ="class")
pred_m1

pred_m1_tibble <- as_tibble (pred_m1) %>%
  mutate(class = ifelse(high >= low & high >= medium, "high",
                        ifelse(low >= high & low >= medium, "low",
                               ifelse(medium >= low & medium >= high, "medium", NA)))) #NA (valoare lipsă)

pred_m1_tibble
#print(pred_m1, n = 319)

table(pred_m1_tibble$class, milknew_test$Grade)
confusionMatrix(factor(pred_m1_tibble$class), factor(milknew_test$Grade))

datasetTree <- data.frame(
  actual.class <- factor(milknew_test$Grade),
  probability <- pred_m1[, 1]
)

multiclass.roc(actual.class ~ pred_m1, datasetTree)

set.seed(123)
m2 <- rpart(Grade ~., 
            data = milknew_train,
            method = "class",
            control = list(cp=0))
m2
summary(m2)
rpart.plot(m2)

pred_m2 <- predict(m2, newdata = milknew_test, target = "class")
pred_m2 <- as_tibble (pred_m2) %>%
  mutate(class = ifelse(high >= low & high >= medium, "high",
                        ifelse(low >= high & low >= medium, "low",
                               ifelse(medium >= low & medium >= high, "medium", NA)))) #NA (valoare lipsă)

confusionMatrix(factor(pred_m2$class), factor(milknew_test$Grade))

m2_pruned <- prune(m2, cp = 0.04)
summary(m2_pruned)
pred_m2_pruned <- predict(m2_pruned, newdata = milknew_test, target = "class")
pred_m2_pruned
pred_m2_pruned_tibble <- as_tibble(pred_m2_pruned) %>%
  mutate(class = ifelse(high >= low & high >= medium, "high",
                        ifelse(low >= high & low >= medium, "low",
                               ifelse(medium >= low & medium >= high, "medium", NA))))

confusionMatrix(factor(pred_m2_pruned_tibble$class), factor(milknew_test$Grade))


# curba ROC
datasetTree <- data.frame(
  actual.class <- factor(milknew_test$Grade),
  probability <- pred_m2_pruned[, 1]
)
multiclass.roc(actual.class ~ probability, datasetTree)


#entropie -> entropy

install.packages("tree")
library(tree)

set.seed(123)    
m1_tree <- tree(Grade~., data = milknew_train)
m1_tree
summary(m1_tree)

pred_m1_tree <- predict(m1_tree, newdata = milknew_test, target = "class")
pred_m1_tree
pred_m1_tree_tibble <- as_tibble(pred_m1_tree) %>%
  mutate(class = ifelse(high >= low & high >= medium, "high",
                        ifelse(low >= high & low >= medium, "low",
                               ifelse(medium >= low & medium >= high, "medium", NA))))

confusionMatrix(factor(pred_m1_tree_tibble$class), factor(milknew_test$Grade))

datasetTree <- data.frame(
  actual.class <- factor(milknew_test$Grade),
  probability <- pred_m1_tree[, 1]
)

multiclass.roc(actual.class ~ pred_m1_tree, datasetTree)

# gini
set.seed(123)    
m1_tree_gini <- tree(Grade~., data = milknew_train, split = "gini")
m1_tree_gini
summary(m1_tree_gini)

pred_m1_tree_gini <- predict(m1_tree_gini, newdata = milknew_test, target = "class")
pred_m1_tree_gini
pred_m1_tree_gini_tibble <- as_tibble(pred_m1_tree_gini) %>%
  mutate(class = ifelse(high >= low & high >= medium, "high",
                        ifelse(low >= high & low >= medium, "low",
                               ifelse(medium >= low & medium >= high, "medium", NA))))

confusionMatrix(factor(pred_m1_tree_gini_tibble$class), factor(milknew_test$Grade))

datasetTree <- data.frame(
  actual.class <- factor(milknew_test$Grade),
  probability <- pred_m1_tree[, 1]
)

multiclass.roc(actual.class ~ pred_m1_tree, datasetTree)


#bagging
install.packages("ipred")
library(ipred)

set.seed(123)
bagged_m1 <- bagging(Grade ~ .,
                     data = milknew_train, coob = TRUE)
bagged_m1
summary(bagged_m1)
pred_bagged_m1 <- predict(bagged_m1, newdata = milknew_test, target = "class")
confusionMatrix(pred_bagged_m1, factor(milknew_test$Grade))

ntree <- seq(10, 50, by = 1)
misclassification <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging( 
    Grade ~.,
    data = milknew_train,
    coob = TRUE,
    nbag = ntree[i])
  misclassification[i] = model$err
}
plot(ntree, misclassification, type="l", lwd="2")
#ceva mai mult de 40 bags sunt necesare pentru a stabiliza rata de eroare

bagged_m1_32 <- bagging(Grade ~ .,
                        data = milknew_train, coob = TRUE, nbag = 32)
bagged_m1_32
summary(bagged_m1_32)
pred_bagged_m1_32 <- predict(bagged_m1_32, newdata = milknew_test, target = "class")
confusionMatrix(pred_bagged_m1_32, factor(milknew_test$Grade))


#random forests
install.packages("randomForest")
library(randomForest)

set.seed(123)
m1_rf <- randomForest(
  formula = Grade ~ .,
  data = milknew_train
) #selecteaza aleatoriu 3 variabile la fiecare impartire 
m1_rf
plot(m1_rf)
m1_rf$err.rate
m1_rf$confusion
which.min(m1_rf$err.rate[,1])
m1_rf$err.rate[which.min(m1_rf$err.rate[,1])]

pred_m1_rf <- predict(m1_rf, newdata = milknew_test, target = "class")
confusionMatrix(pred_m1_rf, factor(milknew_test$Grade))


#tuning
install.packages("ranger")
library(ranger)

set.seed(123)
hyper_grid <- expand.grid(
  mtry = seq(2, 7, by = 1), # the dataset has only 10 predictors
  node_size = seq(3, 9, by = 2), 
  sample_size = c(.55, .632, .7, .8),
  OOB_ERR = 0
)

View(hyper_grid)

nrow(hyper_grid)
for (i in 1:nrow(hyper_grid)) {
  model <- ranger(
    formula = Grade ~ .,
    data = milknew_train,
    num.trees = 500, 
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sample_size[i],
    seed = 123
  )
  hyper_grid$OOB_ERR[i] <- model$prediction.error
}
hyper_grid %>%
  arrange(desc(OOB_ERR)) %>%
  top_n(-10)
OOB_ERR <- vector(mode = "numeric", length = 100)
for(i in seq_along(OOB_ERR)) {
  optimal_ranger <- ranger(
    formula         = Grade ~ ., 
    data            = milknew_train, 
    num.trees       = 500,
    mtry            = 6,
    min.node.size   = 3,
    sample.fraction = .55,
    importance      = 'none'
  )
  
  OOB_ERR[i] <- optimal_ranger$prediction.error
}
hist(OOB_ERR, breaks = 20)
mean(OOB_ERR)

pred_ranger <- predict(optimal_ranger, milknew_test, target ="class")  
confusionMatrix(pred_ranger$predictions, factor(milknew_test$Grade))

