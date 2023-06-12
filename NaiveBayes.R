install.packages('rpart.plot')
install.packages('partykit')
install.packages("corrplot")
install.packages("modeldata")

library(rsample)
library(tidyverse)
library(caret)
library(corrplot)
library(modeldata)
library(readr)
library(partykit)
milknew <- read_csv("milknew.csv")

View(milknew)
table(milknew$Grade)

milknew %>%
  ggplot(aes(pH)) +
  geom_density(show.legend = TRUE)

milknew %>%
  ggplot(aes(Temprature)) +
  geom_density(show.legend = TRUE)

range_of_values <- range(milknew$Colour)
range_of_values

milknew %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

milknew <- milknew %>%
  mutate(
    Taste = factor (Taste),
    Odor = factor (Odor),
    Fat = factor (Fat),
    Turbidity =factor (Turbidity),
    Grade = factor(Grade)
  )

milknew %>%
  filter (Grade == "high") %>%
  select_if(is.numeric) %>%
  cor()%>%
  corrplot::corrplot()

milknew %>%
  filter (Grade == "low") %>%
  select_if(is.numeric) %>%
  cor()%>%
  corrplot::corrplot()

milknew %>%
  filter (Grade == "medium") %>%
  select_if(is.numeric) %>%
  cor()%>%
  corrplot::corrplot()

set.seed(123)  # metine datele reproductibile

split <- initial_split(milknew, prop = 0.7, strata = "Grade")
train <- training (split)
test <- testing (split)
table(train$Grade)
table(test$Grade)

features <- setdiff(names(train), "Grade")
x <- train[, features]
y <- train$Grade

fitControl <- trainControl(
  method = "cv",
  number = 10
)

modNbSimpleCV <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = fitControl
)

modNbSimpleCV
confusionMatrix(modNbSimpleCV)

pred <- predict(modNbSimpleCV, test)
predProb <- predict(modNbSimpleCV, test, type = "prob")
confusionMatrix(pred, factor(test$Grade))
dataset <- data.frame(
  actual.class <- factor(test$Grade),
  probability <- predProb[, 1] 
)

multiclass.roc(actual.class ~ probability, dataset)

searchGrid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0.5,
  adjust = seq(0, 5, by = 1)
)


modNbCVSearch <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = fitControl,
  tuneGrid = searchGrid
)
modNbCVSearch
confusionMatrix(modNbCVSearch)

modNbCVSearch$results %>%
  top_n(7, wt = Accuracy) %>%
  arrange(desc(Accuracy))

pred <- predict(modNbCVSearch, test)
pred
predProb <- predict(modNbCVSearch, test, type = "prob")
predProb
confusionMatrix(pred, factor(test$Grade))

library(pROC)

dataset <- data.frame(
  actual.class <- factor(test$Grade),
  probability <- predProb[, 1] 
)

multiclass.roc(actual.class ~ probability, dataset)

searchOne <- expand.grid(
  usekernel = TRUE,
  fL = 0.5,
  adjust = 1
)
fitControlNone <- trainControl(
  method = "none"
)

modNbNone <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = fitControlNone,
  tuneGrid = searchOne
)

modNbNone
summary(modNbNone)
predNone <- predict(modNbNone, test)
confusionMatrix(predNone, test$Grade)

ggplot(modNbCVSearch)
ggplot(modNbCVSearch, metric = "Accuracy")

fitControlROC <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,
  savePredictions = TRUE
)

modNbCVSearchROC = train(
  x = x,
  y = y,
  method = "nb",
  trControl = fitControlROC,
  tuneGrid = searchGrid,
  metric = "ROC"
)
modNbCVSearchROC

confusionMatrix(modNbCVSearchROC)

predROC <- predict(modNbCVSearchROC, test)
predProbROC <- predict(modNbCVSearchROC, test, type = "prob")
confusionMatrix(predROC, factor(test$Grade))
