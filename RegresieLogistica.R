library(rsample)
library(tidyverse)
library(caret)
library(modelr)
library(nnet)

milknew <- read_csv("milknew.csv")
milknew <- milknew %>%
  mutate(
    Taste = factor (Taste),
    Odor = factor (Odor),
    Fat = factor (Fat),
    Turbidity =factor (Turbidity),
    Grade = factor(Grade)
  )

#vizualizarea datelor

ggplot(milknew) +
  geom_point (aes (x = pH, y = Temprature, color = Grade, shape = Grade)) +
  theme (text = element_text (size=20))

ggplot (milknew) +
  geom_boxplot (aes (x = Grade, y = pH, fill = Grade)) + 
  theme (text = element_text (size=20))

ggplot (milknew) +
  geom_boxplot (aes (x = Grade, y = Temprature, fill = Grade)) + 
  theme (text = element_text (size=20))

by_Grade <- group_by(milknew, Grade)
summarize(by_Grade, count = n())


set.seed(123)

split <- initial_split(milknew, prop = 0.7, strata = "Grade")
train <- training (split)
test <- testing (split)

install.packages("VGAM")
library(VGAM)

mod_multiple <- vglm(
  Grade ~ .,
  data = milknew,
  family = multinomial
)
mod_multiple
summary(mod_multiple)

predProbReg <- predict(mod_multiple, test, type = "response")
predictions <- colnames(predProbReg)[apply(predProbReg, 1, which.max)]
confusionMatrix(factor(predictions), factor(test$Grade))


threshold <- 0.2
predictions <- colnames(predProbReg)[apply(predProbReg, 1, function(x) {
  max_prob <- max(x)
  if (max_prob >= threshold) {
    which(x >= threshold)[1]  # Returns the first class index that exceeds the threshold
  } else {
    which.max(x)  # Returns the class index with the highest predicted probability
  }
})]
confusionMatrix(factor(predictions), factor(test$Grade))



threshold <- 0.3
predictions <- colnames(predProbReg)[apply(predProbReg, 1, function(x) {
  max_prob <- max(x)
  if (max_prob >= threshold) {
    which(x >= threshold)[1]  # Returns the first class index that exceeds the threshold
  } else {
    which.max(x)  # Returns the class index with the highest predicted probability
  }
})]
confusionMatrix(factor(predictions), factor(test$Grade))


threshold <- 0.5
predictions <- colnames(predProbReg)[apply(predProbReg, 1, function(x) {
  max_prob <- max(x)
  if (max_prob >= threshold) {
    which(x >= threshold)[1]  # Returns the first class index that exceeds the threshold
  } else {
    which.max(x)  # Returns the class index with the highest predicted probability
  }
})]
confusionMatrix(factor(predictions), factor(test$Grade))


library(pROC)

dataset <- data.frame(
  actual.class <- factor(test$Grade),
  probability <- predProbReg[, 1] 
)
multiclass.roc(actual.class ~ predProbReg, dataset)


# Comparararea variabilelor dependente
mod_multiple <- vglm(         # Cele mai importante
  Grade ~ pH + Temprature + Fat,
  data = milknew,
  family = multinomial
)
mod_multiple
summary(mod_multiple)

predProbReg <- predict(mod_multiple, test, type = "response")
predictions <- colnames(predProbReg)[apply(predProbReg, 1, which.max)]
confusionMatrix(factor(predictions), factor(test$Grade))

mod_multiple <- vglm(        # Cele mai putin importante
  Grade ~ Taste + Odor + Turbidity + Colour,
  data = milknew,
  family = multinomial
)
mod_multiple
summary(mod_multiple)

predProbReg <- predict(mod_multiple, test, type = "response")
predictions <- colnames(predProbReg)[apply(predProbReg, 1, which.max)]
confusionMatrix(factor(predictions), factor(test$Grade))
