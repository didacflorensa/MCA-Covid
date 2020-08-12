
library(oddsratio)
library(nlme)
library(mgcv)
library(questionr)



precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

covid <- read.csv("~/RelacioSimptomes-Lleida-01062020-31072020-R-BO-MODEL.csv")
covid$DataSimptomes<- NULL
covid$Disnea <- NULL
covid$Diarrea <- NULL
covid$SDR <- NULL
covid$IRA <- NULL
covid$Diabetis <- NULL
covid$Hepatopatiacronica <- NULL
covid$MNeurologica <- NULL
covid$Immunodeficiencia <- NULL
covid$IngresUCI <- NULL
covid$Defuncio <- NULL

covid$Ingreshopitalari

reg <- glm(Onada~., data=covid, family=binomial)
resulttable <- odds.ratio(reg, level=0.90)
summary(reg)

write.csv(resulttable, file = "~/RelacioSimptomes-oddsratio.csv")
write.csv(summary.glm(reg)$coefficients, file = "~/RelacioSimptomes-model.csv")


covid.training <- read.csv("~/RelacioSimptomes-Tots-Training.csv")
covid.testing <- read.csv("~/RelacioSimptomes-Tots-Testing.csv")
covid.training$DataSimptomes<- NULL
covid.testing$DataSimptomes<- NULL
covid.training$Disnea <- NULL
covid.training$Vomits <- NULL
covid.training$SDR <- NULL
covid.training$InsuficienciaRenalAguda <- NULL
covid.training$Diabetis <- NULL
covid.training$Hepatopatiacronica <- NULL
covid.testing$Hepatopatiacronica <- NULL

covid.training$MNeurologica <- NULL
covid.training$Immunodeficiencia <- NULL
covid.training$IngresUCI <- NULL
covid.training$Defuncio <- NULL
covid.training

reg <- glm(Onada~., data=covid.training, family=binomial)
predict <- predict(reg, covid.training, type = 'response')
table_mat <- table(covid.training$Onada, predict > 0.5)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

library(caret)
sensitivity(table_mat)
specificity(table_mat)

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}


prec <- precision(table_mat)
rec <- recall(table_mat)

prec
rec

f1 <- 2 * ((prec * rec) / (prec + rec))

library(ROCR)
ROCRpred <- prediction(predict, covid.training$Onada)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

write.csv(resulttable, file = "/Users/Didac/OneDrive - Generalitat de Catalunya/COVID19/COVID_SIMPTOMES_ESTUDI/MCA-Covid/data/private/RelacioSimptomes-oddsratio.csv")
write.csv(summary.glm(reg)$coefficients, file = "/Users/Didac/OneDrive - Generalitat de Catalunya/COVID19/COVID_SIMPTOMES_ESTUDI/MCA-Covid/data/private/RelacioSimptomes-model.csv")

results_df <-summary.glm(reg)$deviance





#TESTING
covid.training <- read.csv("~/RelacioSimptomes-Tots-Training.csv")
covid.testing <- read.csv("~/RelacioSimptomes-Tots-Testing.csv")
covid.training$DataSimptomes<- NULL
covid.testing$DataSimptomes<- NULL


set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


data_train <- create_train_test(covid.training, 0.8, train = TRUE)
data_test <- create_train_test(covid.testing, 0.8, train = FALSE)
dim(data_train)
dim(data_test)


reg <- glm(Onada~., data=data_train, family=binomial)
summary(reg)

predict <- predict(reg, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$Onada, predict > 0.5)
table_mat
summary(table_mat)


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

library(caret)
sensitivity(table_mat)
specificity(table_mat)

sensitivity_mat <- (208 / (117+28))
sensitivity_mat
specificity_mat <- (55/(55+14))
specificity_mat

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec


f1 <- 2 * ((prec * rec) / (prec + rec))
f1

library(ROCR)
ROCRpred <- prediction(predict, data_test$Onada)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7), xlab="Specificity", ylab="Sensitivity")

