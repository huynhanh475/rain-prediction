# ---------------- INSTALL AND LOAD PACKAGES ----------------
library(datasets)
library(dplyr)
library(caTools)
library(ROCR)
library(ggplot2)
library(cowplot)

# ---------------- IMPORT DATA ----------------
weather_data <- read.csv("./weather_data.csv", )
head(weather_data)
# ---------------- PRE-PROCESS DATA ----------------
# Replace NA data with 0
weather_data[is.na(weather_data)] <- 0
# Convert today and tomorrow rain data to binary
weather_data <- weather_data %>% mutate(
                    todayrain = if_else(preciptype == "rain", 1, 0),
                    tomorrowrain = if_else(lead(preciptype) == "rain", 1, 0)
                )
# Drop unnecessary column
drops <- c("name", "datetime", "snow",
            "precipprob" ,"precip", "precipcover", "preciptype", "sealevelpressure",
            "snowdepth", "conditions", "description",
            "icon", "sunrise", "sunset", "stations")
weather_data <- weather_data[ , !(names(weather_data) %in% drops)]
# Fill last missing rain prediction result
weather_data[is.na(weather_data)] <- 1
summary(weather_data)
# ---------------- SPLITTING DATASET ----------------
split <- sample.split(weather_data, SplitRatio = 0.8)
train_reg <- subset(weather_data, split == "TRUE")
test_reg <- subset(weather_data, split == "FALSE")

# ---------------- TRAINING MODEL ----------------
logistic_model <- glm(tomorrowrain ~ .,
                      data = train_reg,
                      family = "binomial")

# ---------------- SUMMARY ----------------
summary(logistic_model)

# ---------------- EVALUATE MODEL ----------------
# Predict test data based on model
predict_reg <- predict(logistic_model,
                       test_reg, type = "response")
predict_reg
# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)
predict_reg

# Evaluating model accuracy
# Using confusion matrix
table(test_reg$tomorrowrain, predict_reg)

missing_classerr <- mean(predict_reg != test_reg$tomorrowrain)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, test_reg$tomorrowrain)
ROCPer <- performance(ROCPred, measure = "tpr",
                             x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

## Calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic_model$null.deviance/-2
ll.proposed <- logistic_model$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic_model$coefficients)-1))

## Plot the data
predicted.data <- data.frame(
  probability.of.tomorrowrain=logistic_model$fitted.values,
  tomorrowrain=train_reg$tomorrowrain)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.tomorrowrain, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

## Plot the predicted probabilities for each sample raining the day after
## and color by whether or not they actually rained
ggplot(data=predicted.data, aes(x=rank, y=probability.of.tomorrowrain)) +
  geom_point(aes(color=tomorrowrain), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of raining tomorrow")

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
