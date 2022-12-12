# ---------------- INSTALL AND LOAD PACKAGES ----------------
library(datasets)
library(dplyr)

# ---------------- IMPORT DATA ----------------
weather_data <- read.csv("./weather_data.csv", )
head(weather_data)

# ---------------- PRE-PROCESS DATA ----------------
# Replace NA data with 0
weather_data[is.na(weather_data)] <- 0
# Convert today rain to binary
weather_data <- weather_data %>% mutate(
                    todayrain = if_else(preciptype == "rain", 1, 0),
                    tomorrowrain = if_else(lead(preciptype) == "rain", 1, 0)
                )
# Drop unnecessary column
drops <- c("name", "datetime", "snow",
             "precipprob", "precipcover", "preciptype",
            "snowdepth", "conditions", "description",
            "icon", "severerisk", "sunrise", "sunset", "stations")
weather_data <- weather_data[ , !(names(weather_data) %in% drops)]
# Fill last missing rain prediction result
weather_data[is.na(weather_data)] <- 1
# Convert todayrain to a factor to be treated as a categorical variable
# weather_data$todayrain <- factor(weather_data$todayrain)
summary(weather_data)
# ---------------- Splitting dataset ----------------
split <- sample.split(weather_data, SplitRatio = 0.8)
split

train_reg <- subset(weather_data, split == "TRUE")
test_reg <- subset(weather_data, split == "FALSE")

# ---------------- Training model ----------------
logistic_model <- glm(tomorrowrain ~ .,
                      data = train_reg,
                      family = "binomial")
logistic_model

# ---------------- Summary ----------------
summary(logistic_model)

# Predict test data based on model
predict_reg <- predict(logistic_model,
                       test_reg, type = "response")
predict_reg

# Changing probabilities
predict_reg <- ifelse(predict_reg > 0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(test_reg$tomorrowrain, predict_reg)

missing_classerr <- mean(predict_reg != test_reg$tomorrowrain)
print(paste('Accuracy =', 1 - missing_classerr))

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
