library(dplyr)
library(e1071)
library(glmnet)
library(randomForest)
library(readr)
library(caret)
library(nnet)

# Read the dataset
data <- read_csv("cleaned_data1.csv")

#print all the datatypes
str(data)

# Remove height and weight columns
data <- data %>%
  select(-Height, -Weight)

# Convert character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Perform feature scaling on numerical variables (Age)
num_vars <- c("Age")
data[num_vars] <- lapply(data[num_vars], scale)

# Split the data into training and testing sets
set.seed(123)  # Set a seed for reproducibility
train_indices <- sample(nrow(data), nrow(data) * 0.7)  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Select features and target variable
features <- c("Gender", "Age", "Family_Overweight_History", 
              "High_Caloric_Food_Consumption", "Vegetable_Consumption_Frequency", 
              "Daily_meal_intake", "Food_Consumption_between_meals", "Smoking", 
              "Daily_Water_Intake", "Carbonated_Drinks_Consumption", 
              "Physical_Activity_Frequency", "Technology_Use_Duration", 
              "Alcohol_Consumption_Frequency", "Mode_of_Transportation")
target <- "Obesity_Level"

# Train the Random Forest model
rf_model <- randomForest(formula = as.formula(paste(target, "~", paste(features, collapse = "+"))),
                         data = train_data)
importance(rf_model)

# Evaluate the Random Forest model
predictions_rf <- predict(rf_model, newdata = test_data)
accuracy_rf <- sum(predictions_rf == test_data$Obesity_Level) / nrow(test_data)
cat("Random Forest Accuracy:", round(accuracy_rf * 100, 2), "%\n")

# Calculate the confusion matrix for Random Forest
confusion_rf <- confusionMatrix(predictions_rf, test_data$Obesity_Level)
confusion_table <- confusion_rf$table
cat("Random Forest Confusion Matrix:\n")
print(confusion_table)


# Train the Support Vector Machine (SVM) model
svm_model <- svm(formula = as.formula(paste(target, "~", paste(features, collapse = "+"))),
                 data = train_data)

# Evaluate the SVM model on the testing set
predictions_svm <- predict(svm_model, newdata = test_data)
accuracy_svm <- sum(predictions_svm == test_data$Obesity_Level) / nrow(test_data)
cat("SVM Accuracy:", round(accuracy_svm * 100, 2), "%\n")

# Calculate the confusion matrix for SVM
confusion_svm <- confusionMatrix(predictions_svm, test_data$Obesity_Level)
cat("SVM Confusion Matrix:\n")
print(confusion_svm$table)

# Train the Multinomial Logistic Regression model
multinom_model <- multinom(formula = as.formula(paste(target, "~", paste(features, collapse = "+"))),
                           data = train_data)

# Evaluate the Multinomial Logistic Regression model on the testing set
predictions_multinom <- predict(multinom_model, newdata = test_data, type = "class")
accuracy_multinom <- sum(predictions_multinom == test_data$Obesity_Level) / nrow(test_data)
cat("Multinomial Logistic Regression Accuracy:", round(accuracy_multinom * 100, 2), "%\n")

# Calculate the confusion matrix for Multinomial Logistic Regression
confusion_multinom <- confusionMatrix(predictions_multinom, test_data$Obesity_Level)
cat("Multinomial Logistic Regression Confusion Matrix:\n")
print(confusion_multinom$table)

# Save the Random Forest model as an RDS file in the "models" directory
saveRDS(rf_model, file = "C:\\Users\\ProUser\\Documents\\R\\Obestimator\\random_forest_model.rds")



