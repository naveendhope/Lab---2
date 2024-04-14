# Load Libraries
library(caret)
library(randomForest)
library(ggplot2)

# Read data from the file path
file_path <- "C:/Users/navee/OneDrive/Desktop/Business Analytics/Machine Learning/Lab 2/oulad-assessments.csv"

data <- read.csv(file_path)

# Remove rows with missing values

data <- na.omit(data)

# Convert factors
data$code_module <- as.factor(data$code_module)
data$code_presentation <- as.factor(data$code_presentation)
data$assessment_type <- as.factor(data$assessment_type)

# Creating a new target variable for classification based on submission timeliness
data$early_submission <- as.factor(ifelse(data$date_submitted <= data$date, "OnTime", "Late"))

# Split data into training and testing
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(data$early_submission, p = 0.8, list = FALSE)
train_data <- data[training_indices, ]
test_data <- data[-training_indices, ]

# Build a Classification Model (Random Forest)
model_rf <- randomForest(early_submission ~ code_module + code_presentation + assessment_type, data = train_data)

# Predict and Evaluate the Model
predictions_rf <- predict(model_rf, test_data)
conf_matrix <- confusionMatrix(predictions_rf, test_data$early_submission)
print(conf_matrix)

# Importance of Variables
importance(model_rf)
varImpPlot(model_rf)

# Visualization of Factor Levels Impact using ggplot2
ggplot(train_data, aes(x=code_module, fill=early_submission)) +
  geom_bar(position="fill") +
  labs(title="Submission Timeliness by Module", y="Proportion", x="Code Module") +
  theme_minimal()
