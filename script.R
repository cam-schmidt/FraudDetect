rm(list=ls())

library(ROSE)
library(randomForest)
# Set seed for reproducibility
set.seed(123)

# Load data
fraud_data <- read.csv("____PATH___TO___DATA____", header=TRUE)

# Split data into 80% training and 20% test set
sample_size <- floor(0.8 * nrow(fraud_data))
train_ind <- sample(seq_len(nrow(fraud_data)), size = sample_size)

train_data <- fraud_data[train_ind, ]
test_data <- fraud_data[-train_ind, ]

# Calculate the number of desired fraudulent cases for balancing
fraud_cases <- sum(train_data$Class == 1)
non_fraud_cases <- sum(train_data$Class == 0)
desired_fraud_cases <- non_fraud_cases - fraud_cases

# Convert Class variable to factor for training and test data
train_data$Class <- as.factor(train_data$Class)
test_data$Class <- as.factor(test_data$Class)

# Oversample the minority class using ROSE's ovun.sample function
balanced_data <- ovun.sample(Class ~ ., data = train_data, method = "over", 
                             N = nrow(train_data) + desired_fraud_cases)$data

# Train the random forest model on balanced data
rf_model_balanced <- randomForest(Class ~ ., data=balanced_data, ntree=100)

# Print model details
print(rf_model_balanced)
