# Load required libraries
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(caret)
  
# Read the CSV file
df <- read.csv("pr5.csv")

# Display the first few rows of the dataset
head(df)

# Summary statistics of the dataset
summary(df)

# Check for missing values column-wise
colSums(is.na(df))

# Handle missing values by replacing '?' with 'Missing' in categorical columns
# No need to handle missing values as the sample data doesn't contain them

# Convert categorical variables into factors
df$job <- as.factor(df$job)
df$marital <- as.factor(df$marital)
df$default <- as.factor(df$default)
df$education <- as.factor(df$education)
df$housing <- as.factor(df$housing)
df$loan <- as.factor(df$loan)
df$contact <- as.factor(df$contact)
df$month <- as.factor(df$month)
df$poutcome <- as.factor(df$poutcome)
df$y <- as.factor(df$y)

# Explore and visualize numeric variables
plot_intro(df)
plot_missing(df)

# Explore and visualize categorical variables
plot_bar(df)

# Boxplot of numeric variables
boxplot(df$balance)
boxplot(df$duration)

# Create age categories
df$age_category <- ifelse(df$age <= 30, 'Young', ifelse(df$age > 30 & df$age <= 50, 'Mid-Age', 'Old'))
df$age_category <- as.factor(df$age_category)
summary(df$age_category)

# Remove unnecessary columns
df <- select(df, -day)

# Split the dataset into training and testing sets
set.seed(1000)
index <- sample(nrow(df), 0.70 * nrow(df), replace = FALSE)
train <- df[index, ]
test <- df[-index, ]

# Check distribution of the target variable in training and testing sets
table(train$y) / nrow(train)
table(test$y) / nrow(test)

# Build the logistic regression model
mod <- glm(y ~ ., data = train, family = 'binomial')
summary(mod)

# Feature selection using stepwise regression
step_mod <- step(mod, direction = 'both')

# Build the final logistic regression model
final_mod <- glm(formula = y ~ job + marital + default + education + balance + housing + loan + contact + month + duration + campaign + pdays + previous + poutcome + age_category, family = "binomial", data = train)
summary(final_mod)

# Model validation
pred <- ifelse(predict(final_mod, type = "response", newdata = test) >= 0.5, 1, 0)
pred <- as.factor(pred)

# Confusion matrix for model accuracy
confusionMatrix(pred, test$y, positive = "1")
