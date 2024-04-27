# Load necessary libraries
library(tidyverse)  # for data manipulation and visualization
library(dplyr)
library(ggplot2)

# Set paths to your datasets
dataset_path_1 <- "/Users/vyduryaraghu/Documents/dissertation/dissertationdataset/dataset1.csv"

# Load the datasets
dataset1 <- read_csv(dataset_path_1)

# Check the first few rows of dataset1
head(dataset1)


# Let's say we want to look at the distribution of genders in dataset 1
gender_distribution <- dataset1 %>%
  count(gender) %>%
  ggplot(aes(x = gender, y = n, fill = gender)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gender Distribution in Dataset 1",
       x = "Gender",
       y = "Count")

# Print the plot to view it
print(gender_distribution)

# Display structure of dataset1
str(dataset1)

# Display summary of dataset1
summary(dataset1)

# Show the first few rows of dataset1
head(dataset1)

# Show the last few rows of dataset1
tail(dataset1)

# View the dataset in an interactive viewer
View(dataset1)

# Count missing values in each column of dataset1
colSums(is.na(dataset1))

# Show the unique values of a specific column, e.g., 'gender'
unique(dataset1$gender)

# Create a frequency table for the 'gender' column
table(dataset1$gender)

# Dimensions of the dataset
dim(dataset1)

# Names of the columns
names(dataset1)
# Convert categorical variables to factors
dataset1$gender <- as.factor(dataset1$gender)
dataset1$ind_degree <- as.factor(dataset1$ind_degree)
dataset1$company <- as.factor(dataset1$company)

# Removing unnecessary columns
dataset1_clean <- select(dataset1, -c(age, nationality, sport))

# Checking the structure after modifications
str(dataset1_clean)
# Correctly convert 'ind-degree' to a factor
dataset1_clean$`ind-degree` <- as.factor(dataset1_clean$`ind-degree`)
# Visualize the distribution of gender
ggplot(dataset1_clean, aes(x = gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal()
# Visualize gender distribution by company
ggplot(dataset1_clean, aes(x = company, fill = gender)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Gender Distribution by Company", x = "Company", y = "Count") +
  theme_minimal()

# Calculate hiring rates by gender
hiring_rates_gender <- dataset1_clean %>%
  group_by(gender) %>%
  summarise(Hired = sum(decision, na.rm = TRUE),
            Total = n(),
            Rate = Hired / Total)

# View the hiring rates
print(hiring_rates_gender)
# Visualize hiring rates by gender
ggplot(hiring_rates_gender, aes(x = gender, y = Rate, fill = gender)) +
  geom_col() +
  labs(title = "Hiring Rates by Gender", x = "Gender", y = "Hiring Rate") +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal()
# Visualize university grade distribution by gender
ggplot(dataset1_clean, aes(x = gender, y = `ind-university_grade`, fill = gender)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel2") +
  labs(title = "University Grade Distribution by Gender", x = "Gender", y = "University Grade") +
  theme_minimal()


# Summarize hiring decisions by gender and company
gender_bias_summary <- dataset1 %>%
  group_by(company, gender) %>%
  summarise(hired = sum(decision, na.rm = TRUE),
            not_hired = sum(!decision, na.rm = TRUE),
            total = n()) %>%
  ungroup()

# Visualize the hiring decisions by gender for each company
ggplot(gender_bias_summary, aes(x = company, y = total, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hiring Decisions by Gender and Company", x = "Company", y = "Total Applicants") +
  theme_minimal()

# Conduct a Chi-squared test for each company to see if there's a significant difference
# in hiring decisions based on gender
chi_squared_tests <- lapply(split(dataset1, dataset1$company), function(data) {
  table <- table(data$gender, data$decision)
  chisq.test(table)
})

chi_squared_tests
# Prepare the dataset excluding gender, age, nationality, and sport
dataset_without_gender <- dataset1 %>%
  select(-c(gender, age, nationality, sport))

# Convert logical columns to numeric for logistic regression
dataset_without_gender <- dataset_without_gender %>%
  mutate(across(where(is.logical), as.numeric))

# Logistic Regression Model
model_without_gender <- glm(decision ~ . - Id - company, data = dataset_without_gender, family = "binomial")

# Summary of the model to check the influence of each variable
summary(model_without_gender)

# You can also consider comparing this with a model that includes gender for reference.
model_with_gender <- glm(decision ~ . - Id - company, data = dataset1 %>% mutate(across(where(is.logical), as.numeric)), family = "binomial")

# Summary of the model with gender
summary(model_with_gender)
# Adding interaction terms between gender and other significant predictors
model_interaction <- glm(decision ~ gender*`ind-university_grade` + gender*`ind-international_exp` + gender*`ind-languages` + gender*`ind-degree` + . - Id - company,
                         data = dataset1 %>% mutate(across(where(is.logical), as.numeric)),
                         family = "binomial")


summary(model_interaction)
# Filter dataset by gender and run analysis/model for each subgroup
dataset_female <- dataset1 %>% filter(gender == "female")
dataset_male <- dataset1 %>% filter(gender == "male")

# Example model for females
model_female <- glm(decision ~ . - Id - company - gender, data = dataset_female %>% mutate(across(where(is.logical), as.numeric)), family = "binomial")
summary(model_female)

# Repeat for males and compare
# Example: Distribution of university grades by gender
ggplot(dataset1, aes(x = `ind-university_grade`, fill = gender)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of University Grades by Gender", x = "University Grade", y = "Count")

library(randomForest)

# Define predictors and response variable explicitly
# Correcting the column names to those actually in dataset1
predictors <- dataset1[, c("gender", "ind-university_grade", "ind-debateclub", "ind-programming_exp",
                           "ind-international_exp", "ind-entrepeneur_exp", "ind-languages", "ind-exact_study", "ind-degree")]

response <- dataset1$decision
response <- as.factor(dataset1$decision)

# Ensure 'gender' and 'ind_degree' are factors if they aren't already
predictors$gender <- as.factor(predictors$gender)
predictors$ind_degree <- as.factor(predictors$ind_degree)

# Convert logicals to numeric explicitly
predictors <- data.frame(lapply(predictors, function(x) if(is.logical(x)) as.numeric(x) else x))

# Run randomForest
set.seed(123)
rf_model_explicit <- randomForest(x = predictors, y = response, ntree = 100, importance = TRUE)

print(rf_model_explicit)
varImpPlot(rf_model_explicit)

install.packages("gbm")

library(gbm)

# Create a copy of the dataset for modifications
dataset_prepared <- dataset1
# This approach ensures FALSE becomes 0 and TRUE becomes 1 directly
dataset_prepared$decision <- as.integer(dataset_prepared$decision) - 1

# Ensure other categorical variables are in the correct format
dataset_prepared$gender <- as.factor(dataset_prepared$gender)
dataset_prepared$`ind-degree` <- as.factor(dataset_prepared$`ind-degree`)

# Exclude 'Id' and 'company' from modeling, and convert character columns to factors if not already done
dataset_prepared <- dataset_prepared %>%
  dplyr::select(-Id, -company) %>%
  dplyr::mutate_if(is.character, as.factor)

str(dataset_prepared)
dataset_prepared$`ind-debateclub` <- as.numeric(dataset_prepared$`ind-debateclub`)
dataset_prepared$`ind-programming_exp` <- as.numeric(dataset_prepared$`ind-programming_exp`)
dataset_prepared$`ind-international_exp` <- as.numeric(dataset_prepared$`ind-international_exp`)
dataset_prepared$`ind-entrepeneur_exp` <- as.numeric(dataset_prepared$`ind-entrepeneur_exp`)
dataset_prepared$`ind-exact_study` <- as.numeric(dataset_prepared$`ind-exact_study`)

# Fit the GBM model on the prepared dataset
gbm_model <- gbm::gbm(decision ~ .,
                      data = dataset_prepared,
                      distribution = "bernoulli", # For binary outcomes
                      n.trees = 100,  # Number of trees to grow
                      interaction.depth = 1,  # Depth of tree interactions
                      shrinkage = 0.01,  # Learning rate
                      verbose = FALSE)  # Turn off verbose output

# After fitting the model, examine variable importance
summary(gbm_model)
# Fit the logistic regression model
# Fit the logistic regression model without using '...'
logistic_model <- glm(decision ~ gender + age + `ind-university_grade` + `ind-languages` + `ind-degree`,
                      family = binomial(link = "logit"),
                      data = dataset_prepared)

# Now, let's summarize the model to examine the coefficients and their significance
summary(logistic_model)


# Summarize the model to examine the coefficients and significance
summary(logistic_model)

# Add interaction between gender and other significant predictors
logistic_model_interaction <- glm(decision ~ gender * age + gender * `ind-university_grade` + gender * `ind-languages` + gender * `ind-degree`,
                                  family = binomial(link = "logit"),
                                  data = dataset_prepared)
summary(logistic_model_interaction)
library(pROC)

# Predict probabilities
probabilities <- predict(logistic_model, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

# Actual outcomes
actual_classes <- dataset_prepared$decision

# Confusion matrix
table(Predicted = predicted_classes, Actual = actual_classes)

# ROC curve and AUC
roc_response <- roc(actual_classes, probabilities)
plot(roc_response)
auc(roc_response)

# Check VIF
library(car)
vif(logistic_model)
library(caret)
folds <- createFolds(dataset_prepared$decision, k = 5, list = TRUE, returnTrain = TRUE)
cv_results <- lapply(folds, function(trainIndex) {
  trainingData <- dataset_prepared[trainIndex, ]
  testData <- dataset_prepared[-trainIndex, ]

  model <- glm(decision ~ gender + age + `ind-university_grade` + `ind-languages` + `ind-degree`,
               family = binomial(link = "logit"),
               data = trainingData)

  pred <- predict(model, newdata = testData, type = "response")
  actual <- testData$decision

  # Calculate some metric (e.g., AUC)
  roc_auc <- pROC::auc(pROC::roc(actual, pred))
  return(roc_auc)
})

mean_auc <- mean(sapply(cv_results, function(x) x$auc))
# Calculate the mean AUC directly from the cv_results
mean_auc <- mean(sapply(cv_results, identity))
print(mean_auc)

