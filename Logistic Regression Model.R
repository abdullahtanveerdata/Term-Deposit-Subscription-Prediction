#Step by step
#1) Data quality check
#2) Hypothesis setup e.g Hypothesis_1 <- c("living_area", "bedrooms", "bathrooms", 
                                          #"high_schools", "year_built", "sale_price")
#Hypothesis_2 <- data[Hypothesis_1] (Choose the variables that have the most correlation with the target variables)
#3) Check for outliers through multiple visualizations for the hypothesis variables
#4) Data correction and outliers treatment
#5) Data exploration visuals and correlations
#6) Regression models = 5
#7) RMSE Calculation
#8) Multicollinearity check
#9) Logistic regression
#I want the code to be modelled after the sample shared below if possible. The steps for the coding 
#should be explained in the comments and please make sure to tweak the visualizations and make them 
#better than the one shared below.

# For data manipulation and visualization
library(tidyverse)
# For advanced visualization
library(ggplot2)
library(caret) # For machine learning and data preprocessing
library(psych)
## Correlation Testing
#This section installs and loads the `Hmisc` package to perform correlation testing, which provides both 
#Pearson and Spearman correlation coefficients.
install.packages("Hmisc")
library(Hmisc)
#Made with Xodo PDF Reader and Editor
## Set Working Directory 
#This section allows you to set the working directory for the project files.
getwd()
setwd("E:/subscription")
## Read in the Data
#Here, we load the Austin Housing dataset from an Excel file and view a summary of the data along with
#its structure to understand the contents.
library(readxl)
data <- read_excel("termsv.xlsx")
summary(data) # Summary statistics of the dataset
describe(data)
str(data) # Structure of the dataset
## Data Quality Checks
### Check for Missing Values
#This section checks for missing values in the dataset and prints the count of missing values for each column.
missing_values <- sapply(data, function(x) sum(is.na(x)))
print("Missing values:")
print(missing_values) # Identifies columns with missing values
### Check for Duplicate Rows
#This code checks for duplicate rows in the dataset and prints the number of duplicates found.
duplicaterows <- sum(duplicated(data))
print(paste("Number of duplicate rows:", duplicaterows))
## Hypothesis Setup
#Made with Xodo PDF Reader and Editor
#This step sets up the variables of interest and subsets the data accordingly. The hypothesis only contain the categorical variables
Hypothesis_1 <- c("gender",  "occupation",  "salary",  "marital_status", "education_level" , "mortgage", "personal_loan", "car_insurance",  "life_insurance", "savings_account", "current_account", "subscribed" )
Hypothesis_a <- data[Hypothesis_1]
summary(Hypothesis_a) 

Hypothesis_2 <- c("age","last_contacted", "emp_var_rate","cons_price_idx", "cons_conf_idx","euribor_3m"  , "n_employed" )
#Hypothesis based on the numerical variables
Hypothesis_b <- data[Hypothesis_2]
summary(Hypothesis_b)


## Outlier Checks using Boxplots
## Boxplot contact duration##
summary(data$contact_duration) #to summarize the sale price
ggplot(data, aes(y = contact_duration)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", notch = TRUE) +
  labs(title = "Boxplot of contact Duration", y = "Contact Duration") +
  theme_minimal()
### Boxplot for emp var rate
#This boxplot shows the distribution of the `emp var rate` variable and visualizes any potential outliers.
boxplot(data$emp_var_rate,
        main = "Number of customers vs emp_var_rate",
        xlab = "Number of customers",
        ylab = "emp_var_rate",
        horizontal = TRUE, notch = FALSE)

###Boxplot for age
boxplot(data$age, 
        main = "age Distribution", ylab = "Count")
### Boxplot for previous contacts
#This boxplot visualizes the distribution of the `previous contacts` variable and checks for outliers.
boxplot(data$cons_price_idx,main = "Distribution of cons_price_idx",horizontal = TRUE, notch = TRUE)
### Boxplot for emp_v rate
#This boxplot shows the distribution of the cons_conf_idx variable and its relationship with housing prices.
boxplot(data$cons_conf_idx,
        main = "Distribution of cons_conf_idx",
        ylab="cons_conf_idx",
        horizontal = TRUE, notch = FALSE)
#Boxplot of n_employed
boxplot(data$n_employed,
        main = "Distribution of Number of Employes",
        ylab = "N_employee",
        horizontal = TRUE, notch = FALSE)

# Boxplot for euribor_3m
boxplot(data$euribor_3m,
        main = "Distribution of euribor_3m",
        ylab = "euribot 3m",
        horizontal = TRUE, notch = FALSE)

boxplot(data$last_contacted,
        main = "Distribution of last contacted",
        ylab = "Last contacted",
        horizontal = TRUE, notch = FALSE)
### Handling Outliers
# Addressing 'age' outliers
data$age[data$age > 70] <- NA
# Addressing 'contact duration' outliers
data$last_contacted[data$last_contacted > 100] <- NA

### Data Exploration Visuals
### Histogram for age
# Relationship between age and subscribed
ggplot(data , aes(x = age, fill = as.factor(subscribed))) +
  geom_histogram()+
  labs(
    title = "DISTRIBUTION OF AGE WITH SUBSCRIBED",
    x = "Age",
    y = "Number of Customers",
  ) +
  theme_minimal() 

##Relationship between n_employed and subscribed
ggplot(data , aes(x = n_employed, fill = as.factor(subscribed))) +
  geom_histogram()+
  labs(
    title = "DISTRIBUTION OF n_employed WITH SUBSCRIBED",
    x = "n_Employed",
    y = "Number of Customers",
  ) +
  theme_minimal()  

### Histogram for emp_var_rate and subscribed

ggplot(data , aes(x = emp_var_rate, fill = as.factor(subscribed))) +
  geom_histogram()+
  labs(
    title = "DISTRIBUTION OF emp_var_rate WITH SUBSCRIBED",
    x = "em_var_rate",
    y = "Number of Customers",
  ) +
  theme_minimal() 

# Relationship of age and gender by subscribed
ggplot(data, aes(x =age, fill = gender)) +
  geom_histogram() +
  labs(
    title = "Distribution of age by gender and subscription",
    x = "Number custom",
    y = "Sale Price ($)"
  ) +
  theme_minimal() # Ensures consistent styling for the plot
#Relation of Marital status with Subscription
ggplot(data, aes(x =marital_status, fill = as.factor(subscribed))) +
  geom_bar() +
  labs(
  title = "Relationship of marital Status and subscription",
  x = "Marital status",
  y = "Number of customers"
  ) +
  theme_minimal() # Ensures consistent styling for the plot
# Relationship of salary by subscribed
ggplot(data, aes(x =salary, fill = as.factor(subscribed))) +
  geom_bar() +
  labs(
  title = "Distribution of Salaryr and subscription",
  x = "Salary",
  y = "Number of customers"
  ) +
  theme_minimal() # Ensures consistent styling for the plot



library(car)
library(caret)


### Split Data by Seeding
# Split the data into training and testing sets
set.seed(12345) # Replace with your student number for reproducibility
train_index <- sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


Hypothesis_a <- train_data[Hypothesis_1]

# Numerical variables

Hypothesis_b <- train_data[Hypothesis_2]

### Build Logistic Regression Model
# Using categorical variables only
model1 <- glm(subscribed ~ gender + occupation + salary + marital_status + education_level + 
                mortgage + personal_loan + car_insurance + life_insurance + savings_account + 
                current_account, 
              data = train_data, 
              family = binomial)
summary(model1)
plot(model1)
# Using numerical variables only
model2 <- glm(subscribed ~ age + last_contacted + emp_var_rate + cons_price_idx + cons_conf_idx + 
                euribor_3m + n_employed, 
              data = train_data, 
              family = binomial)
summary(model2)
plot(model2)

# Combined model
model3 <- glm(subscribed ~ gender + occupation + salary + marital_status + education_level + 
                mortgage + personal_loan + car_insurance + life_insurance + savings_account + 
                current_account + age + last_contacted + emp_var_rate + cons_price_idx + 
                cons_conf_idx + euribor_3m + n_employed, 
              data = train_data, 
              family = binomial)
summary(model3)
plot(model3)


# Combined model
model4 <- glm(subscribed ~ gender + occupation + salary + marital_status + education_level + 
                mortgage + personal_loan + car_insurance + life_insurance + savings_account + 
                current_account + age + last_contacted , 
              data = train_data, 
              family = binomial)
summary(model4)

model5 <- glm(subscribed ~  salary + marital_status + education_level + 
                mortgage + personal_loan + car_insurance + life_insurance + savings_account + 
                current_account + age + last_contacted , 
              data = train_data, 
              family = binomial)
summary(model5)
plot(model5)
### Evaluate Model Performance
# Predict probabilities for test data
predictions <- predict(model1, newdata = test_data, type = "response")
predicted_classes1 <- ifelse(predictions > 0.5, 1, 0)
predictions <- predict(model2, newdata = test_data, type = "response")
predicted_classes2 <- ifelse(predictions > 0.5, 1, 0)
predictions <- predict(model3, newdata = test_data, type = "response")
predicted_classes3 <- ifelse(predictions > 0.5, 1, 0)
predictions <- predict(model4, newdata = test_data, type = "response")
predicted_classes4 <- ifelse(predictions > 0.5, 1, 0)
predictions <- predict(model5, newdata = test_data, type = "response")
predicted_classes5 <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix1 <- confusionMatrix(as.factor(predicted_classes1), as.factor(test_data$subscribed))
print(conf_matrix1)

conf_matrix2 <- confusionMatrix(as.factor(predicted_classes2), as.factor(test_data$subscribed))
print(conf_matrix2)

conf_matrix3 <- confusionMatrix(as.factor(predicted_classes3), as.factor(test_data$subscribed))
print(conf_matrix3)

conf_matrix4 <- confusionMatrix(as.factor(predicted_classes4), as.factor(test_data$subscribed))
print(conf_matrix4)

conf_matrix5 <- confusionMatrix(as.factor(predicted_classes5), as.factor(test_data$subscribed))
print(conf_matrix5)

# Calculate RMSE
rmse <- sqrt(mean((predictions - test_data$subscribed)^2, na.rm = TRUE))
print(paste("RMSE:", rmse))


### Correlation Analysis
# Pearson correlation for numerical variables
cor_matrix <- cor(Hypothesis_b, use = "complete.obs", method = "pearson")
print("Pearson's Correlation Matrix:")
print(cor_matrix)

library(pROC)

# Evaluate ROC and AUC for each model
# Function to calculate and plot ROC
evaluate_model_roc <- function(model, test_data, model_name) {
  # Predict probabilities
  predictions <- predict(model, newdata = test_data, type = "response")
  
  # Generate ROC curve
  roc_curve <- roc(test_data$subscribed, predictions)
  
  # Calculate AUC
  auc_value <- auc(roc_curve)
  
  # Plot ROC curve
  plot(roc_curve, col = "blue", main = paste("ROC Curve -", model_name), print.auc = TRUE)
  abline(a = 0, b = 1, lty = 2, col = "red") # Diagonal line
  
  return(auc_value)
}

# ROC for Model 1 (Categorical Variables Only)
auc_model1 <- evaluate_model_roc(model1, test_data, "Model 1 (Categorical)")

# ROC for Model 2 (Numerical Variables Only)
auc_model2 <- evaluate_model_roc(model2, test_data, "Model 2 (Numerical)")

# ROC for Model 3 (Combined Variables)
auc_model3 <- evaluate_model_roc(model3, test_data, "Model 3 (Combined)")

auc_model4 <- evaluate_model_roc(model4, test_data, "Model 4 (Combined)")

auc_model5 <- evaluate_model_roc(model5, test_data, "Model 5 (Combined)")

# Print AUC values
cat("AUC Values:\n")
cat("Model 1 (Categorical):", auc_model1, "\n")
cat("Model 2 (Numerical):", auc_model2, "\n")
cat("Model 3 (Combined):", auc_model3, "\n")
cat("Model 4 (Combined):", auc_model4, "\n")
cat("Model 5 (Combined):", auc_model5, "\n")

