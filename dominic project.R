library(readxl)
library(tidyverse)
library(ggplot2)
library(caret)
# Load the HR data
HR_Data <- read_excel("HR Data.xlsx", sheet = "HR data")
# Initial exploration
head(HR_Data)
str(HR_Data)
summary(HR_Data)
# Attrition rate by department
attrition_by_dept <- HR_Data %>%
  group_by(Department, Attrition) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(attrition_by_dept, aes(x = Department, y = percentage, fill = Attrition)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition Rate by Department", x = "Department", y = "Percentage") +
  theme_minimal()

# Key factors correlated with attrition
attrition_factors <- HR_Data %>%
  select(Attrition, `Job Satisfaction`, `Monthly Income`, `Years At Company`, 
         `Work Life Balance`, `Years Since Last Promotion`, `Business Travel`) %>%
  mutate(Attrition = as.factor(Attrition))

# Correlation matrix for numeric variables
cor_matrix <- attrition_factors %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

# Job satisfaction by job role
job_satisfaction <- HR_Data %>%
  group_by(`Job Role`, `Job Satisfaction`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(job_satisfaction, aes(x = `Job Role`, y = percentage, fill = as.factor(`Job Satisfaction`))) +
  geom_bar(stat = "identity") +
  labs(title = "Job Satisfaction by Role", x = "Job Role", y = "Percentage", fill = "Satisfaction Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Satisfaction vs monthly income
ggplot(HR_Data, aes(x = `Monthly Income`, y = `Job Satisfaction`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Job Satisfaction vs Monthly Income", x = "Monthly Income", y = "Job Satisfaction") +
  theme_minimal()

# Income distribution by department and gender
ggplot(HR_Data, aes(x = Department, y = `Monthly Income`, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Monthly Income Distribution by Department and Gender", 
       x = "Department", y = "Monthly Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Corrected Salary hike analysis
salary_hike_analysis <- HR_Data %>%
  group_by(Department, `Performance Rating`) %>%
  summarise(avg_salary_hike = mean(`Percent Salary Hike`, na.rm = TRUE),
            avg_monthly_income = mean(`Monthly Income`, na.rm = TRUE))

ggplot(salary_hike_analysis, aes(x = factor(`Performance Rating`), y = avg_salary_hike, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Salary Hike by Performance Rating", 
       x = "Performance Rating", y = "Average Salary Hike (%)") +
  theme_minimal()
# Work-life balance by job role
wlb_by_role <- HR_Data %>%
  group_by(`Job Role`, `Work Life Balance`) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(wlb_by_role, aes(x = `Job Role`, y = percentage, fill = as.factor(`Work Life Balance`))) +
  geom_bar(stat = "identity") +
  labs(title = "Work-Life Balance by Job Role", 
       x = "Job Role", y = "Percentage", fill = "Work-Life Balance Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Overtime analysis
overtime_analysis <- HR_Data %>%
  group_by(`Over Time`, Attrition) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(overtime_analysis, aes(x = `Over Time`, y = percentage, fill = Attrition)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition Rate by Overtime Status", x = "Overtime", y = "Percentage") +
  theme_minimal()
# Prepare data for modeling
hr_model_data <- HR_Data %>%
  select(Attrition, Age, Gender, Department, `Job Role`, `Marital Status`, 
         `Monthly Income`, `Years At Company`, `Years Since Last Promotion`,
         `Work Life Balance`, `Job Satisfaction`, `Business Travel`, `Over Time`) %>%
  mutate_if(is.character, as.factor)

# Split data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(hr_model_data$Attrition, p = 0.8, list = FALSE)
train_data <- hr_model_data[trainIndex, ]
test_data <- hr_model_data[-trainIndex, ]

# Train logistic regression model
model <- train(Attrition ~ ., 
               data = train_data, 
               method = "glm", 
               family = "binomial",
               trControl = trainControl(method = "cv", number = 5))

# Model evaluation
predictions <- predict(model, newdata = test_data)
confusionMatrix(predictions, test_data$Attrition)

# Feature importance
varImp(model)
# Time since last promotion analysis
promotion_analysis <- HR_Data %>%
  group_by(Department, `Job Level`) %>%
  summarise(avg_years_since_promotion = mean(`Years Since Last Promotion`),
            avg_years_at_company = mean(`Years At Company`))

ggplot(promotion_analysis, aes(x = `Job Level`, y = avg_years_since_promotion, fill = Department)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Years Since Last Promotion by Job Level", 
       x = "Job Level", y = "Average Years Since Last Promotion") +
  theme_minimal()

# Relationship between promotions and job satisfaction
ggplot(HR_Data, aes(x = `Years Since Last Promotion`, y = `Job Satisfaction`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Job Satisfaction vs Years Since Last Promotion", 
       x = "Years Since Last Promotion", y = "Job Satisfaction") +
  theme_minimal()
# Age distribution by attrition
ggplot(HR_Data, aes(x = `CF_age band`, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition Rate by Age Band", x = "Age Band", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Marital status analysis
marital_analysis <- HR_Data %>%
  group_by(`Marital Status`, Attrition) %>%
  summarise(count = n()) %>%
  mutate(percentage = count/sum(count)*100)

ggplot(marital_analysis, aes(x = `Marital Status`, y = percentage, fill = Attrition)) +
  geom_bar(stat = "identity") +
  labs(title = "Attrition Rate by Marital Status", x = "Marital Status", y = "Percentage") +
  theme_minimal()
