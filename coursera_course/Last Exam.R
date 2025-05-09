#Little Practice
p <- 0.3
n <- 50

se <- sqrt((p * (1 - p)) / n)
se

p1 <- 0.4931
n1 <- 144
p2 <- 0.5758
n2 <- 389

se <- sqrt((p1 * (1 - p1) / n1) + (p2 * (1 - p2) / n2))
se


# Create a contingency table
smoking_status <- c("smoker", "non-smoker")
lung_cancer <- c("lung cancer", "not sure")
observed <- matrix(c(647, 2, 622, 27), nrow = 2, byrow = TRUE, dimnames = list(lung_cancer, smoking_status))

# Perform a chi-square test
result <- chisq.test(observed)

# Get the p-value
p_value <- result$p.value

# Print the p-value
p_value
library(infer)

library(statsr)
library(dplyr)
library(ggplot2)


data(nc)
str(nc)
summary(nc$gained)
# Create a data frame with the variables 'habit' and 'weight'
habit_weight <- nc[, c('habit', 'weight')]

# Create side-by-side boxplots
ggplot(habit_weight, aes(x = habit, y = weight, fill = habit)) +
  geom_boxplot() +
  xlab('Smoking Habit') +
  ylab('Birth Weight') +
  labs(fill = 'Smoking Habit')

nc %>%
  group_by(habit) %>%
  summarise(mean_weight = mean(weight))

inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")

# Create side-by-side boxplots and confidence interval
ggplot(habit_weight, aes(x = habit, y = weight, fill = habit)) +
  geom_boxplot() +
  geom_errorbar(stat = "summary", fun.data = "mean_cl_normal", width = 0.2) +
  xlab('Smoking Habit') +
  ylab('Birth Weight') +
  labs(fill = 'Smoking Habit')


inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ci", 
          method = "theoretical", order = c("smoker","nonsmoker"))


# Calculate 99% confidence interval for the average length of pregnancies

ci_weeks <- mean_ci(nc$weeks, conf_level = 0.99)

ci_weeks

# Create a box plot of mother's age
# Identify unique categories in the "mature" variable
unique_categories <- unique(nc$mature)

# Loop through the categories and find the corresponding age ranges
for (category in unique_categories) {
  age_range <- range(nc$mage[nc$mature == category])
  print(paste("Category:", category))
  print(paste("Age Range:", age_range))
}

