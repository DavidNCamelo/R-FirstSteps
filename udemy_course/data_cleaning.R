# Cleaning data
# Required libraries
library(readr)
library(dplyr)

# Load data
grades <- read.csv("./R-FirstSteps/udemy_course/data/dataset_cap3.csv")

# Review outliers
outliers <- boxplot.stats(grades$nota)$out

# Filtering
outliers_grades <- grades %>%
  filter(nota %in% outliers)

# Q1 - 1.5IQR and Q3 + 1.5IQR as outliers reference

clean_grades <- grades %>%
  filter(!(nota %in% outliers))

# Stats treating to review outliers by z-score

z_grades <- grades %>%
  mutate(
    mean = mean(nota, na.rm = TRUE),
    sd = sd(nota, na.rm = TRUE),
    z_score = (nota - mean) / sd
  )

# Outliers
z_grades %>%
  filter(abs(z_score) > 3)
