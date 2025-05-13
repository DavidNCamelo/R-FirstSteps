# Data transformation

# Required Libraries
library(tibble)
library(dplyr)
library(tidyr)

# Create dataset
data <- tribble(
  ~student, ~id_full, ~grade,
  "Ana",    "2021_Math",    15,
  "Luis",   "2022_History", 14,
  "Marta",  "2023_Physics", 18
)

# Separating values
separated_data <- data %>%
  separate(
    col = id_full,
    into = c("year", "course"),
    sep = "_"
  )

# Values fusion
unite_data <- data %>%
  unite(
    col = student_grade,
    student, grade,
    sep = "_"
  )

# Separate and keep transformed column
data %>%
  separate(
    col = id_full,
    into = c("year", "course"),
    sep = "_",
    remove = FALSE
  )

# Unite and delete transformed column
data %>%
  unite(
    col = student_grade,
    student, grade,
    sep = "_",
    remove = FALSE
  )