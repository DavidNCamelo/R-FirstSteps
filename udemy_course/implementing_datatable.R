# Implementing data.table

# data.table: [i, j, by]

library(data.table)

# Create dataframe
dt <- data.table(
  id = 1:10,
  nanme = c(
    "Ana",
    "Luis",
    "Marta",
    "Juan",
    "Lucía",
    "Carlos",
    "Sofía",
    "Luis",
    "Ana",
    "Marta"
  ),
  group = sample(c("A", "B"), 10, replace = TRUE),
  grade = sample(10:20, 10, replace = TRUE)
)

# Filtering data

dt[grade > 15]

dt[nanme == 'Ana']

# Selecting columns

dt[, .(nanme, grade)]

# Selecting and altering
dt[, .(nanme, grade, double_grade = grade * 2)]

# Selection, altering and grouping
dt[, .(mean = mean(grade)), by = nanme]

# Counting
dt[, .N, by = group]

# Add column
dt[, adjusted_grade := grade + 1]

dt[, double_grade := grade * 2]

# New data to implement .SD
dt2 <- data.table(
  student = rep(c("Ana", "Luis", "Marta", "Juan"), each = 3),
  course = rep(c("Math", "History", "Physics"), times = 4),
  year = rep(2021:2023, times = 4),
  grade1 = sample(10:20, 12, replace = TRUE),
  grade2 = sample(10:20, 12, replace = TRUE)
)

# Counting by students
dt2[, .N, by = student]

# Adding calculated columns by specific group and selecting just specific columns
dt2[, lapply(.SD, mean), by = student, .SDcols = c("grade1", "grade2")]

# Adding calculated columns by specific group
dt2[, .(max_grade1 = max(grade1), min_grade2 = min(grade2)), by = course]

# selecting just numeric columns
cols_num <- names(dt2)[sapply(dt2, is.numeric)]

# Adding calculated columns by specific group and selecting just numeric columns
dt2[, lapply(.SD, mean), by = course, .SDcols = cols_num]

# Adding calculated column to the original dataset
dt2[, mean_grade := rowMeans(.SD), .SDcols = c("grade1", "grade2")]

# .SD : extremadamente útil para aplicar funciones personalizadas por grupo, sin necesidad de pivotear.

# .N  : es una forma rápida de contar observaciones por grupo.

# Juntos, .SD, .N y .SDcols te permiten replicar muchos casos de uso de dplyr
