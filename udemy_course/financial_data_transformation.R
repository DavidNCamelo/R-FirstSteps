# Required Libraries
library(ggplot2)
library(dplyr)

# Importing data
data("economics")

# Review data sample
head(economics)
glimpse(economics)
summary(economics)

# Calculate unemployment rate
economics <- economics |>
  mutate(unemploy_rate = (unemploy / pop) * 100)

# Extract decade from the date
economics <- economics |>
  mutate(decade = paste0(floor(lubridate::year(date) / 10) * 10, "s"))


# Show summarise data by decades
economics |>
  group_by(decade) |>
  summarise(
    avg_pce = mean(pce),
    avg_desempleo = mean(unemploy_rate),
    avg_savings = mean(psavert)
  )

# Graph unemploy rate through years
ggplot(economics, aes(x = date, y = unemploy_rate)) +
  geom_line(color = 'tomato3') +
  labs(title = "Adjusted unemploy rate", y = "% unemploy")


# Graph saving distribution
boxplot(economics$psavert, main = "Personal savings percentage")
boxplot.stats(economics$psavert)$out

# Show the lowest savings
economics |>
  filter(psavert < 5) |>
  select(date, pce, psavert) |>
  arrange(date)
