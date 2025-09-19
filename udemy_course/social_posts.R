# Social Networks treatment: workig with relational data
# Required libraies
library(readr)
library(dplyr)
library(lubridate)

# Importing data
users <- read.csv("./R-FirstSteps/udemy_course/data/users.csv")
posts <- read.csv("./R-FirstSteps/udemy_course/data/posts.csv")
likes <- read.csv("./R-FirstSteps/udemy_course/data/likes.csv")

# Joining files
likes_full <- likes |>
  left_join(users, by = 'user_id') |>
  left_join(posts |> rename(date_post = fecha), by = 'post_id')

# Summarise info
likes_full |>
  count(nombre, sort = TRUE)

# Summarisig data through join
likes_full |>
  count(post_id, sort = TRUE) |>
  left_join(posts, by = 'post_id') |>
  select(post_id, contenido, n)

# Transformin data
posts <- posts |>
  mutate(
    hashtag = stringr::str_extract(contenido, '#\\w+'),
    clear_content = stringr::str_remove(contenido, '#\\w+')
  )

# Graphing
likes |>
  mutate(week = floor_date(ymd(fecha), unit = 'week')) |>
  count(week) |>
  ggplot(aes(x = week, y = n)) +
  geom_line(color = 'orange') +
  labs(title = 'Likes per week', y = 'Likes Quantity')

# Counting and graphing likes per posts
likes_per_post <- likes |>
  count(post_id)

post_with_likes <- posts |>
  left_join(likes_per_post, by = 'post_id') |>
  left_join(users, by = 'user_id')

# Render graph
p <- post_with_likes |>
  group_by(nombre) |>
  summarise(
    posts = n(),
    total_likes = sum(n, na.rm = TRUE)
  ) |>
  arrange(desc(total_likes))

# Pivot table
p_long <- p |>
  tidyr::pivot_longer(
    cols = c(posts, total_likes),
    names_to = "variable",
    values_to = "value"
  )

# Grouped chart
ggplot(p_long, aes(x = nombre, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(x = "Name", y = "Quantity", fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
