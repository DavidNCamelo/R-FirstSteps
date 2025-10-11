# Based in the section Whole game of R for Data Science book

#=====================================#
#         1. Data Visualization       #
#=====================================#

# Talking about first steps, let's begin about implementing ggplot

# Required libraries
library(palmerpenguins)

penguins

# Exploring dataset
dplyr::glimpse(penguins)

# R4DS gives a great introduction about how to use ggplot2, explaining layer building

# Starting by a empty canvas
ggplot2::ggplot(data = penguins)

# Next, we need to tell ggplot() how the information from our data will be visually represented.
# The mapping argument of the ggplot() function defines how variables in your dataset are mapped to visual properties (aesthetics) of your plot.
# The mapping argument is always defined in the aes() function, and the x and y arguments of aes() specify which variables to map to the x and y axes.

# Thi is also an empty canvas but more structured
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
)

# This is because we have not yet articulated, in our code, how to represent the observations from our data frame on our plot.
# To do so, we need to define a geom: the geometrical object that a plot uses to represent data.
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
) +
  ggplot2::geom_point()

# Now is possible to add aesthetics and new layers over the graph
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = species
  ) # Color give aesthetics in this case
) +
  ggplot2::geom_point()

# Now let’s add one more layer: a smooth curve displaying the relationship between body mass and flipper length.
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(
    x = flipper_length_mm,
    y = body_mass_g,
    color = species
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

# The one above represents a global option, that's related to color mapping assigned
# that's why we see smooth by groups, but there are way to include a singel smooth for all series
# let's see it
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
) +
  ggplot2::geom_point(mapping = ggplot2::aes(color = species)) + # The mapping color was traslated here
  ggplot2::geom_smooth(method = "lm")

# Aesthetics also include points drawing, citation
# It’s generally not a good idea to represent information using only colors on a plot,
# as people perceive colors differently due to color blindness or other color vision differences.
# Therefore, in addition to color, we can also map species to the shape aesthetic.
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(color = species, shape = species)
  ) + # By shape we change draws
  ggplot2::geom_smooth(method = "lm")

# And what about if we include more things (useful for researching)
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = flipper_length_mm, y = body_mass_g)
) +
  ggplot2::geom_point(ggplot2::aes(color = species, shape = species)) +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  ) +
  ggthemes::scale_color_colorblind()


# Exercise

# How many rows are in penguins? How many columns?
# R: Implementing glimpse it'll show the answer 344 rows and 8 columns
dplyr::glimpse(penguins)

# Make a scatterplot of bill_depth_mm vs. bill_length_mm.
# That is, make a scatterplot with bill_depth_mm on the y-axis and bill_length_mm on the x-axis.
# Describe the relationship between these two variables.

ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = bill_depth_mm, y = bill_length_mm)
) +
  ggplot2::geom_point()


#What happens if you make a scatterplot of species vs. bill_depth_mm? What might be a better choice of geom?
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = species, y = bill_length_mm)
) +
  ggplot2::geom_point(mapping = ggplot2::aes(color = species))

# This show distribution by spcies, so is better use a boxplot or area
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = species, y = bill_length_mm)
) +
  ggplot2::geom_boxplot(mapping = ggplot2::aes(fill = species))

# Or violin
ggplot2::ggplot(
  data = penguins,
  mapping = ggplot2::aes(x = species, y = bill_length_mm)
) +
  ggplot2::stat_ydensity(mapping = ggplot2::aes(fill = species))

# 4 data transdformation
# Now the data for exersices ir flights

library(nycflights13)

dplyr::glimpse(flights)

# 1 In a single pipeline for each condition, find all flights that meet the condition:

# a. Had an arrival delay of two or more hours

flights |>
  dplyr::filter(arr_delay >= 120) |>
  dplyr::arrange(desc(arr_delay))

# b. Flew to Houston (IAH or HOU)
flights |>
  dplyr::filter(dest %in% c('IAH', 'HOU'))

# c. Were operated by United, American, or Delta
flights |>
  dplyr::filter(carrier %in% c("UA", "DL", "AA"))

# 3. Fastest flights, measured as miles per hour:

flights |>
  dplyr::mutate(speed = distance / (air_time / 60)) |>
  dplyr::arrange(desc(speed)) |>
  dplyr::relocate(speed)


# 3.6 Case study: aggregates and sample size

batters <- Lahman::Batting |>
  dplyr::group_by(playerID) |>
  dplyr::summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters


# Graph

batters |>
  dplyr::filter(n > 100) |>
  ggplot2::ggplot(ggplot2::aes(x = n, y = performance)) +
  ggplot2::geom_point(alpha = 1 / 10) +
  ggplot2::geom_smooth(se = FALSE) # Try to visualize patterns


# Workflow style: Restyling

# Original
# As I'm using Positron and Air extention + setting json added component
# This extensian made restyling automatically
# -------------------------
# flights|>filter(dest=="IAH")|>group_by(year,month,day)|>summarize(n=n(),
# delay=mean(arr_delay,na.rm=TRUE))|>filter(n>10)

# flights|>filter(carrier=="UA",dest%in%c("IAH","HOU"),sched_dep_time>
# 0900,sched_arr_time<2000)|>group_by(flight)|>summarize(delay=mean(
# arr_delay,na.rm=TRUE),cancelled=sum(is.na(arr_delay)),n=n())|>filter(n>10)
# ------------------------------

# Result
# Notice that I'm implementing another kind of library writing
# That consist in library::function
flights |>
  dplyr::filter(dest == "IAH") |>
  dplyr::group_by(year, month, day) |>
  dplyr::summarize(
    n = dplyr::n(),
    delay = mean(arr_delay, na.rm = TRUE)
  ) |>
  dplyr::filter(n > 10)

flights |>
  dplyr::filter(
    carrier == "UA",
    dest %in% c("IAH", "HOU"),
    sched_dep_time > 0900,
    sched_arr_time < 2000
  ) |>
  dplyr::group_by(flight) |>
  dplyr::summarize(
    delay = mean(
      arr_delay,
      na.rm = TRUE
    ),
    cancelled = sum(is.na(arr_delay)),
    n = dplyr::n()
  ) |>
  dplyr::filter(n > 10)


# Import data

students <- readr::read_csv("https://pos.it/r4ds-students-csv")

students

# Cleaning null values
students <- readr::read_csv(
  "https://pos.it/r4ds-students-csv",
  na = c("N/A", "")
)

# Normalizing columns
students |> janitor::clean_names()


dplyr::glimpse(students)

# Chage types
students2 <- students |>
  janitor::clean_names() |>
  dplyr::mutate(meal_plan = factor(meal_plan))

# review new types
dplyr::glimpse(students2)

# Or even content

students <- students |>
  janitor::clean_names() |>
  dplyr::mutate(
    meal_plan = factor(meal_plan),
    age = readr::parse_number(dplyr::if_else(age == "five", "5", age))
  )
