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
