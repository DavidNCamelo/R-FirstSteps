# This is the second section of the R4DS 2° Edition book
# Focused on expand the learned foundations about data visualization

# Book example
ggplot2::mpg

ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, color = class)) +
  ggplot2::geom_point()

# Right
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, shape = class)) +
  ggplot2::geom_point()


# Similarly, we can map class to size or alpha aesthetics as well,
# which control the size and the transparency of the points, respectively.

# Left
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, size = class)) +
  ggplot2::geom_point()
#> Warning: Using size for a discrete variable is not advised.

# Right
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, alpha = class)) +
  ggplot2::geom_point()
#> Warning: Using alpha for a discrete variable is not advised.

ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy)) +
  ggplot2::geom_point(color = 'pink', shape = 2)


# What happens if you map an aesthetic to something other than a variable name,
#  like aes(color = displ < 5)? Note, you’ll also need to specify x and y.

ggplot2::ggplot(
  ggplot2::mpg,
  ggplot2::aes(x = displ, y = hwy, color = displ < 5)
) +
  ggplot2::geom_point(shape = 2)

# Rt: the result create categories about the displ values
# if the condition it's true or false

# Ways to draw smooth lines
# Continuous and segmented
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, shape = drv)) +
  ggplot2::geom_smooth()

# Grouping and changing line type
ggplot2::ggplot(
  ggplot2::mpg,
  ggplot2::aes(x = displ, y = hwy, linetype = drv)
) +
  ggplot2::geom_smooth()

# Combining points and smooth, and even including groups
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, color = drv)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(ggplot2::aes(linetype = drv))

# Cointinuous line with points and cetegories
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy)) +
  ggplot2::geom_point(ggplot2::aes(color = class)) +
  ggplot2::geom_smooth()
