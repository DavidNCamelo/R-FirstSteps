# This is the second section of the R4DS 2° Edition book
# Focused on expand the learned foundations about data visualization

# Book example
ggplot2::mpg

ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, color = class)) +
  ggplot2::geom_point()

# Right
ggplot2::ggplot(ggplot2::mpg, ggplot2::aes(x = displ, y = hwy, shape = class)) +
  ggplot2::geom_point()
