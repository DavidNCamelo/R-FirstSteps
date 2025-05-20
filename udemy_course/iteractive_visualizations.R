# Iteractive visualizations

# Required Libraries
library(ggplot2)
library(plotly)

# Chart1: Scatter chartwith default ggplot desing
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "Weight vs Consumption of Gasoline Relationship",
       x = "Weight (1000 lbs)", 
       y = "Miles by gallon",
       color = "Cilinders")

# Render the graph
ggplotly(p)

# chart2: Scatter chart implementing Plotly
p <- plot_ly(data = mtcars, x = ~wt, y = ~mpg, type = 'scatter', mode = 'markers',
             marker = list(size = 10, color = ~cyl, colorscale = 'Viridis')) %>%
  layout(title = "Weight vs Consumption of Gasoline Relationship",
         xaxis = list(title = "Weight (1000 lbs)"),
         yaxis = list(title = "Miles by gallon"))

p

# Creating interactive maps
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = -73.0832, lat = 7.0926, popup = "Somewhere Colombia")


# Level up, increasing the level a little bit
library(ggiraph)

# Create the graph
# Crear un gráfico de dispersión interactivo
p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point_interactive(aes(tooltip = paste("Weight:", wt, "<br>MPG:", mpg, "<br>Cilinders:", cyl)), # Personalized Tooltip
                         size = 4) +
  labs(title = "Weight vs Consumption of Gasoline Relationship",
       x = "Weight (1000 lbs)",
       y = "Miles by gallon",
       color = "Cilinders")

# Render with girafe()
girafe(ggobj = p, width_svg = 6, height_svg = 4)

# Interactive bar chart
# New dataset
df <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(10, 25, 15, 30)
)

# Create the chart
p <- ggplot(df, aes(x = category, y = value)) +
  geom_bar_interactive(stat = "identity",
                       aes(fill = category, # Highlighting the bars
                           tooltip = paste("value:", value), # Personalize tooltip
                           data_id = category),
                       width = 0.5) +
  labs(title = "Interactive bar chart")

# Render with girafe()
girafe(ggobj = p)


# New simulated data
df2 <- data.frame(
  date = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
  sales = cumsum(runif(12, min = 5, max = 20))
)

# Line and point interactive chart
p <- ggplot(df2, aes(x = date, y = sales)) +
  geom_line_interactive(aes(tooltip = paste("date:", date, "<br>sales:", sales)),
                        color = "blue", size = 1) +
  geom_point_interactive(aes(tooltip = paste("date:", date, "<br>sales:", sales)), size = 3, color = "red") +
  labs(title = "Monthly sales behavior",
       x = "date", y = "sales")

# Render with girafe()
girafe(ggobj = p)


