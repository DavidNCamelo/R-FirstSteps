# Udemy course projec

# Required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Import data
imbd_data <- read_csv("./R-FirstSteps/udemy_course/data/imdb_top_1000.csv")

# Omit null values
imbd_data <- na.omit(imbd_data)

# transform data
imbd_data$Released_Year <- as.numeric(imbd_data$Released_Year)
imbd_data$IMDB_Rating <- as.numeric(imbd_data$IMDB_Rating)
imbd_data$Main_Genre <- sapply(strsplit(imbd_data$Genre, ","), `[`, 1)

# Review summarise data
summary(imbd_data)


# Plotting data
# Qualification Histogram
ggplot(imbd_data, aes(x = IMDB_Rating)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "gray", alpha = 0.7) +
  labs(title = "Qualification Distribution IMBD", x = "Qualification", y = "Frequency") +
  theme_minimal()

# Qualification timeline evolution
ggplot(imbd_data, aes(x = Released_Year, y = IMDB_Rating)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Qualification Evolution", x = "Year", y = "Value") +
  theme_minimal()

# Movies by Gender
ggplot(imbd_data, aes(x = reorder(Main_Genre, -table(Main_Genre)[Main_Genre]))) +
  geom_bar(fill = "goldenrod", color = "gray") +
  labs(title = "Movies by Genre", x = 'Genre', y = 'Quantity') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Qualification distribution by genre
ggplot(imbd_data, aes(x = Main_Genre, y = IMDB_Rating, fill = Main_Genre)) +
  geom_boxplot() +
  labs(title = "Qualification Distibution by Year", x = "Genre", y = "IMDB Qualification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Votes qualification relationship
ggplot(imbd_data, aes(x = No_of_Vores, y = IMDB_Rating)) +
  geom_point(alpha = 0.5, color = "orangered4") +
  geom_smooth(method = "lm", color = "chartreuse4", se = FALSE) +
  labs(title = "Votes Qualification relationship", x = "Votes Quantity", y = "IMDB Qualification") + 
  theme_minimal()
