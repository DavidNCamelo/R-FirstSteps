library(dplyr)
library(ggplot2)
library(statsr)
arbt <- data("arbuthnot")
#Determine the dimensions of Dataset
dim(arbt)
#Quantity or Boys ang Girls in dataset
boys <- arbuthnot$boys
girls <- arbuthnot$girls
#Made aimple plot of the number of girls baptized per year with the command
ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point()

#Open a new dataset
data("present")
dim(present)
#Calculate total of births per year
present <- present %>% mutate(total = boys + girls)
#Calculate proportion os boys in dataset
present <- present %>%
  mutate(prop_boys = boys / (boys + girls))
#Add True or False according proportions
present <- present %>%
  mutate(prop_boys = boys / (boys + girls),
         mayor_proporcion = ifelse(prop_boys > (1 - prop_boys), TRUE, FALSE))

#Generate ratio between genders
present <- present %>%
  mutate(boy_to_girl_ratio = boys / girls)
#Plotting the previous
ggplot(data = present, aes(x = year, y = boy_to_girl_ratio)) + geom_point()

#View the year with more qutanity of years of Dataset
max_total <- present %>%
  mutate(total = boys + girls) %>%
  filter(total == max(total)) %>%
  select(year)


#New dataframe
data("nycflights")
#Creating a new variable where flights are headed by SFO on Febrary.
#Suppose we are interested in flights headed to San Francisco (SFO) in that month
sfo_feb_flights <- nycflights %>%
  filter(origin == "SFO" | dest == "SFO", month == 2)
#Make a histogram with arrivals delays
ggplot(data = sfo_feb_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "white") +
  labs(title = "Histogram of Arrival Delays for SFO Flights in February",
       x = "Arrival Delay (minutes)", y = "Frequency")

# Calculate summary statistics for arrival delays
summary_stats <- summary(sfo_feb_flights$arr_delay)

#Now we want group the new dataframe by carrier and calculate
#median and interquartile of this
sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_dd = median(arr_delay), 
            iqr_dd = IQR(arr_delay),
            sd_dd = sd(arr_delay), n = n())

#Average departure delay  from nyc per month
nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

#Median departure delay  from nyc per month
nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))


#Distribution of delays per month
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

#Lets Create anew Category based o dep_delay
#where the classification will be delayed and on time flights
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

#Now we will see the %On time for each NY Airport
nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))


# Mutate the data frame to add a new variable "avg_speed" in mph
nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time / 60))

# Find the tail number of the plane with the fastest avg_speed
nycflights_tailnum <- nycflights %>%
  filter(avg_speed == max(avg_speed)) %>%
  select(avg_speed, tailnum)

#Make a scatterplot with avg_speed vs distance
ggplot(data = nycflights, aes(x = distance, y = avg_speed)) +
  geom_point() +
  labs(x = "Distance", y = "Average Speed (mph)",
       title = "Scatterplot of Average Speed vs. Distance")

# To finish this fase, now we want to know proportion of flights  that 
#were `"delayed"` departing arrive `"on time". #We know the respective departure
#calssifacation, assuming the same things, we applied that conecpts on arrivals
nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed")) %>%
  group_by(dep_type) %>%
  summarise(ot_arr_rate = sum(arr_type == "on time") / n())


#Last Dataset for this section
data(kobe_basket)
dim(kobe_basket)

#Now we'll calculate Kobe_streak and see its distribution
kobe_streak <- calc_streak(kobe_basket$shot)
ggplot(data = kobe_streak, aes(x = length)) +
  geom_histogram(binwidth = 1)

#Coin Outcomes
coin_outcomes <- c("heads", "tails")
sample(coin_outcomes, size = 1, replace = TRUE)
sim_fair_coin <- sample(coin_outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)
sim_unfair_coin <- sample(coin_outcomes, size = 100, replace = TRUE, 
                          prob = c(0.2, 0.8))
shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 1, replace = TRUE)


#Using `calc_streak`, compute the streak lengths of `sim_basket`
#and save the results in a data frame called `sim_streak`. 
#Note that since the `sim_streak` object is just a vector and not a 
#variable in a data frame, we don't need to first select it from a data frame
#like we did earlier when we calculated the streak lengths for Kobe's shots
# Define the function to calculate streak lengths
# Define the function to calculate streak lengths
calc_streak <- function(x) {
  rle_result <- rle(x)
  lengths <- rle_result$lengths
  values <- rle_result$values
  streaks <- lengths[values == "H"] # Replace "H" with the desired outcome
  return(streaks)
}

# Define the shooting percentage and number of shots
shooting_percentage <- 0.45
num_shots <- 133

# Simulate multiple shots and calculate streak lengths
num_simulations <- 10000
sim_streaks <- replicate(num_simulations, {
  sim_basket <- sample(c("H", "M"), size = num_shots, replace = TRUE, prob = c(shooting_percentage, 1 - shooting_percentage))
  calc_streak(sim_basket)
})

# Calculate the typical streak length and longest streak length
typical_streak <- median(unlist(sim_streaks))
longest_streak <- max(sapply(sim_streaks, max))

# Plot the distribution of simulated streak lengths
hist(unlist(sim_streaks), main = "Distribution of Simulated Streak Lengths",
     xlab = "Streak Length", ylab = "Frequency", col = "lightblue")

# Add vertical lines for typical and longest streak lengths
abline(v = typical_streak, col = "red", lwd = 2)
abline(v = longest_streak, col = "blue", lwd = 2)

# Add a legend
legend("topright", legend = c("Typical Streak", "Longest Streak"),
       col = c("red", "blue"), lwd = 2)

# Print the typical and longest streak lengths
cat("Typical Streak Length:", typical_streak, "\n")
cat("Longest Streak Length:", longest_streak, "\n")

