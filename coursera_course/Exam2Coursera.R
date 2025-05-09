#A little new practice exercise

min_value <- 147.2  # Example min valor
q1_value <- 163.8   # example first quartile
median_value <- 170.3   # Example median value
mean_value <- 171.1   # Example mean value
sd_value <- 9.4  # Example Standard Deviation Value
q3_value <- 177.8   # Example third quartile
max_value <- 198.1   # Example max value

n <- 507  # Example sample size

variance <- ((max_value - min_value) / 6)^2   # Calculate Variance
bias_squared <- ((q1_value - min_value) + 
                   (median_value - q1_value) + 
                   (mean_value - median_value) + 
                   (q3_value - mean_value) + 
                   (max_value - q3_value))^2 / 4   # Calculate bias squared

mse <- (variance + bias_squared) / n   # Calculate MSE
standard_error <- sd_value / sqrt(n)

#Second concep example
sample_mean <- 415   # Average increase in percentage
sample_sd <- 220   # Standard deviation in percentage
sample_size <- 100   # Sample size

confidence_level <- 0.95   # Confidence level

margin_of_error <- qt((1 - confidence_level) / 2, df = sample_size - 1) * 
  (sample_sd / sqrt(sample_size))

lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

confidence_interval <- c(lower_bound, upper_bound)

print(confidence_interval)

#Third version concept example
sample_mean3 <- 30.69   # Average increase in percentage
sample_sd3 <- 4.31   # Standard deviation in percentage
sample_size3 <- 36   # Sample size

confidence_level3 <- 0.90   # Confidence level

margin_of_error3 <- qt((1 - confidence_level3) / 2, df = sample_size3 - 1) * 
  (sample_sd3 / sqrt(sample_size3))

lower_bound3 <- sample_mean3 - margin_of_error3
upper_bound3 <- sample_mean3 + margin_of_error3

confidence_interval3 <- c(lower_bound3, upper_bound3)

print(confidence_interval3)

#High level Exercise
library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

data(ames)
#Lets create a first visualization of dataset
ggplot(data = ames, aes(x = area)) +
  geom_histogram(binwidth = 250)
#Sumarize Statitics
ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75))  # third quartile, 75th percentile
#Sampling
samp1 <- ames %>%
  sample_n(size = 50)
ggplot(data = samp1, aes(x = area)) +
  geom_histogram(binwidth = 250)

samp2 <- ames %>%
  sample_n(size = 100)
ggplot(data = samp2, aes(x = area)) +
  geom_histogram(binwidth = 250)

samp3 <- ames %>%
  sample_n(size = 1000)
ggplot(data = samp3, aes(x = area)) +
  geom_histogram(binwidth = 250)

#Sampling ramdom
ames %>%
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))
sample_means50 <- ames %>%
  rep_sample_n(size = 50, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)


ames %>%
  sample_n(size = 100) %>%
  summarise(x_bar = mean(area))
sample_means100 <- ames %>%
  rep_sample_n(size = 100, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means100, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)

ames %>%
  sample_n(size = 1000) %>%
  summarise(x_bar = mean(area))
sample_means1000 <- ames %>%
  rep_sample_n(size = 1000, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means1000, aes(x = x_bar)) +
  geom_histogram(binwidth = 20)


# Create an empty data frame to store the sample means
sample_means_small <- data.frame()

# Generate 25 sample means
for (i in 1:25) {
  # Take a random sample of size 10 from the ames dataset
  sample_data <- sample_n(ames, size = 10)
  
  # Calculate the sample mean and add it to the data frame
  sample_mean <- mean(sample_data$area)
  sample_means_small[i, "SampleMean"] <- sample_mean
}

# Print the output
print(sample_means_small)


# Use the app below to create sampling distributions of means of `area`s 
#from samples of size 10, 50, and 100. Use 5,000 simulations.
#What does each observation in the sampling distribution represent?
#How does the mean, standard error, and shape of the sampling distribution
#change as the sample size increases? How (if at all) do these values change
#if you increase the number of simulations?

shinyApp(
  ui <- fluidPage(
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        selectInput("selected_var",
                    "Variable:",
                    choices = list("area", "price"),
                    selected = "area"),         
        
        numericInput("n_samp",
                     "Sample size:",
                     min = 1,
                     max = nrow(ames),
                     value = 30),
        
        numericInput("n_sim",
                     "Number of samples:",
                     min = 1,
                     max = 30000,
                     value = 15000) 
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("sampling_plot"),
        verbatimTextOutput("sampling_mean"),
        verbatimTextOutput("sampling_se")
      )
    )
  ),
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    # create sampling distribution
    sampling_dist <- reactive({
      ames[[input$selected_var]] %>%
        sample(size = input$n_samp * input$n_sim, replace = TRUE) %>%
        matrix(ncol = input$n_samp) %>%
        rowMeans() %>%
        data.frame(x_bar = .)
      #ames %>%
      #  rep_sample_n(size = input$n_samp, reps = input$n_sim, replace = TRUE) %>%
      #  summarise_(x_bar = mean(input$selected_var))
    })
    
    # plot sampling distribution
    output$sampling_plot <- renderPlot({
      x_min <- quantile(ames[[input$selected_var]], 0.1)
      x_max <- quantile(ames[[input$selected_var]], 0.9)
      
      ggplot(sampling_dist(), aes(x = x_bar)) +
        geom_histogram() +
        xlim(x_min, x_max) +
        ylim(0, input$n_sim * 0.35) +
        ggtitle(paste0("Sampling distribution of mean ", 
                       input$selected_var, " (n = ", input$n_samp, ")")) +
        xlab(paste("mean", input$selected_var)) +
        theme(plot.title = element_text(face = "bold", size = 16))
    })
    
    # mean of sampling distribution
    output$sampling_mean <- renderText({
      paste0("mean of sampling distribution = ", round(mean(sampling_dist()$x_bar), 2))
    })
    
    # mean of sampling distribution
    output$sampling_se <- renderText({
      paste0("SE of sampling distribution = ", round(sd(sampling_dist()$x_bar), 2))
    })
  },
  
  options = list(height = 500) 
)


#Typing a new quiz, calculating a sample size
Z <- qnorm(0.975)  # Z-score for 95% confidence level
sigma <- 300  # standard deviation
ME <- 25  # margin of error

n <- ceiling((Z * sigma / ME)^2)
n


#Calculate a new sample size
Z2 <- qnorm(0.99)  # Z-score for 98% confidence level
sigma2 <- 300  # standard deviation
ME2 <- 40  # margin of error

n2 <- ceiling((Z2 * sigma2 / ME2)^2)
n2
#Calculating a p-value
# Sample statistics
n <- 36
mean <- 30.69
sd <- 4.31

# Hypothesized population mean
hypothesized_mean <- 32

# Calculate the test statistic (t-value)
t_value <- (mean - hypothesized_mean) / (sd / sqrt(n))

# Calculate the degrees of freedom
df <- n - 1

# Calculate the p-value
p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)

# Print the p-value
print(p_value)

#Take the previous Dataset again
data(ames)
n <- 60
samp_ames <- sample_n(ames,n)
#View plat and distribution
ggplot(data = samp_ames, aes(x = area)) +
  geom_histogram(binwidth = 250)

#We can find the critical value for a 95% confidence interal using
z_star_95 <- qnorm(0.975)
z_star_95
#Let's finally calculate the confidence interval:
samp_ames %>%
  summarise(lower = mean(area) - z_star_95 * (sd(area) / sqrt(n)),
            upper = mean(area) + z_star_95 * (sd(area) / sqrt(n)))

#Sumarize Parameter of all data
params <- ames %>%
  summarise(mu = mean(area))


true_mu <- params$mu
# Calculate lower and upper bounds of the confidence interval
lower_bound <- mean(samp_ames$area) - z_star_95 * (sd(samp_ames$area) / sqrt(n))
upper_bound <- mean(samp_ames$area) + z_star_95 * (sd(samp_ames$area) / sqrt(n))

# Compare with the confidence interval
if (true_mu >= lower_bound && true_mu <= upper_bound) {
  print("The confidence interval captures the true average size of houses in Ames.")
} else {
  print("The confidence interval does not capture the true average size of houses in Ames.")
}


Z_star_99 <- qnorm(1-(1-0.99)/2)

