library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(shinythemes)
library(patchwork)

# no warning messages
options(warn = -1)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  titlePanel("Law of Large Numbers - Simulation"),
  
  # Row for plots
  fluidRow(
    column(width = 12, 
           plotOutput("plot"),
           tableOutput("table"))
  ),
  
  # Row for controls
  fluidRow(
    column(width = 6, 
           sliderInput("n",
                       "Sample Size Maximum:",
                       min = 100,
                       max = 10000,
                       value = 1000)
    ),
    column(width = 6,
           selectInput("view",
                       "View:",
                       choices = c("Graphical Simulation", "Numerical Simulation"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression for the simulation data
  simData <- reactive({
    req(input$n)
    n <- as.numeric(input$n)
    # a large population
    population <- rgamma(n = 10000, shape = 5, rate = 2)
    # define sample sizes
    sample_sizes <- seq(10, n, by = 10)
    # a fixed number of simulations for each sample size
    m <- 100
    # Initialize
    variance_sample_mean <- numeric(length(sample_sizes))
    sample_mean <- numeric(length(sample_sizes))
    for (i in 1:length(sample_sizes)) {
      n <- sample_sizes[i]
      sample <- sample(population, size = n)
      sample_means <- numeric(m)
      sample_mean[i] <- mean(sample)
      # vairance of sample mean is based on m simulations of same sample size
      for (j in 1:m) {
        sample <- rgamma(n = n, shape = 5, rate = 2)
        sample_means[j] <- mean(sample)
      }
      #  sample_mean[i] <- mean(sample_means)
      variance_sample_mean[i] <- var(sample_means)
    }
    # define expected value
    expected_value <- 2.5
    error_mean <- abs(sample_mean - expected_value)
    error_var <- abs(variance_sample_mean)
    
    list(sample_size = sample_sizes,
         sample_mean = sample_mean,
         variance_sample_mean = variance_sample_mean,
         error_mean = error_mean,
         error_var = error_var)
  })
  
  # Plot outputs
  output$plot <- renderPlot({
    df <- simData()
    if(input$view == "Graphical Simulation") {
      df <- data.frame(sample_size = df$sample_size,
                       sample_mean = df$sample_mean,
                       variance_sample_mean = df$variance_sample_mean)
      meanplot <- ggplot(df, aes(x = sample_size, y = sample_mean)) +
        geom_line() +
        geom_hline(yintercept = 2.5, col = "red") +
        labs(title = "Convergence of Sample Mean to Expected Value", x = "", y = "Sample Mean") +
        theme_bw()
      
      varplot <- ggplot(df, aes(x = sample_size, y = variance_sample_mean)) +
        geom_line() +
        labs(title = "Convergence of Variance of Sample Mean to 0", x = "sample size", y = "Variance of Sample Mean") +
        theme_bw()
      
      return(meanplot / varplot)
    } else {
      df <- data.frame(sample_size = df$sample_size,
                       error_mean = df$error_mean,
                       error_var = df$error_var)
      
      errorplot <- ggplot(df, aes(x = sample_size)) +
        geom_line(aes(y = error_mean), col = "blue") +
        geom_line(aes(y = error_var), col = "red") +
        labs(title = "Convergence of Sample Mean and Variance of Sample Mean", x = "sample size", y = "Error") +
        theme_bw()
      n <- nrow(df)
      percentile_indices <- round(c(0.01, 0.25, 0.50, 0.75, 0.99) * n)
      ordered_df <- df %>%
        arrange(sample_size)
      calc_stats <- function(data, index) {
        subset_data <- data[index, ]
        return(data.frame(
          error_mean = subset_data$error_mean,
          error_var = subset_data$error_var
        ))
      }
      qs <- lapply(percentile_indices, function(index) calc_stats(ordered_df, index))
      qs <- do.call(rbind, qs)
      rownames(qs) <- c("1%", "25%", "50%", "75%", "99%")
      qs <- qs %>%
        mutate(error_mean = round(error_mean, 4),
               error_var = round(error_var, 6))
      table <- tableGrob(as.data.frame(qs))
      return(grid.arrange(errorplot, table, ncol = 1))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
