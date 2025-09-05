library(shiny)
library(dplyr)
library(ggplot2)
library(glmnet)
library(readr)

# Load and clean the dataset
spoty <- read_csv("Spotify-2000.csv")
cleaned_spoty <- spoty %>%
  select(`Top Genre`, Year, `Beats Per Minute (BPM)`, Danceability, Popularity) %>%
  filter(!is.na(`Top Genre`), !is.na(Year), !is.na(Danceability), !is.na(`Beats Per Minute (BPM)`), !is.na(Popularity))

# Example data frame for 'most_popular_genre' and 'average_danceability_per_year'
most_popular_genre <- cleaned_spoty %>%
  group_by(`Top Genre`) %>%
  summarize(Most_Popular_Genre = first(`Top Genre`), 
            Most_Popular_Genre_Avg_Popularity = mean(Popularity), 
            Decade = floor(Year / 10) * 10)

average_danceability_per_year <- cleaned_spoty %>%
  group_by(Year) %>%
  summarize(Average_Danceability = mean(Danceability))

server <- function(input, output, session) {
  
  # Update the choices for checkboxGroupInput based on the available genres
  observe({
    updateCheckboxGroupInput(session, "selected_genres", 
                             choices = unique(most_popular_genre$Most_Popular_Genre), 
                             selected = unique(most_popular_genre$Most_Popular_Genre))
  })
  
  # Reactive expression to filter data based on user input for popularity
  filtered_data <- reactive({
    req(input$selected_genres, input$popularity_range, input$decade_range)
    data <- most_popular_genre %>%
      filter(Most_Popular_Genre %in% input$selected_genres,
             Most_Popular_Genre_Avg_Popularity >= input$popularity_range[1],
             Most_Popular_Genre_Avg_Popularity <= input$popularity_range[2],
             Decade >= input$decade_range[1],
             Decade <= input$decade_range[2])
    print(data) # Add this line
    data
  })
  
  # Render the plot for Most Popular Genre's Average Popularity Over the Past 7 Decades
  output$popularityPlot <- renderPlot({
    data <- filtered_data()
    req(nrow(data) > 0)
    ggplot(data, aes(x = Decade, y = Most_Popular_Genre_Avg_Popularity, color = Most_Popular_Genre)) +
      geom_segment(aes(xend = Decade + 10, yend = Most_Popular_Genre_Avg_Popularity), size = 4) +
      labs(title = "Most Popular Genre's Average Popularity Over the Past 7 Decades",
           x = "Decade",
           y = "Average Popularity",
           color = "Genre") +
      scale_x_continuous(breaks = seq(min(data$Decade), max(data$Decade) + 10, by = 10), 
                         limits = c(min(data$Decade), 2020)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5))
  })
  
  # Reactive expression to filter data based on user input for danceability
  filtered_danceability_data <- reactive({
    req(input$year_range, input$danceability_range)
    average_danceability_per_year %>%
      filter(Year >= input$year_range[1],
             Year <= input$year_range[2],
             Average_Danceability >= input$danceability_range[1],
             Average_Danceability <= input$danceability_range[2])
  })
  
  # Render the plot for Average Danceability Over the Years
  output$danceabilityPlot <- renderPlot({
    data <- filtered_danceability_data()
    req(nrow(data) > 0)
    
    plot <- ggplot(data, aes(x = Year, y = Average_Danceability)) +
      geom_line(color = "#0082C2", size = 0.8) + 
      labs(title = "Average Danceability Over the Years",
           x = "Year",
           y = "Average Danceability") +
      scale_x_continuous(breaks = seq(1950, 2020, by = 10)) + 
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
        axis.text.y = element_text(size = 10),  
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "gray90"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "gray60"),
        panel.border = element_rect(color = "gray60", fill = NA, size = 0.5))
    
    if (input$log_scale) {
      plot <- plot + scale_y_log10()
    }
    
    plot
  })
  
  # Reactive expression to get the dataset for Lasso regression
  dataset <- reactive({
    cleaned_spoty
  })
  
  # Fit the Lasso model
  lasso_fit <- reactive({
    x <- model.matrix(Popularity ~ . - 1, data = dataset())
    y <- dataset()$Popularity
    cv_fit <- cv.glmnet(x, y, alpha = 1)
    return(cv_fit)
  })
  
  # Output the best lambda
  output$best_lambda <- renderText({
    paste("Best lambda:", lasso_fit()$lambda.min)
  })
  
  # Coefficient plot
  output$coeff_plot <- renderPlot({
    coef_plot <- as.data.frame(as.matrix(coef(lasso_fit(), s = "lambda.min")))
    coef_plot$variable <- rownames(coef_plot)
    ggplot(coef_plot, aes(x = variable, y = V1)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = "Lasso Coefficients", y = "Coefficient", x = "Variable")
  })
  
  # Predictions plot
  output$pred_plot <- renderPlot({
    x <- model.matrix(Popularity ~ . - 1, data = dataset())
    y <- dataset()$Popularity
    y_pred <- predict(lasso_fit(), s = "lambda.min", newx = x)
    pred_df <- data.frame(Actual = y, Predicted = y_pred)
    ggplot(pred_df, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, col = "red") +
      labs(title = "Actual vs Predicted Popularity", x = "Actual", y = "Predicted")
  })
  
  # Interactive lambda selection
  output$lambda_plot <- renderPlot({
    plot(lasso_fit())
  })
}
