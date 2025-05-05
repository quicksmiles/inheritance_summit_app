library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)

# Server logic
server <- function(input, output, session) {
  
  # Reactive function to generate data using normal distribution
  genetic_data <- eventReactive(input$run, {
    generations <- input$generations
    sim_n <- 1000
    rel_sd <- 0.10 # 10% relative standard deviation
    
    df <- data.frame(Generation = 1:generations)
    df$Mean <- 1 / (2 ^ df$Generation)
    
    # Simulate proportions and calculate 95% confidence intervals
    sim <- lapply(df$Mean, function(mu) {
      samples <- rnorm(sim_n, mean = mu, sd = mu * rel_sd)
      samples <- pmin(pmax(samples, 0), 1)  # clip to [0, 1]
      quantile(samples, c(0.025, 0.975))
    })
    
    df$CI_Lower <- sapply(sim, `[`, 1)
    df$CI_Upper <- sapply(sim, `[`, 2)
    
    df
  })
  
  # Updated plot with normal-distribution CI and CI labels
  output$genetic_plot <- renderPlot({
    req(genetic_data())
    data <- genetic_data()
    
    ggplot(data, aes(x = Generation, y = Mean)) +
      geom_col(fill = "skyblue", color = "navy", width = 0.6) +
      geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                    width = 0.2, color = "darkred", linewidth = 1.2) +
      geom_text(aes(label = paste0(round(Mean * 100, 2), "%")), 
                nudge_x = 0.0, nudge_y = 0.18, color = "black", size = 6) +  # Move average label inside bar
      geom_text(aes(y = CI_Upper, 
                    label = paste0("↑ ", round(CI_Upper * 100, 2), "%")),
                vjust = -0.2, size = 6, color = "darkred") +
      geom_text(aes(y = CI_Lower, 
                    label = paste0("↓ ", round(CI_Lower * 100, 2), "%")),
                vjust = 1.2, size = 6, color = "darkred") +
      labs(title = "Inherited DNA Over Generations",
           x = "Generations (1 = Parents)", y = "Proportion of shared DNA",
           caption = "Bars: Average DNA inherited\nRed error bars: 95% confidence interval from recombination\nSimulation: n = 1000 per generation, Normal Distribution") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.75)) +
      theme_minimal(base_size = 14)
  })
  
  
  # Human figure-style stacked bar to simulate DNA inheritance
  output$family_tree <- renderPlot({
    req(genetic_data())
    
    data <- genetic_data()
    data$Label <- sapply(data$Generation, function(g) {
      switch(as.character(g),
             "1" = "Parents",
             "2" = "Grandparents",
             "3" = "Great-Grandparents",
             "4" = "2nd Great-Grandparents",
             "5" = "3rd Great-Grandparents",
             "6" = "4th Great-Grandparents",
             "7" = "5th Great-Grandparents",
             "8" = "6th Great-Grandparents",
             "9" = "7th Great-Grandparents",
             "10" = "8th Great-Grandparents")
    })
    
    # Update label with the Mean proportion from genetic_data
    data$Label <- paste0(data$Label, " (", round(data$Mean * 100, 2), "%)")
    
    # Use Mean for the proportions in the stacked bar chart
    data$ymin <- cumsum(c(0, head(data$Mean, -1)))
    data$ymax <- cumsum(data$Mean)
    data$color <- RColorBrewer::brewer.pal(n = 10, name = "Set3")[1:nrow(data)]
    
    # Set factor levels to control legend order (top to bottom matching the plot)
    data$Label <- factor(data$Label, levels = rev(data$Label))
    
    ggplot(data) +
      geom_rect(aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = Label), color = "black") +
      geom_text(data = data %>% filter(Generation <= 4),
                aes(x = 0.5, y = (ymin + ymax) / 2,
                    label = gsub(" \\(.*\\)", "", as.character(Label))),  # Remove proportion from inner label
                size = 4.5, color = "black") +
      scale_fill_manual(values = setNames(data$color, levels(data$Label))) +
      coord_fixed(ratio = 1) +
      theme_void(base_size = 14) +
      labs(title = "Average proportion of shared DNA by Generation",
           fill = "Generation") +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14)
      )
  })
  
  
  # Quiz Logic
  observeEvent(input$submit_quiz, {
    if (input$quiz_q1 == "12.5%") {
      output$quiz_feedback <- renderText("✅ Correct! You share 12.5% of your DNA with your great-grandparents.")
    } else {
      output$quiz_feedback <- renderText("❌ Incorrect. Try again!")
    }
  })
}
