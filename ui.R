library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)

# UI
ui <- fluidPage(
  useShinyjs(), # For animations and interactions
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  
  # Title
  titlePanel("🧬 How Much DNA Do You Inherit Across Generations?"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("generations", 
                  "Select Number of Generations:", 
                  min = 1, max = 10, value = 3, step = 1),
      
      actionButton("run", "Run Simulation", icon = icon("play"), 
                   class = "btn-success"),
      
      hr(),
      h4("🎯 Quiz: Test Your Knowledge"),
      selectInput("quiz_q1", "How much DNA, on average, do you share with your great-grandparents?",
                  choices = c("25%", "12.5%", "6.25%", "3.125%")),
      
      actionButton("submit_quiz", "Submit Quiz", icon = icon("check")),
      
      textOutput("quiz_feedback")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Visualization",
                 plotOutput("genetic_plot", height = "500px"),
                 plotOutput("family_tree", height = "600px"),
        ),
        
        tabPanel("📚 Explanation",
                 p("Each generation, you inherit ~50% of your DNA from each parent."),
                 p("As you move back generations, the proportion of shared DNA with ancestors decreases geometrically."),
                 p("Imagine each generation you add a flip of a coin"),
                 p("Use the slider to see how quickly the DNA contribution from your ancestors becomes negligible!"))
      )
    )
  )
  # change to average proportion of genetics shared
  # also change colors for visuals
  # normal distribution with 1000 ppl for confidence intervals
)
