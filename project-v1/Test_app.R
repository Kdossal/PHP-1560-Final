# Load the Shiny library
library(shiny)
source(file = 'proj_simulation.R', local = TRUE)

# Load the ggplot2 library for plotting
library(ggplot2)

seasons <- c('Spring','Summer','Fall','Winter')

# Define the user interface
ui <- fluidPage(
  
  # App title and background color
  titlePanel("Simulation", title = "Hospital Bed Simulation"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Slider
      sliderInput("population", "Population:", min = 25000, max = 5000000, value = 25000, step = 25000), 
      
      # Females to Males Ratio
      sliderInput("f_m_ratio", "Female:Male Ratio", 0.5, min = 0, max = 1),
      
      # Season
      selectInput("season", "Select season", seasons, "Summer"),
      
      # Button
      actionButton("button", "Refresh plot")
    ),
    
    # Main panel
    mainPanel(
      
      # Output
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # When the button is clicked, generate a plot
  observeEvent(input$button, {
    output$plot1 <- renderPlot({
      
      # Run the simulation with the specified population
      result <- data.frame(x = seq(1:100))
      
      # Plot the result using ggplot2
      ggplot(result, aes(x = x)) +
        replicate(10, geom_line(aes(y = simulation(pop = input$population)),
                                color = "lightblue", size = 1)) +
        labs(title = "Simulated Number of Beds Needed Over Time", x= "Days", y = "Number of Beds Needed") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold')) 
    })
    output$plot2 <- renderPlot({
      LoS_dist <- rpois(input$population, lambda = 4.6)
      hist(LoS_dist, 
           main = "Patients' Length of Stay",
           ylab='# Patients',
           xlab ='Days')
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)