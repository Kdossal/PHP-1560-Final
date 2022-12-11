# Load the Shiny library
library(shiny)
source(file = 'proj_simulation.R', local = TRUE)

# Load the ggplot2 library for plotting
library(ggplot2)

inc_level <- c('Quartile 1 (lowest income)','Quartile 2','Quartile 3','Quartile 4 (highest income)')

# Define the user interface
ui <- fluidPage(
  
  # App title and background color
  titlePanel("Simulation", title = "Hospital Bed Simulation"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Slider
      sliderInput("population", "Population:", min = 25000, max = 1000000, value = 25000, step = 25000), 
      
      # Females to Males Ratio
      sliderInput("f_m", "Female : Male Ratio", 0.5, min = 0, max = 1),
      
      # Beds
      sliderInput("n_beds", "Number of Beds", 500, min = 100, max = 2000, step = 25),
      
      # Season
      selectInput("inc", "Select Community-Level Income", inc_level, "Quartile 1 (lowest income)"),
      
      # Button
      actionButton("button", "Refresh plot"),
      
    ),
    
    # Main panel
    mainPanel(
      
      # Output
      plotOutput("plot1"),
      plotOutput("plot2"), 
      plotOutput('plot3'), 
      plotOutput('plot4')
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # When the button is clicked, generate a plot
  observeEvent(input$button, {
    
    # Generates data for figures displaying daily totals
    data <- simulation(pop = input$population, sex = input$f_m, input$n_beds, input$inc, output = 'Daily Totals')
    
    # Generates Data for Length of Stay and Wait Time Distribution Figures
    LoS_data <- simulation(pop = input$population, sex = input$f_m, input$n_beds, input$inc, output = 'LoS')
    wait_data <- simulation(pop = input$population, sex = input$f_m, input$n_beds, input$inc, output = 'Wait Times')
    
    # Updates Number of Beds
    beds = input$n_beds
    
    output$plot1 <- renderPlot({
      
      # Plot Simulation
      ggplot(data = data) + geom_line(aes(x = days, y = daily_total)) + geom_line(aes(x = days, y = q_len)) +
        geom_line(aes(x = seq(1:100), y = beds)) +
        labs(title = "Simulated Number of Beds Needed Over Time", x= "Days", y = "Number of Beds Needed") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold')) 
    })
    
    output$plot2 <- renderPlot({
      
      # Plots Distribution of Length of Stays
      ggplot() +
        geom_histogram(aes(x = LoS_data), binwidth = 1, color = 'blue', fill = 'lightblue') +
        labs(title = "Patients' Length of Stay", x= "Days", y = "Number of Patients") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold'))
    })
    
    output$plot3 <- renderPlot({
      
      # Plots Distribution of Wait Times
      ggplot(data = data) + geom_line(aes(x = days, y = daily_loss)) +
        labs(title = "Hospital Daily Losses", x= "Days", y = "$ in Thousands") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold'))
    })
    
    output$plot4 <- renderPlot({
      
      # Plots Distribution of Wait Times
      ggplot() +
        geom_histogram(aes(x = wait_data), binwidth = 1, color = 'blue', fill = 'lightblue') +
        labs(title = "Patients' Wait Times", x= "Days", y = "Number of Patients") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold'))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)