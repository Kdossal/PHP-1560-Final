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
      sliderInput("population", "Population:", min = 25000, max = 1000000, value = 25000, step = 25000), 
      
      # Females to Males Ratio
      sliderInput("f_m", "Female : Male Ratio", 0.5, min = 0, max = 1),
      
      # Beds
      sliderInput("n_beds", "Number of Beds", 500, min = 100, max = 2000),
      
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
      
      data <- simulation(pop = input$population, sex = input$f_m, input$n_beds, output = 'Daily Totals')
      
      # Plot Simulation
      ggplot(data = data) + geom_line(aes(x = days, y = daily_total)) + geom_line(aes(x = days, y = queue)) +
        geom_line(aes(x = seq(1:100), y = input$n_beds)) +
        #replicate(5, 
        #          geom_line(aes(x = seq(1:100), y = simulation(pop = input$population, sex = input$f_m, output = 'Daily Total')),
        #                        color = sample(c("lightblue", 'grey', 'orchid'), 1), size = 1)) +
        labs(title = "Simulated Number of Beds Needed Over Time", x= "Days", y = "Number of Beds Needed") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold')) 
    })
    
    LoS_data <- simulation(pop = input$population, sex = input$f_m, input$n_beds, output = 'LoS')
    
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
  })
}

# Run the app
shinyApp(ui = ui, server = server)