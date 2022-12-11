# Load the Shiny library
library(shiny)
source(file = 'proj_simulation.R', local = TRUE)

# Load theme library for aesthetics
library(shinythemes)

# Load the ggplot2 library for plotting
library(ggplot2)

inc_level <- c('Quartile 1 (lowest income)','Quartile 2','Quartile 3','Quartile 4 (highest income)')

# Define the user interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # App title and background color
  titlePanel("Simulation", title = "Hospital Bed Simulation"),
  
  # Styling for placeholder text
  tags$head(
    tags$style(HTML("
      #avg_beds, #avg_LoS, #avg_wait_time, #avg_loss  {
        color: black;
        background: white;
        font-family: 'Helvetica',Arial,sans-serif;
      }
      #plot_borders {
          border-bottom: 1.5px solid gray;
      }
      "))
  ),
  
  # Sidebar
  fluidRow(
    column(4, 
       wellPanel(
        
          h4("Input Parameters"),    
  
          # Slider
          sliderInput("population", "Population:", min = 25000, max = 1000000, value = 25000, step = 25000), 
          
          # Females to Males Ratio
          sliderInput("f_m", "Female : Male Ratio", 0.5, min = 0, max = 1),
          
          # Beds
          sliderInput("n_beds", "Number of Beds", 500, min = 100, max = 2000, step = 25),
          
          # Season
          selectInput("inc", "Select Community-Level Income", inc_level, "Quartile 1 (lowest income)"),
          
          # Button
          actionButton("button", "Run simulation"),
          
          ),
  
        wellPanel( 
          # Display Calculated Values
          h4("Calculated Averages"),
          p(h5("Beds Needed:"), verbatimTextOutput("avg_beds",placeholder=T)),
          p(h5("Length of Stay (days):"), verbatimTextOutput("avg_LoS",placeholder=T)),
          p(h5("Wait Time (days):"), verbatimTextOutput("avg_wait_time",placeholder=T)),
          p(h5("Hospital Losses ($ in Thousands): "), helpText("Due to Being Under/Over Capacity"), verbatimTextOutput("avg_loss",placeholder=T))
        ),
    ),
    
    # Main panel showing plots
    column(8,
           # Use if we want borders below plots
           # fluidRow(id = "plot_borders",
           #          plotOutput("plot1")
           #          ),
           # fluidRow(id = "plot_borders",
           #          plotOutput("plot2")
           # ),
           # fluidRow(id = "plot_borders",
           #          plotOutput("plot4")
           # ),
           # fluidRow(id = "plot_borders",
           #          plotOutput("plot3")
           # ),
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput('plot4'),
      plotOutput('plot3')
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
    
    # Average Calculated Values for Display
    output$avg_beds <- renderText(as.character({mean(data$daily_total)}))
    output$avg_LoS <- renderText({mean(LoS_data)})
    output$avg_loss <- renderText({mean(data$daily_loss)})
    output$avg_wait_time <- renderText({mean(wait_data)})
    
    # Updates Number of Beds
    beds = input$n_beds
    
    output$plot1 <- renderPlot({
      # Plot Simulation
      
      # colors <- c("Beds_Utilized" = "#00AFBB", "Wait_Times" = "#C4961A", "Available_Beds" = "red")
      
      ggplot(data = data, aes(x = days)) + 
        geom_line(aes(y = daily_total),color='#00AFBB') +
        geom_line(aes(y = q_len),color='#C4961A') +
        geom_line(aes(y = beds),color="red") +
        
        # Trying toi add a legend here
        # geom_line(aes(y = daily_total),color='Beds_Utilized') + 
        # geom_line(aes(y = q_len),color='Wait_Times') +
        # geom_line(aes(y = beds),color="Available_Beds") +
        # geom_label(aes(label = list(c("Beds_Utilized","Wait_Times","Available_Beds"))), nudge_x = 0.35, size = 4) +
        # scale_color_manual(name = "Legend", 
        #   breaks=c('Beds_Utilized', 'Wait_Times', 'Available_Beds'),
        #   values = c("Beds_Utilized" = "#00AFBB", "Wait_Times" = "#C4961A", "Available_Beds" = "red")) +
        
        labs(title = "Simulated Number of Beds Used Over Time", x= "Days", y = "Number of Beds Needed")  + 
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7),
              plot.margin = margin(20, 15, 20, 25), 
              legend.position="bottom",
              legend.title = element_text(face='bold'))
      
    })
    
    output$plot2 <- renderPlot({
      
      # Plots Distribution of Length of Stays
      ggplot() +
        geom_histogram(aes(x = LoS_data), binwidth = 1, color = 'black', fill = '#00AFBB') +
        labs(title = "Histogram of Patients' Length of Stay", x= "Days", y = "Number of Patients") +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold'))
    })
    
    output$plot3 <- renderPlot({
      
      # Plots Distribution of Wait Times
      ggplot(data = data) + geom_line(aes(x = days, y = daily_loss),color="#00AFBB") +
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
        geom_histogram(aes(x = wait_data), binwidth = 1, color = 'black', fill = '#00AFBB') +
        labs(title = "Histogram of Patients' Wait Times", x= "Days", y = "Number of Patients") +
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