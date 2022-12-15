# Load the Shiny library
library(shiny)
library(scales)
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
    column(2, 
        # Create pop-up that cites appropriate references and sources
        actionButton("proj_desc", "Project Description", 
                     style = "margin-bottom:10px"),
      
        # Create section for user input parameters
       wellPanel(
        
          h4("Input Parameters"),    
  
          # Slider
          sliderInput("population", "Population:", min = 25000, max = 1000000, value = 25000, step = 25000), 
          
          # Females to Males Ratio
          sliderInput("f_m", "Female : Male Ratio", 0.5, min = 0, max = 1),
          
          # Available Beds
          sliderInput("n_beds", "Number of Beds", 500, min = 100, max = 2000, step = 25),
          
          # Income Level
          selectInput("inc", "Select Community-Level Income", inc_level, "Quartile 1 (lowest income)"),
          
          # Button
          actionButton("button", "Run simulation"),
          
          ),
  
       # Create section for displaying calculated parameters
        wellPanel( 
          h4("Calculated Averages"),
          p(h5("Beds In Use:"), verbatimTextOutput("avg_beds",placeholder=T)),
          p(h5("Length of Stay (days):"), verbatimTextOutput("avg_LoS",placeholder=T)),
          p(h5("Wait Time (days):"), verbatimTextOutput("avg_wait_time",placeholder=T)),
          p(h5("Hospital Daily Loss ($ in Millions): "), helpText("Due to Being Under/Over Capacity"), verbatimTextOutput("avg_loss",placeholder=T))
        ),
    ),
    
    # Main panel showing plots
    column(5,
      plotOutput("plot1"),
      plotOutput("plot2"),
    ),
    column(5,
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
    
    # Average Calculated Values for Display
    output$avg_beds <- renderText(as.character({round(mean(data$daily_total),2)}))
    output$avg_LoS <- renderText({round(mean(LoS_data), 2)})
    output$avg_loss <- renderText({round(mean(data$daily_loss),-1)/1000})
    output$avg_wait_time <- renderText({round(mean(wait_data),2)})
    
    beds <- input$n_beds
    
    output$plot1 <- renderPlot({
      
      # Plot Simulation
      ggplot(data = data, aes(x = days)) + 
        geom_line(aes(y = daily_total, color='line3'), size=1) +
        geom_line(aes(y = q_len, color='line1'), size=1) +
        geom_line(aes(y = beds, color="line2"), size=1) +
        scale_color_manual(values = c("line3" = "#00AFBB", "line1" = "red", 'line2' = 'black'),
                           labels = c('line3'="Patients Occupying Beds", 'line1'="Patients Waiting", 'line2'='Number of Available Beds')) +
        labs(title = "Hospital Daily Occupancy", x = "Day", y = "Number of Patients", color = '')  + 
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
      ggplot(data = data) + geom_line(aes(x = days, y = daily_loss,color="line1"), size=1) +
        labs(title = "Hospital Daily Financial Losses", x= "Day", y = "Loss ($ in Millions)", color='') +
        scale_color_manual(values = c("line1" = "red"),
                           labels = c('line1'="Daily Loss from Being Under/Over Capacity")) +
        scale_y_continuous(labels = label_number(scale = 1e-3, prefix = "$", suffix = "M", accuracy = 1)) +
        theme_minimal() +
        theme(plot.title = element_text(size=16, face="bold", vjust=4, hjust=.5, lineheight=0.6), 
              axis.title.x = element_text(size=12,face="bold", vjust=-3),
              axis.title.y = element_text(size=12,face="bold", vjust=7), plot.margin = margin(20, 15, 20, 25), 
              legend.title = element_text(face='bold'),
              legend.position = "bottom")
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
  
  # pop-up text 
  plotPopup <- observeEvent(input$proj_desc, {
    showModal(modalDialog(title = "Project Description", 
                          HTML("<p> The goal of this app is to simulate the hospital bed usage over the course of 100 days. Some of the relevant
                               factors include population size, sex ratio, bed availability, and income (1). Numbers were pulled from poisson distributions that 
                               were supported by current literature in Health Care Utilization Project data reports (2). Inspiration for this project
                               comes from 'discrete simulation' models that are common in Health Care resource allocation (3)(4). This project simplifies the complexity
                               of the simulation by only accounting for beds as the resource.</p>
                                 <ol>
                                  <li>https://www.cdc.gov/nchs/data/hus/2020-2021/BedComSt.pdf</li>
                                  <li>https://www.hcup-us.ahrq.gov/reports/statbriefs/sb246-Geographic-Variation-Hospital-Stays.pdf</li>
                                  <li>https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8625660/</li>
                                  <li>https://www.codeproject.com/Articles/1111093/Discrete-Event-Simulation-using-R-Hospital-Capacity</li>
                                </ol>
                               <p>Note: Please make sure to view this app on a full-size window for the best results.</p>"))) 
  })
}

# Run the app
shinyApp(ui = ui, server = server)