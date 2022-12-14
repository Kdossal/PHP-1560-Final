library(shiny)
library(glue)

style_choices <- c("none", "dotted", "dashed", "solid", "double")

ui <- fluidPage(theme = "theme.css",
                titlePanel("Hospital Admission Simulation"),
                
                sidebarLayout(
                  sidebarPanel(
                    # helpText("Style your buttons!"),
                    # 
                    # Label
                    textInput("label", "Select text form button", "Label"),
                    # 
                    # # Text size
                    # numericInput("font_size", "Select text size", 18, min = 1, max = 50, step = 1),
                    # 
                    # Border style
                    selectInput("border_style", "Select border style", style_choices, "solid"),
                   
                    # Population size
                    numericInput("population_size", "Input population size", 1000000, min = 20000, max = 5000000),
                    
                    # Females to Males Ratio
                    sliderInput("f_m_ratio", "Females to Males Ratio", 0.5, min = 0, max = 1),
                    
                    # Input: actionButton() to defer the rendering of output ----
                    # until the user explicitly clicks the button (rather than
                    # doing it immediately when inputs change). This is useful if
                    # the computations required to render output are inordinately
                    # time-consuming.
                    actionButton("run", "Run Simulation")
                  ),
                  
                  mainPanel(
                    h2("Styled Button"),
                    htmlOutput("button", align = "center"),
                    h2("Code"),
                    verbatimTextOutput("text")
                  )
                  
                )
)

server <- function(input, output) {
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$run, {
    switch(input$dataset,
           "popluation_size" = popluation_size,
           "f_m_ratio" = f_m_ratio,
           )
  }, ignoreNULL = FALSE)
  
  output$button <- renderUI({
    style <- paste0(collapse = " ",
                    glue("background-color:{input$background};
                  color:{input$color};
                  border-color:{input$border_color};
                  border-style:{input$border_style};
                  border-width:{input$border_width}px;
                  border-radius:{input$border_radius}%;
                  font-size:{input$font_size}px;")
    )
    actionButton("button", input$label, style = style)
  })
  output$tooltip <- renderUI({
    shiny::tooltipText("Simulated data drew from a poisson distribution based off HCUP statistics" )
  })
  
  output$text <- renderText({
    start <- glue('actionButton("button", "{input$label}",')
    
    s22 <- "                      "
    style <- glue('background-color:{input$background};
                            {s22}color:{input$color};
                            {s22}border-color:{input$border_color};
                            {s22}border-style:{input$border_style};
                            {s22}border-width:{input$border_width}px;
                            {s22}border-radius:{input$border_radius}%;
                            {s22}font-size:{input$font_size}px;"')
    
    glue('{start}\n {"             "}style = "{style})')
  })
  
}

shinyApp(ui, server)