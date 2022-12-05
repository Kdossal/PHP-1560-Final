library(shiny)
library(glue)
# library(colorpicker)

style_choices <- c("none", "dotted", "dashed", "solid", "double")

ui <- fluidPage(theme = "theme.css",
                titlePanel("Button Styler"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Style your buttons!"),
                    
                    # Label
                    textInput("label", "Select text form button", "Label"),
                    
                    # Text color
                    # colourInput("color", "Select text colour", "black"),
                    
                    # Text size
                    numericInput("font_size", "Select text size", 18, min = 1, max = 50),
                    
                    # Background color
                    # colourInput("background", "Select background colour", "white"),
                    
                    # Border color
                    # colourInput("border_color", "Select border color", "gray"),
                    
                    # Border style
                    selectInput("border_style", "Select border style", style_choices, "solid"),
                    
                    # Border size
                    numericInput("border_width", "Select border width", 1, min = 1, max = 10),
                    
                    # Border radius
                    sliderInput("border_radius", "Select border radius", 5, min = 0, max = 100)
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