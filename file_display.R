library(shiny)
library(DT)

image.directory <- c(list.files(path = "C:/Users/wmichael/Google Drive/Projects/ShinyViewer/www",
                                full.names = TRUE))


ui <- fluidPage(
  titlePanel("Imagery QAQC Tool"),
  sidebarLayout(
    sidebarPanel(
      
      # Image Selection Section
      h3("Seal Image Quality Check"),
      selectInput(
      inputId = "Choice", 
      label = "Choose a file", 
      choices = image.directory),
      
      # Choice Selection Section
      p("Please check the appropriate box below"),
      DT::dataTableOutput('checkbox_table'),
      br(),
      actionButton(inputId = "submit", label= "Submit Form")
      
      
      ), # Input: Select a file ----
    
    mainPanel(imageOutput(outputId = "myImage",
                          width = "100%",
                          height = "400px")) 
  )
)

server <- function(input, output) {
  output$myImage <- renderImage(
    list(src = input$Choice,
         alt = "Something went wrong, check the image/filepath"),
    deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)





# output$myImage <- renderImage({
#   
#   list(src = input$file1, 
#        alt = "This is alternate text")
# }, deleteFile = TRUE)