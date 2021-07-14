library(shiny)
library(DT)

image.directory <- c(list.files(path = "C:/Users/wmichael/Google Drive/Projects/ShinyViewer/www",
                                full.names = TRUE))

#----------------------------#
#          
#           UI 
#
#----------------------------#
ui <- fluidPage(
  titlePanel("Imagery QAQC Tool"),
  
  #----------------------------#
  # Side bar contents
  #----------------------------#
  
  sidebarLayout(
    sidebarPanel(
      
      
      tags$style(
        HTML(
          ".error {
                    background-color: red;
                    color: white;
                    }
                    .success {
                    background-color: green;
                    color: white;
                    }"
        )),
      
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
    
    #----------------------------#
    # Main panel contents
    #----------------------------#
    mainPanel(
      imageOutput(outputId = "myImage",
                          width = "100%",
                          height = "400px")
      ) 
  )
)




#----------------------------#
#          
#           SERVER 
#
#----------------------------#
server <- function(input, output, session) {
  
  # create vector of possible answers
  answer_options <- c("Keep",
                      "Check",
                      "Discard")
  
  output$myImage <- renderImage(
    list(src = input$Choice,
         alt = "Something went wrong, check the image/filepath"),
    deleteFile = FALSE)
  
  
  ### 1. create a datatable with checkboxes ###
  # taken from https://github.com/rstudio/DT/issues/93/#issuecomment-111001538
  
  
  # a) function to create inputs
  shinyInput <- function(FUN, ids, ...) {
    inputs <- NULL
    inputs <- sapply(ids, function(x) {
      inputs[x] <- as.character(FUN(inputId = x, label = NULL, ...))
    })
    inputs
  }
  
  # b) create dataframe with the checkboxes
  df <- data.frame(
    Choice = answer_options,
    Selection = shinyInput(checkboxInput, answer_options),
    stringsAsFactors = FALSE
  )
  
  # c) create the datatable
  output$checkbox_table <- DT::renderDataTable(
    df,
    server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE,
    options = list(
      dom = 't', paging = FALSE, ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
  )
  
  
  ### 2. save rows when user hits submit -- either to new or existing csv ###
  observeEvent(input$submit, {
    responses <- data.frame(ImageID = input$Choice,
                            Choice = answer_options,
                            Selection = sapply(answer_options, function(i) input[[i]], USE.NAMES = FALSE))
    
    # if file doesn't exist in current wd, col.names = TRUE + append = FALSE
    # if file does exist in current wd, col.names = FALSE + append = TRUE
    if(!file.exists("SealViewerResponses.csv")) {
      write.table(responses, "SealViewerResponses.csv", 
                  col.names = TRUE, 
                  row.names = FALSE,
                  append = FALSE,
                  sep = ",")
    } else {
      write.table(responses, "SealViewerResponses.csv", 
                  col.names = FALSE, 
                  row.names = FALSE,
                  append = TRUE, 
                  sep = ",")
    }
    # tell user form was successfully submitted
    showModal(modalDialog("Successfully submitted",
                          easyClose = TRUE,
                          footer = NULL,
                          class = "success")) 
    
    # # reset all checkboxes and username
    # updateCheckboxInput(session, answer_options, value = FALSE)
    
    sapply(answer_options, function(x) updateCheckboxInput(session, x, value = FALSE))
    # updateTextInput(session, "Choice", value = "")
    
  })
}




#----------------------------#
#          
#           APP COMPILER 
#
#----------------------------#

shinyApp(ui = ui, server = server)



