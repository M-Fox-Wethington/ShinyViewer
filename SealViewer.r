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
      
      actionButton("previous", "Previous"),
      actionButton("next", "Next"),
      
      
      # Choice Selection Section
      p("Please check the appropriate box below"),
      DT::dataTableOutput('checkbox_table'),
      br(),
      actionButton(inputId = "submit", label= "Log Selection")
      ), 
    
    #----------------------------#
    # Main panel contents
    #----------------------------#
    mainPanel(
      imageOutput(outputId = "myImage"),
      # textOutput("selected_var")
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
                      "Multiple_Seals",
                      "Area_Misestimation",
                      "Other_Error")
  
  
  index <- reactiveVal(1)
  
  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  
  observeEvent(input[["next"]], {
    index(min(index()+1, length(image.directory)))
  })
  
  output$myImage <- renderImage({
    x <- image.directory[index()] 
    list(src = x, alt = "alternate text")
  }, deleteFile = FALSE)
  
  output$selected_var <- renderText(paste("Photo ID:", image.directory[index()]))
  
  
  
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
    responses <- data.frame(ImageID = image.directory[index()],
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
    sapply(answer_options, function(x) updateCheckboxInput(session, x, value = FALSE))

  })
}




#----------------------------#
#          
#           APP COMPILER 
#
#----------------------------#

shinyApp(ui = ui, server = server)



