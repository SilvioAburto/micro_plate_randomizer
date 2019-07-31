
#Written by Silvio Ortiz Aburto
#July 2019

library(shiny)
library(shinyjs)
library(tidyverse)

ui <- fluidPage(
    tags$head(includeCSS(file.path("www", "style.css"))),
    useShinyjs(),
    title="Plate Map Randomizer",
    
    div(id="header",
        h2("Plate Map Randomizer"),
        span(
          style = "font-size: 1.2em",
          span("Created by "),
          a("Silvio Ortiz", href = "https://www.linkedin.com/in/silvio-ortiz-aburto/"),
          HTML("&bull;"),
          span("Code"),
          a("on GitHub", href = "https://github.com/SilvioAburto/micro_plate_randomizer"),br(),
          span("July, 2019")
        )
      ),
    fluidRow(
      column(width=2,
       wellPanel(
          textAreaInput('strains_input', 'Strains separated by a comma. (eg.Strain1,Strain2,Strain3)', width = '220px', height = '75px'),
          uiOutput("rep_slider"),
          actionButton("strains_go","Set"),
          radioButtons("order","Randomized?", choices= c("Yes", "No"), selected="Yes"),
          radioButtons("pt", "Plate Type:", choices = c("96-well","48-well", "24-well"), selected = "96-well"),
          span(
            shinyjs::hidden(
              div(id="download_buttons",
                  downloadButton("download_pm_wide","Download Wide Table"),
                  downloadButton("download_pm_long","Download Long Table")
              ))
          ) 
          ) #end of well panel
        ),
      column(width = 10,
       uiOutput("rep_number"),
       div(class="grid_select",
        uiOutput("grid_ui")),br(),
       plotOutput("plate_map")
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #Assign variables
  #fixed
  columns <- list(seq(from=1, to=12),seq(from=1,to=8), seq(from=1,to=6))
  rows <- list(c("A","B","C","D","E","F","G","H"), c("A","B","C","D","E","F"), c("A","B","C","D") )
  wells_n <- c(96,48,24)
  
  
  # for testing
  # rows_sequence <- paste(rep(rows[[2]],length(columns[[2]])))
  # column_sequence <-rep(columns[[2]], each=length(rows[[2]]))
  # wells<- paste(rows_sequence,column_sequence, sep="")
  # 
  # 
  # #dummy data frame
  # df <- data.frame("well"=wells,
  #                            "column"=column_sequence,
  #                            "row"=rows[[2]])
  # df$strain <- "Test Strain"
  # 
  # data <- df %>%
  #   select(-well) %>%
  #   spread(column, strain)
  
  #Reactive
  rows_sequence <- reactive({paste(rep(rows[[plate_format()]],length(columns[[plate_format()]])))})
  column_sequence <- reactive({rep(columns[[plate_format()]], each=length(rows[[plate_format()]]))})
  wells<- reactive({paste(rows_sequence(),column_sequence(), sep="")})

  sequence_type <- reactive({switch(input$order,
                                   "Yes"="strain_random",
                                   "No"="strain"
  )})

  plate_format <- reactive({switch(input$pt,
                                   "96-well"=1,
                                   "48-well"=2,
                                   "24-well"=3)})
    
  
  output$grid_ui <- renderUI({
    
    options <- c("N/A","2 Channel","3 Channel", "4 Channel")
    if(plate_format() ==1){
     options <- options
    }
    if(plate_format() == 2){
      options <- options[-3]
    }
    if(plate_format() == 3){
      options <- options[-4]
    } 
    selectInput("grid", "Grid Type. (Reservoir)", choices = options)
    
  })
  #dummy data frame
  df <- reactive({data.frame("well"=wells(),
                   "column"=column_sequence(),
                   "row"=rows[[plate_format()]])
                  })
  #strain input
  strains <- c("Strain1", "Strain2","Strain 3")
  
  #Strains
  observeEvent(input$strains_go,{
    
  
  strains <- unlist(strsplit(input$strains_input,","))
  #Highest possible rep number
  strain_rep<- reactive({floor(wells_n[plate_format()]/length(strains))})
  
  #randomize sequence
  strain_sequence <- reactive({append(rep(strains, input$rep_var), rep(NA, (wells_n[plate_format()]-length(strains)*input$rep_var)))})
  #random_sequence <- append(rep(strains, strain_rep), rep(NA, (wells_n[1]- length(strains)*strain_rep)))
  
  df1 <- reactive({ df() %>%
                 dplyr::mutate(strain=strain_sequence(),
                               strain_random = sample(strain_sequence()),
                               strain = factor(strain, levels = strains),
                               strain_random = factor(strain_random, levels = strains),
                               column = as.factor(column),
                               row = factor(row, levels=rev(unique(row)))
                               )
      })
  
  #Selected rep number
  output$rep_slider <- renderUI({
    sliderInput("rep_var","Select Replication", min= 1, max=strain_rep(), value=strain_rep(), step=1)
  })
  

  #show selected replicate
  output$rep_number <- renderUI({
    HTML(paste("Replicate number: ", input$rep_var,
               "</br>Empty wells:", (wells_n[plate_format()]-(length(strains)*input$rep_var)))
    )
  })
  
  #randomize sequence
  #random_sequence <- reactive({sample(rep(append(strains, input$rep_var), rep(NA, (wells_n[1]- length(strains)*input$rep_var))))})
  
  #output plate map
  output$plate_map <- renderPlot({
    
    if(is.null(input$rep_var)){
      return(NULL)
    }
    p <- ggplot(df1(),aes_string(x="column", y="row", fill=sequence_type())) +
      geom_tile(colour="black",size=1) +
      # geom_vline(xintercept = 3.5, color="white", size=2, linetype="dotted") +
      # geom_vline(xintercept = 6.5, color="white", size=2,linetype="dashed") +
      # geom_vline(xintercept = 9.5, color="white", size=2,linetype="longdash") +
      geom_text(aes_string(label=sequence_type()), size=6) +
      theme_bw(24) +
      theme(axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())+
      scale_x_discrete(position="top")
    if(input$grid == "N/A"){
      return(p)
    }
    if(input$grid == "2 Channel"){
      if(plate_format()==1){
        x1 = 6.5
      }
      if(plate_format()==2){
        x1 = 4.5
      }
      if(plate_format()==3){
        x1 = 3.5
      }
      return(
        p + 
        geom_vline(xintercept = x1, color="white", size=2,linetype="longdash") 
      )
    }
    if(input$grid == "3 Channel"){
      if(plate_format()==1){
        x1 = 4.5
        x2 = 8.5
      }
      if(plate_format()==2){
        x1 = 4.5
        x2 = 8.5
      }
      if(plate_format()==3){
        x1 = 2.5
        x2 = 4.5
      }
      return(
        p + 
          geom_vline(xintercept = x1, color="white", size=2, linetype="longdash") +
          geom_vline(xintercept = x2, color="white", size=2,linetype="longdash")
      )    
      
    }
    if(input$grid == "4 Channel"){
      if(plate_format()==1){
        x1 = 3.5
        x2 = 6.5
        x3 = 9.5
      }
      if(plate_format()==2){
        x1 = 2.5
        x2 = 4.5
        x3 = 6.5
      }

      return(
        p + 
          geom_vline(xintercept = x1, color="white", size=2, linetype="longdash") +
          geom_vline(xintercept = x2, color="white", size=2,linetype="longdash") +
          geom_vline(xintercept = x3, color="white", size=2,linetype="longdash") 
      )    
      
    }  
  }) # end of render plot
  
  #download wide table
  output$download_pm_wide <- downloadHandler(
    filename = function() {
      paste("wide_map", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df1() %>% select(-strain,-well) %>% spread(column,strain_random) %>% arrange(desc(row)), file,row.names = FALSE)
    }
  )
  #download long table
  output$download_pm_long <- downloadHandler(
    filename = function() {
      paste("long_map_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # data <- df1() %>%
      #         filter(-strain)
      write.csv(df1() %>% select(-strain), file, row.names = FALSE)
    }
  ) 
  
  
  shinyjs::show("download_buttons")
  
  }) #end of observe event
  
}

# Run the application 
shinyApp(ui, server, enableBookmarking = "url")

