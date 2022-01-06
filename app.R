## app.R ##
# shiny dashboard version

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(data.table)
#library(modelr)
#library(broom)
library(DT)
library(tidydrc)
library(memoise)

functionsList <- c("L.3", "L.4", "L.5")
#
# ui ******

  header <- dashboardHeader(title = "FitGrowth")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Load data", tabName = "data", icon = icon("database")),
      menuItem("Model", tabName = "model", icon = icon("rocket")),
      
      menuItem("Summary table", tabName = "summaryTable", icon = icon("table")),
      hr(),
      fluidRow(valueBoxOutput("numSamplesBox", width = 12), (valueBoxOutput("successSamplesBox", width = 12))),
      hr(),
      
      menuItem("Help", tabName = "help", icon = icon("user-o"))
        )
      ) #end of sidebar
    
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, 
              "Load the data as a text file (tsv, csv,...) , the first column must be named", tags$b("time"), "and all other columns are treated as samples. Example data can be downloaded", tags$a(href = "https://github.com/angelovangel/fitgrowth/tree/main/testdata", target = "_blank", "here")
              ),
          box(width = 3,
            title = "Read file", status = "primary",
            fileInput('file1', 'Choose file'),
            tags$hr(),
            
            radioButtons("timeUnits", "Select time unit", 
                         c("Hours" = "h", "Minutes" = "min", "Seconds" = "sec"), selected = "h")
            
            ),
          
          
          box(width = 9, status = "primary", DT::dataTableOutput("data"))
          )
        ),
      tabItem(tabName = "model",
              #h2("Model plot"),
              fluidRow(
                box(width = 12, 
                  h5("Plots of original data, the points used in the model are in blue, model fit is a red line. Change time slider to re-calculate."),
                    column(4, selectizeInput("selectedSamples", 
                                             "Select which samples to analyse", choices = c("A"), 
                                             multiple = TRUE)
                           ),
                  column(2, selectizeInput("sampleData", "Sample data", 
                                           choices = list("no sampling" = 0, "1/5" = 4, "1/10" = 9, "1/100" = 99), selected = 0, multiple = FALSE)
                         ),
              
                    column(2, selectizeInput("model", "Select model to use", choices = functionsList, 
                                             selected = "L.4", 
                                             multiple = FALSE) 
                           ),
                    column(2, selectizeInput("theme", "Plot theme", 
                                           choices = c("blackwhite", 
                                                       "minimal",
                                                       "pubclean",
                                                       "pubr"),
                                           selected = "blackwhite",
                                           multiple = F)
                           ),
                    
                    column(2, selectizeInput("facetCols", "Number of plot columns", choices = c(1:12), selected =4)
                           )
                    
                    ),
                
                box(width = 12, title = "Model plot", status = "primary",
                    
                    plotOutput("model", height = "350px")),
                column(4, sliderInput("trim", label = h5("Trim time"), min=0, max=150, value=c(0,40))),
                column(3, sliderInput("pointsalpha", label = h5("Adjust point opacity"), min = 0, max = 1, value = 0.3)),
                column(1, checkboxInput("confidence", label = "Show confidence interval", value = FALSE)),
                column(1, checkboxInput("ED50", label = "Show ED50", value = FALSE)),
                column(1, checkboxInput("doublingtime", label = "Show doubling time", value = FALSE)),
                column(2, downloadButton('downloadModelPlot', 'Download Plot (pdf)'))
                    
              )
            ),
      
      tabItem(tabName = "summaryTable",
              fluidRow(
                
                box(width = 12, title= "Growth rate constant for the selected samples",status = "primary", 
                    DT::dataTableOutput("summaryTable"))
              )),
      tabItem(tabName = "help", 
              box(width = 12, title = "Usage", status = "primary", 
                  htmlOutput("usage")),
              box(width = 12, title = "About", status = "warning", 
                  htmlOutput("about"))
              
          ))
              
        ) #end of dashboard body
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
    
  df <- reactive({
  
    inFile <- input$file1
    # if(is.null(inFile))
    # {return(NULL)}
    
    validate(
      need(expr = !is.null(input$file1), "Please select file first")
    )
    df <- fread(inFile$datapath, header = TRUE)
    return(sample_frac( df, 1/(as.numeric(input$sampleData) + 1)) )
  })

# selectize initialisation and updates
  observe({  
  
    sampleslist <- colnames(df()) 
    updateSelectizeInput(session, "selectedSamples", 
                         choices = sampleslist[2:length(sampleslist)],       #excluding time, which is the first element
                         selected = sampleslist[2],                         
                         server = TRUE) 
    updateSliderInput(session, "trim", max = max(df()$time), value = c(0, max(df()$time))
                      )
    
  })
  
  observe({
  samples_react <- reactiveValues(allsamples = colnames(df())[-1], modelledsamples = input$selectedSamples)
  print(input$model)
  })
  
#******************************************    
## model df, fits model after filtering by time
    
    dflong <- function() {
      validate(need
               (input$selectedSamples, "Select samples first.. ")   
      )
      df() %>% 
        
        dplyr::select(time, all_of(input$selectedSamples)) %>% # here actual filtering on selectedSamples , notice one_of!       
        rename(t = time) %>%
        gather(sample, n, -t)
      
    }
    
    
    # try memoise? to avoid fitting repeatedly
    # does not work
    # then try dflong as argument? works!
    df1 <- memoise(function(x, model = input$model, trim1 = input$trim[1], trim2 = input$trim[2]){
    if(input$model %in% functionsList ) {
    # security feature
      x %>%
        filter(between(t, trim1, trim2)) %>%
        tidydrc::tidydrc_model(dose = t, response = n, model = get(model)(), sample) # crazy stuff going on here
      }
          # get statistics with lapply on the drm output, e.g lapply(drmout$models, summary)
    })

    
    
    #observe({print(df1(dflong()))})
    
    
    modelplot <- function() {
      model <- df1( dflong() )
      predictions <- model %>% unnest(pred)
      data <- model %>% unnest(data)
      
     p <- dflong() %>%
      ggplot() + 
      geom_point(aes(t, n), alpha = input$pointsalpha, size = 2, stroke = 0) +
       # main pred line
      geom_line(aes(dose, pred), color = "red", linetype = 4, 
                data = predictions) +
      
      geom_point(aes(t, n), alpha = input$pointsalpha, color = "steelblue", size = 2, stroke = 0,
                 data = data) +
      # geom_vline(aes(xintercept = input$trim[1]), linetype = 5, size = 0.2) +
      # geom_vline(aes(xintercept = input$trim[2]), linetype = 5, size = 0.2) +
      
       
      facet_wrap(~ sample, ncol = as.integer(input$facetCols)) +
      xlab(paste0("Time [", input$timeUnits, "]")) +
      ylab("OD")
      
     if(input$confidence) { p <-
       p + geom_ribbon(
         aes(dose, ymin = Lower, ymax = Upper),
         alpha = 0.3,
         fill = "#F5B7B1",
         data = predictions
       )
     }
     
     if(input$ED50) { p <-
       p + 
       geom_vline(aes_(xintercept = ~ ED50), linetype = 5, alpha = 0.5, data = dtt() 
                      ) + 
       geom_text(aes(x = ED50, y = -Inf, label = paste0(round(ED50, 2), " ", input$timeUnits)), 
                 hjust = -0.1, 
                 vjust = -0.5,
                 size = 2.5, 
                 color = "steelblue",
                 data = dtt() )
     }
     
     if(input$doublingtime) { p <- 
       p + geom_text(
         aes(x = Inf, y = -Inf, label = paste0(round(dt, 2), " ", input$timeUnits)),
         hjust = 1.1,
         vjust = -0.5,
         size = 2.5,
         color = "steelblue",
         data = dtt()
       )
     }
     
        if(input$theme == "blackwhite") p <- p + theme_bw()
        if(input$theme == "minimal")    p <- p + theme_minimal()
        if(input$theme == "pubclean") p <- p + theme_pubclean()
        if(input$theme == "pubr") p <- p + theme_pubr()
        
      p <- p + theme(aspect.ratio = 1)
     
      print(p)
    }
    
    

    dtt <- reactive({
      df1(dflong()) %>% 
        unnest(coefs) %>% 
        pivot_wider(names_from = parameter, values_from = value) %>% 
        mutate(growth_rate = abs(`b:(Intercept)`), # b is slope
               ED50 = abs(`e:(Intercept)`), # e is ED50
               dt = log(2)/growth_rate, 
               K = abs(`d:(Intercept)`)) # d is carrying capacity
        
      #dtsterr = exp(Estimate + `Std. Error`) - exp(Estimate))
    })
  
      
    sampledf <- reactive({
      df() %>% 
      gather(sample, od, -1) %>% 
      group_by(sample) %>% 
      summarise(NAs = sum(is.na(od)), measurements = n() - NAs)
  })
    
## outputs ##
    output$data <- DT::renderDataTable({
      datatable(sampledf(), 
        rownames = FALSE, 
        options = list(scrollX = TRUE, 
        dom = "rltip", 
        columnDefs = list(list(className = 'dt-left', targets = 0))
                      )
                ) %>%
      formatRound(columns = c(2,3), digits = 0) %>%
      formatStyle(1, fontWeight = "bold") %>%
      formatStyle(1, backgroundColor = "steelblue", color = "white")
                                       })
    
   
     output$model <- renderPlot({
      modelplot()
    }, res = 120)
     
   
    
    #if (ncol(df) <= 4) plotHeight2 = 200 else (plotHeight2 = ncol(df) * 25) #vary plot height according to number of samples
     
    
    output$summaryTable <- DT::renderDataTable({
      dtt() %>%
      dplyr::select(Sample = sample, 
                    "Growth rate constant" = growth_rate, 
                    #"Growth rate std. error" = `Std. Error`,
                    "Doubling time" = dt, 
                    "Carrying capacity" = K, 
                    "ED50" = ED50) %>%
                    
      datatable( 
                caption = paste0(input$model, " parameters, the time range used in the model is between ", input$trim[1], " and ", input$trim[2], " ", input$timeUnits),
                rownames = FALSE, 
                extensions = 'Buttons', 
                options = list(dom = 'Brltip', 
                               buttons = c("copy", "csv", "print"))
                ) %>%
    formatRound(2:5, 3) %>%
    formatStyle(2:5, fontWeight = "bold") %>%
    formatStyle(1, backgroundColor = "steelblue", color = "white")
        })
   
     output$numSamplesBox <- renderValueBox({
      color <- "green"
      if (ncol(df()) < 2) color <- "red"
      valueBox(value = ncol(df())-1, "samples read", color = color)
    })
    
    
     output$successSamplesBox <- renderValueBox({
      
      ifelse (ncol(df()) >= 2, successSample <- length(input$selectedSamples), successSample <- 0)
      ifelse(successSample == 0, color2 <- "red", #yes
                                  ifelse(successSample < ncol(df())-1, color2 <- "yellow", color2 <- "green")
        )
      
      valueBox(value = successSample, "samples fitted", color=color2) 
    })

     #*** plot download handlers
     output$downloadModelPlot <- downloadHandler(
       filename = "modelPlot.pdf",
       content = function(file) {
          ggsave(file, plot = modelplot(), device = "pdf", width = 8, height = 8, units = "in")
             })    
     
     
     
     #***   
     
    
  
  
  
  output$usage <- renderUI({
    HTML(paste("<p>This app fits a logistic model to the growth data. 
    The best parameters are found using the <code>drc</code> library in <code>R</code>. More specifically, 
    the four-parameter logistic function is used (<code>L.4</code> in <code>drc</code>). 
    The app handles one or many samples (tested with 96), as well as
    <code>NA</code> values. You can get an example file <a href= https://github.com/angelovangel/fitgrowth/tree/main/testdata target=_blank >here</a>.</p> Instructions: 
    load the data as a text file, the first column <u>must</u> be named <b>time</b>, all other columns are treated as
    samples. After uploading the file, go to the <b>Model</b> tab to select which samples to analyse.
    Note that the parameters of the model are re-calculated when the time interval is changed with the slider."))
        })
  output$about <- renderUI({
    HTML(paste("Angel Angelov <p>aangeloo@gmail.com</p>
               
               <p> Built in <code>R</code> using the libraries <code>shiny</code>, <code>drc</code> and <code>tidyverse</code>. 
               The source code is available from GitHub <a href = https://github.com/angelovangel/fitgrowth>here</a>.</p>"))
        })
}

shinyApp(ui, server)