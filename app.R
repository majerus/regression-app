

pkg <- c("shiny", "shinyapps", "googleVis", "knitr", "dplyr", 
         "ggplot2", "shinysky", "magrittr")

new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}


library(shinyapps)
library(googleVis)
library(knitr)
library(shiny)
library(shinysky)


download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("mlb11.RData")

rownames(mlb11) <- mlb11$team
mlb11$team <- NULL




server <- function(input, output) {
  
  # list of data sets
  datasetInput <- reactive({
    switch(input$dataset,
           "cars" = mtcars,
           "longley" = longley,
           "MLB" = mlb11,    
           "rock" = rock,
           "pressure" = pressure, 
           "Your Data" = df())
  })
  
  # dependent variable
  output$dv = renderUI({
    selectInput('dv', h5('Dependent Variable'), choices = names(datasetInput()))
  })
  
  # independent variable
  output$iv = renderUI({
    selectInput('iv', h5('Independent Variable'), choices = names(datasetInput()))
  })
  
  # regression formula
  regFormula <- reactive({
    as.formula(paste(input$dv, '~', input$iv))
  })
  
  # bivariate model
  model <- reactive({
    lm(regFormula(), data = datasetInput())
  })
  
  
  # create graphics 
  
  # data view 
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # summary statistics
  output$summary <- renderPrint({
    summary(cbind(datasetInput()[input$dv], datasetInput()[input$iv]))
  })
  
  # histograms   
  output$distPlot_dv <- renderPlot({
    x    <- datasetInput()[,input$dv]  
    bins <- seq(min(x), max(x), length.out = input$bins_dv + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Dependent Variable', xlab = input$dv)
  })
  
  
  
  output$distPlot_iv <- renderPlot({
    x    <- datasetInput()[,input$iv]  
    bins <- seq(min(x), max(x), length.out = input$bins_iv + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = 'Independent Variable', xlab = input$iv)
  })
  
  # scatter plot 
  output$scatter <- renderPlot({
    plot(datasetInput()[,input$iv], datasetInput()[,input$dv],
         xlab = input$iv, ylab = input$dv,  main = "Scatter Plot of Independent and Dependent Variables", pch = 16, 
         col = "black", cex = 1) 
    
    abline(lm(datasetInput()[,input$dv]~datasetInput()[,input$iv]), col="grey", lwd = 2) 
  })
  
  # correlation matrix
  output$corr <- renderGvis({
    d <- datasetInput()[,sapply(datasetInput(),is.integer)|sapply(datasetInput(),is.numeric)] 
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    gvisTable(cor) 
  })
  
  # bivariate model
  output$model <- renderPrint({
    summary(model())
  })
  
  # residuals
  output$residuals_hist <- renderPlot({
    hist(model()$residuals, main = paste(input$dv, '~', input$iv), xlab = 'Residuals') 
  })
  
  output$residuals_scatter <- renderPlot({
    plot(model()$residuals ~ datasetInput()[,input$iv], xlab = input$iv, ylab = 'Residuals')
    abline(h = 0, lty = 3) 
  })
  
  output$residuals_qqline <- renderPlot({
    qqnorm(model()$residuals)
    qqline(model()$residuals) 
  })
  
  
  # hotable
  output$hotable1 <- renderHotable({
    df <- data.frame(String = c('a', 'b', 'c', 'd', 'e','a', 'b', 'c', 'd', 'e'), Numeric1 = numeric(10), Numeric2 = numeric(10))
    return(df)
  }, readOnly = FALSE)
  
  df <- reactive({
    hot.to.df(input$hotable1) # this will convert your input into a data.frame
  })
  
  
  
  # values <- reactiveValues()
  # 
  # values$df <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
  # newEntry <- observe({
  #   if(input$update > 0) {
  #     newLine <- isolate(c(input$text1, input$text2))
  #     isolate(values$df[nrow(values$df) + 1,] <- c(input$text1, input$text2))
  #   }
  # #   if(input$update > 0) {
  # #   colnames(values$df) <- c(input$label1, input$label2)
  # #   }
  # })
  # 
  # output$table1 <- renderTable({values$df})
  
  
  
  # download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )  
}


ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Regression"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      textInput("name", label = h5("Name"), 
                value = "Name"),
      
      HTML('<br />'),
      
      selectInput("dataset", h5("Choose a dataset:"), 
                  choices = c("cars", "longley", "MLB", 
                              "rock", "pressure", "Your Data")),        
      
      HTML('<br />'),
      
      uiOutput('dv'),    
      
      HTML('<br /> '),
      
      uiOutput('iv'),
      
      HTML('<br /> '),
      
      radioButtons('format', 
                   h5('Document format'),
                   c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      
      downloadButton('downloadReport'),
      
      HTML("<br /> <br />  
           This app was designed for the Introduction to Statistics course at Reed College. 
           All of the text that you enter in the fields will be included in the downloaded report. 
           If you download your report in Word, then you will be able to easily edit your text.
           Contact <a href='mailto:majerus@reed.edu'> majerus@reed.edu </a> with questions, comments, or suggestions.")
      ),
    
    
    
    
    
    # main panel 
    
    mainPanel(
      tabsetPanel(type = "tabs", 
                  
                  tabPanel("Data",  
                           HTML("Select a data set from the 'Choose a dataset menu' or enter your own data below <br /> <br />"),
                           numericInput("obs", 
                                        label = h5("Number of observations to view"), 
                                        10),
                           tableOutput("view"),                           
                           #                           tableOutput("table1"),
                           #                            ,textInput("label1", "Column 1 Name"),
                           #                            textInput("label2", "Column 2 Name"),
                           #                            textInput("text1", "Column 1"),
                           #                            textInput("text2", "Column 2"),
                           #                            actionButton("update", "Update Table")),
                           HTML('<br /> <br />'),
                           h5("Input Your Own Data"), 
                           ("Then select 'Your Data' from the data set menu. This functionality is still in progress and may not work properly.")
                           ,div(class="well container-fluid"
                                ,hotable("hotable1"))),
                  
                  
                  tabPanel("Summary Statistics",
                           verbatimTextOutput("summary"),
                           textInput("text_summary", label = "Interpretation", value = "Enter text...")),
                  
                  tabPanel("Histograms",                   
                           plotOutput("distPlot_dv"),
                           sliderInput("bins_dv",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 7),  
                           textInput("text_hist_dv", label = "Interpretation", value = "Enter text..."),
                           
                           plotOutput("distPlot_iv"),
                           sliderInput("bins_iv",
                                       "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 7),
                           textInput("text_hist_iv", label = "Interpretation", value = "Enter text...")),                       
                  
                  tabPanel("Scatter Plot",                   
                           plotOutput("scatter"),
                           textInput("text_scatter", label = "Interpretation", value = "Enter text...")),  
                  
                  tabPanel("Correlations",                   
                           htmlOutput("corr"),
                           HTML('<br /> <br />'),
                           textInput("text_correlation", label = "Interpretation", value = "Enter text...")),
                  
                  tabPanel("Model",                   
                           verbatimTextOutput("model"),
                           textInput("text_model", label = "Interpretation", value = "Enter text...")),
                  
                  tabPanel("Residuals",                   
                           plotOutput("residuals_hist"),
                           plotOutput("residuals_scatter"),
                           plotOutput("residuals_qqline"),
                           textInput("text_residuals", label = "Interpretation", value = "Enter text..."))
                  
                  
                  
                  
                  
      )                         
    )
      )
    ))

shinyApp(ui = ui, server = server)

