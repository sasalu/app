library(shiny)
shinyUI(fluidPage(
  titlePanel("Application"),
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        condition="input.tabs=='Data'",
      fileInput("file","Upload the file"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
      checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
      ),
      
      conditionalPanel(
        condition="input.tabs=='Correlation'",
        textInput("strenght", "Strenght of correlation", "0.9")
      ),
      
      conditionalPanel(
        condition="input.tabs=='Graphs'",
        radioButtons("plotvolba","", list("Boxplot"=1,"Barplot"=2)),
        
        conditionalPanel(
          condition="input.plotvolba=='1'",
          selectInput("y", "first:"," "),
          selectInput("x", "second"," ")),
        
        conditionalPanel(
          condition="input.plotvolba=='2'",
          selectInput("a", "first:"," "),
          selectInput("b", "second"," "))
      ),
      
      conditionalPanel(
        condition="input.tabs=='Rules'",
        h5('Association rules'),
        numericInput("z", "Number of rules to show", 3),
        numericInput("s", "Rule support", 0.2, min=0, max=1, step=0.1),
        numericInput("c", "Confidence", 0.8, min=0, max=1, step=0.1),
        selectInput("target", "Choose the target"," "),
        textInput("target_value", "Choose the target value",""),
        actionButton("Show","show"),
        actionButton("Show2","reset")
        ),
      
      conditionalPanel(
        condition="input.tabs=='Logistic'",
        selectInput("log_target", "Choose the target"," "),
        textInput("log_formula", "select variables",""),
        actionButton("log_submit","submit"),
        actionButton("log_reset","reset")
        ),
      
      conditionalPanel(
        condition="input.tabs=='Decision'",
        selectInput("decision_target", "Choose the target"," "),
        actionButton("decision_submit","submit"),
        actionButton("decision_reset","reset")
      )
   ),
    
    mainPanel(
      tabsetPanel(type="tabs", 
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Correlation", tableOutput("cor")),
                  tabPanel("Graphs", plotOutput("graf1")),
                  tabPanel("Rules", plotOutput("graf_pravidla"), verbatimTextOutput("pravidla")),
                  tabPanel("Logistic", verbatimTextOutput("regresia"),verbatimTextOutput("interval")),
                  tabPanel("Decision", plotOutput("strom")),
                  id="tabs"
      ))
    
  )
))