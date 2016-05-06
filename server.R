library(shiny)
library(ggplot2)
library(data.table)
library(xtable)
library(arules)
library(arulesViz)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

shinyServer(function(input,output, session){
  observe({
  
    file1 <- input$file
    if(is.null(file1)){return()} 
    data=fread(input=file1$datapath, sep="auto", header = input$header, stringsAsFactors = input$stringAsFactors, data.table=FALSE)
    
  output$table <- renderTable(data)
  
  nums <- sapply(data, is.numeric)
  fact <- !sapply(data, is.numeric)
  data_numeric=data[ , nums]
  data_fact <- data[,fact]
  col_names <- names(data_fact)
  data_fact[,col_names] <- lapply(data_fact[,col_names] , factor)
  
  updateSelectInput(session, "y", "first", colnames(data_numeric) [1:ncol(data_numeric)])
  updateSelectInput(session, "x", "second", colnames(data) [1:ncol(data)])
  updateSelectInput(session, "a", "first", colnames(data) [1:ncol(data)])
  updateSelectInput(session, "b", "second", colnames(data) [1:ncol(data)])
  updateSelectInput(session, "target", "Choose the target", colnames(data_fact) [1:ncol(data_fact)])
  updateSelectInput(session, "decision_target", "Choose the target", colnames(data_fact) [1:ncol(data_fact)])
  updateTextInput(session, "target_value", "Choose the target value", "")
  updateSelectInput(session, "log_target", "Choose the target", colnames(data) [1:ncol(data)])
  updateTextInput(session, "log_formula", "select variables", "")
  
  output$table2 <- renderTable(data_fact)
  
  boxplotText <- reactive({
    paste(input$y, " ~", input$x)
  })
  
  output$graf1 <- renderPlot({
    if(input$plotvolba==1){
      validate(need(ncol(data_numeric) != 0, "There are no numeric attributes! To make boxplot you need one! Try it with barplot!"))
      validate(need(input$y!=input$x, "Selected attributes are same! One of chosen need to be different!"))
      boxplot(as.formula(boxplotText()),data=data, main=paste(input$y, " vs ", input$x),horizontal=FALSE, xlab=input$x, ylab=input$y)}
    else if(input$plotvolba==2){
      ggplot(data=data, aes_string(x=input$a, fill=input$b)) + geom_bar(position=position_dodge())}
  })
  
  asociacne<-function(support, confidence){
      mojedata <- as(data_fact, "transactions")
      asociacne <- apriori(mojedata, parameter=list(support=input$s, confidence=input$c, maxlen=4))
  }
  
  asociacne_ciel<-function(support, confidence){
    mojedata <- as(data_fact, "transactions")
    asociacne <- apriori(mojedata, parameter=list(support=input$s, confidence=input$c, maxlen=4), appearance = list(rhs = c(paste0(input$target,"=",input$target_value)), default="lhs"))
      }
  
  values1 <- reactiveValues(shouldShow = FALSE)
  observe({
    if (input$Show == 0) return()
    
    values1$shouldShow = TRUE
  })
  observe({
    if (is.null(input$Show2) || input$Show2 == 0)
      return()
    values1$shouldShow = FALSE
  })
  
  output$graf_pravidla<- renderPlot({
    if (values1$shouldShow) {
      plot(asociacne_ciel(support, confidence))
                             }
    else {plot(asociacne(support, confidence))}
  })
  
  #VYPISANIE PRAVIDIEL#
  output$pravidla <- renderPrint({
    if (values1$shouldShow) {
            inspect(head(sort(asociacne_ciel(support, confidence), by ="lift"),input$z))
                            }
      else {inspect(head(sort(asociacne(support, confidence), by ="lift"),input$z))}
  })
  
  output$cor <- renderTable({
    nums <- sapply(data, is.numeric)
    data_numeric=data[ , nums]
    validate(need(ncol(data_numeric) != 0, "There are no numeric attributes!"))
    cor_matrix=cor(data_numeric, use="complete.obs", method="pearson")
    k=0
   df=data.frame(1,1,1)
   colnames(df)<-c("correlation","V1","V2")
  
   for (i in 1:ncol(data_numeric))
    for (j in 1:ncol(data_numeric))
      
    {if (abs(cor_matrix[i,j])>=input$strenght & abs(cor_matrix[i,j])< 1.0 ) {k=k+1;
    df[k,1]=cor_matrix[i,j];
    df[k,2]=colnames(data_numeric[j]);
    df[k,3]=colnames(data_numeric[i])
    }}
   print(df)})
  
  decision_values <- reactiveValues(shouldShow = FALSE)
  observe({
    if (input$decision_submit == 0) return()
    
    decision_values$shouldShow = TRUE
  })
  observe({
    if (is.null(input$decision_reset) || input$decision_reset == 0)
      return()
    decision_values$shouldShow = FALSE
  })
  
  decisionText <- reactive({
    paste(input$decision_target, " ~ .")
  })
  
  strom_ciel<-function(){
    tree1 <- rpart(decisionText(), method="class",data=data)
    }
  
  output$strom<- renderPlot({
    if (decision_values$shouldShow) {
      plot(strom_ciel())
      text(strom_ciel())
      fancyRpartPlot(strom_ciel())      
    }
  })
  
  log_values <- reactiveValues(shouldShow = FALSE)
  observe({
    if (input$log_submit == 0) return()
    
    log_values$shouldShow = TRUE
  })
  observe({
    if (is.null(input$log_reset) || input$log_reset == 0)
      return()
    log_values$shouldShow = FALSE
  })
  
  output$regresia <- renderPrint({
    if (log_values$shouldShow) {fit=glm(paste0(input$log_target,"~",input$log_formula),family=binomial, data=data)
    summary(fit)} 
  })
  
  output$interval <- renderPrint({
    if (log_values$shouldShow) {fit=glm(paste0(input$log_target,"~",input$log_formula),family=binomial, data=data)
    confint(fit,level=0.9)
    exp(coef(fit))} 
  })
  
    })
})
