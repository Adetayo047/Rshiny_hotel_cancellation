
### installing the libraries
library(shiny)
library(data.table)
library(randomForest)
library(tidyverse)


### loading the model
t_model <- readRDS("r_model1.rds")



####################################
# User interface                   #
####################################
ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Hotel Cancellation Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("LeadTime", 
                 label = "Lead Time", 
                 value = ""),
    numericInput("ArrivalDateYear", 
                 label = "Arrival Date Year", 
                 value = ""),
    selectInput("ArrivalDateMonth", 
                label = "ArrivalDateMonth", 
                choices=c(January="January",February="February",March="March",April="April",May="May",
                          June="June",July="July",August="August",September="September",
                          October="October",November="November",December="December")),
    
    numericInput("ArrivalDateWeekNumber", 
                 label = "Arrival Date Week Number", 
                 value = ""),
    numericInput("ArrivalDateDayOfMonth", 
                 label = "Arrival Date Day Of Month", 
                 value = ""),
    numericInput("StaysInWeekendNights", 
                 label = "Stay in weekend nights", 
                 value = ""),
    numericInput("StaysInWeekNights", 
                 label = "Stay in week nights", 
                 value = ""),
    
    textInput("Country", 
              label = "Country", 
              value = ""),
    selectInput("MarketSegment", 
                label = "Market Segment", 
                choices=c(Aviation="Aviation",Complementary="Complementary",Corporate="Corporate",
                          Direct="Direct",
                          Groups="Groups",Offline_TA_TO_="Offline TA/TO",
                          Online_TA="Online TA",Undefined="Undefined")),    
    
    numericInput("PreviousCancellations", 
                 label = "Previous Cancellations", 
                 value = ""),
    
    selectInput("AssignedRoomType", 
                label = "Assigned Room Type", 
                choices=c(A="A",B="B",C="C",D="D",E="E",F="F",G="G",H="H",I="I",K="K",L="L",P="P")),
    numericInput("BookingChanges", 
                 label = "Booking Changes", 
                 value = ""),
    selectInput("DepositType", 
                label = "Deposit Type", 
                choices=c(No_deposit="No deposit",Non_refund="Non refund",Refundable="Refundable")),
    
    selectInput("CustomerType", 
                label = "Customer Type", 
                choices=c(contract="Contract",Group="Group",Transient="Transient",
                          Transient_Party="Transient-Party")),
    numericInput("ADR", 
                 label = "ADR", 
                 value = ""),
    numericInput("RequiredCarParkingSpaces", 
                 label = "Required Car Parking Spaces", 
                 value = ""),
    
    numericInput("TotalOfSpecialRequests", 
                 label = "Total Of Special Requests", 
                 value = ""),
    
    
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('IsCanceled')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)



####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    thedf <- data.frame(
      Name = c("LeadTime",
               "ArrivalDateYear",
               "ArrivalDateMonth",
               "ArrivalDateWeekNumber",
               "ArrivalDateDayOfMonth", 
               "StaysInWeekendNights",
               "StaysInWeekNights",
               "Country", 
               "MarketSegment", 
               "PreviousCancellations",
               "AssignedRoomType", 
               "BookingChanges", 
               "DepositType", 
               "CustomerType",
               "ADR", 
               "RequiredCarParkingSpaces",
               "TotalOfSpecialRequests"),
      
      Value = as.character(c(input$LeadTime,
                             input$ArrivalDateYear,
                             input$ArrivalDateMonth,
                             input$ArrivalDateWeekNumber,
                             input$ArrivalDateDayOfMonth, 
                             input$StaysInWeekendNights,
                             input$StaysInWeekNights,
                             input$Country, 
                             input$MarketSegment,
                             input$PreviousCancellations,
                             input$AssignedRoomType, 
                             input$BookingChanges, 
                             input$DepositType, 
                             input$CustomerType,
                             input$ADR, 
                             input$RequiredCarParkingSpaces,
                             input$TotalOfSpecialRequests)),
      stringsAsFactors = FALSE)
    
    IsCanceled <- "IsCanceled"
    thedf <- rbind(IsCanceled,thedf)
    input <- transpose(thedf)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    hot_test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    View(hot_test)
    
    hot_test$CustomerType <-as.factor(hot_test$CustomerType)
    
    hot_test$DepositType  <- as.factor(hot_test$DepositType) 
    
    hot_test$MarketSegment <- as.factor(hot_test$MarketSegment) 
    
    hot_test$AssignedRoomType  <- as.factor(hot_test$AssignedRoomType)
    
    
    
    
    Output <- data.frame(Prediction=predict(t_model,hot_test), round(predict(t_model,hot_test,
                                                                             type="prob"), 3))
    print(Output)
    
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}
####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)