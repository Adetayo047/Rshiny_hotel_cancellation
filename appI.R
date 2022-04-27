# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("r_model1.rds")
#trainbinded <- read.csv(paste("trainbinded", ".csv", sep=""), header = TRUE)
trainbinded<- read_csv("trainbinded1.csv")
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
    sliderInput("LeadTime", "LeadTime:",
                min = 0, max = 737,
                value = 104),
    selectInput("ArrivalDateYear", label = "ArrivalDateYear:", 
                choices = list("2015" = "2015", "2016" = "2016", "2017" = "2017"), 
                selected = "2016"),
    selectInput("ArrivalDateMonth", label = "ArrivalDateMonth:", 
                choices = list("January"="January","February"="February","March"="March","April"="April","May"="May",
                               "June"="June","July"="July","August"="August","September"="September",
                               "October"="October","November"="November","December"="December"), 
                selected = "August"),
    sliderInput("ArrivalDateWeekNumber", "ArrivalDateWeekNumber:",
                min = 1, max = 53,
                value = 27),
    sliderInput("ArrivalDateDayOfMonth", "ArrivalDateDayOfMonth:",
                min = 1, max = 37,
                value = 16),
    sliderInput("StaysInWeekendNights", "StaysInWeekendNights:",
                min = 0, max = 19,
                value = 12),
    sliderInput("StaysInWeekNights", "StaysInWeekNights:",
                min = 0, max = 50,
                value = 20),
    selectInput("Country", label = "Country:", 
                choices = list("ITA" ="ITA",  "GBR"="GBR",  "DEU"="DEU",  "PRT"="PRT",  "FIN"="FIN",
                               "FRA"="FRA",  "AUT"="AUT", "TUR"="TUR",  "BEL"="BEL",  "NLD"="NLD", 
                               "USA"="USA",  "ESP"="ESP",  "BGR"="BGR",  "NOR"="NOR",  "CN"="CN",  
                               "MAR"="MAR",  "IRL"="IRL",  "BOL"="BOL",  "ROU" ="ROU", "CHE"="CHE", 
                               "BRA"="BRA",  "EGY"="EGY",  "POL"="POL",  "IND" ="IND", "SVN" ="SVN", 
                               "CHN" ="CHN", "SWE" ="SWE", "NULL" ="NULL","HUN" ="HUN", "DNK"="DNK", 
                               "ISR" ="ISR", "SRB"="SRB",  "THA"="THA",  "CHL"="CHL",  "RUS"="RUS",  
                               "KEN"="KEN",  "AUS"="AUS",  "CZE"="CZE",  "ARG"="ARG",  "JPN" ="JPN", 
                               "GUY"="GUY",  "ATF" ="ATF", "ZAF"="ZAF",  "MUS"="MUS",  "GRC" ="GRC"), 
                selected = "USA"),
    selectInput("MarketSegment", label = "MarketSegment:", 
                choices = list('Aviation'="Aviation","Complementary"="Complementary","Corporate"="Corporate",
                               "Direct"="Direct",
                               'Groups'="Groups","Offline TA/TO"="Offline TA/TO",
                               "Online TA"="Online TA","Undefined"="Undefined"), 
                selected = "Aviation"),
    sliderInput("PreviousCancellations", "PreviousCancellations:",
                min = 0, max = 26,
                value = 2),
    selectInput("AssignedRoomType", label = "AssignedRoomType:", 
                choices = list('A'="A","B"="B","C"="C",
                               "D"="D",'E'="E","F"="F",
                               "G"="G","H"="H","I"="I",
                               "K"="K","L"="L","P"="P"), 
                selected = "G"),
    sliderInput("BookingChanges", "BookingChanges:",
                min = 0, max = 21,
                value = 0),
    selectInput("DepositType", label = "DepositType:", 
                choices = list("No Deposit"= "No Deposit", 
                               "Non Refund"="Non Refund", 
                               "Refundable"="Refundable"),
                selected = "Refundable"),
    selectInput("CustomerType", label = "CustomerType:",
                choices = list("Transient-Party"= "Transient-Party",
                               "Transient"="Transient",
                               "Group"="Group",
                               "Contract"="Contract"),
                selected = "Contract"),
    sliderInput("ADR", "ADR:",
                min = 0, max = 5400,
                value = 101),
    sliderInput("RequiredCarParkingSpaces", "RequiredCarParkingSpaces:",
                min = 1, max = 8,
                value = 3),
    sliderInput("TotalOfSpecialRequests", "TotalOfSpecialRequests:",
                min = 0, max = 5,
                value = 2),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
    
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
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
    
    df <- data.frame(
      Name = c( "LeadTime", "ArrivalDateYear","ArrivalDateMonth" ,
                "ArrivalDateWeekNumber" , "ArrivalDateDayOfMonth","StaysInWeekendNights",
                "StaysInWeekNights" ,"Country" , "MarketSegment" ,"PreviousCancellations",
                "AssignedRoomType" ,"BookingChanges", "DepositType" ,"CustomerType", "ADR",                     
                "RequiredCarParkingSpaces", "TotalOfSpecialRequests"),
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
    df <- rbind(IsCanceled, df)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    
    test$LeadTime <- as.numeric(test$LeadTime)
    test$ArrivalDateYear <- as.numeric(test$ArrivalDateYear)
    test$ArrivalDateMonth <- as.factor(test$ArrivalDateMonth)
    test$ArrivalDateWeekNumber <- as.numeric(test$ArrivalDateWeekNumber)
    test$ArrivalDateDayOfMonth <- as.numeric(test$ArrivalDateDayOfMonth)
    test$StaysInWeekendNights <- as.numeric(test$StaysInWeekendNights)
    test$StaysInWeekNights <- as.numeric(test$StaysInWeekNights)
    test$Country <- as.character(test$Country)
    test$MarketSegment <- as.factor(test$MarketSegment)
    test$PreviousCancellations <- as.numeric(test$PreviousCancellations)
    test$AssignedRoomType <- as.factor(test$AssignedRoomType)
    test$BookingChanges <- as.numeric(test$BookingChanges)
    test$DepositType <- as.factor(test$DepositType)
    test$CustomerType <- as.factor(test$CustomerType)
    test$ADR <- as.numeric(test$ADR)
    test$RequiredCarParkingSpaces <- as.numeric(test$RequiredCarParkingSpaces)
    test$TotalOfSpecialRequests <- as.numeric(test$TotalOfSpecialRequests)
    View(test)
    
    levels(test$ArrivalDateMonth) <- levels(trainbinded$ArrivalDateMonth)
    levels(test$MarketSegment) <- levels(trainbinded$MarketSegment)
    levels(test$AssignedRoomType) <- levels(trainbinded$AssignedRoomType)
    levels(test$DepositType) <- levels(trainbinded$DepositType)
    levels(test$CustomerType) <- levels(trainbinded$CustomerType)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
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



