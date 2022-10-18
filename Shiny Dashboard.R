
library(shiny)
library(ggplot2)
library(DT)

mobility<-read.csv("movement_data.csv",sep=';')
mobility$Date <-as.Date(mobility$Date) #as.Date() method returns the object of a class "Date"
mobility$Province <- as.factor(mobility$Province)

# Define UI for application that show a simple project
ui <- fluidPage (
  titlePanel("COVID 19 Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dv",label = "Category", 
                  choices = c("Retail_Recreation","Grocery_Pharmacy","Parks","Transit_Station","Workplaces","Residential"),
                  selected = "Grocery_Pharmacy"),
      
      selectInput(inputId = "provinces","Provinces(s)", 
                  choices = levels(mobility$Province),
                  multiple=TRUE,
                  selected = c("Freetown","Palmonova")),
      dateRangeInput(inputId = "date",label="Date Range",
                     start=min(mobility$Date),
                     end= max(mobility$Date)),
      mainPanel(
        plotOutput(outputId ="plot" ),
        em("Positive and negative percenatges indicate an increase and decrease from the baseline period(median value between January 3 and February 6, 2020) respetively"),
        
        DT::dataTableOutput(outputId = "table")
        
      )
    )
),
  
  server= function(input, output) {
    filtered_data <- reactive({
      subset(mobility,
             Province %in% input$provinces &
               Date >= input$date[1] & Date <= input$date[2])
    })
    
    output$plot <- renderPlot({
      ggplot(filtered_data(),
             aes_string(x="Date",y=input$dv, color="Province")) + geom_point(alpha=0.5) + 
        ylab("%change from baseline")
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)