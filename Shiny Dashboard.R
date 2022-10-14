library(shiny)
library(ggplot2)
library(bslib)
library(DT)
mobility <- read.csv("movement_data.csv", sep = ";" )
mobility$Date <- as.Date(mobility$Date)
mobility$Province <- as.factor(mobility$Province)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
   #Applying theme
   theme = bs_theme(
   bootswatch = "superhero"),

   # Application title
    titlePanel("Covid 19 Movement Data in Utopia"),
   
   #Layout design
    sidebarLayout(
    sidebarPanel(
    selectInput(inputId = "category",
                label = "Category",
                choices =  c("Retail_Recreation","Grocery_Pharmarcy","Parks","Transit_Stations","Workplaces","Residential"),
                selected = "Grocery_Pharmarcy"),
    
    selectInput(inputId = "Province",
                label = "Province(s)",
                choices = levels(mobility$Province),
                multiple = TRUE,
                selected = "Freetown"
              ),
    dateRangeInput(inputId = "Date",
                   label = "Date Range",
                   start = min(mobility$Date),
                   end = max(mobility$Date)),
    downloadButton("download_data", "Download"),
    ),
   
     mainPanel(
       plotOutput(outputId = "plot"),
       em("Positive and negative percentages indicate an increase and decrease baseline period(median value between January 3 Aand February 6,2020) respectively"),
       DT::dataTableOutput("table")
     )
    )
    )   
# Defining server logic to show histogram
 server= function(input,output){
   filtered_data <- reactive({
     subset(mobility, 
            Province %in% input$Province &
              Date >= input$Date[1] & Date<= input$Date[2])
   })
   
   output$plot <- renderPlot({
     ggplot(filtered_data(),
            aes_string(x="Date", y= input$category, color="Province")) + geom_point(alpha=0.5) +
              ylab("%change from baseline")
   })
   
   output$table <- renderDT(Movement_data, options = list(lenghtChange= TRUE))
   
   output$download_data <- downloadHandler(
     filename = "download_data.csv",
     content = function(file){
       data <- filtered_data()
       write.csv(data, file ,row.names = FALSE)
     }
   )
   
 }
 
 


# Run the application 
shinyApp(ui = ui, server = server)