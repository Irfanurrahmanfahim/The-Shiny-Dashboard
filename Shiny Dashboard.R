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
      
      #Category Drop-down Selection menu  
      selectInput(inputId = "category",
                  label = "Category",
                  choices =  c("Retail_Recreation","Grocery_Pharmarcy","Parks","Transit_Stations","Workplaces","Residential"),
                  selected = "Grocery_Pharmarcy"),
      
      #Multiple Province select
      selectInput(inputId = "Province",
                  label = "Province(s)",
                  choices = levels(mobility$Province),
                  multiple = TRUE,
                  selected = c("Freetown","Auroville","Palmanova","Ctesiphon")),
      
      
      #Date Range Layout
      dateRangeInput(inputId = "Date",
                     label = "Date Range",
                     start = min(mobility$Date),
                     end = max(mobility$Date)),
      
      
      #Download Button
      downloadButton("download_data", "Download"),
      
    ),
    
    #Main-panel Layout
    mainPanel(
      plotOutput(outputId = "plot"),
      em("Positive and negative percentages indicate an increase and decrease baseline period(median value between January 3 And February 6,2020) respectively"),
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
  
  
  #Graph
  output$plot <- renderPlot({
    ggplot(filtered_data(),
           aes_string(x="Date", y= input$category, color="Province")) + geom_point(alpha=0.5) +
      ylab("%change from baseline")
  })
  
  
  #Table
  output$table <- DT::renderDT({
    filtered_data()
  })
  
  
  #Download
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
