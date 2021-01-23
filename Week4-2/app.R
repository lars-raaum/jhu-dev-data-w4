#
# This application renders Norwegian employment data based on the Labour Force Survey (AKU)
# Data documentation: https://www.ssb.no/en/aku/ 

# Load required packages

library(shiny)
library(dplyr)
library(ggplot2)
library(httr)
library(rjstat)

# Get the base data from Statistics Norway (SSB) API

options(encoding="UTF-8")
url <- "https://data.ssb.no/api/v0/no/table/05110/"

data <- '
{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74",
          "15-24",
          "25-54",
          "55-74"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "1988K3",
          "1989K3",
          "1990K3",
          "1991K3",
          "1992K3",
          "1993K3",
          "1994K3",
          "1995K3",
          "1996K3",
          "1997K3",
          "1998K3",
          "1999K3",
          "2000K3",
          "2001K3",
          "2002K3",
          "2003K3",
          "2004K3",
          "2005K3",
          "2006K3",
          "2006K3 Gml",
          "2007K3",
          "2008K3",
          "2009K3",
          "2010K3",
          "2011K3",
          "2012K3",
          "2013K3",
          "2014K3",
          "2015K3",
          "2016K3",
          "2017K3",
          "2018K3",
          "2019K3",
          "2020K3"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'
    d.tmp <- POST(url , body = data, encode = "json", verbose())
    # Extract contents from d.tmp as text, then fromJSONstat
    sbtabell <- fromJSONstat(content(d.tmp, "text"))
    
    sbtabell$alder <- as.factor(sbtabell$alder)
    sbtabell$value <- as.numeric(sbtabell$value)
    sbtabell$kvartal <- substr(sbtabell$kvartal, 0, 4)
    sbtabell$kvartal <- as.numeric(sbtabell$kvartal)
    

# Define UI for application that draws a line plot
ui <- fluidPage(

    # Application title
    titlePanel("Norwegian unemployment rate for Q3 each year, by age"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("years",
                        "Years:",
                        min = min(sbtabell$kvartal),
                        max = max(sbtabell$kvartal),
                        value = c(min(sbtabell$kvartal), max(sbtabell$kvartal)),
                        sep = "")
                        
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", plotOutput("distPlot")),
                        tabPanel("Summary", verbatimTextOutput("summary")),
                        tabPanel("Table", tableOutput("table")),
                        tabPanel("Documentation", htmlOutput("documentation"))
           
        )
    )
)
)

# Define server logic required to draw a line diagram
server <- function(input, output) {
    # Filter the dataset based on the year slider
    df <- reactive({
        filter(sbtabell, kvartal >= input$years[1] & kvartal <= input$years[2])
    })   
    
    output$distPlot <- renderPlot({
        # Draw the diagram with ggplot
        ggplot(df(), aes(x=kvartal, y=value)) + geom_line(aes(colour=alder, group = alder))
        
    })
    
    output$summary <- renderPrint({
        summary(df())
    })
    
    output$table <- renderTable({
        df()
    })
    
    output$documentation <- renderUI({
        HTML("- This is a simple app visualizing Norwegian unemployment data from the Norwegian Labour Force Survey <br />
- The Survey is done quarterly among a large sample of the Norwegian population<br />
- Documentation of the data is available at https://www.ssb.no/en/aku/ <br />
- The most recent data is from Q3 2020, thus i compare Q3 data for the entire time series<br />
- The data is collected from the Statistics Norway (SSB) API and some minor cleaning operations are performed <br />
- Use the slider for the range of years you want to show in the graph<br />
- The graph shows the unemployment rate for Q3 in each year, by age group.<br /> 
- GitHub repo: https://github.com/lars-raaum/jhu-dev-data-w4")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
