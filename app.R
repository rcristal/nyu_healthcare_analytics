# Rexford Cristal
# Healthcare Analytics, Fall 2018
# Final Project
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(data.table)
library(tables)
library(plyr)
library(dplyr)
library(sqldf)
library(ggplot2)
library(plotrix)

healthdata_dx = read.csv("data/healthdata_dx.csv")
labels <- sqldf("select distinct pt_disposition from healthdata_dx")
labels$pt_disposition <- as.character(labels$pt_disposition)
labels2 <- c(head(labels$pt_disposition, 5))
pt_disposition_count <- c(head(healthdata_dx$N, 5))
piepercent <- round(100 * pt_disposition_count / sum(pt_disposition_count), 1)

healthdata2_expired = read.csv("data/healthdata2_expired.csv")
healthdata2_expired$LOS2 <- parse_number(healthdata2_expired$LOS)

ui <- fluidPage(
  titlePanel("NYU Healthcare Analytics Project",
             windowTitle = "New York University Healthcare Analytics Project"),
  
  mainPanel(
    "Rexford Cristal, MD",
    hr(),
    h3("Sudden Cardiac Arrest, Ventricular Fibrillation, and Patient Disposition", align = "center"),
    plotOutput("pie"),
    fluidRow(
      column(4,
             selectInput("pd",
                         "Patient Disposition:",
                         c("All",
                           unique(as.character(healthdata_dx$pt_disposition))))
      )
    ),
    dataTableOutput("table"),
    hr(),
    h3("Length of Stay and Expiration", align = "center"),
    plotOutput("q", hover = "plot_hover"),
    verbatimTextOutput("info"),
    hr(),
    "Source: Hospital Inpatient Discharges (SPARCS De-Identified), 2016",
    width = 12
  )
)

server <- function(input, output) {
  output$pie <- renderPlot({
    pie(pt_disposition_count, labels = piepercent, main = "Percentage of Patients with SCA or VF By Patient Disposition in NY State (SPARCS 2016)", col = rainbow(length(labels2)))
    legend("bottomleft", labels, cex = 1, fill = rainbow(length(pt_disposition_count)), legend = labels2)
  })
  
  output$table <- renderDataTable(data.table({
    data <- healthdata_dx
    if (input$pd != "All") {
      data <- data[data$pt_disposition == input$pd,]
    }
    data
  }))
  
  output$q <- renderPlot({
    qplot(LOS2, N, data = healthdata2_expired, main = "Relationship Between Length of Stay and Number of Expired Patients (N) in NY State (SPARCS 2016)", xlab = "Length of Stay", ylab = "Expired Patients (N)", color = N)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    
    paste0("hover: ", xy_str(input$plot_hover))
  })
}

shinyApp(ui = ui, server = server)
