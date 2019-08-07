#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    plotOutput("plot")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        withProgress(message = 'Calculation in progress',
                     detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                             incProgress(1/15)
                             Sys.sleep(0.25)
                         }
                     })
        plot(cars)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
