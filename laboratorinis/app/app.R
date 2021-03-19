library(shiny)
library(tidyverse)
library(ggplot2)

ui = fluidPage(
    titlePanel("692000 Apskaitos, buhalterijos ir audito veikla"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "imones_pavadinimas",
                           label = "Imones pavadinimas",
                           choices = NULL,
                           selected = NULL)),
        mainPanel(
            tabsetPanel(
                tabPanel("Bendra informacija apie imone", tableOutput("lentele")),
                tabPanel("Imones vidutiniai atlyginimai per menesi", plotOutput("grafikas1")),
                tabPanel("Imones sumoketi mokesciai per menesi", plotOutput("grafikas2")),
                tabPanel("Apdrausti darbuotojai imoneje", plotOutput("grafikas3"))
            )
        )
    )
)
server = function(input, output, session) 
{
    data = read_csv("../data/lab_sodra.csv")
    
    data = data %>%
        mutate(month_value=as.integer(substr(month, 5 ,7)))
    
    FillteredData = data %>% 
        filter(ecoActCode == "692000")
    
    updateSelectizeInput(session, "imones_pavadinimas", choices = FillteredData$name, server = TRUE)
    
    output$lentele = renderTable(
        FillteredData %>%
            filter(name == input$imones_pavadinimas), digits = 0
    )
    
    output$grafikas1 = renderPlot(
        FillteredData %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month_value, y = avgWage)) +
            scale_x_continuous("Month", breaks=1:12, limits=c(1,12)) +
            geom_line(col = "cyan") +
            ylab("Average wage") +
            theme_light() +
            geom_point()
    )
    
    output$grafikas2 = renderPlot(
        FillteredData %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month_value, y = tax)) +
            scale_x_continuous("Month", breaks=1:12, limits=c(1,12)) +
            geom_line(col = "green") +
            ylab("Tax") +
            theme_light() +
            geom_point()      
    )
    
    output$grafikas3 = renderPlot(
        FillteredData %>%
            filter(name == input$imones_pavadinimas) %>%
            ggplot(aes(x = month_value, y = numInsured)) +
            scale_x_continuous("Month", breaks=1:12, limits=c(1,12)) +
            geom_line(col = "red") +
            ylab("Number of insured employees") +
            theme_light() +
            geom_point()
    )
}
shinyApp(ui, server)