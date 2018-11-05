library(tidyverse)
library(readr)
library(shiny)
library(shinythemes)
library(plotly)
library(stringr)
library(readxl)
library(data.table)
library(janitor)

wine_data <- read_xlsx("Summary_of_NZ_Wines.xlsx") 

names(wine_data) <- c("year", paste0("y", 2009:2018))

wine_data <- as.data.frame(t(wine_data))

names(wine_data) <- lapply(wine_data[1, ], as.character)

wine_data <- wine_data[-c(1), ] 
wine_data <- setDT(wine_data, keep.rownames = TRUE)

wine_data$rn <- str_remove(wine_data$rn, pattern = "y")

wine_data <- clean_names(wine_data)



wine_options <- c( "Number of wineries"= "number_of_wineries", "Number of growers"= "number_of_growers", "Producing area" = "producing_area") 


#options <- c("Gender" = "sex", 
             #"Age"  = "age", 
             #"Educational attainment" = "educ2",
             #"Race and ethnicity" = "racethn",
             #"Region" = "cregion",
             #"Phone type" = "hphoneuse")  

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("paper"),
                
                # Application title
                titlePanel("Understanding New Zealand Wine Production"),
                h5("Data source: Miss. Paterson"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "y", #internal label 
                                label = "Select the variable to visualize", #label that user sees
                                choices = c(wine_options), #vector of choices for user to pick from 
                                selected = "Number of wineries")),
                  
                  # Show a plot of the generated distribution
                  mainPanel(plotlyOutput("plot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$plot <- renderPlotly({
    ggplotly(ggplot(data = wine_data, 
                    aes_string(y = input$y, x = "rn"))+ 
               geom_point() +
               labs(x = "Year", y = names(wine_options[which(wine_options == input$y)])))})
}

# Run the application 
shinyApp(ui = ui, server = server)

