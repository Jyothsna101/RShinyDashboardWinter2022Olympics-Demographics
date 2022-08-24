library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(scales)
library(sqldf)

a_events <- read.csv("/Users/jyothsnaakula/Documents/6070/Data/athlete_events.csv")
events <- filter(a_events, Season == 'Winter')
str(events)
avg_w = events %>% select(., Year, Weight, Sex, Team) %>% group_by(Year, Sex) %>% mutate(Average_weight = mean(Weight, na.rm = TRUE))
avg_weight = sqldf("SELECT * from avg_w where Year > 1994")
avg_h = events %>% select(., Year, Height, Sex, Team) %>% group_by(Year, Sex) %>% mutate(Average_height = mean(Height, na.rm = TRUE))
avg_height = sqldf("SELECT * from avg_h where Year > 1994")
avg_age = events %>% select(., Year, Age, Sex, Team) %>% group_by(Year, Sex) %>% mutate(Average_age = mean(Age, na.rm = TRUE))


ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = "Number of Athletes", titleWidth = 1200),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        
                        #ValueBoxes
                        valueBoxOutput("height"),
                        valueBoxOutput("youngest"),
                        valueBoxOutput("weight"),
                        box(
                          title = "Height(cm) vs Gender - 1998 to 2014",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotOutput("height_gender",height = "500px")
                        ),
                        box(
                          title = "Weight(kg) vs Gender - 1998 to 2014",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotOutput("weight_gender",height = "500px")
                        ),
                        box(
                          title = "Age vs Gender - 1924 to 2014",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotOutput("age_gender", height = "500px")
                        ),
                        box(
                          title = "Weight(kg) vs Height(cm) - 1924 to 2014",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          plotOutput("weight_height", height = "500px")
                        )
                      )))

server <- function(input, output) {
  
  output$height <- renderValueBox({
    valueBox(formatC(max(events$Height, na.rm = TRUE), big.mark = ","),
             "Tallest Height(cm) of an Athlete from Winter Olympics - 1924 to 2014", 
             icon = icon("users", lib = "glyphicon"),
             color = "blue"
    )
  })
  output$youngest <- renderValueBox({
    valueBox(formatC(min(events$Age, na.rm = TRUE), big.mark=','),
             'Youngest Age of an Athlete in Olympics from 1924 to 2014',
             color = "blue")
  })
  
  output$weight <- renderValueBox({
    valueBox(formatC(max(events$Weight, na.rm = TRUE), big.mark = ","),
             'Heaviest Weight(kg) of an Athlete from Winter Olympics - 1924 to 2014',
             color = "blue")
  })
  
  #Plot for Weight vs. Gender
  output$weight_gender <- renderPlot({
    ggplot(data = avg_weight, aes(x = Year, y = Weight)) +
      geom_point(shape = 1, alpha = .3) +
      geom_line(data = avg_weight %>% filter(Team == "China"),
                aes(color = Sex, size = .1))
  })
  
  #Plot for Height vs. Gender
  output$height_gender <- renderPlot({
    ggplot(data = avg_height, aes(x = Year, y = Height)) +
      geom_point(shape = 1, alpha = .3) +
      geom_line(data = avg_height %>% filter(Team == "Canada"),
                aes(color = Sex, size = .1))
  })
  
  #Plot for Age vs. Gender
    output$age_gender <- renderPlot({
      ggplot(data = avg_age, aes(x = Sex, y = Age, color = Sex)) + 
        geom_boxplot()
        
  })
  
  #Plot for Height(kg) and Weight(cm)
  output$weight_height <- renderPlot({
    ggplot(data = events, aes(x = Height, y = Weight, color = Sex)) +
      geom_point() +
      xlab("Athlete Height(kg)") +
      ylab("Athlete Weight(cm)") +
      theme_classic()
  })
}

#Running the Application
shinyApp(ui = ui, server = server)
