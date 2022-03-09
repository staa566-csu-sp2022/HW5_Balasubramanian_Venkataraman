## Load Packages and data   

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(DT)
library(kableExtra)

#Load IPL cricket data
data = read.csv("IPL_Game_Metadata.txt", sep = "|")


## Shiny Dashboard Code
Data$GameYear = format(as.Date(data$GameDate, format = "%Y-%m-%d"), "%Y%")

server = function(input, output){
  
  ## Bar Chart for Winners
  output$bar_chart_winners = renderPlotly({
    
    in_year = input$in_year
    #Filter out rows with blank winners
    data_winners = data %>%
      filter(Winner != "")
    
    #Filter out the temporary teams
    data_winners = data_winners %>%
      filter(Winner %in% c("Sunrisers Hyderabad", "Royal Challengers Bangalore", "Chennai Super Kings", "Rajasthan Royals", "Mumbai Indians", "Kolkata Knight Riders", "Kings XI Punjab", "Delhi Daredevils", "Delhi Capitals", "Deccan Chargers")) %>% filter(GameYear == in_year)
    
    bar_chart_winners = ggplot(data = data_winners, mapping = aes(x = Winner)) +
      geom_bar(mapping = aes(fill = Winner)) +
      coord_flip() +
      theme_classic()+
      scale_fill_brewer(palette = "BrBG") +
      xlab("Number of Wins") +
      ylab("Team Name") 
    
    ggplotly(bar_chart_winners)
  })
  
  ## Bar chart for games played every year
  output$bar_chart_game_count = renderPlotly({
    
    in_name = input$in_name
    
    data_game_count = data %>% filter(GameYear <= 2018 & Winner == in_name)
    
    bar_chart_game_count = ggplot(data = data_game_count, mapping = aes(GameYear))+
      geom_bar(mapping = aes(fill = GameYear)) +
      theme_classic()+
      scale_fill_brewer(palette = "Spectral") +
      xlab("Year") +
      ylab("Number of Games")+
      labs(fill = "Year")
    
    ggplotly(bar_chart_game_count)
  })
  
  ## Data Table
  output$tbl_data = renderDataTable({
    
    in_name = input$in_name
    data_winners %>% select(GameYear,HomeTeam,AwayTeam,Winner) %>% filter(Winner==in_name)
  })
  
  output$in_name1 = renderText({
    input$in_name
  })
  
  output$in_name2 = renderText({
    input$in_year
  })
}

ui = dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "IPL"
  ),
  dashboardSidebar(
    sidebarMenu(
      textInput("in_name", "Team Name", value = "Chennai Super Kings"),
      textInput("in_year","Year", value = "2008"),
      menuItem("Analysis By Teams", tabName = "by_teams"),
      menuItem("Analysis By Year", tabName = "by_year")
      
    )
  ),
  dashboardBody(
    #First Page
    tabItems(
      tabItem("by_teams",
              h2("Games Won by ", textOutput("in_name1", inline = TRUE), "in IPL"),
              h3("Number of Games Won"),
              box(plotlyOutput("bar_chart_game_count"), width = 500),
              h3("Details of Games Won"),
              box(dataTableOutput("tbl_data"), width = 500)
      ),
      
      #Second Page
      tabItem("by_year",
              h2("Games Won by Teams in Year ", textOutput("in_name2", inline = TRUE)),
              box(plotlyOutput("bar_chart_winners"), width = 500)
      )
    )
  )
)

shinyApp(ui,server)
