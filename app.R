#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# ui <- fluidPage(
#     titlePanel("Cricket Explorer"),
#     sidebarLayout(
#         sidebarPanel(
#             
#             p("This is an application exploring the performance of T20 cricket players."),
#             
#  
#             
#             selectInput(inputId = "var1", label = "Variable 1", choices = ""),
#             selectInput(inputId = "var2", label = "Variable 2", choices = ""),
#             
#             p(strong("Plot")),
#             checkboxInput(inputId = "regressionLine", label = "Show regression line", value = TRUE),
#             checkboxInput(inputId = "residuals", label = "Show residuals", 
#                           value = FALSE),
#             
#             p(strong("Table")),
#             checkboxInput(inputId = "conf.int", label = "Display confidence interval", value = FALSE),
#             
#             actionButton(inputId = "update", label = "Update")
#         ),
#         mainPanel(
#             h3("Plot"),
#             plotOutput(outputId = "plot"),
#             tableOutput(outputId = "table")
#         )
#     )
# )
# 
# 
# 
# 
# 
# library(plotly)
# 
# ui <- fluidPage(
#     headerPanel('Cricket Data Explorer'),
#     sidebarPanel(
#         # selectInput('xcol','X Variable', names(shiny_periods_table)),
#         # selectInput('ycol','Y Variable', names(shiny_periods_table)),
#         selected = names(shiny_periods_table)[[2]],
#         selectInput("innings", "Innings", shiny_periods_table$innings),
#         selectInput("period", "Period of game", shiny_periods_table$period),
#         selectInput("year", "Year", shiny_periods_table$year)),
#     mainPanel(
#         plotlyOutput('plot')
#     )
# )


#### OLD #####


# ui <- dashboardPage(
#     dashboardHeader(title = "Cricket Data Explorer"),
#     dashboardSidebar(
#         selected = names(shiny_periods_table)[[2]],
#         selectInput("innings", "Innings", shiny_periods_table$innings),
#         selectInput("period", "Period of game", shiny_periods_table$period)
#         
#         
#         
#         
#     ),
#     dashboardBody(
#         # Boxes need to be put in a row (or column)
#         fluidRow(
#             box(plotlyOutput('plot')),
#             box(selectInput("year", "Year", shiny_periods_table$year))
#             
#         )
#     )
# )

# test <- shiny_periods_table %>% filter(innings %in% c("1st"))
# test <- test %>% filter(period %in% c("Death"))
# test <- test %>% filter(year >= min(c(2020)) & year <= max(c(2020))) %>%
#     ungroup() %>% 
#     summarise(runs = sum(runs),
#               balls = sum(balls),
#               wickets = sum(out)) %>%
#     mutate(strike_rate = round((runs/balls)*100, 2),
#            average = round(runs/wickets, 2))




##########################
#### ShinyDashboard ######
##########################

library(shinydashboard)
library(plotly)
library(shiny)
library(DT)
library(crosstalk)

shiny_periods_table <- shiny_periods_table %>% 
    mutate( year = year(as.Date(year, "%Y")))









############################
############ UI ############
############################

ui <- dashboardPage(
    dashboardHeader(title = "IPL Data Explorer"),
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Player Comparison", tabName = "dashboard", icon = icon("dashboard")),
            
            menuItem(text = "Filters",
            menuSubItem(icon = NULL, sliderInput("year", "Year", min = 2008, max = 2020, value = c(2020, 2020), sep = "")),
            menuSubItem(icon = NULL, selectInput("innings", "Innings", shiny_periods_table$innings, multiple = TRUE, selected = c("1st", "2nd"))),
            menuSubItem(icon = NULL, selectInput("period", "Period of game", shiny_periods_table$period, multiple = TRUE, selected = c("Powerplay", "Middle", "Death")))),
            
            
            
            menuItem("Batting Performance", tabName = "widgets", icon = icon("dashboard")),
            
            menuItem(text = "Filters",
            
            menuSubItem(icon = NULL, selectInput("player", "Player", shiny_periods_table$player)),
            
            
            menuItem("Bowling Performance", tabName = "bowling", icon = icon("dashboard")),
            
            menuItem(text = "Filters",
                     
                     menuSubItem(icon = NULL, selectInput("player", "Player", shiny_periods_table$player)))
            
            
            
        
            )
        )
    ),
    
    
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(DT::dataTableOutput("table")),
            box(plotlyOutput('plot')),
            box(plotlyOutput('plot4'))

            )
        ),
        
      
            # Second tab content
            tabItem(tabName = "widgets",
                     fluidRow(
                       
                       infoBox(title = "Name",
                         textOutput("Name"),
                         icon = icon("caret-square-right", lib = "font-awesome"), color = "aqua"),
                         
                       
                       infoBox(title = "Date of Birth",
                         textOutput("DOB"),
                       icon = icon("caret-square-right", lib = "font-awesome"), color = "orange"),
                    
                    infoBox(title = "Bat",
                         textOutput("Bat"),
                         icon = icon("caret-square-right", lib = "font-awesome"), color = "blue"),
                         
                     box(plotlyOutput('plot2')),

                     box(plotlyOutput('plot3')
  
                     )
            #          
                     )
                     
                     
                     
                     
            )
        )
    )
)








######################################

# filterData <- SharedData$new(shiny_periods_table)


# Define server logic to plot various variables against mpg ----
##############################
######### Server ###########
#############################


server <- function(input, output) {
    

    ####################
    ####### Plot 1 #####
    ###################
    
    
        filterData <- reactiveVal(shiny_periods_table)

        
     filtered_data <- reactive({       
        
         res <- filterData() %>% filter(innings %in% c(input$innings))
         res <- res %>% filter(period %in% c(input$period))
         res <- res %>% filter(year >= min(c(input$year)) & year <= max(c(input$year))) %>% 
             group_by(player) %>% 
             summarise(runs = sum(runs),
                       balls = sum(balls),
                       wickets = sum(out)) %>% 
             mutate(strike_rate = round((runs/balls)*100, 2),
                    average = round(runs/wickets, 2))
         
         
         res
        
    })
     
     table <- reactive({
         
         res5 <- filterData() %>% filter(innings %in% c(input$innings))
         res5 <- res5 %>% filter(period %in% c(input$period))
         res5 <- res5 %>% filter(year >= min(c(input$year)) & year <= max(c(input$year))) %>% 
             group_by(player) %>% 
             summarise(runs = sum(runs),
                       balls = sum(balls),
                       wickets = sum(out)) %>% 
             mutate(strike_rate = round((runs/balls)*100, 2),
                    average = round(runs/wickets, 2),
                    runs = round(runs, 0)) %>% 
             select(player, runs, average, strike_rate) %>% 
             arrange(desc(runs))
             
         
         
         
         # res5 <- res5[1:10,]
         res5
         
         
     })
     
     
     
     overall <- reactive({
         
         overall <- filterData() %>% filter(innings %in% c(input$innings))
         overall <- overall %>% filter(period %in% c(input$period))
         overall <- overall %>% filter(year >= min(c(input$year)) & year <= max(c(input$year))) %>%
             ungroup() %>% 
             summarise(runs = sum(runs),
                       balls = sum(balls),
                       wickets = sum(out),
                       dot_ball = sum(dot_ball),
                       four = sum(four),
                       six = sum(six)) %>% 
             mutate(strike_rate = round((runs/balls)*100, 2),
                    average = round(runs/wickets, 2),
                    db_percentage = round((dot_ball/balls)*100, 2),
                    boundary_percentage  = round(((four+six)/balls)*100, 2))
             
         
         overall
         
         
     })
     
     
     ########### Table output ########
     
     
     output$table <- DT::renderDataTable({DT::datatable(table(), options = list(pageLength = 10)) })
     
     
     #####################
     ####### Plot ########
     ####################
     
    
    output$plot <- renderPlotly({
        
        s <- input$table_rows_selected
        
        
               plot1 <- plot_ly(
            x = filtered_data()$strike_rate,
            y =filtered_data()$average,
            text = filtered_data()$player,
            type = 'scatter',
            name = "Batsman",
            mode = 'markers'
            )
        
        plot1 <- plot1 %>% layout(title = 'Batsman SR against Average',
                                  xaxis = list(title = "Strike Rate", showgrid = FALSE, zeroline = FALSE),
                                  yaxis = list(side = 'left', title = 'Average',  showgrid = FALSE, zeroline = FALSE),
                                  showlegend = FALSE)
                                      
                                      

                                     plot1 <- plot1 %>%  add_segments(
                                                                     x = overall()$strike_rate[1],
                                                    y = 0,
                                                    xend = overall()$strike_rate[1],
                                                  # yend = (overall()$average[1]  + (overall()$average[1] * .5)),
                                                  yend = 150,
                                            
                                                  color = "red",
                                                  hoverinfo = overall()$strike_rate[1] )
                                     
                     

                                     plot1 <- plot1 %>%  add_segments(
                                         x = 0,
                                         y = overall()$average[1],
                                         xend = max(filtered_data()$strike_rate),
                                         yend = overall()$average[1],
                                         color = "red",
                                         hoverinfo = overall()$average[1] )
                                      
        
        if(!length(s)) {
        

            plot1
       
        }   else if (length(s)) {
            
            ########### Select Rows ###########
            
            filter_dt <- filtered_data()%>% 
                arrange(desc(runs))
            filter_dt <- filter_dt[s,] 

            
            plot1 <- plot1 %>%
                add_trace(data = filter_dt, x = filter_dt$strike_rate,
                          y = filter_dt$average,
                          type = "scatter", mode = "markers",
                          color = I('red'), name = 'Filtered')
            
            ##################################################
            
        }
    }
        )
    
    
    ##############
    #### Plot2 ####
    #############
    
    match_up_filter <- reactiveVal(match_up_table)


    match_up_filter2 <- reactive({


        res2 <- match_up_filter() %>%
            # filter(innings == input$innings)
        # res2 <- res2 %>%  filter(period == input$period)
        
                   # res2 <- res2 %>%  filter(year == input$year)
        filter(player == input$player) %>% 
            group_by(year) %>% 
            summarise(runs = sum(runs),
                      balls = sum(balls),
                      out = sum(out)) %>% 
            mutate(strike_rate2 = round((runs/balls) *100, 2),
                   average = round(runs/out, 2))
                   # res2 <- res2 %>%  filter(bowl_type == input$bowl_type)



        res2

    })
    # 
    output$plot2 <- renderPlotly({
        plot2 <- plot_ly(
            x = match_up_filter2()$year,
            y = match_up_filter2()$average,
            text = match_up_filter2()$average,
            name = "Average",
            type = 'bar')
        
        plot2 <- plot2 %>% add_trace(x = match_up_filter2()$year, 
                                     y = match_up_filter2()$strike_rate2 , 
                                     type = 'scatter',
                                     mode = 'lines',
                                     name = 'SR', 
                                     yaxis = 'y2',
                                     hoverinfo = "text",
                                     text = match_up_filter2()$strike_rate2)
        
        plot2 <- plot2 %>% layout(title = 'Batsman average and SR by Year',
                                  xaxis = list(title = "Year"),
                                  yaxis = list(side = 'left', title = 'Average', showgrid = FALSE, zeroline = FALSE),
                                  yaxis2 = list(side = 'right', overlaying = "y", title = 'Strike Rate', showgrid = FALSE, zeroline = FALSE))
        
      
        
        
        
        }
    )
    
    ####################
    ##### Plot 3 #######
    ####################
    
    match_up_filter3 <- reactive({
        
        
        res3 <- match_up_filter() %>%
            # filter(innings == input$innings)
            # res2 <- res2 %>%  filter(period == input$period)
            
            # res2 <- res2 %>%  filter(year == input$year)
            filter(player == input$player) %>% 
            group_by(bowl_type) %>% 
            summarise(runs = sum(runs),
                      balls = sum(balls),
                      out = sum(out)) %>% 
            mutate(strike_rate2 = round((runs/balls)*100, 2),
                   average = round(runs/out, 2))
        # res2 <- res2 %>%  filter(bowl_type == input$bowl_type)
        
        
        
        res3
        
    })
    # 
    output$plot3 <- renderPlotly({
        plot3 <- plot_ly(
            x = match_up_filter3()$bowl_type,
            y = match_up_filter3()$average, 
            # color = match_up_filter3()$bowl_type,
            # colors = 'Dark2',
            text = match_up_filter3()$average,
            type = 'bar',
            name = "Average",
          
            # marker = list(color = ~match_up_filter3()$bowl_type, colors = 'Dark2')
            # 
            # # color = I("lightred")
            # # ,
            marker = list(color = '#DC143C')
        )
        
        plot3 <- plot3 %>% add_trace(x = match_up_filter3()$bowl_type, 
                                     y = match_up_filter3()$strike_rate2 , 
                                     type = 'scatter',
                                     mode = 'markers',
                                     name = 'SR', 
                                     yaxis = 'y2',
                                     text = match_up_filter3()$bowl_type,
                                     hoverinfo = "text",
                                     marker = list(color = '#0000ff'),
                                  
                                     text = match_up_filter3()$strike_rate2)
        
        plot3 <- plot3 %>% layout(title = 'Batsman match ups - SR and average',
                                  xaxis = list(title = "Bowl Type"),
                                  yaxis = list(side = 'left', title = 'Average', showgrid = FALSE, zeroline = FALSE),
                                  yaxis2 = list(side = 'right', overlaying = "y", title = 'Strike Rate', showgrid = FALSE, zeroline = FALSE))
        
        
        
        
    }
    )
    
    
    ###### Text Output #####
    
    player_details <- reactiveVal(player_table)
    
    output$Name <- renderText(input$player)
    output$DOB <- renderText(player_details()$born[player_details()$player == input$player])
    output$Bat <- renderText(player_details()$bat_type[player_details()$player == input$player])
    
    
    
    ################
    ##### Plot 4 ####
    ################
    
    filtered_data4 <- reactive({       
        
        
        

        
        res3 <- filterData() %>% filter(innings %in% c(input$innings))
        res3 <- res3 %>% filter(period %in% c(input$period))
        res3 <- res3 %>% filter(year >= min(c(input$year)) & year <= max(c(input$year))) %>% 
            group_by(player) %>% 
            summarise(runs = sum(runs),
                      balls = sum(balls),
                      wickets = sum(out),
                      dot_ball = sum(dot_ball),
                      four = sum(four),
                      six = sum(six)) %>% 
            mutate(strike_rate = round((runs/balls)*100, 2),
                   average = round(runs/wickets, 2),
                   db_percentage = round((dot_ball/balls)*100, 2),
                   boundary_percentage  = round(((four+six)/balls)*100, 2))
        
        
        res3
        
    })
    
    
    output$plot4 <- renderPlotly({
        
        s <- input$table_rows_selected
        
        plot4 <- plot_ly(
           
            x = filtered_data4()$boundary_percentage,
            y = filtered_data4()$db_percentage,
            text = c(filtered_data4()$player),
            type = 'scatter',
            mode = "markers",
            marker = list(color = '#EB69B2')
            
        )
     
        plot4 <- plot4 %>% layout(title = 'Dot Ball % against Boundary %',
                                  xaxis = list(title = "Boundary %", showgrid = FALSE, zeroline = FALSE, ticksuffix = "%"),
                                  yaxis = list(side = 'left', title = 'Dot Ball %',  showgrid = FALSE, zeroline = FALSE, ticksuffix = "%"),
                                  showlegend = FALSE)
                          
           
        
        plot4<- plot4 %>%  add_segments(
            x = overall()$boundary_percentage[1],
            y = 0,
            xend = overall()$boundary_percentage[1],
          
            yend = 100,
            
            color = "red",
            hoverinfo = overall()$boundary_percentage[1] )
        
        
        
        plot4 <- plot4 %>%  add_segments(
            x = 0,
            y = overall()$db_percentage[1],
            # xend = max(filtered_data()$boundary_percentage, na.rm = TRUE),
            xend = 50,
            yend = overall()$db_percentage[1],
            color = "red",
            hoverinfo = overall()$db_percentage[1] )
        
        
        if(!length(s)) {
            
            
            plot4
            
        }   else if (length(s)) {
            
            ########### Select Rows ###########
            
            filter_dt2 <- filtered_data4()%>% 
                arrange(desc(runs))
            filter_dt2 <- filter_dt2[s,] 
            
            
            
            plot4 <- plot4 %>%
                add_trace(data = filter_dt2, x = filter_dt2$boundary_percentage,
                          y = filter_dt2$db_percentage,
                          type = "scatter", mode = "markers",
                          color = I('red'), name = 'Filtered')
            
        }
        
        

    }
    
    
    )
    
    
    
    
}
    
    



shinyApp(ui, server)












install.packages('rsconnect')

rsconnect::setAccountInfo(name='fdignan',
                          token='A2161D0EFEABB21DA07582153C85EDFC',
                          secret='iS2C2pudI5T2QAhQg9K1K4sk2QW/SNyYX9tXUiY/')

library(rsconnect)
rsconnect::deployApp('path/to/your/app')



    
    
    
    # output$plot <- renderPlotly({
    #     plot1 <- ggplot(filtered_data(), aes(x = strike_rate, y = average)) +
    #                         geom_point(aes(text = player))
    #     
    # 
    #     
    #     p <- ggplotly(plot1)
    #     p
    # })
    









# plot2 <- plot_ly(
#     x = x(),
#     y = y(), 
#     type = 'scatter',
#     mode = 'markers'
# )
# 
# plot2
# 
# 
# # Run the application 
# # install.packages("shinydashboard")
# 
# library(shiny)
# 
# 
# ui <- dashboardPage(
#     dashboardHeader(title = "T20 Data Explorer"),
#     dashboardSidebar(),
#     dashboardBody()
# )
# 
# server <- function(input, output) { }
# 
# shinyApp(ui, server)
