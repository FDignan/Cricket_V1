library(dplyr)
library(tidyr)
library(gdata)
library(readxl)
library(data.table)
library(lubridate)
library(stringr)
library(odbc)
library(formattable)
library(openxlsx)
library(yaml)
library(reshape2)
library(purrr)
library(downloader)
library(RSQLite)
library(shiny)

setwd("C:/Users/Dignanf/Documents/Cricket/")

death_query <- paste0("select player_table.player,
strftime('%Y', date(dates_1)) as year,
 sum(innings_table.runs_batsman) as runs,
 sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as out,
			
			sum(innings_table.runs_batsman)/sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as average,
			
			count(innings_table.runs_batsman) as balls,
			
			round((sum(innings_table.runs_batsman)/count(innings_table.runs_batsman)), 3) as sr
			
			
			
 
 from player_table
 
 JOIN innings_table on innings_table.batsman = player_table.player
 join matches_table on matches_table.match_id = innings_table.match_id

 Where
matches_table.competition = 'IPL'
and cast(innings_table.over as real) > 15

 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1))
 ")

all_query <- paste0("select player_table.player,
innings_table.innings,
count( distinct innings_table.match_id) as number_of_innings,
cast(innings_table.over as real) as over,
strftime('%Y', date(dates_1)) as year,
sum(case when innings_table.runs_batsman = 4 then 1
else 0
end) as four,
sum(case when innings_table.runs_batsman = 6 then 1

else 0

end) as six, 
sum(case when innings_table.runs_batsman = 0 then 1
else 0 
end) as dot_ball,
 sum(innings_table.runs_batsman) as runs,
 sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as out,
			
			sum(innings_table.runs_batsman)/sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as average,
			
			sum(case when cast(innings_table.extras_wides as real) < 1 then 1
			
			else 0
			
			end) as balls
			

			
			
			
 
 from player_table
 
 JOIN innings_table on innings_table.batsman = player_table.player
 join matches_table on matches_table.match_id = innings_table.match_id

 Where
matches_table.competition = 'IPL'
and innings_table.innings in ('1st', '2nd')


 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1)),
 innings_table.innings,
 cast(innings_table.over as real)
 ")

# query_test <- "select * from matches_table"

conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test5.db")

all_table <- dbGetQuery(conn, all_query)

dbDisconnect(conn)


shiny_periods_table <- all_table %>% 
  select(-average) %>% 
  mutate(period = case_when(over < 7 ~ "Powerplay",
                   over > 6 & over < 16 ~ "Middle",
                   over > 15 ~ "Death")) %>% 
  group_by(player, year, innings, period) %>% 
    summarise_all(sum) %>% 
  mutate(dot_ball_percentage = dot_ball/balls,
         boundary_percentage = (four+six)/balls,
         strike_rate = round((runs/balls) * 100, 2),
         average = round(runs/out, 2),
         runs_per_innings = runs/number_of_innings) 
  
  

test_data1 <- all_table %>% 
  filter(player == "KL Rahul",
         year == "2020") %>% 
  summarise(batsman_runs = sum(runs))





test_shiny <- shiny_periods_table %>% 
  filter(year == "2018",
         innings == "1st",
         period == "Middle") %>% 
  distinct(player, .keep_all = TRUE)



########### With bowlers ######


match_up_query <- paste0("select player_table.player,
innings_table.innings,
count( distinct innings_table.match_id) as number_of_innings,
cast(innings_table.over as real) as over,
strftime('%Y', date(dates_1)) as year,
a.bowl_type,
sum(case when innings_table.runs_batsman = 4 then 1
else 0
end) as four,
sum(case when innings_table.runs_batsman = 6 then 1

else 0

end) as six, 
sum(case when innings_table.runs_batsman = 0 then 1
else 0 
end) as dot_ball,
 sum(innings_table.runs_batsman) as runs,
 sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as out,
			
			sum(innings_table.runs_batsman)/sum(case 
			when innings_table.wicket_player_out = innings_table.batsman then 1
			
			else 0
			
			end) as average,
			
			count(innings_table.runs_batsman) as balls
			

			
			
			
 
 from player_table
 
 JOIN innings_table on innings_table.batsman = player_table.player
 join matches_table on matches_table.match_id = innings_table.match_id
 join player_table a on a.player = innings_table.bowler

 Where
matches_table.competition = 'IPL'
and innings_table.innings in ('1st', '2nd')


 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1)),
 innings_table.innings,
 cast(innings_table.over as real),
 a.bowl_type
 ")


player_query <- "select * from player_table"


bowl_query <- "select
 strftime('%Y', date(dates_1)) as year,
 cast(innings_table.over as real) as over,
 player_table.player,

 sum(cast(innings_table.runs_batsman as real) + 
 
 cast(innings_table.extras_wides as real) +
 
    cast(IFNULL(innings_table.extras_noballs, 0) as real))  as runs_conceded,
	
	
 sum(case when (cast(innings_table.extras_wides as real) > 0 or cast(ifnull(innings_table.extras_noballs,0) as real) > 0) then 0
 
 else 1
 
 END) as balls,
  sum(case 
			when length(innings_table.wicket_kind) > 1  AND
			player_table.player = innings_table.bowler and 
		innings_table.wicket_kind != 'run out'	then 1
			
			else 0
			
			end) as wickets
			
			
			 from player_table
 
 JOIN innings_table on innings_table.bowler = player_table.player
 join matches_table on matches_table.match_id = innings_table.match_id

where
matches_table.competition = 'IPL' AND
innings_table.innings in ('1st', '2nd')


group by
player_table.player,
 strftime('%Y', date(dates_1)),
 innings_table.innings,
 cast(innings_table.over as real)"




conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test5.db")

match_up_table2 <- dbGetQuery(conn, match_up_query)

bowl_table <- dbGetQuery(conn, bowl_query) 

player_table <- dbGetQuery(conn, player_query)



dbDisconnect(conn)

############ All Bowl Query #############













test_mu <- match_up_table2 %>% 
  group_by(bowl_type) %>% 
  summarise(n = n())

fwrite(test_mu)

match_up_table <- match_up_table2 %>% 
  mutate(bowl_type = case_when(grepl("Legbreak", bowl_type) ~ "Legbreak",
                               grepl("Offbreak|offbreak", bowl_type) ~ "Offbreak",
                               bowl_type == "Right-arm fast" ~ "Right-arm fast",
                               bowl_type == "Left-arm fast" ~ "Left-arm fast",
                               bowl_type %in% c("Left-arm fast-medium", "Left-arm medium", "Left-arm medium-fast") ~ "Left-arm medium",
                               bowl_type %in% c("Right-arm fast-medium", "Right-arm medium", "Right-arm medium-fast") ~ "Right-arm medium",
                               bowl_type == "Slow left-arm orthodox" ~ "SLA",
                               bowl_type == "Slow left-arm chinaman" ~ "Chinaman",
                               TRUE ~ bowl_type
         ))



















# error_test <- innings_table %>% 
#   group_by(innings) %>% 
#   summarise(n = n())
# 
# error_test2 <- innings_table %>% 
#   filter(!(innings %in% c("1st", "2nd")))
# 
# 
# graph_table <- all_table %>% 
#   mutate()
#   
#   #### Shiny dashboard ######
#   
#   ui <- pageWithSidebar(
#     
#     # App title ----
#     headerPanel("Cricket Explorer"),
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(),
#     
#     # Main panel for displaying outputs ----
#     mainPanel()
#   )
#   
#   
#   # Define server logic to plot various variables against mpg ----
#   server <- function(input, output) {
#     
#   }
#   
#   shinyApp(ui, server)
#   

  


