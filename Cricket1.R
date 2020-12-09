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

#### NEWER #####

# install.packages("downloader")

setwd("C:/Users/Dignanf/Documents/Cricket/")

test <- yaml.load_file("ipl_male/335982.yaml")



temp <- tempfile()


wd <- getwd()

download.file("https://cricsheet.org/downloads/recently_added_7_male.zip", temp)

unzip(temp, exdir = paste0(wd, "/test"))

unlink(temp)


# install.packages("usethis")

library(usethis)
use_git_config(user.name = "FDignan", user.email = "finn.dignan0205@gmail.com")


### Database Tables ####

#### Balls #####
#### Match ID
##### Innings ID
##### Ball ID 
###### Player IDs
###### Runs
###### Extras 
###### Wicket
###### Fielder
###### Method


###### Matches ####
###### Date
###### Match ID 
###### Series ID
###### Umpires
###### Ground
###### Players ID
##### Outcome
##### Teams ID
###### Toss
##### Overs
##### Scores



###### Players ####
###### Handed
###### DOB 
###### Country


###### Series 


###### Grounds


##### Teams ##

# x = melt(test)
# y = data.frame(x)
# 
# 
# meta = y[y$L1 == 'meta',]
# meta = meta[, colSums(is.na(meta)) != nrow(meta)]
# data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
# 
# 
# 
# 
# data_innings = y[(y$L1 == 'innings') & (y$L4 == 'deliveries'),]
# data_innings$new = paste(data_innings$L7,data_innings$L8,sep="_")
# data_innings = subset(data_innings, select=-c(L7,L8,L4,L1,L5))
# data_innings = reshape(data_innings,idvar=c('L2','L3','L6'),direction = "wide",timevar = c('new'))
# # write.csv(data_innings,"data_innings.csv",row.names = F)
# 
# info = y[y$L1 == 'info',]
# info = info[, colSums(is.na(info)) != nrow(info)]
# info = subset(data_innings, select=-c(L1))










######### Second attempt ########

# data_innings_full = data.frame(matrix(ncol=0,nrow=0))
# matches_full = data.frame(matrix(ncol=0,nrow=0))
# datalist = list()
# 
# path <- "ipl_male/"
# path2 <- "Cricsheet_CSVs/"
# 
# aggr_fielder <- function(x) {
#   paste0(x, collapse="/")
# }
# 
# convertCricsheetData <- function(source){
#   require(yaml)
#   require(reshape2)
#   require(data.table)
#   all.files <- list.files(path = source,
#                           pattern = ".yaml",
#                           full.names = TRUE)
#   
#   for (i in 1:length(all.files)) {
#     data = yaml.load_file(all.files[i])
#     x = melt(data)
#     y = data.table(x)
#     
#     meta = y[y$L1 == 'meta',]
#     meta = meta[, colSums(is.na(meta)) != nrow(meta), with=FALSE]
#     data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
#     
#     info = y[y$L1 == 'info',]
#     info = info[, colSums(is.na(info)) != nrow(info), with=FALSE]
#     info[, L1 := NULL]
#     # info[,match_no := i]
#     
#     data_innings = y[(y$L1 == 'innings') & (y$L4 == 'deliveries'),]
#     data_innings[, new := paste(data_innings$L7,data_innings$L8,sep="_")]
#     data_innings [, c("L7","L8","L4","L1","L5") := NULL]
#     data_innings = dcast(data_innings, L2+L3+L6 ~ new, fun.aggregate = aggr_fielder,fill = "") 
#     
#     
#     ### Formats tables - binds together #######
#     
# 
#     
#     
#     # info2 <- info %>% 
#     #   rename(column = L2) %>% 
#     #   mutate(column = case_when(column %in% c("outcome", "toss") ~ paste0(column, "_", L3),
#     #                             TRUE ~ column)) %>% 
#     #   mutate(value = case_when(column == "outcome_by" ~ paste0(value, " ", L4),
#     #                            TRUE ~ value)) %>% 
#     #   select(value, column) %>% 
#     #   group_by(column) %>% 
#     #   mutate(sequence = row_number()) %>% 
#     #   mutate(column = case_when(column %in% c("teams", "umpires") ~ paste0(column, "_", sequence),
#     #                             TRUE ~ column)) %>% 
#     #   ungroup() %>%
#     #   select(-sequence) %>% 
#     #   spread(column, value) %>%
#     #   separate(outcome_by, into = c("outcome_by_value", "outcome_by_type"), sep = " ") %>% 
#     #   mutate(match_id = paste(competition, "_", i))
#     # 
#     # 
#     # competition_var <- info$competition
#     
# 
#       # data_innings2 = data_innings %>% 
#       # rename(match_id = L2,
#       #        innings = L3,
#       #        over = L6,
#       #        bowler = bowler_NA,
#       #        batsman = batsman_NA,
#       #        non_striker = non_striker_NA
#       # ) %>% 
#       # mutate(innings = substr(innings, 0,3)) %>% 
#       # separate(over, into =  c("over", "ball"), sep = "\\.") 
#       # 
#     # %>% 
#     #   mutate(match_id = paste(competition_var, "_", i))
#        
#        
#        
#     # matches_full <- bind_rows(matches_full, info2, .id = NULL)
#     
#     # data_innings_full <- bind_rows(data_innings_full, data_innings2, .id = NULL)
#     
#     
#     datalist[[i]] <- data_innings
#     
#     
#     # data_innings[,match_no := i]
#     # write.csv(data_innings,paste0(destination,paste(c(info[info$L2 == "dates",]$value,info[info$L2 == "teams",]$value), collapse = "-"),".csv"),row.names = F)
#     # write.csv(info,paste0(destination,paste(c("info",info[info$L2 == "dates",]$value,info[info$L2 == "teams",]$value), collapse = "-"),".csv"),row.names = F)
#   
#   }
# }
# 
# 
# convertCricsheetData(path)
# 
# 
# 
# remove(data_innings)
# remove(info)
# 
# remove(data_innings2)
# 
# 
# 
# y = data.table(x)
# meta = y[y$L1 == 'meta',]
# meta = meta[, colSums(is.na(meta)) != nrow(meta), with=FALSE]
# data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
# 
# info = y[y$L1 == 'info',]
# info = info[, colSums(is.na(info)) != nrow(info), with=FALSE]
# info[, L1 := NULL]
# info[,match_no := 1]
# 
# data_innings = y[(y$L1 == 'innings') & (y$L4 == 'deliveries'),]
# data_innings[, new := paste(data_innings$L7,data_innings$L8,sep="_")]
# data_innings [, c("L7","L8","L4","L1","L5") := NULL]
# data_innings = dcast(data_innings, L2+L3+L6 ~ new, fun.aggregate = aggr_fielder,fill = "")
# 
# 
# 
# 
# data_innings2 <- data_innings %>% 
#   rename(match_id = L2,
#          innings = L3,
#          over = L6,
#          bowler = bowler_NA,
#          batsman = batsman_NA,
#          non_striker = non_striker_NA
#          ) %>% 
# mutate(innings = substr(innings, 0,3)) %>% 
#  separate(over, into =  c("over", "ball"), sep = "\\.")
# 
# 
# 
# info2 <- info %>% 
#   rename(column = L2) %>% 
#   mutate(column = case_when(column %in% c("outcome", "toss") ~ paste0(column, "_", L3),
#                                                         TRUE ~ column)) %>% 
#   mutate(value = ifelse(column == "outcome_by", paste0(value, " ", L4), value)) %>% 
#   select(value, column) %>% 
#   group_by(column) %>% 
#   mutate(sequence = row_number()) %>% 
#   mutate(column = case_when(column %in% c("teams", "umpires") ~ paste0(column, "_", sequence),
#                            TRUE ~ column)) %>% 
#   ungroup() %>%
#   select(-sequence) %>% 
#   spread(column, value) %>%
#   separate(outcome_by, into = c("outcome_by_value", "outcome_by_type"), " ") %>% 
#   mutate(match_id = paste(competition, "_", i))
# 
# 
# 
# 
# info2 <- info %>% 
#   rename(column = L2) %>% 
#   mutate(column = case_when(column %in% c("outcome", "toss") ~ paste0(column, "_", L3),
#                             TRUE ~ column)) %>% 
#   mutate(value = case_when(column == "outcome_by" ~  paste0(value, " ", L4),
#                            TRUE ~ value)) %>% 
#   select(value, column) %>% 
#   group_by(column) %>% 
#   mutate(sequence = row_number()) %>% 
#   mutate(column = case_when(column %in% c("teams", "umpires") ~ paste0(column, "_", sequence),
#                             TRUE ~ column)) %>% 
#   ungroup() %>%
#   select(-sequence) %>% 
#   spread(column, value) %>%
#   separate(outcome_by, into = c("outcome_by_value", "outcome_by_type"), " ") 
#   
# 
# 
# 
# matches_full2 <- rbind(matches_full, info2)

######################
#######################

remove(x)
remove(y)
remove(data_innings)
remove(info)

aggr_fielder <- function(x) {
  paste0(x, collapse="/")
}




innings_list <- list()
matches_list <- list()


path = "All T20/"


require(yaml)
require(reshape2)
require(data.table)
all.files <- list.files(path = path,
                        pattern = ".yaml",
                        full.names = TRUE)

for (i in 1:length(all.files)) {
  data = yaml.load_file(all.files[i])
  x = melt(data)
  y = data.table(x)
  
  
  
  meta = y[y$L1 == 'meta',]
  meta = meta[, colSums(is.na(meta)) != nrow(meta), with=FALSE]
  data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
  
  info = y[y$L1 == 'info',]
  info = info[, colSums(is.na(info)) != nrow(info), with=FALSE]
  info[, L1 := NULL]
  # info[,match_no := i]
  
  # !("tie" %in% as.vector(info$value)|"no result" %in% as.vector(info$value))|!
  
  if (("L4" %in% colnames(info))  ) {info2 <- info %>%
    rename(column = L2) %>%
    mutate(column = case_when(column %in% c("outcome", "toss") ~ paste0(column, "_", L3),
                              TRUE ~ column)) %>%
    mutate(value = case_when(column == "outcome_by" ~ paste0(value, " ", L4),
                             TRUE ~ value)) %>%
    select(value, column) %>%
    group_by(column) %>%
    mutate(sequence = row_number()) %>%
    mutate(column = case_when(column %in% c("teams", "umpires", "dates", "player_of_match") ~ paste0(column, "_", sequence),
                              TRUE ~ column)) %>%
    ungroup() %>%
    select(-sequence) %>%
    spread(column, value) %>%
    mutate(match_id = paste0(competition, "-", i)) %>% 
    separate(outcome_by, into = c("outcome_by_value", "outcome_by_type"), sep = " ")
  
  
  }   else {
    
    info2 <- info %>%
      rename(column = L2) %>%
      
      mutate(column = case_when(column  == "outcome" & L3 == "eliminator" ~ paste0(column, "_", "winner"),
                                column == "outcome" & L3 == "result" ~ paste0(column, "_", "by_type"),
                                column == "outcome" & L3 == "method" ~ paste0(column, "_", L3),
                                TRUE ~ column),
             column = case_when(column == "toss" ~ paste0(column, "_", L3),
                                TRUE ~ column)) %>%
      select(value, column) %>%
      group_by(column) %>%
      mutate(sequence = row_number()) %>%
      mutate(column = case_when(column %in% c("teams", "umpires", "dates", "player_of_match") ~ paste0(column, "_", sequence),
                                TRUE ~ column)) %>%
      ungroup() %>%
      select(-sequence) %>%
      spread(column, value) %>%
      mutate(match_id = paste0(competition, "-", i),
             outcome_by_value = "")
    
    
    
    
  }
  
  
  
  competition_var <- info2$competition
  
  
  data_innings = y[(y$L1 == 'innings') & (y$L4 == 'deliveries'),]
  data_innings[, new := paste(data_innings$L7,data_innings$L8,sep="_")]
  data_innings [, c("L7","L8","L4","L1","L5") := NULL]
  data_innings = dcast(data_innings, L2+L3+L6 ~ new, fun.aggregate = aggr_fielder,fill = "")
  
  
  
  data_innings2 = data_innings %>%
    rename(match_id = L2,
           innings = L3,
           over = L6,
           bowler = bowler_NA,
           batsman = batsman_NA,
           non_striker = non_striker_NA
    ) %>%
    mutate(innings = substr(innings, 0,3)) %>%
    separate(over, into =  c("over", "ball"), sep = "\\.") %>% 
    mutate(match_id = paste0(competition_var, "-", i))
  
  
  innings_list[[i]] <- data_innings2
  
  matches_list[[i]] <- info2
  
  
}





innings_table <- do.call(bind_rows, innings_list)

matches_table <- do.call(bind_rows, matches_list)


matches_table <- matches_table %>% 
  mutate(teams_1 = ifelse(teams_1 == "Rising Pune Supergiant", "Rising Pune Supergiants", teams_1),
         teams_2 = ifelse(teams_2 == "Rising Pune Supergiant", "Rising Pune Supergiants", teams_2)) %>% 
  mutate(competition = ifelse(competition %in% c("NatWest T20 Blast", "Vitality Blast"), "English T20", competition))




innings_table <- innings_table %>% 
  mutate(batsman = as.character(map(strsplit(batsman, "/"), 1)),
         non_striker = as.character(map(strsplit(non_striker, "/"), 1)),
         bowler = as.character(map(strsplit(bowler, "/"), 1))) 




#### Player table ######


player_table <- data.frame(c(as.vector(innings_table$batsman), 
                             as.vector(innings_table$non_striker), 
                             as.vector(innings_table$bowler)
) ) 

names(player_table) <- "player"

player_table <- player_table %>% 
  distinct(player)



########## Competition table ####



competition_table <- matches_table %>% 
  distinct(competition) %>% 
  mutate(competition_abbr = ifelse(grepl(" ", competition), toupper(sapply(strsplit(competition, " "), function(x){
    paste(substring(x, 1, 1), collapse = "")})), 
    toupper(substr(competition, 0, 3))))



########## Grounds #######

grounds_table <- matches_table %>% 
  select(venue, city) %>% 
  distinct(venue, .keep_all = TRUE)

####### Teams #####

teams_table <- data.frame(c(as.vector(matches_table$teams_1),
                            as.vector(matches_table$teams_2)))

names(teams_table) <- "team"


teams_table <- teams_table %>% 
  distinct(team, .keep_all = TRUE) %>% 
  mutate(team_abbr = ifelse(grepl(" ", team), toupper(sapply(strsplit(team, " "), function(x){
    paste(substring(x, 1, 1), collapse = "")})), 
    toupper(substr(team, 0, 3))
    
  )) %>% 
  group_by(team_abbr) %>% 
  mutate(sequence = row_number()) %>% 
  mutate(team_abbr = ifelse(sequence == 2, paste0(team_abbr, sequence), team_abbr)) %>% 
  select(-sequence)


#### Creating unique match key #####

# matches_table2 <- matches_table %>%
#   # left_join(teams_table, by = c("teams_1" = "team")) %>% 
#   # left_join(teams_table, by = c("teams_2" = "team")) %>% 
#   left_join(competition_table, by = c("competition" = "competition")) %>% 
#   # mutate(match_id2 = paste0(competition_abbr, team_abbr.x, team_abbr.y, format(as.Date(dates_1), "%Y%m%d")))
# 
# 
# 
# 
# matches_table <- matches_table2 %>% 
#   mutate(match_id = match_id2) %>% 
#   select(-match_id2)


##############################

######### Supplementary Data #######
#### Players #####

lineups <- fread("T20+BBB+Data/team_lineups.csv") %>% 
  distinct(player_id, .keep_all = TRUE) %>% 
  select(player_id, player_name)

supp_player <- fread("T20+BBB+Data/player_info.csv") %>% 
  left_join(lineups, by = c("player_id" = "player_id"))


player_table2 <- player_table %>% 
  left_join(supp_player, by = c("player" = "player_name"))



missing_players <- player_table2 %>% 
  filter(is.na(player_id)) %>% 
  select(player, names) %>% 
  left_join(supp_player, by = c("player" = "names")) %>% 
  filter(!is.na(player_id))



#########################################




supp_match <- fread("T20+BBB+Data/match_info.csv") %>% 
  mutate(home_team_name = ifelse(home_team_name == "Rising Pune Supergiant", "Rising Pune Supergiants", home_team_name),
         away_team_name = ifelse(away_team_name == "Rising Pune Supergiant", "Rising Pune Supergiants", away_team_name)) %>% 
  mutate(dates_1 = as.Date(start_local, "%d/%m/%Y %H:%M"))


test6 <- supp_match %>% 
  filter(is.na(home_team_name))


competition_table2 <- fread("comp2.csv")

###################


teams_table_supp <- data.frame(c(as.vector(supp_match$home_team_name),
                                 as.vector(supp_match$away_team_name)))

names(teams_table_supp) <- "team"


teams_table3 <- teams_table_supp %>% 
  distinct(team) %>% 
  left_join(teams_table, by = c("team" = "team")) %>% 
  filter(is.na(team_abbr))


teams_table4 <- teams_table3 %>% 
  # rename(team = home_team_name) %>% 
  bind_rows(teams_table) %>% 
  select(-team_abbr) %>% 
  mutate(team_abbr = ifelse(grepl(" ", team), toupper(sapply(strsplit(team, " "), function(x){
    paste(substring(x, 1, 1), collapse = "")})), 
    toupper(substr(team, 0, 3)))) %>% 
  group_by(team_abbr) %>% 
  mutate(sequence = row_number()) %>% 
  mutate(team_abbr = ifelse(sequence > 1, paste0(team_abbr, sequence), team_abbr))%>% 
  select(-sequence) %>% 
  ungroup() %>% 
  arrange(team) %>% 
  mutate(sequence2 = row_number())



#### Finds unique matches in two datasets #########


supp_match2 <- supp_match %>% 
  left_join(teams_table4, by = c("home_team_name" = "team")) %>% 
  left_join(teams_table4, by = c("away_team_name" = "team")) %>% 
  left_join(competition_table2, by = c("series" = "series")) %>% 
  
  mutate(match_id2 = paste0(comp_abbr, ifelse(sequence2.x < sequence2.y, team_abbr.x, team_abbr.y),
                            ifelse(sequence2.x < sequence2.y, team_abbr.y, team_abbr.x),
                            format(as.Date(dates_1), "%Y%m%d"))) %>% 
  separate(ground_name, into = c("venue", "city"), sep = ",(?=[^,]+$)") %>% 
  mutate(gender = "male",
         match_type = "T20",
         toss_winner = case_when(toss_winner == "Home" ~ home_team_name,
                                 toss_winner == "Away" ~ away_team_name),
         outcome_winner = case_when(match_winner == "Home" ~  home_team_name, 
                                    match_winner == "Away" ~ away_team_name),
         dates_1 = as.Date(dates_1, "%Y/%m/%d"),
  ) %>% 
  rename(teams_1 = home_team_name,
         teams_2 = away_team_name) %>% 
  select(-start_local) %>% 
  rename(match_id_old = match_id,
         match_id = match_id2) %>% 
  mutate(match_id_old = as.character(match_id_old)) %>% 
  filter(!(is.na(teams_1)&is.na(teams_2)))




matches_table3 <- matches_table %>%
  left_join(teams_table4, by = c("teams_1" = "team")) %>% 
  left_join(teams_table4, by = c("teams_2" = "team")) %>% 
  left_join(competition_table, by = c("competition" = "competition")) %>%
  mutate(match_id2 = paste0(competition_abbr, ifelse(sequence2.x < sequence2.y, team_abbr.x, team_abbr.y),
                            ifelse(sequence2.x < sequence2.y, team_abbr.y, team_abbr.x),
                            format(as.Date(dates_1), "%Y%m%d")),
         dates_1 = as.Date(dates_1, "%Y-%m-%d")) %>% 
  rename(match_id_old = match_id,
         match_id = match_id2) 









match_ids_join <- supp_match2 %>%
  select(match_id, match_id_old) %>% 
  mutate(match_id_old = as.numeric(match_id_old))


match_ids <- matches_table3 %>% 
  distinct(match_id, .keep_all = TRUE) %>% 
  select(match_id, competition_abbr)

combi_match <- supp_match2 %>% 
  left_join(match_ids, by = c("match_id" = "match_id")) %>% 
  filter(is.na(competition_abbr))


non_dupe_match_keys <- combi_match %>% 
  select(match_id) %>% 
  mutate(dupe = 1)

non_dupe_match_keys2 <- combi_match %>% 
  select(match_id) %>% 
  mutate(dupe = 1)

supp_match_join <- supp_match2 %>% 
  filter((match_id %in% as.vector(non_dupe_match_keys2$match_id)))



match_table_final <- bind_rows(matches_table3, supp_match_join) %>% 
  mutate(dates_1 = as.character(dates_1))



##### Final innings table #####

key_table <- matches_table3 %>% 
  select(match_id, match_id_old) %>% 
  rename(match_id_new = match_id)

innings_table <- innings_table %>% 
  left_join(key_table, by  = c("match_id" = "match_id_old")) %>% 
  mutate(match_id = match_id_new) %>% 
  select( -match_id_new)
###########################################

supp_ball <- fread("T20+BBB+Data/ball_info.csv")  




ball_test <- supp_ball %>% 
  # left_join(non_dupe_match_keys) %>% 
  # filter(!is.na(match_id2)) %>% 
  group_by(match_id, innings, ball_id) %>% 
  mutate(seq = row_number()) %>% 
  filter(seq != 1) %>% 
  mutate(dupe = 1)


######  Team order determined alphabetically #####


ball_test3 <- ball_test %>% 
  ungroup() %>% 
  distinct(match_id, .keep_all = TRUE) %>% 
  mutate(dupe = 1)



ball_test2 <- ball_test %>% 
  ungroup() %>% 
  select(match_id, innings, ball_id) %>% 
  mutate(dupe = 1)



ball_join <- supp_ball %>% 
  left_join(ball_test3, by = c("match_id" = "match_id",
                               "innings" = "innings",
                               "ball_id" = "ball_id")) %>% 
  filter(!is.na(dupe)) %>% 
  ungroup() %>% 
  distinct(match_id)

###########################

supp_ball_transform <- supp_ball %>% 
  left_join(match_ids_join, by = c("match_id" = "match_id_old")) %>% 
  rename(match_id_old = match_id,
         match_id = match_id.y) %>% 
  filter(!(match_id_old %in% ball_join$match_id )) %>% 
  filter((match_id %in% non_dupe_match_keys$match_id)) %>% 
  spread(play_type, score_value, fill = 0) %>% 
  mutate(runs_batsman = six + four + run) %>% 
  rename(extras_byes = bye,
         extras_wides = wide,
         extras_noballs = `no ball`,
         extras_legbyes = `leg bye`,
         wicket_player_out = wicket_who,
         wicket_kind = wicket_how,
         wicket_fielders = fielder) %>% 
  select(-out, -six, -four, -run, -`no run`,-ball_no, -is_keeper ) %>% 
  separate(ball_id, into =  c("over", "ball"), sep = "\\.") %>% 
  mutate(ball = sub("^[0]+", "", ball),
         innings = ifelse(innings == "1", "1st", "2nd"),
         runs_extras = extras_byes + extras_wides + extras_noballs + extras_legbyes) %>% 
  mutate(runs_total = runs_extras + runs_batsman) %>% 
  
  mutate_at(vars(contains(c("extras", "runs"), ignore.case = FALSE)), as.character)




test_innings <- bind_rows(innings_table, supp_ball_transform) 

###########################################################
### Match Table has new key, innings table does not #######
###########################################################

test_query <- innings_table %>% 
  filter(batsman == "JM Bairstow") %>% 
  summarise(sum = sum(as.numeric(runs_batsman), na.rm = TRUE))

### Write to Database #####


conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test3.db")

dbWriteTable(conn, "matches_table", match_table_final, overwrite = TRUE)

dbWriteTable(conn, "innings_table", test_innings, overwrite = TRUE)

dbWriteTable(conn, "player_table", player_table2, overwrite = TRUE)

dbWriteTable(conn, "grounds_table", grounds_table, overwrite = TRUE)

dbWriteTable(conn, "competition_table", competition_table2 , overwrite = TRUE)

dbWriteTable(conn, "teams_table", teams_table4, overwrite = TRUE)


dbDisconnect(conn)



query <- paste0("select player_table.player,
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

where player_table.player = 'DA Warner' and matches_table.competition = 'IPL'

 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1))
 ")


query2 <- paste0("select player_table.player,
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
and innings_table.innings = '1st'

 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1))
 ")

query3 <- paste0("select player_table.player,
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
and innings_table.over > 15

 
 GROUP BY player_table.player,
 strftime('%Y', date(dates_1))
 ")


##### Query test ######

conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test2.db")

selecttest_table <- dbGetQuery(conn, query2)


death <- dbGetQuery(conn, query3)

dbDisconnect(conn)

sr_test <- selecttest_table %>% 
  mutate(SR = (runs/balls)*100,
         average = runs/out) %>% 
  filter(runs > 100)

library(ggplot2)

death2 <- death %>% 
  mutate(SR = (runs/balls)*100,
         average = runs/out) %>% 
  filter(balls > 50)

death_good <- death2 %>% 
  filter(SR > 175 & average > 25)




high2 <-sr_test %>% 
  filter(average > 25 & SR > 160)

bad2 <- sr_test %>% 
  filter(average < 40 & SR < 100)


test_sr <- ggplot(death2, aes(x = SR, y = average)) + 
  geom_point() +
  theme_bw()

test_sr






high <-sr_test %>% 
  filter(average > 25 & SR > 160)

bad <- sr_test %>% 
  filter(average < 40 & SR < 100)


########


high <-sr_test %>% 
  filter(average > 25 & SR > 160)

bad <- sr_test %>% 
  filter(average < 40 & SR < 100)














