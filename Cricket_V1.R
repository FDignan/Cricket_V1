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
library(janitor)





# install.packages("usethis")

library(usethis)
use_git_config(user.name = "FDignan", user.email = "finn.dignan0205@gmail.com")

###########################################
###### Unzip and move files ###################

setwd("C:/Users/fdignan/OneDrive - Imperial College London/Temp/All T20/")

t20_files <- list.files(pattern = "*.zip")

mapply(unzip, t20_files,  exdir = "yaml" )


##########################################



###################################################################################
############ Unpacks YAML files and organises into tabular form ##################
###################################################################################


aggr_fielder <- function(x) {
  paste0(x, collapse="/")
}




innings_list <- list()
matches_list <- list()
players_list <- list()


path = "yaml/"


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
  
  
     if ( "bowl_out" %in% y$L2 ) next 

  
  meta = y[y$L1 == 'meta',]
  meta = meta[, colSums(is.na(meta)) != nrow(meta), with=FALSE]
  data_meta = reshape(meta,direction = 'wide',timevar = 'L2',idvar = 'L1')
  
  info = y[y$L1 == 'info',]
  info = info[, colSums(is.na(info)) != nrow(info), with=FALSE]
  info[, L1 := NULL]

 
  ######## Code if not a tie or NR ##############
  
  
  if (!( "no result" %in% info$value|"tie" %in% info$value ) ) {info2 <- info %>%
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
    filter(!(column %in% c("players", "registry", "supersubs")) ) %>%
    spread(column, value)
    
    
    ##### International Competition ######
    
    if ("competition" %in% colnames(info2)){ info2 <- info2 %>%
    
    mutate(match_id = paste0(competition, "-", i))
    } else {
      
      info2 <- info2 %>%
        
        mutate(match_id = paste0("International", "-", i))
      
    }
  
  ##### Error if Outcome_by does not exist ######
  
  if ("outcome_by" %in% colnames(info2)){ info2 <- info2 %>% 
    separate(outcome_by, into = c("outcome_by_value", "outcome_by_type"), sep = " ")
  }
  
  
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
      filter(!(column %in% c("players", "registry", "supersubs")) ) %>% 
      spread(column, value) %>%
            
      mutate(
             outcome_by_value = "")
    
      if ("competition" %in% colnames(info2)){ info2 <- info2 %>%
        
        mutate(match_id = paste0(competition, "-", i))
      } else {
        
        info2 <- info2 %>%
          
          mutate(match_id = paste0("International", "-", i))
        
      }
      

    
    
    
    
  }
  
  

  
 if("competition" %in% colnames(info2))
  { competition_var <- info2$competition
  } else {
    competition_var <- "International"  
    
    }
  
  
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
  
  
  
  if ("players" %in% info$L2) {
    
    players <- info %>% 
      filter(L2 == "players") %>% 
      select(value, L3) %>% 
      mutate(match_id = paste0(competition_var, "-", i))
    
  }
  
  
  
  
  
  players_list[[i]] <- players
  
  innings_list[[i]] <- data_innings2
  
  matches_list[[i]] <- info2
  
  
}




########################################################




innings_table <- do.call(bind_rows, innings_list)

matches_table <- do.call(bind_rows, matches_list)

players_table <- do.call(bind_rows, players_list)


setwd("C:/Users/fdignan/OneDrive - Imperial College London/Temp/")

###### Amalgamates competitions and team names ######

matches_table <- matches_table %>% 
  mutate(teams_1 = ifelse(teams_1 == "Rising Pune Supergiant", "Rising Pune Supergiants", teams_1),
         teams_2 = ifelse(teams_2 == "Rising Pune Supergiant", "Rising Pune Supergiants", teams_2)) %>% 
  mutate(competition = case_when(competition %in% c("NatWest T20 Blast", "Vitality Blast") ~ "English T20",
                                 is.na(competition) ~ "International", 
                                 TRUE ~ competition)) 

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

######################################
######### Players Tables ###########
######################################


match_lineup_table <- players_table %>% 
  rename("player" = "value") 

base_player_table <- match_lineup_table %>% 
  distinct(player)


#### Cricsheet data #####

setwd("Cricket_V1/")

cricsheet_player_table <- fread("people_cricsheet.csv")

base_player_table2 <- base_player_table %>% 
  left_join(cricsheet_player_table, by = c("player" = "unique_name")) %>% 
  select(player, identifier, key_cricinfo)

### Cricinfo scraped #####

cricinfo_player_database <- fread("SuperCleaned.csv") %>% 
  clean_names() %>% 
  select(id, name, country, full_name, birthdate, birthplace, major_teams, batting_style, bowling_style)



########## Supplemantary player data #########
#### Players #####

setwd("All T20/")

lineups <- fread("T20+BBB+Data/team_lineups.csv") %>% 
  distinct(player_id, .keep_all = TRUE) %>% 
  select(player_id, player_name)

supp_players <- lineups %>% 
  filter(!(player_id %in% base_player_table2$key_cricinfo)) %>% 
  rename("player" = "player_name",
         "key_cricinfo" = "player_id")


########################################################
########### Final Player Table ##########################
#######################################################

base_player_table3 <- base_player_table2 %>% 
  bind_rows(supp_players) %>% 
  left_join(cricinfo_player_database, by = c("key_cricinfo" = "id")) %>% 
  mutate(bowling_style = gsub(",.*", "", bowling_style)) %>% 
  mutate(rolled_up_bowl_type = case_when(grepl("Legbreak", bowling_style) ~ "Legbreak",
                               grepl("Offbreak|offbreak",bowling_style) ~ "Offbreak",
                               bowling_style == "Right-arm fast" ~ "Right-arm fast",
                               bowling_style == "Left-arm fast" ~ "Left-arm fast",
                               bowling_style %in% c("Left-arm fast-medium", "Left-arm medium", "Left-arm medium-fast") ~ "Left-arm medium",
                               bowling_style %in% c("Right-arm fast-medium", "Right-arm medium", "Right-arm medium-fast") ~ "Right-arm medium",
                               bowling_style == "Slow left-arm orthodox" ~ "SLA",
                               bowling_style == "Slow left-arm chinaman" ~ "Chinaman",
                               TRUE ~ bowling_style
  ))



##################################
########## Competition table ####
###################################

setwd("Cricket_V1/")


competition_table <- matches_table %>% 
  distinct(competition) %>% 
  mutate(competition_abbr = ifelse(grepl(" ", competition), toupper(sapply(strsplit(competition, " "), function(x){
    paste(substring(x, 1, 1), collapse = "")})), 
    toupper(substr(competition, 0, 3))))


###########################
########## Grounds #######
############################


grounds_table <- matches_table %>% 
  select(venue, city) %>% 
  distinct(venue, .keep_all = TRUE) %>% 
  mutate(city = ifelse(is.na(city), gsub("\\s.*", "", venue), city))

####################
####### Teams #####
####################

teams_table <- data.frame(c(as.vector(matches_table$teams_1),
                            as.vector(matches_table$teams_2)))

names(teams_table) <- "team"


########################################


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


#########################################
######### Supplementary Data #######
#############################################
##############################


supp_match <- fread("T20+BBB+Data/match_info.csv") %>% 
  mutate(home_team_name = ifelse(home_team_name == "Rising Pune Supergiant", "Rising Pune Supergiants", home_team_name),
         away_team_name = ifelse(away_team_name == "Rising Pune Supergiant", "Rising Pune Supergiants", away_team_name)) %>% 
  mutate(dates_1 = as.Date(start_local, "%d/%m/%Y %H:%M"))


##############################################
############ Competition Lookup Table #####


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



#########################################################


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

test_query <- test_innings %>% 
  filter(batsman == "JM Bairstow") %>% 
  summarise(sum = sum(as.numeric(runs_batsman), na.rm = TRUE),
            matches = n_distinct(match_id)) 

########################################################
##### Final Transform ##############################


### Write to Database #####


conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test7.db")

dbWriteTable(conn, "matches_table", match_table_final, overwrite = TRUE)

dbWriteTable(conn, "innings_table", test_innings, overwrite = TRUE)

dbWriteTable(conn, "player_table", base_player_table3, overwrite = TRUE)

dbWriteTable(conn, "grounds_table", grounds_table, overwrite = TRUE)

dbWriteTable(conn, "competition_table", competition_table , overwrite = TRUE)

dbWriteTable(conn, "teams_table", teams_table4, overwrite = TRUE)


dbDisconnect(conn)













