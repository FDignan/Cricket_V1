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


# install.packages("downloader")

setwd("C:/Users/Dignanf/Documents/Cricket/")

# test <- yaml.load_file("ipl_male/335982.yaml")



temp <- tempfile()


wd <- getwd()

download.file("https://cricsheet.org/downloads/recently_played_7_male.zip", temp)

unzip(temp, exdir = paste0(wd, "/test2"))

unlink(temp)




aggr_fielder <- function(x) {
  paste0(x, collapse="/")
}




innings_list <- list()
matches_list <- list()


path = paste0(wd, "/test2")

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
  
  
  if (!("competition" %in% info$L2 )) { next }
  
  
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



##### Brings in existing tables #######

teams_query <- "Select * from teams_table"

matches_query <- "select * from matches_table"

comp_query <- "select * from competition_table"

ground_query <- "select * from grounds_table"

player_query <- "select * from player_table"


conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test3.db")

teams_table <- dbGetQuery(conn, teams_query)

matches_table_old <- dbGetQuery(conn, matches_query)

comp_table <- dbGetQuery(conn, comp_query)

ground_table <- dbGetQuery(conn, ground_query)

player_table <- dbGetQuery(conn, player_query)


dbDisconnect(conn)

#########################################
##### New players ######################

player_table_update <- data.frame(c(as.vector(innings_table$batsman), 
                             as.vector(innings_table$non_striker), 
                             as.vector(innings_table$bowler)
) ) 

names(player_table_update) <- "player"

player_table_update <- player_table_update %>% 
  distinct(player)

new_player <- player_table_update %>% 
  filter(!(player %in% player_table$player))

player_table2 <- bind_rows(player_table, new_player)

#######################################
######### Grounds Update #############

grounds_table_update <- matches_table %>% 
  select(venue, city) %>% 
  distinct(venue, .keep_all = TRUE) %>% 
  filter(!(venue %in% ground_table$venue))

grounds_table_new <- bind_rows(ground_table, grounds_table_update) 

###########################################
############# Teams Update ###############

teams_table_new <- data.frame(c(as.vector(matches_table$teams_1),
                            as.vector(matches_table$teams_2)))

names(teams_table_new) <- "team"

teams_table_new <- teams_table_new %>% 
  filter(!(team %in% teams_table$team)) %>% 
distinct(team, .keep_all = TRUE) %>% 
  mutate(team_abbr = ifelse(grepl(" ", team), toupper(sapply(strsplit(team, " "), function(x){
    paste(substring(x, 1, 1), collapse = "")})), 
    toupper(substr(team, 0, 3))
    
  )) %>% 
  group_by(team_abbr) %>% 
  mutate(sequence = row_number()) %>% 
  mutate(team_abbr = ifelse(sequence == 2, paste0(team_abbr, sequence), team_abbr)) %>% 
  select(-sequence)


# teams_table_update <- bind_rows(teams_table, teams_table_new)

#################################################
############# Matches Update ####################

matches_table_new <- matches_table %>% 

left_join(teams_table, by = c("teams_1" = "team")) %>% 
  left_join(teams_table, by = c("teams_2" = "team")) %>% 
  left_join(comp_table, by = c("competition" = "competition")) %>%
  mutate(match_id2 = paste0(competition_abbr, ifelse(sequence2.x < sequence2.y, team_abbr.x, team_abbr.y),
                            ifelse(sequence2.x < sequence2.y, team_abbr.y, team_abbr.x),
                            format(as.Date(dates_1), "%Y%m%d")),
         dates_1 = as.Date(dates_1, "%Y-%m-%d")) %>% 
  rename(match_id_old = match_id,
         match_id = match_id2) %>% 
  filter(!(match_id %in% matches_table_old$match_id)) %>% 
  mutate(dates_1 = as.character(dates_1))

match_table_all <- bind_rows(matches_table_old, matches_table_new)

test_matches <- match_table_all %>% 
  filter(competition_abbr == "IPL") %>% 
  group_by(dates_1) %>% 
  summarise(n = n())

###################################################
############ Innings update #######################

key_table <- matches_table_new %>% 
  select(match_id, match_id_old) %>% 
  rename(match_id_new = match_id)

innings_table <- innings_table %>% 
  left_join(key_table, by  = c("match_id" = "match_id_old")) %>% 
  mutate(match_id = match_id_new) %>% 
  select( -match_id_new)

innings_table_update <- innings_table %>% 
  filter(!is.na(match_id))

# innings_table_update <- bind_rows()

##################################################

conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test3.db")

dbWriteTable(conn, "matches_table", matches_table_new, append = TRUE)

dbWriteTable(conn, "innings_table", innings_table_update, append = TRUE)

dbWriteTable(conn, "player_table", new_player, append = TRUE)

dbWriteTable(conn, "grounds_table", grounds_table_update, append = TRUE)


dbWriteTable(conn, "teams_table", teams_table_new, append = TRUE)


dbDisconnect(conn)


query_test <- "select * from matches_table"

conn <- dbConnect(RSQLite::SQLite(), "Cricket_Database_test3.db")

test_table <- dbGetQuery(conn, query_test)

dbDisconnect(conn)














