# Remove players duplicates names

##
##
.rename_PlayerDuplicates <- function(data) {
  
  data <- mutate(data, Player = gsub("*","",Player, fixed=TRUE)) %>%
    group_by(Player) %>%
    mutate(yearBorn = as.numeric(substr(Season,1,4)) - Age) %>%
    as.data.frame()
  # Players with the same name will create silly duplicates. Identify them
  playerDups <- group_by(data,Player) %>%
    filter(max(yearBorn)-min(yearBorn) > 1) %>%
    distinct(Player, Season, .keep_all = TRUE) %>%
    arrange(Player,desc(Season)) %>%
    group_by(Player,yearBorn) %>%
    arrange(Player, yearBorn) %>%
    distinct(Player,yearBorn,.keep_all=TRUE) %>%
    dplyr::select(Player,yearBorn) %>%
    group_by(Player) %>%
    mutate(id = row_number()) %>%
    as.data.frame()
  # Rename them: second: 2, third: 3, etc.
  data <- merge(data,playerDups, by=c("Player","yearBorn"),all.x = TRUE) %>%
    mutate(Player = ifelse(!is.na(id), ifelse(id > 1, paste(Player,id),Player), Player)) %>%
    select(-id, -yearBorn) %>% distinct(Player, Tm, Season, .keep_all=TRUE) %>% as.data.frame()
  
}
