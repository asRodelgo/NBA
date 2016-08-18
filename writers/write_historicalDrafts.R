# Read draft players: http://www.basketball-reference.com/draft/NBA_2016.html

writeHistoricalDraftedRookies <- function(){
  
  rookiesHist <- data.frame()
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  for (draftYear in 1979:lastDraft) {
    
    url <- paste0("http://www.basketball-reference.com/draft/NBA_",draftYear,".html")
    thisSeasonDraft <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisSeasonDraft <- thisSeasonDraft[[1]]
    names(thisSeasonDraft) <- thisSeasonDraft[1,]
    thisSeasonDraft <- as.data.frame(thisSeasonDraft[-1,])
    thisSeasonDraft <- thisSeasonDraft[,1:10]
    thisSeasonDraft <- dplyr::select(thisSeasonDraft, Pick = Pk, Team = Tm, Player, College)
    thisSeasonDraft <- thisSeasonDraft[which(!(thisSeasonDraft$Pick=="" | thisSeasonDraft$Pick=="Pk")),]
    thisSeasonDraft <- mutate(thisSeasonDraft, Season = draftYear)
    if (nrow(rookiesHist)>0){
      rookiesHist <- bind_rows(rookiesHist,thisSeasonDraft)
    } else{
      rookiesHist <- thisSeasonDraft
    }
    
  }
  
  write.csv(rookiesHist, "data/rookiesHist.csv",row.names = FALSE)
  
}

write_HistCollegeStats <- function(col_G,num_pages){
  # Read stats from college players and match to drafted players
  # query college players who played at least col_G games. Min per games not recorded before 2009
  col_G <- 15
  num_pages <- 30
  # First 100 sorted desc by Total Points: 
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=1994&year_max=1994&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts
  # subsequent players in batches of 100:
  # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=1994&year_max=1994&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset=100
  collegePlayersHist <- data.frame()
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  for (season in 1994:(lastDraft-1)){
    
    url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                  season,"&year_max=",season,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                  col_G,"&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts")
    
    thisCollege <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisCollege <- thisCollege[[1]]
    names(thisCollege) <- thisCollege[1,]
    thisCollege <- thisCollege[-1,]
    collegePlayers <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
    
    for (page in 1:(num_pages-1)){ # read a total of num_pages*100 college players
      url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                    season,"&year_max=",season,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_f=Y&pos_is_fg=Y&pos_is_fc=Y&pos_is_c=Y&pos_is_cf=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                    col_G,"&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset=",
                    page*100)
      thisCollege <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="stats"]') %>%
        html_table(fill = TRUE)
      thisCollege <- thisCollege[[1]]
      names(thisCollege) <- thisCollege[1,]
      thisCollege <- thisCollege[-1,]
      thisCollege <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
      collegePlayers <- bind_rows(collegePlayers,thisCollege)
    }
    
    collegePlayers <- mutate(collegePlayers, Season = season)
    if (nrow(collegePlayersHist)>0){
      collegePlayersHist <- bind_rows(collegePlayersHist,collegePlayers)
    } else{
      collegePlayersHist <- collegePlayers
    }
    
  }
  
  write.csv(collegePlayersHist, "data/collegePlayersHist.csv", row.names = FALSE)
}

# Merge drafted players with college players
rookies <- read.csv("data/rookies.csv", stringsAsFactors = FALSE)
collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE)
rookieStats <- merge(rookies, collegePlayers, by = "Player",all.x=TRUE)

# Find stats from european players drafted
europePlayers <- data.frame()
lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1

for (i in 1:nrow(rookieStats)){
  
  if (rookieStats$College[i]==""){
    thisPlayer <- as.character(rookieStats$Player[i])
    name_edited <- tolower(thisPlayer)
    name_edited <- gsub(" ","-",name_edited)
    url <- paste0("http://www.basketball-reference.com/euro/players/",name_edited,"-1.html")
    thisEurope <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="totals"]') %>%
      html_table(fill = TRUE)
    if (length(thisEurope)>0){
      rookieStats$College[i] <- "Europe"
      thisEurope <- thisEurope[[1]] %>%
        filter(G == max(G)) %>%
        mutate(Player = thisPlayer)
      europePlayers <- bind_rows(europePlayers,thisEurope)
    } else{
      rookieStats$College[i] <- "International"
    }
  }
  
}
# remove duplicates in europePlayers and merge with rookieStats
europePlayers <- distinct(europePlayers, G, Player, .keep_all=TRUE)
europePlayers <- dplyr::select(europePlayers, Player, everything())



