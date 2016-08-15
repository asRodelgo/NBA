# Read draft players: http://www.basketball-reference.com/draft/NBA_2016.html

writeDraftedRookies <- function(){
  
  rookies <- data.frame()
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  url <- paste0("http://www.basketball-reference.com/draft/NBA_",lastDraft,".html")
  
  thisSeasonDraft <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="stats"]') %>%
    html_table(fill = TRUE)
  thisSeasonDraft <- thisSeasonDraft[[1]]
  names(thisSeasonDraft) <- thisSeasonDraft[1,]
  thisSeasonDraft <- as.data.frame(thisSeasonDraft[-1,])
  rookies <- thisSeasonDraft[,1:10]
  rookies <- dplyr::select(rookies, Pick = Pk, Team = Tm, Player, College)
  rookies <- rookies[which(!(rookies$Pick=="" | rookies$Pick=="Pk")),]
  
  # Correct spelling errors 2016 draft
  rookies[grepl("Chris",rookies$Player),]$Player <- "Marquese Chriss"
  rookies[grepl("Dami",rookies$Player),]$Player <- "Damian Jones"
  rookies[grepl("Zimmerm",rookies$Player),]$Player <- "Stephen Zimmerman Jr."
  
  write.csv(rookies, "data/rookies.csv",row.names = FALSE)

}

# now read stats from college players and match to drafted players
# query college players who played at least 15 games and 15 min/game last season
# First 100 sorted desc by PER: 
# http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per
# subsequent players in batches of 100:
# http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=100
collegePlayers <- data.frame()
lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1

url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",lastDraft,"&year_max=",lastDraft,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=mp_per_g&c2comp=gt&c2val=15&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per")

thisCollege <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="stats"]') %>%
  html_table(fill = TRUE)
thisCollege <- thisCollege[[1]]
names(thisCollege) <- thisCollege[1,]
thisCollege <- thisCollege[-1,]
collegePlayers <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]

for (page in 1:19){ # read a total of 2000 college players
  url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",lastDraft,"&year_max=",lastDraft,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=15&c2stat=mp_per_g&c2comp=gt&c2val=15&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=",page*100)
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

# Merge drafted players with college players
rookies <- read.csv("data/rookies.csv", stringsAsFactors = FALSE)
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




