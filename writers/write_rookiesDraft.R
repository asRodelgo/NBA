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
  # rookies[grepl("Chris",rookies$Player),]$Player <- "Marquese Chriss"
  # rookies[grepl("Dami",rookies$Player),]$Player <- "Damian Jones"
  # rookies[grepl("Zimmerm",rookies$Player),]$Player <- "Stephen Zimmerman Jr."
  
  write.csv(rookies, "data/rookies.csv",row.names = FALSE)

}

# write all rookies (all draft rounds and non drafted)
writeAllRookies <- function(){
  
  rookies <- data.frame()
  newSeason <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 2
  teamList <- unique(filter(playersHist,Season == paste0(newSeason-2,"-",newSeason-1))$Tm)
  teamList <- teamList[which(!(teamList == "TOT"))]
  for (team in teamList) {
    
    url <- paste0("http://www.basketball-reference.com/teams/",team,"/",newSeason,".html")
    #https://www.basketball-reference.com/teams/NYK/2018.html
    thisSeasonRookies <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="roster"]') %>%
      html_table(fill = TRUE)
    thisSeasonRookies <- thisSeasonRookies[[1]]
    thisSeasonRookies <- thisSeasonRookies[,names(thisSeasonRookies)[which(nchar(names(thisSeasonRookies))>0)]]
    thisRookies <- filter(thisSeasonRookies, Exp == "R")
    thisRookies <- dplyr::select(thisRookies, Player, College) %>%
      mutate(Tm = team)
    
    if (nrow(rookies)>0) {
      rookies <- rbind(rookies, thisRookies)
    } else {
      rookies <- thisRookies
    }
  }
  
  write.csv(rookies, "data/rookies.csv",row.names = FALSE)
  
}


write_CollegePlayers <- function(col_G,col_MP,num_pages,from,to){
  # Read stats from college players and match to drafted players
  # query college players who played at least col_G games and col_MP min/game last season
  from <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) - 1
  to <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  collegePlayers <- data.frame()
  
  #for (lastDraft in from:to){
    col_G <- 15
    col_MP <- 7
    num_pages <- 90
    # First 100 sorted desc by PER: 
    # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per
    # subsequent players in batches of 100:
    # http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=25&c2stat=mp_per_g&c2comp=gt&c2val=20&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=100
    #collegePlayers <- data.frame()
    #lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
    
    url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                  from,"&year_max=",to,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                  col_G,"&c2stat=mp_per_g&c2comp=gt&c2val=",col_MP,"&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per")
    
    thisCollege <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="stats"]') %>%
      html_table(fill = TRUE)
    thisCollege <- thisCollege[[1]]
    names(thisCollege) <- thisCollege[1,]
    thisCollege <- thisCollege[-1,]
    collegePlayers <- thisCollege[which(!(thisCollege$Rk=="" | thisCollege$Rk=="Rk")),]
    
    for (page in 1:(num_pages-1)){ # read a total of num_pages*100 college players
      print(paste0("processing year: ",lastDraft, " page: ",page))
      url <- paste0("http://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=",
                    from,"&year_max=",to,"&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_gf=Y&pos_is_fg=Y&pos_is_f=Y&pos_is_fc=Y&pos_is_cf=Y&pos_is_c=Y&games_type=A&qual=&c1stat=g&c1comp=gt&c1val=",
                    col_G,"&c2stat=mp_per_g&c2comp=gt&c2val=",col_MP,"&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset=",
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
  
  write.csv(collegePlayers, "data/collegePlayers.csv", row.names = FALSE)
}

# Merge drafted players with college players
write_RookieStats <- function(){
  
  rookies <- read.csv("data/rookies.csv", stringsAsFactors = FALSE) # from writeAllRookies
  collegePlayers <- read.csv("data/collegePlayers.csv", stringsAsFactors = FALSE) %>% # from write_CollegePlayers
    group_by(Player) %>%
    summarise_if(is.numeric, mean)
  # Correct spelling errors 2017 draft
  collegePlayers[grepl("Nazareth Mitrou-Long",collegePlayers$Player),]$Player <- "Naz Mitrou-Long"
  collegePlayers[grepl("Royce O'Neale",collegePlayers$Player),]$Player <- "Royce O'Neal"
  collegePlayers[grepl("Jacorey Williams",collegePlayers$Player),]$Player <- "JaCorey Williams"
  collegePlayers[grepl("Andrew White III",collegePlayers$Player),]$Player <- "Andrew White"
  collegePlayers[grepl("TJ Leaf",collegePlayers$Player),]$Player <- "T.J. Leaf"
  collegePlayers[grepl("Frank Mason",collegePlayers$Player),]$Player <- "Frank Mason III"
  collegePlayers[grepl("Akim Mitchell",collegePlayers$Player),]$Player <- "Akil Mitchell"
  
  
  #collegePlayers[grepl("Dennis Smith",collegePlayers$Player),]$Player <- "Dennis Smith Jr."
  #ollegePlayers[grepl("Leaf",collegePlayers$Player),]$Player <- "TJ Leaf"
  
  rookieStats <- merge(rookies, collegePlayers, by = "Player",all.x=TRUE) %>% 
    group_by(Player) %>% summarise_if(is.numeric,funs(mean(.,na.rm=TRUE))) %>% 
    left_join(rookies, c("Player"="Player"))
  
  lastDraft <- as.numeric(substr(max(as.character(playersHist$Season)),1,4)) + 1
  
  rookieReady <- filter(rookieStats, !is.nan(G)) %>% select(one_of(names(playersHist)),College) %>%
    mutate(Season = lastDraft)
  rookieLeftout <- filter(rookieStats, is.nan(G)) %>% select(Player,College,Tm)
  
  # Find stats from european players drafted
  europePlayers <- data.frame()
  
  require(httr)
  for (i in 1:nrow(rookieLeftout)){
    
    if (rookieLeftout$College[i]==""){
      thisPlayer <- as.character(rookieLeftout$Player[i])
      name_edited <- tolower(thisPlayer)
      name_edited <- gsub(" ","-",name_edited)
      url <- paste0("https://www.basketball-reference.com/euro/players/",name_edited,"-1.html")
      
      if (status_code(GET(url))==200){ #european player
        thisEurope <- url %>%
          read_html() %>%
          html_nodes(xpath='//*[@id="per_gameALL0"]') %>%
          html_table(fill = TRUE)
        if (length(thisEurope)>0){
          rookieLeftout$College[i] <- "Europe"
          thisEurope <- thisEurope[[1]]
          print(paste0("Processing: ",thisPlayer))
          names(thisEurope)[4] <- "Country"
          thisEurope <- thisEurope %>%
            filter(G == max(G)) %>%
            mutate(Player = thisPlayer) %>%
            head(1)
          thisEurope$Tm <- rookieLeftout$Tm[i]
          thisEurope$College <- rookieLeftout$College[i]
          europePlayers <- rbind(europePlayers,thisEurope)
        } else { # international or european without stats
          rookieLeftout$College[i] <- "International"
        }
      } else { # college player that didn't find a match in collegePlayers. Find the reason!
        rookieLeftout$College[i] <- "International"
      }
    }
  }
  
  rookieLeftout <- filter(rookieLeftout, !(College == "Europe")) %>% select(Player,College,Tm)
  rookieLeftout$Season <- lastDraft
  
  # For international players or non-matched college players use averages of their respective groups for their stats
  averageCollegeRookie <- rookieReady %>%
    summarise_if(is.numeric, function(x) mean(x,na.rm = TRUE)) %>%
    select(-Season)
  
  names(europePlayers) <- gsub("%",".",names(europePlayers), fixed = TRUE)
  names(europePlayers) <- gsub("2","X2",names(europePlayers),fixed = TRUE)
  names(europePlayers) <- gsub("3","X3",names(europePlayers),fixed = TRUE)
  europePlayers <- select(europePlayers, Player, everything(), 
                          -c(`League(s)`,Country,FG.,X3P.,X2P.,FT.))
  
  averageEuropeRookie <- europePlayers %>%
    summarise_if(is.numeric,function(x) mean(x,na.rm = TRUE))
  
  rookieLeftoutStats <- data.frame()
  for (i in 1:nrow(rookieLeftout)) {
    if (rookieLeftout$College[i] == "International") {
      thisRookie <- cbind(rookieLeftout[i,],averageEuropeRookie)
    } else {
      thisRookie <- cbind(rookieLeftout[i,],averageCollegeRookie)
    }
    rookieLeftoutStats <- bind_rows(rookieLeftoutStats,thisRookie)
  }
  
  rookieReady <- select(rookieReady, -Season)
  rookieLeftoutStats <- select(rookieLeftoutStats, -Season)
  europePlayers <- select(europePlayers, -Season)
  rookieStatsFinal <- bind_rows(rookieReady,rookieLeftoutStats,europePlayers) %>%
    mutate(FG. = ifelse(FGA == 0,0,FG/FGA), X3P. = ifelse(X3PA == 0,0,X3P/X3PA), 
           X2P. = ifelse(X2PA == 0,0,X2P/X2PA), FT. = ifelse(FTA == 0,0,FT/FTA), 
           Season = paste0(lastDraft,"-",lastDraft+1))
    
  write.csv(rookieStatsFinal, "data/rookieStats.csv", row.names = FALSE)
  write.csv(europePlayers, "data/europePlayers.csv", row.names = FALSE)
  
}

# Once per game stats have been compiled for all rookies (college and international) compute their
# per-minute stats
write_Rookies_efficientStats <- function() {
  
  rookieStats <- read.csv("data/rookieStats.csv", stringsAsFactors = FALSE)
  # In college and Europe they play 40-min game. I will calculate their pre-minute stats
  # based on a 48-min game as the "price" for being a rookie in the NBA
  rookieEffStats <- rookieStats %>%
    group_by(Player) %>%
    mutate(effMin = MP/3936, effFG = FG/(3936*effMin),
           effFGA = FGA/(3936*effMin),eff3PM = X3P/(3936*effMin),eff3PA = X3PA/(3936*effMin),
           eff2PM = X2P/(3936*effMin),eff2PA = X2PA/(3936*effMin),
           effFTM = FT/(3936*effMin),effFTA = FTA/(3936*effMin),
           effORB = ORB/(3936*effMin),effDRB = DRB/(3936*effMin),
           effTRB = TRB/(3936*effMin),effAST = AST/(3936*effMin),
           effSTL = STL/(3936*effMin),effBLK = BLK/(3936*effMin),
           effTOV = TOV/(3936*effMin),effPF = PF/(3936*effMin),
           effPTS = PTS/(3936*effMin)) %>%
    dplyr::select(Player,Season,Tm,FGPer = FG.,FG3Per = X3P., FG2Per = X2P., effFGPer = eFG.,
                  FTPer = FT., starts_with("eff"),
                  -G,-MP,FG,-FGA,-X3P,-X3PA,-X2P,-X2PA,-FG,-FTA,-ORB,-DRB,-TRB,-AST,
                  -BLK,-TOV,-PF,-FT,-STL,-PTS)
  
  # Impute NAs by 0. If NA means no shot attempted, ie, 
  # either the player didn't play enough time or is really bad at this particular type of shot.
  for (i in 5:ncol(rookieEffStats)){
    rookieEffStats[is.na(rookieEffStats[,i]),i] <- 0
  }
  
  rookieEffStats <- as.data.frame(rookieEffStats)
  
  write.csv(rookieEffStats, "data/rookieEfficientStats.csv", row.names = FALSE)
}



