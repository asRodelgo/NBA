# pre-calculate tsne points for all players and seasons
write_tSNE_compute_All <- function(num_iter, max_num_neighbors){
  
  # data_tsne contains the input data for tSNE filtered and cleaned up
  # from: data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
  # calculate tsne-points Dimensionality reduction to 2-D
  
  library(doMC) # use parallel processing on this machine through "foreach"
  registerDoMC(2) # As far as I know my MAC works on 2 cores
  #data_tsne_sample <- dplyr::sample_n(data_tsne,1000)
  data_tsne_sample <- filter(data_tsne,Season > "1995-1996")
                             #%in% c("2012-2013","2013-2014","2014-2015","2015-2016"))
    #"2012-2013","2013-2014","2014-2015",
  
  if (nrow(data_tsne)>0){
    num_iter <- 400
    max_num_neighbors <- 50
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample[,-c(1:5)], 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    #plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_All.csv",row.names = FALSE)
  
}

write_MVPs <- function(){
  
  library(httr)
  library(rvest)
  
  mvps <- data.frame(Player=NULL, Season=NULL)
  for (thisSeason in 1980:as.numeric(thisYear)){
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,".html")
    
    thisPlayer <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="meta"]/div[2]/p[2]/a') %>%
      html_text()
    
    thisMVP <- data.frame(Player = thisPlayer, Season = paste0(thisSeason-1,"-",thisSeason))
    if (nrow(mvps) > 0) mvps <- rbind(mvps,thisMVP) else mvps <- thisMVP
  }
  
  write.csv(mvps, "data/mvps.csv", row.names = FALSE)
}

# compile league awards: mvp, def player, rookie of the year, etc.
write_Awards <- function(){ # Not working!
  
  library(httr)
  library(rvest)
  
  awards <- data.frame(Player=NULL, Season=NULL, award=NULL)
  for (thisSeason in 1980:as.numeric(thisYear)){
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                  thisSeason,".html")
    
    thisPlayer <- url %>%
      read_html() %>%
      html_nodes(xpath='//*[@id="div_all-nba"]') %>%
      #html_children() %>%
      #html_children() %>%
      html_nodes('table')
    
    thisAwards <- data.frame(Player = thisPlayer, Season = paste0(thisSeason-1,"-",thisSeason), award = )
    if (nrow(awards) > 0) awards <- rbind(awards,thisAwards) else awards <- thisAwards
  }
  
  write.csv(mvps, "data/mvps.csv", row.names = FALSE)
}

# put together tsne_ready to load at the start of dashboards
write_tsne_ready_historical <- function() {
  
  source("helper_functions/similarityFunctions.R")
  #.teamsPredictedPower() 
  tsne_points <- read.csv("data/tsne_points_All.csv",stringsAsFactors = FALSE)
  
  # load data
  data_tsne <- .tSNE_prepare_All() # for tSNE visualization from similarityFunctions.R
  data_tsne_sample <- filter(data_tsne,Season > "1995-1996")
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_hist.csv", row.names = FALSE)
  
}

# precalculate t-SNE for predicted stats (new season)
write_tSNE_newSeason <- function(num_iter, max_num_neighbors) {
  
  require(tsne)
  data_tsne_sample <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE) %>%
    select_if(is.numeric) %>% select(-Pick)
  
  if (nrow(data_tsne_sample)>0){
    num_iter <- 600
    max_num_neighbors <- 20
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_newSeason.csv",row.names = FALSE)
  
}

# put together tsne_ready predicted to load at start of dashboards
write_tSNE_ready_newSeason <- function(){
  
  source("helper_functions/similarityFunctions.R")
  tsne_points <- read.csv("data/tsne_points_newSeason.csv",stringsAsFactors = FALSE)
  
  # load data
  data_tsne_sample <- read.csv("data/playersNewPredicted_Final_adjMin.csv", stringsAsFactors = FALSE) %>%
    select_if(is.character)
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_newSeason.csv", row.names = FALSE)
}

write_tSNE_teams <- function(){
  
  require(tsne)
  playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
  teamStats <- .computeTeamStats(data = playersPredictedStats_adjPer)
  data_tsne_sample <- teamStats %>% 
    select_if(is.numeric) %>%
    mutate_all(function(x) (x-min(x))/(max(x)-min(x)))
  
  if (nrow(data_tsne_sample)>0){
    num_iter <- 1500
    max_num_neighbors <- 2
    set.seed(456) # reproducitility
    tsne_points <- tsne(data_tsne_sample, 
                        max_iter=as.numeric(num_iter), 
                        perplexity=as.numeric(max_num_neighbors), 
                        epoch=100)
    plot(tsne_points)
  } else {
    tsne_points <- c()
  }
  write.csv(tsne_points, "data/tsne_points_teams.csv",row.names = FALSE)
}

# put together tsne_ready predicted to load at start of dashboards
write_tSNE_ready_teams <- function(){
  
  source("helper_functions/similarityFunctions.R")
  tsne_points <- read.csv("data/tsne_points_teams.csv",stringsAsFactors = FALSE)
  
  # load data
  playersPredictedStats_adjPer <- read.csv("data/playersNewPredicted_Final_adjPer.csv", stringsAsFactors = FALSE)
  data_tsne_sample <- .computeTeamStats(data = playersPredictedStats_adjPer) %>%
    select_if(is.character)
  # tsne_points are pre-calculated from write_tSNE_All.R and saved in data/ directory
  # using this function: tsne_points <- write_tSNE_compute_All()
  if (!nrow(data_tsne_sample)==nrow(tsne_points)){ # in case labels and coordinates have different sizes
    tsne_ready <- tsne_points
  } else {
    tsne_ready <- cbind(data_tsne_sample,tsne_points)
  }
  
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  write.csv(tsne_ready, "data/tsne_ready_teams.csv", row.names = FALSE)
}