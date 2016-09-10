# Print a playoff bracket ---------------------

.playoffBracket <- function(){
  
  # Compute playoffs results -----------------
  standings <- regSeasonOutcome[[1]]
  
  pEast <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "E"), desc(win/(win+lose))),8)$teamCode)
  pWest <- as.character(head(arrange(filter(dplyr::select(standings[[tail(datesRange,1)]], teamCode,conference,win,lose), conference == "W"), desc(win/(win+lose))),8)$teamCode)
  
  playoffs <- .computePlayoffs(pEast,pWest)
  
  # Print bracket -----------------
  x<-seq(0,220,(221/67))
  y<-0:66
  
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n", 
       axes=F, col="white")
  
  # left hand side bracket
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  # central part
  segments(100,c(27,37),120,c(27,37))
  # right hand side bracket
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  # team names ----------------------
  # West quarterfinals
  text(49.8,61.5,filter(playoffs,conference == "W",round==8)$teamCode[1],cex=.4)
  text(49.8,53.5,filter(playoffs,conference == "W",round==8)$teamCode[8],cex=.4)
  text(49.8,45.5,filter(playoffs,conference == "W",round==8)$teamCode[4],cex=.4)
  text(49.8,37.5,filter(playoffs,conference == "W",round==8)$teamCode[5],cex=.4)
  
  text(49.8,27.5,filter(playoffs,conference == "W",round==8)$teamCode[2],cex=.4)
  text(49.8,19.5,filter(playoffs,conference == "W",round==8)$teamCode[7],cex=.4)
  text(49.8,11.5,filter(playoffs,conference == "W",round==8)$teamCode[3],cex=.4)
  text(49.8,3.5,filter(playoffs,conference == "W",round==8)$teamCode[6],cex=.4)
  
  # West semifinals
  text(69.8,57.5,filter(playoffs,conference == "W",round==4)$teamCode[1],cex=.4)
  text(69.8,41.5,filter(playoffs,conference == "W",round==4)$teamCode[4],cex=.4)
  
  text(69.8,23.5,filter(playoffs,conference == "W",round==4)$teamCode[2],cex=.4)
  text(69.8,7.5,filter(playoffs,conference == "W",round==4)$teamCode[3],cex=.4)
  
  # East quarterfinals
  text(169.8,61.5,filter(playoffs,conference == "E",round==8)$teamCode[1],cex=.4)
  text(169.8,53.5,filter(playoffs,conference == "E",round==8)$teamCode[8],cex=.4)
  text(169.8,45.5,filter(playoffs,conference == "E",round==8)$teamCode[4],cex=.4)
  text(169.8,37.5,filter(playoffs,conference == "E",round==8)$teamCode[5],cex=.4)
  
  text(169.8,27.5,filter(playoffs,conference == "E",round==8)$teamCode[2],cex=.4)
  text(169.8,19.5,filter(playoffs,conference == "E",round==8)$teamCode[7],cex=.4)
  text(169.8,11.5,filter(playoffs,conference == "E",round==8)$teamCode[3],cex=.4)
  text(169.8,3.5,filter(playoffs,conference == "E",round==8)$teamCode[5],cex=.4)
  
  # East semifinals
  text(149.8,57.5,filter(playoffs,conference == "E",round==4)$teamCode[1],cex=.4)
  text(149.8,41.5,filter(playoffs,conference == "E",round==4)$teamCode[4],cex=.4)
  
  text(149.8,23.5,filter(playoffs,conference == "E",round==4)$teamCode[2],cex=.4)
  text(149.8,7.5,filter(playoffs,conference == "E",round==4)$teamCode[3],cex=.4)
  
  # Conference Finals
  text(89.8,49.5,filter(playoffs,conference == "W",round==2)$teamCode[1],cex=.4)
  text(129.8,49.5,filter(playoffs,conference == "E",round==2)$teamCode[1],cex=.4)
  text(89.8,15.5,filter(playoffs,conference == "W",round==2)$teamCode[2],cex=.4)
  text(129.8,15.5,filter(playoffs,conference == "E",round==2)$teamCode[2],cex=.4)
  
  # Finals
  text(109.8,37.5,filter(playoffs,conference == "W",round==1)$teamCode[1],cex=.4)
  text(109.8,27.5,filter(playoffs,conference == "E",round==1)$teamCode[1],cex=.4)
  
  # Champion
  text(109.8,32.5,filter(playoffs,round==0)$teamCode[1],cex=2.5)
}