# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  # tsne_points contains pairs of coordinate points to plot
  library(scales)
  # Parameters -----------
  # 
  colSeason <- "2015-2016"
  colPlayer <- "All"
  colTeam <- "All"
  colAge <- "All"
  colSkill <- "effAST"
  # ----------------------
  #
  # Default selector choices -----------
  teams_list <- sort(unique(data_tsne_sample$Tm))
  ages_list <- sort(unique(data_tsne_sample$Age))
  seasons_list <- sort(unique(data_tsne_sample$Season))
  players_list <- sort(unique(data_tsne_sample$Player))
  skills_list <- names(data_tsne_sample)[6:ncol(data_tsne_sample)]
  # ------------------------------------
  if (colPlayer=="All") colPlayer <- players_list
  if (colAge=="All") colAge <- ages_list
  if (colTeam=="All") colTeam <- teams_list
  
  #
  tsne_ready <- cbind(data_tsne_sample,tsne_points)
  names(tsne_ready)[ncol(tsne_ready)-1] <- "x"
  names(tsne_ready)[ncol(tsne_ready)] <- "y"
  
  if (length(tsne_ready)>0){ # in case there are data do stuff
    
    tsne_ready <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "lightgrey")
    
    par(mar=c(0,0,0,0))
    
    
    # Skills filter
    if (!(colSkill=="All")){
      
      
      this_skill <- paste0("mutate(tsne_ready, colorDots = ifelse(",colSkill,"<(mean(",colSkill,")-1.5*sd(",colSkill,")),'red',
                                                        ifelse(",colSkill,"<(mean(",colSkill,")-sd(",colSkill,")),'orange',
                                                               ifelse(",colSkill,">(mean(",colSkill,")+1.5*sd(",colSkill,")),'blue',
                                                                      ifelse(",colSkill,">(mean(",colSkill,")+sd(",colSkill,")),'lightblue',
                                                                             alpha('lightgrey',0.1))))))")
      tsne_ready <- eval(parse(text=this_skill))
    }
    # Season filter: if Season selected, show dots for that season only
    if (!(colSeason == "All")){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Season==colSeason,colorDots,alpha("lightgrey",0)))
    }
    # General Filters
    tsne_points_filter <- tsne_ready %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam) %>%
      dplyr::select(Player,Season, Age, Tm, x,y,color,colorDots)
    
    # Manage the color column of tsne_ready or filters
    
#     if (!(colPlayer=="All") & (colSeason=="All") & (colAge=="All")){
#       tsne_ready <- mutate(tsne_ready, color = ifelse(Player==colPlayer,"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Player == colPlayer) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     } else if ((colPlayer=="All") & !(colSeason=="All") & (colAge=="All")){
#       tsne_ready <- mutate(tsne_ready, color = ifelse(Season==colSeason,"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Season == colSeason) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     } else if ((colPlayer=="All") & (colSeason=="All") & !(colAge=="All")){
#       tsne_ready <- mutate(tsne_ready, color = ifelse(Age==colAge,"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Age == colAge) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     } else if ((colPlayer=="All") & !(colSeason=="All") & !(colAge=="All")){
#       tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Season == colSeason),"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Age == colAge & Season == colSeason) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     } else if (!(colPlayer=="All") & (colSeason=="All") & !(colAge=="All")){  
#       tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Player == colPlayer),"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Age == colAge & Player == colPlayer) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     } else if (!(colPlayer=="All") & !(colSeason=="All") & !(colAge=="All")){
#       tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Player == colPlayer) & (Season == colSeason),"blue",color))
#       tsne_points_filter <- tsne_ready %>%
#         filter(Age == colAge & Player == colPlayer & Season == colSeason) %>%
#         dplyr::select(Player,Season, Age,x,y,color,colorDots)
#     }  
    # Colors for skills
#     if (colSkill=="Blocks"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effBLK<(mean(effBLK)-1.5*sd(effBLK)),"red",
#                                                           ifelse(effBLK<(mean(effBLK)-sd(effBLK)),"orange",
#                                                                  ifelse(effBLK>(mean(effBLK)+1.5*sd(effBLK)),"blue",
#                                                                         ifelse(effBLK>(mean(effBLK)+sd(effBLK)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="Free Throws"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effFTM<(mean(effFTM)-1.5*sd(effFTM)),"red",
#                                                           ifelse(effFTM<(mean(effFTM)-sd(effFTM)),"orange",
#                                                                  ifelse(effFTM>(mean(effFTM)+1.5*sd(effFTM)),"blue",
#                                                                         ifelse(effFTM>(mean(effFTM)+sd(effFTM)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="Rebounds"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effTRB<(mean(effTRB)-1.5*sd(effTRB)),"red",
#                                                           ifelse(effTRB<(mean(effTRB)-sd(effTRB)),"orange",
#                                                                  ifelse(effTRB>(mean(effTRB)+1.5*sd(effTRB)),"blue",
#                                                                         ifelse(effTRB>(mean(effTRB)+sd(effTRB)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="Assists"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effAST<(mean(effAST)-1.5*sd(effAST)),"red",
#                                                           ifelse(effAST<(mean(effAST)-sd(effAST)),"orange",
#                                                                  ifelse(effAST>(mean(effAST)+1.5*sd(effAST)),"blue",
#                                                                         ifelse(effAST>(mean(effAST)+sd(effAST)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="Points"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effPTS<(mean(effPTS)-1.5*sd(effPTS)),"red",
#                                                           ifelse(effPTS<(mean(effPTS)-sd(effPTS)),"orange",
#                                                                  ifelse(effPTS>(mean(effPTS)+1.5*sd(effPTS)),"blue",
#                                                                         ifelse(effPTS>(mean(effPTS)+sd(effPTS)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="3 pointers"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(eff3PM<(mean(eff3PM)-1.5*sd(eff3PM)),"red",
#                                                           ifelse(eff3PM<(mean(eff3PM)-sd(eff3PM)),"orange",
#                                                                  ifelse(eff3PM>(mean(eff3PM)+1.5*sd(eff3PM)),"blue",
#                                                                         ifelse(eff3PM>(mean(eff3PM)+sd(eff3PM)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colSkill=="Minutes played"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effMin<(mean(effMin)-1.5*sd(effMin)),"red",
#                                                           ifelse(effMin<(mean(effMin)-sd(effMin)),"orange",
#                                                                  ifelse(effMin>(mean(effMin)+1.5*sd(effMin)),"blue",
#                                                                         ifelse(effMin>(mean(effMin)+sd(effMin)),"lightblue",
#                                                                                alpha("lightgrey",0.1))))))
#     }
#     if (colTeam == "GSW"){
#       tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Tm=="GSW","green","lightgrey"))
#     }
        
    
    plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
    graphics::text(tsne_points_filter[,c("x","y")],
                   labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
                   col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

