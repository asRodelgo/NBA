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
  colSkill <- "effPTS"
  # ----------------------
  #
  # Default selector choices -----------
  teams_list <- sort(unique(data_tsne_sample$Tm))
  ages_list <- sort(unique(data_tsne_sample$Age))
  seasons_list <- sort(unique(data_tsne_sample$Season))
  players_list <- sort(unique(data_tsne_sample$Player))
  skills_list <- names(data_tsne_sample)[6:ncol(data_tsne_sample)]
  # ------------------------------------
  #if (colPlayer=="All") colPlayer <- players_list
  #if (colAge=="All") colAge <- ages_list
  #if (colTeam=="All") colTeam <- teams_list
  
  #
  
  if (length(tsne_ready)>0){ # in case there are data do stuff
    
    tsne_ready <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
    
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
    
    # Dots filters
    if (!(colSeason == "All")){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Season==colSeason,colorDots,alpha("grey",0)))
                                                         #
    }
    if (!(colTeam == "All")){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Tm==colTeam,colorDots,alpha("lightgrey",0)))
    }
    if (!(colAge == "All")){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Age==colAge,colorDots,alpha("lightgrey",0)))
    }
    if (!(colPlayer == "All")){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Player==colPlayer,colorDots,alpha("lightgrey",0)))
    }
    
    # Text Filters
 #   tsne_points_filter <- tsne_ready %>%
#      filter(Player %in% colPlayer & Age %in% colAge
#             & Tm %in% colTeam) %>%
#      dplyr::select(Player,Season, Age, Tm, x,y,color,colorDots)
    
    # plot 
    
    ggplot(tsne_ready, aes(x,y,color = eval(parse(text=colSkill)))) +
      geom_point() +
      #scale_colour_manual(breaks = unique(tsne_ready$colorDots),values = c("lightgrey","blue"))+
      scale_color_gradient2(midpoint=median(eval(parse(text=paste0("tsne_ready$",colSkill)))), low="red", mid="white",
                            high="blue")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.text = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank())
    #plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
#    graphics::text(tsne_points_filter[,c("x","y")],
#                   labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
#                   col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

