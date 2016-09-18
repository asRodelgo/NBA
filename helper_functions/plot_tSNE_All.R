# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  # tsne_points contains pairs of coordinate points to plot
  library(scales)
  library(RColorBrewer)
  # Parameters -----------
  # 
  #colSeason <- "2015-2016"
  #colPlayer <- "All"
  #colTeam <- "All"
  #colAge <- "All"
  #colSkill <- "All"
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
  if (colSeason=="All") colSeason <- seasons_list
  #if (colSkill=="All") colSkill <- skills_list
  
  #
  
  if (length(tsne_ready)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
   
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason))
    # Skills filter
    if (!(colSkill=="All")){
      
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colSkill)))) +
        scale_color_gradient2(midpoint=median(eval(parse(text=paste0("tsne_ready_filter$",colSkill)))), low="red", mid="white",high="blue")+
        geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.01)) + 
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

    } else {
      
        ggplot(NULL, aes(x,y)) +  
        geom_point(data=tsne_points_filter,color = "blue") +
        geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.01)) + 
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
    }
    
    #plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
#    graphics::text(tsne_points_filter[,c("x","y")],
#                   labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
#                   col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

