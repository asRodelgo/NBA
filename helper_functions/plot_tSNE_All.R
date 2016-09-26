# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  # tsne_points contains pairs of coordinate points to plot
  # Parameters -----------
  # 
  #colSeason <- "2015-2016"
  #colPlayer <- "All"
  #colTeam <- "All"
  #colAge <- "All"
  #colSkill <- "All"
  # ----------------------
  #
  # ------------------------------------
  if (colPlayer=="All") colPlayer <- players_list
  if (colAge=="All") colAge <- ages_list
  if (colTeam=="All") colTeam <- teams_list
  if (colSeason=="All") colSeason <- seasons_list
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
        scale_color_gradient2(midpoint=median(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="red", mid="white",high="blue")+
        #geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.01)) + 
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
        #geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.01)) + 
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

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  #
  if (colPlayer=="All") colPlayer <- players_list
  if (colAge=="All") colAge <- ages_list
  if (colTeam=="All") colTeam <- teams_list
  if (colSeason=="All") colSeason <- seasons_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    # General Filters
    tsne_points_filter <- tsne_ready %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
    tsne_points_filter_out <- tsne_ready %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
               & Tm %in% colTeam & Season %in% colSeason))
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  return(tsne_points_filter)
}

# Density plots
.densityPlots <- function(colTeam,colSeason,colPlayer,colAge,colSkill,clickPlayer,clickSeason){
  
  tsne_points_filter <- .tSNE_plot_filter(colTeam,colSeason,colPlayer,colAge,colSkill)
  tsne_points_filter <- gather(tsne_points_filter, skill, value, -Player,-Tm,-Age,-Season,-Pos,-x,-y)
  
  if (is.null(clickPlayer)){
    
    ggplot(tsne_points_filter,aes(value)) + 
      geom_density(aes(y=..density..),alpha=.4, fill="green") +  
      facet_wrap(~skill, nrow=1, scales="free_x") +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
  } else {
    
    verticalLine <- tsne_points_filter %>%
      filter(Player == clickPlayer, Season == clickSeason) %>%
      dplyr::select(skill, value)
    
    ggplot(tsne_points_filter,aes(value)) + 
      geom_density(aes(y=..density..),alpha=.4, fill="green") +  
      facet_wrap(~skill, nrow=1, scales="free_x") +
      geom_vline(data=verticalLine, aes(xintercept = value), colour="red", size = 1) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  }
    
}

.radarPlot <- function(colPlayer, colSeason){
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(-Age,-Pos,-Tm,-x,-y) %>%
    mutate_at(vars(starts_with("eff")), funs(min,max)) %>% # rescale to [0,1]
    filter(Season == colSeason) %>%
    #tibble::column_to_rownames("Player") %>%
    dplyr::select(-Season)
  
  tsne_min <- tsne_radar %>%
    dplyr::select(contains("_min")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(Player = "") %>%
    dplyr::select(Player, everything())
  
  names(tsne_min) <- gsub("_min","",names(tsne_min))
  
  tsne_max <- tsne_radar %>%
    dplyr::select(contains("_max")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(Player = "") %>%
    dplyr::select(Player, everything())
  
  names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- tsne_radar %>%
    dplyr::select(-contains("_m")) %>%
    filter(Player %in% colPlayer)
  tsne_radar <- bind_rows(tsne_radar,tsne_max,tsne_min)
  
  #ez.radarmap(df, "model", stats="mean", lwd=1, angle=0, fontsize=0.6, facet=T, facetfontsize=1, color=id, linetype=NULL)
  ez.radarmap(tsne_radar, "Player", stats="mean", lwd=1, angle=0, fontsize=1.5, facet=F, facetfontsize=1, color=id, linetype=NULL)
  
  
}