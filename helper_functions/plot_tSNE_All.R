# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(data = tsne_ready,colTeam,colSeason,colPlayer,colAge,colSkill,num_clusters = 10){
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
  # Check whether the user has selected any input
  color_details <- FALSE
  
  if (colPlayer=="All") colPlayer <- players_list else color_details <- TRUE
  if (colAge=="All") colAge <- ages_list else color_details <- TRUE
  if (colTeam=="All") colTeam <- teams_list else color_details <- TRUE
  if (colSeason=="All") colSeason <- seasons_list else color_details <- TRUE
  # Add clusters
  set.seed(456)
  playerCluster <- kmeans(data[, c("x","y")], num_clusters, nstart = 10, iter.max = 20)
  data <- cbind(data, cluster = playerCluster$cluster)
  
  #centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
  #x_limit <- c(min(tsne_points_filter$x)-5,max(tsne_points_filter$x)+10)
  #y_limit <- c(min(tsne_points_filter$y)-5,max(tsne_points_filter$y)+10)
  
  cluster_representative <- group_by(data, cluster) %>%
    mutate(x_mean = mean(x), y_mean=mean(y)) %>%
    mutate(dist = sqrt((x-x_mean)^2+(y-y_mean)^2)) %>%
    filter(dist==min(dist)) %>%
    select(cluster, Player,Season, x,y,dist) %>%
    ungroup()
  
  if (length(data)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- data %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
   
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
      
    centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason))
    # Skills filter
      
    #if (color_details) {
      if (colSkill == "K-means" & color_details) { # color by K-means clusters
        ggplot(NULL, aes(x,y)) +
          geom_point(data=data,aes(color = as.factor(cluster)),alpha = 0.2) +
          geom_jitter(data=tsne_points_filter,color = "red",size=4,width = 1.5, height = 1.5) +
          geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
          geom_text(data=tsne_points_filter,aes(label = Player),color="blue",size=3, nudge_y = 1) +
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
      } else if (colSkill == "K-means"){
        ggplot(NULL, aes(x,y)) +
          geom_point(data=data,aes(color = as.factor(cluster)),alpha = 0.2) +
          geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
          #scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
          #geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
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
          geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colSkill))),size=2) +
          scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
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
    # } else {
    #   ggplot(NULL, aes(x,y)) +
    #     geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colSkill))),size=2) +
    #     scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colSkill)))), low="blue", mid="white",high="red")+
    #     geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
    #     theme(legend.key=element_blank(),
    #           legend.title=element_blank(),
    #           legend.text = element_blank(),
    #           panel.border = element_blank(),
    #           panel.background = element_blank(),
    #           axis.text.x = element_blank(),
    #           axis.text.y = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.title.y = element_blank(),
    #           axis.ticks = element_blank())
    # }
    # } else {
    #   
    #     # starPlayers <- c("LeBron James","Russell Westbrook","Kawhi Leonard","Stephen Curry",
    #     #                  "Draymond Green","Kevin Durant","Anthony Davis",
    #     #                  "James Harden","Klay Thompson","DeMarcus Cousins",
    #     #                  "Chris Paul","John Wall","Paul George",
    #     #                  "Marc Gasol","JaVale McGee","Andre Drummond",
    #     #                  "Kelly Oubre","Ryan Anderson","Kelly Olynyk","Channing Frye",
    #     #                  "Brandon Jennings","Tony Allen","Tristan Thompson","Ian Mahinmi",
    #     #                  "Dennis Schroder","Justin Harper","Luis Scola")
    #     #labelsPlayers <- filter(tsne_ready_plot, Player %in% starPlayers, Season == "2016-2017")
    #     ggplot(NULL, aes(x,y)) +  
    #     geom_point(data=tsne_points_filter,color = "blue",size=4) +
    #     geom_text(data=tsne_points_filter,aes(label = paste0(Player," (",Season,")"))) +
    #     geom_text(data = cluster_representative, aes(label = paste0(Player," (",Season,")")),color = "grey",size=3, nudge_y = 1) +
    #     #geom_text(data=labelsPlayers,aes(label = Player,group = Player,color = Player)) +  
    #     geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
    #     #geom_point(data=centroid,color="red",size=3) + 
    #     theme(legend.key=element_blank(),
    #           legend.title=element_blank(),
    #           legend.text = element_blank(),
    #           legend.position = "top",
    #           panel.border = element_blank(),
    #           panel.background = element_blank(),
    #           axis.text.x = element_blank(),
    #           axis.text.y = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.title.y = element_blank(),
    #           axis.ticks = element_blank())
    # }
    
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
.tSNE_plot_filter <- function(data = tsne_ready, colTeam,colSeason,colPlayer,colAge,colSkill){
  #
  if (colPlayer=="All") colPlayer <- players_list
  if (colAge=="All") colAge <- ages_list
  if (colTeam=="All") colTeam <- teams_list
  if (colSeason=="All") colSeason <- seasons_list
  
  if (length(data)>0){ # if data do stuff
    # General Filters
    tsne_points_filter <- data %>%
      filter(Player %in% colPlayer & Age %in% colAge
             & Tm %in% colTeam & Season %in% colSeason)
    tsne_points_filter_out <- data %>%
      filter(!(Player %in% colPlayer & Age %in% colAge
               & Tm %in% colTeam & Season %in% colSeason))
    
  } else{ return()}
    #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

# Density plots
.densityPlots <- function(colTeam,colSeason,colPlayer,colAge,colSkill,clickPlayer,clickSeason,horiz=TRUE){
  
  tsne_points_filter <- .tSNE_plot_filter(colTeam,colSeason,colPlayer,colAge,colSkill)
  tsne_points_filter <- gather(tsne_points_filter, skill, value, -Player,-Tm,-Age,-Season,-Pos,-x,-y)
  tsne_ready_gather <- gather(tsne_ready, skill, value, -Player,-Tm,-Age,-Season,-Pos,-x,-y)
  
  if (!horiz){
    
    if (is.null(clickPlayer)){
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="blue") +  
        facet_wrap(~skill, ncol=1, scales="free_x") +
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
      
      ggplot(data=tsne_ready_gather,aes(value)) + 
        geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
        geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.8, fill="blue") +  
        facet_wrap(~skill, ncol=1, scales="free_x") +
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
    
    } else {
      if (is.null(clickPlayer)){
        
        ggplot(data=tsne_ready_gather,aes(value)) + 
          geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
          geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="blue") +  
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
        
        ggplot(data=tsne_ready_gather,aes(value)) + 
          geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey",colour="grey",size=1) +  
          geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.8, fill="blue") +  
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
}

.radarPlot <- function(brushPoints){#,Off_Deff="All"){
  
#   if (Off_Deff == "Offense"){
#     list_skills <- c("effPTS","eff2PM","eff2PA","eff3PM","eff3PA","effFTA","effFTM","effAST")
#   } else if (Off_Deff == "Defense"){
#     list_skills <- c("effBLK","effDRB","effORB","effSTL","effTOV","effPF")
#   } else {
     list_skills <- c("effPTS","eff2PM","eff2PA","eff3PM","eff3PA","effFTA","effFTM","effAST",
                      "effSTL","effBLK","effDRB","effORB","effTOV","effPF","effMin")
#     }
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(-Age,-Pos,-Tm,-x,-y) %>%
    mutate_at(vars(starts_with("eff")), funs(max,mean)) %>%
    #filter(Season == colSeason) %>%
    dplyr::select(-Season)
  
  #brushPoints <- filter(tsne_ready, Tm == "CHI")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(-Age,-Pos,-Tm,-x,-y,-Season) %>%
      mutate_at(vars(starts_with("eff")), funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(Player = "mean of selected") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(Player, everything())
      
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    tsne_mean <- tsne_radar %>%
      dplyr::select(ends_with("_mean")) %>%
      distinct(.keep_all=TRUE) %>%
      mutate(Player = "mean of selected") %>%
      dplyr::select(Player, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
  }
  
  tsne_max <- tsne_radar %>%
    dplyr::select(ends_with("_max")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(Player = "max") %>%
    dplyr::select(Player, everything())
  
  names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, Player, one_of(list_skills))
  #ez.radarmap(df, "model", stats="mean", lwd=1, angle=0, fontsize=0.6, facet=T, facetfontsize=1, color=id, linetype=NULL)
  ez.radarmap(tsne_radar, "Player", stats="none", lwd=1, angle=0, fontsize=1.5, facet=F, facetfontsize=1, color=id, linetype=NULL) +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom",
          #panel.border = element_blank(),
          #panel.background = element_blank(),
          plot.title = element_text(lineheight=.5),
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          #axis.ticks = element_blank()
          )  
  
}

.brushTable <- function(brushPoints){
  
  brushPoints <- as.data.frame(brushPoints)
  #return(str(brushPoints))
}

.compare10_click <- function(colSeason,colPlayer,data=tsne_ready){
  
  if (colPlayer=="All" || is.null(colPlayer)) colPlayer <- players_list
  if (colSeason=="All" || is.null(colSeason)) colSeason <- seasons_list
  
  if (length(data)>0){ # if data do stuff
    
    # coordenates x,y of clicked point
    distCouPerX <- filter(data,Player %in% colPlayer, Season %in% colSeason)$x
    distCouPerY <- filter(data,Player %in% colPlayer, Season %in% colSeason)$y
    
    tsne_points_filter <- data %>%
      dplyr::select(Season,Player,x,y) %>%
      mutate(dist = sqrt((x-distCouPerX)^2+(y-distCouPerY)^2)) %>%
      arrange(dist) %>%
      dplyr::select(-x,-y)
    
  } else{ return()}
  
  return(tsne_points_filter)
}

