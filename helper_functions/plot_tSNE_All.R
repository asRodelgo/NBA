# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colTeam,colSeason,colPlayer,colAge,colSkill){
  # tsne_points contains pairs of coordinate points to plot
  # put tsne_points and data_tsne together:
  #tsne_ready <- cbind(data_tsne,tsne_points)
  tsne_ready <- cbind(data_tsne_sample,tsne_points)
  if (length(tsne_ready)>0){
    tsne_ready <- tsne_ready %>%
      mutate(color = "lightgrey", colorDots = "lightgrey", x = `1`, y = `2`) %>%
      dplyr::select(-`1`, -`2`)
    
    par(mar=c(0,0,0,0))
    # Manage the color column of tsne_ready
    if (!(colPlayer=="All") & (colSeason=="All") & (colAge=="All")){
      tsne_ready <- mutate(tsne_ready, color = ifelse(Player==colPlayer,"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Player == colPlayer) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    } else if ((colPlayer=="All") & !(colSeason=="All") & (colAge=="All")){
      tsne_ready <- mutate(tsne_ready, color = ifelse(Season==colSeason,"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Season == colSeason) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    } else if ((colPlayer=="All") & (colSeason=="All") & !(colAge=="All")){
      tsne_ready <- mutate(tsne_ready, color = ifelse(Age==colAge,"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Age == colAge) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    } else if ((colPlayer=="All") & !(colSeason=="All") & !(colAge=="All")){
      tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Season == colSeason),"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Age == colAge & Season == colSeason) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    } else if (!(colPlayer=="All") & (colSeason=="All") & !(colAge=="All")){  
      tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Player == colPlayer),"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Age == colAge & Player == colPlayer) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    } else if (!(colPlayer=="All") & !(colSeason=="All") & !(colAge=="All")){
      tsne_ready <- mutate(tsne_ready, color = ifelse((Age==colAge) & (Player == colPlayer) & (Season == colSeason),"blue",color))
      tsne_points_filter <- tsne_ready %>%
        filter(Age == colAge & Player == colPlayer & Season == colSeason) %>%
        dplyr::select(Player,Season, Age,x,y,color,colorDots)
    }  
    # Colors for skills
    if (colSkill=="Blocks"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effBLK<(mean(effBLK)-1.5*sd(effBLK)),"red",
                                                          ifelse(effBLK<(mean(effBLK)-sd(effBLK)),"orange",
                                                                 ifelse(effBLK>(mean(effBLK)+1.5*sd(effBLK)),"blue",
                                                                        ifelse(effBLK>(mean(effBLK)+sd(effBLK)),"lightblue",
                                                                               "lightgrey")))))
    }
    if (colSkill=="Rebounds"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effTRB<(mean(effTRB)-1.5*sd(effTRB)),"red",
                                                          ifelse(effTRB<(mean(effTRB)-sd(effTRB)),"orange",
                                                                 ifelse(effTRB>(mean(effTRB)+1.5*sd(effTRB)),"blue",
                                                                        ifelse(effTRB>(mean(effTRB)+sd(effTRB)),"lightblue",
                                                                               "lightgrey")))))
    }
    if (colSkill=="Assists"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effAST<(mean(effAST)-1.5*sd(effAST)),"red",
                                                          ifelse(effAST<(mean(effAST)-sd(effAST)),"orange",
                                                                 ifelse(effAST>(mean(effAST)+1.5*sd(effAST)),"blue",
                                                                        ifelse(effAST>(mean(effAST)+sd(effAST)),"lightblue",
                                                                               "lightgrey")))))
    }
    if (colSkill=="Points"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(effPTS<(mean(effPTS)-1.5*sd(effPTS)),"red",
                                                          ifelse(effPTS<(mean(effPTS)-sd(effPTS)),"orange",
                                                                 ifelse(effPTS>(mean(effPTS)+1.5*sd(effPTS)),"blue",
                                                                        ifelse(effPTS>(mean(effPTS)+sd(effPTS)),"lightblue",
                                                                               "lightgrey")))))
    }
    if (colSkill=="3 pointers"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(eff3PM<(mean(eff3PM)-1.5*sd(eff3PM)),"red",
                                                          ifelse(eff3PM<(mean(eff3PM)-sd(eff3PM)),"orange",
                                                                 ifelse(eff3PM>(mean(eff3PM)+1.5*sd(eff3PM)),"blue",
                                                                        ifelse(eff3PM>(mean(eff3PM)+sd(eff3PM)),"lightblue",
                                                                               "lightgrey")))))
    }
    if (colTeam == "GSW"){
      tsne_ready <- mutate(tsne_ready, colorDots = ifelse(Tm=="GSW","green","lightgrey"))
    }
    plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
#     graphics::text(tsne_points_filter[,c("x","y")],
#                    labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
#                    col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

