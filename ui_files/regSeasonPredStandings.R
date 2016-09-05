# regular season predicted standings -----------------------------------------------------
fluidPage(
      column(6,
        h4("Western Conference", style="color:#3399ff"),   
        DT::dataTableOutput('westPredStandings'),br()
      ),
      column(6,
        h4("Eastern Conference", style="color:#3399ff"),   
        DT::dataTableOutput('eastPredStandings'),br()
      )
)
