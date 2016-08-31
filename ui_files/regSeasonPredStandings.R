# regular season predicted standings -----------------------------------------------------
fluidPage(
  column(12, h4("Regular Season Standings"),
         h5("Predicted regular season standings"),
         h6(
           a("How this works: ", 
             "http://http://www.basketball-reference.com")),
         #h6("Download: ",downloadLink("dataMacro","data",class = "plot-download")),
         tags$style(HTML("
                         .jqstooltip{
                         box-sizing: content-box;
                         }")), # adjust tooltips in datatables
         column(6,
           h4("Western Conference", style="color:#3399ff"),   
           dataTableOutput('westPredStandings'),br()
         ),
         column(6,
           h4("Eastern Conference", style="color:#3399ff"),   
           dataTableOutput('eastPredStandings'),br()
         )
  )
)

