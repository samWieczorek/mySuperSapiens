dashboardPage(
  dashboardHeader(title = "mySuperSapiens"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Suivi glycemie", tabName = "rawdata"),
      menuItem("Stats", tabName = "stats"),
      menuItem("Alimentation", tabName = "alimentation"),
      menuItem("Sport", tabName = "sport")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("rawdata",
        # fluidRow(
        #   valueBoxOutput("rate"),
        #   valueBoxOutput("count"),
        #   valueBoxOutput("users")
        # ),
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Glycémie",
            checkboxInput('showzones', 'Show zones', value = FALSE),
            highchartOutput('showRawData', height = '800px')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Mean per day",
            highchartOutput("meanPerDay")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Variance per day",
            highchartOutput('variancePerDay')
          )
        )
      ),
      tabItem("stats",
        box(
          width = 6, status = "info", solidHeader = TRUE,
          title = "Time spent in glucose zones",
          highchartOutput('timeInGlucoseZones')
        ),
        box(
          width = 6, status = "info", solidHeader = TRUE,
          title = "Heatmap per hour",
          highchartOutput('heatmapPerHour')
        )
        
        
        
        
        ),
       tabItem("alimentation",
         box(
         width = 12, status = "info", solidHeader = TRUE,
         title = "Glycémie",
         p('alimentation')
       )),
      tabItem("sport",
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Glycémie",
          p('sport')
        ))
    )
  )
)
