dashboardPage(
  dashboardHeader(title = "mySuperSapiens"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Suivi glycemie", tabName = "rawdata"),
      menuItem("Stats", tabName = "stats"),
      menuItem("Alimentation", tabName = "alimentation"),
      menuItem("Sport", tabName = "sport"),
      menuItem("Settings", tabName = "Configure")
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
            p('Défini en pourcentage du coefficient de variation (%CV) ; Cible ≤ 36 %'),
            highchartOutput('variancePerDay')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Mean per hour",
            highchartOutput("meanPerHour")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Variance per hour",
            p('Défini en pourcentage du coefficient de variation (%CV) ; Cible ≤ 36 %'),
            highchartOutput('variancePerHour')
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
        ),
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Profil de Glucose Ambulatoire (PGA)",
          highchartOutput('pga')
        ),
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Whole rushes",
          highchartOutput('wholerushes')
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
        )),
      tabItem("Configure",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Define zones",
            uiOutput('zones_UI')
          )
        )
      )
    )
  )
)
