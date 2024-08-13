dashboardPage(
  dashboardHeader(title = "mySuperSapiens"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Suivi glycemie", tabName = "rawdata"),
      menuItem("Glucose rushes", tabName = "rushes"),
      menuItem("Stats", tabName = "stats"),
      menuItem("Alimentation", tabName = "alimentation"),
      menuItem("Analyze FIT records", tabName = "sport"),
      menuItem("Settings", tabName = "Configure")
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem("rawdata",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Glycémie",
            checkboxInput('showzones', 'Show zones', value = FALSE),
            #uiOutput('chooseFitFile'),
            
            highchartOutput('showRawData', height = '800px')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Mean per day",
            highchartOutput("meanPerDay_UI")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Variance per day",
            p('Défini en pourcentage du coefficient de variation (%CV) ; Cible ≤ 36 %'),
            highchartOutput('variancePerDay_UI')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Mean per hour",
            highchartOutput("meanPerHour_UI")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Variance per hour",
            p('Défini en pourcentage du coefficient de variation (%CV) ; Cible ≤ 36 %'),
            highchartOutput('variancePerHour_UI')
          )
        )
      ),
      tabItem("rushes",
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Glucose rushes",
            highchartOutput('wholeRushes', height = '800px')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Mean per day",
            highchartOutput("meanPerDay")
          ),
          box(
            width = 6, status = "info", solidHeader = TRUE,
            title = "Rushes over day",
            p('Défini en pourcentage du coefficient de variation (%CV) ; Cible ≤ 36 %'),
            highchartOutput('rushesOverDay')
          ),
          box(
            width = 6, status = "info",solidHeader = TRUE,
            title = "Rushes over hour",
            highchartOutput("rushesOverHour")
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
          title = "Glycémie with FIT record",
          uiOutput('chooseFitFile'),
          highchartOutput('showFit', height = '800px')
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
