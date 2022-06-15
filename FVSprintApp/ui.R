#e_theme_register("GHouseCustom.json", name = 'GHouseCustom')
e_common(theme = "chalk")

#### Header --------------------
header <- dashboardHeader( title = "Sprint Profiling App", titleWidth = 250)


#### Sidebar --------------------
sidebar <- dashboardSidebar( width = 250,
                             sidebarMenu(
                               menuItem(
                                 text = "Player Profile",
                                 tabName = "tabProfile",
                                 icon = icon("running")
                               ),
                               menuItem(
                                 text = "Perforamnce Tracking",
                                 tabName = "tabTracking",
                                 icon = icon("chart-line")
                               ),
                               menuItem(
                                 text = "Comparisons",
                                 tabName = "tabComparisons",
                                 icon = icon("people-arrows")
                               )
                             )
)


#### Body --------------------
body <- dashboardBody(
  tags$head(tags$style(
    HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
  )),
  tabItems(
    ####--------------------| Profile  --------------------####
    tabItem( tabName = "tabProfile",
             fluidPage(
               # |--| Column 1: Athlete Filter & Rep Summary --------------------
               column(
                 width = 3,
                 # |----| Row 1: Profile Filter --------------------
                 fluidRow(
                   box(
                     background = "black",
                     width = 12,
                     height = 100,
                     # ^ 
                     selectInput(
                       inputId = "profFilter",
                       label = p(strong("Athlete: ")),
                       choices = repIds,
                       multiple = FALSE
                     )
                   )
                 ),
                 # |----| Row 2: Rep Summary Table -------------------- 
                 fluidRow(
                   box(
                     title = "Sprint Rep Summary",
                     background = "black",
                     width = 12,
                     height = 300,
                     tableOutput(outputId = "tableRepSummary")
                   )
                 )
               ),
               # |--| Column 2: Rep Data - Info Boxes & Plots --------------------
               column(
                 width = 9,
                 # |----| Row 1: Rep Key Outcomes Infoboxes --------------------
                 fluidRow(
                   box(width = 12,
                       background = "black",
                       # Peak Force
                       infoBoxOutput("infoboxPeakF", width = 3),
                       # Peak Velocity 
                       infoBoxOutput("infoboxPeakV", width = 3),
                       # Power Max
                       infoBoxOutput("infoboxPmax", width = 3),
                       # FV Slope
                       infoBoxOutput("infoboxFVslope", width = 3),
                       # RF Max %
                       infoBoxOutput("infoboxRFMax", width = 3),
                       # DRF %
                       infoBoxOutput("infoboxDRF", width = 3),
                       # Optimal Velocity
                       infoBoxOutput("infoboxOptV", width = 3),
                       # Max Speed
                       infoBoxOutput("infoboxMaxSpeed", width = 3)
                   )
                 ),
                 # |----| Row 2: Rep Plots --------------------
                 fluidRow(
                   # |------| Model Fit --------------------
                   box(
                     background = "black",
                     width = 6,
                     height = 225,
                     echarts4rOutput(
                       outputId = "plotRecorded",
                       width = "100%",
                       height = "210px"
                     )
                   ),
                   # |------| Force-Velocity-Time --------------------
                   box(
                     background = "black",
                     width = 6,
                     height = 225,
                     echarts4rOutput(
                       outputId = "plotFVT",
                       width = "100%",
                       height = "210px"
                     )
                   ),
                   # |------| Velocity-RF-DRF Plot -------------------
                   box(
                     background = "black",
                     width = 6,
                     height = 225,
                     echarts4rOutput(
                       outputId = "plotFPV",
                       width = "100%",
                       height = "210px"
                     )
                   ),
                   # |------| Velocity-RF-DRF Plot -------------------
                   box(
                     background = "black",
                     width = 6,
                     height = 225,
                     echarts4rOutput(
                       outputId = "plotPDP",
                       width = "100%",
                       height = "210px"
                     )
                   )
                 )
               )
             )
    ),
    
    ##########--------------------| Tracking  --------------------##########
    tabItem(
      tabName = "tabTracking",
      fluidRow(
        # |--| Athlete Filter --------------------
        box(
          background = "black",
          width = 2,
          height = 200,
          selectInput(
            inputId = "chronFilter",
            label = p(strong("Athlete: ")),
            choices = distinct(cModel, Name),
            multiple = FALSE
          ),
          selectInput(
            inputId = "chronWeekFilter",
            label = p(strong("Week No: ")),
            choices = c(2:10),
            multiple = FALSE
          )
        ),
        box(
          background = "black",
          width = 4,
          height = 200,
          echarts4rBoxOutput("chronV0", height = 75),
          echarts4rBoxOutput("chronF0", height = 75)
        ),
        box(
          background = "black",
          width = 4,
          height = 200,
          echarts4rBoxOutput("chronP", height = 75)
        )
      ),
      fluidRow(
        box(
          width = 4,
          #background = "black",
          echarts4rOutput("chronpHRZ", height = "225px")
        )
      )
    ),
    ##########--------------------| Comparisons  --------------------##########
    tabItem(
      tabName = "tabComparisons",
      # |--| Row 1: Athlete Filters, Summaries, InfoBoxes   --------------------
      fluidRow(
        
        # |----| Column 1: Athlete 1 --------------------
        column(
          width = 3,
          box(
            background = "maroon",
            width = 12,
            height = 350,
            # |------| Athlete 1 Filter --------------------
            selectInput(
              inputId = "compFilter1",
              label = p(strong("Select Athlete 1")),
              choices = repIds,
              multiple = FALSE
            ),
            # |------| Athlete 1 Summary Table --------------------
            tableOutput(outputId = "tableCompSummary1")
          )
        ),
        # |----| Column 2:Comparison infoBox Column --------------------
        column(
          width = 6,
          box(
            background = "black",
            width = 12,
            # Peak Force
            infoBoxOutput("compPeakV1", width = 6),
            infoBoxOutput("compPeakV2", width = 6),
            # Peak Velocity 
            infoBoxOutput("compPeakF1", width = 6),
            infoBoxOutput("compPeakF2", width = 6),
            # Power Max
            infoBoxOutput("compPmax1", width = 6),
            infoBoxOutput("compPmax2", width = 6)
          )
        ),
        # |----| Column 3: Athlete 2 --------------------
        column(
          width = 3,
          box(
            background = "aqua",
            width = 12,
            height = 350,
            # |------| Athlete 2 Filter --------------------
            selectInput(
              inputId = "compFilter2",
              label = p(strong("Select Athlete 2")),
              choices = repIds,
              multiple = FALSE
            ),
            # |------| Athlete 2 Summary Table --------------------
            tableOutput(outputId = "tableCompSummary2")
          )
        )
      ),
      # |--| Profile Comparison Plots  --------------------
      fluidRow(
        tabBox(
          id = "compTabSet1",
          side = "right",
          title = tagList(shiny::icon("running"), "Profile Comparison"),
          width = 12,
          height = 350,
          # |----| FV-Time Plot --------------------
          # |----| PR-Distance Plot --------------------
          tabPanel(
            title = "Power-Ratio",
            value = "PwrRatio",
            # Athlete 1
            box(
              height = 290,
              width = 6,
              background = "maroon",
              echarts4rOutput(
                outputId = "CompPlotPFD1",
                width = "100%",
                height = "275px"
              )
            ),
            # Athlete 2
            box(
              height = 290,
              width = 6,
              background = "aqua",
              echarts4rOutput(
                outputId = "CompPlotPFD2",
                width = "100%",
                height = "275px"
              )
            )
          ),
          # |----| FP-Velocity Plot --------------------
          tabPanel(
            title = "Force-Power",
            value = "FrcePwr",
            # Athlete 1
            box(
              height = 290,
              width = 6,
              background = "maroon",
              echarts4rOutput(
                outputId = "CompPlotFPV1",
                width = "100%",
                height = "275px"
              )
            ),
            # Athlete 2
            box(
              height = 290,
              width = 6,
              background = "aqua",
              echarts4rOutput(
                outputId = "CompPlotFPV2",
                width = "100%",
                height = "275px"
              )
            )
          ),
          # |----| FV-Time Plot --------------------
          tabPanel(
            title = "Force-Velo",
            value = "FrceVelo",
            # Athlete 1
            box(
              height = 290,
              width = 6,
              background = "maroon",
              echarts4rOutput(
                outputId = "CompPlotFVT1",
                width = "100%",
                height = "275px"
              )
            ),
            # Athlete 2
            box(
              height = 290,
              width = 6,
              background = "aqua",
              echarts4rOutput(
                outputId = "CompPlotFVT2",
                width = "100%",
                height = "275px"
              )
            )
          )
        )
      )
    )
  )
)





#### > App --------------------
dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)
