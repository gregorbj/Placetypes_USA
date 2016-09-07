library(shiny)
library(leaflet)
library(RColorBrewer)


shinyUI(fluidPage(

  sidebarLayout(

    sidebarPanel(
      selectInput(
        inputId = "State",
        label = "Select State",
        selected = "Oregon",
        choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                    "Colorado", "Connecticut", "Delaware", "District of Columbia",
                    "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                    "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                    "Maryland", "Massachusetts", "Michigan", "Minnesota",
                    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
                    "New Hampshire", "New Jersey", "New Mexico", "New York",
                    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
                    "Pennsylvania", "Rhode Island", "South Carolina",
                    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                    "Virginia", "Washington", "West Virginia", "Wisconsin",
                    "Wyoming")
        ),
      selectInput(
        inputId = "Map",
        label = "Information to Map",
        selected = "None",
        choices = c("Location Type", "Area Type", "Development Type",
                    "Density Levels", "Diversity Levels", "Design Levels",
                    "Transit Levels", "Accessibility Levels")
      ),
      radioButtons(
        inputId = "Type",
        label = "Choose Place Type Component to Edit",
        choices = c("Location Type", "Area Type", "Development Type")
        ),
      conditionalPanel(
        condition = "input.Type == 'Location Type'",
        numericInput(inputId = "UrbanizedThreshold",
                     label = "Urbanized Area Threshold - Population Within 5 Miles",
                     min = 0, max = 100000, value = 30000, step = 1000
        ),
        numericInput(inputId = "UrbanizedAreaDensity",
                     label = "Urbanized Area Urban Density Threshold - Population Within 1 Mile",
                     min = 0, max = 5000, value = 1000, step = 100
        ),
        numericInput(inputId = "NearUrbanized",
                     label = "Urbanized Vicinity Threshold - Population Within 15 Miles",
                     min = 0, max = 100000, value = 60000, step = 1000
        ),
        numericInput(inputId = "NearUrbanizedDensity",
                     label = "Urbanized Vicinity Urban Density Threshold - Population Within 1 Mile",
                     min = 0, max = 5000, value = 2000, step = 100
        ),
        numericInput(inputId = "OtherUrbanDensity",
                     label = "Non-urbanized Vicinity Urban Density Threshold - Population Within 2 Miles",
                     min = 0, max = 5000, value = 2000, step = 100
        )
      ),
      conditionalPanel(
        condition = "input.Type == 'Area Type'",
        div(
          div(style="display:inline-block",
              selectInput(inputId = "JobAccess",
                          label = "Jobs Access Variable",
                          choices = c("EMPTOT_0.25", "EMPTOT_1", "EMPTOT_2",
                                      "EMPTOT_5", "EMPTOT_10", "EMPTOT_15"),
                          selected = "EMPTOT_2"
              )),
          div(style="display:inline-block",
              selectInput(inputId = "PopAccess",
                          label = "Population Access Variable",
                          choices = c("TOTPOP10_0.25", "TOTPOP10_1", "TOTPOP10_2",
                                      "TOTPOP10_5", "TOTPOP10_10", "TOTPOP10_15"),
                          selected = "TOTPOP10_5"
              ))
        ),
        div(
          p(strong("Regional Access Levels")),
          div(style="display:inline-block", numericInput(inputId = "AccessL", label = "Low", value = 0.1, min = 0, max = 5, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "AccessM", label = "Medium", value = 0.5, min = 0, max = 5, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "AccessH", label = "High", value = 2, min = 0, max = 5, step = 0.1))
          ),
        div(
          p(strong("Density Levels")),
          div(style="display:inline-block", numericInput(inputId = "DensityL", label = "Low", value = 0.1, min = 0, max = 10, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "DensityM", label = "Medium", value = 1.0, min = 0, max = 10, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "DensityH", label = "High", value = 5.0, min = 0, max = 10, step = 0.1))
        ),
        div(
          p(strong("Design1 Levels")),
          div(style="display:inline-block", numericInput(inputId = "Design1L", label = "Low", value = 1.3, min = 0, max = 10, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "Design1M", label = "Medium", value = 2.5, min = 0, max = 10, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "Design1H", label = "High", value = 3.3, min = 0, max = 10, step = 0.1))
        ),
        div(
          p(strong("Design2 Levels")),
          div(style="display:inline-block", numericInput(inputId = "Design2L", label = "Low", value = 12.5, min = 0, max = 30, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "Design2M", label = "Medium", value = 15.6, min = 0, max = 30, step = 0.1)),
          div(style="display:inline-block", numericInput(inputId = "Design2H", label = "High", value = 20, min = 0, max = 30, step = 0.1))
        )
      ),
      conditionalPanel(
        condition = "input.Type == 'Development Type'",
        div(
          p(strong("Diversity1 Levels")),
          div(
            div(style="display:inline-block", numericInput(inputId = "Diversity1LMin", label = "Low Min", value = 0.15, min = 0, max = 1, step = 0.05)),
            div(style="display:inline-block", numericInput(inputId = "Diversity1MMin", label = "Med. Min", value = 0.25, min = 0, max = 1, step = 0.05)),
            div(style="display:inline-block", numericInput(inputId = "Diversity1HMin", label = "High Min", value = 0.5, min = 0, max = 1, step = 0.05))

          ),
          div(
            div(style="display:inline-block", numericInput(inputId = "Diversity1LMax", label = "Low Max", value = 8, min = 1, max = 10, step = 0.5)),
            div(style="display:inline-block", numericInput(inputId = "Diversity1MMax", label = "Med. Max", value = 4, min = 1, max = 10, step = 0.5)),
            div(style="display:inline-block", numericInput(inputId = "Diversity1HMax", label = "High Max", value = 2, min = 0, max = 10, step = 0.5))
         )
        ),
        div(
          p(strong("Diversity2 Levels")),
          div(
            div(style="display:inline-block", numericInput(inputId = "Diversity2LMin", label = "Low Min", value = 0.05, min = 0, max = 1, step = 0.05)),
            div(style="display:inline-block", numericInput(inputId = "Diversity2MMin", label = "Med. Min", value = 0.15, min = 0, max = 1, step = 0.05)),
            div(style="display:inline-block", numericInput(inputId = "Diversity2HMin", label = "High Min", value = 0.25, min = 0, max = 1, step = 0.05))
          ),
          div(
            div(style="display:inline-block", numericInput(inputId = "Diversity2LMax", label = "Low Max", value = 40, min = 1, max = 50, step = 0.5)),
            div(style="display:inline-block", numericInput(inputId = "Diversity2MMax", label = "Med. Max", value = 20, min = 1, max = 50, step = 0.5)),
            div(style="display:inline-block", numericInput(inputId = "Diversity2HMax", label = "High Max", value = 10, min = 0, max = 50, step = 0.5))
          )
        ),
        div(
          p(strong("Transit Levels")),
          div(style="display:inline-block", numericInput(inputId = "TransitL", label = "Low", value = 1, min = 0, max = 200, step = 1)),
          div(style="display:inline-block", numericInput(inputId = "TransitM", label = "Medium", value = 20, min = 0, max = 200, step = 1)),
          div(style="display:inline-block", numericInput(inputId = "TransitH", label = "High", value = 150, min = 0, max = 200, step = 1))
        )
      ),
      hr(),
      actionButton(
        inputId = "Calculate",
        label = "Calculate"
      ),
      actionButton(
        inputId = "Undo",
        label = "Undo"
      ),
      actionButton(
        inputId = "Restore",
        label = "Restore"
      ),
      actionButton(
        inputId = "Save",
        label = "Save"
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Map",
          h3(textOutput("MapToDisplay")),
          leafletOutput("map", width = 800, height = 800),
          width = 1200
        ),
        tabPanel(
          title = "App Info",
          includeHTML("info/app.html")
        ),
        tabPanel(
          title = "Location Type Info",
          includeHTML("info/location_type.html")
        ),
        tabPanel(
          title = "Area Type Info",
          includeHTML("info/area_type.html")
        ),
        tabPanel(
          title = "Development Type Info",
          includeHTML("info/development_type.html")
        ),
        tabPanel(
          title = "Summary Statistics",
          h5("Density Summary"),
          verbatimTextOutput("DensitySummary"),
          h5("Diversity1 Summary"),
          verbatimTextOutput("Diversity1Summary"),
          h5("Diversity2 Summary"),
          verbatimTextOutput("Diversity2Summary"),
          h5("Design1 Summary"),
          verbatimTextOutput("Design1Summary"),
          h5("Design2 Summary"),
          verbatimTextOutput("Design2Summary"),
          h5("Transit Summary"),
          verbatimTextOutput("TransitSummary"),
          h5("AccessLvl Summary"),
          verbatimTextOutput("AccessLvlSummary"),
          h5("DensityLvl Summary"),
          verbatimTextOutput("DensityLvlSummary"),
          h5("DesignLvl Summary"),
          verbatimTextOutput("DesignLvlSummary"),
          h5("DiversityLvl Summary"),
          verbatimTextOutput("DiversityLvlSummary"),
          h5("TransitLvl Summary"),
          verbatimTextOutput("TransitLvlSummary"),
          h5("Location Type Summary"),
          verbatimTextOutput("LocationTypeSummary"),
          h5("Area Type Summary"),
          verbatimTextOutput("AreaTypeSummary"),
          h5("Development Type Summary"),
          verbatimTextOutput("DevelopmentTypeSummary")
        )
      )
    )

  )
))

