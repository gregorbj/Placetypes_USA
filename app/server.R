library(shiny)
library(leaflet)
library(RColorBrewer)

source("helper.r")

shinyServer(function(input, output, session) {

  #Load the place data
  load("PlaceInp_df.Rda")

  #Load state FIPS crosswalk table
  load("StateFips_df.Rda")

  #Load list of all saved inputs
  load("Inputs_ls.Rda")

  #Load last set of saved parameters
  loadParameters <- function(Inp_ls){
    #Load numeric inputs
    for(Name in names(Inp_ls$numericInput)) {
      updateNumericInput(session, Name, value = as.vector(Inp_ls[["numericInput"]][Name]))
    }
    #Load select inputs
    for(Name in names(Inp_ls$selectInput)) {
      updateSelectInput(session, Name, selected = as.vector(Inp_ls[["selectInput"]][Name]))
    }
  }
  loadParameters(Inputs_ls)

  #Create reactive inputs dataset
  Inputs <- reactiveValues(
    Orig = Inputs_ls,
    Current = Inputs_ls,
    Last = NULL
    )

  #Define function to retrieve a vector of the GUI input values
  getInputs <- function(CurrInp_ls) {
    NewInp_ls <- CurrInp_ls
    for(Name in names(NewInp_ls[["numericInput"]])) {
      NewInp_ls[["numericInput"]][Name] <- input[[Name]]
    }
    for(Name in names(NewInp_ls[["selectInput"]])) {
      NewInp_ls[["selectInput"]][Name] <- input[[Name]]
    }
    NewInp_ls
  }

  #Calculate and create initial place type reactive dataset
  PlaceType <- reactiveValues( Data = NULL )
  PlaceType$Data <- local({
    NewInputs_ls <- isolate(Inputs$Current)
    PlaceType_df <- calcLocationType(Data_df = PlaceInp_df,
                                     Inp_ls = NewInputs_ls)
    PlaceType_df <- calcAreaType(Data_df = PlaceType_df,
                                 Inp_ls = NewInputs_ls)
    PlaceType_df <- calcDevelopmentType(Data_df = PlaceType_df,
                                        Inp_ls = NewInputs_ls)
    PlaceType_df
  })

  #Recalculate
  observeEvent(input$Calculate, {
    XInputs_ls <- isolate(Inputs$Current)
    NewInputs_ls <- getInputs(XInputs_ls)
    Inputs$Current <- NewInputs_ls
    Inputs$Last <- XInputs_ls
    PlaceType_df <- calcLocationType(Data_df = PlaceInp_df,
                                     Inp_ls = NewInputs_ls)
    PlaceType_df <- calcAreaType(Data_df = PlaceType_df,
                                 Inp_ls = NewInputs_ls)
    PlaceType_df <- calcDevelopmentType(Data_df = PlaceType_df,
                                        Inp_ls = NewInputs_ls)
    PlaceType$Data <- PlaceType_df
  })

  #Undo
  observeEvent(input$Undo, {
    XInputs_ls <- isolate(Inputs$Current)
    NewInputs_ls <- isolate(Inputs$Last)
    loadParameters(NewInputs_ls)
    Inputs$Current <- NewInputs_ls
    Inputs$Last <- XInputs_ls
    PlaceType_df <- calcLocationType(Data_df = PlaceInp_df,
                                     Inp_ls = NewInputs_ls)
    PlaceType_df <- calcAreaType(Data_df = PlaceType_df,
                                 Inp_ls = NewInputs_ls)
    PlaceType_df <- calcDevelopmentType(Data_df = PlaceType_df,
                                        Inp_ls = NewInputs_ls)
    PlaceType$Data <- PlaceType_df
  })

  #Restore
  observeEvent(input$Restore, {
    XInputs_ls <- isolate(Inputs$Current)
    NewInputs_ls <- isolate(Inputs$Orig)
    loadParameters(NewInputs_ls)
    Inputs$Current <- NewInputs_ls
    Inputs$Last <- XInputs_ls
    PlaceType_df <- calcLocationType(Data_df = PlaceInp_df,
                                     Inp_ls = NewInputs_ls)
    PlaceType_df <- calcAreaType(Data_df = PlaceType_df,
                                 Inp_ls = NewInputs_ls)
    PlaceType_df <- calcDevelopmentType(Data_df = PlaceType_df,
                                        Inp_ls = NewInputs_ls)
    PlaceType$Data <- PlaceType_df
  })

  #Create reactive value for selected state data and initialize
  StateData <- reactive({
    PlaceType$Data[PlaceType$Data$SFIPS == StateFips_df$FIPS[StateFips_df$Name == input$State], ]
  })

  #Set up mapping palette depending on the information to be mapped
  Pal <- reactive({
    LevelCol_ <- c("red", "orange", "yellow", "green")
    LocCol_ <- c("blue", "green", "red", "orange", "purple")
    AreaCol_ <- c("red", "orange", "yellow", "green")
    DevCol_ <- c("green", "red", "blue", "purple", "magenta", "yellow" )
    switch(input$Map,
           "Location Type" = colorFactor(palette = LocCol_, PlaceType$Data$LocationType),
           "Area Type" = colorFactor(palette = AreaCol_, PlaceType$Data$AreaType),
           "Development Type" = colorFactor(palette = DevCol_, PlaceType$Data$DevelopmentType),
           "Density Levels" = colorFactor(palette = LevelCol_, PlaceType$Data$DensityLvl),
           "Diversity Levels" = colorFactor(palette = LevelCol_, PlaceType$Data$DiversityLvl),
           "Design Levels" = colorFactor(palette = LevelCol_, PlaceType$Data$DesignLvl),
           "Transit Levels" = colorFactor(palette = LevelCol_, PlaceType$Data$TransitLvl),
           "Accessibility Levels" = colorFactor(palette = LevelCol_, PlaceType$Data$AccessLvl)
           )
  })

  #Render base map
  output$map <- renderLeaflet({
    State <- input$State
    StateFips <- StateFips_df$FIPS[StateFips_df$Name == State]
    MapDef_df <- PlaceInp_df[PlaceInp_df$SFIPS == StateFips, c("lat", "lng")]
    leaflet(MapDef_df) %>% addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
  })

  #Render information to map
  observe({
    MapToDisplay <- input$Map
    pal <- Pal()
    if (MapToDisplay == "Location Type") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(LocationType), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Area Type") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(AreaType), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Development Type") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(DevelopmentType), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Density Levels") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(DensityLvl), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Diversity Levels") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(DiversityLvl), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Design Levels") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(DesignLvl), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Transit Levels") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(TransitLvl), fillOpacity = 0.8)
    }
    if (MapToDisplay == "Accessibility Levels") {
      leafletProxy("map", data = StateData()) %>% clearShapes() %>%
        addCircles(radius = ~500, weight = 1, color = "#777777",
                   fillColor = ~pal(AccessLvl), fillOpacity = 0.8)
    }
  })

  #Render legend
  observe({
    MapToDisplay <- input$Map
    if (MapToDisplay == "Location Type") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$LocationType)
    }
    if (MapToDisplay == "Area Type") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$AreaType)
    }
    if (MapToDisplay == "Development Type") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$DevelopmentType)
    }
    if (MapToDisplay == "Density Levels") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$DensityLvl)
    }
    if (MapToDisplay == "Diversity Levels") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$DiversityLvl)
    }
    if (MapToDisplay == "Design Levels") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$DesignLvl)
    }
    if (MapToDisplay == "Transit Levels") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$TransitLvl)
    }
    if (MapToDisplay == "Accessibility Levels") {
      leafletProxy("map", data = StateData()) %>% clearControls() %>%
        addLegend(position = "bottomright", pal = Pal(), values = StateData()$AccessLvl)
    }
  })

  #Render map title
  output$MapToDisplay <- renderText({
    input$Map
  })

  #Observe save button to save data
  observeEvent(input$Save, {
    if (!file.exists("outputs")) {
      dir.create("outputs")
    }
    Outputs_df <- PlaceType$Data
    save(Outputs_df, file = "outputs/PlaceType.Rda")
    # write.table(Outputs_df, file = "outputs/place_types.csv", row.names = FALSE,
    #             col.names = TRUE, sep = ",")
    Inputs_ls <- Inputs$Values
    save(Inputs_ls, file = "outputs/Inputs_ls.Rda")
  })

  #Create summary statistics of all measures
  output$DensitySummary <- renderPrint(summary(PlaceType$Data$Density))
  output$Diversity1Summary <- renderPrint(summary(PlaceType$Data$Diversity1))
  output$Diversity2Summary <- renderPrint(summary(PlaceType$Data$Diversity2))
  output$Design1Summary <- renderPrint(summary(PlaceType$Data$Design1))
  output$Design2Summary <- renderPrint(summary(PlaceType$Data$Design2))
  output$TransitSummary <- renderPrint(summary(PlaceType$Data$D4c))
  output$AccessLvlSummary <- renderPrint(summary(PlaceType$Data$AccessLvl))
  output$DensityLvlSummary <- renderPrint(summary(PlaceType$Data$DensityLvl))
  output$DesignLvlSummary <- renderPrint(summary(PlaceType$Data$DesignLvl))
  output$DiversityLvlSummary <- renderPrint(summary(PlaceType$Data$DiversityLvl))
  output$TransitLvlSummary <- renderPrint(summary(PlaceType$Data$TransitLvl))
  output$LocationTypeSummary <- renderPrint(summary(PlaceType$Data$LocationType))
  output$AreaTypeSummary <- renderPrint(summary(PlaceType$Data$AreaType))
  output$DevelopmentTypeSummary <- renderPrint(summary(PlaceType$Data$DevelopmentType))

})
