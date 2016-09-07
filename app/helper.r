#========
#helper.r
#========


#Calculate location type
#=======================
calcLocationType <- function(Data_df, Inp_ls) {
  #Get input parameters used in calculations
  input <- as.list(
    Inp_ls[["numericInput"]][c("NearUrbanized", "OtherUrbanDensity",
                               "UrbanizedThreshold", "UrbanizedAreaDensity",
                               "NearUrbanizedDensity")]
  )
  #Calculated categories used in applying rules to determine location type
  IsIsolated_ <- Data_df$TOTPOP10_15 < input$NearUrbanized
  IsIsolatedUrban_ <- IsIsolated_ & (Data_df$TOTPOP10_2 >= input$OtherUrbanDensity)
  IsIsolatedRural_ <- IsIsolated_ & (Data_df$TOTPOP10_2 < input$OtherUrbanDensity)
  IsUrbanized_ <- (Data_df$TOTPOP10_5 >= input$UrbanizedThreshold) &
    (Data_df$TOTPOP10_1 >= input$UrbanizedAreaDensity) & !IsIsolated_
  IsUrbanNearUrbanized_ <- !IsIsolated_ & (Data_df$TOTPOP10_1 >= input$NearUrbanizedDensity)
  #Apply rules to determine location type
  LocationType_ <- rep("Rural Near Urbanized", nrow(Data_df))
  LocationType_[IsIsolatedRural_] <- "Rural Not Near Urbanized"
  LocationType_[IsIsolatedUrban_] <- "Urban Not Near Urbanized"
  LocationType_[IsUrbanNearUrbanized_] <- "Urban Near Urbanized"
  LocationType_[IsUrbanized_] <- "Urbanized"
  LocationType_ <- factor(LocationType_)
  #Return the result
  Data_df$LocationType <- LocationType_
  Data_df
}


#Calculate area type and related variables
#=========================================
calcAreaType <- function(Data_df, Inp_ls) {
  N <- nrow(Data_df)
  #Get input parameters used in calculations
  input1 <- as.list(
    Inp_ls[["selectInput"]][c("JobAccess", "PopAccess")]
  )
  input2 <- as.list(
    Inp_ls[["numericInput"]][c("AccessL", "AccessM", "AccessH",
                               "DensityL", "DensityM", "DensityH",
                               "Design1L", "Design1M", "Design1H",
                               "Design2L", "Design2M", "Design2H")]
  )
  input <- c(input1, input2)
  #Calculate accessibility variable
  Jobs_ <- Data_df[[input$JobAccess]]
  Jobs_[Jobs_ == 0] <- 1
  Pop_ <- Data_df[[input$PopAccess]]
  Pop_[Pop_ == 0] <- 1
  Ratio_ <- 2 * (Jobs_ * Pop_) / (Jobs_ + Pop_)
  Ratio_ <- Ratio_ / 10000
  Ratio_[is.nan(Ratio_)] <- 0
  #Calculate access level
  Access_ <- rep("VL", N)
  Access_[Ratio_ > input$AccessL] <- "L"
  Access_[Ratio_ > input$AccessM] <- "M"
  Access_[Ratio_ > input$AccessH] <- "H"
  Access_ <- factor(Access_, levels = c("VL", "L", "M", "H"), ordered = TRUE)
  #Calculate density level
  Density_ <- rep("VL", N)
  Density_[Ratio_ > input$DensityL] <- "L"
  Density_[Ratio_ > input$DensityM] <- "M"
  Density_[Ratio_ > input$DensityH] <- "H"
  Density_ <- factor(Density_, levels = c("VL", "L", "M", "H"), ordered = TRUE)
  #Calculate design1 level
  Design1_ <- rep("VL", N)
  Design1_[Ratio_ > input$Design1L] <- "L"
  Design1_[Ratio_ > input$Design1M] <- "M"
  Design1_[Ratio_ > input$Design1H] <- "H"
  Design1_ <- factor(Design1_, levels = c("VL", "L", "M", "H"), ordered = TRUE)
  #Calculate design2 level
  Design2_ <- rep("VL", N)
  Design2_[Ratio_ > input$Design2L] <- "L"
  Design2_[Ratio_ > input$Design2M] <- "M"
  Design2_[Ratio_ > input$Design2H] <- "H"
  Design2_ <- factor(Design2_, levels = c("VL", "L", "M", "H"), ordered = TRUE)
  #Calculate final design level
  Design_ <- rep("VL", N)
  Design_[(Design1_ == "L") | (Design2_ == "L")] <- "L"
  Design_[(Design1_ == "M") | (Design2_ == "M")] <- "M"
  Design_[(Design1_ == "H") | (Design2_ == "H")] <- "H"
  Design_ <- factor( Design_, levels=c("VL", "L", "M", "H"), ordered = TRUE )
  #Calculate area type
  Type_ <- rep("Low Density/Rural", length(Ratio_))
  Type_[Access_ == "H" & Density_ %in% c("M", "H") & Design_ == "H" ] <- "Regional Center"
  Type_[Access_ == "H" & Density_ %in% c("M", "H") & Design_ %in% c("VL", "L", "M") ] <- "Close In Community"
  Type_[Access_ == "H" & Density_ == "L"] <- "Close In Community"
  Type_[Access_ == "H" & Density_ == "VL"] <- "Suburb/Town"
  Type_[Access_ == "M" & Density_ %in% c("M", "H")] <- "Close In Community"
  Type_[Access_ == "M" & Density_ %in% c("L", "VL")] <- "Suburb/Town"
  Type_[Access_ %in% c("L", "VL") & Density_ != "VL"] <- "Suburb/Town"
  Type_ <-
    factor(Type_,
           levels = c("Low Density/Rural", "Suburb/Town",
                      "Close In Community", "Regional Center"),
           ordered = TRUE)
  #Return result
  Data_df$AccessLvl <- Access_
  Data_df$DensityLvl <- Density_
  Data_df$DesignLvl <- Design_
  Data_df$AreaType <- Type_
  Data_df
}


#Calculate Development Type
#==========================
calcDevelopmentType <- function(Data_df, Inp_ls) {
  N <- nrow(Data_df)
  #Get input parameters used in calculations
  input <- as.list(
    Inp_ls[["numericInput"]][c("Diversity1LMin", "Diversity1LMax",
                               "Diversity1MMin", "Diversity1MMax",
                               "Diversity1HMin", "Diversity1HMax",
                               "Diversity2LMin", "Diversity2LMax",
                               "Diversity2MMin", "Diversity2MMax",
                               "Diversity2HMin", "Diversity2HMax",
                               "TransitL", "TransitM", "TransitH")]
  )
  #Calculate diversity1 level
  OkDensity <- Data_df$DensityLvl != "VL"
  Diversity1_ <- rep("VL", N)
  Diversity1_[OkDensity & (Data_df$Diversity1 >= input$Diversity1LMin) & (Data_df$Diversity1 <= input$Diversity1LMax)] <- "L"
  Diversity1_[OkDensity & (Data_df$Diversity1 >= input$Diversity1MMin) & (Data_df$Diversity1 <= input$Diversity1MMax)] <- "M"
  Diversity1_[OkDensity & (Data_df$Diversity1 >= input$Diversity1HMin) & (Data_df$Diversity1 <= input$Diversity1HMax)] <- "H"
  #Calculate diversity2 levels
  Diversity2_ <- rep("VL", N)
  Diversity2_[OkDensity & (Data_df$Diversity2 >= input$Diversity2LMin) & (Data_df$Diversity2 <= input$Diversity2LMax)] <- "L"
  Diversity2_[OkDensity & (Data_df$Diversity2 >= input$Diversity2MMin) & (Data_df$Diversity2 <= input$Diversity2MMax)] <- "M"
  Diversity2_[OkDensity & (Data_df$Diversity2 >= input$Diversity2HMin) & (Data_df$Diversity2 <= input$Diversity2HMax)] <- "H"
  #Calculate final diversity levels
  Diversity_ <- rep("VL", N)
  Diversity_[(Diversity1_ == "L") | (Diversity2_ == "L")] <- "L"
  Diversity_[(Diversity1_ == "M") | (Diversity2_ == "M")] <- "M"
  Diversity_[(Diversity1_ == "H") | (Diversity2_ == "H")] <- "H"
  #Calculate transit levels
  Service_ <- Data_df$D4c
  Service_[is.na(Service_)] <- 0
  Transit_ <- rep("VL", N)
  Transit_[Service_ >= input$TransitL] <- "L"
  Transit_[Service_ >= input$TransitM] <- "M"
  Transit_[Service_ >= input$TransitH] <- "H"
  #Calculate development type
  Type_ <- rep("Low Density/Rural", N)
  OkDensity <- Data_df$DensityLvl != "VL"
  Type_[OkDensity & (Data_df$Diversity1 >= 1)] <- "Employment"
  Type_[OkDensity & (Data_df$Diversity1 < 1)] <- "Residential"
  Type_[(Diversity_ == "H") & (Data_df$DensityLvl %in% c("M","H")) & (Data_df$DesignLvl %in% c("M", "H"))] <- "Mixed"
  Type_[(Type_ == "Mixed") & (Data_df$DensityLvl == "H") & (Data_df$DesignLvl == "H")] <- "Mixed High"
  Type_[(Type_ == "Mixed High") & (Transit_ == "H") & (Data_df$DesignLvl == "H")] <- "TOD"
  Type_[(Type_ == "Employment") & (Transit_ == "H") & (Data_df$DensityLvl == "H") & (Data_df$DesignLvl == "H")] <- "TOD"
  Type_ <-
    factor(Type_, levels = c("Low Density/Rural", "Employment", "Residential",
                             "Mixed", "Mixed High", "TOD"))
  #Return result
  Diversity_ <- factor(Diversity_, levels=c("VL", "L", "M", "H"), ordered = TRUE)
  Transit_ <- factor(Transit_, levels=c("VL", "L", "M", "H"), ordered = TRUE)
  Data_df$DiversityLvl <- Diversity_
  Data_df$TransitLvl <- Transit_
  Data_df$DevelopmentType <- Type_
  Data_df
}

