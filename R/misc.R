#======
#misc.R
#======

#Miscellaneous data processing to support place type classification.
#This script creates and saves an R data frame which associates state names with
#Census FIPS codes. This is table is used in the Shiny application to
#select a state data set using a dropdown list. The script also saves starting
#values for the key parameter widgets in the Shiny application.

#Save data frame of state FIPS crosswalk table
#=============================================
StateFips_df <-
  read.delim("inst/extdata/state_fips.txt", colClasses = rep("character", 3))
save(StateFips_df, file = "data/StateFips_df.Rda")

#Save default parameter input values
#===================================
Inputs_ls = list(
  numericInput = c(
    UrbanizedThreshold = 30000,
    UrbanizedAreaDensity = 1000,
    NearUrbanized = 60000,
    NearUrbanizedDensity = 2000,
    OtherUrbanDensity = 2000,
    AccessL = 0.1,
    AccessM = 0.5,
    AccessH = 2,
    DensityL = 0.1,
    DensityM = 1.0,
    DensityH = 5.0,
    Design1L = 1.3,
    Design1M = 2.5,
    Design1H = 3.3,
    Design2L = 12.5,
    Design2M = 15.6,
    Design2H = 20,
    Diversity1LMin = 0.15,
    Diversity1LMax = 8,
    Diversity1MMin = 0.25,
    Diversity1MMax = 4,
    Diversity1HMin = 0.5,
    Diversity1HMax = 2,
    Diversity2LMin = 0.05,
    Diversity2LMax = 40,
    Diversity2MMin = 0.15,
    Diversity2MMax = 20,
    Diversity2HMin = 0.25,
    Diversity2HMax = 10,
    TransitL = 1,
    TransitM = 20,
    TransitH = 150
  ),
  selectInput = c(
    JobAccess = "EMPTOT_2",
    PopAccess = "TOTPOP10_5"
  )
)
save(Inputs_ls, file = "app/Inputs_ls.Rda")

