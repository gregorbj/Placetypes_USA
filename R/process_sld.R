#=============
#process_sld.R
#=============


#Purpose
#=======
#This script creates a data set from the EPA's Smart Location Database (SLD)
#which includes variables that are used to define place types. Place types are
#categories of land uses such as urban center, suburban, mixed use, rural, etc.
#The SLD contains a number of smart-growth-related transportation and land use
#variables at the Census block group level. This script reduces the number of
#variables to those that have previously been found to be most useful for
#categorizing place types. The script also attributes the dataset with the
#longitudes and latitudes of the census block groups, and uses those centroids
#to calculate the amounts of population and employment located within various
#radii of each block group.


#Load resources
#==============
library(rgdal)
library(foreign)
library(geosphere)
library(fields)


#Read in U.S. block group shapefile and save as R binary file
#============================================================
#' Reads and saves shapefile to Rda
#'
#' \code{readSaveBlkGrpShapefile} Reads in shapefile of U.S. blockgroups and
#' saves to R binary file.
#'
#' This function reads the shapefile of U.S. blockgroups that is stored in the
#' inst/extdata/tl_2010_US_bg10 directory, assigns to BlkGrp_shp, and saves in
#' data directory as BlkGrp_shp.Rda if this has not already been done.
#' @param Redo a logical that determines whether the function is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
readSaveBlkGrpShapefile <- function(Redo = FALSE) {
  ShapeBinaryPathName <- "data/BlkGrp_shp.Rda"
  ShapeFilePathName <- "inst/extdata/tl_2010_US_bg10/tl_2010_US_bg10.shp"
  if (!file.exists(ShapeBinaryPathName) | Redo) {
    if (file.exists(ShapeFilePathName)) {
      BlkGrp_shp <- readOGR("inst/extdata/tl_2010_US_bg10", "tl_2010_US_bg10")
      save(BlkGrp_shp, file = ShapeBinaryPathName)
    } else {
      return(FALSE)
    }
  }
  TRUE
}
#Call the function
readSaveBlkGrpShapefile()


#Process shapefile to make list of centroids and data from shapefile
#===================================================================
#' Create block group centroid table
#'
#' \code{createBlkGrpCentroidFile} Reads in the saved R binary for the
#' shapefile, calculates centroids, and saves table.
#'
#' This function reads in the binary representation of the U.S. block group
#' shapefile, BlkGrp_shp.Rda, if it exists. After the file is loaded, the
#' block group centroids are calculated and a data frame is made with the
#' longitudes and latitudes of the centroids and census fips codes for states,
#' counties, tracts, and block groups.
#' @param Redo a logical that determines whether the function is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
createBlkGrpCentroidFile <- function(Redo = FALSE) {
  ShapeBinaryPathName <- "data/BlkGrp_shp.Rda"
  BlockGroupCtrFilePathName <- "data/BlkGrpCtr_df.Rda"
  if (!file.exists(BlockGroupCtrFilePathName) | Redo) {
    if (file.exists(ShapeBinaryPathName)) {
      load(ShapeBinaryPathName)
    } else {
      return(FALSE)
    }
    BlkGrpCtr_mx <- coordinates(BlkGrp_shp)
    BlkGrpCtr_ls <- list(lng = BlkGrpCtr_mx[,1],
                         lat = BlkGrpCtr_mx[,2])
    BlkGrpCtr_ls$StFp <- as.character(BlkGrp_shp$STATEFP10)
    BlkGrpCtr_ls$CoFp <- as.character(BlkGrp_shp$COUNTYFP10)
    BlkGrpCtr_ls$TrFp <- as.character(BlkGrp_shp$TRACTCE10)
    BlkGrpCtr_ls$BgFp <- as.character(BlkGrp_shp$BLKGRPCE10)
    BlkGrpCtr_ls$GeoId <- as.character(BlkGrp_shp$GEOID10)
    BlkGrpCtr_df <- data.frame(BlkGrpCtr_ls, stringsAsFactors = FALSE)
    save(BlkGrpCtr_df, file = "data/BlkGrpCtr_df.Rda")
    write.table(BlkGrpCtr_df, file = "inst/extdata/BG_Centroids.csv",
                row.names = FALSE, col.names = TRUE, sep = ",")
  }
  TRUE
}
#Call the function
createBlkGrpCentroidFile()


#Process Census center of block group population file
#====================================================
#' Process center of block group population file
#'
#' \code{processPopCenterFile} Reads in a U.S. Census file that identifies the
#' population-weighted centroid of each block group, formats, and saves as a
#' R binary file
#'
#' This function reads in a CSV-formatted text file provided by the U.S. Census
#' Bureau that identifies the population center of each census block group. It
#' puts the information into a data frame that is formatted similarly to the
#' block group centroid data frame. It saves the data frame as a R binary file.
#' @param Redo a logical that determines whether the function is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
processPopCenterFile <- function(Redo = FALSE) {
  PopCtrInputFilePath <- "inst/extdata/CenPop2010_Mean_BG.csv"
  PopCtrBinaryFilePath <- "data/BlkGrpPopCtr_df.Rda"
  if (!file.exists(PopCtrBinaryFilePath) | Redo) {
    if (file.exists(PopCtrInputFilePath)) {
      PopCtrInput_df <- read.csv(PopCtrInputFilePath,
                                 colClasses = c(rep("character", 4), rep("numeric", 3)))
    } else {
      return(FALSE)
    }

    #Define function to pad values with leading zeros when necessary
    padCensusId <- function(Id_, NumChar) {
      sapply(Id_, function(x) paste0(rep("0", NumChar - nchar(x)), x))
    }
    #Make list of attributes using similar names as BlkGrpCtr_df
    BlkGrpPopCtr_ls <- list()
    BlkGrpPopCtr_ls$StFp <- padCensusId(PopCtrInput_df$STATEFP, 2)
    BlkGrpPopCtr_ls$CoFp <- padCensusId(PopCtrInput_df$COUNTYFP, 3)
    BlkGrpPopCtr_ls$TrFp <- padCensusId(PopCtrInput_df$TRACTCE, 6)
    BlkGrpPopCtr_ls$BgFp <- PopCtrInput_df$BLKGRPCE
    BlkGrpPopCtr_ls$GeoId <- paste0(BlkGrpPopCtr_ls$StFp,
                                    BlkGrpPopCtr_ls$CoFp,
                                    BlkGrpPopCtr_ls$TrFp,
                                    BlkGrpPopCtr_ls$BgFp)
    BlkGrpPopCtr_ls$lng <- PopCtrInput_df$LONGITUDE
    BlkGrpPopCtr_ls$lat <- PopCtrInput_df$LATITUDE
    # Create data frame from list & save
    BlkGrpPopCtr_df <- data.frame(BlkGrpPopCtr_ls, stringsAsFactors = FALSE)
    save(BlkGrpPopCtr_df, file = PopCtrBinaryFilePath)
  }
}
#Call the function
processPopCenterFile()


#Add Centroid Locations to Smart Location Database
#=================================================
#' Read in Smart Location Database and add centroid locations
#'
#' \code{addCentroidsToSld} Reads in the Smart Location Database file, adds
#' geographic centroid locations, and saves to R binary file.
#'
#' This function reads in the EPA's Smart Location Database file, which is
#' organized by census block group. It adds the latitude and longitude
#' information for the geographic and population centroids of the block groups.
#' It removes records of block groups located in American Samoa, Guam, and the
#' Northern Mariana Islands as they don't have centroid locations and are not
#' needed for the analysis. It also fills in missing geographic centroid
#' locations for block groups in Washington DC by substituting the population
#' centroid location information. It saves the resulting data frameas a R binary
#' file.
#'
#' @param Redo a logical that determines whether the function is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
addCentroidsToSld <- function(Redo = FALSE) {
  SldBinaryFilePath <- "data/Sld_df.Rda"
  if (!file.exists(SldBinaryFilePath) | Redo) {
    SldFilePath <- "inst/extdata/SmartLocationDb.dbf"
    if (file.exists(SldFilePath)) {
      Sld_df <- read.dbf("inst/extdata/SmartLocationDb.dbf", as.is=TRUE)
      Sld_df[Sld_df == -99999] <- NA
      Sld_df$GeoId <- Sld_df$GEOID10
    } else {
      return(FALSE)
    }
    if (file.exists("data/BlkGrpCtr_df.Rda") &
        file.exists("data/BlkGrpPopCtr_df.Rda")) {
      load("data/BlkGrpCtr_df.Rda")
      load("data/BlkGrpPopCtr_df.Rda")
      #Add longitude and latitude of geographic centroids to SLD data frame
      Sld_df$lng <- BlkGrpCtr_df$lng[match(Sld_df$GeoId, BlkGrpCtr_df$GeoId)]
      Sld_df$lat <- BlkGrpCtr_df$lat[match(Sld_df$GeoId, BlkGrpCtr_df$GeoId)]
      #Add longitude and latitude of population centers to SLD data frame
      Sld_df$lng2 <- BlkGrpPopCtr_df$lng[match(Sld_df$GeoId, BlkGrpPopCtr_df$GeoId)]
      Sld_df$lat2 <- BlkGrpPopCtr_df$lat[match(Sld_df$GeoId, BlkGrpPopCtr_df$GeoId)]
      #Remove records missing population center longitude and latitude:
      #American Samoa, Guam, and Northern Mariana Islands
      Sld_df <- Sld_df[!is.na(Sld_df$lng2),]
      #Geographic centroids missing from DC (State FIPS 11), substitute pop. centers
      Sld_df$lng[is.na(Sld_df$lng)] <-Sld_df$lng2[is.na(Sld_df$lng)]
      Sld_df$lat[is.na(Sld_df$lat)] <- Sld_df$lat2[is.na(Sld_df$lat)]
      #Save the dataset
      save(Sld_df, file = "data/Sld_df.Rda")
    } else {
      return(FALSE)
    }
  }
  TRUE
}
#Call the function
addCentroidsToSld()


#Simplify the data
#=================
#' Simplify the data by removing unwanted fields
#'
#' \code{simplifyDataset} Removes SLD fields that aren't be used
#'
#' This function simplifies the dataset by removing fields that aren't used
#' in subsequent steps
#'
#' @param Redo a logical that determines whether the funciton is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either beause the dataset already exists or because it was
#' produced.
simplifyDataset <- function(Redo = FALSE) {
  SldSimpleFileDataPath <- "data/SldSimple_df.Rda"
  if (!file.exists(SldSimpleFileDataPath) | Redo) {
    if (file.exists("data/Sld_df.Rda")) {
      load("data/Sld_df.Rda")
    } else {
      return(FALSE)
    }
    #Select just the fields that will be used in the analysis
    FieldsToKeep <- c("GEOID10", "SFIPS", "CBSA", "CBSA_Name", "HH","EMPTOT","TOTPOP10",
                      "E5_RET10","E5_SVC10", "D1D","D2A_JPHH","D3amm","D3apo",
                      "D4a", "D4c", "D4d", "lat", "lng")
    SldSimple_df <- Sld_df[, FieldsToKeep]
    #Save the result
    save(SldSimple_df, file = SldSimpleFileDataPath)
  }
  TRUE
}

#Call the function
simplifyDataset()


#Tabulate values within specified distances of each block group
#==============================================================
#Some factors that affect place type designations are based on the
#the characteristics of the census tracts surrounding each census tracts. These
#values are calculated for each urban area and added to the datasets.

#Function to sum values for block groups within specified distances
#-----------------------------------------------------------------
#' Sum values for block groups within specified distances
#'
#' \code{sumValsInDist} sums specified values for block groups whose centroids
#' are located within a specified distance cutoff.
#'
#' This function sums for each block group, the values of a specified attribute
#' of all the block groups whose centroids are within the specified distance of
#' the the block group. The attributes values summed for the distances are
#' appended to the data frame of SLD values and saved as a new data frame. The
#' new field names are the concatenation of the attributes and the distances
#' over which they are summed.
#'
#' @param DistCutoffs_ a numeric value or vector of values specifying the
#' straight line distance in miles to use as the distance threshold.
#' @param FieldsToSum_ a string or string vector that identifies the fields in
#' Data_df to sum. The elements of FieldsToSum_ correspond to the elements of
#' DistCutoffs_.
#' @param Redo a logical that determines whether the funciton is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
sumValsInDist <- function(DistCutoffs_, FieldsToSum_, Redo = FALSE){
  SldPlusFileDataPath <- "data/SldPlus_df.Rda"
  if (!file.exists(SldPlusFileDataPath) | Redo) {
    if (file.exists("data/SldSimple_df.Rda")) {
      load("data/SldSimple_df.Rda")
    } else {
      return(FALSE)
    }
    if (length(DistCutoffs_) != length(FieldsToSum_)) {
      Message <- "Length of Dist_ argument must equal length of Field_ argument"
      stop(Message)
    }
    Results_mx <- matrix(NA, nrow = nrow(SldSimple_df), ncol = length(DistCutoffs_))
    #Calculate longitude and latitude ranges corresponding to the maximum distance
    #BufferDist is the maximum distance in meters
    BufferDist <- max(DistCutoffs_) * 1609.34
    North <- 0
    South <- -180
    East <- 90
    West <- -90
    SldSimple_df$MinLng <-
      destPoint(as.matrix(SldSimple_df[,c("lng", "lat")]), West, BufferDist)[,1]
    SldSimple_df$MaxLng <-
      destPoint(as.matrix(SldSimple_df[,c("lng", "lat")]), East, BufferDist)[,1]
    SldSimple_df$MinLat <-
      destPoint(as.matrix(SldSimple_df[,c("lng", "lat")]), South, BufferDist)[,2]
    SldSimple_df$MaxLat <-
      destPoint(as.matrix(SldSimple_df[,c("lng", "lat")]), North, BufferDist)[,2]
    #Define function to select dataset within longitude-latitude range
    selectData <- function(Index) {
      IdxSelect <- which(
        (SldSimple_df$lat > SldSimple_df$MinLat[Index]) &
          (SldSimple_df$lat < SldSimple_df$MaxLat[Index]) &
          (SldSimple_df$lng > SldSimple_df$MinLng[Index]) &
          (SldSimple_df$lng < SldSimple_df$MaxLng[Index])
      )
      SldSimple_df[IdxSelect,]
    }
    #Define function to sum a value for block groups within the specified distance
    sumValsInCutoff <- function(DistCutoff, FieldToSum) {
      sum(Selected_df[which(Dist_Bg <= DistCutoff), FieldToSum])
    }
    N <- nrow(Results_mx)
    for(i in 1:N){
      print(round(100 * i / N, 1))
      Selected_df <- selectData(i)
      Dist_Bg <- rdist.earth( SldSimple_df[i,c("lng", "lat")],
                              Selected_df[,c("lng", "lat")], miles=TRUE, R=6371 )
      Results_mx[i,] <- mapply(sumValsInCutoff, DistCutoff = DistCutoffs_,
                               FieldToSum = FieldsToSum_)
    }
    colnames(Results_mx) <- paste(FieldsToSum_, DistCutoffs_, sep = "_")
    SldPlus_df <- SldSimple_df
    #Add the distance tabulations
    for (nm in colnames(Results_mx)) {
      SldPlus_df[[nm]] <- Results_mx[, nm]
    }
    #Remove the minimum and maximum longitude and latitude bounds
    SldPlus_df$MinLng = NULL
    SldPlus_df$MaxLng = NULL
    SldPlus_df$MinLat = NULL
    SldPlus_df$MaxLat = NULL
    #Save the attributed file
    save(SldPlus_df, file = SldPlusFileDataPath)
  }
  TRUE
}

#Call the function
sumValsInDist(DistCutoffs_ <- c(0.25, 1, 2, 5, 10, 15, 0.25, 1, 2, 5, 10, 15),
              FieldsToSum_ <- c(rep("TOTPOP10", 6), rep("EMPTOT", 6)),
              Redo = FALSE)


#Calculate place type measures
#=============================
#' Calculate measures to be used in determining place types
#'
#' \code{calcPlaceTypeMeasures} calculates measures derived from the SLD that
#' are used in the determination of place types.
#'
#' This function calculates several measures that have been found previously
#' to be useful for determining place types. These include:
#' Density: a population density measure derived from the SLD measure named D1D;
#' Diversity1: a mixed use measure derived from the SLD measure named D2A_JPHH;
#' Diversity2: a mixed use measure derived from the SLD measures named HH,
#' E5_RET10, and E5_SVC10;
#' Design1: a measure of network accessibility for non-auto travel modes derived
#' from the SLD measure named D3_amm;
#' Design2: a measure of network accessibility for pedestrian travel derived from
#' the SLD measure named D3_apo;
#' @param Redo a logical that determines whether the funciton is to be rerun
#' even if it has already been run and has produced the resulting dataset. The
#' default value is FALSE.
#' @return A logical that identifies whether the function has completed
#' successfully either because the dataset already exists or because it was
#' produced.
calcPlaceTypeMeasures <- function(Redo = FALSE){
  PlaceInpFileDataPath <- "data/PlaceInp_df.Rda"
  if (!file.exists(PlaceInpFileDataPath) | Redo) {
    if (file.exists("data/SldPlus_df.Rda")) {
      load("data/SldPlus_df.Rda")
    } else {
      return(FALSE)
    }
    PlaceInp_df <- SldPlus_df
    PlaceInp_df$Density <- SldPlus_df$D1D
    PlaceInp_df$Diversity1 <- SldPlus_df$D2A_JPHH
    PlaceInp_df$Diversity2 <-
      (SldPlus_df$E5_RET10 + SldPlus_df$E5_SVC10) / SldPlus_df$HH
    PlaceInp_df$Diversity2[SldPlus_df$HH == 0] <- 0
    PlaceInp_df$Design1 <- SldPlus_df$D3amm
    PlaceInp_df$Design2 <- SldPlus_df$D3apo
    #Save the attributed file
    save(PlaceInp_df, file = PlaceInpFileDataPath)
  }
  TRUE
}

#Call the function
calcPlaceTypeMeasures()

