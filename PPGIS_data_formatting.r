#Formatting and cleaning of spatial data from CONNECT project

#Set working directory
setwd("D:/Box Sync/Arctic/Data")

#Libraries
library(sf)
library(tidyverse)

#Set options


#Load datasets
interviewDF <- readxl::read_excel("Interview data/checked data/TUNDRA_Access_171116_CR.xlsx", sheet = "Interviewee Key")

###############
#Norway
###############
#Norway processed manually

###############
#Russia
###############
#Russia data column names are good, just need to add interviewee ID to each row, and merge all the shps.
#Merge shps was done in ArcGIS because it is quicker
#reproject to lambert azimuthal - this was done in ArcGIS, I don't trust R to do it correctly


dirlist <- list("PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Russia/Murmansk/Shape", 
                "PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Russia/Taimyr/Shape",
                "PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Russia/Yamal/Yamal-200/Shape",
                "PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Russia/Yamal/Yamal-500/Shape")

#extract the interviewee id from the shp file name
#set up fn
namefun <- function(shpdir) {
  filelist <- list.files(shpdir, "*.shp$") %>% purrr::map(function(currfile) gsub("-", "_", currfile)) %>% str_split("_")
  filelist <- do.call(rbind, splitnames)
  return(filelist)
}
#Extract the interviewee name from the shapefile name
idcodereplace <- lapply(dirlist, shpdir){
  #list shp files in directory
  filelist <- list.files(shpdir, "*.shp$", full.names=TRUE)
  codeDF <- c()
  
  for (currfile in filelist) {
    #extract the name
    splitnames <- gsub("-", "_", basename(currfile)) %>% str_split("_")
    #recombine to match the interviewee column in .xlsx
    idfromshpname <- paste(splitnames[[1]][1], sprintf("%03d", as.numeric(splitnames[[1]][3])), sep="_")
    
    #extract the idcode to match
    idcode <- interviewDF[which(interviewDF$Interviewee==idfromshpname), 1:2]
    codeDF <- rbind(codeDF, c(idcode, basename(currfile)))
    
    #load the shp
    currshp <- read_sf(currfile)
    
    #replace code
    currshp$ID <- idcode[1]
    
    #save shp
    st_write(currshp, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/", basename(currfile)))
  }
  return(codeDF)
}  

write.csv(idcodereplace, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/checkidcodereplacement.csv")

#Merge shps was done in ArcGIS because it is quicker
#reproject to lambert azimuthal - this was done in ArcGIS (check because I don't trust R to do it correctly

###############
#Canada
###############


###############
#Alaska
###############
 




