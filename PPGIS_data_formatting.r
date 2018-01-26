#Formatting and cleaning of spatial data from CONNECT project

#Set working directory
setwd("D:/Box Sync/Arctic/Data")

#Libraries
library(sf)
library(tidyverse)

#Set options


#Load datasets
interviewDF <- readxl::read_excel("Interview data/checked data/TUNDRA_Access_171116_CR.xlsx", sheet = "Interviewee Key")
colheaders <- readxl::read_excel("PPGIS_CONNECT/Raw/List_of_shp_column_headers.xlsx", sheet = "Sheet1")

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
idcodereplace <- lapply(dirlist, function(shpdir) {
  #list shp files in directory
  filelist <- list.files(shpdir, "*.shp$", full.names=TRUE)
  codeDF <- c()
  
  for (currfile in filelist) {
    print(paste0("Starting ", currfile))
    #extract the name
    splitnames <- gsub("-", "_", basename(currfile)) %>% str_split("_")
    #recombine to match the interviewee column in .xlsx
    if(is.numeric(splitnames[[1]][3])) {
    idfromshpname <- paste(splitnames[[1]][1], sprintf("%03d", as.numeric(splitnames[[1]][3])), sep="_")
    } else {
      m <- gregexpr('[0-9]+',splitnames[[1]][3] ) #if there are letters in the shp name eg A1 extract only the numbers eg. 01
      splitnames[[1]][3] <- regmatches(splitnames[[1]][3], m)
    idfromshpname <- paste(splitnames[[1]][1], sprintf("%03d", as.numeric(splitnames[[1]][3])), sep="_")  
    }
    print(idfromshpname)
    #extract the idcode to match
    idcode <- interviewDF[which(interviewDF$Interviewee==idfromshpname), 1:2]
    codeDF <- rbind(codeDF, c(idcode, basename(currfile)))
    
    #load the shp
    currshp <- read_sf(currfile)
    
    #replace code
    currshp[, "Id"] <- idcode[1]
    
    #save shp
    st_write(currshp, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/", basename(currfile)))
  }
  return(codeDF)
} ) 
idcodereplace <- do.call(rbind, idcodereplace)
write.csv(idcodereplace, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/checkidcodereplacement.csv"))

#Merge shps was done in ArcGIS because it is quicker
#reproject to lambert azimuthal - this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

###############
#Canada
###############
#These are in GCS_North_America_1983 which is EPSG:4269

#filelist <- list.files("PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Canada", pattern="append", full.names = TRUE) 
#filelist <- grep(".shp$", filelist, value=TRUE)
#shpList <- lapply(filelist, function(currfile) read_sf(currfile))
#canadaShp <- do.call(rbind, shpList)
#st_write(canadaShp, paste0("PPGIS_CONNECT/Processed/CONNECT/Canada/"))

#Merge shps was done in ArcGIS because it is quicker
#reproject to lambert azimuthal - this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

#reformat columns
canadaShp <- read_sf("PPGIS_CONNECT/Processed/Canada/Canada_allpolygons_laea.shp")

###############
#Alaska
###############
#reproject to lambert azimuthal - this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

# Read the feature class
alaskaShp <- read_sf(dsn="PPGIS_CONNECT/Processed/CONNECT/Alaska.gdb",layer="Alaska_allpolygons_laea")

#rename column headers
names(alaskaShp)[1:49] <- colheaders$Alaska_rename[1:49]


#split the polygon id from column UniqueID2 (format = 6 digit interviewee id 2 digit polygon id). Add 1 because they start at zero
alaskaShp$AA_Number <- (as.numeric(do.call(rbind, lapply(alaskaShp$UniqueID2, function (x) str_split(x, "_")[[1]][3]))) +1)

#split cabin and camp

#what to do with duplicated columns?

#pull I_ who they go with from notes

#N_Nights from Length column

#N_Length from Frequency and from Notes

#N_Season from Frequency and Notes

#G what they do from Notes

#H how they travel from Notes



