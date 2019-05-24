#Formatting and cleaning of spatial data from CONNECT project

#Set working directory
setwd("D:/Box Sync/Arctic/Data")

#Libraries
library(sf)
library(tidyverse)

#Set options
options(stringsAsFactors = FALSE)

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
#reproject to lambert azimuthal - this was done in ArcGIS, I don't trust R to do it correctly

#1. Fill in the interviewee ID column ####

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
    st_write(currshp, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/intermediate/", basename(currfile)))
  }
  return(codeDF)
} ) 
idcodereplace <- do.call(rbind, idcodereplace)
write.csv(idcodereplace, paste0("PPGIS_CONNECT/Processed/CONNECT/Russia/checkidcodereplacement.csv"))

# 2. Merge shps #### 
#they have different numbers of columns so we do a bit of pre-processing
procfiles <- list.files("PPGIS_CONNECT/Processed/CONNECT/Russia/intermediate/", "*.shp$", full.names=TRUE)
# make a list of all the column headers in the .shps
coldf <- data.frame()
currnames <- c()
for(i in procfiles) {
  x <- read_sf(i)
  currrow <- data.frame(as.character(basename(i)), ncol(x), nrow(x))
  coldf <- rbind(coldf, currrow)
  currnames <- append(currnames, names(x))
}
uniquecols <- unique(currnames)

procfiles <- list.files("PPGIS_CONNECT/Processed/CONNECT/Russia/intermediate/", "*.shp$", full.names=TRUE)
# make all the shps have the same column headers
tempfileList <- lapply(procfiles, function (x) {
  currshp <-  read_sf(x)
  #pull out the missing columns headers
  fillcols <- uniquecols[which(!uniquecols %in% names(currshp))]
  #append these columns (filled with NAs) to the current shapefile
  filldf <- matrix(data=as.character(NA), nrow=nrow(currshp), ncol=length(fillcols), dimnames=list(NULL, fillcols))
  currshp_full <- cbind(currshp, filldf)
  #they all have different crs (all stereographic polar, but lon ranges from 35 to 100), reproject
  currshp_full <- st_transform(currshp_full, "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=50 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  return(currshp_full)
})

#finally merge the .shps
#For polygons ####
polids <- which(grepl("*pol.shp$", procfiles))
russiashp_pol <- do.call(rbind, tempfileList[polids])
write_sf(russiashp_pol, "PPGIS_CONNECT/Processed/CONNECT/Russia/output/Russia_allpolygons_stereopolarWGS84.shp")

#finally merge the .shps
#For lines ####
linids <- which(grepl("*lin.shp$", procfiles))
russiashp_lin <- do.call(rbind, tempfileList[linids])
write_sf(russiashp_lin, "PPGIS_CONNECT/Processed/CONNECT/Russia/output/Russia_alllines_stereopolarWGS84.shp")

#3. reproject to lambert azimuthal ####
#this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

###############
#Canada
###############
#These are in GCS_North_America_1983 which is EPSG:4269

#1. Merge was done in ArcGIS, but here is some code to do it in R ####
#filelist <- list.files("PPGIS_CONNECT/Raw/TUNDRA_Shapefiles_Canada", pattern="append", full.names = TRUE) 
#filelist <- grep(".shp$", filelist, value=TRUE)
#shpList <- lapply(filelist, function(currfile) read_sf(currfile))
#canadaShp <- do.call(rbind, shpList)
#st_write(canadaShp, paste0("PPGIS_CONNECT/Processed/CONNECT/Canada/"))

#2. reproject to lambert azimuthal ####
#this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

#3. reformat columns ####
#load file
canadaShp <- read_sf(dsn="PPGIS_CONNECT/Processed/CONNECT/Canada.gdb", "Canada_allpolygons_laea")

#rename columns
names(canadaShp)[which(names(canadaShp) == "Id")] <- "AA_Id"
names(canadaShp)[which(names(canadaShp) == "UniqueID")] <- "AA_Navn"

#split the polygon id from column UniqueID2 (format = 6 digit interviewee id 2 digit polygon id). Add 1 because they start at zero 
canadaShp$AA_Number <- (as.numeric(do.call(rbind, lapply(canadaShp$UniqueID2, function (x) str_split(x, "_")[[1]][3]))) +1)

#Split cabin & camp 
canadaShp <- canadaShp %>%
              mutate(L_Cabin = case_when(Camp_Cabin %in% c("Cabin", "Camp and Cabin", "Cabin/Camp") ~ 1),
                     L_Camp = case_when(Camp_Cabin %in% c("Camp", "Camp and Cabin", "Cabin/Camp") ~ 1))

#split Harvest ID 
#unique(canadaShp$HarvestID)
#write.csv(unique(canadaShp$HarvestID), "PPGIS_CONNECT/Raw/Reclass_of_harvestIDs_canada.csv", #row.names=FALSE)
#I went through these manually and renamed them according to the Norwegian letter convention eg. A_Caribou
harvestcode <- read.csv("PPGIS_CONNECT/Raw/Reclass_of_harvestIDs_canada.csv")
canadaShp <- merge(canadaShp, harvestcode, by.x="HarvestID",by.y="harvestID", all.x=TRUE)

canadaShp2 <- canadaShp %>% 
              mutate(colval=1) %>%
              spread(New_col_name, value = colval) 

#save 
canadaShp2 <- canadaShp2[, -which(names(canadaShp2) %in% c("Shape_Length","Shape_Area", "V1"))] #drop unnecesary columns
write_sf(canadaShp2, "PPGIS_CONNECT/Processed/CONNECT/Canada/Canada_allpolygons_laea_reformated.shp")


#pull I_ who they go with from notes
#N_Nights from Length column
#N_Length from Frequency and from Notes
#N_Season from Frequency and Notes
#G what they do from Notes
#H how they travel from Notes


###############
#Alaska
###############
#reproject to lambert azimuthal - this was done in ArcGIS (checked preserve shape; because I don't trust R to do it correctly)

# Read the shapefile
alaskaShp <- read_sf(dsn="PPGIS_CONNECT/Processed/CONNECT/Alaska.gdb",layer="Alaska_allpolygons_laea")

#rename column headers based on excel spreadsheet
names(alaskaShp)[1:49] <- colheaders$Alaska_rename[1:49]

#extract the idcode to match
alaskaShp$AA_Id <- unlist(lapply(alaskaShp$AA_Navn, function(x) return(interviewDF$ID[which(interviewDF$Interviewee==x)])))

#split the polygon id from column UniqueID2 (format = 6 digit interviewee id 2 digit polygon id). Add 1 because they start at zero
alaskaShp$AA_Number <- (as.numeric(do.call(rbind, lapply(alaskaShp$UniqueID2, function (x) str_split(x, "_")[[1]][3]))) +1)

#save
alaskaShp <- alaskaShp[, -which(names(alaskaShp) %in% c("Shape_Length","Shape_Area"))] #drop unnecesary columns
write_sf(alaskaShp, "PPGIS_CONNECT/Processed/CONNECT/Alaska/Alaska_allpolygons_laea_reformated.shp")


# cabin from notes
#what to do with duplicated columns?
#pull I_ who they go with from notes
#N_Nights from Length column
#N_Length from Frequency and from Notes
#N_Season from Frequency and Notes
#G what they do from Notes
#H how they travel from Notes



