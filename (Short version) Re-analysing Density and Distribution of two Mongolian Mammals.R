# (SHORT VERSION) RE-ANALYSING DENSITY AND DISTRIBUTION OF TWO MONGOLIAN MAMMALS RESEARCH PROJECT ####


# First, I need to install few packages to perform all the analysis: 


install.packages(c("knitr", "DataCombine", "fasttime", "rgdal", "data.table", "dsm", "ggplot2", "suncalc", 
                   "RCurl", "stringi", "dplyr", "Distance", "tidyr", "captioner", "maptools", "tweedie",
                   "splancs", "readxl"))


require(knitr)

require(DataCombine)

require(fasttime)

require(rgdal)

require(data.table)

require(dsm)

require(ggplot2)

require(suncalc)

require(RCurl)

require(stringi)

require(dplyr)

require(Distance)

require(tidyr) 

require(captioner)

require(maptools)

require(tweedie)

require(splancs)

require(readxl)

options(stringsAsFactors=FALSE)


# IN CASE YOU DON'T WANT TO RUN THE WHOLE CODE, BUT ONLY TO SEE THE RESULTS OF IT ####

load("re-analysing 15-04-2022.RData")


# In this Rscript, I skipped all the data wrangling of the "metadata" file (the one with information of each and every 
# images taken by the first batch of 90 Camera Trap deployed in the GGASPA). This, due to the huge weight of the metadata
# datafile that makes impossible to host it in my Github page.

# Also, I skipped the part of the calculation of sunlight times (to decide which CT image will be considered part of the
# dataset or not), and the distance calculation from each animal spotted on images to the CT (due to Marcus' Github link 
# to relevant functions is not working anymore).

# From now on, it's the same script that I sent you before as a WeTransfer link.


# COORDINATES CONVERSION IN GGASPA DEPLOYMENT, COVARIATES JUNE AND SURVEY AREA DATA ####


# Now I'll work with the Camera Trap deployment and Covariates June datasheets.

# I need to generate a dataset with sunlight times for each date by Camera Trap (each one with a specific
# latitude and longitude coordinates) that will allow me to select only Camera Trap's timelapse images that
# were taken during sunlight hours in the metdat_redo file:

depdat <- read.csv("GGASPA_deployment_data.csv", header = TRUE, stringsAsFactors = FALSE)


# I will load the Covariates June file which has UTM coordinates where CT were actually placed, instead of
# the coordinates from the Camera Trap deployment datasheet, which are the ones assigned for each CT in the desk.
# I will paste the former coordinates into depdat.

covjune <- read_xls("covariates_June.xls", col_names = TRUE)

str(covjune)
# Camera.ID       : num [1:88] 1 2 3 4 5 6 7 8 9 10 ...
# Easting         : num [1:88] 488274 509570 531084 294944 313470 ...
# Northing        : num [1:88] 4723329 4723624 4723299 4752237 4755585 ...
# Slope           : num [1:88] 1 1 1 1 1 1 1 1 1 1 ...
# Aster           : num [1:88] 1223 1179 1156 1236 1446 ...
# NVDI            : num [1:88] 0.0334 0.0427 0.0364 0.0534 0.027 ...
# Vegetation.class: num [1:88] 34 18 34 33 18 33 29 33 18 34 ...
# VRM             : num [1:88] 0.00721 0.00116 0.00115 0.0015 0.0037 ...
# Cost.Distance   : num [1:88] 4251 2594 3616 2779 2821 ...


# As I know from the previous processing, "point_id" of each Camera Trap in metdat is just the first portion 
# of their ID, without the "ZSL or W", just the number. So, "point_id" = R1-ZSL76 corresponds to the 
# column "point" = 1 in depdat and to "Camera.ID" = 1 in covjune, and RG86-W5 corresponds 
# to "point" and "Camera.ID" = 86.

# Now, I will need to add character values to each number of "point" and "Camera.ID" column from depdat and covjune,
# and also change both column names to "point_id", because I will need to match these files with metdat 
# to paste on it certain columns 


# An exploration of the nature of "depdat$point" and "covjune$Camera.ID" values

class(depdat$point) # they are integers
str(depdat$point) # 90 CTs

class(covjune$Camera.ID) # they are numeric
str(covjune$Camera.ID) # 88 Cts


# Adding up the letter "R" to each of the values in depdat$point and covjune$Camera.ID

depdat$point <- sub("^", "R", depdat$point)
covjune$Camera.ID <- sub("^", "R", covjune$Camera.ID)

# Checking if it worked:

head(depdat$point)
tail(depdat$point)

head(covjune$Camera.ID)
tail(covjune$Camera.ID)


# There are 3 Camera Traps that have another letter on their ID: RG86-W5, RG87-ZSL23, and RG90-ZSL25, 
# so I have to add "G" to their character string:

depdat$point <- sub("R86", "RG86", depdat$point)
depdat$point <- sub("R87", "RG87", depdat$point)
depdat$point <- sub("R90", "RG90", depdat$point)

covjune$Camera.ID <- sub("R86", "RG86", covjune$Camera.ID)
covjune$Camera.ID <- sub("R87", "RG87", covjune$Camera.ID)
covjune$Camera.ID <- sub("R90", "RG90", covjune$Camera.ID)


# Checking if it worked:

tail(depdat$point)
tail(covjune$Camera.ID)


# Then, I rename the "point" and "Camera.ID" columns in depdat and covjune files to "point_id"

names(depdat)[names(depdat) == "point"] <- "point_id"
names(depdat)

names(covjune)[names(covjune) == "Camera.ID"] <- "point_id"
names(covjune)


# Now, I need to rename covjune's UTM coordinates columns Easting and Northing (the exact CT location in the field) 
# to not be confounded with those of depdat (CT location assigned in the desk)

names(covjune)[match(c("Easting", "Northing"), names(covjune))] <- c("cov_utmx", "cov_utmy")
names(covjune)


# In order to use the "suncalc" package to calculate sunlight times and then select only Camera Trap's timelapse 
# images that were taken during sunlight hours in metdat file, I need to transform "cov_utmx" and "cov_utmy" 
# from covjune file to longitude and latitude coordinates, to then paste them on depdat file

# * UTM Easting or X = longitude ; UTM Northing or Y = latitude (in depdat and covjune files)

# First, I need to extract zone column from depdat and paste it in covjune by point_id

covzone <- subset(depdat)[c("point_id", "zone")]
i <- match(covjune$point_id, covzone$point_id)
covjune$zone <- covzone$zone[i]
rm(covzone)
rm(i)


# And then, run this code for utm coordinates transformation to longitude/latitude

zones <- unique(covjune$zone)

utmdat <- lapply(zones, function(z){
  subdat <- subset(covjune, zone == z)[, c("cov_utmx", "cov_utmy")]
  proj <- CRS(paste0("+proj=utm +zone=", z, "+datum=WGS84"))
  SpatialPoints(subdat, proj)
})

latlondat <- lapply(utmdat, spTransform, CRS("+proj=longlat +datum=WGS84"))

latlondat <- rbindlist(lapply(latlondat, as.data.frame))


# Here I backtransformed UTM coordinates on covjune file to long/lat coordinates by each point_id (CT) in 
# the corresponding rows of "latlondat"

# Renaming cov_utmx and cov_utmy columns in latlondat with long and lat

names(latlondat)[match(c("cov_utmx", "cov_utmy"), names(latlondat))] <- c("long", "lat")
names(latlondat)


# Then, I will bind both vectors by columns into the covjune file: 

covjune <- cbind(as.data.frame(latlondat), covjune)


# Now I need to rename depdat columns called "long" and "lat" to their real projection, which is UTM coordinates, 
# to not be confounded with the new longitude/latitude coordinates from "latlondat". I will name it "utmx" 
# (UTM long) and "utmy" (UTM lat).

names(depdat)[match(c("long", "lat"), names(depdat))] <- c("utmx", "utmy")
names(depdat)


# GGASPA PROCESSING ####

# Now I will load my GGASPA survey area shapefile (already in UTM Easting or X and Northing or Y coordinates) into R 
# environment. I will transform its UTM coordinates to distance-based x,y coordinates in metres (m), using a 
# centroid coordinate to extrapolate all the remaining, using this coordinate format in the plotting stage


# GGASPA <- readOGR(dsn=":/.../.../.../GGASPA")     # adapt it at your convenience

GGASPA <- readOGR(dsn = "D:/MSc SILWOOD 2019/MAIN PROJECT/REDO RESEARCH PROJECT 2020/GGASPA")


# Now I will transform GGASPA SpatialPolygonsDataFrame class to a dataframe:

GGASPA <- data.frame(GGASPA@polygons[[1]]@Polygons[[1]]@coords)

# and change its column names to UTM X and Y coordinates, their real units:

names(GGASPA) <- c("utmx", "utmy")
names(GGASPA)


# I need first to backtransform the UTM coordinates on GGASPA to Longitude and Latitude, to then transform it 
# to distance-based x,y coordinates:


GGASPA2 <- SpatialPoints(GGASPA)

proj4string(GGASPA2) <- CRS("+init=epsg:32647")    # project the dataframe on UTM 47N

GGASPA2 <- spTransform(GGASPA2, CRS("+proj=longlat +datum=WGS84"))  # project the dataframe on LongLat coordinates

GGASPA2 <- data.frame(long=GGASPA2@coords[, 1], lat=GGASPA2@coords[, 2]) 

GGASPA <- cbind(GGASPA, GGASPA2)

rm(GGASPA2)


# Adding the distance-based x,y coordinates in metres (m) to GGASPA file, which I will use in the plotting stage

lon0 <- mean(GGASPA$long)
lat0 <- mean(GGASPA$lat)
sa.tmp <- latlong2km(GGASPA$long, GGASPA$lat, lon0 = lon0, lat0 = lat0)
GGASPA$xkm <- sa.tmp$km.e
GGASPA$ykm <- sa.tmp$km.n
GGASPA$x <- sa.tmp$km.e*1000
GGASPA$y <- sa.tmp$km.n*1000

rm(sa.tmp)


# Then, I will calculate and add a distance-based x,y coordinates in metres (m) for covjune file, using the 
# centroid of the GGASPA file as a projection, for then plotting CT locations on the GGASPA.

sa.tmp <- latlong2km(covjune$long, covjune$lat, lon0 = lon0, lat0 = lat0)
covjune$xkm <- sa.tmp$km.e
covjune$ykm <- sa.tmp$km.n
covjune$x <- sa.tmp$km.e*1000
covjune$y <- sa.tmp$km.n*1000

rm(sa.tmp)


# The code below generates a Figure which shows the GGASPA survey area with the CT locations from covjune file overlaid
# (the ones which were actually deployed, at different locations from the ones desk-assigned as in depdat file)

# plotting options

gg.opts <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.text = element_text(size = 14), 
                 axis.title = element_text(size = 14, face = "bold"))

ctloc <- qplot(data=GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
               ylab = "ykm", xlab = "xkm", alpha=I(0.7))
ctloc <- ctloc + coord_equal()
ctloc <- ctloc + geom_point(aes(xkm, ykm, group = point_id),color = "red", data = covjune)  
ctloc <- ctloc + geom_text(aes(label = point_id), hjust=-0.3, vjust=-0.2, size=3, data = covjune)
ctloc <- ctloc + gg.opts + ggtitle("CT locations on GGASPA")
ctloc <- ctloc + theme (plot.title = element_text(size = 23))
print(ctloc)


# I better used here the study area map with CTs overlaid from covjune, because it is more accurate on the CT location,
# and also on the actual number of CTs deployed (CT N°0 and N°78 were never installed, N°80 never existed, hence the
# difference in total CT numbers: 90 for depdat vs 88 for covjune).


## FROM NOW ON, I WILL USE PREVIOUS FILE FROM RESEARCH PROJECT TO CONTINUE WITH THE ANALYSIS. 
## I WILL PASTE COVARIATES AND COORDINATES FROM COVJUNE_DIST FILE TO DISTDATA_GAZ AND DISTDATA_KUL, 
## IN ORDER TO USE THEM AT DETECTION FUNCTION STAGE


# LOAD DISTDATA_GAZ, DISTDATA_KUL AND DEPDAT_DIST, AND PASTE COVARIATES FROM THE LATER TO THE FORMER ONES

depdat_dist <- read.csv("depdat_dist.csv", header = TRUE, stringsAsFactors = FALSE)

distdata_gaz <- read.csv("distdata_gazelle.csv", header = TRUE, stringsAsFactors = FALSE)

distdata_kul <- read.csv("distdata_kulan.csv", header = TRUE, stringsAsFactors = FALSE)


## I need to add "point_id" column to distdata_gaz and distdata_kul, which doesn't have this

# For Gazelle:

point_id <- strsplit(distdata_gaz$object, "-", fixed=TRUE)

point_id <- unlist(lapply(point_id, function(x) x[1]))

point_id <- stri_sub(point_id, -11, -9)

distdata_gaz <- cbind(as.data.frame(point_id), distdata_gaz)

rm(point_id)

# For Kulan:

point_id <- strsplit(distdata_kul$object, "-", fixed=TRUE)

point_id <- unlist(lapply(point_id, function(x) x[1]))

point_id <- stri_sub(point_id, -11, -9)

distdata_kul <- cbind(as.data.frame(point_id), distdata_kul)

rm(point_id)


# Now I need to add a new column on covjune file about the count of animals per CT for whom distance 
# was estimated:

gazelle.tab <- table(distdata_gaz$point_id)
gazelle.tab

kulan.tab <- table(distdata_kul$point_id)
kulan.tab

covjune$gazelle_count <- covjune$kulan_count <- 0

covjune$gazelle_count[match(names(gazelle.tab), covjune$point_id)] <- gazelle.tab

covjune$kulan_count[match(names(kulan.tab), covjune$point_id)] <- kulan.tab


# Now, I need to calculate an appropriate Effort (number of timelapse images per CT, scaled up by CT's Field of View)
# and add it up as a new "Effort" (case-sensitive) column to my depdat, covjune and distdata (gazelle and kulan) files
# to correspond with the required fields of those datafiles in the spatial analysis stage.

## I DON'T HAVE AN "EFFORT" COLUMN FOR DEPDAT_DIST FILE, AND THIS IS NEEDED IN THE DETECTION FUNCTION STAGE, BUT
## DISTDATA_GAZ AND DISTDATA_KUL HAS THIS COLUMN (STILL DON'T KNOW HOW IT WENT FROM DEPDAT TO DISTDATA)


# Adding Effort column to the segment.data file (covjune) and distdata files (for a reason I can't explain, 
# distdata files for gazelle and kulan have the "raw" effort, before being scaled up by particular FOV of Camera Traps), 
# but Effort of R76 is different than in metdat_effort, so I'm pasting  Effort column by point_id again on distdata files

# So, I will calculate the sampling effort:

cteffort <- read.csv("cteffort.csv", header = TRUE, stringsAsFactors = FALSE)


# And now, I will add "istimelapse" as "Effort" column to my covjune and distdata files:

distdata_gaz$Effort <- cteffort$istimelapse[match(distdata_gaz$point_id, cteffort$point_id)]

class(cteffort$istimelapse) # "integer"
class(distdata_gaz$Effort)  # "integer"

distdata_kul$Effort <- cteffort$istimelapse[match(distdata_kul$point_id, cteffort$point_id)]

class(distdata_kul$Effort)  # "integer"

covjune$Effort <- cteffort$istimelapse[match(covjune$point_id, cteffort$point_id)]  # needed for DSM stage

class(covjune$Effort)  # "integer"


# Here, the sampling effort it is not just the number of timelapse images taken during sunlight hours per Camera
# Trap. The definition of effort needs to take account of Camera Trap "field of view", as a proportion of a 
# full circular area. In this particular case for the used Camera Trap models, the sampling effort is about 24% of 
# a circular area of 1 (1 image) I KNOW THAT FROM THE R SCRIPT DELIVERED FOR MY RESEARCH PROJECT


distdata_gaz$Effort <- 0.24 * distdata_gaz$Effort

sum(distdata_gaz$Effort == "NA")  # 0


distdata_kul$Effort <- 0.24 * distdata_kul$Effort

sum(distdata_kul$Effort == "NA")  # 0


covjune$Effort <- 0.24 * covjune$Effort

covjune <- subset(covjune, !is.na(Effort))
# because NA values appeared in covjune$Effort column, due to certain CT deployed yield no images

# My covjune file now is composed by 77 CT (the ones yielded images)


# PASTING RELEVANT COLUMNS FROM COVARIATESFEB AND DEPDAT_DIST FILES TO COVJUNE ####
# (SHOULD NOT BE NECESSARY TO DO IT IN R, MAYBE PASTING THEM BEFORE PROCESSING IT IN R?)


# Loading CovariatesFEB file (same as covariates_June, but with more CT covariates):
# need to select and copy relevant cells from the file first before running next code


covfeb <- read_xls("CovariatesFEB.xls", col_names = TRUE)


# An exploration of the nature of "covfeb$ID" values

class(covfeb$ID) # they are numeric
str(covfeb$ID) # 86 CTs

# Adding up the letter "R" to each of the values in covfeb$ID

covfeb$ID <- sub("^", "R", covfeb$ID)
head(covfeb$ID)
tail(covfeb$ID)

# There are 3 Camera Traps that have another letter on their ID: RG86-W5, RG87-ZSL23, and RG90-ZSL25, 
# so I have to add "G" to their character string:

covfeb$ID <- sub("R86", "RG86", covfeb$ID)
covfeb$ID <- sub("R87", "RG87", covfeb$ID)
covfeb$ID <- sub("R90", "RG90", covfeb$ID)

# Checking if it worked:

tail(covfeb$ID)
# RG90 doesn't exists in covfeb


# Now, I rename the "ID" column in covfeb file to "point_id"

names(covfeb)[names(covfeb) == "ID"] <- "point_id"
names(covfeb)

# Then I will paste "CostPath" column from covfeb to covjune

covjune$CostPath <- covfeb$CostPath[match(covjune$point_id, covfeb$point_id)]

# Now I will paste "Elevation" column from depdat file to covjune

covjune$elev <- depdat$elev[match(covjune$point_id, depdat$point_id)]

str(covjune)
# all relevant covariates are "numeric" or "integer"


# SELECTION OF THE DETECTION FUNCTION ####

# Exploration of Distance data ####

# First, I will have a look at the frequency distributions of distances from CT to detected animals of both
# focal species, and its relationship with the count numbers per CT.


# Histogram of frequency distribution of recorded distances

par(mfrow = c(1, 2))

hist(distdata_gaz$distance, ylim = c(0, 25), xlab = "Distance of detection (metres)", 
     main = "Goitered gazelle", col = "white")

hist(distdata_kul$distance, ylim = c(0, 15), xlab = "Distance of detection (metres)", 
     main = "Mongolian Kulan", col = "white")

dev.off()

summary(distdata_gaz$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.924  25.907  51.331  49.611  68.866 120.545

summary(distdata_kul$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.43   86.89  146.06  150.51  190.33  362.28

# Distance of detection for Gazelle ranged from 7-120m with peak detections at an average of 50m; for Kulan
# ranged from 40-362m with peak at 150m. Funny that peak detections for Kulan were pass at maximum range of 
# detection for Gazelle


# I am able now to plot the count of animals of both species detected in timelapse images per CT 
# location, for whom the distances were calculated

# Plot of count detections of animals per CT location for Goitered gazelle:

gazcount <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
                  ylab = "ykm", xlab = "xkm", alpha = I(0.7))
gazcount <- gazcount + gg.opts + ggtitle("Goitered gazelle detections per CT location on GGASPA")
gazcount <- gazcount + coord_equal() 
gazcount <- gazcount + geom_point(aes(xkm, ykm, size = gazelle_count), data = covjune, colour = "red", alpha = I(0.7))
gazcount <- gazcount + labs(x= "xkm", y= "ykm", size = "Gazelle Detections")
gazcount <- gazcount + theme(legend.title = element_text(size = 21), 
                             legend.text = element_text(size = 19),  
                             plot.title = element_text(size = 23))
print(gazcount)


# Plot of count detections of animals per CT location (with CT ID) for Goitered gazelle (in case seems more informative):

gazcountct <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
                     ylab = "ykm", xlab = "xkm", alpha = I(0.7))
gazcountct <- gazcountct + gg.opts + ggtitle("Goitered gazelle detections per CT location on GGASPA")
gazcountct <- gazcountct + coord_equal() 
gazcountct <- gazcountct + geom_point(aes(xkm, ykm, size = gazelle_count), data = covjune, 
                                        colour = "red", alpha = I(0.7))
gazcountct <- gazcountct + labs(x= "xkm", y= "ykm", size = "Gazelle Detections")
gazcountct <- gazcountct + geom_text(aes(label = point_id), hjust=-0.5, vjust=-0.4, size=3, data = covjune)
gazcountct <- gazcountct + theme(legend.title = element_text(size = 21), 
                                   legend.text = element_text(size = 19),  
                                   plot.title = element_text(size = 23))
print(gazcountct)


# Plot of count detections of animals per CT location for Kulan:

kulcount <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
                  ylab = "ykm", xlab = "xkm", alpha = I(0.7))
kulcount <- kulcount + gg.opts + ggtitle("Mongolian Kulan detections per CT location on GGASPA")
kulcount <- kulcount + coord_equal() 
kulcount <- kulcount + geom_point(aes(xkm, ykm, size = kulan_count), data = covjune, colour = "red", alpha = I(0.7))
kulcount <- kulcount + labs(x= "xkm", y= "ykm", size = "Kulan Detections") 
kulcount <- kulcount + theme(legend.title = element_text(size = 21), 
                             legend.text = element_text(size = 19),  
                             plot.title = element_text(size = 23))
print(kulcount)


# Plot of count detections of animals per CT location (with CT ID) for Kulan (in case seems more informative):

kulcountct <- qplot(data = GGASPA, x = xkm, y = ykm, geom = "polygon", fill = I("lightblue"),
                    ylab = "ykm", xlab = "xkm", alpha = I(0.7))
kulcountct <- kulcountct + gg.opts + ggtitle("Mongolian Kulan detections per CT location on GGASPA") 
kulcountct <- kulcountct + coord_equal() 
kulcountct <- kulcountct + geom_point(aes(xkm, ykm, size = kulan_count), data = covjune, 
                                      colour = "red", alpha = I(0.7))
kulcountct <- kulcountct + labs(x= "xkm", y= "ykm", size = "Kulan Detections") 
kulcountct <- kulcountct + geom_text(aes(label = point_id), hjust=-0.5, vjust=-0.4, size=3, data = covjune)
kulcountct <- kulcountct + theme(legend.title = element_text(size = 21), 
                                 legend.text = element_text(size = 19),  
                                 plot.title = element_text(size = 23))
print(kulcountct)


# Estimating the Detection Function in Conventional Distance Sampling (CDS) ####

# I will use the ds() function in the package Distance to fit the detection function to the estimated distances
# in CDS, that means only using coordinates to explain spatial distributions of focal species.

# I need to try different existing keys (Half-normal and Hazard-rate with no adjustment terms, 
# Uniform with adjustment terms) to model the detection function that will allow me to estimate 
# density and abundance of both focal species. Comparing their AIC/QAIC/c^ values, I will be able to select 
# the one that best fits their distance data:


# I tried to add, even extract from GGASPA the study area to get a value of density and abundance from ds() function
# and contrast it to the values I will get from DSM stage for both species, but nothing worked so far,
# so I skipped that chunk of code


# Looking for the detection function in CDS that best fits Goitered gazelle distance data ####

# I will try 3 key functions (HN, HR and Uniform), 2 of them that can go without adjustment terms (HN, HR), 
# and their combinations with adjustment terms that "get along well with them" 
# (as said by Len Thomas on his lecture, find out a reference for that... maybe Buckland et al 2015?), 

# Combinations are: Half normal-Hazard rate without adjustment terms (2 combinations, can't use Uniform 
# without adjustment terms) Unif-cos, HN-herm, HN-cos, HR-poly (4 combinations); Total 6 combinations


# DF in CDS Gaz HN no adjustment no trunc

gaz.hn <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", adjustment = NULL)

check.mono(gaz.hn$ddf)
# TRUE

summary(gaz.hn)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 736.1109 
# Chi-square P = 0.22318 with 7 degrees of freedom      # way up to > 0.05, so retain! our Ho is that our DF model
# is good enough to fit real data
0.22318/7
# 0.03188286 values >1 mean overdispersion (from little to large, depends on the number), so we're good!

# *Chi-square p-value obtained from the ddf.gof() function below

# With the following code I will generate a plot of the fitted detection function and quantile-quantile plot:
# pdf: probability density function. Plotting option only available for point transects, shows how well 
# fitted is the detection function key to the raw distance data

par(mfrow = c(1, 2))
plot(gaz.hn, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz.hn")
ddf.gof(gaz.hn$ddf, main = "gaz.hn")
dev.off()

# Checking distribution of probability of detections (Pa) in case truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(gaz.hn, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3    80          1
# 0.3 - 0.4     0          0
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.22 - 0.22

# Detection function seems well fitted (all observation are Pa >= 0.2)

plot(gaz.hn, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz.hn")

# in PDF plot (observed detection distances scaled up by expected detection distances from the model) 
# number of detections fell rapidly beyond 95m, but in DF plot detection probability is < 0.15 at 80m more or less
# I did right truncation at 95m before (and results improved in magnitude and precision) but now I will try with
# right truncation at 80m


# DF in CDS Gaz HR no adjustment no trunc

gaz.hr <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hr", adjustment = NULL)

check.mono(gaz.hr$ddf)
# TRUE

summary(gaz.hr)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 741.363 
# Chi-square P = 0.013635 with 6 degrees of freedom           # < 0.05 so don't retain!
0.013635/6
# 0.0022725 no way of overdispersion

par(mfrow = c(1, 2))
plot(gaz.hr, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz.hr")
ddf.gof(gaz.hr$ddf, main = "gaz.hr")
dev.off()

p_dist_table(gaz.hr, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4    80          1
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.38 - 0.38

# DF well fitted (Pa >= 0.3)

plot(gaz.hr, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz.hr")
# here, truncation seems certain at 90m, in pdf plot detection fell rapidly after 95m


# DF in CDS Gaz Unif-cos no trunc

gaz.u.cos <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "unif", adjustment = "cos")

check.mono(gaz.u.cos$ddf)
# TRUE

summary(gaz.u.cos)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# Model : Uniform key function with cosine adjustment terms of order 1,2 
# AIC   : 737.0185
# Chi-square P = 0.18324 with 6 degrees of freedom    # > 0.05, so retain!
0.18324/6
# 0.03054   no overdispersion

par(mfrow = c(1, 2))
plot(gaz.u.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.u.cos")
ddf.gof(gaz.u.cos$ddf, main ="gaz.u.cos")
dev.off()

p_dist_table(gaz.u.cos, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3    80          1
# 0.3 - 0.4     0          0
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.23 - 0.23  

# DF well fitted (Pa >= 0.2)

plot(gaz.u.cos, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.u.cos")
# here, truncation at 80m seems right; in pdf plot detections fell after 95m


# DF in CDS Gaz HN-herm no trunc

gaz.hn.herm <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", adjustment = "herm")
# selected HN no adjustments


# DF in CDS Gaz HN-cos no trunc

gaz.hn.cos <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", adjustment = "cos")
# selected HN no adjustments


#DF in  CDS Gaz HR-poly no trunc

gaz.hr.poly <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hr", adjustment = "poly")
# selected HR no adjustments


# I ran combinations of HN-poly and HR-herm, and selected was always the DF key without adjustment terms.
# When I ran gaz.hr.cos, the model was stored and in QAIC/c^ model selection this was chosen


# CDS DF model selection by AIC for Gazelle no truncation ####

df.gaz.notrunc <- list(gaz.hn, gaz.u.cos)   # the ones with Chi-square P >= 0.05

gaz.aic.notrunc <- kable(summarize_ds_models(gaz.hn, gaz.u.cos, sort = "AIC", delta_only = FALSE, output = "plain"), 
                         caption = "Summary of Gazelle CDS no trunc models analysis", digits = 3)

gaz.aic.notrunc

# Table: Summary of Gazelle CDS no trunc models analysis

# |Model     |Key function                                      |Formula | C-vM $p$-value|     AIC| Delta AIC|
# |:---------|:-------------------------------------------------|:-------|--------------:|-------:|---------:|
# |gaz.hn    |Half-normal                                       |~1      |          0.625| 736.111|     0.000|
# |gaz.u.cos |Uniform with cosine adjustment terms of order 1,2 |NA      |          0.669| 737.019|     0.908|

# Based on its lowest AIC and delta AIC values, the DF model no trunc that best fitted the Gazelle distance data
# in CDS is Half-normal with no adjustment terms, but since there is no difference >2 in AIC values with 
# U-cos, it will produce not significantly different results than HN-no adjs


# Model selection adjustments from overdispersion (QAIC/c^)

# Overdispersion causes AIC to select overly-complex models, so analysts should specify the number/order of
# adjustment terms manually when fitting distance sampling models to data from camera traps, rather than 
# allowing automated selection using AIC. The first method of Howe, Buckland, Després-Einspenner, & Kühl (2019) 
# employs a two-step process. First, an overdispersion factor (c^) is computed for each key function family
# from the most complex model in each family. The c^ is derived from the Chi-square goodness of fit test 
# statistic divided by its degrees of freedom. This results in an adjusted AIC score for each model in the 
# key function family:


# CDS DF model selection by QAIC for Gazelle no truncation ####


# Computing QAIC for each set of key function models for CDS Gaz no trunc

# here I can't select DF models within families, because I have only one model per family, so I go for step 2.


# The second step of model selection ranks the models by their c^ values.

chats_df.gaz.notrunc <- chi2_select(gaz.hn, gaz.u.cos)

modnames <- unlist(lapply(list(gaz.hn, gaz.u.cos), function(x) x$ddf$name.message))

results_df.gaz.notrunc <- data.frame(modnames, chats_df.gaz.notrunc)

results.sort_df.gaz.notrunc <- results_df.gaz.notrunc[order(results_df.gaz.notrunc$criteria),]

gaz.qaic.notrunc <- knitr::kable(results.sort_df.gaz.notrunc, digits=2, row.names = FALSE,
                                 caption="CDS DF models Gaz no trunc ranked by their c^ value")

gaz.qaic.notrunc

# Table: CDS DF models Gaz no trunc ranked by their c^ value
#|modnames                                          | criteria|
#|:-------------------------------------------------|--------:|
#|half-normal key function                          |     1.35|
#|uniform key function with cosine(1,2) adjustments |     1.47|

# Here, for CDS DF model selection Gaz no trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is the same (half-normal no adjustments) as it had been chosen by conventional 
# model selection based on AIC values (half-normal no adjustments).

# so, selected is gaz.hn


# DF in CDS Gaz HN no adjustment 80m trunc

gaz80.hn <- ds(distdata_gaz, transect = "point", key = "hn", adjustment = NULL, truncation = 80)

check.mono(gaz80.hn)
# NULL              (¿what this means?)

summary(gaz80.hn)
# Number of observations :  71
# Distance range         :  0  -  80 
# AIC   : 616.5207 
# Chi-square P = 0.62783 with 6 degrees of freedom            # way up to > 0.05, so retain!
0.62783/6
# 0.1046383 values >1 mean overdispersion (from little to large, depends on the number), so we're good!

par(mfrow = c(1, 2))
plot(gaz80.hn, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz80.hn")
ddf.gof(gaz80.hn$ddf, main = "gaz80.hn")
dev.off()

# Checking distribution of probability of detections (Pa) in case further truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(gaz80.hn, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4     0          0
# 0.4 - 0.5    71          1
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.5 - 0.5

# Detection function seems well fitted (all observation are Pa => 0.4)

plot(gaz80.hn, pl.col = "white", xlab = "Distance of detection (m)", main = "gaz80.hn")
# 80m truncation seems about right, besides Pa is well fitted so no need of further truncation


# DF in CDS Gaz HR no adjustment 80m trunc

gaz80.hr <- ds(distdata_gaz, transect = "point", key = "hr", adjustment = NULL, truncation = 80)

check.mono(gaz80.hr$ddf)
# TRUE

summary(gaz80.hr)
# Number of observations :  71 
# Distance range         :  0  -  80 
# AIC   : 617.0363 
# Chi-square P = 0.69508 with 5 degrees of freedom              # >> 0.05, so retain!
0.69508/5
# 0.139016, no overdispersion

par(mfrow = c(1, 2))
plot(gaz80.hr, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr")
ddf.gof(gaz80.hr$ddf, main ="gaz80.hr")
dev.off()

p_dist_table(gaz80.hr, proportion = TRUE)
#               p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4    71          1
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.37 - 0.37

# DF well fitted (Pa > 0.3)

plot(gaz80.hr, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr")
# here seems that a different truncation distance was more right, but for analysis sake I used the same
# anyway, Pa is well fitted so no need of further truncation


# DF in CDS Gaz U-cos 80m trunc

gaz80.u.cos <- ds(distdata_gaz, transect = "point", key = "unif", adjustment = "cos", truncation = 80)

check.mono(gaz80.u.cos$ddf)
# TRUE

summary(gaz80.u.cos)
# Number of observations :  71 
# Distance range         :  0  -  80 
# Model : Uniform key function with cosine adjustment term of order 1 
# AIC   : 616.5169 
# Chi-square P = 0.63321 with 6 degrees of freedom              # >> 0.05, so retain!
0.63321/6
# 0.105535, no overdispersion

par(mfrow = c(1, 2))
plot(gaz80.u.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.u.cos")
ddf.gof(gaz80.u.cos$ddf, main ="gaz80.u.cos")
dev.off()

p_dist_table(gaz80.u.cos, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4     0          0
# 0.4 - 0.5    71          1
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.48 - 0.48

# DF well fitted (Pa > 0.4)

plot(gaz80.u.cos, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.u.cos")
# Pa is well fitted so no need of further truncation, but what if maybe less?


# DF in CDS Gaz HN-herm 80m trunc

gaz80.hn.herm <- ds(distdata_gaz, transect = "point", key = "hn", adjustment = "herm", truncation = 80)
# selected HN no adjustments


# DF in CDS Gaz HN-cos 80m trunc

gaz80.hn.cos <- ds(distdata_gaz, transect = "point", key = "hn", adjustment = "cos", truncation = 80)

check.mono(gaz80.hn.cos$ddf)
# TRUE

summary(gaz80.hn.cos)
# Number of observations :  71 
# Distance range         :  0  -  80 
# Model : Half-normal key function with cosine adjustment term of order 2 
# AIC   : 615.8793
# Chi-square P = 0.84089 with 5 degrees of freedom                      # >> 0.05, so retain!
0.84089/5
# 0.168178, no overdispersion

par(mfrow = c(1, 2))
plot(gaz80.hn.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.cos")
ddf.gof(gaz80.hn.cos$ddf, main ="gaz80.hn.cos")
dev.off()

p_dist_table(gaz80.hn.cos, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4    71          1
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.3 - 0.3

# DF well fitted (Pa > 0.3)

plot(gaz80.hn.cos, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.cos")
# even if truncation could be lower, Pa is well fitted so no need of further truncation


# Just in case, I tried HN-poly, but selected HN no adjustments too


# DF in CDS Gaz HR-poly 80m trunc

gaz80.hr.poly <- ds(distdata_gaz, transect = "point", key = "hr", adjustment = "poly", truncation = 80)
# selected HR no adjustments


# I also tried HR-cos and HR-herm, but selected HR no adjustments too


# CDS DF model selection by AIC for Gazelle 80m truncation ####

df.gaz.trunc <- list(gaz80.hn, gaz80.hr, gaz80.u.cos, gaz80.hn.cos)   # the ones with Chi-square P >= 0.05

gaz.aic.trunc <- kable(summarize_ds_models(gaz80.hn, gaz80.hr, gaz80.u.cos, gaz80.hn.cos, 
                                           sort = "AIC", delta_only = FALSE, output = "plain"), 
                       caption = "Summary of Gazelle CDS 80m trunc models analysis", digits = 3)

gaz.aic.trunc

# Table: Summary of Gazelle CDS 80m trunc models analysis

#|   |Model        |Key function                                |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:--|:------------|:-------------------------------------------|:-------|--------------:|-------:|---------:|
#|4  |gaz80.hn.cos |HN with cosine adjustment term of order 2   |~1      |          0.981| 615.879|     0.000|
#|3  |gaz80.u.cos  |Unif with cosine adjustment term of order 1 |NA      |          0.715| 616.517|     0.638|
#|1  |gaz80.hn     |Half-normal                                 |~1      |          0.633| 616.521|     0.641|
#|2  |gaz80.hr     |Hazard-rate                                 |~1      |          0.928| 617.036|     1.157|


# Based on its lowest AIC and delta AIC values, the DF 80m trunc that best fitted the Gazelle distance data 
# in CDS is Half-normal with cosine adjustment terms, but since there is no difference >2 in AIC values with U-cos, 
# HN-no adjs and HR-no adjs, they will produce not significantly different results than HN-cos


# CDS DF model selection by QAIC for Gazelle 80m truncation ####

# Computing QAIC for HN key function models in CDS Gaz 80m trunc (only family to perform)

gaz80.hn.qaic <- QAIC(gaz80.hn, gaz80.hn.cos)

gaz80tab.hn.qaic <- knitr::kable(gaz80.hn.qaic, caption="Gaz CDS QAIC values for HN key 80m trunc models")

gaz80tab.hn.qaic

# Table: Gaz CDS QAIC values for HN key 80m trunc models
#|             | df|     QAIC|
#|:------------|--:|--------:|
#|gaz80.hn     |  1| 1496.097|
#|gaz80.hn.cos |  2| 1491.684|

# here I selected gaz80.hn.cos as a DF model within HN key family.


# The second step of model selection ranks the models by their c^ values.

chats_df.gaz.trunc <- chi2_select(gaz80.hr, gaz80.u.cos, gaz80.hn.cos) 

modnames2 <- unlist(lapply(list(gaz80.hr, gaz80.u.cos, gaz80.hn.cos), function(x) x$ddf$name.message))

results_df.gaz.trunc <- data.frame(modnames2, chats_df.gaz.trunc)

results.sort_df.gaz.trunc <- results_df.gaz.trunc[order(results_df.gaz.trunc$criteria),]

gaz.qaic.trunc <- knitr::kable(results.sort_df.gaz.trunc, digits=2, row.names = FALSE,
                               caption="CDS DF models Gaz 80m trunc ranked by their c^ value")

gaz.qaic.trunc

# Table: CDS DF models Gaz 80m trunc ranked by their c^ value
#|modnames2                                           | criteria|
#|:---------------------------------------------------|--------:|
#|half-normal key function with cosine(2) adjustments |     0.41|
#|hazard-rate key function                            |     0.61|
#|uniform key function with cosine(1) adjustments     |     0.72|

# Here, for CDS DF model selection Gaz 80m trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is the same (half normal key cosine adjustments) as it had been chosen by conventional 
# model selection based on AIC values (half-normal cosine adjustments).

# so, selected is gaz80.hn.cos


# Then, between truncated and not truncated models (although not comparable because of different truncation):
AIC(gaz.hn, gaz80.hn.cos)
#              df      AIC
# gaz.hn        1 736.1109
# gaz80.hn.cos  2 615.8793

# the model with truncation distance has lower AIC (obviously), better Chi-square p-values (0.8 vs 0.22) and 
# even better Pa distribution (0.3 vs 0.2), and certainly would provide more accurate estimations of abundance 


# So, in CDS the DF model selected for Goitered gazelle is gaz80.hn.cos ####
# (in research project, gaz.hn no adjustments was selected)


# Looking for the detection function in CDS that best fits Kulan distance data ####


# DF in CDS Kul HN no adjustment no trunc

kul.hn <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", adjustment = NULL)

check.mono(kul.hn$ddf)
# TRUE

summary(kul.hn)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 526.7377 
# Chi-square P = 0.77852 with 5 degrees of freedom                # >> 0.05, so retain!
0.77852/5
# 0.155704, no overdispersion

par(mfrow = c(1, 2))
plot(kul.hn, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn")
ddf.gof(kul.hn$ddf, main ="kul.hn")
dev.off()

# Checking distribution of probability of detections (Pa) in case truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(kul.hn, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3    46          1
# 0.3 - 0.4     0          0
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.23 - 0.23

# DF well fitted (Pa > 0.2)

plot(kul.hn, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn")
# seems that at 260m detection probability is =< 0.15, so I will right truncate at this distance in a further stage


# DF in CDS Kul HR no adjustment no trunc

kul.hr <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hr", adjustment = NULL)

check.mono(kul.hr$ddf)
# TRUE

summary(kul.hr)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 528.013 
# Chi-square P = 0.65997 with 4 degrees of freedom              # >> 0.05, so retain!
0.65997/4
# 0.1649925, no overdispersion

par(mfrow = c(1, 2))
plot(kul.hr, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr")
ddf.gof(kul.hr$ddf, main ="kul.hr")
dev.off()

p_dist_table(kul.hr, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3    46          1
# 0.3 - 0.4     0          0
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.26 - 0.26

# well fitted DF (Pa > 0.2)

plot(kul.hr, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr")
# detection probability is =< 0.15 at 260m more or less, let's right truncate


# DF in CDS Kul U-cos no trunc

kul.u.cos <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "unif", adjustment = "cos")

check.mono(kul.u.cos$ddf)
# TRUE

summary(kul.u.cos)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# Model : Uniform key function with cosine adjustment terms of order 1,2 
# AIC   : 525.3204
# Chi-square P = 0.69734 with 4 degrees of freedom                  # >> 0.05, so retain!
0.68726/4
# 0.171815, no overdispersion

par(mfrow = c(1, 2))
plot(kul.u.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.u.cos")
ddf.gof(kul.u.cos$ddf, main ="kul.u.cos")
dev.off()

p_dist_table(kul.u.cos, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3    46          1
# 0.3 - 0.4     0          0
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.23 - 0.23

# well fitted DF (Pa > 0.2)

plot(kul.u.cos, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.u.cos")
# seems that 0.15 detection probability is at 240m here, but for the sake of analysis I will use the same
# truncation distance (260m)


# DF in CDS Kul HN-herm no trunc

kul.hn.herm <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", adjustment = "herm")
# HN-no adjs selected instead


# DF in CDS Kul HN-cos no trunc

kul.hn.cos <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", adjustment = "cos")
# HN-no adjs selected instead

# I tried HN-poly just in case, but still selected HN no adjustment


# DF in CDS Kul HR-poly no trunc

kul.hr.poly <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hr", adjustment = "poly")
# HR-no adjs selected instead

# I tried HR-cos and HR-herm just in case, but still selected HR no adjustment


# CDS DF model selection by AIC for Kulan no truncation ####

df.kul.notrunc <- list(kul.hn, kul.hr, kul.u.cos)   # the ones with Chi-square P >= 0.05

kul.aic.notrunc <- kable(summarize_ds_models(kul.hn, kul.hr, kul.u.cos, sort = "AIC", 
                                             delta_only = FALSE, output = "plain"), 
                         caption = "Summary of Kulan CDS no trunc models analysis", digits = 3)

kul.aic.notrunc

# Table: Summary of Kulan CDS no trunc models analysis

#|   |Model     |Key function                                |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:--|:---------|:-------------------------------------------|:-------|--------------:|-------:|---------:|
#|3  |kul.u.cos |Unif with cos adjustment terms of order 1,2 |NA      |          0.955| 525.320|     0.000|
#|1  |kul.hn    |Half-normal                                 |~1      |          0.926| 526.738|     1.417|
#|2  |kul.hr    |Hazard-rate                                 |~1      |          0.952| 528.013|     2.693|

# Based on its lowest AIC and delta AIC values, the DF no trunc that best fitted the Kulan distance data in 
# CDS is Uniform with cosine adjustment terms, but since there is no difference >2 in AIC values with HN-no adjs,
# it will produce not significantly different results.


# CDS DF model selection by QAIC for Kulan no truncation ####


# Computing QAIC for each set of key function models for CDS Kul no truncation

# here I can't select DF models within families, because I have only one model per family, so I go for step 2.


# The second step of model selection ranks the models by their c^ values.

chats_df.kul.notrunc <- chi2_select(kul.hn, kul.hr, kul.u.cos)

modnames3 <- unlist(lapply(list(kul.hn, kul.hr, kul.u.cos), function(x) x$ddf$name.message))

results_df.kul.notrunc <- data.frame(modnames3, chats_df.kul.notrunc)

results.sort_df.kul.notrunc <- results_df.kul.notrunc[order(results_df.kul.notrunc$criteria),]

kul.qaic.notrunc <- knitr::kable(results.sort_df.kul.notrunc, digits=2, row.names = FALSE,
                                 caption="CDS DF models Kul no trunc ranked by their c^ value")

kul.qaic.notrunc

# Table: CDS DF models Kul no trunc ranked by their c^ value
#|modnames3                                         | criteria|
#|:-------------------------------------------------|--------:|
#|half-normal key function                          |     0.50|
#|uniform key function with cosine(1,2) adjustments |     0.57|
#|hazard-rate key function                          |     0.60|

# Here, for CDS DF model selection Kul no truncation, the model chosen by this algorithm that adjusts for 
# overdispersion is different (half normal key no adjustments) as it had been chosen by conventional 
# model selection based on AIC values (uniform key cosine adjustments).

# so, selected is kul.hn 


# DF in CDS Kul HN no adjustment 260m trunc

kul260.hn <- ds(distdata_kul, transect = "point", key = "hn", adjustment = NULL, truncation = 260)

check.mono(kul260.hn$ddf)
# TRUE

summary(kul260.hn)
# Number of observations :  42 
# Distance range         :  0  -  260 
# AIC   : 460.3787 
# Chi-square P = 0.75922 with 4 degrees of freedom              # >> 0.05, so retain!
0.75922/4
# 0.189805, no overdispersion

par(mfrow = c(1, 2))
plot(kul260.hn, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn")
ddf.gof(kul260.hn$ddf, main ="kul260.hn")
dev.off()

p_dist_table(kul260.hn, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4    42          1
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.39 - 0.39

# better fitted than without truncation (Pa > 0.3)

plot(kul260.hn, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn")
# Pa well fitted so no need of further truncation


# CDS Kul HR no adjustment 260m trunc

kul260.hr <- ds(distdata_kul, transect = "point", key = "hr", adjustment = NULL, truncation = 260)

check.mono(kul260.hr$ddf)
# TRUE

summary(kul260.hr)
# Number of observations :  42 
# Distance range         :  0  -  260 
# AIC   : 462.1462
# Chi-square P = 0.60123 with 3 degrees of freedom                # >> 0.05, so retain!
0.60123/3
# 0.20041, no overdispersion

par(mfrow = c(1, 2))
plot(kul260.hr, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr")
ddf.gof(kul260.hr$ddf, main ="kul260.hr")
dev.off()

p_dist_table(kul260.hr, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4     0          0
# 0.4 - 0.5    42          1
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.41 - 0.41 

# way better fitted df than without truncation (Pa > 0.4)

plot(kul260.hr, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr")
# Pa well fitted so no need of further truncation


# DF in CDS Kul U-cos 260m trunc

kul260.u.cos <- ds(distdata_kul, transect = "point", key = "unif", adjustment = "cos", truncation = 260)

check.mono(kul260.u.cos$ddf)
# TRUE

summary(kul260.u.cos)
# Number of observations :  42 
# Distance range         :  0  -  260 
# Model : Uniform key function with cosine adjustment term of order 1 
# AIC   : 459.9358 
# Chi-square P = 0.77683 with 4 degrees of freedom                  # >> 0.05, so retain!
0.77683/4
# 0.1942075, no overdispersion

par(mfrow = c(1, 2))
plot(kul260.u.cos, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.u.cos")
ddf.gof(kul260.u.cos$ddf, main ="kul260.u.cos")
dev.off()

p_dist_table(kul260.u.cos, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0          0
# 0.1 - 0.2     0          0
# 0.2 - 0.3     0          0
# 0.3 - 0.4    42          1
# 0.4 - 0.5     0          0
# 0.5 - 0.6     0          0
# 0.6 - 0.7     0          0
# 0.7 - 0.8     0          0
# 0.8 - 0.9     0          0
#   0.9 - 1     0          0
# Range of probabilities:  0.39 - 0.39

# better fitted DF than without trunc (Pa > 0.3)

plot(kul260.u.cos, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.u.cos")
# Pa well fitted so no need of further truncation


# DF in CDS Kul HN-herm 260m trunc

kul260.hn.herm <- ds(distdata_kul, transect = "point", key = "hn", adjustment = "herm", truncation = 260)
# HN-no adjs selected instead


# DF in CDS Kul HN-cos 260m trunc

kul260.hn.cos <- ds(distdata_kul, transect = "point", key = "hn", adjustment = "cos", truncation = 260)
# HN-no adjs selected instead

# I tried HN-poly just in case, but HN-no adjs was selected instead


# DF in CDS Kul HR-poly 260m trunc

kul260.hr.poly <- ds(distdata_kul, transect = "point", key = "hr", adjustment = "poly", truncation = 260)
# HR-no adjs selected instead

# I tried HR-cos and HR-poly just in case, but HR-no adjs was selected instead


# CDS DF model selection by AIC for Kulan 260m truncation ####

df.kul.trunc <- list(kul260.hn, kul260.hr, kul260.u.cos)   # the ones with Chi-square P >= 0.05

kul.aic.trunc <- kable(summarize_ds_models(kul260.hn, kul260.hr, kul260.u.cos, 
                                           sort = "AIC", delta_only = FALSE, output = "plain"), 
                       caption = "Summary of Kulan CDS 260m trunc models analysis", digits = 3)

kul.aic.trunc

# Table: Summary of Kulan CDS 260m trunc models analysis
#|   |Model        |Key function                                   |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:--|:------------|:----------------------------------------------|:-------|--------------:|-------:|---------:|
#|3  |kul260.u.cos |Uniform with cosine adjustment term of order 1 |NA      |          0.963| 459.936|     0.000|
#|1  |kul260.hn    |Half-normal                                    |~1      |          0.955| 460.379|     0.443|
#|2  |kul260.hr    |Hazard-rate                                    |~1      |          0.961| 462.146|     2.210|

# Based on its lowest AIC and delta AIC values, the DF 260m trunc that best fitted Kulan distance data in CDS 
# is Uniform with cosine adjustment terms, but since there is no difference >2 in AIC values with HN-no adjs, 
# they will produce not significantly different results than the first


# CDS DF model selection by QAIC for Kulan 260m truncation ####


# Computing QAIC for each set of key function models for CDS Kul 260m trunc

# here I can't select DF models within families, because I have only one model per family, so I go for step 2.


# The second step of model selection ranks the models by their c^ values.

chats_df.kul.trunc <- chi2_select(kul260.hn, kul260.hr, kul260.u.cos)

modnames4 <- unlist(lapply(list(kul260.hn, kul260.hr, kul260.u.cos), function(x) x$ddf$name.message))

results_df.kul.trunc <- data.frame(modnames4, chats_df.kul.trunc)

results.sort_df.kul.trunc <- results_df.kul.trunc[order(results_df.kul.trunc$criteria),]

kul.qaic.trunc <- knitr::kable(results.sort_df.kul.trunc, digits=2, row.names = FALSE,
                               caption="CDS DF models Kul 260m trunc ranked by their c^ value")

kul.qaic.trunc

# Table: CDS DF models Kul 260m trunc ranked by their c^ value
#|modnames4                                       | criteria|
#|:-----------------------------------------------|--------:|
#|uniform key function with cosine(1) adjustments |     0.44|
#|half-normal key function                        |     0.47|
#|hazard-rate key function                        |     0.62|

# Here, for CDS DF model selection Kul 260m trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is the same (uniform key cosine adjustments) as it had been chosen by conventional 
# model selection based on AIC values (uniform key cosine adjustments).

# so, selected is kul260.u.cos


# Then, between trunc and no trunc models (although not comparable because of different truncation):
AIC(kul.hn, kul260.u.cos)
#              df      AIC
# kul.hn        1 526.7377
# kul260.u.cos  1 459.9358

# the model with truncation distance has lower AIC (obviously) but similar Chi-square P values (0.77), moreover 
# it was selected by its c^ value, and would provide more accurate estimations of abundance (even has Pa in a range 
# greater than not truncated DF, 0.39 vs 0.22)


# So, in CDS the DF model selected for Kulan is kul260.u.cos ####


# Fitting a Spatial Density Surface Model (DSM) in Conventional Distance Sampling (CDS) ####


# First of all, I need to change columns names/add columns on the files that will be used in this stage, 
# which will link both files included in the analysis: the covjune file with the identifier, sampling effort 
# and coordinates for each sampling unit (CT location); and the distdata files, which also has an identifier 
# per sampling unit along with their coordinates and recorded distances by every animal detected in 
# timelapse images. 

# The identifier, which links both files, is the Sample.Label (case-sensitive) column:

names(distdata_gaz)[names(distdata_gaz) == "point_id"] <- "Sample.Label"
names(distdata_gaz)

names(distdata_kul)[names(distdata_kul) == "point_id"] <- "Sample.Label"
names(distdata_kul)

names(covjune)[names(covjune) == "point_id"] <- "Sample.Label"
names(covjune)


# I will start my DSM model analysis performing a CDS model, which assume that the number of animals detected 
# in timelapse images per each Camera Trap are quasi-Poisson distributed (the default in the model code), and that 
# these numbers (adjusted by the detection function) are a smooth function of their spatial coordinates.  As a 
# detection function, for both species I will use the one which resulted to have the lowest AIC/QAIC/c^ value between
# all key and adjustment terms combinations. I set "method = REML" to ensure that smooth terms are estimated reliably.


# Then, I will run the DSM with the chosen detection function in CDS for both focal species, but this time applying 
# two different error distributions to the count response: Tweedie and Negative Binomial distributions, testing if 
# them fit better these data, in case the quasi-Poisson distribution does not give them adequate flexibility and 
# does not capture their overdispersion.


# Goitered Gazelle spatial DSM (CDS) ####

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(gaz80.hn.cos)
# Distance range         :  0  -  80 
# so I need to eliminate observations at distances >= 80m

distdata_gaz80 <- distdata_gaz[distdata_gaz$distance <= 80, ] 

summary(distdata_gaz80$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.924  24.451  45.905  44.265  61.853  79.849 


# Running a spatial DSM (CDS) with Half-normal key cosine adjustment terms Detection Function, 
# 80m truncation and quasi-Poisson error distribution: 

gaz80hncos.xy.qpois <- dsm(count ~ s(x, y), gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#            Fitting terminated with step failure - check results carefully

summary(gaz80hncos.xy.qpois)
# Approximate significance of smooth terms:
#          edf Ref.df         F p-value    
# s(x,y) 28.99      5 6.011e+12  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1051.6  Scale est. = 3.1368e-15  n = 77

# don't know where the 29 EDFs comes from (there is no 30 combinations of x and y values), but EDFs go down
# in other error distributions

# GAM check

par(mfrow=c(2,2))
gam.check(gaz80hncos.xy.qpois)
dev.off()


# Fitting warning, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q and 
# resids vs linear plots flat and not OK) and suspiciously high deviance explained and very low -REML value 
# all suggest fitting problems for quasi-Poisson DSM model (THE SAME AS THESIS RESULT, EVEN THOUGH THIS HAS TRUNCATION)


# Running a spatial DSM (CDS) with Half-normal key cosine adjustment terms Detection Function, 
# 80m truncation and Tweedie error distribution:

gaz80hncos.xy.tw <- dsm(count ~ s(x, y), gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                        transect = "point", method = "REML", family = tw())

# no warnings

summary(gaz80hncos.xy.tw)
# Family: Tweedie(p=1.212)
# Approximate significance of smooth terms:
#          edf Ref.df     F  p-value    
# s(x,y) 15.71  19.47 2.888 0.000901 ***

# R-sq.(adj) =  0.968   Deviance explained = 93.3%
# -REML = 46.281  Scale est. = 2.8869    n = 77

par(mfrow=c(2,2))
gam.check(gaz80hncos.xy.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois DSM). -REML is positive now.


# Running a spatial DSM (CDS) with Half-normal key cosine adjustment terms Detection Function, 
# 80m truncation and Negative Binomial error distribution:

gaz80hncos.xy.nb <- dsm(count ~ s(x, y), gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                        transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hncos.xy.nb)
# Family: Negative Binomial(11806.013) 
# Approximate significance of smooth terms:
#          edf Ref.df Chi.sq p-value    
# s(x,y) 17.07  20.17  107.6  <2e-16 ***

# R-sq.(adj) =  0.998   Deviance explained = 98.6%
# -REML = 47.074  Scale est. = 1         n = 77

par(mfrow=c(2,2))
gam.check(gaz80hncos.xy.nb)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois DSM, Q-Q plot narrower than Tw DSM).


# Let's compare Gazelle spatial models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(gaz80hncos.xy.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(gaz80hncos.xy.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(gaz80hncos.xy.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Qpois model is out of competition due to its suspiciously high EDFs and Deviance explained, and it's lower -REML
# Both Negative Binomial and Tweedie distribution models have gam plots that looked OK-ish; NB even has
# a Q-Q plot with less dispersion of data
# The Negative Binomial has a slightly greater REML than the Tweedie one, but its deviance explained is
# 5 points greater also. So, for the Goitered gazelle count data adjusted by the detection function, 
# I will select the spatial DSM in CDS with Negative Binomial error distribution (gaz80hncos.xy.nb) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA  
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE, AND DF HN-NO ADJS WASN'T TRUNCATED)


# Kulan spatial DSM (CDS) ####

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

# kul260.u.cos

summary(kul260.u.cos)
# Distance range         :  0  -  260
# so need to eliminate observations at distances >= 260m

distdata_kul260 <- distdata_kul[distdata_kul$distance <= 260, ]

summary(distdata_kul260$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.43   83.00  143.48  134.51  179.42  255.06


# Running a spatial DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and quasi-Poisson error distribution:

kul260ucos.xy.qpois <- dsm(count ~ s(x, y), kul260.u.cos$ddf, covjune, distdata_kul260, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#            Fitting terminated with step failure - check results carefully

summary(kul260ucos.xy.qpois)
# Approximate significance of smooth terms:
#          edf Ref.df         F p-value    
# s(x,y) 28.95      4 1.874e+12  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1128.4  Scale est. = 3.3979e-15  n = 77

par(mfrow=c(2,2))
gam.check(kul260ucos.xy.qpois)
dev.off()

# Fitting warning, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson DSM model
# (THE SAME AS THESIS RESULT, EVEN THOUGH THIS HAS TRUNCATION)


# Running a spatial DSM (CDS) with Uniform key cosine adjustment Detection Function, 
# 260m truncation and Tweedie error distribution:

kul260ucos.xy.tw <- dsm(count ~ s(x, y), kul260.u.cos$ddf, covjune, distdata_kul260, 
                        transect = "point", method = "REML", family = tw())

# no warnings

summary(kul260ucos.xy.tw)
# Family: Tweedie(p=1.01)
# Approximate significance of smooth terms:
#          edf Ref.df     F  p-value    
# s(x,y) 9.211  10.66 3.741 0.000415 ***

# R-sq.(adj) =      1   Deviance explained = 99.6%
# -REML = 19.231  Scale est. = 0.99248   n = 77

par(mfrow=c(2,2))
gam.check(kul260ucos.xy.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois DSM).


# Running a spatial DSM (CDS) with Uniform key cosine adjustment Detection Function, 
# 260m truncation and Negative Binomial error distribution:

kul260ucos.xy.nb <- dsm(count ~ s(x, y), kul260.u.cos$ddf, covjune, distdata_kul260, 
                        transect = "point", method = "REML", family = nb())

# no warnings

summary(kul260ucos.xy.nb)
# Family: Negative Binomial(49814.535)
# Approximate significance of smooth terms:
#          edf Ref.df Chi.sq  p-value    
# s(x,y) 9.197  10.64  40.16 3.19e-05 ***
# R-sq.(adj) =      1   Deviance explained = 99.6%
# -REML = 21.737  Scale est. = 1         n = 77

par(mfrow=c(2,2))
gam.check(kul260ucos.xy.nb)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (almost the same as those of Tw DSM).


# Let's compare Kulan spatial models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(kul260ucos.xy.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(kul260ucos.xy.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(kul260ucos.xy.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Qpois model is out of competition due to its suspiciously high EDFs and Deviance explained, and it's lower -REML
# Both Negative Binomial and Tweedie distribution models have gam plots that looks OK-ish and very similar,
# here with Q-Q plots that show the same dispersion; even both models have the exact same Deviance explained, 
# but the Negative Binomial REML is about 2.5 points higher than the one for Tweedie. 
# So, for the Kulan count data adjusted by the detection function, 
# I will select the spatial DSM in CDS with Negative Binomial error distribution (kul260ucos.xy.nb) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE, AND DF HN-NO ADJS WASN'T TRUNCATED)


# Fitting a Spatial Analysis in DSM for Conventional Distance Sampling ####


# In this stage, I need to make a grid of spatial coordinates along the whole survey area, over which 
# be able to predict the spatial distribution and expected abundance and density of both species, 
# according to the selected DSM (CDS) model for each one.

# First step is to construct the prediction grid for the GGASPA, so I will select from the survey area 
# the columns of distance-based x,y coordinates (in metres):

GGASPA2 <- select(GGASPA, c("x", "y"))  # inout() function seems sensitive to these column names

# Then, I will construct the square grid of distance-based coordinates 
# ("1000" value is the dimension along each axis for each cell in the grid):

grd <- with(GGASPA2, expand.grid(x=seq(min(x), max(x), 1000), y=seq(min(y), max(y), 1000)))  

plot(grd)

# After that, I need to select from the squared prediction grid the area corresponding to the GGASPA, 
# so I shaped the grid as the survey area with inout() function (needs columns "x" and "y", case-sensitive)

grd.in <- grd[inout(grd, GGASPA2),]   

plot(grd.in)


# Now, I am able to make predictions over the grid and estimate density and abundance for both focal species, 
# using for the prediction the spatial DSM (CDS) selected for each one:


# Density and Abundance estimation for Goitered gazelle in spatial DSM (CDS) ####
# (using DSM with Half-normal key cosine adjustment terms detection function, 80m truncation and 
# Negative Binomial error distribution: (gaz80hncos.xy.nb)

off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance

predn_gaz80hncos.xy.nb <- predict(gaz80hncos.xy.nb, grd.in, off.set)

# I need to bind on my prediction file with the data from the prediction grid used to create them:

aa <- cbind(grd.in, predn_gaz80hncos.xy.nb)

# And then, add a column on this prediction grid file of abundances with the dimension for both axis 
# of each cell, from which calculate density:

aa$dim <- 1000


# Now that I have the predictions, I will plot the density for Goitered gazelle over the GGASPA:

predngaz.xy.cds <- ggplot(aa) + gg.opts + ggtitle("Goitered gazelle spatial DSM (CDS)")
predngaz.xy.cds <- predngaz.xy.cds + geom_tile(aes(x = x, y = y, fill = predn_gaz80hncos.xy.nb, 
                                                   width = dim, height = dim))
predngaz.xy.cds <- predngaz.xy.cds + coord_equal() + scale_fill_viridis_c(option = "D") 
# different than in thesis, which had "shades" of blue
predngaz.xy.cds <- predngaz.xy.cds + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.cds <- predngaz.xy.cds + labs(fill = "Density / km2")
predngaz.xy.cds <- predngaz.xy.cds + theme(legend.title = element_text(size = 21), 
                                           legend.text = element_text(size = 18), 
                                           plot.title = element_text(size = 23))
print(predngaz.xy.cds) 


# Letting aside that it is a different plot, with colour scales that makes differences more evident, the density plot
# is different from thesis, hotspots are a bit brighter, the location of the bottom hotspots
# changed a little bit (the bottom center hotspot seemed to "enlarge" giving the sensation of movement from where 
# it was in thesis plot), and even are 4 instead of 3 (one new in the bit that popped out to left from area), 
# also the scale is different (max 1.5 inds/km2 in thesis, and with 80m truncation is 5-ish/km2!!).


# Plot of density + CT location for Goitered gazelle over the GGASPA: (not included in the research project report, 
# just an exploration of which look more informative)


predngaz.xy.cds2 <- ggplot(aa) + gg.opts + ggtitle("Goitered gazelle spatial DSM (CDS)")
predngaz.xy.cds2 <- predngaz.xy.cds2 + geom_tile(aes(x = x, y = y, fill = predn_gaz80hncos.xy.nb, 
                                                     width = dim, height = dim))
predngaz.xy.cds2 <- predngaz.xy.cds2 + coord_equal() + scale_fill_viridis_c(option = "D")
predngaz.xy.cds2 <- predngaz.xy.cds2 + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.cds2 <- predngaz.xy.cds2 + geom_point(aes(x, y, size = gazelle_count), data = covjune, 
                                                  colour = "red", alpha = I(0.7))
predngaz.xy.cds2 <- predngaz.xy.cds2 + labs(fill = "Density / km2", size = "Detections")
predngaz.xy.cds2 <- predngaz.xy.cds2 + theme(legend.title = element_text(size = 21), 
                                             legend.text = element_text(size = 18), 
                                             plot.title = element_text(size = 23))
print(predngaz.xy.cds2)


# Plot of density + CT location and name for Goitered gazelle over the GGASPA: (not included in the research project 
# report, just exploration of which look more informative)

predngaz.xy.cds3 <- ggplot(aa) + gg.opts + ggtitle("Goitered gazelle spatial DSM (CDS)")
predngaz.xy.cds3 <- predngaz.xy.cds3 + geom_tile(aes(x = x, y = y, fill = predn_gaz80hncos.xy.nb, 
                                                     width = dim, height = dim))
predngaz.xy.cds3 <- predngaz.xy.cds3 + coord_equal() + scale_fill_viridis_c(option = "D")
predngaz.xy.cds3 <- predngaz.xy.cds3 + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.cds3 <- predngaz.xy.cds3 + geom_point(aes(x, y, size = gazelle_count), data = covjune, 
                                                  colour = "red", alpha = I(0.7))
predngaz.xy.cds3 <- predngaz.xy.cds3 + labs(fill = "Density / km2", size = "Detections")
predngaz.xy.cds3 <- predngaz.xy.cds3 + geom_text(aes(x = x, y = y, label = Sample.Label), hjust=-0.5, vjust=-0.4, 
                                                 size=3, colour ="grey", data = covjune)
predngaz.xy.cds3 <- predngaz.xy.cds3 + theme(legend.title = element_text(size = 21), 
                                             legend.text = element_text(size = 18), 
                                             plot.title = element_text(size = 23))
print(predngaz.xy.cds3)
# don't like it very much


# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Goitered gazelle 
# in the GGASPA:

est.abun.gaz80hncos.xy.nb <- sum(predn_gaz80hncos.xy.nb)

est.abun.gaz80hncos.xy.nb
# 6995.329

# (was 2071.026 individuals of Goitered gazelle at GGASPA in thesis (HN-no adjs, TW), 
# so truncation to 80m and different key adjustment term and error distribution worked!)

# Also, with the estimated total abundance I can calculate the estimated total density for the species 
# in the GGASPA. I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) 
# to calculate total area which will divide total estimated abundance to obtain total estimated density:

length(predn_gaz80hncos.xy.nb)
# 45864 (in km2), a bit different from what is known area of GGASPA = 45,945 km2 (should it be the difference
# between known real totally fitted area vs squared-cells area constructed for prediction?)


est.dens.gaz80hncos.xy.nb <- est.abun.gaz80hncos.xy.nb / length(predn_gaz80hncos.xy.nb)

est.dens.gaz80hncos.xy.nb
# 0.1525233

# (was 0.0451558 individuals of Goitered gazelle per km2 at GGASPA in thesis (HN-no adjs, TW),
# so truncation to 80m and different key adjustment term and error distribution worked!)

# Transforming density into individuals/100km2

est.dens.gaz80hncos.xy.nb * 100
# 15.25233 individuals/100 km2

# (was 4.51558 individuals of Goitered gazelle/100 km2 at GGASPA in thesis (HN-no adjs, TW),
# so truncation to 80m and different key adjustment term and error distribution worked!).


# Density and Abundance estimation for Kulan in spatial DSM (CDS) ####
# (using DSM with Uniform key cosine adjustment Detection Function, 260m truncation 
# and Negative Binomial error distribution: (kul260ucos.xy.nb)


# off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance

predn_kul260ucos.xy.nb <- predict(kul260ucos.xy.nb, grd.in, off.set)


# I need to bind on my predictions file with the data from the prediction grid used to create them:

bb <- cbind(grd.in, predn_kul260ucos.xy.nb)


# And then, add a column on this prediction grid file of abundances with the dimension for both axis 
# of each cell, from which calculate density:

bb$dim <- 1000


# Now that I have the predictions, I will plot the density for Kulan over the GGASPA:

prednkul.xy.cds <- ggplot(bb) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (CDS)")
prednkul.xy.cds <- prednkul.xy.cds + geom_tile(aes(x = x, y = y, fill = predn_kul260ucos.xy.nb, 
                                                   width = dim, height = dim))
prednkul.xy.cds <- prednkul.xy.cds + coord_equal() + scale_fill_viridis_c(option = "D")
prednkul.xy.cds <- prednkul.xy.cds + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.cds <- prednkul.xy.cds + labs(fill = "Density / km2")
prednkul.xy.cds <- prednkul.xy.cds + theme(legend.title = element_text(size = 21), 
                                           legend.text = element_text(size = 18), 
                                           plot.title = element_text(size = 23))
print(prednkul.xy.cds) 

# Letting aside that it is a different plot, with colour scales that makes differences more evident, the density plot
# seems a bit different from thesis; same number of hotspots but 2nd is less brighter here than in thesis, 
# and the scale is different (max 0.15 inds/km2 in thesis, with 80m truncation the scale is 0.4-ish/km2).


# Plot of density + CT location for Kulan over the GGASPA: (not included in the research project report, just 
# exploration of which look more informative)

prednkul.xy.cds2 <- ggplot(bb) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (CDS)")
prednkul.xy.cds2 <- prednkul.xy.cds2 + geom_tile(aes(x = x, y = y, fill = predn_kul260ucos.xy.nb, 
                                                     width = dim, height = dim))
prednkul.xy.cds2 <- prednkul.xy.cds2 + coord_equal() + scale_fill_viridis_c(option = "D")
prednkul.xy.cds2 <- prednkul.xy.cds2 + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.cds2 <- prednkul.xy.cds2 + geom_point(aes(x, y, size = kulan_count), data = covjune, 
                                                  colour = "red", alpha = I(0.7))
prednkul.xy.cds2 <- prednkul.xy.cds2 + labs(fill = "Density / km2", size = "Detections")
prednkul.xy.cds2 <- prednkul.xy.cds2 + theme(legend.title = element_text(size = 21), 
                                             legend.text = element_text(size = 18), 
                                             plot.title = element_text(size = 23))
print(prednkul.xy.cds2)


# Plot of density + CT location and name for Kulan over the GGASPA: (not included in the research project report, 
# just exploration of which look more informative)

prednkul.xy.cds3 <- ggplot(bb) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (CDS)")
prednkul.xy.cds3 <- prednkul.xy.cds3 + geom_tile(aes(x = x, y = y, fill = predn_kul260ucos.xy.nb, 
                                                     width = dim, height = dim))
prednkul.xy.cds3 <- prednkul.xy.cds3 + coord_equal() + scale_fill_viridis_c(option = "D")
prednkul.xy.cds3 <- prednkul.xy.cds3 + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.cds3 <- prednkul.xy.cds3 + geom_point(aes(x, y, size = kulan_count), data = covjune, 
                                                  colour = "red", alpha = I(0.7))
prednkul.xy.cds3 <- prednkul.xy.cds3 + labs(fill = "Density / km2", size = "Detections")
prednkul.xy.cds3 <- prednkul.xy.cds3 + geom_text(aes(x = x, y = y, label = Sample.Label), hjust=-0.5, vjust=-0.4, 
                                                 size=3, colour ="grey", data = covjune)
prednkul.xy.cds3 <- prednkul.xy.cds3 + theme(legend.title = element_text(size = 18), 
                                             legend.text = element_text(size = 18), 
                                             plot.title = element_text(size = 23))
print(prednkul.xy.cds3)
# don't like it very much


# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Kulan 
# in the GGASPA:

est.abun.kul260ucos.xy.nb <- sum(predn_kul260ucos.xy.nb)

est.abun.kul260ucos.xy.nb
# 256.9379 

# (was 166.7125 individuals of Kulan at GGASPA in thesis (HN-no adjs, TW), so truncation to 260m and 
# different key adjustment term and error distribution worked!).

# Also, with the estimated total abundance I can calculate the estimated total density for the species 
# in the GGASPA. I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) 
# to calculate total area which will divide total estimated abundance to obtain total estimated density:

est.dens.kul260ucos.xy.nb <- est.abun.kul260ucos.xy.nb / length(predn_kul260ucos.xy.nb)

est.dens.kul260ucos.xy.nb
# 0.005602169 per km2

# (was 0.003634931 individuals of Kulan per km2 at GGASPA in thesis, so truncation to 260m and 
# different key adjustment term and error distribution worked!).

# Transforming density into individuals/100km2

est.dens.kul260ucos.xy.nb * 100
# 0.5602169 individuals/100 km2

# (was 0.3634931 individuals of Kulan/100 km2 at GGASPA in thesis, so truncation to 260m and 
# different key adjustment term and error distribution worked!).


# Variance of Estimated Density and Abundance in spatial DSM for Conventional Distance Sampling ####

# Here I will calculate the variance of the density and abundance estimates for both species using 
# the estimation of their DSM uncertainty (uncertainty of GAM models) and combining it with the 
# uncertainty of the detection function (the Delta method). This approach assumes independency 
# between the detection function and spatial modelling of density and abundance.


# Variance calculation of Density and Abundance estimates for Goitered gazelle using a spatial DSM (CDS) model 

varprop_gaz80hncos.xy.nb <- dsm.var.gam(gaz80hncos.xy.nb, grd.in, off.set)

summary(varprop_gaz80hncos.xy.nb)
# Approximate asymptotic confidence interval:
#      2.5%      Mean     97.5% 
#  2123.539  6995.329 23043.899 
# Point estimate                 : 6995.329 
# CV of detection function       : 0.2959453    # in thesis was 0.1261167
# CV from GAM                    : 0.6001       # in thesis was 0.6328
# Total standard error           : 4680.589 
# Total coefficient of variation : 0.6691       # in thesis was 0.6453


# so, CDS DF with same key but adjustment terms plus truncation and different error distribution improved the estimates 
# but got worse variance calculation for detection function and total CV, and slightly better GAM CV than in thesis 
# however total CV it's still higher than we wanted or expected (good Total CV is expected around 30%?)

# OK, all in all a CDS DF HN-cos adjs with 80m truncation maybe seemed not totally good, but spatial DSM with 
# these CDS DF and NB error distribution instead of TW (former in thesis) was an improvement, 
# giving greater abundance estimates, but it's still at an undesirable level of variance.


# Variance calculation of Density and Abundance estimates for Kulan using a spatial DSM (CDS) model

varprop_kul260ucos.xy.nb <- dsm.var.gam(kul260ucos.xy.nb, grd.in, off.set)

summary(varprop_kul260ucos.xy.nb)
# Approximate asymptotic confidence interval:
#      2.5%       Mean      97.5% 
#  65.52569  256.93790 1007.49929
# Point estimate                 : 256.9379
# CV of detection function       : 0.1319073        # in thesis was 0.146196
# CV from GAM                    : 0.78             # in thesis was 0.5366
# Total standard error           : 203.2626 
# Total coefficient of variation : 0.7911           # in thesis was 0.5562

# so, different key-adjustment term, truncation selected, and different error distribution 
# worsen the calculation variation (much worse total CV and GAM CV, slightly better DF CV) 

# OK, all in all we got slightly better abundance estimates with different CDS DF key-adjs terms + truncation 
# and DSM different error distribution (was DF: HN-no adjs no trunc DSM: TW in thesis), and even we got 
# slightly better DF CV with the change of key-adj and truncation, but change in DSM was worse for GAM CV, 
# giving a worst total CV (which was undesirable already).


# THERE'S NO CHANCE TO GO BACK TO ESTIMATES AND VARIANCE CALCULATION AS THOSE FROM THESIS (MAYBE DUE TO RECENT 
# IMPROVEMENTS IN DISTANCE AND MRDS PACKAGES?)


# Estimating the Detection Function in Multiple Covariate Distance Sampling (MCDS) #####

# Covariates on segment and observation/distance data files (covjune and distdata_gaz/kul):

# NVDI: vegetation index; most of the time correlated with habitat type
# hab (numeric): code for habitat type
# hab_name (text): code for habitat type
# VRM: "Vector Ruggedness Measure", measures terrain ruggedness
# Cost.Dist: cost distance between the camera and a source of water, it uses the VRM data
# CostPath: closest path of least resistance between the camera and water, using the cost distance data
# elev: elevation (m.a.s.l.)


# let's check class of every column on the files:

str(covjune)
# $ NVDI            : num  0.0334 0.0427 0.0364 0.0534 0.027 ...
# $ Vegetation.class: num  34 18 34 33 18 29 33 18 18 18 ...
# $ VRM             : num  0.00721 0.00116 0.00115 0.0015 0.0037 ...
# $ Cost.Distance   : num  4251 2594 3616 2779 2821 ...
# $ CostPath        : num  87 86 88 76 75 77 82 78 80 65 ...
# $ elev            : int  1505 1169 1256 1222 1459 1224 1248 1242 1277 1398 ...

# covariates that need to be numeric or integer are in the correct class

str(distdata_gaz)
# $ elev        : int  1037 1037 1037 1037 1037 1037 1037 1037 1037 1037 ...
# $ hab         : int  34 34 34 34 34 34 34 34 34 34 ...
# $ hab_name    : chr  "Salty haloxylon semi-desert" "Salty haloxylon semi-desert" 

# covariates that need to be numeric or integer are in the correct class

str(distdata_kul)
# $ elev        : int  1404 1404 1404 1404 1404 1404 1404 1404 1404 1404 ...
# $ hab         : int  18 18 18 18 18 18 18 18 18 18 ...
# $ hab_name    : chr  "Higher and intermediate dry steppe/ shrub communities" 

# covariates that need to be numeric or integer are in the correct class


names(covjune)

names(distdata_gaz)

names(distdata_kul)

# here in covjune, "Vegetation.class" column corresponds to "hab" column on distdata files

# need to change name on covjune to match name in distdata files

names(covjune)[names(covjune) == "Vegetation.class"] <- "hab"
names(covjune)   # worked
str(covjune)     # same class as in distdata files


# When I ran spatial DSM (CDS) models there were issues with number of unique values for the hab variable, 
# which only has 7 different values in covjune file and 2 to 3 different values in distdata files. So, I will
# include this as a factor in MCDS as I would in a glm/lm, and then as a factor or including it as a
# random effect bs="re" in multiple smooth DSM.

unique(covjune$hab) 
# 34 18 33 29 35 25 26 

# just seven different values for habitat covariate in covariate's file (we should aim to >10 to not get problems 
# with DSM engine when running Multiple Smooth DSM)

unique(distdata_gaz$hab)
# 34 18 33

unique(distdata_gaz80$hab)
# 34 18 33

unique(distdata_kul$hab)
# 18 34

unique(distdata_kul260$hab)
# 18 34

# First I need to say R to treat hab (integer or numeric) as a factor covariate, so need to transform it to see 
# its non-linear effect on detectability

class(covjune$hab)
# "numeric"

class(distdata_gaz$hab)
# "integer"
distdata_gaz$hab <- factor(distdata_gaz$hab)
class(distdata_gaz$hab)
# "factor"

class(distdata_gaz80$hab)
# "integer"
distdata_gaz80$hab <- factor(distdata_gaz80$hab)
class(distdata_gaz80$hab)
# "factor"

class(distdata_kul$hab)
# "integer"
distdata_kul$hab <- factor(distdata_kul$hab)
class(distdata_kul$hab)
# "factor"

class(distdata_kul260$hab)
# "integer"
distdata_kul260$hab <- factor(distdata_kul260$hab)
class(distdata_kul260$hab)
# "factor"


# Now we get into analysis. First, checking behaviour of covariates against distance


# Distance by habitat type

# 18 is dry steppe/shrub communities; 33 is haloxylon semi-desert and 34 is salty haloxylon semi-desert


boxplot(distdata_gaz$distance ~ distdata_gaz$hab, xlab = "Habitat type", ylab = "Distance (m)", 
        ylim = c(0, 130), main = "Gazelle no trunc distance ~ hab")

# seems that hab 18 has longer distances recorded, whereas 33 and 34 differ in their span but have similar 
# mean distance values recorded


boxplot(distdata_gaz80$distance ~ distdata_gaz80$hab, xlab = "Habitat type", ylab = "Distance (m)", 
        ylim = c(0, 90), main = "Gazelle 80m trunc distance ~ hab")

# same pattern


boxplot(distdata_kul$distance ~ distdata_kul$hab, xlab = "Habitat type", ylab = "Distance (m)", 
        ylim = c(0, 360), main = "Kulan no trunc distance ~ hab")

# opposite as gazelle, 18 has lower distances recorded than 34; they differ in span and mean (hab 34 with both greater)


boxplot(distdata_kul260$distance ~ distdata_kul260$hab, xlab = "Habitat type", ylab = "Distance (m)", 
        ylim = c(0, 280), main = "Kulan 260m trunc distance ~ hab")

# but with truncation the situation is inverse, hab 18 has longer mean distances recorded, and 34 still has greater span


# so we can say that habitat type influences differentially the distance of detection for each species, with
# greater distances recorded in "more vegetated" habitats
# (but does it affects our ability to spot animals and to record distances in timelapse images?).


# Distance by elevation

unique(distdata_gaz$elev)
# 1037 1055 1096 1355 1480 1543 1527 1242

unique(distdata_gaz80$elev)
# 1037 1055 1096 1355 1480 1543 1527 1242

unique(distdata_kul$elev)
# 1404 1461   NA 1272 1480

unique(distdata_kul260$elev)
# 1404 1461   NA 1272 1480


# There's no elevation for CT R27 in distdata_kul, distdata_kul260 and covjune; and doesn't exists in any 
# of files I have so I need to fill elev column info for R27 (which has only NAs on them); 
# in GGASPA R27 is spatially between R26 and R28; R26 was eliminated because it didn't yield any images, so I will fill
# elevation column for R27 with the same value than for R28 (I know it's not correct, but just for now...)

distdata_kul$elev[distdata_kul$Sample.Label == "R27"] <- 1272

distdata_kul260$elev[distdata_kul260$Sample.Label == "R27"] <- 1272

covjune$elev[covjune$Sample.Label == "R27"] <- 1272


unique(covjune$elev)
# here there are a lot of different values, almost same number as CT, but NAs are gone

unique(distdata_kul$elev)
# 1404 1461 1272 1480

unique(distdata_kul260$elev)
# 1404 1461 1272 1480

# Although in covjune there are a lot of different elev values, in distdata files there are only between 
# 4 and 8 different elevation values, so I will transform this covariate as a factor as I did with hab

class(distdata_gaz$elev)
# "integer"
distdata_gaz$elev <- factor(distdata_gaz$elev)
class(distdata_gaz$elev)
# "factor"

class(distdata_gaz80$elev)
# "integer"
distdata_gaz80$elev <- factor(distdata_gaz80$elev)
class(distdata_gaz80$elev)
# "factor"

class(distdata_kul$elev)
# "numeric"
distdata_kul$elev <- factor(distdata_kul$elev)
class(distdata_kul$elev)
# "factor"

class(distdata_kul260$elev)
# "numeric"
distdata_kul260$elev <- factor(distdata_kul260$elev)
class(distdata_kul260$elev)
# "factor"


# Now back to the analysis

boxplot(distdata_gaz$distance ~ distdata_gaz$elev, xlab = "elevation (m)", ylab = "Distance (m)", 
        ylim = c(0, 130), main = "Gazelle no trunc distance ~ elevation")

# not clear pattern, goes up and down; but seems to be something about a certain level of elevation until
# it goes up and then down


boxplot(distdata_gaz80$distance ~ distdata_gaz80$elev, xlab = "elevation (m)", ylab = "Distance (m)", 
        ylim = c(0, 90), main = "Gazelle 80m trunc distance ~ elevation")

# same not clear pattern


boxplot(distdata_kul$distance ~ distdata_kul$elev, xlab = "elevation (m)", ylab = "Distance (m)", 
        ylim = c(0, 360), main = "Kulan no trunc distance ~ elevation")

# distance recorded diminishes as elevation increases


boxplot(distdata_kul260$distance ~ distdata_kul260$elev, xlab = "elevation (m)", ylab = "Distance (m)", 
        ylim = c(0, 280), main = "Kulan 260m trunc distance ~ elevation")

# different pattern; begins lower, go up and then goes down. First and third elevation value have same mean distance
# (with the truncation at 260m, distances recorded for the first and fourth elevation value diminishes, 
# with lower mean distance)

# so, as for Gazelle there is no clear pattern, with Kulan the tendency is to lower distances recorded 
# at higher elevation


# From the analysis of box plots, I can say that maybe adding habitat and elevation 
# could influence the fit of the detection function
# I will use and try hab-elevation as covariates in MCDS looking for a detection function, although in this case 
# I have my reservations about their influence in detectability, I'm more sure that they affect encounter rate and 
# have to be put into a DSM instead
# Let's see what I get...


# Then, I will look for correlation between these two covariates in both species

# If there is collinearity between certain covariates and they are included in the detection
# function, they will explain sort of the same variation in the recorded distances (detectability)
# reducing their importance as a potential covariate


# I did this evaluation before transform these covariates into factor, because correlation analysis
# requires numeric variables to be performed

# Habitat type and Elevation correlation analysis

cor(distdata_gaz$hab, distdata_gaz$elev)
# -0.6346694; the correlation exists, is mild and negative

plot(distdata_gaz$hab ~ distdata_gaz$elev, xlab = "Habitat", ylab = "Elevation (m)", 
     main = "Correlation plot Gazelle no trunc Hab ~ Elev")


cor(distdata_gaz80$hab, distdata_gaz80$elev)
# -0.61425; the correlation exists, is still mild and negative

plot(distdata_gaz80$hab ~ distdata_gaz80$elev, xlab = "Habitat", ylab = "Elevation", 
     main = "Correlation plot Gazelle 80m trunc Hab ~ Elev")


cor(distdata_kul$hab, distdata_kul$elev)
# -0.9319193; correlation exists, is high and negative

plot(distdata_kul$hab ~ distdata_kul$elev, xlab = "Habitat", ylab = "Elevation", 
     main = "Correlation plot Kulan no trunc Hab ~ Elev")

# visual inspection is not as clear as correlation level suggests


cor(distdata_kul260$hab, distdata_kul260$elev)
# -0.9183377; the correlation exists, is still high and negative

plot(distdata_kul260$hab ~ distdata_kul260$elev, xlab = "Habitat", ylab = "Elevation", 
     main = "Correlation plot Kulan 260m trunc Hab ~ Elev")

# visual inspection is not as clear as correlation level suggests

# So, I will have to be careful when include both covariates in the same DF model


# The other covariates are purely environmental covariates which influences encounter rate of animals 
# (explaining their spatial distribution). In this ecosystem, vegetation is not dense so either habitat type 
# offers a clear field of view (even though there is a difference on recorded distance between habitat types)
# and elevation has a clearer pattern for Kulan than for Gazelle.


# Just considering the inclusion of 2 covariates to the analysis, there are multiple possible combinations 
# for each of their values; if we also consider key/adjustment term combinations, the possibilities are several.
# It's not of use to try every possible combination. See Buckland et al 2015 to see a subset of suitable combinations


# In CDS I used three different key functions combined with selected adjustment terms to which "they get along well"
# (as said by Len Thomas on his lecture, find out a reference for that... maybe Buckland et al 2015), those were:
# Half normal-Hazard rate without adjustment terms (2 combinations, can't use Uniform without adjustment terms)
# Unif-cos, HN-herm, HN-cos, HR-poly (4 combinations)


# But here, Uniform key does not allow to use covariates at same time; if we use covariates with any other
# key, we can not use adjustment terms simultaneously. 

# So, I will not use adjustment terms, as recommended by Eric Rexstad and David Miller in their practicals. 
# My list ended up trying just HN and HR keys with no adjustments terms.


# Looking for Detection Function in MCDS that best fits Goitered gazelle distance data #### 

# first, I need to check if there are NA values that can cause models not run

sum(is.na(distdata_gaz$hab)) # 0
sum(is.na(distdata_gaz$elev)) # 0


sum(is.na(distdata_gaz80$hab)) # 0
sum(is.na(distdata_gaz80$elev)) # 0


# DF in MCDS (elev) with HN key no adjustments no truncation

gaz.hn.elev <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", 
                  adjustment = NULL, formula = ~elev)

check.mono(gaz.hn.elev$ddf)
# [1] TRUE

summary(gaz.hn.elev)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 727.8508 
# No Chi-square p-value
# No degrees of freedom for test, so impossible to check overdispersion (Chi-square p/degrees of freedom)


# With the following code I will generate a plot of the fitted detection function and quantile-quantile plot:
# pdf: probability density function. Plotting option only available for point transects, shows how well 
# fitted is the detection function key to the raw distance data

par(mfrow = c(1, 2))
plot(gaz.hn.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hn.elev")
ddf.gof(gaz.hn.elev$ddf, main ="gaz.hn.elev")
dev.off()

# not so good really

# Checking distribution of probability of detections (Pa) in case truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(gaz.hn.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     8       0.10
# 0.1 - 0.2    15       0.19
# 0.2 - 0.3    38       0.47
# 0.3 - 0.4     0       0.00
# 0.4 - 0.5    19       0.24
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.024 - 0.44

# truncation is needed (more than 5% Pa < 0.2); let's see what I get using distdata_gaz80


plot(gaz.hn.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hn.elev")

# in PDF plot (observed detection distances scaled up by expected detection distances from the model) 
# number of detections fell rapidly beyond 95m, but in DF plot detection probability is < 0.15 at 80m
# I will try here with right truncation at 80m as in MCDS


# DF in MCDS (hab) with HN key no adjustments no truncation

gaz.hn.hab <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", 
                 adjustment = NULL, formula = ~hab)

check.mono(gaz.hn.hab$ddf)
# [1] TRUE

summary(gaz.hn.hab)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 737.4142
# Chi-square p: 0.082546 with 5 degrees of freedom            # barely > 0.05, but retain anyway
0.082546/5
# 0.0165092, no possibility of overdispersion

par(mfrow = c(1, 2))
plot(gaz.hn.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hn.hab")
ddf.gof(gaz.hn.hab$ddf, main ="gaz.hn.hab")
dev.off()

# it's just because of the scale that pdf looks more "normal", but is as strange as others, and its q-q plot too
# but having a Chi-square p value makes it better than other models (maybe because of less categories
# of values, 8 elevation instead of 3 hab?)


# Checking distribution of probability of detections (Pa) in case truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(gaz.hn.hab, proportion = TRUE)
#               p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2    12       0.15
# 0.2 - 0.3    68       0.85
# 0.3 - 0.4     0       0.00
# 0.4 - 0.5     0       0.00
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.16 - 0.28

# so, truncation needed (more than 5% Pa < 0.2)

plot(gaz.hn.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hn.hab")

# still, 80m truncation seems right


# DF in MCDS (elev+hab) with HN key no adjustments no truncation

gaz.hn.elev.hab <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hn", 
                      adjustment = NULL, formula = ~hab+elev)

# lots of warnings:
# missing value where TRUE/FALSE needed, 
# Model is not full rank - not all parameters are estimable)
# Model failed to converge.
# No models could be fitted.


# DF in MCDS (elev) with HN key no adjustments 80m truncation

gaz80.hn.elev <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hn", 
                    adjustment = NULL, formula = ~elev)

check.mono(gaz80.hn.elev$ddf)
# [1] TRUE

summary(gaz80.hn.elev)
# Number of observations :  71 
# Distance range         :  0  -  80 
# AIC   : 610.0127 
# No Chi-square p
# No degrees of freedom for test, so impossible to check overdispersion (Chi-square p/degrees of freedom)

par(mfrow = c(1, 2))
plot(gaz80.hn.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.elev")
ddf.gof(gaz80.hn.elev$ddf, main ="gaz80.hn.elev")
dev.off()

# again, not good looking

p_dist_table(gaz80.hn.elev, proportion = TRUE)
#               p count proportion
#   0 - 0.1     3      0.042
# 0.1 - 0.2     5      0.070
# 0.2 - 0.3     2      0.028
# 0.3 - 0.4    11      0.155
# 0.4 - 0.5     0      0.000
# 0.5 - 0.6    36      0.507
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8     0      0.000
# 0.8 - 0.9     0      0.000
#   0.9 - 1    14      0.197
# Range of probabilities:  0.054 - 1

# again, seems to need further truncation (any Pa < 0.1)

plot(gaz80.hn.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.elev")

# here, detection probability is < 0.15 at 70m, but I will stick with this truncation distance 
# (tried further truncation and results were as bad as here)


# DF in MCDS (hab) with HN key no adjustments 80m truncation

gaz80.hn.hab <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hn", 
                   adjustment = NULL, formula = ~hab)

check.mono(gaz80.hn.hab$ddf)
# [1] TRUE

summary(gaz80.hn.hab)
# Number of observations :  71 
# Distance range         :  0  -  80 
# AIC   : 618.8886
# Chi-square P = 0.37841 with 4 degrees of freedom      # way > 0.05, so retain!
0.37841/4
# 0.0946025, no overdispersion

par(mfrow = c(1, 2))
plot(gaz80.hn.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.hab")
ddf.gof(gaz80.hn.hab$ddf, main ="gaz80.hn.hab")
dev.off()

# as bad as previous model with no truncation, better looking because of scale

p_dist_table(gaz80.hn.hab, proportion = TRUE)
#               p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3     0       0.00
# 0.3 - 0.4    11       0.15
# 0.4 - 0.5     0       0.00
# 0.5 - 0.6    37       0.52
# 0.6 - 0.7    23       0.32
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.32 - 0.6

# solve the problem than no trunc, even better than gaz.hn.elev and its truncations; no p < 0.2, the best so far

plot(gaz80.hn.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hn.hab")

# no more truncation needed


# DF in MCDS (elev+hab) with HN key no adjustments 80m truncation

gaz80.hn.elev.hab <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hn", 
                        adjustment = NULL, formula = ~elev+hab)

# lots of warnings:
# number of items to replace is not a multiple of replacement length, 
# Model is not full rank - not all parameters are estimable)
# Model failed to converge.
# No models could be fitted.


# DF in MCDS (elev) with HR key no adjustments no trunc

gaz.hr.elev <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hr", 
                  adjustment = NULL, formula = ~elev)

check.mono(gaz.hr.elev$ddf)
# [1] TRUE

summary(gaz.hr.elev)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 724.6202
# No Chi-square p value, no degrees of freedom

par(mfrow = c(1, 2))
plot(gaz.hr.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hr.elev")
ddf.gof(gaz.hr.elev$ddf, main ="gaz.hr.elev")
dev.off()

# weird, there are two peaks with the second lower but said is monotonically decreasing, and there's no q-q plot, 
# so also no Chi-square p value

p_dist_table(gaz.hr.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     4      0.050
# 0.1 - 0.2     6      0.075
# 0.2 - 0.3    13      0.163
# 0.3 - 0.4    38      0.475
# 0.4 - 0.5     0      0.000
# 0.5 - 0.6    19      0.237
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8     0      0.000
# 0.8 - 0.9     0      0.000
#   0.9 - 1     0      0.000
# Range of probabilities:  0.045 - 0.58

# truncation needed (more than 5% Pa < 0.2)

plot(gaz.hr.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hr.elev")

# odd looking, lots of elevation values mantain for so long at g(x)=1 and then fall rapidly (due to the "shoulder"
# of the key I guess), detection probability < 0.15 is still at 80m.


# DF in MCDS (hab) with HR key no adjustments no trunc

gaz.hr.hab <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hr", 
                 adjustment = NULL, formula = ~hab)

check.mono(gaz.hr.hab$ddf)
# [1] TRUE

summary(gaz.hr.hab)
# Number of observations :  80 
# Distance range         :  0  -  120.5449 
# AIC   : 740.7407
# Chi-square P = 0.0037743 with 4 degrees of freedom      # < 0.05, so don't retain? but its plots looks ok-ish
0.0037743/4
# 0.000943575; no overdispersion

par(mfrow = c(1, 2))
plot(gaz.hr.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hr.hab")
ddf.gof(gaz.hr.hab$ddf, main ="gaz.hr.hab")
dev.off()

# pdf plot looks more as a normal pdf plot for point transects; q-q plot is not well fitted

p_dist_table(gaz.hr.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3    12       0.15
# 0.3 - 0.4    40       0.50
# 0.4 - 0.5    28       0.35
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.27 - 0.48

# cool! third best Pa distribution so far, no Pa < 0.2; maybe with hab as a covariate there's no need of truncation
# the best was gaz80.hn.hab

plot(gaz.hr.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz.hr.hab")

# there's a shoulder for long before fell down, but different hab categories follow the shape of detection function
# closer than in gaz.hr.elev. Detection probability is < 0.15 at 80m more or less.


# DF in MCDS (elev+hab) with HR key no adjustments no trunc

gaz.hr.elev.hab <- ds(distdata_gaz, max(distdata_gaz$distance), transect = "point", key = "hr", 
                      adjustment = NULL, formula = ~elev+hab)

# Warning in ddf.ds(model = dsmodel, data, meta.data = meta.data, control = control,  :
# Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
# Error in integrate(dpdf, lower = x[i, 1], upper = x[i, 2], width = width[i],  : 
# non-finite function value
# In addition: There were 49 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_gaz, max(distdata_gaz$distance), transect = "point",  : 
# No models could be fitted.


# DF in MCDS (elev) with HR key no adjustments 80m trunc

gaz80.hr.elev <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hr", 
                    adjustment = NULL, formula = ~elev)

check.mono(gaz80.hr.elev$ddf)
# [1] TRUE

summary(gaz80.hr.elev)
# Number of observations :  71 
# Distance range         :  0  -  80 
# AIC   : 613.2183
# No Chi-square p-values, no degrees of freedom

par(mfrow = c(1, 2))
plot(gaz80.hr.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr.elev")
ddf.gof(gaz80.hr.elev$ddf, main ="gaz80.hr.elev")
dev.off()

# pdf looks ugly, but q-q plot is better fitted than any others

p_dist_table(gaz80.hr.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     7      0.099
# 0.1 - 0.2     0      0.000
# 0.2 - 0.3     1      0.014
# 0.3 - 0.4     0      0.000
# 0.4 - 0.5    11      0.155
# 0.5 - 0.6    37      0.521
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8     1      0.014
# 0.8 - 0.9     0      0.000
#   0.9 - 1    14      0.197
# Range of probabilities:  0.051 - 1 

# not good, further truncation needed (any Pa < 0.1)

plot(gaz80.hr.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr.elev")
# I tried with further truncation and was not of use, stick with 80m.


# DF in MCDS (hab) with HR key no adjustments 80m trunc 

gaz80.hr.hab <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hr", 
                   adjustment = NULL, formula = ~hab)

check.mono(gaz80.hr.hab$ddf)
# [1] TRUE

summary(gaz80.hr.hab)
# Number of observations :  71 
# Distance range         :  0  -  80 
# AIC   : 620.8771
# Chi-square P = 0.39593 with 3 degrees of freedom      # way > 0.05, so retain model!
0.39593/3
# 0.1319767, no overdispersion

par(mfrow = c(1, 2))
plot(gaz80.hr.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr.hab")
ddf.gof(gaz80.hr.hab$ddf, main ="gaz80.hr.hab")
dev.off()

# soooo weird looking pdf plot, but q-q plot seems ok fitted

p_dist_table(gaz80.hr.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3     0       0.00
# 0.3 - 0.4    34       0.48
# 0.4 - 0.5    37       0.52
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.3 - 0.41

# second best Pa so far, better than its counterpart gaz.hr.hab; so even though truncation wasn't needed, it improved

plot(gaz80.hr.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="gaz80.hr.hab")

# not much of a shoulder here, but looks "well behaved"; hab categories follow shape of detection function
# seems no further truncation needed.


# DF in MCDS (elev+hab) with HR key no adjustments 80m trunc

gaz80.hr.elev.hab <- ds(distdata_gaz, truncation = 80, transect = "point", key = "hr", 
                        adjustment = NULL, formula = ~elev+hab)

# Warning in ddf.ds(model = dsmodel, data, meta.data = meta.data, control = control,  :
# Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
# Error in integrate(dpdf, lower = x[i, 1], upper = x[i, 2], width = width[i],  : 
# non-finite function value
# In addition: There were 49 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_gaz, max(distdata_gaz$distance), transect = "point",  : 
# No models could be fitted.


# Now I can decide on Gazelle DF Models (with/without truncation) in MCDS 


# MCDS DF model selection by AIC for Gazelle no truncation ####


df.mcds.gaz.notrunc <- list(gaz.hn.elev, gaz.hn.hab, gaz.hr.elev, gaz.hr.hab)   

# here the only model retained is gaz.hn.hab by its Chi-square p value > 0.05 (but barely, Chi-square p = 0.08)
# so I included also models without calculated p-value or that has not to be retained, or we lack of enough models


gaz.aic.mcds.notrunc <- kable(summarize_ds_models(gaz.hn.elev, gaz.hn.hab, gaz.hr.hab, 
                                                  sort = "AIC", delta_only = FALSE, output = "plain"), 
                              caption = "Summary of Gazelle MCDS no trunc models AIC analysis", digits = 3)

# Error in integrate(dpdf, lower = x[i, 1], upper = x[i, 2], width = width[i],  : 
# the integral is probably divergent

# for some reason I don't understand, including gaz.hr.elev throws error and makes impossible to make table
# (this model was the weirdest pdf plot, with two peaks). When it's eliminated, code ran


gaz.aic.mcds.notrunc

# Table: Summary of Gazelle MCDS no trunc models AIC analysis

#|Model       |Key function |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:-----------|:------------|:-------|--------------:|-------:|---------:|
#|gaz.hn.elev |Half-normal  |~elev   |          0.340| 727.851|     0.000|
#|gaz.hn.hab  |Half-normal  |~hab    |          0.462| 737.414|     9.563|
#|gaz.hr.hab  |Hazard-rate  |~hab    |          0.179| 740.741|    12.890|

# According to AIC selection criterion, the best MCDS model is gaz.hn.elev, with >2 difference with the next model
# on the round. Surprisingly, selected model didn't have Chi-square p value. Still bear in mind that gaz.hn.hab
# was retained according its Chi-square p value.


modlist.gaz.aic.mcds.notrunc <- AIC(gaz.hn.elev, gaz.hn.hab, gaz.hr.elev, gaz.hr.hab)

knitr::kable(modlist.gaz.aic.mcds.notrunc, caption = "AIC values for Gazelle MCDS no trunc models")

# Table: AIC values for Gazelle MCDS no trunc models
#|            | df|      AIC|
#|:-----------|--:|--------:|
#|gaz.hn.elev |  8| 727.8508|
#|gaz.hn.hab  |  3| 737.4142|
#|gaz.hr.elev |  9| 724.6202|
#|gaz.hr.hab  |  4| 740.7407|

# Here the order is different; gaz.hr.elev is the model with lower AIC, second best is gaz.hn.elev with a difference
# >3 points, even though gaz.hr.hab has the best distribution probability
# Anyway, I can't compute QAIC if gaz.hr.elev is included, so I will work only with gaz.hn.elev, gaz.hn.hab and
# gaz.hr.hab as the only existing models for comparison.


# MCDS DF model selection by QAIC for Gazelle no truncation ####

# Computing QAIC for HN key function models for MCDS Gaz no trunc

# here I can only select DF models within HN family, because I have only one model in HR family.


gaz.mcds.hn.qaic <- QAIC(gaz.hn.elev, gaz.hn.hab)

gaztab.mcds.hn.qaic <- knitr::kable(gaz.mcds.hn.qaic, caption="Gaz MCDS QAIC values HN key no trunc models.")

gaztab.mcds.hn.qaic

# Table: Gaz MCDS QAIC values HN key no trunc models.
#|            | df| QAIC|
#|:-----------|--:|----:|
#|gaz.hn.elev |  8|   NA|
#|gaz.hn.hab  |  3|   NA|

# No QAIC value was computed because there were no Chi-square p-value calculated for gaz.hn.elev
# but there was Chi-square p-value and c^ value for gaz.hn.hab, so don't understand. 
# Besides, I can't compute QAIC for only one model

# Anyway, I selected gaz.hn.hab as MCDS DF model within HN key family because, no matter gaz.hn.elev has 
# lower AIC value, we can compute a c^ value for gaz.hn.hab also.
# (I did try to use gaz.hn.elev instead of gaz.hn.hab, and c^ value can't be computed, giving the false idea of 
# HR key function being the best option, when was HN key function instead as seen in the next analysis)


# The second step of model selection ranks the models by their c^ values.

chats_df.gaz.mcds.notrunc <- chi2_select(gaz.hn.hab, gaz.hr.hab)

# as all models included in this formula must be different key functions, 
# I selected within HN key the one that has c^ value (gaz.hn.hab)

modnames5 <- unlist(lapply(list(gaz.hn.hab, gaz.hr.hab), function(x) x$ddf$name.message))

results_df.gaz.mcds.notrunc <- data.frame(modnames5, chats_df.gaz.mcds.notrunc)

results.sort_df.gaz.mcds.notrunc <- results_df.gaz.mcds.notrunc[order(results_df.gaz.mcds.notrunc$criteria),]

gaz.qaic.mcds.notrunc <- knitr::kable(results.sort_df.gaz.mcds.notrunc, digits=2, row.names = FALSE,
                                      caption="MCDS DF models Gazelle no trunc ranked by their c^ value")

gaz.qaic.mcds.notrunc

# Table: MCDS DF models Gazelle no trunc ranked by their c^ value
#|modnames5                | criteria|
#|:------------------------|--------:|
#|half-normal key function |     1.95|
#|hazard-rate key function |     3.87|

# Here, for MCDS QAIC DF model selection Gazelle no trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is different in covariate but same key function (half-normal no adjustments; gaz.hn.hab) 
# as it had been chosen by conventional model selection based on AIC values (half-normal no adjustments; gaz.hn.elev).

# so, selected model is gaz.hn.hab in MCDS Gazelle no truncation
# (the third option according to AIC, which is not the best analysis to tell).


# MCDS DF model selection by AIC for Gazelle 80m truncation ####

df.mcds.gaz.trunc <- list(gaz80.hn.elev, gaz80.hn.hab, gaz80.hr.elev, gaz80.hr.hab)  

# gaz80.hn.elev, gaz80.hr.elev, don't have Chi-square p value, but were included or we lack of enough models
# gaz80.hn.hab and gaz80.hr.hab have Chi-square p-value computed and were retained


gaz.aic.mcds.trunc <- kable(summarize_ds_models(gaz80.hn.elev, gaz80.hn.hab, gaz80.hr.elev, gaz80.hr.hab, 
                                                sort = "AIC", delta_only = FALSE, output = "plain"), 
                            caption = "Summary of Gazelle MCDS 80m trunc models AIC analysis", digits = 3)

gaz.aic.mcds.trunc

# Table: Summary of Gazelle MCDS 80m trunc models AIC analysis
#|   |Model         |Key function |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:--|:-------------|:------------|:-------|--------------:|-------:|---------:|
#|1  |gaz80.hn.elev |Half-normal  |~elev   |          0.548| 610.013|     0.000|
#|3  |gaz80.hr.elev |Hazard-rate  |~elev   |          0.811| 613.218|     3.206|
#|2  |gaz80.hn.hab  |Half-normal  |~hab    |          0.521| 618.889|     8.876|
#|4  |gaz80.hr.hab  |Hazard-rate  |~hab    |          0.925| 620.877|    10.864|

# According to AIC selection criterion, for Gazelle the best MCDS DF model with 80m truncation is gaz80.hn.elev, 
# with >2 difference with the next model on the round. Surprisingly, selected model and its second best didn't have 
# computed Chi-square p value. 
# Still bear in mind that gaz80.hn.hab and gaz80.hr.hab were retained according its Chi-square p value


# MCDS DF model selection by QAIC for Gazelle 80m truncation ####

# Fist step: Computing QAIC for HN and HR family key functions models for MCDS Gaz 80m trunc

gaz80.mcds.hn.qaic <- QAIC(gaz80.hn.elev, gaz80.hn.hab)

gaz80.mcds.hr.qaic <- QAIC(gaz80.hr.elev, gaz80.hr.hab)


# Tables of QAIC values for each key function family are shown below 

gaz80tab.mcds.hn.qaic <- knitr::kable(gaz80.mcds.hn.qaic, caption="Gaz MCDS QAIC values HN key 80m trunc models.")

gaz80tab.mcds.hn.qaic

#Table: Gaz MCDS QAIC values HN key 80m trunc models.
#|              | df| QAIC|
#|:-------------|--:|----:|
#|gaz80.hn.elev |  8|   NA|
#|gaz80.hn.hab  |  3|   NA|

# Again, no QIAC value was computed because there were no Chi-square p-value calculated for gaz.hn.elev
# but there was c^ value for gaz80.hn.hab, so don't understand. Besides, I can't compute QAIC for only one model
# Anyway, I selected gaz80.hn.hab as MCDS DF model within HN key family to go next round because, 
# no matter gaz80.hn.elev has lower AIC value, we can compute a c^ value for gaz80.hn.hab also.


gaz80tab.mcds.hr.qaic <- knitr::kable(gaz80.mcds.hr.qaic, caption="Gaz MCDS QAIC values HR key 80m trunc models")

gaz80tab.mcds.hr.qaic

#Table: Gaz MCDS QAIC values HR key 80m trunc models
#|              | df| QAIC|
#|:-------------|--:|----:|
#|gaz80.hr.elev |  9|   NA|
#|gaz80.hr.hab  |  4|   NA|

# Same. No QAIC computed besides gaz80.hr.hab has Chi-square p-value but gaz80.hr.elev don't, and I can't compute QAIC
# for just one model. So, selected was gaz80.hr.hab as MCDS DF model within HR key family to go next round because 
# can be computed a c^ value for it


# The second step of model selection ranks the models by their c^ values.

chats_df.gaz.mcds.trunc <- chi2_select(gaz80.hn.hab, gaz80.hr.hab)

# all models included in this formula must be different key functions 

modnames6 <- unlist(lapply(list(gaz80.hn.hab, gaz80.hr.hab), function(x) x$ddf$name.message))

results_df.gaz.mcds.trunc <- data.frame(modnames6, chats_df.gaz.mcds.trunc)

results.sort_df.gaz.mcds.trunc <- results_df.gaz.mcds.trunc[order(results_df.gaz.mcds.trunc$criteria),]

gaz.qaic.mcds.trunc <- knitr::kable(results.sort_df.gaz.mcds.trunc, digits=2, row.names = FALSE,
                                    caption="MCDS DF models Gazelle 80m trunc ranked by their c^ value")

gaz.qaic.mcds.trunc

# Table: MCDS DF models Gazelle 80m trunc ranked by their c^ value
#|modnames6                | criteria|
#|:------------------------|--------:|
#|hazard-rate key function |     0.99|
#|half-normal key function |     1.05|

# Here, for MCDS QAIC DF model selection Gazelle 80m trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is obviously different in key function and covariate (hazard rate no adjustments; gaz80.hr.hab) 
# as it had been chosen by conventional model selection based on AIC values (half-normal no adjustments; gaz80.hn.elev).
# More surprisingly, selected model by QAIC path has the worse AIC value, with >10 units of difference with selected
# model by this path.

# Anyway, I select gaz80.hr.hab as a MCDS DF Gazelle 80m trunc model


# Then, in MCDS DF models for Gazelle, between trunc and no trunc models (although not comparable because of 
# different truncation):

AIC(gaz.hn.hab, gaz80.hr.hab)
#              df      AIC
# gaz.hn.hab    3 737.4142
# gaz80.hr.hab  4 620.8771

# the model with truncation distance obviously has lower AIC but also better Chi-square p values 
# than the one without truncation (0.3 vs 0.08), and certainly would provide more accurate estimations of abundance 
# (even has Pa in a range greater than not truncated model, 0.3 vs 0.16 which clearly needed further truncation)

# So, in MCDS the DF model selected for Goitered gazelle is gaz80.hr.hab ####
# (in CDS gaz80.hn.cos was the model selected; so there a big difference in key function selected here) 
# (in thesis was gaz.hn no adjustments).


# Looking for Detection Function in MCDS that best fits Kulan distance data ####


# First, I need to check if there are NA values from the covariates to be used; if so change them

sum(is.na(distdata_kul$hab)) # 0
sum(is.na(distdata_kul$elev)) # 0

sum(is.na(distdata_kul260$hab)) # 0
sum(is.na(distdata_kul260$elev)) # 0


# DF in MCDS(elev) with HN key no adjustments no truncation

kul.hn.elev <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", 
                  adjustment = NULL, formula = ~elev)

check.mono(kul.hn.elev$ddf)
# [1] TRUE

summary(kul.hn.elev)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 519.8769 
# Chi-square P = 0.27455 with 2 degrees of freedom        # >> 0.05, so retain!
0.27464/2
# 0.13732; no overdispersion

par(mfrow = c(1, 2))
plot(kul.hn.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn.elev")
ddf.gof(kul.hn.elev$ddf, main ="kul.hn.elev")
dev.off()

# not much of a good looking here for pdf, also not well fitted q-q plot

# Checking distribution of probability of detections (Pa) in case truncation is needed:
# * consider reducing truncation if more than 5% of Pa is <0.2 or if any is <0.1

p_dist_table(kul.hn.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     4      0.087
# 0.1 - 0.2     7      0.152
# 0.2 - 0.3    23      0.500
# 0.3 - 0.4    12      0.261
# 0.4 - 0.5     0      0.000
# 0.5 - 0.6     0      0.000
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8     0      0.000
# 0.8 - 0.9     0      0.000
#   0.9 - 1     0      0.000
# Range of probabilities:  0.042 - 0.4

# very bad distribution, truncation is needed (more than 5% of Pa < 0.2 and any < 0.1); 

plot(kul.hn.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn.elev")

# seems that at 260m detection probability is =< 0.15, so I will right truncate at this distance in a further stage
# (but is it where this imaginary line crosses the distance bins, which is at 260m, or is it where it crosses
# the line of fit, which is at 220m?? take in consideration if further truncation is needed).


# DF in MCDS(hab) with HN key no adjustments no truncation

kul.hn.hab <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", 
                 adjustment = NULL, formula = ~hab)

check.mono(kul.hn.hab$ddf)
# [1] TRUE

summary(kul.hn.hab)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 524.6567
# Chi-square P = 0.70935 with 4 degrees of freedom        # >> 0.05, so retain!
0.70935/4
# 0.1773375, no overdispersion

par(mfrow = c(1, 2))
plot(kul.hn.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn.hab")
ddf.gof(kul.hn.hab$ddf, main ="kul.hn.hab")
dev.off()

# good looking pdf plot; no so well fitted q-q plot though

p_dist_table(kul.hn.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2    34       0.74
# 0.2 - 0.3     0       0.00
# 0.3 - 0.4    12       0.26
# 0.4 - 0.5     0       0.00
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.19 - 0.4

# bad detection probability range and also 74% detections with p < 0.2, so further truncation needed

plot(kul.hn.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hn.hab")
# seems that detection probability =< 0.15 at 260m (at bins) but 220m at line of fit.


# DF in MCDS(elev+hab) with HN key no adjustments no truncation

kul.hn.elev.hab <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hn", 
                      adjustment = NULL, formula = ~elev+hab)

# Error in while (any(int1 < 0) & (i < 2)) { : 
# missing value where TRUE/FALSE needed
# In addition: There were 49 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_kul, max(distdata_kul$distance), transect = "point",  : 
# No models could be fitted.


# DF in MCDS(elev) with HN key no adjustments 260m truncation

kul260.hn.elev <- ds(distdata_kul, truncation = 260, transect = "point", key = "hn", 
                     adjustment = NULL, formula = ~elev)

check.mono(kul260.hn.elev$ddf)
# [1] TRUE

summary(kul260.hn.elev)
# Number of observations :  42          # looses about 9% of observations
# Distance range         :  0  -  260 
# AIC   : 457.3469 
# Chi-square P = 0.13772 with 1 degrees of freedom          # > 0.05, so retain!
0.13771/1
# 0.13771; no overdispersion

par(mfrow = c(1, 2))
plot(kul260.hn.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn.elev")
ddf.gof(kul260.hn.elev$ddf, main ="kul260.hn.elev")
dev.off()

# odd looking pdf but better fitted q-q plot than not truncated version

p_dist_table(kul260.hn.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     4      0.095
# 0.1 - 0.2     0      0.000
# 0.2 - 0.3     7      0.167
# 0.3 - 0.4     0      0.000
# 0.4 - 0.5     9      0.214
# 0.5 - 0.6    22      0.524
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8     0      0.000
# 0.8 - 0.9     0      0.000
#   0.9 - 1     0      0.000
# Range of probabilities:  0.082 - 0.54

# worse Pa distribution than in not truncated version (any Pa < 0.1)

plot(kul260.hn.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn.elev")

# seems that detection probability is =< 0.15 at 220m; I tried further truncation before and they were not
# better options than this truncation.


# DF in MCDS(hab) with HN key no adjustments 260m truncation

kul260.hn.hab <- ds(distdata_kul, truncation = 260, transect = "point", key = "hn", 
                    adjustment = NULL, formula = ~hab)

check.mono(kul260.hn.hab$ddf)
# [1] TRUE

summary(kul260.hn.hab)
# Number of observations :  42            # looses about 9% of observations
# Distance range         :  0  -  260 
# AIC   : 462.1231 
# Chi-square P = 0.60072 with 3 degrees of freedom      # >> 0.05, retain!
0.60072/3
# 0.20024, no overdispersion

par(mfrow = c(1, 2))
plot(kul260.hn.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn.hab")
ddf.gof(kul260.hn.hab$ddf, main ="kul260.hn.hab")
dev.off()

# nice looking pdf plot, but not well fitted q-q plot

p_dist_table(kul260.hn.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3     0       0.00
# 0.3 - 0.4    33       0.79
# 0.4 - 0.5     9       0.21
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.37 - 0.48

# cool! best Pa distribution so far, even though not very restricted, 0.37 > p < 0.48; no further truncation needed 

plot(kul260.hn.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hn.hab")


# DF in MCDS(elev+hab) with HN key no adjustments 260m truncation

kul260.hn.elev.hab <- ds(distdata_kul, truncation = 260, transect = "point", key = "hn", 
                         adjustment = NULL, formula = ~elev+hab)

# Error in while (any(int1 < 0) & (i < 2)) { : 
# missing value where TRUE/FALSE needed
# In addition: There were 49 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_kul, max(distdata_kul$distance), transect = "point",  : 
# No models could be fitted.


# DF in MCDS(elev) with HR key no adjustments no truncation

kul.hr.elev <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hr", 
                  adjustment = NULL, formula = ~elev)

check.mono(kul.hr.elev$ddf)
# [1] TRUE

summary(kul.hr.elev)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 521.1881 
# Chi-square P = 0.031908 with 1 degrees of freedom     # < 0.05, so don't retain
0.031955/1
# 0.031955, no overdispersion

par(mfrow = c(1, 2))
plot(kul.hr.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr.elev")
ddf.gof(kul.hr.elev$ddf, main ="kul.hr.elev")
dev.off()

# most wiggly pdf plot so far, even though is monotonically decreasing; not fitted q-q plot


p_dist_table(kul.hr.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     4      0.087
# 0.1 - 0.2     0      0.000
# 0.2 - 0.3     7      0.152
# 0.3 - 0.4     0      0.000
# 0.4 - 0.5    23      0.500
# 0.5 - 0.6     0      0.000
# 0.6 - 0.7     0      0.000
# 0.7 - 0.8    12      0.261
# 0.8 - 0.9     0      0.000
#   0.9 - 1     0      0.000
# Range of probabilities:  0.085 - 0.72

# very bad distribution, with a wide range but starting below 0.1; it obviously need a truncation

plot(kul.hr.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr.elev")
# seems that detection probability =< 0.15 at 260m (at bins and line of fit) 


# DF in MCDS(hab) with HR key no adjustments no truncation

kul.hr.hab <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hr", 
                 adjustment = NULL, formula = ~hab)

check.mono(kul.hr.hab$ddf)
# [1] TRUE

summary(kul.hr.hab)
# Number of observations :  46 
# Distance range         :  0  -  362.2762 
# AIC   : 527.9095
# Chi-square P = 0.28724 with 3 degrees of freedom      # >> 0.05, so retain!
0.28724/3
# 0.09574667, no overdispersion

par(mfrow = c(1, 2))
plot(kul.hr.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr.hab")
ddf.gof(kul.hr.hab$ddf, main ="kul.hr.hab")
dev.off()

# kind of nice looking pdf plot, but ylim values so small, not fitted q-q plot

p_dist_table(kul.hr.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3    34       0.74
# 0.3 - 0.4     0       0.00
# 0.4 - 0.5     0       0.00
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7    12       0.26
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.29 - 0.65 

# very good detection probability distribution at very beginning, means no truncation is needed, but still do
# for the sake of the analysis

plot(kul.hr.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="kul.hr.hab")
# seems detection probability is =< 0.15 at 260m more or less, even though it is not necessary


# DF in MCDS(elev+hab) with HR key no adjustments no truncation

kul.hr.elev.hab <- ds(distdata_kul, max(distdata_kul$distance), transect = "point", key = "hr", 
                      adjustment = NULL, formula = ~elev+hab)

# Warning in ddf.ds(model = dsmodel, data, meta.data = meta.data, control = control,  :
# Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
# Error in integrate(dpdf, lower = x[i, 1], upper = x[i, 2], width = width[i],  : 
# non-finite function value
# In addition: There were 49 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_kul, max(distdata_kul$distance), transect = "point",  : 
# No models could be fitted.


# DF in MCDS(elev) with HR key no adjustments 260m truncation

kul260.hr.elev <- ds(distdata_kul, truncation = 260, transect = "point", key = "hr", 
                     adjustment = NULL, formula = ~elev)

check.mono(kul260.hr.elev$ddf)
# [1] TRUE

summary(kul260.hr.elev)
# Number of observations :  42          # looses 9% of observations
# Distance range         :  0  -  260 
# AIC   : 460.2622 
# No Chi-square p value nor degrees of freedom for test

par(mfrow = c(1, 2))
plot(kul260.hr.elev, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr.elev")
ddf.gof(kul260.hr.elev$ddf, main ="kul260.hr.elev")
dev.off()

# odd looking pdf plot; not well fitted q-q plot but better than the one before

p_dist_table(kul260.hr.elev, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0      0.000
# 0.1 - 0.2     4      0.095
# 0.2 - 0.3     0      0.000
# 0.3 - 0.4     7      0.167
# 0.4 - 0.5     9      0.214
# 0.5 - 0.6     0      0.000
# 0.6 - 0.7    22      0.524
# 0.7 - 0.8     0      0.000
# 0.8 - 0.9     0      0.000
#   0.9 - 1     0      0.000
# Range of probabilities:  0.1 - 0.68

# better distribution than before but not what we look for; still > 5% of detections with Pa < 0.2, so further
# truncation needed (but I won't do it)

plot(kul260.hr.elev, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr.elev")
# detection probability is =< 0.15 at 220m more or less (but I won't do further truncation)


# DF in MCDS(hab) with HR key no adjustments 260m truncation

kul260.hr.hab <- ds(distdata_kul, truncation = 260, transect = "point", key = "hr", 
                    adjustment = NULL, formula = ~hab)

check.mono(kul260.hr.hab$ddf)
# [1] TRUE

summary(kul260.hr.hab)
# Number of observations :  42 
# Distance range         :  0  -  260 
# AIC   : 464.1039
# Chi-square P = 0.39241 with 2 degrees of freedom    # >>> 0.05, so retain!
0.39241/2
# 0.196205; no overdispersion

par(mfrow = c(1, 2))
plot(kul260.hr.hab, pdf=TRUE, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr.hab")
ddf.gof(kul260.hr.hab$ddf, main ="kul260.hr.hab")
dev.off()

# better looking pdf plot and better fitted q-q plot than model without truncation 

p_dist_table(kul260.hr.hab, proportion = TRUE)
#         p count proportion
#   0 - 0.1     0       0.00
# 0.1 - 0.2     0       0.00
# 0.2 - 0.3     0       0.00
# 0.3 - 0.4     9       0.21
# 0.4 - 0.5    33       0.79
# 0.5 - 0.6     0       0.00
# 0.6 - 0.7     0       0.00
# 0.7 - 0.8     0       0.00
# 0.8 - 0.9     0       0.00
#   0.9 - 1     0       0.00
# Range of probabilities:  0.36 - 0.42

# even better detection probability distribution than not truncated model, but range is very stretch

plot(kul260.hr.hab, pl.col = "white", xlab = "Distance of detection (m)", main ="kul260.hr.hab")
# seems no further truncation is needed.


# DF in MCDS(elev+hab) with HR key no adjustments 260m truncation

kul260.hr.elev.hab <- ds(distdata_kul, truncation = 260, transect = "point", key = "hr", 
                         adjustment = NULL, formula = ~elev+hab)

# Warning in ddf.ds(model = dsmodel, data, meta.data = meta.data, control = control,  :
# Estimated hazard-rate scale parameter close to 0 (on log scale). Possible problem in data (e.g., spike near zero distance).
# Error in integrate(dpdf, lower = x[i, 1], upper = x[i, 2], width = width[i],  : 
# non-finite function value
# In addition: There were 47 warnings (use warnings() to see them)
# Model failed to converge.
# Error in ds(distdata_kul, truncation = 260, transect = "point", key = "hr",  : 
# No models could be fitted.


# Now I can decide on Kulan DF Models (with/without truncation) in MCDS 


# MCDS DF model selection by AIC for Kulan no truncation ####

df.mcds.kul.notrunc <- list(kul.hn.elev, kul.hn.hab, kul.hr.elev, kul.hr.hab)

# kul.hn.elev and kul.hn.hab were retained for their Chi-square p value >> 0.05, but both showed an awful distribution
# of detection probabilities (p < 0.2) and not well fitted q-q plots
# kul.hr.elev was not retained for its Chi-square p value < 0.05, and also showed an awful distribution
# of detection probabilities (p < 0.2) and not well fitted q-q plot; although it will be included in the analysis for
# the comparison with QAIC stage
# kul.hr.hab was retained for its Chi-square p value > 0.05, a nice distribution of detection probabilities (p > 0.2)
# which means a better fit of the model (even though it didn't look nicely fitted)


kul.aic.mcds.notrunc <- kable(summarize_ds_models(kul.hn.elev, kul.hn.hab, kul.hr.elev, kul.hr.hab, 
                                                  sort = "AIC", delta_only = FALSE, output = "plain"), 
                              caption = "Summary of Kulan MCDS no trunc models AIC analysis", digits = 3)

kul.aic.mcds.notrunc

# Table: Summary of Kulan MCDS no trunc models AIC analysis

#|Model       |Key function |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:-----------|:------------|:-------|--------------:|-------:|---------:|
#|kul.hn.elev |Half-normal  |~elev   |          0.723| 519.877|     0.000|
#|kul.hr.elev |Hazard-rate  |~elev   |          0.420| 521.188|     1.311|
#|kul.hn.hab  |Half-normal  |~hab    |          0.965| 524.657|     4.780|
#|kul.hr.hab  |Hazard-rate  |~hab    |          0.516| 527.910|     8.033|


# According to AIC selection criterion, the best MCDS model is kul.hn.elev, with <2 difference with the next model
# on the round (kul.hr.elev, which was recommended to not retain!) At least the selected model is one recommended
# to retain by its Chi-square p value, but not the highest (it was kul.hn.hab); and the model with best distribution
# of detection probabilities was kul.hr.hab, which is funny because both ended up at the bottom of AIC comparison.


# MCDS DF model selection by QAIC for Kulan no truncation ####

# Fist step: Computing QAIC for HN and HR key functions models for MCDS Kulan no trunc

kul.mcds.hn.qaic <- QAIC(kul.hn.elev, kul.hn.hab)

kul.mcds.hr.qaic <- QAIC(kul.hr.elev, kul.hr.hab)


# Tables of QAIC values for each key function family are shown below 

kultab.mcds.hn.qaic <- knitr::kable(kul.mcds.hn.qaic, caption="Kulan QAIC values MCDS HN key no trunc models.")

kultab.mcds.hn.qaic

#Table: Kulan QAIC values MCDS HN key no trunc models.
#|            | df|     QAIC|
#|:-----------|--:|--------:|
#|kul.hn.elev |  4| 405.9997|
#|kul.hn.hab  |  2| 408.7920|

# Based on QIAC values computed, the preferable model within HN key function is kul.hn.elev, in accordance with
# AIC analysis; even though it had not the highest Chi-square p value, it was kul.hn.hab.
# Anyway, selected for the next round is kul.hn.elev


kultab.mcds.hr.qaic <- knitr::kable(kul.mcds.hr.qaic, caption="Kulan QAIC values MCDS HR key no trunc models")

kultab.mcds.hr.qaic

#Table: Kulan QAIC values MCDS HR key no trunc models
#|            | df|     QAIC|
#|:-----------|--:|--------:|
#|kul.hr.elev |  5| 123.1060|
#|kul.hr.hab  |  3| 121.4363|

# # Based on QIAC values computed, the preferable model within HR key function is kul.hr.hab, the model with 
# best distribution of detection probabilities (at least was not kul.hr.elev, the only model not recommended to retain)
# Anyway, selected for the next round is kul.hr.hab


# The second step of model selection ranks the models by their c^ values.

chats_df.kul.mcds.notrunc <- chi2_select(kul.hn.elev, kul.hr.hab)

modnames8 <- unlist(lapply(list(kul.hn.elev, kul.hr.hab), function(x) x$ddf$name.message))

results_df.kul.mcds.notrunc <- data.frame(modnames8, chats_df.kul.mcds.notrunc)

results.sort_df.kul.mcds.notrunc <- results_df.kul.mcds.notrunc[order(results_df.kul.mcds.notrunc$criteria),]

kul.qaic.mcds.notrunc <- knitr::kable(results.sort_df.kul.mcds.notrunc, digits=2, row.names = FALSE,
                                      caption="MCDS DF models Kulan no trunc ranked by their c^ value")

kul.qaic.mcds.notrunc

# Table: MCDS DF models Kulan no trunc ranked by their c^ value
#|modnames8                | criteria|
#|:------------------------|--------:|
#|hazard-rate key function |     1.26|
#|half-normal key function |     1.29|

# Here, for MCDS QAIC DF model selection Kulan no trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is different in covariate and also in key function (hazard rate no adjustments; kul.hr.hab) 
# as it had been chosen by conventional model selection based on AIC values (half-normal no adjustments; kul.hn.elev).

# so, selected model in MCDS Kulan no trunc is kul.hr.hab 
# (the fourth option according to AIC, which is not the best analysis to tell; but anyway is still under a difference
# of 10 points in AIC, which means that it would produce some "similar" results as the model with lower AIC value)
# With Gazelle MCDS no truncation models something similar happened; AIC's third model option was selected.


# MCDS DF model selection by AIC for Kulan 260m truncation ####

df.mcds.kul.trunc <- list(kul260.hn.elev, kul260.hn.hab, kul260.hr.elev, kul260.hr.hab)

# kul260.hn.elev retained by its Chi-square p value > 0.05 and better fitted q-q plot than not truncated version, but
# worse distribution of distribution of detection probabilities (Pa < 0.1)
# kul260.hn.hab retained by its Chi-square p value >> 0.05 and best distribution of detection 
# probabilities (Pa > 0.3), but not well fitted q-q plot than not truncated version
# kul260.hr.elev doesn't had Chi-square p value, their q-q plots looked bad fitted and its Pa distribution > 0.1
# kul260.hr.hab retained by its Chi-square p value >> 0.05 and best distribution of detection 
# probabilities (Pa > 0.3), also well fitted q-q plot than not truncated version


kul.aic.mcds.trunc <- kable(summarize_ds_models(kul260.hn.elev, kul260.hn.hab, kul260.hr.elev, kul260.hr.hab, 
                                                sort = "AIC", delta_only = FALSE, output = "plain"), 
                            caption = "Summary of Kulan MCDS 260m trunc models AIC analysis", digits = 3)

kul.aic.mcds.trunc

# Table: Summary of Kulan MCDS 260m trunc models AIC analysis
#|   |Model          |Key function |Formula | C-vM $p$-value|     AIC| Delta AIC|
#|:--|:--------------|:------------|:-------|--------------:|-------:|---------:|
#|1  |kul260.hn.elev |Half-normal  |~elev   |          0.963| 457.347|     0.000|
#|3  |kul260.hr.elev |Hazard-rate  |~elev   |          0.947| 460.262|     2.915|
#|2  |kul260.hn.hab  |Half-normal  |~hab    |          0.881| 462.123|     4.776|
#|4  |kul260.hr.hab  |Hazard-rate  |~hab    |          0.984| 464.104|     6.757|

# According to AIC selection criterion, for Kulan the best MCDS DF model with 260m truncation is kul260.hn.elev, 
# with >2 difference with the next model on the round (kul260.hr.elev). Surprisingly, second best model didn't have 
# computed Chi-square p value. 
# Still bear in mind that kul260.hn.hab and kul260.hr.hab were retained according their Chi-square p values


# MCDS DF model selection by QAIC for Kulan 260m truncation ####

# Fist step: Computing QAIC for HN and HR key functions models for MCDS Kulan 260m trunc

kul260.mcds.hn.qaic <- QAIC(kul260.hn.elev, kul260.hn.hab)

kul260.mcds.hr.qaic <- QAIC(kul260.hr.elev, kul260.hr.hab)


# Tables of QAIC values for each key function family are shown below 

kul260tab.mcds.hn.qaic <- knitr::kable(kul260.mcds.hn.qaic, caption="Kulan MCDS QAIC values HN key 260m trunc models.")

kul260tab.mcds.hn.qaic

# Table: Kulan MCDS QAIC values HN key 260m trunc models.
#|               | df|     QAIC|
#|:--------------|--:|--------:|
#|kul260.hn.elev |  4| 213.9408|
#|kul260.hn.hab  |  2| 213.9239|

# Unbelievably, based on QIAC values computed, the barely preferable model within HN key function is kul260.hn.hab, 
# on the contrary as in AIC analysis (when selected was kul260.hn.elev).
# Even though their difference is only around the second decimal number, kul260.hn.hab has greater Chi-square p value
# and better Pa distribution
# So, selected for the next round is kul260.hn.hab


kul260tab.mcds.hr.qaic <- knitr::kable(kul260.mcds.hr.qaic, caption="Kulan MCDS QAIC values HR key 260m trunc models")

kul260tab.mcds.hr.qaic

# Table: Kulan MCDS QAIC values HR key 260m trunc models
#|               | df| QAIC|
#|:--------------|--:|----:|
#|kul260.hr.elev |  5|   NA|
#|kul260.hr.hab  |  3|   NA|

# Here, no QIAC value was computed because there was no Chi-square p value calculated for kul260.hr.elev
# but there was Chi-square p and c^ values for kul260.hr.hab, so don't understand. 
# Besides, I can't compute QAIC for only one model
# Anyway, I selected kul260.hr.hab as MCDS DF model within HR key family to go next round because, 
# no matter kul260.hr.elev has lower AIC value, we can compute a c^ value only for kul260.hr.hab also.


# The second step of model selection ranks the models by their c^ values.

chats_df.kul.mcds.trunc <- chi2_select(kul260.hn.hab, kul260.hr.hab)

modnames9 <- unlist(lapply(list(kul260.hn.hab, kul260.hr.hab), function(x) x$ddf$name.message))

results_df.kul.mcds.trunc <- data.frame(modnames9, chats_df.kul.mcds.trunc)

results.sort_df.kul.mcds.trunc <- results_df.kul.mcds.trunc[order(results_df.kul.mcds.trunc$criteria),]

kul.qaic.mcds.trunc <- knitr::kable(results.sort_df.kul.mcds.trunc, digits=2, row.names = FALSE,
                                    caption="MCDS DF models Kulan 260m trunc ranked by their c^ value")

kul.qaic.mcds.trunc

# Table: MCDS DF models Kulan 260m trunc ranked by their c^ value
#|modnames9                | criteria|
#|:------------------------|--------:|
#|half-normal key function |     0.62|
#|hazard-rate key function |     0.94|

# Here, for MCDS QAIC DF model selection Kulan 260m trunc, the model chosen by this algorithm that adjusts for 
# overdispersion is different in covariate but similar key function (half-normal no adjustments; kul260.hn.hab) 
# as it had been chosen by conventional model selection based on AIC values (half-normal no adjustments; kul260.hn.elev).
# More surprisingly, selected model by QAIC path is the third at AIC value's ranking, with >4 units of difference 
# with selected model by this path.

# Anyway, I select kul260.hn.hab as a MCDS DF Kulan 260m trunc model


# Then, in MCDS DF models for Kulan, between truncated and not truncated models (although not comparable because of 
# different truncation):

AIC(kul.hr.hab, kul260.hn.hab)
#               df      AIC
# kul.hr.hab     3 527.9095
# kul260.hn.hab  2 462.1231

# the model with truncation distance obviously has lower AIC but also better Chi-square p values 
# than the one without truncation (0.2 vs 0.6), and certainly would provide more accurate estimations of abundance 
# (even has Pa in a range greater than not truncated model, 0.29 vs 0.37)

# So, in MCDS 260m truncation the DF model selected for Kulan is kul260.hn.hab ####
# (in CDS kul260.u.cos was the model selected; so there is a big difference in key function selected here but due
# to we can't use Uniform key without adjustment terms) 
# (in thesis was kul.hn no adjustments; could it be because of the improvements done on mrds and Distance packages?)


# Fitting a Spatial Density Surface Model (DSM) in Multiple Covariate Distance Sampling (MCDS) ####


# Goitered Gazelle spatial DSM (MCDS) ####


# I will use as a MCDS DF the model gaz80.hr.hab in a simple DSM, with a spatial smooth of s(x, y)

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(gaz80.hr.hab)
# Distance range         :  0  -  80 
# so need to eliminate observations at distances >= 80m

# but I will use distdata_gaz80 from DSM (CDS) which has the desired distribution of distances

summary(distdata_gaz80$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.924  24.451  45.905  44.265  61.853  79.849 


# So far I've used "count" as the response. That is, we adjusted the offset of the model to make 
# it take into account the "effective area" of the segments.
# Instead of using "count" we could use "abundance.est", which will leave the segment areas as they are 
# and calculate the Horvitz-Thompson estimates of the abundance per segment and use that as the response 
# in the model. This is most useful when we have covariates in the detection function (though we can use 
# it any time). I'm still trying to digest that explanation, but it worked.


# Running a spatial DSM (MCDS) with HR key Detection Function and habitat covariate, 
# 80m truncation and quasi-Poisson error distribution:

gaz80hrhab.xy.qpois <- dsm(abundance.est ~ s(x, y), gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(gaz80hrhab.xy.qpois)
# Approximate significance of smooth terms:
#        edf Ref.df         F p-value    
# s(x,y)  29      4 5.489e+12  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1062.6  Scale est. = 3.1376e-15  n = 77

# no idea where the 29 EDFs comes from (there is no 30 combinations of x and y values), but EDFs go down
# in other error distributions, Spatial smooth significant at its p-value

# GAM check

par(mfrow=c(2,2))
gam.check(gaz80hrhab.xy.qpois)
dev.off()

# Fitting warning, high EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and
# resids vs linear pred. flat and not OK); and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson MCDS spatial DSM model (THE SAME AS THESIS RESULT, EVEN THOUGH THIS DF MODEL HAS
# TRUNCATION AND COVARIATES)


# Running a spatial DSM (MCDS) with HR key Detection Function and habitat covariate, 
# 80m truncation and Tweedie error distribution:

gaz80hrhab.xy.tw <- dsm(abundance.est ~ s(x, y), gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                        transect = "point", method = "REML", family = tw())

# no warnings, that's good

summary(gaz80hrhab.xy.tw)
# Approximate significance of smooth terms:
#          edf Ref.df    F  p-value    
# s(x,y) 16.52  19.86 3.71 4.51e-05 ***
# R-sq.(adj) =  0.994   Deviance explained = 97.2%
# -REML =  53.75  Scale est. = 4.3052    n = 77

par(mfrow=c(2,2))
gam.check(gaz80hrhab.xy.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois distribution). Spatial smooth significant 
# at its p-value


# Running a spatial DSM (MCDS) with with HR key Detection Function and habitat covariate, 
# 80m truncation and Negative Binomial error distribution:

gaz80hrhab.xy.nb <- dsm(abundance.est ~ s(x, y), gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                        transect = "point", method = "REML", family = nb())

# no warnings, that's good

summary(gaz80hrhab.xy.nb)
# Approximate significance of smooth terms:
#        edf Ref.df Chi.sq p-value
# s(x,y)   2      2  2.657   0.265
# R-sq.(adj) =  0.104   Deviance explained = 13.7%
# -REML = 56.599  Scale est. = 1         n = 77

par(mfrow=c(2,2))
gam.check(gaz80hrhab.xy.nb)
dev.off()

# No fitting warnings and GAM check plots (Q-Q plot and resids vs linear pred.) that looked OK-ish 
# (Q-Q plot looks more fitted than in Tweedie distribution, but resids vs linear looks going down and less 
# as a "starry night" than in Tweedie, but certainly better than in qpois distribution). Spatial smooth is 
# not significant at its p-value and its EDF are really low


# Let's compare Gazelle spatial DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(gaz80hrhab.xy.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(gaz80hrhab.xy.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(gaz80hrhab.xy.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Only Tweedie and Negative Binomial distribution models have gam plots that looked OK-ish. (even NB Q-Q plot looked
# more fitted than Tweedie's, but here not). 
# Qpois models are out of competition by its ridiculously high deviance explained and low REML. Also, their lower
# deviance explained and EDF, and its spatial smooth non significant puts Negative Binomial distribution out too.

# So, for the Goitered gazelle estimated abundance data adjusted by the detection function, 
# I will select the spatial DSM in MCDS with Tweedie error distribution (gaz80hrhab.xy.tw) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION TOO, BUT WITH DIFFERENT DF HN-NO ADJS, NOT TRUNCATED NO COV)
# (IN SIMPLE DSM CDS SELECTED DISTRIBUTION WAS NB, SO THIS IS NEW)


# Kulan spatial DSM (MCDS) ####


# I will use as a MCDS DF the model kul260.hn.hab in a simple DSM, with a spatial smooth of s(x, y)


# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(kul260.hn.hab)
# Distance range         :  0  -  260
# so need to eliminate observations at distances >= 260m

# but I will use distdata_kul260 from DSM (CDS) which has the desired distribution of distances

summary(distdata_kul260$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.43   83.00  143.48  134.51  179.42  255.06 


# Running a spatial DSM (MCDS) with HN key Detection Function and habitat covariate, 
# 260m truncation and quasi-Poisson error distribution: 

kul260hnhab.xy.qpois <- dsm(abundance.est ~ s(x, y), kul260.hn.hab$ddf, covjune, distdata_kul260, 
                            transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#  Fitting terminated with step failure - check results carefully

summary(kul260hnhab.xy.qpois)
# Approximate significance of smooth terms:
#          edf Ref.df         F p-value    
# s(x,y) 28.98      2 7.593e+10  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1130.5  Scale est. = 3.3998e-15  n = 77

# no idea where the 29 EDFs comes from (there is no 30 combinations of x and y values), but EDFs go down
# in other error distributions. Spatial smooth significant at its p-value

par(mfrow=c(2,2))
gam.check(kul260hnhab.xy.qpois)
dev.off()

# Fitting warning, high EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and
# resids vs linear pred. flat and not OK); and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson MCDS simple DSM model (THE SAME AS THESIS RESULT, EVEN THOUGH THIS DF MODEL HAS
# TRUNCATION AND COVARIATES)


# Running a spatial DSM (MCDS) with HN key Detection Function and habitat covariate, 
# 260m truncation and Tweedie error distribution:

kul260hnhab.xy.tw <- dsm(abundance.est ~ s(x, y), kul260.hn.hab$ddf, covjune, distdata_kul260, 
                         transect = "point", method = "REML", family = tw())

# no warnings, that's good

summary(kul260hnhab.xy.tw)
# Approximate significance of smooth terms:
#          edf Ref.df    F p-value   
# s(x,y) 9.138  10.66 3.09 0.00239 **

# R-sq.(adj) =  0.999   Deviance explained = 99.4%
# -REML = 23.839  Scale est. = 3.6548    n = 77

par(mfrow=c(2,2))
gam.check(kul260hnhab.xy.tw)
dev.off()

# less EDF (9-ish) and spatial smooth significant at its p-value. No fitting warnings, lower EDFs (less wiggly) 
# and GAM check plots (Q-Q plot and resids vs linear pred.) looked OK-ish (Q-Q plot best looking than 
# resids vs linear, but both being better than in qpois distribution).



# Running a spatial DSM (MCDS) with HN key Detection Function and habitat covariate, 
# 260m truncation and Negative Binomial error distribution:

kul260hnhab.xy.nb <- dsm(abundance.est ~ s(x, y), kul260.hn.hab$ddf, covjune, distdata_kul260, 
                         transect = "point", method = "REML", family = nb())

# no warnings, that's good

summary(kul260hnhab.xy.nb)
# Approximate significance of smooth terms:
#         edf Ref.df Chi.sq p-value    
# s(x,y) 9.25  10.55  91.35  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained = 99.9%
# -REML = 26.728  Scale est. = 1         n = 77

par(mfrow=c(2,2))
gam.check(kul260hnhab.xy.nb)
dev.off()

# less EDF (9-ish) and spatial smooth significant at its p-value. No fitting warnings, lower EDFs (less wiggly) 
# and GAM check plots (Q-Q plot and resids vs linear pred.) looked OK-ish and similar to those on Tweedie distribution
# (Q-Q plot and resids vs linear looking better than in qpois distribution).


# Let's compare Kulan spatial DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(kul260hnhab.xy.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(kul260hnhab.xy.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(kul260hnhab.xy.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Qpois models are out of competition by its ridiculously high deviance explained and very low -REML.
# Both Negative Binomial and Tweedie distribution models have gam plots that looked OK-ish; even their Q-Q plots look
# very similar, differing only in the scale (greater for Tweedie distribution). 
# The Negative Binomial has a 3 point greater REML than the Tweedie one, and its deviance explained is
# only (but, still) 0.5% greater. 

# So, for Kulan estimated abundance data adjusted by the detection function, 
# I will select the spatial DSM in MCDS with Negative Binomial error distribution (kul260hnhab.xy.nb) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE, AND DF HN-NO ADJS WASN'T TRUNCATED AND WITHOUT COVARIATES; 
# MAYBE THAT WOULD EXPLAIN THE DIFFERENCE?)
# (IN SIMPLE DSM CDS SELECTED DISTRIBUTION WAS NB TOO)


# Fitting a Spatial Analysis in DSM for Multiple Covariate Distance Sampling ####

# Density and Abundance estimation for Goitered gazelle in a spatial DSM (MCDS) ####
# (using spatial DSM with HR key detection function and habitat as covariate, 80m truncation 
# and Tweedie error distribution: (gaz80hrhab.xy.tw)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_gaz80hrhab.xy.tw <- predict(gaz80hrhab.xy.tw, grd.in, off.set)


# I need to bind on my predictions file with the data from the prediction grid used to create them:

cc <- cbind(grd.in, predn_gaz80hrhab.xy.tw)


# And then, add a column on this prediction grid file of abundances with the dimension for both axis 
# of each cell, from which calculate density:

cc$dim <- 1000


# Now that I have the predictions, I will plot the density for Goitered gazelle over the GGASPA:

predngaz.xy.mcds <- ggplot(cc) + gg.opts + ggtitle("Goitered gazelle spatial DSM (MCDS)")
predngaz.xy.mcds <- predngaz.xy.mcds + geom_tile(aes(x = x, y = y, fill = predn_gaz80hrhab.xy.tw, 
                                                     width = dim, height = dim))
predngaz.xy.mcds <- predngaz.xy.mcds + coord_equal() + scale_fill_viridis_c(option = "D") 
predngaz.xy.mcds <- predngaz.xy.mcds + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.mcds <- predngaz.xy.mcds + labs(fill = "Density / km2")
predngaz.xy.mcds <- predngaz.xy.mcds + theme(legend.title = element_text(size = 18), 
                                             legend.text = element_text(size = 18), 
                                             plot.title = element_text(size = 23))
print(predngaz.xy.mcds) 


# Letting aside that it is a different plot, with colour scales that makes differences more evident, 
# the density plot is different from thesis: hotspots are a bit brighter, the location of the bottom hotspots
# changed a little bit (the bottom center hotspot seemed to "enlarge" giving the sensation of movement from where 
# it was in thesis plot), and even are 4 instead of 3 (one new in the bit that popped out to left from area). 
# Also, the scale is different: max 1.5-ish inds/km2 in thesis, with 80m truncation was 5-ish/km2!!, 
# but here with 80m trunc+cov go down again to 3-ish inds/km2)

print(predngaz.xy.cds)  # simple DSM (CDS) only with 80m truncation


# Plot of density + CT location for Goitered gazelle over the GGASPA: (not included in the research project report, 
# just an exploration of which look more informative)

predngaz.xy.mcds2 <- ggplot(cc) + gg.opts + ggtitle("Goitered gazelle spatial DSM (MCDS)")
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + geom_tile(aes(x = x, y = y, fill = predn_gaz80hrhab.xy.tw, 
                                                       width = dim, height = dim))
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + coord_equal() + scale_fill_viridis_c(option = "D")
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + geom_point(aes(x, y, size = gazelle_count), data = covjune, 
                                                    colour = "red", alpha = I(0.7))
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + labs(fill = "Density / km2", size = "Detections")
predngaz.xy.mcds2 <- predngaz.xy.mcds2 + theme(legend.title = element_text(size = 18), 
                                               legend.text = element_text(size = 18), 
                                               plot.title = element_text(size = 23))
print(predngaz.xy.mcds2)


# Plot of density + CT location and name for Goitered gazelle over the GGASPA: (not included in the research project 
# report, just exploration of which look more informative)

predngaz.xy.mcds3 <- ggplot(cc) + gg.opts + ggtitle("Goitered gazelle spatial DSM (MCDS)")
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + geom_tile(aes(x = x, y = y, fill = predn_gaz80hrhab.xy.tw, 
                                                       width = dim, height = dim))
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + coord_equal() + scale_fill_viridis_c(option = "D")
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + geom_path(aes(x = x, y = y), data = GGASPA)
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + geom_point(aes(x, y, size = gazelle_count), data = covjune, 
                                                    colour = "red", alpha = I(0.7))
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + labs(fill = "Density / km2", size = "Detections")
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + geom_text(aes(x = x, y = y, label = Sample.Label), hjust=-0.5, vjust=-0.4, 
                                                   size=3, colour ="grey", data = covjune)
predngaz.xy.mcds3 <- predngaz.xy.mcds3 + theme(legend.title = element_text(size = 18), 
                                               legend.text = element_text(size = 18), 
                                               plot.title = element_text(size = 23))
print(predngaz.xy.mcds3)
# don't like it very much


# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Goitered gazelle 
# in the GGASPA:

est.abun.gaz80hrhab.xy.tw <- sum(predn_gaz80hrhab.xy.tw)

est.abun.gaz80hrhab.xy.tw
# 5721.538

# (was 2071.026 individuals of Gazelle at GGASPA in thesis (HN-no adjs, TW) and 6995.329 inds with HN-cos and 
# 80m trunc NB). So gaz80hrhab.xy.tw puts our results in a middle ground


# Also, with the estimated total abundance I can calculate the estimated total density for the species 
# in the GGASPA. I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) 
# to calculate total area which will divide total estimated abundance to obtain total estimated density:

length(predn_gaz80hrhab.xy.tw)
# 45864 (in km2), a bit different from what is known area of GGASPA = 45,945 km2 (should be the difference
# between known real totally fitted area vs squared-cells area constructed for prediction)

est.dens.gaz80hrhab.xy.tw <- est.abun.gaz80hrhab.xy.tw / length(predn_gaz80hrhab.xy.tw)

est.dens.gaz80hrhab.xy.tw
# 0.1247501

# (was 0.0451558 individuals of Gazelle per km2 at GGASPA in thesis (HN-no adjs, TW) and 0.1525233 inds/km2 with 
# HN-cos and 80m trunc NB). So gaz80hrhab.xy.tw puts us in a middle ground again.


# Transforming density into individuals/100km2

est.dens.gaz80hrhab.xy.tw * 100
# 12.47501 individuals/100 km2

# (was 4.51558 individuals of Gazelle/100 km2 at GGASPA in thesis (HN-no adjs, TW) and 15.25233 inds/100 km2 with 
# HN-cos and 80m trunc NB). So gaz80hrhab.xy.tw puts us in a middle ground again.

# MAYBE PREDICTED ABUNDANCE AND DENSITY DIFFERENCES WITH SPATIAL DSM (CDS) 80M TRUNC(NB) 
# AND SPATIAL DSM (MCDS) 80M TRUNC+COV(TW) IS JUST THE ERROR DISTRIBUTION AND INCLUSION OF COVARIATES?


# Density and Abundance estimation for Kulan in a spatial DSM (MCDS) ####
# (using DSM with HN key Detection Function and habitat as covariate, 260m truncation 
# and Negative Binomial error distribution: (kul260hnhab.xy.nb)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_kul260hnhab.xy.nb <- predict(kul260hnhab.xy.nb, grd.in, off.set)


# I need to bind on my predictions file with the data from the prediction grid used to create them:

dd <- cbind(grd.in, predn_kul260hnhab.xy.nb)


# And then, add a column on this prediction grid file of abundances with the dimension for both axis 
# of each cell, from which calculate density:

dd$dim <- 1000


# Now that I have the predictions, I will plot the density for Kulan over the GGASPA:

prednkul.xy.mcds <- ggplot(dd) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (MCDS)")
prednkul.xy.mcds <- prednkul.xy.mcds + geom_tile(aes(x = x, y = y, fill = predn_kul260hnhab.xy.nb, 
                                                     width = dim, height = dim))
prednkul.xy.mcds <- prednkul.xy.mcds + coord_equal() + scale_fill_viridis_c(option = "D")
prednkul.xy.mcds <- prednkul.xy.mcds + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.mcds <- prednkul.xy.mcds + labs(fill = "Density / km2")
prednkul.xy.mcds <- prednkul.xy.mcds + theme(legend.title = element_text(size = 18), 
                                             legend.text = element_text(size = 18),
                                             plot.title = element_text(size = 23))
print(prednkul.xy.mcds) 


# Letting aside that it is a different plot, with colour scales that makes differences more evident, the density plot
# seems a bit different from thesis, same number of hotspots but 2nd and centered is less brighter, 
# and the scale is different (max 0.15 inds/km2 in thesis; with 260m trunc only is 0.4-ish/km2; 
# and with 260m trunc + hab cov is 0.6-ish inds/km2), so kul260hnhab.xy.nb left us better than ever in density estimates

print(prednkul.xy.cds) # simple DSM (CDS) model only with 260m trunc


# Plot of density + CT location for Kulan over the GGASPA: (not included in the research project report, just 
# exploration of which look more informative)

prednkul.xy.mcds2 <- ggplot(dd) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (MCDS)")
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + geom_tile(aes(x = x, y = y, fill = predn_kul260hnhab.xy.nb, 
                                                       width = dim, height = dim))
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + coord_equal() + scale_fill_viridis_c(option = "D") 
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + geom_point(aes(x, y, size = kulan_count), data = covjune, 
                                                    colour = "red", alpha = I(0.7))
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + labs(fill = "Density / km2", size= "Detections")
prednkul.xy.mcds2 <- prednkul.xy.mcds2 + theme(legend.title = element_text(size = 18), 
                                               legend.text = element_text(size = 18),
                                               plot.title = element_text(size = 23))
print(prednkul.xy.mcds2)


# Plot of density + CT location and name for Kulan over the GGASPA: (not included in the research project report, 
# just exploration of which look more informative)

prednkul.xy.mcds3 <- ggplot(dd) + gg.opts + ggtitle("Mongolian Kulan spatial DSM (MCDS)")
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + geom_tile(aes(x = x, y = y, fill = predn_kul260hnhab.xy.nb, 
                                                       width = dim, height = dim))
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + coord_equal() + scale_fill_viridis_c(option = "D") 
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + geom_path(aes(x = x, y = y), data = GGASPA)
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + geom_point(aes(x, y, size = kulan_count), data = covjune, 
                                                    colour = "red", alpha = I(0.7))
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + labs(fill = "Density / km2", size = "Detections")
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + geom_text(aes(x = x, y = y, label = Sample.Label), hjust=-0.5, vjust=-0.4, 
                                                   size=3, colour ="grey", data = covjune)
prednkul.xy.mcds3 <- prednkul.xy.mcds3 + theme(legend.title = element_text(size = 18), 
                                               legend.text = element_text(size = 18),
                                               plot.title = element_text(size = 23))
print(prednkul.xy.mcds3)
# don't like it very much


# Now, summing these cell grid predictions, I can obtain the estimated total abundance for Kulan 
# in the GGASPA:

est.abun.kul260hnhab.xy.nb <- sum(predn_kul260hnhab.xy.nb)

est.abun.kul260hnhab.xy.nb
# 321.5203


# (was 166.7125 individuals of Kulan at GGASPA in thesis (HN-no adjs, TW) and 256.9379 with U-cos NB and 260m trunc)
# so kul260hnhab.xy.nb left us better than ever in abundance estimates


# Also, with the estimated total abundance I can calculate the estimated total density for the species 
# in the GGASPA. I need to sum the total cell size of prediction grid (each cell is 1 km2 of size) 
# to calculate total area which will divide total estimated abundance to obtain total estimated density:

length(predn_kul260hnhab.xy.nb)
# 45864 (in km2), a bit different from what is known area of GGASPA = 45.945 km2 (should be the difference
# between known real totally fitted area vs squared-cells area contructed for prediction)


est.dens.kul260hnhab.xy.nb <- est.abun.kul260hnhab.xy.nb / length(predn_kul260hnhab.xy.nb)

est.dens.kul260hnhab.xy.nb
# 0.007010299


# (was 0.003634931 individuals of Kulan per km2 at GGASPA in thesis (HN-no adjs, TW) and 0.005602169 inds/km2 
# with U-cos NB and 260m trunc), so kul260hnhab.xy.nb left us better than ever in density estimates


# Transforming density into individuals/100km2

est.dens.kul260hnhab.xy.nb * 100
# 0.7010299 individuals/100 km2


# (was 0.3634931 individuals of Kulan/100 km2 at GGASPA in thesis (HN-no adjs, TW) and 0.5602169 inds/100 km2 
# with U-cos NB and 260m trunc), so kul260hnhab.xy.nb left us better than ever in density estimates


# Variance of Estimated Density and Abundance in spatial DSM for Multiple Covariate Distance Sampling ####


# Variance calculation of Density and Abundance estimates for Goitered gazelle using a spatial DSM (MCDS) model 

varprop_gaz80hrhab.xy.tw <- dsm.var.gam(gaz80hrhab.xy.tw, grd.in, off.set)

summary(varprop_gaz80hrhab.xy.tw)
# Approximate asymptotic confidence interval:
#     2.5%      Mean     97.5% 
# 1513.106  5721.538 21634.965 
# (Using log-Normal approximation)
# Point estimate                 : 5721.538       # in thesis 2071.026; with 80m trunc 6995.329
# CV of detection function       : 0.5412055      # in thesis 0.126; with 80m trunc 0.295
# CV from GAM                    : 0.5404         # in thesis 0.6328; with 80m trunc 0.6001
# Total standard error           : 4375.793 
# Total coefficient of variation : 0.7648         # in thesis 0.6453; with 80m trunc 0.6691


# So, from thesis results passing by a spatial DSM (MCDS) improved the estimates a lot, but not much
# as simple DSM (CDS) did. Variance calculation in spatial DSM (MCDS) has had mixed results: inclusion of covariates
# in DF increased a lot its variance but had lower variance from GAM than other DSM. Anyway, the greater variance
# incorporated in the DF made total CV a lot worse than any other model (even more higher than we wanted or expected)

# If the aim is to obtain a model that improves a lot the estimates while keeping uncertainty (variance in calculations)
# lower, our best model for Gazelle would be a spatial DSM (CDS) model, with 80m truncation and NB error distribution


# Variance calculation of Density and Abundance estimates for Kulan using a spatial DSM (MCDS) model

varprop_kul260hnhab.xy.nb <- dsm.var.gam(kul260hnhab.xy.nb, grd.in, off.set)

summary(varprop_kul260hnhab.xy.nb)
# Approximate asymptotic confidence interval:
#     2.5%       Mean      97.5% 
# 62.18687  321.52035 1662.33374 
# (Using log-Normal approximation)
# Point estimate                 : 321.5204           # in thesis 166.7125; with 260m trunc 256.9379
# CV of detection function       : 0.1960913          # in thesis 0.146196; with 260m trunc 0.1319073
# CV from GAM                    : 0.9903             # in thesis 0.5366; with 260m trunc 0.78
# Total standard error           : 324.5737 
# Total coefficient of variation : 1.0095             # in thesis 0.5562; with 260m trunc 0.7911


# Here, a spatial DSM (MCDS) model gave the best estimates obtained for Kulan so far, even getting a variance in DF
# somewhat similar to those previous models, making it a favoured candidate. But the problem arises when we look at
# variance calculation in GAM and total CV; it scratches the 100% variance in both, I don't even know how to interpret
# this situation ---> should it be a model to retain, or should it be discarded due to its worse performance
# on variance calculation?

# Variance in estimates is less worse (better?) with 260m trunc; but the estimates itself are in the middle
# So, what should I prioritise? Better estimates or lower variance in estimates' calculation?


# Fitting a Multiple Smooth Density Surface Model (DSM) in Conventional Distance Sampling (CDS) ####


# This time I will use the selected models in CDS for both species to perform a multiple smooth DSM 
# where count numbers are smooth functions of multiple covariates (multiple smooths), and not only of 
# their spatial coordinates as before (s (x, y)). 
# First, using a quasi-Poisson error distribution (the default in the model code), and then applying Tweedie and 
# Negative Binomial distributions, testing if them fit better these data. I set "method = REML" to ensure that 
# smooth terms are estimated reliably.


# Goitered Gazelle multiple smooth DSM (CDS) ####

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(gaz80.hn.cos)
# Distance range         :  0  -  80 
# so need to eliminate observations at distances >= 80m

# distdata_gaz80 was already done 

summary(distdata_gaz80$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.924  24.451  45.905  44.265  61.853  79.849


# Running multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and quasi-Poisson error distribution: 

gaz80hncos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             s(hab) + 
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath) + 
                             s(elev), 
                           gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                           transect = "point", method = "REML")

# Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
# A term has fewer unique covariate combinations than specified maximum degrees of freedom

# should it refers to hab and elev covariates, which are still "numeric" in covjune file?

str(covjune)
# $ hab          : num  34 18 34 33 18 29 33 18 18 18 ...
# $ elev         : num  1505 1169 1256 1222 1459 ...

# This was part of the response from David Miller when I reached him because of having problems to run these DSM:

# "When I ran your model there were issues with number of unique values for the hab variable, which only has 
# 7 different values. I'd recommend including this as a factor as you would in a glm/lm or including it as a
# random effect bs="re" if that seems appropriate in your setting"

# So, I'm transforming hab and elev covariates here at covjune to factor

covjune2 <- covjune

str(covjune2)
# $ hab          : num  34 18 34 33 18 29 33 18 18 18 ...
# $ elev         : num  1505 1169 1256 1222 1459 ...

covjune2$hab <- factor(covjune2$hab)
class(covjune2$hab)
# "factor"

covjune2$elev <- factor(covjune2$elev)
class(covjune2$elev)
# "factor"

# Running again Qpois model:

gaz80hncos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             s(hab) + 
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath) + 
                             s(elev), 
                           gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                           transect = "point", method = "REML")

# Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#  NA/NaN/Inf in foreign function call (arg 1)
# In addition: Warning messages:
#  1: In mean.default(xx) : argument is not numeric or logical: returning NA
#  2: In Ops.factor(xx, shift[i]) : '-' not meaningful for factors

# for some reason, before I tried eliminating factor covariates (hab, elev), and the model ran!

# somehow, no matter elevation covariate has as many different values as CT locations on the covariate file (covjune)
# it also present problems to the DSM engine and had to be eliminated like happened to habitat covariate... even though
# both elev and hab covariates were transformed into factor on covjune2...why? 


# Running multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and quasi-Poisson error distribution: 

gaz80hncos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             #s(hab) +                          # eliminated to run the model
                             #s(elev) +                         # eliminated to run the model
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#  Fitting terminated with step failure - check results carefully

# I'm currently asking David Miller why is it happening, why with both covariates specifically, and why no matter
# their class, the problems always are with those covariates?

summary(gaz80hncos.ms.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df        F p-value    
# s(x,y)           28.967  8.000 238407.7  <2e-16 ***
# s(NVDI)           1.000  1.000    649.1  <2e-16 ***
# s(VRM)            1.001  1.001    102.9  <2e-16 ***
# s(Cost.Distance)  1.000  1.000    182.0  <2e-16 ***
# s(CostPath)       1.000  1.000    302.0  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1025.5  Scale est. = 3.3991e-15  n = 75

# don't know where the 29 EDFs comes from (there is no 30 combinations of x and y values), but EDFs go down 
# in other error distributions. All included smooths are significant at their p-value, and also have EDF > 1, 
# so I will keep every term in the model

# GAM check

par(mfrow=c(2,2))
gam.check(gaz80hncos.ms.qpois)
dev.off()

# Fitting warning, high EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson distribution (THE SAME AS THESIS RESULT, EVEN THOUGH THIS HAS A DF MODEL WITH
# TRUNCATION AND IS NOT A SIMPLE DSM)


# Running multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and Tweedie error distribution: 

gaz80hncos.ms.tw <- dsm(count ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                           # eliminated to run the model
                          #s(elev) +                          # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                        transect = "point", method = "REML", family = tw())

# no warnings, cool!

summary(gaz80hncos.ms.tw)
# Approximate significance of smooth terms:
#                  edf Ref.df      F  p-value    
# s(x,y)             2      2 10.031 0.000152 ***
# s(NVDI)            1      1  9.590 0.002842 ** 
# s(VRM)             1      1  3.923 0.051677 .  
# s(Cost.Distance)   1      1  8.560 0.004667 ** 
# s(CostPath)        1      1  6.366 0.013974 *  

# R-sq.(adj) =  0.896   Deviance explained = 76.8%
# -REML = 34.106  Scale est. = 4.7819    n = 75

# all terms have EDFs >= 1, but not all are significant, so I need to eliminate VRM term from the model, 
# because it's the only one with its p-value non significant. I'll keep eliminating terms until I get only
# ones with significant p-values 


gaz80hncos.ms1.tw <- dsm(count ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                        # eliminated to run the model
                         #s(elev) +                       # eliminated to run the model
                         #s(VRM) +                        # 1st eliminated
                         #s(Cost.Distance), # +           # 3rd eliminated
                         #s(CostPath),                    # 2nd eliminated
                         gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = tw())

# no warnings

summary(gaz80hncos.ms1.tw)
# Approximate significance of smooth terms:
#         edf Ref.df      F  p-value    
# s(x,y)    2      2  5.394 0.006531 ** 
# s(NVDI)   1      1 11.907 0.000933 ***

# R-sq.(adj) =  0.395   Deviance explained = 55.5%
# -REML = 40.511  Scale est. = 8.6238    n = 77

# every included term is significant and have EDF >=1, so I stop here.

par(mfrow=c(2,2))
gam.check(gaz80hncos.ms1.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois distribution). Spatial and NVDI smooths 
# significant at their p-values


# Running multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and Negative Binomial error distribution: 

gaz80hncos.ms.nb <- dsm(count ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                             # eliminated to run the model
                          #s(elev) +                            # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                        transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hncos.ms.nb)
# Approximate significance of smooth terms:
#                  edf Ref.df Chi.sq p-value  
# s(x,y)             2      2  6.569  0.0375 *
# s(NVDI)            1      1  3.325  0.0682 .
# s(VRM)             1      1  0.885  0.3469  
# s(Cost.Distance)   1      1  4.062  0.0439 *
# s(CostPath)        1      1  1.576  0.2093  

# R-sq.(adj) =  0.323   Deviance explained = 61.4%
# -REML = 35.604  Scale est. = 1         n = 75

# hmm, just two terms barely significant, the spatial smooth and Cost.Distance; and as different as with Tweedie
# model, NVDI is barely non significant. I will eliminate first the more non significant which is VRM as always 
# (following the same elimination path as in Tweedie model before)


gaz80hncos.ms1.nb <- dsm(count ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                              # eliminated to run the model
                         #s(elev) +                             # eliminated to run the model
                         #s(VRM) +                              # 1st eliminated
                         #s(Cost.Distance), # +                 # 3rd eliminated   
                         #s(CostPath),                          # 2nd eliminated
                         gaz80.hn.cos$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hncos.ms1.nb)
# Approximate significance of smooth terms:
#         edf Ref.df Chi.sq p-value   
# s(x,y)    2      2  6.421 0.04033 * 
# s(NVDI)   1      1  7.563 0.00596 **

# R-sq.(adj) =  0.183   Deviance explained = 50.8%
# -REML = 40.665  Scale est. = 1         n = 77

# now every term included is significant and have EDFs >= 1, so I'll keep them


par(mfrow=c(2,2))
gam.check(gaz80hncos.ms1.nb)
dev.off()

# No fitting warnings and GAM check plots (Q-Q plot and resids vs linear pred.) that looked OK-ish 
# (Q-Q and resids vs linear plots look similarly fitted as in Tweedie distribution, and certainly better than 
# in qpois distribution). Spatial and NVDI smooths significant at their p-values


# Let's compare Gazelle multiple smooths DSM (CDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(gaz80hncos.ms.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(gaz80hncos.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(gaz80hncos.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Here NB Q-Q plot looked more fitted than TW, with less dispersion from the line of fit, or narrower units. 
# Then, Qpois models are out of competition by its ridiculously high deviance explained and very low -REML. 
# Both TW and NB models have the same significant smooth terms, and also have a very similar REML, but TW has a 
# Deviance explained almost 5 units higher than NB

# So, for the Goitered gazelle estimated abundance data adjusted by the detection function, 
# I will select the multiple smooth DSM in CDS with Tweedie error distribution (gaz80hncos.ms1.tw) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION TOO, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SPATIAL DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB, SO THIS IS NEW)


# Kulan multiple smooth DSM (CDS) #####

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(kul260.u.cos)
# Distance range         :  0  -  260 
# so need to eliminate observations at distances >= 80m

# distdata_gaz80 was already done 

summary(distdata_kul260$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.43   83.00  143.48  134.51  179.42  255.06


# Running multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and quasi-Poisson error distribution: 

kul260ucos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             s(hab) + 
                             s(elev) +
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           kul260.u.cos$ddf, covjune, distdata_kul260, 
                           transect = "point", method = "REML")

# Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
# A term has fewer unique covariate combinations than specified maximum degrees of freedom

# same warning as for Gazelle, so I'll use the covjune2 datafile which has hab and elev as factor covariates


# Running again Qpois model:

kul260ucos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             s(hab) + 
                             s(elev) +
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           kul260.u.cos$ddf, covjune2, distdata_kul260, 
                           transect = "point", method = "REML")


# Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#  NA/NaN/Inf in foreign function call (arg 1)
# In addition: Warning messages:
#  1: In mean.default(xx) : argument is not numeric or logical: returning NA
#  2: In Ops.factor(xx, shift[i]) : '-' not meaningful for factors

# same warning again as for Gazelle; I'll need to eliminate hab and elev smooths from the model


# Running multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and quasi-Poisson error distribution:

kul260ucos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             #s(hab) +                            # eliminated to run the model
                             #s(elev) +                           # eliminated to run the model
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           kul260.u.cos$ddf, covjune2, distdata_kul260, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(kul260ucos.ms.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df       F  p-value    
# s(x,y)           28.491      4 575.913  < 2e-16 ***
# s(NVDI)           6.906      4  16.124 3.76e-07 ***
# s(VRM)            6.590      4  45.967 2.21e-12 ***
# s(Cost.Distance)  1.000      1   9.815  0.00385 ** 
# s(CostPath)       1.000      1  97.925  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1095.6  Scale est. = 5.2729e-15  n = 75

# Well, n = 75 is weird, because my covjune files (both) have 77 rows, meaning 77 CT IDs, so there has to be some NA
# values in any of the smooths

# In the visual inspection of both files, there are NA values in CostPath column

sum(is.na(covjune$CostPath))
# 2

sum(is.na(covjune2$CostPath))
# 2

# According to their values of VRM and Cost.Distance (from whose CostPath is calculated), RG90 CostPath should be 
# between R8 and R12 (82 and 80, respectively); so I will assign it the value of 81 (I know it's not correct, 
# but just for now...)

covjune$CostPath[covjune$Sample.Label == "RG90"] <- 81

covjune2$CostPath[covjune2$Sample.Label == "RG90"] <- 81

# Then, I need to fill CostPath column info for R27 (which has NA on it)
# in GGASPA R27 is spatially between R26 and R28; R26 was eliminated because it didn't yield any images
# so I will look for a value near to R24 and R28 and assign it to R27 CostPath

covjune$CostPath == "55"

# there's no CostPath value of 55, so I will assign this value to R27 CostPath (I know it's not correct, 
# but just for now...)

covjune$CostPath[covjune$Sample.Label == "R27"] <- 55

covjune2$CostPath[covjune2$Sample.Label == "R27"] <- 55


# Now I could run the Kulan Qpois model again:

kul260ucos.ms.qpois <- dsm(count ~ s(x, y) + 
                             s(NVDI) + 
                             #s(hab) +                            # eliminated to run the model
                             #s(elev) +                           # eliminated to run the model
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           kul260.u.cos$ddf, covjune2, distdata_kul260, 
                           transect = "point", method = "REML")

# same warnings as before

summary(kul260ucos.ms.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df       F  p-value    
# s(x,y)           28.399  5.000 840.209  < 2e-16 ***
# s(NVDI)           7.693  5.000  23.803 6.24e-10 ***
# s(VRM)            5.782  5.000  15.653 8.33e-08 ***
# s(Cost.Distance)  1.064  1.122   0.624    0.465    
# s(CostPath)       1.000  1.000 123.915  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1112.3  Scale est. = 4.9864e-15  n = 77

# Now we have 77 CT included in the analysis again!. No idea where the 29 EDFs comes from (there is no 30 combinations 
# of x and y values) but EDFs go down for other smooth terms in other error distributions

# This is new for a Quasi-Poisson distribution, to have some smooth terms non significant at their p-value, 
# but here is newer because VRM is significant, so will not be the first term to be removed! (it will be Cost.Distance,
# the only term non significant). Smooth terms also have EDF > 1, 
# (this don't follow the same path of model selection in other distributions for Gazelle and Kulan!).


kul260ucos.ms1.qpois <- dsm(count ~ s(x, y) + 
                              s(NVDI) + 
                              #s(hab) +                            # eliminated to run the model
                              #s(elev) +                           # eliminated to run the model
                              #s(VRM) +                            # 2nd eliminated
                              #s(Cost.Distance) +                  # 1st eliminated
                              s(CostPath), 
                            kul260.u.cos$ddf, covjune2, distdata_kul260, 
                            transect = "point", method = "REML")

# same warning as before

summary(kul260ucos.ms1.qpois)
# Approximate significance of smooth terms:
#               edf Ref.df      F p-value    
# s(x,y)      27.895      5 1019.8  <2e-16 ***
# s(NVDI)      8.993      5  434.2  <2e-16 ***
# s(CostPath)  1.000      1 1085.0  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML =  -1129  Scale est. = 4.1948e-15  n = 77

# with this model selection path, all smooth terms included are significant at their p-values and with EDFs > 1, 
# so I will keep them all in the model (spatial, NVDI and CostPath smooths)

# GAM check

par(mfrow=c(2,2))
gam.check(kul260ucos.ms1.qpois)
dev.off()

# Fitting warning, high EDFs for spatial smooth (compared to other error distributions), gam.check plots 
# (mostly Q-Q plot and resids vs linear pred. flat and not OK); and suspiciously high deviance explained 
# and very low -REML value all suggest fitting problems for quasi-Poisson CDS complex DSM model 
# (THE SAME AS THESIS RESULT, EVEN THOUGH THIS DF MODEL HAS TRUNCATION).


# Running multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and Tweedie error distribution: 

kul260ucos.ms.tw <- dsm(count ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                                   # eliminated to run the model
                          #s(elev) +                                  # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        kul260.u.cos$ddf, covjune2, distdata_kul260, 
                        transect = "point", method = "REML", family = tw())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

# This is new for Tweedie distribution models; there was no warnings in spatial DSM (CDS) nor in spatial DSM (MCDS)
# but it appeared first time here

summary(kul260ucos.ms.tw)
# Approximate significance of smooth terms:
#                     edf Ref.df F p-value
# s(x,y)           21.294  6.000 0       1
# s(NVDI)           1.009  1.016 0       1
# s(VRM)            3.066  3.530 0       1
# s(Cost.Distance)  1.000  1.000 0       1
# s(CostPath)       1.000  1.000 0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -10.313  Scale est. = 2.1857    n = 77

# whaaaat?!?!? all smooth terms with a p-value of 1?
# I'll try to replicate the Gazelle previous model selection path eliminating VRM term first, in case anything happens;
# if not, I'll leave only spatial and NVDI terms as happened in the most repeated elimination path


kul260ucos.ms1.tw <- dsm(count ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                                   # eliminated to run the model
                         #s(elev) +                                  # eliminated to run the model
                         #s(VRM) +                                   # 1st eliminated
                         #s(Cost.Distance), # +                      # 3rd eliminated
                         #s(CostPath),                               # 2nd eliminated
                         kul260.u.cos$ddf, covjune2, distdata_kul260, 
                         transect = "point", method = "REML", family = tw())

# same warning as before when eliminating VRM, but ceased with subsequent eliminations

summary(kul260ucos.ms1.tw)
# Approximate significance of smooth terms:
#           edf Ref.df     F p-value
# s(x,y)  8.751  10.31 1.423   0.191
# s(NVDI) 1.000   1.00 0.055   0.815

# R-sq.(adj) =  0.999   Deviance explained = 99.4%
# -REML = 17.857  Scale est. = 2.1851    n = 77

# p-values of 1 keep coming for smooth terms when eliminating VRM, but when I eliminated CostPath p-values diminished
# to decimals but are still near to 1 and non significant; conversely, -REML is not suspiciously high and negative
# I ended with a most common model with spatial and NVDI smooths, but their p-values are non significant. 
# I will stop here and store it, even though this model is useless

par(mfrow=c(2,2))
gam.check(kul260ucos.ms1.tw)
dev.off()

# No fitting warnings in the end, lower EDFs (less wiggly) and GAM check plots similar to those for Gazelle TW 
# (Q-Q plot and resids vs linear pred.) which looked OK-ish (resids vs linear more of a "starry night" than 
# qpois distribution). Spatial and NVDI smooths non significant at their p-values (no smooth terms did)


# Running multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and Negative Binomial error distribution: 

kul260ucos.ms.nb <- dsm(count ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                                 # eliminated to run the model
                          #s(elev) +                                # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        kul260.u.cos$ddf, covjune2, distdata_kul260, 
                        transect = "point", method = "REML", family = nb())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#  Fitting terminated with step failure - check results carefully

# This is new for Negative Binomial distribution models; there was no warnings in spatial DSM (CDS) nor 
# in spatial DSM (MCDS) but it appeared first time here

summary(kul260ucos.ms.nb)
# Approximate significance of smooth terms:
#                    edf Ref.df Chi.sq p-value
# s(x,y)           22.58      5      0       1
# s(NVDI)           1.00      1      0       1
# s(VRM)            1.00      1      0       1
# s(Cost.Distance)  1.00      1      0       1
# s(CostPath)       1.00      1      0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -7.6163  Scale est. = 1         n = 77

# whaaaat?!?!? all smooth terms with a p-value of 1? The same happened with Tweedie models for Kulan
# I'll try to replicate the Gazelle previous model selection path eliminating VRM term first, in case anything happens;
# if not, I'll leave only spatial and NVDI terms as happened in the most repeated elimination path


kul260ucos.ms1.nb <- dsm(count ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                                 # eliminated to run the model
                         #s(elev) +                                # eliminated to run the model
                         #s(VRM) +                                 # 1st eliminated
                         #s(Cost.Distance), # +                    # 3rd eliminated
                         #s(CostPath),                             # 2nd eliminated
                         kul260.u.cos$ddf, covjune2, distdata_kul260, 
                         transect = "point", method = "REML", family = nb())

# same warning as before when eliminating VRM, but ceased with subsequent eliminations

summary(kul260ucos.ms1.nb)
# Approximate significance of smooth terms:
#           edf Ref.df Chi.sq p-value   
# s(x,y)  8.869  10.34 24.242 0.00873 **
# s(NVDI) 1.000   1.00  0.072 0.78798   

# R-sq.(adj) =      1   Deviance explained = 99.8%
# -REML = 20.288  Scale est. = 1         n = 77

# this is new, finally eliminating more smooths makes spatial smooth significant, but only it, so I would ended up
# with a simple DSM (spatial) if I don't retain NVDI smooth in the model (which is not my aim here). 
# EDFs are > 1, NVDI is non significant

par(mfrow=c(2,2))
gam.check(kul260ucos.ms1.nb)
dev.off()

# Previous fitting warnings but not in the end, lower EDFs (less wiggly) and gam.check plots similar to those 
# for Gazelle TW (Q-Q plot and resids vs linear pred.) which looked OK-ish (resids vs linear more of a "starry night" 
# than qpois distribution). Only spatial smooth significant at its p-value, but NVDI was also retained


# Let's compare Kulan multiple smooths DSM (CDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(kul260ucos.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(kul260ucos.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(kul260ucos.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()


# no matter which model I compare, QPois Q-Q plot is always flat and TW's and NB's remains with the a similar 
# dispersion from line of fit

# Here, Tweedie and Negative Binomial distribution models have gam plots that looked OK-ish.  
# All three distribution models have a ridiculously high deviance explained (100% or so) but only Qpois has 
# very low REML (lower and negative for Qpois).
# Even though kul260ucos.ms1.nb has the spatial smooth significant, the other is not, making it a pure spatial DSM 
# So, their non significant smooth terms (all of them in some cases with p-value = 1) puts TW and NB distribution 
# models out; on the contrary Qpois model had smooth terms significant at their p-values.

# So, for the Kulan estimated abundance data adjusted by the detection function, 
# I will select the multiple smooth DSM in CDS with Qpois error distribution (kul260ucos.ms1.qpois) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (FIRST TIME SELECTED DISTRIBUTION IN THIS ANALYSIS)
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB)


# Density and Abundance estimation for Goitered gazelle in multiple smooth DSM (CDS) ####
# (using multiple smooth DSM with HN key cosine adjustment term detection function, 80m truncation 
# and Tweedie error distribution: (gaz80hncos.ms1.tw) 

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_gaz80hncos.ms1.tw <- predict(gaz80hncos.ms1.tw, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
#  In predict.gam(object, newdata, type = type, ...) :
#  not all required variables have been supplied in  newdata!

# (I tried each of three error distribution models, just in case, and threw the same warning)


# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Density and Abundance estimation for Kulan in multiple smooth DSM (MCDS) ####
# (using multiple smooth DSM with Uniform key cosine adjustment term detection function, 260m truncation 
# and Qpois error distribution: (kul260ucos.ms1.qpois) 

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_kul260ucos.ms1.qpois <- predict(kul260ucos.ms1.qpois, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
# In predict.gam(object, newdata, type = type, ...) :
#   not all required variables have been supplied in  newdata!

# (I tried each of three error distribution models, just in case, and threw the same warning)

# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Fitting a Multiple Smooth Density Surface Model (DSM) in Multiple Covariate Distance Sampling (MCDS) ####


# As we're using DF models that have covariates in the detection function, and following the example used in
# spatial DSM (MCDS) models before, I will use "abundance.est", which will leave the segment areas as they are 
# and calculate the Horvitz-Thompson estimates of the abundance per segment and use that as the response 
# in the model.


# Goitered Gazelle multiple smooth DSM (MCDS) ####

# I will use as a MCDS DF the model gaz80.hr.hab in a multiple smooth DSM

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(gaz80.hr.hab)
# Distance range         :  0  -  80 
# so need to eliminate observations at distances >= 80m

# distdata_gaz80 was already done 

summary(distdata_gaz80$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.924  24.451  45.905  44.265  61.853  79.849 


# Running multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and quasi-Poisson error distribution: 

gaz80hrhab.ms.qpois <- dsm(abundance.est ~ s(x, y) + 
                             s(NVDI) + 
                             #s(hab) +                              # eliminated to run the model
                             #s(elev) +                             # eliminated to run the model
                             s(VRM) + 
                             s(Cost.Distance) +
                             s(CostPath), 
                           gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                           transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(gaz80hrhab.ms.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df        F p-value    
# s(x,y)           29.000  8.000 22132.58  < 2e-16 ***
# s(NVDI)           1.000  1.000   965.62  < 2e-16 ***
# s(VRM)            1.000  1.000    40.09 5.56e-07 ***
# s(Cost.Distance)  1.000  1.000   262.15  < 2e-16 ***
# s(CostPath)       1.002  1.004   347.90  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1072.3  Scale est. = 3.4296e-15  n = 77

# no idea where the 29 EDFs comes from (there is no 30 combinations of x and y values), but EDFs go down 
# in other error distributions. All included smooths are significant at their p-value, and also have EDF > 1, 
# so I will keep every term in the model

# GAM check

par(mfrow=c(2,2))
gam.check(gaz80hrhab.ms.qpois)
dev.off()

# Fitting warning, high EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and
# resids vs linear pred. flat and not OK); and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson MCDS multiple smooth DSM model 
# (THE SAME AS THESIS RESULT, EVEN THOUGH THIS DF MODEL HAS TRUNCATION AND COVARIATES)


# Running multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and Tweedie error distribution:

gaz80hrhab.ms.tw <- dsm(abundance.est ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                               # eliminated to run the model
                          #s(elev) +                              # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                        transect = "point", method = "REML", family = tw())

# no warnings, that's good!

summary(gaz80hrhab.ms.tw)
# Approximate significance of smooth terms:
#                  edf Ref.df     F  p-value    
# s(x,y)             2      2 8.197 0.000633 ***
# s(NVDI)            1      1 8.767 0.004184 ** 
# s(VRM)             1      1 3.179 0.078931 .  
# s(Cost.Distance)   1      1 7.481 0.007894 ** 
# s(CostPath)        1      1 5.552 0.021255 *  

# R-sq.(adj) =  0.862   Deviance explained = 73.1%
# -REML = 43.431  Scale est. = 11.777    n = 77

# all terms have EDFs >= 1, but not all are significant, so I need to eliminate the most non significant term from 
# the model first, which is VRM. I'll keep eliminating terms until I get only ones with 
# significant p-values


gaz80hrhab.ms1.tw <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                               # eliminated to run the model
                         #s(elev) +                              # eliminated to run the model
                         #s(VRM) +                               # 1st eliminated
                         #s(Cost.Distance), # +                  # 3rd eliminated (was significant before)
                         #s(CostPath),                           # 2nd eliminated (was significant before)
                         gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = tw())

# no warnings again

summary(gaz80hrhab.ms1.tw)
# Approximate significance of smooth terms:
#         edf Ref.df      F  p-value    
# s(x,y)    2      2  4.879 0.010262 *  
# s(NVDI)   1      1 11.780 0.000989 ***

# R-sq.(adj) =  0.364   Deviance explained = 52.8%
# -REML = 49.371  Scale est. = 16.82     n = 77

# here EDFs goes down significantly (from 29 to 2 in spatial smooth), and all included smooths
# are significant at their p-value, and also have EDF >= 1, so I will keep both term in the model

par(mfrow=c(2,2))
gam.check(gaz80hrhab.ms1.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois distribution). Spatial and NVDI smooths 
# significant at their p-values


# Running multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and Negative Binomial error distribution:

gaz80hrhab.ms.nb <- dsm(abundance.est ~ s(x, y) + 
                          s(NVDI) + 
                          #s(hab) +                               # eliminated to run the model
                          #s(elev) +                              # eliminated to run the model
                          s(VRM) + 
                          s(Cost.Distance) +
                          s(CostPath), 
                        gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                        transect = "point", method = "REML", family = nb())

# no warnings, that's good

summary(gaz80hrhab.ms.nb)
# Approximate significance of smooth terms:
#                    edf Ref.df Chi.sq p-value  
# s(x,y)           2.000  2.000  5.500  0.0639 .
# s(NVDI)          1.000  1.000  0.886  0.3465  
# s(VRM)           1.141  1.269  0.505  0.6440  
# s(Cost.Distance) 1.000  1.000  2.943  0.0863 .
# s(CostPath)      1.000  1.000  1.542  0.2143  

# R-sq.(adj) =  0.0407   Deviance explained = 48.7%
# -REML = 47.635  Scale est. = 1         n = 77

# very bad situation; no smooth term is significant at its p-value, even though there's a couple of them that
# are "barely" non significant; but all have EDFs > 1. I will sequentially eliminate terms from the one with less 
# significant p-value: VRM

gaz80hrhab.ms1.nb <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI) + 
                           #s(hab) +                               # eliminated to run the model
                           #s(elev) +                              # eliminated to run the model
                           #s(VRM) +                               # 1st eliminated
                           s(Cost.Distance) +
                           s(CostPath), 
                         gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hrhab.ms1.nb)
# Approximate significance of smooth terms:
#                  edf Ref.df Chi.sq p-value  
# s(x,y)             2      2  6.693  0.0352 *
# s(NVDI)            1      1  1.540  0.2146  
# s(Cost.Distance)   1      1  3.195  0.0739 .
# s(CostPath)        1      1  2.047  0.1525  

# R-sq.(adj) =  -0.198   Deviance explained = 47.2%
# -REML = 49.687  Scale est. = 1         n = 77

# the situation changed; now spatial smooth is significant, and the rest of p-values changed, making them nearer
# to significance. Next to be eliminated should be NVDI, but I will follow the same path as in Tweedie distribution,
# eliminating CostPath second, in case NVDI becomes significant

gaz80hrhab.ms2.nb <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI) + 
                           #s(hab) +                               # eliminated to run the model
                           #s(elev) +                              # eliminated to run the model
                           #s(VRM) +                               # 1st eliminated
                           s(Cost.Distance), # +
                         #s(CostPath),                           # 2nd eliminated
                         gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hrhab.ms2.nb)
# Approximate significance of smooth terms:
#                  edf Ref.df Chi.sq p-value  
# s(x,y)             2      2  5.957  0.0509 .
# s(NVDI)            1      1  4.057  0.0440 *
# s(Cost.Distance)   1      1  1.338  0.2473  
# R-sq.(adj) =  0.024   Deviance explained = 43.2%
# -REML = 51.659  Scale est. = 1         n = 77

# NVDI became significant, but spatial smooth went from significant to non significant (kind of in the edge of both); 
# anyway, following the same path as in Tweedie distribution, now I will eliminate Cost.Distance

gaz80hrhab.ms3.nb <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI), # + 
                         #s(hab) +                               # eliminated to run the model
                         #s(elev) +                              # eliminated to run the model
                         #s(VRM) +                               # 1st eliminated
                         #s(Cost.Distance), # +                  # 3rd eliminated
                         #s(CostPath),                           # 2nd eliminated
                         gaz80.hr.hab$ddf, covjune2, distdata_gaz80, 
                         transect = "point", method = "REML", family = nb())

# no warnings

summary(gaz80hrhab.ms3.nb)
# Approximate significance of smooth terms:
#         edf Ref.df Chi.sq p-value  
# s(x,y)    2      2  4.587  0.1009  
# s(NVDI)   1      1  6.241  0.0125 *
# R-sq.(adj) =  0.0807   Deviance explained = 39.2%
# -REML = 53.033  Scale est. = 1         n = 77

# spatial smooth had a mixed behaviour along the model decision; here is again non significant, but eliminate it
# would mean ending with a pure environmental model (habitat model), which is not my aim. So I will keep it in
# this model with NB distribution, but in the end the one with TW distribution will be selected

par(mfrow=c(2,2))
gam.check(gaz80hrhab.ms3.nb)
dev.off()

# No fitting warnings and GAM check plots (Q-Q plot and resids vs linear pred.) that looked OK-ish 
# (Q-plot looks similarly fitted as in Tweedie distribution, but resids vs linear looks better in Tweedie, 
# and certainly better than in qpois distribution); but spatial smooth is not significant at 
# its p-value; only NVDI is.


# Let's compare Gazelle multiple smooths DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(gaz80hrhab.ms.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(gaz80hrhab.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(gaz80hrhab.ms3.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Only Tweedie and Negative Binomial distribution models have gam plots that looked OK-ish. (here TW Q-Q plot looked
# more fitted than NB, but looking at the scale we can tell that NB has less dispersion). 
# Qpois models are out of competition by its ridiculously high deviance explained and very low and negative REML. 
# Also, their non significant spatial smooth and lower deviance explained puts NB distribution model out too.

# So, for the Goitered gazelle estimated abundance data adjusted by the detection function, 
# I will select the multiple smooth DSM in MCDS with Tweedie error distribution (gaz80hrhab.ms1.tw) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION TOO, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB, SO THIS IS NEW)


# Kulan multiple smooth DSM (MCDS) ####


# I will use as a MCDS DF the model kul260.hn.hab in a multiple smooth DSM

# First, I must remove the observations from the spatial data that were excluded when fitted the 
# detection function -- those observations at distances greater than the truncation.

summary(kul260.hn.hab)
# Distance range         :  0  -  260 
# so need to eliminate observations at distances >= 260m

# distdata_kul260 was already done 

summary(distdata_kul260$distance)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.43   83.00  143.48  134.51  179.42  255.06 


# Running multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and quasi-Poisson error distribution: 

kul260hnhab.ms.qpois <- dsm(abundance.est ~ s(x, y) + 
                              s(NVDI) + 
                              #s(hab) +                               # eliminated to run the model
                              #s(elev) +                              # eliminated to run the model
                              s(VRM) + 
                              s(Cost.Distance) +
                              s(CostPath), 
                            kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                            transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(kul260hnhab.ms.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df        F  p-value    
# s(x,y)           28.892  5.000 4225.823  < 2e-16 ***
# s(NVDI)           3.190  3.938   11.739 4.25e-06 ***
# s(VRM)            2.325  2.739    0.049  0.98867    
# s(Cost.Distance)  4.244  5.000    4.420  0.00302 ** 
# s(CostPath)       1.000  1.000    0.283  0.59790    

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1112.5  Scale est. = 4.421e-15  n = 77

# no idea where the 29 EDFs comes from (there is no 30 combinations of x and y values) but EDFs go down 
# for other smooth terms in other error distributions

# This is new for a Quasi-Poisson distribution, to have some smooth terms non significant at their p-value,
# and they also have EDF > 1, so I will eliminate the most non significant smooth term here first: VRM
# (which follows the same path of model selection in TW and NB distributions for Gazelle)

kul260hnhab.ms1.qpois <- dsm(abundance.est ~ s(x, y) + 
                               s(NVDI) + 
                               #s(hab) +                               # eliminated to run the model
                               #s(elev) +                              # eliminated to run the model
                               #s(VRM) +                               # 1st eliminated
                               s(Cost.Distance) +
                               s(CostPath), 
                             kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                             transect = "point", method = "REML")

# same warning as before

summary(kul260hnhab.ms1.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df       F  p-value    
# s(x,y)           25.687      5 1877.38 < 2e-16 ***
# s(NVDI)           7.369      5  209.65 < 2e-16 ***
# s(Cost.Distance)  1.000      1   12.26 0.00113 ** 
# s(CostPath)       1.000      1  994.61 < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1107.6  Scale est. = 4.1881e-15  n = 77

# with this model selection path, the term that wasn't significant earlier (CostPath) is now significant; making
# all smooth terms included significant at their p-values and with EDFs > 1, so I will keep them all in the model

# GAM check

par(mfrow=c(2,2))
gam.check(kul260hnhab.ms1.qpois)
dev.off()

# Fitting warning, high EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and
# resids vs linear pred. flat and not OK); and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson MCDS multiple smooth DSM model 
# (THE SAME AS THESIS RESULT, EVEN THOUGH THIS DF MODEL HAS TRUNCATION AND COVARIATES)


# Running multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and Tweedie error distribution:

kul260hnhab.ms.tw <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI) + 
                           #s(hab) +                              # eliminated to run the model
                           #s(elev) +                             # eliminated to run the model
                           s(VRM) + 
                           s(Cost.Distance) +
                           s(CostPath), 
                         kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                         transect = "point", method = "REML", family = tw())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#            Fitting terminated with step failure - check results carefully

# This is new for Tweedie distribution models; there was no warnings in spatial DSM (CDS) nor in spatial DSM (MCDS)
# but it appeared first time in multiple smooth DSM (CDS) and now here

summary(kul260hnhab.ms.tw)
# Approximate significance of smooth terms:
#                     edf Ref.df F p-value
# s(x,y)           21.617  5.000 0       1
# s(NVDI)           1.024  1.042 0       1
# s(VRM)            3.169  3.648 0       1
# s(Cost.Distance)  1.000  1.000 0       1
# s(CostPath)       1.000  1.000 0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -6.2925  Scale est. = 4.7806    n = 77

# whaaaat?!?!? all smooth terms with a p-value of 1? I'll try to replicate the previous model selection path
# eliminating VRM term first and so on until I get significant terms, in case anything happens:

kul260hnhab.ms1.tw <- dsm(abundance.est ~ s(x, y) + 
                            s(NVDI), # + 
                          #s(hab) +                              # eliminated to run the model
                          #s(elev) +                             # eliminated to run the model
                          #s(VRM) +                              # 1st eliminated 
                          #s(Cost.Distance), # +                 # 3rd eliminated
                          #s(CostPath),                          # 2nd eliminated                           
                          kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                          transect = "point", method = "REML", family = tw())

# same warning as before, until I eliminated CostPath when warnings ceased

summary(kul260hnhab.ms1.tw)
# Approximate significance of smooth terms:
#           edf Ref.df     F p-value
# s(x,y)  8.756  10.29 1.666   0.108
# s(NVDI) 1.000   1.00 0.043   0.836

# R-sq.(adj) =      1   Deviance explained = 99.5%
# -REML = 22.585  Scale est. = 4.7822    n = 77

# From the elimination of CostPath, smooth terms started to show p-values less than one but barely, and still 
# non significant. Remaining smooths still non significant, but at least not 1. EDFs > 1

par(mfrow=c(2,2))
gam.check(kul260hnhab.ms1.tw)
dev.off()

# Fitting warning at first but not in the end, lower EDFs (less wiggly) and gam.check plots (Q-Q plot and 
# resids vs linear pred.) looked OK-ish(resids vs linear more of a "starry night" than qpois distribution).
# But both smooth terms (spatial and NVDI) included are non significant. So I will keep this model, 
# but in the end other with any distribution will be selected


# Running multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and Negative Binomial error distribution:

kul260hnhab.ms.nb <- dsm(abundance.est ~ s(x, y) + 
                           s(NVDI) + 
                           #s(hab) +                              # eliminated to run the model
                           #s(elev) +                             # eliminated to run the model
                           s(VRM) + 
                           s(Cost.Distance) +
                           s(CostPath), 
                         kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                         transect = "point", method = "REML", family = nb())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(kul260hnhab.ms.nb)
# Approximate significance of smooth terms:
#                     edf Ref.df Chi.sq p-value
# s(x,y)           16.760  5.000      0       1
# s(NVDI)           1.000  1.000      0       1
# s(VRM)            2.014  2.295      0       1
# s(Cost.Distance)  1.000  1.000      0       1
# s(CostPath)       1.000  1.000      0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -3.1535  Scale est. = 1         n = 77

# whaaaat?!?!? all smooth terms with a p-value of 1? I'll try to replicate the previous model selection path
# eliminating VRM term first and so on until I get significant terms, in case anything happens

kul260hnhab.ms1.nb <- dsm(abundance.est ~ s(x, y) + 
                            s(NVDI), # + 
                          #s(hab) +                              # eliminated to run the model
                          #s(elev) +                             # eliminated to run the model
                          #s(VRM) +                              # 1st eliminated
                          #s(Cost.Distance), # +                 # 3rd eliminated
                          #s(CostPath),                          # 2nd eliminated
                          kul260.hn.hab$ddf, covjune2, distdata_kul260, 
                          transect = "point", method = "REML", family = nb())

# same warning as before, but only until VRM elimination; then ceased with subsequent eliminations

summary(kul260hnhab.ms1.nb)
# Approximate significance of smooth terms:
#           edf Ref.df Chi.sq p-value    
# s(x,y)  8.857  10.24 55.744  <2e-16 ***
# s(NVDI) 1.000   1.00  0.067   0.795    

# R-sq.(adj) =      1   Deviance explained = 99.9%
# -REML = 25.041  Scale est. = 1         n = 77

# With the elimination of VRM, remaining smooth terms still had a p-value of 1. When CostPath was eliminated, p-values
# got low, but still non significant.
# This is new, finally eliminating more smooths makes spatial smooth significant, but NVDI is still non significant;
# all included smooths have EDFs > 1. I'll keep NVDI in the model, or I ended up with a pure spatial model, which is
# not my aim here

par(mfrow=c(2,2))
gam.check(kul260hnhab.ms1.nb)
dev.off()

# Fitting warning at first but not in the end, lower EDFs (less wiggly) and gam.check plots (Q-Q plot and 
# resids vs linear pred.) looked OK-ish(better for Tweedie distribution; resids vs linear more of a "starry night" 
# than qpois distribution). Only spatial smooth is significant, but NVDI is non significant. So I will keep this model, 
# but in the end other with any distribution will be selected


# Let's compare Kulan multiple smooths DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(kul260hnhab.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(kul260hnhab.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(kul260hnhab.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# no matter which model I compare, QPois is always flat and TW and NB had the same dispersion shape from line of fit,
# even though scale is lower for NB (less dispersion)

# Here, Tweedie and Negative Binomial models have gam plots that looked OK-ish. (TW Q-Q plot looked similarly fitted
# as NB, but looking at the scale we can tell that NB has less dispersion). 
# All three distribution models have a ridiculously high deviance explained (100% or so) and low REML 
# (lower and negative for Qpois; even positive for TW and NB).
# Even though kul260hnhab.ms1.nb has a spatial smooth significant, the other is not, making it a pure spatial DSM,
# while TW model has none smooth term significant.
# So, their non significant smooth terms (all of them in some cases with p-value = 1) puts TW and NB distribution 
# models out; on the contrary Qpois model had some smooth terms significant at their p-values.

# So, for the Kulan estimated abundance data adjusted by the detection function, 
# I will select the multiple smooth DSM in MCDS with Qpois error distribution (kul260hnhab.ms1.qpois) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (SAME ERROR DISTRIBUTION SELECTED AS IN MULTIPLE SMOOTH DSM (CDS)  FOR KULAN AGAIN)
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB)


# Density and Abundance estimation for Goitered gazelle in multiple smooth DSM (MCDS) ####
# (using multiple smooth DSM with HR key detection function and habitat as covariate, 80m truncation 
# and Tweedie error distribution: (gaz80hrhab.ms1.tw)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_gaz80hrhab.ms1.tw <- predict(gaz80hrhab.ms1.tw, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
# In predict.gam(object, newdata, type = type, ...) :
#   not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)


# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Density and Abundance estimation for Kulan in multiple smooth DSM (MCDS) ####
# (using multiple smooth DSM with HN key detection function and habitat as covariate, 260m truncation
# and Qpois error distribution: (kul260hnhab.ms1.qpois)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_kul260hnhab.ms1.qpois <- predict(kul260hnhab.ms1.qpois, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
# In predict.gam(object, newdata, type = type, ...) :
#   not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)

# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Now I'll try to turn things around, using habitat and elevation smooth terms (which are numeric) as random effect
# in the models, as David Miller suggested (though I can't understand why, I'm doing it just in case...)

class(covjune$hab)
# "numeric"

class(covjune$elev)
# "numeric"


# Fitting a Multiple Smooth DSM, using Habitat Covariate as Random Effect in CDS ####


# Goitered gazelle Random Effect multiple smooth DSM (CDS) ####


# When using only hab as random effect, the only smooth term non significant is hab. Using only elev as random
# effect don´t allow the model to be stored due to failure. And when both numeric covariates are included as 
# random effects, the one non significant is elevation instead...
# I'll take an "expert" call and will use only hab covariate as random effect, in order to eliminate it in the 
# model decision path due to its non significance, as we have another "habitat" covariate also included like NVDI,
# and also to not loose elev as smooth term given it has a lot more different values when compared with hab


# Running Random Effect multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and quasi-Poisson error distribution:

rehab.gaz80hncos.ms.qpois <- dsm(count ~ s(x, y) + 
                                   s(NVDI) + 
                                   s(hab, bs ="re") + 
                                   s(elev) +
                                   s(VRM) + 
                                   s(Cost.Distance) +
                                   s(CostPath), 
                                 gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                                 transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(rehab.gaz80hncos.ms.qpois)
# Approximate significance of smooth terms:
#                        edf Ref.df       F  p-value    
# s(x,y)           2.888e+01  8.000 2476.38  < 2e-16 ***
# s(NVDI)          1.000e+00  1.000  765.26  < 2e-16 ***
# s(hab)           4.103e-18  1.000    0.00    0.981    
# s(elev)          4.891e+00  6.017   15.68  < 2e-16 ***
# s(VRM)           1.000e+00  1.000   39.16 4.09e-07 ***
# s(Cost.Distance) 1.000e+00  1.000  186.68 2.73e-16 ***
# s(CostPath)      1.000e+00  1.000  187.25  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1048.9  Scale est. = 3.8578e-15  n = 77

# The only smooth non significant is hab, and all EDFs are >1 but not so high, so the model will be less wiggly
# First term eliminated will be hab

rehab.gaz80hncos.ms1.qpois <- dsm(count ~ s(x, y) + 
                                    s(NVDI) + 
                                    #s(hab, bs ="re") +                  # 1st eliminated
                                    s(elev) +
                                    s(VRM) + 
                                    s(Cost.Distance) +
                                    s(CostPath), 
                                  gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                                  transect = "point", method = "REML")

# same warning as before

summary(rehab.gaz80hncos.ms1.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df         F p-value    
# s(x,y)           28.98      8 5.988e+07  < 2e-16 ***
# s(NVDI)           1.00      1 7.126e+02  < 2e-16 ***
# s(elev)           1.00      1 6.943e+01  < 2e-16 ***
# s(VRM)            1.00      1 2.678e+02  < 2e-16 ***
# s(Cost.Distance)  1.00      1 8.298e+01 1.68e-11 ***
# s(CostPath)       1.00      1 2.808e+02  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1059.7  Scale est. = 3.5091e-15  n = 77

# all smooth terms are significant and has EDFs >1, so I'll keep them

par(mfrow=c(2,2))
gam.check(rehab.gaz80hncos.ms1.qpois)
dev.off()

# Fitting warning, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson distribution (THE SAME AS THESIS RESULT, EVEN THOUGH THIS HAS A DF MODEL WITH
# TRUNCATION AND IS NOT A SIMPLE DSM)


# Running Random Effect multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and Tweedie error distribution:

rehab.gaz80hncos.ms.tw <- dsm(count ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") + 
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                              transect = "point", method = "REML", family = tw())

# no warnings

summary(rehab.gaz80hncos.ms.tw)
# Approximate significance of smooth terms:
#                        edf Ref.df     F  p-value    
# s(x,y)           2.000e+00      2 8.064 0.000713 ***
# s(NVDI)          1.000e+00      1 4.393 0.039747 *  
# s(hab)           1.089e-06      1 0.000 0.633256    
# s(elev)          1.000e+00      1 0.413 0.522697    
# s(VRM)           1.000e+00      1 2.076 0.154110    
# s(Cost.Distance) 1.000e+00      1 5.449 0.022494 *  
# s(CostPath)      1.000e+00      1 5.417 0.022886 *  

# R-sq.(adj) =   0.87   Deviance explained = 74.8%
# -REML = 33.972  Scale est. = 5.7719    n = 77

# none of hab or elev are significant, besides of VRM, but smooth terms EDFs are all > 1. I'll eliminate the most
# non significant term first, hab, and so on until I get only significant terms

rehab.gaz80hncos.ms1.tw <- dsm(count ~ s(x, y) + 
                                 s(NVDI), # + 
                               #s(hab, bs = "re") +                      # 1st eliminated
                               #s(elev) +                                # 2nd eliminated
                               #s(VRM) +                                 # 3rd eliminated
                               #s(Cost.Distance), # +                    # 5th eliminated
                               #s(CostPath),                             # 4th eliminated
                               gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                               transect = "point", method = "REML", family = tw())

# no warnings

summary(rehab.gaz80hncos.ms1.tw)
# Approximate significance of smooth terms:
#         edf Ref.df      F  p-value    
# s(x,y)    2      2  5.394 0.006531 ** 
# s(NVDI)   1      1 11.907 0.000933 ***

# R-sq.(adj) =  0.395   Deviance explained = 55.5%
# -REML = 40.511  Scale est. = 8.6238    n = 77

# every included term is significant and have EDF >=1, so I stop here.

par(mfrow=c(2,2))
gam.check(rehab.gaz80hncos.ms1.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois distribution). Spatial and NVDI smooths 
# significant at their p-values


# Running Random Effect multiple smooth DSM (CDS) with HN key cosine adjustment terms Detection Function, 
# 80m truncation and Negative Binomial error distribution:

rehab.gaz80hncos.ms.nb <- dsm(count ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") + 
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                              transect = "point", method = "REML", family = nb())

# no warnings

summary(rehab.gaz80hncos.ms.nb)
# Approximate significance of smooth terms:
#                        edf Ref.df Chi.sq p-value  
# s(x,y)           2.000e+00  2.000  4.764  0.0924 .
# s(NVDI)          1.000e+00  1.000  1.675  0.1955  
# s(hab)           5.782e-06  1.000  0.000  0.6363  
# s(elev)          2.386e+00  3.086  0.600  0.9060  
# s(VRM)           1.000e+00  1.000  0.528  0.4676  
# s(Cost.Distance) 1.000e+00  1.000  3.200  0.0736 .
# s(CostPath)      1.000e+00  1.000  1.459  0.2271  

# R-sq.(adj) =  0.312   Deviance explained = 63.5%
# -REML = 34.955  Scale est. = 1         n = 77

# very bad situation, where every smooth term is non significant... I'll use the same path as with Tweedie model

rehab.gaz80hncos.ms1.nb <- dsm(count ~ s(x, y) + 
                                 s(NVDI), # + 
                               #s(hab, bs = "re") +                    # 1st eliminated
                               #s(elev) +                              # 2nd eliminated
                               #s(VRM) +                               # 3rd eliminated                                
                               #s(Cost.Distance), # +                  # 5th eliminated
                               #s(CostPath),                           # 4th eliminated
                               gaz80.hn.cos$ddf, covjune, distdata_gaz80, 
                               transect = "point", method = "REML", family = nb())

# no warnings

summary(rehab.gaz80hncos.ms1.nb)
# Approximate significance of smooth terms:
#         edf Ref.df Chi.sq p-value   
# s(x,y)    2      2  6.421 0.04033 * 
# s(NVDI)   1      1  7.563 0.00596 **

# R-sq.(adj) =  0.183   Deviance explained = 50.8%
# -REML = 40.665  Scale est. = 1         n = 77

# every included term is significant and have EDF >=1, so I stop here.

par(mfrow=c(2,2))
gam.check(rehab.gaz80hncos.ms1.nb)
dev.off()

# No fitting warnings and GAM check plots (Q-Q plot and resids vs linear pred.) that looked OK-ish 
# (Q-Q and resids vs linear plots look similarly fitted as in Tweedie distribution, and certainly better than 
# in qpois distribution); spatial and NVDI smooths significant at their p-values


# Let's compare Gazelle Random Effect multiple smooths DSM (CDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(rehab.gaz80hncos.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(rehab.gaz80hncos.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(rehab.gaz80hncos.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# For the second time in my analysis, the Tweedie and Negative Binomial error distributions don't present a dispersion
# that look like a sand clock, both also have gam plots that looked OK-ish. (here NB Q-Q plot looked more fitted 
# than others, with less dispersion from the line of fit, and narrower units). 
# Then, Qpois models are out of competition by its ridiculously high deviance explained and low -REML (and flat Q-Q plot) 
# Both TW and NB models have the same significant smooth terms, and also have a very similar REML, but TW has 
# a Deviance explained almost 5 units higher than NB (and don't forget that NB model had all smooths non significant 
# at first)

# So, for the Goitered gazelle estimated abundance data adjusted by the detection function, 
# I will select the Random Effect multiple smooth DSM in CDS with TW error distribution (rehab.gaz80hncos.ms1.tw) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IT WAS THE SAME ERROR DISTRIBUTION SELECTED IN MULTIPLE SMOOTH CDS)
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION TOO, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB, SO THIS IS NEW)
# (BUT IN MULTIPLE SMOOTH DSM MCDS, SELECTED WAS ALSO TW DISTRIBUTION)


# Kulan Random Effect multiple smooth DSM (CDS) ####


# Running Random Effect multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and quasi-Poisson error distribution: 

rehab.kul260ucos.ms.qpois <- dsm(count ~ s(x, y) + 
                                   s(NVDI) + 
                                   s(hab, bs = "re") + 
                                   s(elev) +
                                   s(VRM) + 
                                   s(Cost.Distance) +
                                   s(CostPath), 
                                 kul260.u.cos$ddf, covjune, distdata_kul260, 
                                 transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(rehab.kul260ucos.ms.qpois)
# Approximate significance of smooth terms:
#                        edf Ref.df        F  p-value    
# s(x,y)           2.700e+01  5.000  400.390  <2e-16 ***
# s(NVDI)          1.003e+00  1.005   73.301  <2e-16 ***
# s(hab)           3.591e-19  1.000    0.000  0.9080    
# s(elev)          1.001e+00  1.002 1615.926  <2e-16 ***
# s(VRM)           1.000e+00  1.000  404.723  <2e-16 ***
# s(Cost.Distance) 1.000e+00  1.000    3.246  0.0789 .  
# s(CostPath)      3.847e+00  4.817   17.970  <2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1156.1  Scale est. = 3.8884e-15  n = 77

# as for Gazelle, I will eliminate first hab given it is the most non significant smooth term, and so on until
# I get only significant terms in my model


rehab.kul260ucos.ms1.qpois <- dsm(count ~ s(x, y) + 
                                    s(NVDI) + 
                                    #s(hab, bs = "re") +                 # 1st eliminated
                                    s(elev) +
                                    s(VRM) + 
                                    s(Cost.Distance) +
                                    s(CostPath), 
                                  kul260.u.cos$ddf, covjune, distdata_kul260, 
                                  transect = "point", method = "REML")

# same warning as before

summary(rehab.kul260ucos.ms1.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df        F  p-value    
# s(x,y)           27.462  5.000 800.55  < 2e-16 ***
# s(NVDI)           1.031  1.058  87.64  < 2e-16 ***
# s(elev)           4.618  4.000 302.17  < 2e-16 ***
# s(VRM)            1.000  1.000 363.66  < 2e-16 ***
# s(Cost.Distance)  1.394  1.671  30.38 6.54e-06 ***
# s(CostPath)       1.000  1.000  70.05  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1149.9  Scale est. = 4.0728e-15  n = 77

# all smooth terms are significant and has EDFs >1, so I'll keep them, but deviance explained and -REML are still
# suspicious

par(mfrow=c(2,2))
gam.check(rehab.kul260ucos.ms1.qpois)
dev.off()

# Fitting warning, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson distribution (THE SAME AS THESIS RESULT, EVEN THOUGH THIS HAS A DF MODEL WITH
# TRUNCATION AND IS NOT A SIMPLE DSM)


# Running Random Effect multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and Tweedie error distribution:


rehab.kul260ucos.ms.tw <- dsm(count ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") + 
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              kul260.u.cos$ddf, covjune, distdata_kul260, 
                              transect = "point", method = "REML", family = tw())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

# at first, it didn't run, but now did

summary(rehab.kul260ucos.ms.tw)
# Approximate significance of smooth terms:
#                        edf Ref.df     F p-value
# s(x,y)           1.433e+01 18.517 0.027   1.000
# s(NVDI)          1.001e+00  1.002 0.002   0.976
# s(hab)           3.118e-09  1.000 0.000   0.992
# s(elev)          1.001e+00  1.002 0.046   0.833
# s(VRM)           1.000e+00  1.000 0.004   0.950
# s(Cost.Distance) 1.000e+00  1.000 0.000   0.987
# s(CostPath)      1.018e+00  1.030 0.003   0.987

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = 1.1177  Scale est. = 4.0763    n = 77

# but the results are awful; all smooths are non significant, although their EDFs are > 1.
# I'll follow the same elimination path used for Gazelle, hab goes out first, and then until smooths are significant


rehab.kul260ucos.ms1.tw <- dsm(count ~ s(x, y) + 
                                 s(NVDI) + 
                                 #s(hab, bs = "re") +                # 1st eliminated
                                 s(elev) +
                                 s(VRM) + 
                                 s(Cost.Distance) +
                                 s(CostPath), 
                               kul260.u.cos$ddf, covjune, distdata_kul260, 
                               transect = "point", method = "REML", family = tw())

# it took more than 40 minutes and still nothing happened, so maybe don't run it if not necessary... In any case,
# I still don't have the file with covariates' values for each cellgrid, so no matter which model I could choose,
# I wouldn't be able to predict abundance for Kulan in any multiple smooth DSM analysis


# Running Random Effect multiple smooth DSM (CDS) with Uniform key cosine adjustment terms Detection Function, 
# 260m truncation and Negative Binomial error distribution: 

rehab.kul260ucos.ms.nb <- dsm(count ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") + 
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              kul260.u.cos$ddf, covjune, distdata_kul260, 
                              transect = "point", method = "REML", family = nb())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

# There was no warnings of any type in simple DSM (CDS) nor in simple DSM (MCDS), but it appeared first time in 
# multiple smooth DSM (MCDS) as in multiple smooth DSM (CDS) this kind of warning; and now here

summary(rehab.kul260ucos.ms.nb)
# Approximate significance of smooth terms:
#                        edf Ref.df Chi.sq p-value
# s(x,y)           8.430e+00      5      0     1.0
# s(NVDI)          1.000e+00      1      0     1.0
# s(hab)           1.566e-15      1      0     1.0
# s(elev)          1.000e+00      1      0     0.5
# s(VRM)           1.000e+00      1      0     1.0
# s(Cost.Distance) 1.000e+00      1      0     1.0
# s(CostPath)      1.000e+00      1      0     1.0

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -29.597  Scale est. = 1         n = 77

# it happened again to Kulan; for multiple smooth DSM, no matter if it's using MCDS or CDS, or if we're using 
# Random Effects as here, outside QPois, the other distributions present really weird p-values for their smooths
# Here, I will follow the same path of term elimination as for Gazelle; hab goes out first


rehab.kul260ucos.ms1.nb <- dsm(count ~ s(x, y) + 
                                 s(NVDI) + 
                                 #s(hab, bs = "re") +                  # 1st eliminated
                                 s(elev) +
                                 s(VRM) + 
                                 s(Cost.Distance) +
                                 s(CostPath), 
                               kul260.u.cos$ddf, covjune, distdata_kul260, 
                               transect = "point", method = "REML", family = nb())

# same warning as before

summary(rehab.kul260ucos.ms1.nb)
# Approximate significance of smooth terms:
#                   edf Ref.df Chi.sq p-value
# s(x,y)           8.43      5      0       1
# s(NVDI)          1.00      1      0       1
# s(elev)          1.00      1      0       1
# s(VRM)           1.00      1      0       1
# s(Cost.Distance) 1.00      1      0       1
# s(CostPath)      1.00      1      0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -29.597  Scale est. = 1         n = 77

# here I will follow the same path as before, the next term to be eliminated is elev
# As it's similar with this elimination, next will be VRM, CostPath and then Cost.Distance if necessary


rehab.kul260ucos.ms2.nb <- dsm(count ~ s(x, y) + 
                                 s(NVDI), # + 
                               #s(hab, bs = "re") +                  # 1st eliminated
                               #s(elev) +                            # 2nd eliminated
                               #s(VRM) +                             # 3rd eliminated                
                               #s(Cost.Distance), # +                # 5th eliminated
                               #s(CostPath),                         # 4th eliminated
                               kul260.u.cos$ddf, covjune, distdata_kul260, 
                               transect = "point", method = "REML", family = nb())

# same warning as before, but it stopped when I eliminated the 4th term

summary(rehab.kul260ucos.ms2.nb)
# Approximate significance of smooth terms:
#edf Ref.df Chi.sq p-value   
# s(x,y)  8.869  10.34 24.242 0.00873 **
# s(NVDI) 1.000   1.00  0.072 0.78798   

# R-sq.(adj) =      1   Deviance explained = 99.8%
# -REML = 20.288  Scale est. = 1         n = 77

# From the 4th term eliminated, their p-values were less than 1, but still non significant 
# this is not new, because same happened with NB distribution in multiple smooth DSM using CDS and/or MCDS for Kulan; 
# finally eliminating more smooths makes spatial smooth significant, but only it, so to not end up with just a 
# spatial DSM  model, I will keep NVDI smooth even it's not significant.


par(mfrow=c(2,2))
gam.check(rehab.kul260ucos.ms2.nb)
dev.off()

# better looking Q-Q and resids vs linear pred. plots than QPois
# But previous fitting warnings, no smooth term significant until just 2 remain in the model and suspiciously high 
# deviance explained value all suggest fitting problems for NB Random Effect multiple smooth DSM (CDS) model 
# (SEEN HERE AND ALSO AT MULTIPLE SMOOTH DSM WITH CDS/MCDS; IN ANY OTHER CIRCUMSTANCE NB DIDN'T PRESENT 
# FITTING PROBLEMS)


# Let's compare Kulan Random Effect multiple smooths DSM (CDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 2))
qq.gam(rehab.kul260ucos.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(rehab.kul260ucos.ms2.nb, asp = 1, rep = 200, main = "NB")
dev.off()


# Here, only NB distribution model have gam plots that looked OK-ish.  
# Both distribution models have a ridiculously high deviance explained (100% or so) and lower and negative REML 
# for Qpois, but positive for NB.
# Even though rehab.kul260ucos.ms2.nb has the spatial smooth significant, the other is not, making it 
# a pure simple (spatial) DSM 
# So, their non significant smooth terms (all of them in some cases with p-value = 1) puts NB distribution 
# model out; on the contrary Qpois model had smooth terms significant at their p-values.

# So, for the Kulan estimated abundance data adjusted by the detection function, 
# I will select the Random Effect multiple smooth DSM (CDS) Qpois error distribution (rehab.kul260ucos.ms1.qpois) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (AS IT WAS THE SAME FOR THIS ERROR DISTRIBUTION AT MCDS MULTIPLE SMOOTH DSM IN THIS EXERCISE)
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB)


# Density and Abundance estimation for Goitered gazelle in Random Effect multiple smooth DSM (CDS) ####
# (using multiple smooth DSM with HN key cosine adjustment term detection function, 80m truncation 
# and Tweedie error distribution: (rehab.gaz80hncos.ms1.tw) 

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_rehab.gaz80hncos.ms1.tw <- predict(rehab.gaz80hncos.ms1.tw, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
#  In predict.gam(object, newdata, type = type, ...) :
#  not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)


# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Density and Abundance estimation for Kulan in Random Effect multiple smooth DSM (CDS) ####
# (using multiple smooth DSM with Uniform key cosine adjustment term detection function, 260m truncation 
# and Qpois error distribution: (rehab.kul260ucos.ms1.qpois)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_rehab.kul260ucos.ms1.qpois <- predict(rehab.kul260ucos.ms1.qpois, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
# In predict.gam(object, newdata, type = type, ...) :
#   not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)

# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Fitting a Multiple Smooth DSM using Habitat Covariate as Random Effect in MCDS ####


# As we're using DF models that have covariates in the detection function, and following the example used in
# spatial DSM (MCDS) models before, I will use "abundance.est", which will leave the segment areas as they are 
# and calculate the Horvitz-Thompson estimates of the abundance per segment and use that as the response 
# in the model.


# Goitered Gazelle Random Effect multiple smooth DSM (MCDS) ####


# Running Random Effect multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and quasi-Poisson error distribution: 

rehab.gaz80hrhab.ms.qpois <- dsm(abundance.est ~ s(x, y) + 
                                   s(NVDI) + 
                                   s(hab, bs = "re") +
                                   s(elev) +
                                   s(VRM) + 
                                   s(Cost.Distance) +
                                   s(CostPath), 
                                 gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                                 transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(rehab.gaz80hrhab.ms.qpois)
# Approximate significance of smooth terms:
#                        edf Ref.df       F  p-value    
# s(x,y)           2.894e+01      6 6113.08  < 2e-16 ***
# s(NVDI)          1.000e+00      1  780.07  < 2e-16 ***
# s(hab)           3.393e-20      1    0.00    0.934    
# s(elev)          1.000e+00      1   25.83 8.12e-06 ***
# s(VRM)           1.000e+00      1   63.54  < 2e-16 ***
# s(Cost.Distance) 1.000e+00      1  184.04  < 2e-16 ***
# s(CostPath)      1.000e+00      1  328.25  < 2e-16 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1064.6  Scale est. = 3.5061e-15  n = 77

# hugely significant smooth terms, except for the one as a random effect, and EDFs > 1  
# same old story for Gazelle and habitat as numeric covariate; when treated as random effect, habitat is always the
# unique non significant term in QPois error distribution. Suspiciously high deviance explained and very low and
# negative -REML is present as always in Qpois. Next term eliminated will be hab

# By the way, nothing happened with the incongruence of hab covariate' class between DF (factor) and DSM (numeric)


rehab.gaz80hrhab.ms1.qpois <- dsm(abundance.est ~ s(x, y) + 
                                    s(NVDI) + 
                                    #s(hab, bs = "re") +               # 1st eliminated
                                    s(elev) +
                                    s(VRM) + 
                                    s(Cost.Distance) +
                                    s(CostPath), 
                                  gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                                  transect = "point", method = "REML")

# same warning as before

summary(rehab.gaz80hrhab.ms1.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df       F p-value    
# s(x,y)           28.88      8 1070.763  < 2e-16 ***
# s(NVDI)           1.00      1  490.856  < 2e-16 ***
# s(elev)           8.86      8  241.393  < 2e-16 ***
# s(VRM)            1.00      1    4.289    0.046 *  
#(Cost.Distance)  1.00      1  127.280 4.40e-13 ***
# s(CostPath)       1.00      1   22.975 3.18e-05 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML =  -1032  Scale est. = 4.3043e-15  n = 77

# with hab elimination, all smooth terms are hugely significant and has EDFs >1, so I'll keep them, 
# but deviance explained and -REML are still suspicious

par(mfrow=c(2,2))
gam.check(rehab.gaz80hrhab.ms1.qpois)
dev.off()

# Fitting warnings, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson distribution (THE SAME AS EVERY OTHER DSM WHEN GAZELLE AND QPOIS ARE INVOLVED)


# Running Random Effect multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and Tweedie error distribution: 

rehab.gaz80hrhab.ms.tw <- dsm(abundance.est ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") +
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                              transect = "point", method = "REML", family = tw())

# no warnings

summary(rehab.gaz80hrhab.ms.tw)
# Approximate significance of smooth terms:
#                        edf Ref.df     F p-value   
# s(x,y)           2.000e+00      2 7.415 0.00121 **
# s(NVDI)          1.000e+00      1 4.230 0.04350 * 
# s(hab)           2.735e-06      1 0.000 0.77457   
# s(elev)          1.000e+00      1 0.476 0.49271   
# s(VRM)           1.000e+00      1 1.992 0.16265   
# s(Cost.Distance) 1.000e+00      1 5.281 0.02460 * 
# s(CostPath)      1.000e+00      1 5.200 0.02568 * 

# R-sq.(adj) =  0.859   Deviance explained = 72.9%
# -REML = 42.786  Scale est. = 12.635    n = 77

# most non significant term is hab; that will be the first eliminated, and so on until have only significant terms. 
# EDFs are >= 1 and more "normal" than in Qpois


rehab.gaz80hrhab.ms1.tw <- dsm(abundance.est ~ s(x, y) + 
                                 s(NVDI), # + 
                               #s(hab, bs = "re") +                        # 1st eliminated
                               #s(elev) +                                  # 2nd eliminated
                               #s(VRM) +                                   # 3rd eliminated
                               #s(Cost.Distance), # +                      # 5th eliminated
                               #s(CostPath),                               # 4th eliminated
                               gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                               transect = "point", method = "REML", family = tw())

# no warnings, as before

summary(rehab.gaz80hrhab.ms1.tw)
# Approximate significance of smooth terms:
#         edf Ref.df      F  p-value    
# s(x,y)    2      2  4.879 0.010262 *  
# s(NVDI)   1      1 11.780 0.000989 ***

# R-sq.(adj) =  0.364   Deviance explained = 52.8%
# -REML = 49.371  Scale est. = 16.82     n = 77

# remaining smooths are significant and have EDFs >= 1, so I'll keep them

par(mfrow=c(2,2))
gam.check(rehab.gaz80hrhab.ms1.tw)
dev.off()

# No fitting warnings, lower EDFs (less wiggly) and GAM check plots (Q-Q plot and resids vs linear pred.) 
# looked OK-ish (resids vs linear more of a "starry night" than qpois distribution). Spatial and NVDI smooths 
# significant at their p-values


# Running Random Effect multiple smooth DSM (MCDS) with HR key and habitat covariate in Detection Function, 
# 80m truncation and Negative Binomial error distribution: 

rehab.gaz80hrhab.ms.nb <- dsm(abundance.est ~ s(x, y) + 
                                s(NVDI) + 
                                s(hab, bs = "re") +
                                s(elev) +
                                s(VRM) + 
                                s(Cost.Distance) +
                                s(CostPath), 
                              gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                              transect = "point", method = "REML", family = nb())

# no warnings

summary(rehab.gaz80hrhab.ms.nb)
# Approximate significance of smooth terms:
#                     edf Ref.df Chi.sq p-value  
# s(x,y)           2.0001  2.000  5.594   0.061 .
# s(NVDI)          1.0000  1.000  0.684   0.408  
# s(hab)           0.7652  1.000  0.548   0.396  
# s(elev)          1.0000  1.000  0.317   0.574  
# s(VRM)           1.2627  1.481  0.268   0.852  
# s(Cost.Distance) 1.0000  1.000  1.967   0.161  
# s(CostPath)      1.0000  1.000  1.143   0.285  

# R-sq.(adj) =  -0.173   Deviance explained = 52.2%
# -REML = 45.788  Scale est. = 1         n = 77

# very bad situation, where every smooth term is non significant... I'll use the same path as with Tweedie model
# (it happened for every situation when using NB in multiple smooth DSM for Gazelle)


rehab.gaz80hrhab.ms1.nb <- dsm(abundance.est ~ s(x, y) + 
                                 s(NVDI), # + 
                               #s(hab, bs = "re") +                  # 1st eliminated
                               #s(elev) +                            # 2nd eliminated
                               #s(VRM) +                             # 3rd eliminated
                               #s(Cost.Distance), # +                # 5th eliminated
                               #s(CostPath),                         # 4th eliminated
                               gaz80.hr.hab$ddf, covjune, distdata_gaz80, 
                               transect = "point", method = "REML", family = nb())

# no warnings, as before

summary(rehab.gaz80hrhab.ms1.nb)
# Approximate significance of smooth terms:
#         edf Ref.df Chi.sq p-value  
# s(x,y)    2      2  4.587  0.1009  
# s(NVDI)   1      1  6.241  0.0125 *

# R-sq.(adj) =  0.0807   Deviance explained = 39.2%
# -REML = 53.033  Scale est. = 1         n = 77

# spatial smooth had a mixed behaviour along the model decision; here is again non significant, but eliminate it
# would mean ending with a pure environmental model (habitat model), which is not my aim. So I will keep it in
# this model with NB distribution, but in the end the one with TW distribution will be selected

par(mfrow=c(2,2))
gam.check(rehab.gaz80hrhab.ms1.nb)
dev.off()

# No fitting warnings and GAM check plots (Q-Q plot and resids vs linear pred.) that looked OK-ish 
# (Q-plot looks similarly fitted as in Tweedie distribution, but resids vs linear looks better in Tweedie, 
# and certainly better than in qpois distribution); but spatial smooth is not significant at 
# its p-value 


# Let's compare Gazelle Random Effect multiple smooths DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(rehab.gaz80hrhab.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(rehab.gaz80hrhab.ms1.tw, asp = 1, rep = 200, main = "Tw")
qq.gam(rehab.gaz80hrhab.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()

# Only Tweedie and Negative Binomial distribution models have gam plots that looked OK-ish. (here TW Q-Q plot looked
# more fitted than NB). 
# Qpois models are out of competition by its ridiculously high deviance explained and low REML. Also, their non
# significant spatial smooth and lower deviance explained puts NB distribution model out too.

# So, for the Goitered gazelle estimated abundance data adjusted by the detection function, 
# I will select the Random Effect multiple smooth DSM (MCDS) with TW error distribution (rehab.gaz80hrhab.ms1.tw) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION TOO, BUT DF HN-NO ADJS WASN'T TRUNCATED)


# Kulan Random Effect multiple smooth DSM (MCDS) #####


# Running Random Effect multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and quasi-Poisson error distribution: 

rehab.kul260hnhab.ms.qpois <- dsm(abundance.est ~ s(x, y) + 
                                    s(NVDI) + 
                                    s(hab, bs = "re") +
                                    s(elev) +
                                    s(VRM) + 
                                    s(Cost.Distance) +
                                    s(CostPath), 
                                  kul260.hn.hab$ddf, covjune, distdata_kul260, 
                                  transect = "point", method = "REML")

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

summary(rehab.kul260hnhab.ms.qpois)
# Approximate significance of smooth terms:
#                        edf Ref.df        F p-value    
# s(x,y)           2.400e+01  5.000 235.933  < 2e-16 ***
# s(NVDI)          1.727e+00  2.140  11.893 9.12e-05 ***
# s(hab)           1.567e-19  1.000   0.000    0.788    
# s(elev)          1.384e+00  1.598 922.194  < 2e-16 ***
# s(VRM)           1.000e+00  1.000 721.030  < 2e-16 ***
# s(Cost.Distance) 5.011e+00  5.000   1.546    0.201    
# s(CostPath)      7.412e+00  5.000  22.809 3.53e-10 ***  

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1155.8  Scale est. = 4.5101e-15  n = 77

# it was the same for Kulan in random effect multiple smooth DSM (CDS), hab is the most non significant, 
# accompanied in 2nd place by Cost.Distance. All EDFs are >= 1. 1st eliminated will be hab


rehab.kul260hnhab.ms1.qpois <- dsm(abundance.est ~ s(x, y) + 
                                     s(NVDI) + 
                                     #s(hab, bs = "re") +                   # 1st eliminated
                                     s(elev) +
                                     s(VRM) + 
                                     s(Cost.Distance) +
                                     s(CostPath), 
                                   kul260.hn.hab$ddf, covjune, distdata_kul260, 
                                   transect = "point", method = "REML")

# same warning

summary(rehab.kul260hnhab.ms1.qpois)
# Approximate significance of smooth terms:
#                     edf Ref.df        F  p-value    
# s(x,y)           21.564  5.000  276.690  < 2e-16 ***
# s(NVDI)           2.727  3.323   21.805  < 2e-16 ***
# s(elev)           1.456  1.694 1043.524  < 2e-16 ***
# s(VRM)            1.000  1.000  957.590  < 2e-16 ***
# s(Cost.Distance)  5.953  5.000    5.133  0.00102 ** 
# s(CostPath)       4.036  4.836   10.765 2.81e-06 ***

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -1150.9  Scale est. = 4.1988e-15  n = 77

# Now all the smooths are significant and have EDFs > 1, so I'll keep them all in the model

par(mfrow=c(2,2))
gam.check(rehab.kul260hnhab.ms1.qpois)
dev.off()

# Fitting warning, higher EDFs (compared to other error distributions), gam.check plots (mostly Q-Q plot and 
# resids vs linear pred. flat and not OK) and suspiciously high deviance explained and very low -REML value all suggest 
# fitting problems for quasi-Poisson distribution (THE SAME AS EVERY SITUATION WHERE QPOIS DISTRIBUTION IS INVOLVED)


# Running Random Effect multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and Tweedie error distribution: 

rehab.kul260hnhab.ms.tw <- dsm(abundance.est ~ s(x, y) + 
                                 s(NVDI) + 
                                 s(hab, bs = "re") +
                                 s(elev) +
                                 s(VRM) + 
                                 s(Cost.Distance) +
                                 s(CostPath), 
                               kul260.hn.hab$ddf, covjune, distdata_kul260, 
                               transect = "point", method = "REML", family = tw())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Iteration limit reached without full convergence - check carefully

# First time this happened; before it only was looping endlessly and I shut it down. But when I assigned it full
# priority (more cores I guess) it finished, but not without a warning message as the one before. So, I'll come
# back and rerun TW model for kulan in random effect multiple smooth DSM (CDS). Anyway, spoiler alert: this is not
# the selected model, so don't run it unless it's necessary.

summary(rehab.kul260hnhab.ms.tw)
# Approximate significance of smooth terms:
#                        edf Ref.df F p-value
# s(x,y)           2.251e+00  2.371 0       1
# s(NVDI)          1.000e+00  1.000 0       1
# s(hab)           3.950e-15  1.000 0       1
# s(elev)          1.903e+00  2.377 0       1
# s(VRM)           1.000e+00  1.000 0       1
# s(Cost.Distance) 1.000e+00  1.000 0       1
# s(CostPath)      2.821e+00  3.401 0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -32.742  Scale est. = 4.7848    n = 77

# And, as it could be expected, the result is really weird, with all smooths non significant with a p-value of 1, 
# and EDFs > 1 ; but happened before for Kulan at multiple smooth DSM (CDS and MCDS). 
# I'll follow the same elimination path used for Gazelle, hab goes out first, and then until smooths are significant


rehab.kul260hnhab.ms1.tw <- dsm(abundance.est ~ s(x, y) + 
                                  s(NVDI) + 
                                  #s(hab, bs = "re") +                     # 1st eliminated
                                  s(elev) +
                                  s(VRM) + 
                                  s(Cost.Distance) +
                                  s(CostPath), 
                                kul260.hn.hab$ddf, covjune, distdata_kul260, 
                                transect = "point", method = "REML", family = tw())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

# different warning... that's good right?

summary(rehab.kul260hnhab.ms1.tw)
# Approximate significance of smooth terms:
#                    edf Ref.df F p-value
# s(x,y)           7.671      5 0     1.0
# s(NVDI)          1.000      1 0     1.0
# s(elev)          1.000      1 0     0.5
# s(VRM)           1.000      1 0     1.0
# s(Cost.Distance) 1.000      1 0     1.0
# s(CostPath)      1.000      1 0     1.0

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -28.93  Scale est. = 4.7846    n = 77

# here I will follow the same path as before, the next term to be eliminated is elev
# As it's similar with this elimination path, next will be VRM, CostPath and then Cost.Distance if necessary


rehab.kul260hnhab.ms2.tw <- dsm(abundance.est ~ s(x, y) + 
                                  s(NVDI), # + 
                                #s(hab, bs = "re") +                     # 1st eliminated
                                #s(elev) +                               # 2nd eliminated
                                #s(VRM) +                                # 3rd eliminated
                                #s(Cost.Distance), # +                   # 5th eliminated
                                #s(CostPath),                            # 4th eliminated
                                kul260.hn.hab$ddf, covjune, distdata_kul260, 
                                transect = "point", method = "REML", family = tw())

# same warning as before, but it stopped when I eliminated the 4th term

summary(rehab.kul260hnhab.ms2.tw)
# Approximate significance of smooth terms:
#           edf Ref.df     F p-value
# s(x,y)  8.756  10.29 1.666   0.108
# s(NVDI) 1.000   1.00 0.043   0.836

# R-sq.(adj) =      1   Deviance explained = 99.5%
# -REML = 22.585  Scale est. = 4.7822    n = 77


# From the 4th term eliminated, their p-values were less than 1, but still non significant 
# this is not new, because same happened with NB distribution in multiple smooth DSM using CDS and/or MCDS for Kulan; 
# finally eliminating more smooths leaves spatial and NVDI smooths non significant, so I will keep both smooths even 
# they are not significant, but in the end another error distribution model will be selected.

par(mfrow=c(2,2))
gam.check(rehab.kul260hnhab.ms2.tw)
dev.off()


# better looking Q-Q and resids vs linear pred. plots than QPois
# But previous fitting warnings, no smooth term significant in the model and suspiciously high 
# deviance explained value all suggest fitting problems for TW Random Effect multiple smooth DSM (MCDS) model 
# (SEEN HERE AND ALSO AT MULTIPLE SMOOTH DSM WITH CDS/MCDS; IN ANY OTHER CIRCUMSTANCE TW DIDN'T PRESENT 
# FITTING PROBLEMS)


# Running Random Effect multiple smooth DSM (MCDS) with HN key and habitat covariate in Detection Function, 
# 260m truncation and Negative Binomial error distribution: 

rehab.kul260hnhab.ms.nb <- dsm(abundance.est ~ s(x, y) + 
                                 s(NVDI) + 
                                 s(hab, bs = "re") +
                                 s(elev) +
                                 s(VRM) + 
                                 s(Cost.Distance) +
                                 s(CostPath), 
                               kul260.hn.hab$ddf, covjune, distdata_kul260, 
                               transect = "point", method = "REML", family = nb())

# Warning message:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#   Fitting terminated with step failure - check results carefully

# this is not new for Negative Binomial error distribution models; there was no warnings in simple DSM (CDS) nor in 
# simple DSM (MCDS), but it appeared first time in multiple smooth DSM (MCDS) as in multiple smooth DSM (CDS), and also
# in both Random Effect models this kind of warning

summary(rehab.kul260hnhab.ms.nb)
# # Approximate significance of smooth terms:
#                        edf Ref.df Chi.sq p-value
# s(x,y)           8.786e+00      5      0       1
# s(NVDI)          1.000e+00      1      0       1
# s(hab)           2.989e-16      1      0       1
# s(elev)          1.000e+00      1      0       1
# s(VRM)           1.000e+00      1      0       1
# s(Cost.Distance) 1.000e+00      1      0       1
# s(CostPath)      1.000e+00      1      0       1

# R-sq.(adj) =      1   Deviance explained =  100%
# -REML = -25.047  Scale est. = 1         n = 77

# it happened again; for multiple smooth DMS, no matter if it's using MCDS or CDS, or if we're using Random Effect 
# as here outside QPois, TW and NB distributions presented really weird p-values for their smooths
# Here, I will follow the same path of term elimination as for Gazelle; hab goes out first


rehab.kul260hnhab.ms1.nb <- dsm(abundance.est ~ s(x, y) + 
                                  s(NVDI), # + 
                                #s(hab, bs = "re") +                   # 1st eliminated
                                #s(elev) +                             # 2nd eliminated
                                #s(VRM) +                              # 3rd eliminated
                                #s(Cost.Distance), # +                 # 5th eliminated
                                #s(CostPath),                          # 4th eliminated
                                kul260.hn.hab$ddf, covjune, distdata_kul260, 
                                transect = "point", method = "REML", family = nb())



# same warning as before, but it stopped when I eliminated the 4th term

summary(rehab.kul260hnhab.ms1.nb)
# Approximate significance of smooth terms:
#           edf Ref.df Chi.sq p-value    
# s(x,y)  8.857  10.24 55.744  <2e-16 ***
# s(NVDI) 1.000   1.00  0.067   0.795    

# R-sq.(adj) =      1   Deviance explained = 99.9%
# -REML = 25.041  Scale est. = 1         n = 77

# since first elimination, all smooth terms' p-values are 1, and EDFs are > 1 . And from the 4th elimination, remaining
# terms lowered their p-values, but still remained non significant until 5th elimination.

# this is not new, because same happened with NB distribution in multiple smooth DSM using CDS and/or MCDS for Kulan
# and even now in Random Effect multiple smooth DSM CDS/MCDS
# finally eliminating more smooths makes spatial smooth significant, but only it, so to not end up with a simple DSM 
# (spatial) model, I will keep NVDI smooth even it's not significant. 

par(mfrow=c(2,2))
gam.check(rehab.kul260hnhab.ms1.nb)
dev.off()

# better looking Q-Q and resids vs linear pred. plots than QPois, but not ideal
# Anyway, previous fitting warnings, hugely non significant smooth terms at first and suspiciously high deviance 
# explained value all suggest fitting problems for NB Random Effect multiple smooth DSM (MCDS) model 
# (SEEN AT BOTH MULTIPLE SMOOTH DSM WITH CDS/MCDS; IN ANY OTHER CIRCUMSTANCE NB DIDN'T PRESENT 
# FITTING PROBLEMS)


# Let's compare Kulan Random Effect multiple smooths DSM (MCDS) models' Q-Q plots by their distribution
set.seed(1233)

par(mfrow = c(1, 3))
qq.gam(rehab.kul260hnhab.ms1.qpois, asp = 1, rep = 100, main = "Qpois")
qq.gam(rehab.kul260hnhab.ms2.tw, asp = 1, rep = 100, main = "Tw")
qq.gam(rehab.kul260hnhab.ms1.nb, asp = 1, rep = 200, main = "NB")
dev.off()


# Here, TW and NB distribution models have gam plots that looked OK-ish.  
# But, three distribution models have fitting warnings, a ridiculously high deviance explained (100% or so) and 
# lower and negative REML for Qpois, but positive for TW and NB.
# TW model is out of competition due to its non significant smooth terms included, 
# Even though NB model has the spatial smooth significant, the other is not, making it a pure simple (spatial) DSM 
# putting NB distribution model out; on the contrary Qpois model had smooth terms significant at their p-values.

# So, for the Kulan estimated abundance data adjusted by the detection function, 
# I will select the Random Effect multiple smooth DSM (MCDS) Qpois error distribution (rehab.kul260hnhab.ms1.qpois) ####
# to estimate its density and abundance, and to map its distribution in the GGASPA.
# (AS IT WAS THE SAME AT EVERY MULTIPLE SMOOTH DSM IN THIS EXERCISE)
# (IN THESIS, SELECTED DSM CDS WAS TWEEDIE DISTRIBUTION, BUT DF HN-NO ADJS WASN'T TRUNCATED)
# (IN SIMPLE DSM CDS WITH TRUNCATION, SELECTED DISTRIBUTION WAS NB)


# Density and Abundance estimation for Goitered gazelle in Random Effect multiple smooth DSM (MCDS) ####
# (using multiple smooth DSM with HR key and habitat covariate detection function, 80m truncation 
# and Tweedie error distribution: (rehab.gaz80hrhab.ms1.tw) 

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_rehab.gaz80hrhab.ms1.tw <- predict(rehab.gaz80hrhab.ms1.tw, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
#  In predict.gam(object, newdata, type = type, ...) :
#  not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)


# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO


# Density and Abundance estimation for Kulan in Random Effect multiple smooth DSM (MCDS) ####
# (using multiple smooth DSM with HN key and habitat covariate detection function, 260m truncation 
# and Qpois error distribution: (rehab.kul260hnhab.ms1.qpois)

#off.set <- 1000 * 1000      # size of cell grid over which estimate density and abundance, already done
# in simple DSM (CDS) stage

predn_rehab.kul260hnhab.ms1.qpois <- predict(rehab.kul260hnhab.ms1.qpois, grd.in, off.set)

# Error in eval(predvars, data, env) : object 'NVDI' not found
# In addition: Warning message:
# In predict.gam(object, newdata, type = type, ...) :
#   not all required variables have been supplied in  newdata!

# (was the same for each error distribution model tried)

# I THINK I COULD ONLY ADVANCE IN MULTIPLE SMOOTH DSM (CDS) UNTIL HERE, BECAUSE WARNING IS STATING THE LACK OF 
# "NVDI" INFO. IN  FACT, IT IS MISSING A LOT OF VALUES FOR EACH COVARIATE INCLUDED AS A SMOOTH TERM IN DSM FOR
# EVERY SQUARE GRID CELL FROM "GRD.IN" DATAFILE (WHICH HAS 45,864 SQUARE GRID CELLS). PREDICT() FUNCTION WILL FILL IN
# THE VARIABLES IN THE MODEL FORMULA WITH THESE VALUES TO OBTAIN DENSITY/ABUNDANCE PREDICTIONS AT EACH CELL OF
# PREDICTION GRID

# I NEED TO COMPLETE FIRST "GRID.IN" DATAFILE WITH COVARIATES VALUES AT EACH CELL OF THE GRID IN ORDER TO GET 
# A DENSITY/ABUNDANCE PREDICTION. IN ANY CASE, I'M ASKING DAVID MILLER FOR THIS TOO



