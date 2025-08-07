#PLACE BOTH FILES (brt_boot_parallel_main_function and brt_boot_parallel_input) TO YOUR WORKING DIRECTORY
#THIS NEEDS TO BE SET MANUALLY
#setwd("C:\\Users\\Administrator\\Desktop\\sea\\brt automaatskript (apr2020)") #your working directory

library(readxl) #reading excel
library(openxlsx) #writing excel
library(plyr) #rename columns
library(sf)
library(terra)
library(data.table)

andmed=readRDS("QuantitativeSamplesBiomassesKeySpeciesWithCopernicusDataAndDepth.rds") #data for model fitting

names(andmed)
head(andmed)

valjaennustus=FALSE #Do we want to use the model to make numerical predictions? If so, please provide the name of the dataset.
#andmedvalja=readRDS("DepthDataForCopernicus.rds") #respective dataset

if(valjaennustus==TRUE){
newcoord=project(as.matrix(andmed[,c("longitude","latitude")]),from="epsg:4326",to="epsg:3035") #Converts coordinates of training data from WGS84 to ETRS89. Traditionally, the WGS84 projection is used in field sampling, while planar projections are often used for map generation. The ETRS89 projection is used as it represents the standard European planar projection.
andmedvalja=andmedvalja[x>min(newcoord[,1])&x<max(newcoord[,1])&y>min(newcoord[,2])&y<max(newcoord[,2]),] #truncate the field data set based on the coordinates of the training data set
}

piltvalja=FALSE #Do we want to draw a map using the model predictions (the respective file then also needs to have variables X and Y for coordinates)
epsgvalue=3301 #epsg code for the projection eg 3301 (for est1997) or 3035 (for ETRS89 / ETRS-LAEA)

sisendfailina=TRUE #Do we want to generate and fit models described in a file (one row for each model; semicolons separate positions; variables are given by order numbers; dependent variable is in the first position; independent variables are in the second position; direction of relationships in the third position (positive=1,arbitrary=0,negative=-1)?
#example: 13;1:7,9;0,0,0,0,0,0,0,1 in the file means, that dependent variable is variable number 13 in the dataset, variables 1 to 7 and 9 are independent variables, the direction of the effect of last variable is positive, arbitrary for the first six
sisendfail=read.table("mudelid.txt",header=F,dec=".",sep=";",colClasses=c("numeric","character","character")) #File of the desired models

names(sisendfail)

tree.complexity=5 #brt model parameter #Describes the complexness of interactions allowed
lrvalue=0.01 #brt model parameter #learning rate 
timer=10800 #limit time for one model in seconds 
bootn=30 #how many bootstrap iterations are carried out for producing standard errors for predictions and standard errors for predictor curves; typically below 100 is not a good idea 
solmi=30 #for how many different argument values do the the predictor curve values get estimated
abikataloog=getwd() #do not change

######################################################################################
#NECESSARY LIBRARIES gbm; pdp; data.table; raster; rgdal; stringr; doParallel; R.utils
######################################################################################
library(gbm)
library(pdp)
library(data.table)
library(raster)
#library(rgdal) #seda paketti enam pole
library(doParallel)
library(R.utils)
library(stringr)
#######################################################
source("brt_boot_parallel_main_function.r")
#######################################################

#AND THIS STARTS THE RUN!
fun_brt_boot()