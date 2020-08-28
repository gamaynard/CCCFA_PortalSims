## ---------------------------
##
## Script name: LLB_haddock_Subsample_App.R
##
## Purpose of script: Runs an RShiny application to simulate discards of haddock
##    on longline fishing trips and generate estimates of discard weight from
##    different subsampling protocols on those trips
##
## Author: George A. Maynard
##
## Date Created: 2020-08-28
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory
## If working on the development machine, set working directory automatically,
## else, working directory is set to the user's default
if(Sys.info()[[4]]=="HOOK-05"){
  setwd("C:/Users/George/Desktop/Autotask Workplace/Personal Documents/Development/ShinyApps/PortalSims/")
}
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)

## ---------------------------

## load up our functions into memory

## ---------------------------

## Read in and format necessary data
## "data" is a file of lengths of individual discarded haddock along with a trip
## and haul index value for each one (e.g. trip 1, haul 1) to allow for viewing
## the data at a more granular level
data=read.csv("https://raw.githubusercontent.com/gamaynard/CCCFA_PortalSims/master/LLB_haddock_lengths.csv")
data$GEAR=as.character(data$GEAR)
data$TRIP=as.numeric(as.character(data$TRIP))
data$HAUL=as.numeric(as.character(data$HAUL))
data$SPECIES=as.character(data$SPECIES)
data$QUANTITY=as.numeric(as.character(data$QUANTITY))
data$LENGTH=as.numeric(as.character(data$LENGTH))

## "lw" contains the ln(alpha) and beta values necessary to estimate weights of 
## most groundfish species from lengths using the formula:
##    weight=exp(log(a)+b*log(length)) ## kg
##    weight=exp(log(a)+b*log(length))*2.204623 ## lbs
lw=read.csv("https://raw.githubusercontent.com/gamaynard/CCCFA_PortalSims/master/LengthWeightEquations.csv")
## --------------------------


