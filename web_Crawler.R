###############################################################################
# Author: @BrockTibert
# Purpose: Collect Historical NHL Skater Stats 1960 - 2011 (in progress)
# Date: February 2011
#
# Used: R Version 2.12.1, Windows 7 Pro, StatET Plugin for Eclipse
#
# # Copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved. 
###############################################################################

## libraries
library(XML)

## directory for the project
# DIR <- "C:/Users/Brock/Documents/My Dropbox/Projects/NHL"
DIR <- "C:/Data_mining"
setwd(DIR)


URL <- paste("http://www.hockey-reference.com/leagues/NHL_", 
             2011, "_skaters.html", sep="")


#-----------------------------------------------------------------------
# Create a function that will take a year and return a dataframe
#-----------------------------------------------------------------------

GrabSkaters <- function(S) {
  
  # The function takes parameter S which is a string and represents the Season
  # Returns: data frame    
  
  ## create the URL
  URL <- paste("http://www.hockey-reference.com/leagues/NHL_", 
               S, "_skaters.html", sep="")
  
  ## grab the page -- the table is parsed nicely
  tables <- readHTMLTable(URL)
  ds.skaters <- tables$stats
  
  ## determine if the HTML table was well formed (column names are the first record)
  ## can either read in directly or need to force column names
  ## and 
  
  ## I don't like dealing with factors if I don't have to
  ## and I prefer lower case
  for(i in 1:ncol(ds.skaters)) {
    ds.skaters[,i] <- as.character(ds.skaters[,i])
    names(ds.skaters) <- tolower(colnames(ds.skaters))
  }
  
  ## fix a couple of the column names
  colnames(ds.skaters)
  names(ds.skaters)[10] <- "plusmin"
  names(ds.skaters)[17] <- "spct"
  
  ## finally fix the columns - NAs forced by coercion warnings
  for(i in c(1, 3, 6:18)) {
    ds.skaters[,i] <- as.numeric(ds.skaters[, i])
  }
  
  ## convert toi to seconds, and seconds/game
 # ds.skaters$seconds <- (ds.skaters$toi*60)/ds.skaters$gp
  
  ## remove the header and totals row
  ds.skaters <- ds.skaters[!is.na(ds.skaters$rk), ]
  ds.skaters <- ds.skaters[ds.skaters$tm != "TOT", ]
  
  ## add the year
  ds.skaters$season <- S
  
  ## return the dataframe
  return(ds.skaters)
  
}




#-----------------------------------------------------------------------
# Use the function to loop over the seasons and piece together
#-----------------------------------------------------------------------

## define the seasons -- 2005 dataset doesnt exist
## if I was a good coder I would trap the error, but this works
# SEASON <- as.character(c(1960:2004, 2006:2011))
SEASON <- as.character(c(2009:2011))


## create an empy dataset that we will append to
dataset <- data.frame()

## loop over the seasons, use the function to grab the data
## and build the dataset
for (S in SEASON) {
  
  temp <- GrabSkaters(S)
  dataset <- rbind(dataset, temp)
  print(paste("Completed Season ", S, sep=""))
  
  ## pause the script so we don't kill their servers
  Sys.sleep(3)
  
}

## save the dataset
write.table(dataset, "Historical Skater Stats.csv", sep=",",
            row.names=F)