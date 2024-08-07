################################################################################
#                                                                              #
# Spatial homogamy & online partner markets                                    #
# Leena Maa� & Andreas Filser                                                  #      
# Replication Code                                                             #
#                                                                              #
################################################################################


# load packages ----------------------------------------------------------------
if(Sys.getenv("USERNAME")=="filse") .libPaths("D:/R-library4") # f�r AFs R library - irgendwann l�schen wenn's rausgeht
pcks <- c("marginaleffects","tidyverse", "tidyverse", "readstata13", "data.table","haven")
#install.packages(pcks)
lapply(pcks, library, character.only = TRUE)


# data preparation -------------------------------------------------------------
source("./prog/01_data_preparation.R")


# bivariate models -------------------------------------------------------------

# pooled models
source("./prog/modelle_bivariat_gepoolteOLS.R")
# creates Figure ... 


# fixed effects models
source("./prog/modelle_bivariat_fixedeffects.R")
# creates Figure ... 

# multivariate models ----------------------------------------------------------
source("./prog/modelle_vollst�ndig_lm_fixed.R")

