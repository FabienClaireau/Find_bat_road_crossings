#####################################################
###   Trajectographie acoustique - Approche AFPR  ###
###         Script 3 - Find bat crossing          ###
###                     ######                    ###
### Script F. Claireau & Y. Bas - New formatting  ###
###           & corrections T. Ferraille          ###
#####################################################

####################
### Introduction ###
####################

# The acoustic trajectory approach using AFPR, developed by Fabien CLAIREAU and Yves BAS of CESCO-MNHN: Claireau F. et al (2018) Bat overpasses: an insufficient solution to encourage bats across roads https://doi.org/10.1111/1365-2664.13288 MNHN-CESCO/Greifswald University/Naturalia Environnement

# Three R scripts must be run successively:
# -   Script 1 - New Pairing: calculates the TDOA between two microphones of a recorder
# -   Script 2 - New Auto-Offset: calculates the time offset between two recorders
# -   Script 3 - New Find Bat Crossings: matches two recorders and defines crossings

## Inputs and Outputs for Script 3:

# **Inputs for Script 3:**
# -   ListPairs_Dec.csv: output from Script 2
# -   Open the file and check if the 'decalageFinal' column contains any NA values; this would indicate that one of the recorders in the pair recorded in mono rather than stereo.
# -   Delete the row if this is the case.
# -   In any case, save the file as 'ListPairs_Dec_Man' in CSV format (delimiter: semicolon).
# -   TrajTot.csv: output from Script 1 – a file containing all the TDOA data.

# **Outputs for Script 3:**
# -   EXPORTED FILE: TravFinal.csv
# -   This contains the final bat crossings identified by recorder pairs.
# -   Summary of information using a pivot table.



####################################################
### Parameter Section to Modify: paths and files ###
####################################################

rm(list = ls()) # Clear existing objects in the workspace

# Set the path to your tables (excel files, etc (WARNING use "/" instead of "\")
chemin_travail <- "C:/Users/FabienCLAIREAU/Desktop/Formation_MNHN/tables"



########################################
### Execution Section: DO NOT MODIFY ###
###       CTRL A > CTRL ENTER        ###
########################################


# Installing and loading packages
install_load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}
install_load_packages(c("data.table", "Hmisc", "dplyr"))

# ---- Playback of input files ---- #
setwd(chemin_travail)
# Reading TrajTot and ListPairs data
TrajTot <- fread("TrajTot.csv") %>% as.data.frame() # CORRECTION # Data produced during the first stage
ListPaires <- fread("ListPairs_Dec_Man.csv") %>% as.data.frame() # CORRECTION # File produced after deleting lines containing NA

# Initialise the final list of results
TravFinal <- NULL # CORRECTION list()

# ---- Data processing for each pair ---- #
for (h in 1:nrow(ListPaires)) {
  
  # Subsets for both sides of the pair
  TrajTot1 <- subset(TrajTot, participation %in% ListPaires$participation_1[h])
  TrajTot1 <- cbind(TrajTot1, Cote = 1, Point = ListPaires$point_1[h])
  
  TrajTot2 <- subset(TrajTot, participation %in% ListPaires$participation_2[h])
  TrajTot2 <- cbind(TrajTot2, Cote = 2, Point = ListPaires$point_2[h])
  
  # Merging the two subsets
  Trav <- rbind(TrajTot1, TrajTot2)
  
  # Converting columns to numeric
  Trav$DecDeb <- as.numeric(as.character(Trav$DecDeb))
  Trav$DecFin <- as.numeric(as.character(Trav$DecFin))
  Trav$Prob <- as.numeric(as.character(Trav$probabilite))
  
  # Extract time components from files
  Date <- substr(as.character(Trav$File0), nchar(Trav$File0) - 18, nchar(Trav$File0) - 11)
  Heure <- substr(as.character(Trav$File0), nchar(Trav$File0) - 9, nchar(Trav$File0) - 8)
  Minute <- substr(as.character(Trav$File0), nchar(Trav$File0) - 7, nchar(Trav$File0) - 6)
  Seconde <- substr(as.character(Trav$File0), nchar(Trav$File0) - 5, nchar(Trav$File0) - 4)
  MiliSec <- substr(as.character(Trav$File0), nchar(Trav$File0) - 2, nchar(Trav$File0))
  
  # Conversion of time components to digital and calculation of time in seconds
  Temps <- as.numeric(Heure) * 3600 + as.numeric(Minute) * 60 + as.numeric(Seconde) + as.numeric(MiliSec) / 1000
  Trav <- cbind(Trav, Heure = as.numeric(Heure), Minute = as.numeric(Minute), Seconde = as.numeric(Seconde), MiliSec = as.numeric(MiliSec), Temps, Date, Datenum = as.numeric(Date))
  
  # ---- Extraction of input and output events ---- #
  Ent1 <- subset(Trav, DecDeb > 0 & DecFin < 0 & Cote == 1) 
  Sort1 <- subset(Trav, DecDeb < 0 & DecFin > 0 & Cote == 1) 
  Ent2 <- subset(Trav, DecDeb > 0 & DecFin < 0 & Cote == 2)
  Sort2 <- subset(Trav, DecDeb < 0 & DecFin > 0 & Cote == 2)
  
  # Checking the existence of trajectory conditions for both sides
  if (((nrow(Ent1) == 0 | nrow(Sort2) == 0)) & (nrow(Ent2) == 0 | nrow(Sort1) == 0)) {
    TravTot12 <- NA
  } else {
    
    # Apply the offset to the trajectory (adjust the times)
    decalage <- as.numeric(as.character(ListPaires$decalageFinal[h]))
    Ent1$Temps <- Ent1$Temps + decalage
    Sort1$Temps <- Sort1$Temps + decalage
    
    # ---- Matching input and output events ---- #
    if (nrow(Ent1) > 0 & nrow(Sort2) > 0) {
      L12 <- unique(c(levels(as.factor(Ent1$espece)), levels(as.factor(Sort2$espece))))
      TC12 <- find.matches(cbind(Ent1$Datenum, match(Ent1$espece, L12), Ent1$Temps), 
                           cbind(Sort2$Datenum, match(Sort2$espece, L12), Sort2$Temps), 
                           tol = c(0, 0, (ListPaires$inter_distance[h] / 5 + (ListPaires$inter_distance[h] / 5 * 0.5) + 5)), 
                           maxmatch = 1)
      # DecMesure12 <- sapply(1:length(TC12$matches), function(i) if (TC12$matches[i] != 0) Ent1$Temps[i] - Sort2$Temps[TC12$matches[i]]) # CORRECTION
      DecMesure12 <- c()
      for (i in 1:length(TC12$matches)) {
        if (TC12$matches[i] != 0) {
          DecMesure12 <- c(DecMesure12,
                           Ent1$Temps[i] - Sort2$Temps[TC12$matches[i]]) }
      }
    }
    
    if (nrow(Ent2) > 0 & nrow(Sort1) > 0) {
      L12 <- unique(c(levels(as.factor(Ent2$espece)), levels(as.factor(Sort1$espece))))
      TC21 <- find.matches(cbind(Ent2$Datenum, match(Ent2$espece, L12), Ent2$Temps), 
                           cbind(Sort1$Datenum, match(Sort1$espece, L12), Sort1$Temps), 
                           tol = c(0, 0, (ListPaires$inter_distance[h] / 5 + (ListPaires$inter_distance[h] / 5 * 0.5) + 5)), 
                           maxmatch = 1)
      # DecMesure21 <- sapply(1:length(TC21$matches), function(i) if (TC21$matches[i] != 0) Sort1$Temps[TC21$matches[i]] - Ent2$Temps[i])
      DecMesure21 <- c()
      for (i in 1:length(TC21$matches)) {
        if (TC21$matches[i] != 0) {
          DecMesure21 <- c(DecMesure21,
                           Ent2$Temps[i] - Sort1$Temps[TC21$matches[i]]) }
      }
    }
    
    # ---- Combining results ---- #
    if (exists("TC12")) {
      Trav12 <- subset(Ent1, TC12$matches != 0)
    } else {
      Trav12 <- data.frame()}
    if (exists("TC21")) {
      Trav21 <- subset(Ent2, TC21$matches != 0)
    } else {
      Trav21 <- data.frame()}
    # Trav12 <- subset(Ent1, TC12$matches != 0) # CORRECTION
    # Trav21 <- subset(Ent2, TC21$matches != 0) # CORRECTION
    
    if (nrow(Trav12) > 0) {
      Trav12 <- cbind(Trav12, DecMesure = DecMesure12, Sens = 12)
    }else{
      Trav12 <- data.frame()
    }
    if (nrow(Trav21) > 0) {
      Trav21 <- cbind(Trav21, DecMesure = DecMesure21, Sens = 21)
    }else{
      Trav21 <- data.frame()
    }
    
    TravTot12 <- rbind(Trav12, Trav21)
    
    # Add information about the pair and the site
    if (nrow(TravTot12) > 0) {
      TravTot12 <- cbind(TravTot12, Paire = ListPaires$num_paire[h], Site = ListPaires$site[h], Passage = ListPaires$passage[h])
      TravFinal <- rbind(TravFinal, TravTot12)
    }
    
    print(paste(h, "/", nrow(ListPaires)))
  }
}

# ---- Save the final results ---- #
# write.csv2(as.data.frame(TravFinal), "TravFinal.csv", row.names = FALSE) # /!\ Modif! column DecMesure is a list

# TravFinal_df <- as.data.frame(TravFinal) # Convert TravFinal to data.frame
# TravFinal_df <- TravFinal_df %>%
#   mutate(across(where(is.list), ~ lapply(., function(x) if (is.null(x)) "" else x))) # Replace NULL elements in list columns with empty strings
fwrite(x = TravFinal_df, file = "TravFinal.csv", row.names = FALSE)
