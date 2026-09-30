# R script to load in and clean CREMP Octo data for use in Biometry assignments
####

# Load packages and read in datasets--------------------------------------------
library(tidyverse)

octo_tally_master <- read.csv("https://raw.githubusercontent.com/caroline-link/Biometry/refs/heads/main/CREMP_OCTO_Data/CREMP_OCTO_Tallies_Masterfile.csv")

octo_conditions_master <- read.csv("https://raw.githubusercontent.com/caroline-link/Biometry/refs/heads/main/CREMP_OCTO_Data/CREMP_OCTO_Condition_Masterfile.csv")



# Clean tally dataset-----------------------------------------------------------

#Subset to observations from 2012-2025 and rename "Subregion" column to "SubRegion"
tally <- octo_tally_master |>
         filter(SampleYear > 2011) |>
         rename(SubRegion = Subregion) 



# Clean conditions dataset------------------------------------------------------

# Subset to observations from 2012-2025 and rename column names to match tally dataset
conditions <- octo_conditions_master |>
              filter(SampleYear > 2011) |> 
              rename(SubRegion = subRegionId, 
                      Habitat = habitatid,
                      SiteCode = Site.Code,
                      SiteID = siteid,
                      SiteName = sitename) 

# Limit to current CREMP target species, update species codes (including combining NS-PAME and PAME)
conditions_targets <- conditions |>
                      filter(SPP_Code %in% c("PAME",
                                             "NS-PAME",
                                             "PFLE",
                                             "GVEN",
                                             "PBIP",
                                             "PPOR")) |> 
                      mutate(SPP_Code = recode(SPP_Code,
                                               "NS-PAME" = "AAME",
                                               "PAME" = "AAME",
                                               "PFLE" = "EFLE",
                                               "PBIP" = "ABIP"))

# Check that counts of NS-PAME and PAME are equal to combined AAME count
#conditions|>
#  filter(SPP_Code %in% c("PAME", "NS-PAME")) |>
#  nrow()

#conditions_targets|>
#  filter(SPP_Code == "AAME") |>
#  nrow()