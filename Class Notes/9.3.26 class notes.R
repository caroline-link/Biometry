# You can load data from a file, or directly from the internet
avonet <- read.csv("AVONET1_BirdLife.csv")
avonet_2 <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/Avonet/AVONET1_BirdLife.csv",header = TRUE)
identical(avonet,avonet_2)

# Base R can't read excel files, but readxl package can
amphibians <- read.csv("oo_32985.xlsx") # this line gets an error

library(readxl)
amphibians <- read_xlsx("Data/oo_32985.xlsx")

# Looking at amphibians dataset
head(amphibians) # excel file had blank/multiple header rows so it gets read in weird

amphibians<- read_xlsx("Data/oo_32985.xlsx", # skipping first 3 rows, replacing NA marker
                       skip = 3,               
                       sheet = 1,
                       na = "DD")

# use class() to figure out data type
class(amphibians$Species)
class(avonet)

# Looking at amniote dataset
amniotes <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/Amniote_traits/Amniote_Database_Aug_2015.csv")

mean(amniotes$litter_or_clutch_size_n)
mean(amniotes$longevity_y) # mean values are weird bc they used -999 for NA

amniotes <- read.csv("https://raw.githubusercontent.com/bmaitner/biometry_course/refs/heads/main/data/Amniote_traits/Amniote_Database_Aug_2015.csv",
                    na.strings = -999)

mean(amniotes$litter_or_clutch_size_n, na.rm = TRUE)

# Accessing data within an object
avonet[1] # gives all values in column 1
avonet[1,1] # gives value in cell 1

avonet[[1]] 
avonet$Species1

avonet["1",] # pulls full 1st row, only works because row is named "1"

class(avonet$Species1)
class(avonet[,2])

# Looking at data structure 
str(avonet) # gives overview of data structure, data type
summary(avonet) # provides summary info that varies by column class
table() # used for catagorical variables, shoes how often combinations occur

