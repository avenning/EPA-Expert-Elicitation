library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(Hmisc)
library(MASS)
library(readr)

Raw_Data <- read_excel("Water_Quality_Data.xlsx")
Water_Quality_Data <- read_csv("Water_Quality_Data.csv")

sum(!is.na(Raw_Data$ResultMeasureValue))

NA_Values <- Raw_Data %>%
  filter(is.na(ResultMeasureValue) == "TRUE")

Num_Values <- Raw_Data %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  filter(!is.na(ResultMeasureValue == "TRUE"))

Raw_Data %>%
  filter(!is.na(ResultMeasureValue == "TRUE"))


Locations <- Num_Values %>%
  dplyr::select(MonitoringLocationName) %>%
  unique()

{
First <- Locations %>%
  mutate(MonitoringLocationName = tolower(MonitoringLocationName)) %>%
  separate(MonitoringLocationName, sep = " ", into = c("Col1", "Col2", "Col3", "Col4", "Col5", 
                                                       "Col6", "Col7", "Col8", "Col9", "Col10", 
                                                       "Col11", "Col12", "Col13", "Col14", "Col15")) %>%
  select(Col1, Col2, Col3, Col4) %>%
  
  mutate(Name = ifelse(Col1 == "ellerbe" | Col2 == "ellerbe" | Col3 == "ellerbe", "Ellerbe", 
                ifelse(Col1 == "eno" | Col2 == "eno" | Col3 == "eno", "Eno", 
                ifelse(Col1 == "neuse" | Col2 == "neuse" | Col3 == "neuse", "Neuse",
                ifelse(Col1 == "mountain" | Col2 == "mountain" | Col3 == "mountain", "Mountain", 
                ifelse(Col1 == "middle" | Col2 == "middle" | Col3 == "middle", "Middle",
                ifelse(Col1 == "black" | Col2 == "black" | Col3 == "black", "Black",
                ifelse(Col1 == "little" | Col2 == "little" | Col3 == "little", "Little",
                ifelse(Col1 == "swift" | Col2 == "swift" | Col3 == "swift", "Swift", 
                ifelse(Col1 == "knap" | Col2 == "knap" | Col3 == "knap", "Knap of Reeds",
                ifelse(Col1 == "hannah" | Col2 == "hannah" | Col3 == "hannah", "Hannah",
                ifelse(Col1 == "crabtree" | Col2 == "crabtree" | Col3 == "crabtree", "Crabtree",
                ifelse(Col1 == "poplar" | Col2 == "poplar" | Col3 == "poplar", "Poplar", 
                ifelse(Col1 == "smith" | Col2 == "smith" | Col3 == "smith", "Smith",
                ifelse(Col1 == "walnut" | Col2 == "walnut" | Col3 == "walnut", "Walnut",
                ifelse(Col1 == "flat" | Col2 == "flat" | Col3 == "flat", "Flat",
                ifelse(Col1 == "falling" | Col2 == "falling" | Col3 == "falling", "Falling",
                ifelse(Col1 == "mill" | Col2 == "mill" | Col3 == "mill", "Mill",
                ifelse(Col1 == "snipes" | Col2 == "snipes" | Col3 == "snipes", "Snipes",
                ifelse(Col1 == "buffalo" | Col2 == "buffalo" | Col3 == "buffalo", "Buffalo",
                       ifelse(Col1 == "buffalo", "Buffalo",
                ifelse(Col1 == "marks" | Col2 == "marks" | Col3 == "marks", "Marks",
                ifelse(Col1 == "pigeon" | Col2 == "pigeon" | Col3 == "pigeon", "Pigeon House",
                ifelse(Col1 == "oake" | Col2 == "oake" | Col3 == "oake", "White Oak",
                ifelse(Col1 == "juniper" | Col2 == "juniper" | Col3 == "juniper", "Juniper",
                ifelse(Col1 == "deep" | Col2 == "deep" | Col3 == "deep", "Deep",
                ifelse(Col1 == "haw" | Col2 == "haw" | Col3 == "haw", "Haw",
                ifelse(Col1 == "williams" | Col2 == "williams" | Col3 == "williams", "Williams",
                ifelse(Col1 == "northeast" | Col2 == "northeast" | Col3 == "northeast", "Northeast",
                ifelse(Col1 == "rocky" | Col2 == "rocky" | Col3 == "rocky", "Rocky",
                ifelse(Col1 == "muddy" | Col2 == "muddy" | Col3 == "muddy", "Muddy",
                ifelse(Col1 == "morgan" | Col2 == "morgan" | Col3 == "morgan", "Morgan",
                ifelse(Col1 == "collins" | Col2 == "collins" | Col3 == "collins", "Collins",
                ifelse(Col1 == "hasketts" | Col2 == "hasketts" | Col3 == "hasketts", "Hasketts",
                       ifelse(Col1 == "haskett" | Col2 == "haskett" | Col3 == "haskett", "Hasketts",
                ifelse(Col1 == "richland" | Col2 == "richland" | Col3 == "richland", "Richland",
                ifelse(Col1 == "bull" | Col2 == "bull" | Col3 == "bull", "Bull",
                ifelse(Col1 == "vestal" | Col2 == "vestal" | Col3 == "vestal", "Vestal",
                ifelse(Col1 == "panther" | Col2 == "panther" | Col3 == "panther", "Panther",
                ifelse(Col1 == "hope" | Col2 == "hope" | Col3 == "hope", "New Hope",
                ifelse(Col1 == "reedy" | Col2 == "reedy" | Col3 == "reedy", "Reedy",
                ifelse(Col1 == "stagg" | Col2 == "stagg" | Col3 == "stagg", "Stagg",
                ifelse(Col1 == "third" | Col2 == "third" | Col3 == "third", "Third",
                ifelse(Col1 == "moadams" | Col2 == "moadams" | Col3 == "moadams", "Moadams",
                ifelse(Col1 == "jordan" | Col2 == "jordan" | Col3 == "jordan", "Jordan",
                ifelse(Col1 == "robeson" | Col2 == "robeson" | Col3 == "robeson", "Robeson",
                ifelse(Col1 == "mclendons" | Col2 == "mclendons" | Col3 == "mclendons", "McLendons",
                ifelse(Col1 == "cotton" | Col2 == "cotton" | Col3 == "cotton", "Cotton",
                ifelse(Col1 == "tantraugh" | Col2 == "tantraugh" | Col3 == "tantraugh", "Tantraugh",
                ifelse(Col1 == "loves" | Col2 == "loves" | Col3 == "loves", "Loves",
                       NA))))))))))))))))))))))))))))))))))))))))))))))))))

First <- First %>%
  mutate(Name = ifelse(Col1 == "bear" | Col2 == "bear" | Col3 == "bear", "Bear", 
                ifelse(Col1 == "wilson" | Col2 == "wilson" | Col3 == "wilson", "Wilson",
                ifelse(Col1 == "pokeberry" | Col2 == "pokeberry" | Col3 == "pokeberry", "Pokeberry",
                ifelse(Col1 == "rock" | Col2 == "rock" | Col3 == "rock", "Rock",
                ifelse(Col1 == "brooks" | Col2 == "brooks" | Col3 == "brooks", "Brooks",
                ifelse(Col1 == "dry" | Col2 == "dry" | Col3 == "dry", "Dry",
                ifelse(Col1 == "bolin" | Col2 == "bolin" | Col3 == "bolin", "Bolin",
                ifelse(Col1 == "service" | Col2 == "service" | Col3 == "service", "Service", 
                ifelse(Col1 == "town" | Col2 == "town" | Col3 == "town", "Town Branch",
                ifelse(Col1 == "hunting", "Hunting",
                ifelse(Col1 == "varnals" | Col2 == "varnals" | Col3 == "varnals", "Varnals",
                ifelse(Col1 == "booker" | Col2 == "booker" | Col3 == "booker", "Booker",
                ifelse(Col1 == "long" | Col2 == "long" | Col3 == "long", "Long Branch",
                ifelse(Col1 == "oak" | Col2 == "oak" | Col3 == "oak", "White Oak",
                ifelse(Col1 == "cane" | Col2 == "cane" | Col3 == "cane", "Cane",
                ifelse(Col1 == "sandy" | Col2 == "sandy" | Col3 == "sandy", "Sandy",
                ifelse(Col1 == "governors" | Col2 == "governors" | Col3 == "governors", "Big Governors",
                ifelse(Col1 == "big" | Col2 == "big" | Col3 == "big", "Great Alamance",
                ifelse(Col1 == "troublesome" | Col2 == "troublesome" | Col3 == "troublesome", "Little Troublesome",
                ifelse(Col1 == "lick" | Col2 == "lick" | Col3 == "lick", "Little Lick",
                       Name)))))))))))))))))))))
}               
                
                
write_csv(First, "creeknames.csv")
CreekNames <- read_csv("creeknames.csv")  

CreekNames <- CreekNames %>%
  mutate(UT = ifelse(Col1 == "ut" | Col2 == "ut" | Col3 == "ut", TRUE, 
                     ifelse(Col1 == "unnamed" | Col2 == "unnamed" | Col3 == "unnamed", "Sandy",
                            FALSE)))
CreekNames$UT[is.na(CreekNames$UT)] <- FALSE
head(Locations)

tibble(Locations, CreekNames$Name, CreekNames$UT) %>%
  head()

Locations <- Locations %>%
  mutate(CreekName = CreekNames$Name, UT = CreekNames$UT)

Locations <- Locations %>%
  mutate(CreekName = ifelse(MonitoringLocationName == "LITTLE ALAMANCE CR AT SR 2309 NR GRAHAM, NC", "Little Alamance", CreekName),
         CreekName = ifelse(MonitoringLocationName == "Troublesome Crk at US 29 Bus nr Reidsville", "Troublesome", CreekName),
         CreekName = ifelse(MonitoringLocationName == "Troublesome Crk at US 29 Bus near Reidsville", "Troublesome", CreekName),
         CreekName = ifelse(MonitoringLocationName == "Buffalo Creek", "Buffalo", CreekName),
         CreekName = ifelse(MonitoringLocationName == "PERSIMMON CRK AT SR 1237 AT SANFORD", "Persimmon", CreekName),
         CreekName = ifelse(MonitoringLocationName == "BUFFALO CREEK", "Buffalo", CreekName),
         CreekName = ifelse(MonitoringLocationName == "Randleman Lake at SR 1921 near Randleman", "Randleman", CreekName),
         CreekName = ifelse(MonitoringLocationName == "PERSIMMON CRK AT SR 1237 AT SANFORD", "Persimmon", CreekName),
         CreekName = ifelse(MonitoringLocationName == "UNNAMED TRIB 1 TO COLLINS CRK NR WHITE CROSS, NC", "Collins", CreekName),
         CreekName = ifelse(MonitoringLocationName == "UNNAMED TRIB 2 TO COLLINS CREEK NR WHITE CROSS, NC", "Collins", CreekName),
         CreekName = ifelse(MonitoringLocationName == "ALAMANCE CRK AT SR 2116 AT SWEPSONSVILLE", "Great Alamance", CreekName),
         CreekName = ifelse(MonitoringLocationName == "BRANCH CREEK AT NC 54 NR GRAHAM, NC", "Branch", CreekName),
         CreekName = ifelse(MonitoringLocationName == "Hunting Creek", "Hunting", CreekName),
         UT = ifelse(MonitoringLocationName == "UNNAMED TRIB 1 TO COLLINS CRK NR WHITE CROSS, NC", TRUE, UT),
         UT = ifelse(MonitoringLocationName == "UNNAMED TRIB 2 TO COLLINS CREEK NR WHITE CROSS, NC", TRUE, UT))

Locations$CreekName[Locations$CreekName == "Great Alamance"] <- "Big Alamance"
Locations$CreekName[Locations$CreekName == "Third"] <- "Third Fork"

############## Finalized Creek Location Names

Num_Values %>%
  left_join(Locations, by = "MonitoringLocationName") %>%
  dplyr::select(CreekName, Month, Day, Year, 
         CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode,
         LatitudeMeasure, LongitudeMeasure, watershed, UT) %>%
  group_by(CreekName, CharacteristicName) %>%
  summarise(N = n()) %>%
  spread(key = CharacteristicName, value = N)
  colnames()

Eco_Data <- Num_Values %>%
  left_join(Locations, by = "MonitoringLocationName") %>%
  dplyr::select(CreekName, Month, Day, Year, 
         CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode,
         LatitudeMeasure, LongitudeMeasure, watershed, UT) %>%
  filter(grepl(tolower(CharacteristicName), pattern = "kjeldahl")|
         grepl(tolower(CharacteristicName), pattern = "inorganic nitrogen")|
         grepl(tolower(CharacteristicName), pattern = "coliform")|
         grepl(tolower(CharacteristicName), pattern = "turbidity")|
         grepl(tolower(CharacteristicName), pattern = "phosphorus")|
         grepl(tolower(CharacteristicName), pattern = "nitrogen")|
         grepl(tolower(CharacteristicName), pattern = "conductivity")|
         grepl(tolower(CharacteristicName), pattern = "specific conductance"))


unique(Eco_Data$CharacteristicName)


{
Eco_Data %>%
  filter(ResultMeasure.MeasureUnitCode == "mS/cm") %>%
  dplyr::select(ResultMeasureValue, ResultMeasure.MeasureUnitCode)

Eco_Data$ResultMeasure.MeasureUnitCode[Eco_Data$ResultMeasure.MeasureUnitCode == "mS/cm" & !is.na(Eco_Data$ResultMeasure.MeasureUnitCode)] <- "uS/cm"

Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Conductivity"] <- "Specific conductance"
Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Specific conductance"] <- "Specific Conductance"
Eco_Data$ResultMeasure.MeasureUnitCode[Eco_Data$ResultMeasure.MeasureUnitCode == "uS/cm @25C" & !is.na(Eco_Data$ResultMeasure.MeasureUnitCode)] <- "uS/cm"
unique(Eco_Data$CharacteristicName)

Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)"] <- "Inorganic Nitrogen"
Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Inorganic nitrogen (nitrate and nitrite) as N"] <- "Inorganic Nitrogen"     # Flag here if these two inorg. nitrogen measures are not the same
Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Phosphate-phosphorus as P"] <- "Phosphorus"
Eco_Data$CharacteristicName[Eco_Data$CharacteristicName == "Kjeldahl nitrogen"] <- "Kjeldahl Nitrogen"
}

unique(Eco_Data$CharacteristicName)

Eco_Data %>%
  filter(CharacteristicName %in% c("Phosphorus", "Phosphate-phosphorus as P")) %>%
  group_by(CharacteristicName) %>%
  summarise(N = n(), Avg = mean(ResultMeasureValue), SD = sd(ResultMeasureValue))


Eco_Data %>%
  filter(CharacteristicName %in% c("Ammonia-nitrogen", "Ammonia-nitrogen as N", 
                                   "Inorganic nitrogen (nitrate and nitrite)", 
                                   "Inorganic nitrogen (nitrate and nitrite) as N",
                                   "Kjeldahl nitrogen", "Nitrogen", 
                                   "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)",
                                   "Organic Nitrogen")) %>%
  group_by(CharacteristicName) %>%
  summarise(N = n())

Eco_Data %>%
  filter(CreekName == "Bear",
         CharacteristicName == "Inorganic Nitrogen",
         !is.na(ResultMeasureValue)) %>%
  summarise(mean(ResultMeasureValue))

write_csv(Eco_Data, "Eco_Data.csv")
Eco_Data <- read_csv("Eco_Data.csv")

WQ_Data <- Eco_Data %>%
  filter(CharacteristicName == "Turbidity"|
         CharacteristicName == "Fecal Coliform"|
         CharacteristicName == "Phosphorus"|
         CharacteristicName == "Kjeldahl Nitrogen"|
         CharacteristicName == "Inorganic Nitrogen"|
         CharacteristicName == "Specific Conductance") %>%
  rename("Creek" = CreekName,
         "Measurement" = CharacteristicName,
         "Value" = ResultMeasureValue,
         "Unit" = ResultMeasure.MeasureUnitCode,
         "Latitude" = LatitudeMeasure,
         "Longitude" = LongitudeMeasure,
         "Watershed" = watershed)

write_csv(WQ_Data, "WQ_Data.csv")
WQ_Data <- read_csv("WQ_Data.csv")

####################################   Adding NA values below analytical limit    #####################################
{
NA_Vals <- NA_Values %>%
  filter(CharacteristicName == "Turbidity"|
         CharacteristicName == "Fecal Coliform"|
         CharacteristicName == "Phosphorus"|
         CharacteristicName == "Kjeldahl Nitrogen") %>%
  filter(!is.na(ResultCommentText))

NA_CreekNames <- NA_Vals %>%
  select(MonitoringLocationName)

First_NA <- NA_CreekNames %>%
  mutate(MonitoringLocationName = tolower(MonitoringLocationName)) %>%
  separate(MonitoringLocationName, sep = " ", into = c("Col1", "Col2", "Col3", "Col4", "Col5", 
                                                       "Col6", "Col7", "Col8", "Col9", "Col10", 
                                                       "Col11", "Col12", "Col13", "Col14", "Col15")) %>%
  select(Col1, Col2, Col3, Col4) %>%
  
  mutate(Name = ifelse(Col1 == "ellerbe" | Col2 == "ellerbe" | Col3 == "ellerbe", "Ellerbe", 
                       ifelse(Col1 == "eno" | Col2 == "eno" | Col3 == "eno", "Eno", 
                              ifelse(Col1 == "neuse" | Col2 == "neuse" | Col3 == "neuse", "Neuse",
                                     ifelse(Col1 == "mountain" | Col2 == "mountain" | Col3 == "mountain", "Mountain", 
                                            ifelse(Col1 == "middle" | Col2 == "middle" | Col3 == "middle", "Middle",
                                                   ifelse(Col1 == "black" | Col2 == "black" | Col3 == "black", "Black",
                                                          ifelse(Col1 == "little" | Col2 == "little" | Col3 == "little", "Little",
                                                                 ifelse(Col1 == "swift" | Col2 == "swift" | Col3 == "swift", "Swift", 
                                                                        ifelse(Col1 == "knap" | Col2 == "knap" | Col3 == "knap", "Knap of Reeds",
                                                                               ifelse(Col1 == "hannah" | Col2 == "hannah" | Col3 == "hannah", "Hannah",
                                                                                      ifelse(Col1 == "crabtree" | Col2 == "crabtree" | Col3 == "crabtree", "Crabtree",
                                                                                             ifelse(Col1 == "poplar" | Col2 == "poplar" | Col3 == "poplar", "Poplar", 
                                                                                                    ifelse(Col1 == "smith" | Col2 == "smith" | Col3 == "smith", "Smith",
                                                                                                           ifelse(Col1 == "walnut" | Col2 == "walnut" | Col3 == "walnut", "Walnut",
                                                                                                                  ifelse(Col1 == "flat" | Col2 == "flat" | Col3 == "flat", "Flat",
                                                                                                                         ifelse(Col1 == "falling" | Col2 == "falling" | Col3 == "falling", "Falling",
                                                                                                                                ifelse(Col1 == "mill" | Col2 == "mill" | Col3 == "mill", "Mill",
                                                                                                                                       ifelse(Col1 == "snipes" | Col2 == "snipes" | Col3 == "snipes", "Snipes",
                                                                                                                                              ifelse(Col1 == "buffalo" | Col2 == "buffalo" | Col3 == "buffalo", "Buffalo",
                                                                                                                                                     ifelse(Col1 == "buffalo", "Buffalo",
                                                                                                                                                            ifelse(Col1 == "marks" | Col2 == "marks" | Col3 == "marks", "Marks",
                                                                                                                                                                   ifelse(Col1 == "pigeon" | Col2 == "pigeon" | Col3 == "pigeon", "Pigeon House",
                                                                                                                                                                          ifelse(Col1 == "oake" | Col2 == "oake" | Col3 == "oake", "White Oak",
                                                                                                                                                                                 ifelse(Col1 == "juniper" | Col2 == "juniper" | Col3 == "juniper", "Juniper",
                                                                                                                                                                                        ifelse(Col1 == "deep" | Col2 == "deep" | Col3 == "deep", "Deep",
                                                                                                                                                                                               ifelse(Col1 == "haw" | Col2 == "haw" | Col3 == "haw", "Haw",
                                                                                                                                                                                                      ifelse(Col1 == "williams" | Col2 == "williams" | Col3 == "williams", "Williams",
                                                                                                                                                                                                             ifelse(Col1 == "northeast" | Col2 == "northeast" | Col3 == "northeast", "Northeast",
                                                                                                                                                                                                                    ifelse(Col1 == "rocky" | Col2 == "rocky" | Col3 == "rocky", "Rocky",
                                                                                                                                                                                                                           ifelse(Col1 == "muddy" | Col2 == "muddy" | Col3 == "muddy", "Muddy",
                                                                                                                                                                                                                                  ifelse(Col1 == "morgan" | Col2 == "morgan" | Col3 == "morgan", "Morgan",
                                                                                                                                                                                                                                         ifelse(Col1 == "collins" | Col2 == "collins" | Col3 == "collins", "Collins",
                                                                                                                                                                                                                                                ifelse(Col1 == "hasketts" | Col2 == "hasketts" | Col3 == "hasketts", "Hasketts",
                                                                                                                                                                                                                                                       ifelse(Col1 == "haskett" | Col2 == "haskett" | Col3 == "haskett", "Hasketts",
                                                                                                                                                                                                                                                              ifelse(Col1 == "richland" | Col2 == "richland" | Col3 == "richland", "Richland",
                                                                                                                                                                                                                                                                     ifelse(Col1 == "bull" | Col2 == "bull" | Col3 == "bull", "Bull",
                                                                                                                                                                                                                                                                            ifelse(Col1 == "vestal" | Col2 == "vestal" | Col3 == "vestal", "Vestal",
                                                                                                                                                                                                                                                                                   ifelse(Col1 == "panther" | Col2 == "panther" | Col3 == "panther", "Panther",
                                                                                                                                                                                                                                                                                          ifelse(Col1 == "hope" | Col2 == "hope" | Col3 == "hope", "New Hope",
                                                                                                                                                                                                                                                                                                 ifelse(Col1 == "reedy" | Col2 == "reedy" | Col3 == "reedy", "Reedy",
                                                                                                                                                                                                                                                                                                        ifelse(Col1 == "stagg" | Col2 == "stagg" | Col3 == "stagg", "Stagg",
                                                                                                                                                                                                                                                                                                               ifelse(Col1 == "third" | Col2 == "third" | Col3 == "third", "Third",
                                                                                                                                                                                                                                                                                                                      ifelse(Col1 == "moadams" | Col2 == "moadams" | Col3 == "moadams", "Moadams",
   ifelse(Col1 == "jordan" | Col2 == "jordan" | Col3 == "jordan", "Jordan",
                                                                                                                                                                                                                                                                                                                                    ifelse(Col1 == "robeson" | Col2 == "robeson" | Col3 == "robeson", "Robeson",
                                                                                                                                                                                                                                                                                                                                           ifelse(Col1 == "mclendons" | Col2 == "mclendons" | Col3 == "mclendons", "McLendons",
                                                                                                                                                                                                                                                                                                                                                  ifelse(Col1 == "cotton" | Col2 == "cotton" | Col3 == "cotton", "Cotton",
                                                                                                                                                                                                                                                                                                                                                         ifelse(Col1 == "tantraugh" | Col2 == "tantraugh" | Col3 == "tantraugh", "Tantraugh",
                                                                                                                                                                                                                                                                                                                                                                ifelse(Col1 == "loves" | Col2 == "loves" | Col3 == "loves", "Loves",
                                                                                                                                                                                                                                                                                                                                                                       NA))))))))))))))))))))))))))))))))))))))))))))))))))

First_NA <- First_NA %>%
  mutate(Name = ifelse(Col1 == "bear" | Col2 == "bear" | Col3 == "bear", "Bear", 
                       ifelse(Col1 == "wilson" | Col2 == "wilson" | Col3 == "wilson", "Wilson",
                              ifelse(Col1 == "pokeberry" | Col2 == "pokeberry" | Col3 == "pokeberry", "Pokeberry",
                                     ifelse(Col1 == "rock" | Col2 == "rock" | Col3 == "rock", "Rock",
                                            ifelse(Col1 == "brooks" | Col2 == "brooks" | Col3 == "brooks", "Brooks",
                                                   ifelse(Col1 == "dry" | Col2 == "dry" | Col3 == "dry", "Dry",
                                                          ifelse(Col1 == "bolin" | Col2 == "bolin" | Col3 == "bolin", "Bolin",
                                                                 ifelse(Col1 == "service" | Col2 == "service" | Col3 == "service", "Service", 
                                                                        ifelse(Col1 == "town" | Col2 == "town" | Col3 == "town", "Town Branch",
                                                                               ifelse(Col1 == "hunting", "Hunting",
                                                                                      ifelse(Col1 == "varnals" | Col2 == "varnals" | Col3 == "varnals", "Varnals",
                                                                                             ifelse(Col1 == "booker" | Col2 == "booker" | Col3 == "booker", "Booker",
                                                                                                    ifelse(Col1 == "long" | Col2 == "long" | Col3 == "long", "Long Branch",
                                                                                                           ifelse(Col1 == "oak" | Col2 == "oak" | Col3 == "oak", "White Oak",
                                                                                                                  ifelse(Col1 == "cane" | Col2 == "cane" | Col3 == "cane", "Cane",
                                                                                                                         ifelse(Col1 == "sandy" | Col2 == "sandy" | Col3 == "sandy", "Sandy",
                                                                                                                                
                                                                                                                                ifelse(Col1 == "governors" | Col2 == "governors" | Col3 == "governors", "Big Governors",
                                                                                                                                       #ifelse(Col1 == "alamance" | Col2 == "alamance" | Col3 == "alamance", "Alamance",
                                                                                                                                       ifelse(Col1 == "big" | Col2 == "big" | Col3 == "big", "Great Alamance",
                                                                                                                                              Name)))))))))))))))))))#)
  
First_NA <- First_NA %>%
  mutate(UT = ifelse(Col1 == "ut" | Col2 == "ut" | Col3 == "ut", TRUE, FALSE))

First_NA <- First_NA %>%
  mutate(Name = ifelse(Col1 == "hunting", "Hunting", Name),
         UT = ifelse(Col1 == "hunting", FALSE, UT))

First_NA <- First_NA %>%
  mutate(Name = ifelse(Col1 == "persimmon", "Persimmon", Name),
         UT = ifelse(Col1 == "persimmon", FALSE, UT))

First_NA <- First_NA %>%
  mutate(Name = ifelse(Col1 == "third", "Third Fork", Name))

First_NA <- First_NA %>%
  mutate(Name = ifelse(Col2 == "troublesome", "Little Troublesome", Name))




WQ_NA <- cbind(NA_Vals, First_NA) %>%
  rename("Creek" = Name) %>%
  select(Creek, Month, Day, Year, 
         CharacteristicName, ResultCommentText, ResultMeasure.MeasureUnitCode,
         LatitudeMeasure, LongitudeMeasure, watershed, UT) %>%
  filter(grepl("U", ResultCommentText),
         !grepl("Old STORET", ResultCommentText)) %>%
  mutate(Value = 0,
         ResultCommentText = "Analyte not detected above practical quantitation limit.") %>%
  rename("Measurement" = CharacteristicName,
         "Unit" = ResultMeasure.MeasureUnitCode,
         "Comment" = ResultCommentText,
         "Latitude" = LatitudeMeasure, 
         "Longitude" = LongitudeMeasure,
         "Watershed" = watershed) %>%
  select(Creek, Month, Day, Year, Measurement, Value, Unit, Latitude, Longitude, Watershed, UT, Comment)

Comments <- WQ_NA %>%
  select(ResultCommentText) %>%
  unique()

WQ_Complete <- rbind(mutate(WQ_Data,
                     Comment = "NA"), 
                  WQ_NA)
}


#######################################################################################################################

write_csv(WQ_Complete, "WQ_Complete.csv")
WQ_Complete <- read_csv("WQ_Complete.csv", col_types = cols(Creek       = col_character(),
                                                            Month       = col_integer(),
                                                            Day         = col_integer(),
                                                            Year        = col_integer(),
                                                            Measurement = col_character(),
                                                            Value       = col_double(),
                                                            Unit        = col_character(),
                                                            Latitude    = col_double(),
                                                            Longitude   = col_double(),
                                                            Watershed   = col_character(),
                                                            UT          = col_logical(),
                                                            Comment     = col_character()))

WQ_Complete <- WQ_Data            ##### Skip to here if no NA data
WQ_Complete <- WQ_Complete %>%
  filter(Month %in% c(3:11)) 

WQ_Complete$UT[is.na(WQ_Complete$UT)] <- FALSE



#########################################################################################################################
{
WQ_Complete <- WQ_Complete %>%
  filter(Unit != "NTRU")

WQ_Complete %>%
  group_by(Measurement, Unit) %>%
  summarise(N = n(),
            Avg = mean(Value)) %>%
  head(n = 20)

WQ_Complete$Value[WQ_Complete$Unit == "ug/l"] <- 0.065
WQ_Complete$Unit[WQ_Complete$Unit == "ug/l"] <- "mg/L"
WQ_Complete$Unit[WQ_Complete$Unit == "mg/l"] <- "mg/L"
WQ_Complete$Unit[WQ_Complete$Unit == "mg/l as P"] <- "mg/L"
WQ_Complete$Unit[WQ_Complete$Unit == "mg/l as N"] <- "mg/L"
WQ_Complete$Unit[WQ_Complete$Unit == "#/100ml"] <- "cfu/100mL"
WQ_Complete$Unit[WQ_Complete$Unit == "cfu/100ml"] <- "cfu/100mL"
WQ_Complete$Unit[WQ_Complete$Unit == "umho/cm"] <- "uS/cm"
WQ_Complete$Unit[WQ_Complete$Unit == "FTU"] <- "NTU"


} # Checking and correcting issues with units
########################################################################################################################

{Avg %>%
  colnames()

colnames(Avg) <- c("Creek", "Biotic Index (BI)",
                   "Fecal Coliform (cfu/100mL)", "Inorganic Nitrogen (mg/L)", 
                   "Kjeldahl Nitrogen (mg/L)", "Phosphorus (mg/L)", 
                   "Specific Conductance (uS/cm)", "Turbidity (NTU)")



CorMat <- cor(Avg[, 2:8], use = "pairwise.complete.obs")

Corr_Table <- rcorr(as.matrix(Avg[,2:8]))

Corr_Coeff <- Corr_Table$r
Corr_PVal <- Corr_Table$P
Corr_N <- Corr_Table$n


CovMat <- cov(Avg[,2:8], use = "pairwise.complete.obs")



Summary <- Water_Quality %>%
  filter(Creek %in% unique(WQ_Complete$Creek)) %>%
  group_by(Measurement) %>%
  summarise(Avg = mean(Value),
            StDev = sd(Value))

Means <- Summary$Avg
StDev <- Summary$StDev



mvrnorm(n = 2, mu = Means, Sigma = CovMat, empirical = FALSE)
}
# Successfully generates sample data - but includes negative values because it assumes a normal distribution.
# Next try to use a skewed distribution to generate sample data.

###########################################################################################################################
# Introducing biotic index data into WQ_Complete dataset
{sort(unique(WQ_Complete$Creek))


Macrodata_Final2 <- Macrodata_Final %>%
  mutate(Measurement = "Biotic Index (BI)",
         Date = lubridate::mdy(paste(Month, "/1/", Year)),
         Watershed = NA,
         Unit = NA) %>%
  rename("Value" = BI,
         "Lat" = Latitude,
         "Long" = Longitude) %>%
  dplyr::select(Creek, Date, Watershed, Lat, Long, Measurement, Value)

colnames(WQ_Cluster)
colnames(Macrodata_Final2)

Water_Quality <- WQ_Complete %>%
  rbind(Macrodata_Final2)
}
###############################################################################
# Expanding total nitrogen data to include multiple measurements on the same day in different locations


Nitro_Sample_Sites <- Water_Quality %>%
  rename("Lat" = Latitude,
         "Long" = Longitude) %>%
  filter(Measurement %in% c("Kjeldahl Nitrogen", "Inorganic Nitrogen")) %>%
  mutate(Date = mdy(paste(Month, Day, Year, sep = "/"))) %>%
  unite(col = "LatLong", Lat, Long, sep = ", ") %>%
  group_by(Date, Creek, Measurement, LatLong) %>%
  summarise(N = n()) %>%
  spread(key = Measurement, value = N) %>%
  filter(`Inorganic Nitrogen` == 1 & `Kjeldahl Nitrogen` == 1) %>%
  dplyr::select(Date, Creek, LatLong) %>%                                              # Determine each unique instance where inorganic and kjeldahl nitrogen were measured
  tidyr::unite(col = "Creek_Date_LatLong", Creek, Date, LatLong, sep = "_")            # in the same river, at the same location, and on the same day.


Total_Nitrogen <- Water_Quality %>%
  rename("Lat" = Latitude,
         "Long" = Longitude) %>%
  filter(Measurement %in% c("Kjeldahl Nitrogen", "Inorganic Nitrogen")) %>%
  mutate(CreekName = Creek,
         Date = mdy(paste(Month, Day, Year, sep = "/")),
         Date2 = Date, Lat2 = Lat, Long2 = Long) %>%
  unite(col = "LatLong", Lat2, Long2, sep = ", ") %>%
  unite(col = "Creek_Date_LatLong", CreekName, Date2, LatLong, sep = "_") %>%
  filter(Creek_Date_LatLong %in% Nitro_Sample_Sites$Creek_Date_LatLong) %>%
  arrange(Date, Creek) %>%
  spread(key = Measurement, value = Value) %>%
  mutate(Value = `Inorganic Nitrogen` + `Kjeldahl Nitrogen`, Measurement = "Total Nitrogen") #%>%            # Filters general dataset to include only sets of kjeldahl and inorganic nitrogen
  #dplyr::select(-Creek_Date_LatLong, -`Inorganic Nitrogen`, -`Kjeldahl Nitrogen`, -Date)                    # taken in the same place at the same time.

Water_Quality_wTN <- Water_Quality %>%
  filter(!Measurement %in% c("Kjeldahl Nitrogen", "Inorganic Nitrogen")) %>%
  rbind(Total_Nitrogen)

#################################################################################
################### Beginning of Data Generation Algorithm ######################
#################################################################################

Col_Means <- as.numeric(colMeans(WQ_Final[,2:7], na.rm = TRUE))
Data_SDs <- WQ_Final %>%
  summarise(BI = sd(BI, na.rm = TRUE), FC = sd(FC), SC = sd(SC), 
            TN = sd(TN), TP = sd(TP), TU = sd(TU)) %>%
  as.numeric()

# Normalize lognormal data before generating sample data - then return to lognormal format

LogNorm <- WQ_Final[,2:7]
Norm <- log(LogNorm)

NormCov <- cov(Norm, use = "pairwise.complete.obs")
NormMeans <- log(Col_Means)

NormSample <- mvrnorm(n = 1000, mu = NormMeans, Sigma = NormCov)
LogNormSample <- exp(NormSample)
Sample_Data <- round(LogNormSample, 3)

Sample_Data <- tibble(
BI = as.numeric(t(Sample_Data[,1])),
FC = as.numeric(t(Sample_Data[,2])),
SC = as.numeric(t(Sample_Data[,3])),
TN = as.numeric(t(Sample_Data[,4])),
TP = as.numeric(t(Sample_Data[,5])),
TU = as.numeric(t(Sample_Data[,6])))

colnames(Sample_Data) <- c("BI", "FC", "SC", 
                           "TN", "TP", "TU")

Sample_Means <- colMeans(Sample_Data)
Sample_SDs <- Sample_Data %>%
  summarise(BI = sd(BI), FC = sd(FC), SC = sd(SC), 
            TN = sd(TN), TP = sd(TP), TU = sd(TU)) %>%
  as.numeric()

ZVal = tibble(Row = c(1:1000),
              BI = (Sample_Data$BI - Sample_Means[1]) / Sample_SDs[1],
              FC = (Sample_Data$FC - Sample_Means[2]) / Sample_SDs[2],
              SC = (Sample_Data$SC - Sample_Means[3]) / Sample_SDs[3],
              TN = (Sample_Data$TN - Sample_Means[4]) / Sample_SDs[4],
              TP = (Sample_Data$TP - Sample_Means[5]) / Sample_SDs[5],
              TU = (Sample_Data$TU - Sample_Means[6]) / Sample_SDs[6])


############

Top50 <- rbind(
ZVal %>%
  dplyr::select(Row, BI) %>%
  mutate(Abs = abs(BI),
         Meas = "BI") %>%
  rename("Z" = BI) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50),

ZVal %>%
  dplyr::select(Row, FC) %>%
  mutate(Abs = abs(FC),
         Meas = "FC") %>%
  rename("Z" = FC) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50),

ZVal %>%
  dplyr::select(Row, SC) %>%
  mutate(Abs = abs(SC),
         Meas = "SC") %>%
  rename("Z" = SC) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50),

ZVal %>%
  dplyr::select(Row, TN) %>%
  mutate(Abs = abs(TN),
         Meas = "TN") %>%
  rename("Z" = TN) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50),

ZVal %>%
  dplyr::select(Row, TP) %>%
  mutate(Abs = abs(TP),
         Meas = "TP") %>%
  rename("Z" = TP) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50),

ZVal %>%
  dplyr::select(Row, TU) %>%
  mutate(Abs = abs(TU),
         Meas = "TU") %>%
  rename("Z" = TU) %>%
  arrange(desc(Abs)) %>%
  dplyr::select(-Abs) %>%
  head(n = 50)
) %>%
  
  arrange(Row)
  
UniqueRows <- Top50 %>%
  group_by(Row) %>%
  summarise(N = n()) %>%
  filter(N == 1)

RowSelect <- Top50 %>%
  filter(Row %in% UniqueRows$Row) %>%
  arrange(Meas) %>%
  group_by(Meas) %>%
  sample_n(12)






Pct80 <- as.numeric(summarise(Sample_Data,                # Determine the 80th percentile for each variable
            BI = quantile(abs(BI), 0.80),
            FC = quantile(abs(FC), 0.80),
            SC = quantile(abs(SC), 0.80),
            TN = quantile(abs(TN), 0.80),
            TP = quantile(abs(TP), 0.80),
            TU = quantile(abs(TU), 0.80)))

#Pct80 <- Sample_Data %>%
#  gather(value = Value, key = Measurement) %>%
#  group_by(Measurement) %>%
#  summarise(Pct80 = quantile(Value, 0.8)) %>%
#  dplyr::select(Pct80) %>%
#  t() %>%
#  as.numeric()

#######################################################
Sample100 <- rbind(
  Sample_Data %>%                                       # Filter sample data to include selected rows from
    mutate(Row = 1:1000) %>%                           # top five percent of Z-values for each variable
    filter(Row %in% RowSelect$Row) %>%
    left_join(RowSelect, by = "Row") %>%
    dplyr::select(BI, FC, SC, TN, TP, TU, Meas, Z),
  
  Sample_Data %>%                                       # Filter the sample data to exclude the top
    filter(BI < Pct80[1],                              # 20 percent for each variable.
           FC < Pct80[2],
           SC < Pct80[3],
           TN < Pct80[4],
           TP < Pct80[5],
           TU < Pct80[6]) %>%
    sample_n(40) %>%
    mutate(Meas = NA, Z = NA)
) %>%
  arrange(Meas, desc(Z))

colnames(Sample100) <- c("Biotic Index", "Fecal Coliform (cfu/100mL)", "Specific Conductance (uS/cm)",
                         "Total Nitrogen (mg/L)", "Total Phosphorus (mg/L)", "Turbidity (NTU)", 
                         "Key Variable", "Z-Value of Key Variable")

write_csv(Sample100, "Sample100.csv")

##################################################################
############## End of Data Generation Algorithm ##################
##################################################################


# Confirmation of summary statistics for nitrogen variables
{
Total_Nitrogen %>% 
  summarise(AvgKN = mean(`Kjeldahl Nitrogen`),
            StDevKN = sd(`Kjeldahl Nitrogen`),
            AvgIN = mean(`Inorganic Nitrogen`),
            StDevIN = sd(`Inorganic Nitrogen`),
            AvgTN = mean(Value),
            StDevTN = sd(Value))

Sample100 %>%
  summarise(AvgTN = mean(TN),
            StDevTN = sd(TN))

library(tidyverse)

TN_Values <- Water_Quality_wTN %>%
  filter(Measurement == "Total Nitrogen") %>%
  group_by(Creek) %>%
  summarize(Mean = mean(Value),
            SD = sd(Value))

WQ_Complete %>%
  filter(Measurement %in% c("Kjeldahl Nitrogen", "Inorganic Nitrogen")) %>%
  group_by(Creek, Measurement) %>%
  summarize(Mean = mean(Value),
            SD = sd(Value)) %>%
  spread(key = )


Nitrogen_Data <- WQ_Complete %>%
  filter(Measurement %in% c("Kjeldahl Nitrogen", "Inorganic Nitrogen")) %>%
  dplyr::select(Creek, Measurement, Value) %>%
  dplyr::group_by(Creek, Measurement) %>%
  dplyr::summarise(Mean = mean(Value),
                   SD = sd (Value),
                   N = n()) %>%
  unite(col = "Mean_SD_N", Mean, SD, N, sep = ", ") %>%
  spread(key = Measurement, value = 'Mean_SD_N') %>%
  separate(`Inorganic Nitrogen`, into = c('TIN_Mean', 'TIN_SD', 'TIN_N'), sep = ", ") %>%
  separate(`Kjeldahl Nitrogen`, into = c('TKN_Mean', 'TKN_SD', 'TKN_N'), sep = ", ") %>%
  mutate(TIN_Mean = round(as.numeric(TIN_Mean), digits = 3),
         TIN_SD = round(as.numeric(TIN_SD), digits = 3),
         TKN_Mean = round(as.numeric(TKN_Mean), digits = 3),
         TKN_SD = round(as.numeric(TKN_SD), digits = 3))

write_csv(Nitrogen_Data, "TIN and TKN Data.csv")

Nitrogen_Data %>%
  ungroup() %>%
  summarise(Avg = mean(TIN_Mean),
            Min = min(TIN_Mean),
            Q10 = quantile(TIN_Mean, 0.1),
            Q25 = quantile(TIN_Mean, 0.25),
            Q50 = quantile(TIN_Mean, 0.5),
            Q60 = quantile(TIN_Mean, 0.6),
            Q75 = quantile(TIN_Mean, 0.75),
            Q90 = quantile(TIN_Mean, 0.9),
            Max = max(TIN_Mean))
}
#########
#Isolating indivicual measurement events with all 6 measurements for cluster analysis

WQ_wCDLL <- Water_Quality %>%
  mutate(CreekName = Creek,
         Date = mdy(paste(Month, Day, Year, sep = "/")),
         Date2 = Date, Lat2 = Lat, Long2 = Long) %>%
  unite(col = "LatLong", Lat2, Long2, sep = ", ") %>%
  unite(col = "Creek_Date_LatLong", CreekName, Date2, LatLong, sep = "_")

CDLL <- WQ_wCDLL %>%
  group_by(Creek_Date_LatLong, Measurement) %>%
  summarise(N = n()) %>%
  filter(Measurement != "Biotic Index (BI)") %>%
  spread(value = N, key = Measurement) %>%
  rename("FC" = `Fecal Coliform`,
         "IN" = `Inorganic Nitrogen`,
         "KN" = `Kjeldahl Nitrogen`,
         "TP" = `Total Phosphorus`,
         "SC" = `Specific Conductance`,
         "TU" = `Turbidity`) %>%
  filter(FC+IN+KN+TP+SC+TU == 6) %>%
  dplyr::select(Creek_Date_LatLong)

WQ <- WQ_wCDLL %>%
  filter(Creek_Date_LatLong %in% CDLL$Creek_Date_LatLong) %>%
  arrange(Creek_Date_LatLong) %>%
  dplyr::select(-Unit, -Month, -Day, -Year) %>%
  spread(key = Measurement, value = Value) %>%
  rename("FC" = `Fecal Coliform`,
         "IN" = `Inorganic Nitrogen`,
         "KN" = `Kjeldahl Nitrogen`,
         "TP" = `Total Phosphorus`,
         "SC" = `Specific Conductance`,
         "TU" = `Turbidity`) %>%
  mutate(TN = IN + KN) %>%
  dplyr::select(Creek, Date, FC, SC, IN, KN, TN, TP, TU, Lat, Long)

Creek_Avg <- WQ %>%
  group_by(Creek) %>%
  summarise(FC = mean(FC),
            SC = mean(SC),
            IN = mean(IN),
            KN = mean(KN),
            TN = mean(TN),
            TP = mean(TP),
            TU = mean(TU))

####################################################
# K-Means Cluster Analysis
kWQ <- WQ[,5:7]
fit <- kmeans(kWQ, 2)

# get cluster means
aggregate(kWQ,by=list(fit$cluster),FUN=mean)
# append cluster assignment
WQ_Cluster <- data.frame(WQ, fit$cluster) %>%
  filter(fit.cluster == 1)


BI_Join <- Macrodata_Final2 %>%
  filter(Creek %in% unique(WQ_Cluster$Creek)) %>%
  group_by(Creek) %>%
  summarise(BI = round(mean(Value), digits = 2))

WQ_Final <- WQ_Cluster %>%
  left_join(BI_Join, by = "Creek") %>%      
  dplyr::select(Creek, BI, FC, SC, TN, TP, TU) %>%
  filter(FC < 7200)    # Filter to remove fecal coliform outliers


# Recoding to enable lognorm data generation
WQ_Final$FC[WQ_Final$FC == 0] <- 1
WQ_Final$TP[WQ_Final$TP == 0] <- 0.01
WQ_Final$TU[WQ_Final$TU == 0] <- 0.1

WQ_Final %>%
  filter(TU != 0) %>%
  summarise(Min = min(TU))



colMeans(Sample_Data)
colMeans(WQ_Final[,2:7], na.rm = T)

#####################################
##### Fecal coliform outlier analysis

WQ_Cluster %>%
  dplyr::select(FC) %>%
  filter(FC < 15000) %>%
  ggplot(aes(x = FC, y = 1)) +
  geom_dotplot(binwidth = 100)

WQ_Cluster %>%
  dplyr::select(FC) %>%
  summarise(Med = median(FC),
            Q75 = quantile(FC, 0.75),
            Q90 = quantile(FC, 0.90),
            Q95 = quantile(FC, 0.95),
            Q99 = quantile(FC, 0.99),
            Max = max(FC),
            Avg = round(mean(FC), digits = 2))

fWQ <- WQ_Cluster$FC
fitFC <- kmeans(fWQ, 2)

# append cluster assignment
FC_Cluster <- data.frame(WQ_Cluster, fitFC$cluster)
# get cluster means
aggregate(fWQ,by=list(fitFC$cluster),FUN=mean)

FC_Cluster %>%
  mutate(cluster = factor(fitFC.cluster)) %>%
  dplyr::select(FC, cluster) %>%
  filter(FC < 7200) %>%
  ggplot(aes(x = FC, y = 1, fill = cluster, color = cluster)) +
  geom_dotplot(binwidth = 10)




T50 <- function(colNum, colName, Meas) {
  tibble(Row = ZVal$Row,
         Z = abs(ZVal[,colNum])) %>%
    arrange(desc(Z)) %>%
    head(n = 50) %>%
    mutate(Meas = Meas)
}

Top50 <- rbind(T50(ColNum = 1, ColName = BI, Meas = "BI"),
               T50(ColNum = 2, ColName = FC, Meas = "FC"),
               T50(ColNum = 3, ColName = SC, Meas = "SC"),
               T50(ColNum = 4, ColName = TN, Meas = "TN"),
               T50(ColNum = 5, ColName = TP, Meas = "TP"),
               T50(ColNum = 6, ColName = TU, Meas = "TU")
)

ColNum <- 2
Meas = "BI"

T50 <- function(ColNum, ColName, Meas) {
  tibble(Row = ZVal[[1]],
    Z = abs(ZVal[[ColNum]]))
    arrange(desc(Z)) %>%
    head(n = 50) %>%
    mutate(Meas = Meas)
}

Top50 <- rbind(T50(ColNum = 2, ColName = BI, Meas = "BI"),
               T50(ColNum = 3, ColName = FC, Meas = "FC"),
               T50(ColNum = 4, ColName = SC, Meas = "SC"),
               T50(ColNum = 5, ColName = TN, Meas = "TN"),
               T50(ColNum = 6, ColName = TP, Meas = "TP"),
               T50(ColNum = 7, ColName = TU, Meas = "TU"))

ZVal[[1]]

MacroData <- read_csv("MacroData.csv")

BI_Data <- Macrodata_Final2 %>%
  dplyr::select(-Watershed)
write_csv(BI_Data, "BI_Data.csv")


c(mean(WQ_Cluster$FC), sd(WQ_Cluster$FC), max(WQ_Cluster$FC))
(max(WQ_Cluster$FC) - mean(WQ_Cluster$FC)) / sd(WQ_Cluster$FC)


ggplot(WQ, aes(x = TN, y = FC)) +
  geom_point() +
  geom_point(data = WQ_Cluster, aes(x = TN, y = FC), color = "blue")


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

kTN <- WQ[,7]
TNfit <- kmeans(kTN, 2)

# get cluster means
aggregate(kTN,by=list(TNfit$cluster),FUN=mean)
# append cluster assignment
Cluster <- data.frame(WQ, TNfit$cluster)


kFC <- WQ[,3]
FCfit <- kmeans(kFC, 2)

# get cluster means
aggregate(kFC,by=list(FCfit$cluster),FUN=mean)
# append cluster assignment
Cluster <- data.frame(Cluster, FCfit$cluster)



Cluster %>%
  mutate(Cluster = ifelse(TNfit.cluster == 2 & FCfit.cluster == 1, 1, 2)) %>%
  group_by(Cluster) %>%
  summarise(n(), mean(FC), mean(TN))
  
Cluster %>%
  mutate(Cluster = ifelse(TNfit.cluster == 2 & FCfit.cluster == 1, 1, 2),
         Cluster = factor(Cluster)) %>%
  ggplot(aes(x = TN, y = FC, color = Cluster)) +
    geom_point() +
    theme_classic()

Cluster %>%
  mutate(Cluster = ifelse(TNfit.cluster == 2 & FCfit.cluster == 1, 1, 2),
         Cluster = factor(Cluster)) %>%
  filter(FCfit.cluster == 1) %>%
  ggplot(aes(x = TN, y = TP, color = Cluster)) +
  geom_point(alpha = 0.05) +
  theme_classic()



install.packages("rstatix")
library(rstatix)

WQ$FC[WQ$FC == 0] <- 1
WQ$TP[WQ$TP == 0] <- 0.01
WQ$TU[WQ$TU == 0] <- 0.1
WQ %>% 
  dplyr::select(FC, TN) %>%
  mahalanobis_distance() %>%
  #mutate(Line = c(1:12351)) %>%
  #filter(is.outlier == FALSE) %>%
  ggplot(aes(x = TN, y = FC, color = is.outlier)) +
    geom_point(alpha = 0.1) +
    coord_cartesian(ylim = 1:20000)

WQ %>%
  mutate(Line = c(1:12351))

1/cov(WQ[,3:7])

WQ$TN[WQ$TN == 0]

mean(WQ$FC)

Random <- tibble(N = c(1:100)) %>%
  sample_n(100)
Random$N[81:100]

tibble(N = c(1:5)) %>%
  sample_n(5)


