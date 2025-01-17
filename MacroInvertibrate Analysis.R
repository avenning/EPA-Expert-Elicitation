library(readxl)

Macrodata <- read_excel("~/Desktop/EPA Project Data/Water Quality Data and Metadata/Water Quality Data and Metadata/State (NCDEQ) datasets/Macroinvertebrate data/Macrodata_wholestate.xlsx")
MacroNeuse <- read_excel("~/Desktop/EPA Project Data/Water Quality Data and Metadata/Water Quality Data and Metadata/State (NCDEQ) datasets/Macroinvertebrate data/Macrodata_Upper Nuese and Cape Fear.xlsx")

Macrodata %>%
  head()

unique(MacroNeuse$Waterbody)


Macrodata_Names <- MacroNeuse %>%
  dplyr::select(Waterbody) %>%
  unique()



  mutate(Waterbody = tolower(Waterbody)) %>%
  separate(Waterbody, sep = " ", into = c("Col1", "Col2", "Col3", "Col4", 
                                          "Col5", "Col6", "Col7", "Col8")) %>%
  dplyr::select(Col1, Col2, Col3, Col4) %>%
  mutate(Name = NA)

MacroNames$Name[MacroNames$Col1 == "third"] <- "Third Fork"

MacroNames <- MacroNames %>%
  mutate(UT = ifelse(Col1 == "ut" | Col2 == "ut" | Col3 == "ut" | Col4 == "ut", TRUE, FALSE))

MacroNames$UT[is.na(MacroNames$UT)] <- FALSE

unique(CreekNames$Name)

write_csv(MacroNames, "macronames.csv")

##########################################################################################################

MacroDataNames <- tibble(Macrodata_Names, MacroNames) %>%
  mutate(Waterbody = Macrodata_Names$Waterbody,
         Creek     = MacroNames$Name,
         UT        = MacroNames$UT) %>%
  dplyr::select(Waterbody, Creek, UT)

Macrodata_Final <- Macrodata %>%
  left_join(MacroDataNames, by = c("Waterbody")) %>%
  filter(!is.na(Creek),
         Month %in% c(3:11)) %>%
  dplyr::select(Creek, Month, Year, BI, Latitude, Longitude, UT)


#########################################################################################################

head(WQ_Data)
glimpse(Raw_Data[1:2,])


glimpse(MacroNeuse)
write_csv(MacroNeuse, "MacroData.csv")


MacroNeuse <- MacroNeuse %>%
  dplyr::select(-LBank, -RBank, -Riffles, -Pools, -LRiparian, -RRiparian)
