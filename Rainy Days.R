library(readr)
library(tidyverse)
library(lubridate)
Precip <- read_csv("~/Downloads/2348173.csv")
Precip2 <- read_csv("~/Downloads/2351767.csv")

Precip %>%
  mutate(Date = DATE,
         Month = month(DATE),
         Day = day(DATE),
         Year = year(DATE),
         Precip = PRCP,
         Rain = ifelse(Precip >= 0.1, 1, 0)) %>%
  filter(Month %in% c(3:11),
         Year %in% c(1945:2019)) %>%
  dplyr::select(Date, Month, Day, Year, Precip, Rain) %>%
  group_by(Year) %>%
  summarise(RainyDays = sum(Rain),
            TotalDays = n()) %>%
  summarise(Mean = mean(RainyDays),
            StDev = sd(RainyDays))

55.9 + 1.96 * 7.93
55.9 - 1.96 * 7.93

71.5/275
40.4/275


Precip2 %>%
  mutate(Station = ifelse(NAME == "BURLINGTON FIRE STATION NUMBER 5, NC US", "Burlington", "Siler City"),
         Date = DATE,
         Month = month(DATE),
         Day = day(DATE),
         Year = year(DATE),
         Precip = PRCP,
         Rain = ifelse(Precip >= 0.1, 1, 0)) %>%
  select(Station, Date, Month, Day, Year, Precip, Rain) %>%
  filter(Month %in% c(3:11),
         Station == "Siler City") %>%
  group_by(Year) %>%
  summarise(RainyDays = sum(Rain),
            TotalDays = n()) %>%
  filter(!is.na(RainyDays)) %>%
  summarise(Mean = mean(RainyDays),
            StDev = sd(RainyDays))
  head()


unique(Precip2$NAME)


