library(readxl)
library(MASS)
library(tidyverse)
JM_Response <- read_excel("~/Desktop/EPA Project Data/JM_Response.xlsx")

Response <- JM_Response[,2:9]

head(Response)

Sample <- Sample100[c(1:11, 13:23, 25:35, 37:47, 49:59, 61:71, 73:106), 1:6]
colnames(Sample) <- c("BI", "FC", "SC", "TN", "TP", "TU")
Sample

SurveyResponse <- cbind(Sample, Response) %>%
  left_join(Eco_Weight, by = "Eco") %>%
  left_join(Murky_Weight, by = "Murky")
head(SurveyResponse)
SurveyResponse <- SurveyResponse %>%
  mutate(Eco = factor(Eco),
         Murky = factor(Murky))

EcoReg1 <- polr(Eco ~ BI + FC + SC + TN + TP + TU, data = SurveyResponse, Hess = TRUE, weights = Eco_Weight)
summary(EcoReg1)
EcoReg2 <- polr(Eco ~ BI + TN, data = SurveyResponse, Hess = TRUE, weights = Eco_Weight)
summary(EcoReg2)

MurkyReg1 <- polr(Murky ~ BI + FC + SC + TN + TP + TU, data = SurveyResponse, Hess = TRUE, weights = Murky_Weight)
summary(MurkyReg1)
MurkyReg2 <- polr(Murky ~ SC + TN + TP, data = SurveyResponse, Hess = TRUE, weights = Murky_Weight)
summary(MurkyReg2)
MurkyReg3 <- polr(Murky ~ BI + SC + TN + TP, data = SurveyResponse, Hess = TRUE, weights = Murky_Weight)
summary(MurkyReg3)

Eco_Weight <- SurveyResponse %>%
  dplyr::select(Eco) %>%
  group_by(Eco) %>%
  summarise(Eco_Weight = 50/n()) %>%
  mutate(Eco = as.numeric(Eco))

Murky_Weight <- SurveyResponse %>%
  dplyr::select(Murky) %>%
  group_by(Murky) %>%
  summarise(Murky_Weight = 50/n()) %>%
  mutate(Murky = as.numeric(Murky))

Murky_Weight

clval <- sandwich::vcovCL(m, dat$clustervar)
