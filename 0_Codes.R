if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "VennDiagram","xtable")

options(scipen=999)

# Load Data

Cooking.Codes.All <- read.xlsx("Cooking.Codes.All.xlsx")

Cooking.Codes.All.1 <- Cooking.Codes.All %>%
  select(Country, cooking_fuel, Cooking_Fuel, CF)%>%
  mutate(Cooking_Fuel = iconv(Cooking_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  # As an example: CF should be "Charcoal", if Cooking_Fuel is "Charchoal" or "3. Charcoal" or...
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Charcoal", "3. Charcoal"), "Charcoal", CF))%>%
  # maybe one row for each fuel? 
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Electricity", "electricity"), "Electricity", CF))%>%
  
Lighting.Codes.All.1 <- Lighting.Codes.All %>%
  select(Country, lighting_fuel, Lighting_Fuel, LF)%>%
  mutate(Lighting_Fuel = iconv(Lighting_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  mutate(LF = )

Heating.Codes.All.1 <- Heating.Codes.All %>%
  select(Country, heating_fuel, Heating_Fuel, HF)%>%
  mutate(HF =)