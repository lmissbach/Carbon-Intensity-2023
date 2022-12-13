if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "VennDiagram","xtable")

options(scipen=999)

# Load Data

Cooking.Codes.All <- read.xlsx("0_Data/9_Supplementary Information/Cooking.Codes.All.xlsx")

Cooking.Codes.All.1 <- Cooking.Codes.All %>%
  select(Country, cooking_fuel, Cooking_Fuel, CF)%>%
  mutate(Cooking_Fuel = iconv(Cooking_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  # As an example: CF should be "Charcoal", if Cooking_Fuel is "Charchoal" or "3. Charcoal" or...
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Charcoal", "3. Charcoal", "charcoal", "CHARCOAL", "3. CHARCOAL"), "Charcoal", CF))%>%
  # maybe one row for each fuel? 
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Electricity", "electricity", "electricity?", "Publicly-provided electricity/City Power", "Household generator", "9. Electricity", "10. Solar energy", "Electricity form public network", "Electricity from shared generator", "Electricity from private generator", "ELECTRICITY", "16. ELECTRIC", "Electricity from EUCL", "Other electricity distributors", "Solar panel", "Batteries+ Bulb", "Torch/Phone", "Rechargeable battery", "Other source of electricity", "Solar energy system", "Electricity-National grid", "Electricity- Solar", "Electricity- Personal Generator", "Electricity Community/ thermal plant", "Energía eléctrica", "Generator", "5. SOLAR", "Solar energy", "Electric", "Electricity  Community/ thermal plant"), "Electricity", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Mains gas?", "Bulk gas (zeppelin)?", "Gas in tube?", "Gas", "11. Bio gas", "Gobar gas", "Gas por cañería", "Bio Gas", "GAS", "City gas", "biogas", "15. PIPED NATURAL GAS", "13. BIOGAS", "Biogas", "Natural Gas", "Natural gas", "Biogas"), "Gas", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Gas in a carafe?", "Liquefied petroleum gas LPG", "LPG", "8. Butane / gas", "Supergás", "Gasl", "LIQUIFIED PETROLUM", "Liquid gas cylinders", "LPG 3 kg", "LPG 12 kg", "Elpiji 5.5 kg / blue gaz", "14. LPG/ COOKING GAS", "Liquified  petroleum  gas (LPG)"), "LPG", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("kerosene / firewood / charcoal ?", "Kerosene", "7. Kerosene", "Paraffin-Stove", "Queroseno", "Paraffin", "kerosene", "Kerosine", "PARAFFIN", "1. KEROSENE", "Kerosene / firewood / charcoal?"), "Kerosene", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("other ?", "Other", "Unknown", "None/donâ€™t cook", "Other (Specify)", "", "Does not cook", "Ninguno", "12. None", "13. Other specify", "Others","No cooking arrangement", "Don't cook at home", "OTHER (SPECIFY)", "Not stated", "OTHER(specify)", "Otro Cuál?","No cocinan","N/S","Ignorado", "18. OTHER (SPECIFY)", "ninguno no cocina", "nr", "No Cooking", "Lantern _Agatadowa_", "Other _specify_", "Ninguna", "Candle", "17. GARBAGE/PLASTIC", "Candles", "Oil Lamp", "other?", "None,No Cooking", "Other (specify)", "Other Fuel", "Other, specify", "None", "Unspecified", "No cooking"), "Unknown", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Firewood", "1. Collecting fire wood", "2. Purchase fire wood", "Wood", "Firewood and chips", "Leña", "firewood", "COLLECTED FIREWOOD", "PURCHASED FIREWOOD", "4. WOOD"), "Firewood", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Animal waste", "4. Crop residue / leaves", "5. Dung / manure", "6. Sawdust", "Crop residue","Sawdust", "Dung cake", "1Grass (reeds)", "Dried cow dung", "6. ANIMAL WASTE/DUNG","7. CROP RESIDUE/PLANT BIOMASS","8. SAW DUST", "STRAW/GRASS", "CROP RESIDUE","SAW DUST","ANIMAL WASTE", "Straw/shrubs/grass","Animal dung","Agricultural crop residue", "Dung of animals", "Wood, coal, plant-sources", "10. BIOMASS BRIQUETTE", "11. PROCESSED BIOMASS(PELLETS)/ WOODCHIPS", "Aninmal Waste", "Cow Dung"), "Other biomass", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Petrol", "12. ETHANOL", "Petroleum"), "Liquid fuel", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Coke, Coal", "briquette", "coal", "Briquettes", "9. COAL BRIQUETTE", "2. COAL/LIGNITE", "Coal"), "Coal", CF))%>%
  mutate(CF = ifelse(CF == "Biomass", "Other biomass", CF))%>%
  mutate(CF = ifelse(CF == "No Fuel" | CF == "Other", "Unknown", CF))%>%
  mutate(CF = ifelse(is.na(CF) & is.na(Cooking_Fuel), "Unknown", CF))
  
Lighting.Codes.All.1 <- Lighting.Codes.All %>%
  select(Country, lighting_fuel, Lighting_Fuel, LF)%>%
  mutate(Lighting_Fuel = iconv(Lighting_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  mutate(LF = )

Heating.Codes.All.1 <- Heating.Codes.All %>%
  select(Country, heating_fuel, Heating_Fuel, HF)%>%
  mutate(HF =)