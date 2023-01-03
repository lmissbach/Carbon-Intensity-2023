# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "countrycode","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "VennDiagram","xtable")

options(scipen=999)

# 1     Loading Data ####

Country.Set <- c("Argentina", "Armenia", "Bangladesh", "Barbados", "Benin","Bolivia", "Brazil", "Burkina Faso", "Cambodia", "Chile",
                 "Colombia", "Costa Rica", "Cote dIvoire", "Dominican Republic", "Ecuador", "El Salvador", "Ethiopia", 
                 "Europe", 
                 "Ghana","Guatemala", 
                 "Guinea-Bissau", "India", "Indonesia", "Iraq", "Israel", "Kenya", "Liberia", "Malawi", "Maldives", "Mali", 
                 "Mexico", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Niger", "Nigeria", "Norway", "Pakistan", "Paraguay", 
                 "Peru", "Philippines", "Rwanda", "Senegal", "South Africa", "Suriname", "Thailand", "Togo", "Turkey", "Uganda", 
                 "Uruguay"
                 #, 
                 #"Vietnam"
                 )

data_joint_0 <- data.frame()

for(i in Country.Set){
  
  path_0                     <- list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  if(i != "Europe"){
  
    carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", i), show_col_types = FALSE)
  
    household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", i), show_col_types = FALSE)
  
  }
  
  if(i == "Europe"){
    
    carbon_pricing_incidence_0 <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/Carbon_Pricing_Incidence_Europe.csv", show_col_types = FALSE)
    
    household_information_0    <- read_csv("K:/WorkInProgress/2021_Carbon_Footprint_Analysis/Data_Transformed/household_information_Europe_new.csv", show_col_types = FALSE)
    
  }
  
  
  
  burden_decomposition_0     <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Sectoral_Burden_%s.csv", i), show_col_types = FALSE)
  
  # fuel_expenditures          <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/2_Fuel_Expenditure_Data/fuel_expenditures_%s.csv", i))
  
  if(!i %in% c("Chile", "Morocco", "Kenya", "Europe")) appliances_0_1 <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/appliances_0_1_new_%s.csv", path_0, i), show_col_types = FALSE)

  carbon_pricing_incidence_1 <- left_join(household_information_0, carbon_pricing_incidence_0, by = "hh_id")%>%
    left_join(burden_decomposition_0, by = "hh_id")%>%
    mutate(Country_long = i)
  
  if(!i %in% c("Chile", "Morocco", "Kenya", "Europe")) {carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, appliances_0_1, by = "hh_id")}
  
  # Add codes
  
  if("province" %in% colnames(carbon_pricing_incidence_1)){
    Province.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Province.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(province = as.character(province))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(province = as.character(province))%>%
      left_join(Province.Code, by = "province")
  }
  
  if("district" %in% colnames(carbon_pricing_incidence_1)){
    District.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/District.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(district = as.character(district))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(district = as.character(district))%>%
      left_join(District.Code, by = "district")
  }
  
  if("edu_hhh" %in% colnames(carbon_pricing_incidence_1)){
    Education.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Education.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(edu_hhh = as.character(edu_hhh))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(edu_hhh = as.character(edu_hhh))%>%
      left_join(Education.Code, by = "edu_hhh")
    
    # Later: edu_hhh -> ISCED!
  }
  
  if("ind_hhh" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(ind_hhh = as.character(ind_hhh))
    
    # Load codes ?
  }
  
  if("ocu_hhh" %in% colnames(carbon_pricing_incidence_1)){
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(ocu_hhh = as.character(ocu_hhh))
    
    # Load codes ?
  }
  
  if("toilet" %in% colnames(carbon_pricing_incidence_1)){
    Toilet.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Toilet.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(toilet = as.character(toilet))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(toilet = as.character(toilet))%>%
      left_join(Toilet.Code, by = "toilet")
    
    # Later: toilet -> TLT
  }
  
  if("water" %in% colnames(carbon_pricing_incidence_1)){
    Water.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Water.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(water = as.character(water))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(water = as.character(water))%>%
      left_join(Water.Code, by = "water")
    
    # Later: water --> WTR
  }
  
  if("cooking_fuel" %in% colnames(carbon_pricing_incidence_1)){
    Cooking.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Cooking.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(cooking_fuel = as.character(cooking_fuel))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(cooking_fuel = as.character(cooking_fuel))%>%
      left_join(Cooking.Code, by = "cooking_fuel")
    
    # Later: cooking_fuel --> CF
  }
  
  if("heating_fuel" %in% colnames(carbon_pricing_incidence_1)){
    Heating.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Heating.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(heating_fuel = as.character(heating_fuel))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(heating_fuel = as.character(heating_fuel))%>%
      left_join(Heating.Code, by = "heating_fuel")
    
    # Later: heating_fuel --> HF
  }
  
  if("lighting_fuel" %in% colnames(carbon_pricing_incidence_1)){
    Lighting.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Lighting.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(lighting_fuel = as.character(lighting_fuel))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(lighting_fuel = as.character(lighting_fuel))%>%
      left_join(Lighting.Code, by = "lighting_fuel")
    
    # Later: lighting_fuel --> LF
  }
  
  if("ethnicity" %in% colnames(carbon_pricing_incidence_1)){
    Ethnicity.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Ethnicity.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(ethnicity = as.character(ethnicity))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(ethnicity = as.character(ethnicity))%>%
      left_join(Ethnicity.Code, by = "ethnicity")
    
    # if(Country.Name == "Guatemala" | Country.Name == "Nicaragua") Ethnicity.Code <- Ethnicity.Code %>%
    #   select(-Ethnicity_0)
    # 
    # if(Country.Name == "Bolivia") Ethnicity.Code <- Ethnicity.Code %>%
    #   select(-ethnicity,-Ethnicity_1)%>%
    #   rename(ethnicity = Ethnicity_0)
  }   
  
  if("nationality" %in% colnames(carbon_pricing_incidence_1)){
    Nationality.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Nationality.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(nationality = as.character(nationality))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(nationality = as.character(nationality))%>%
      left_join(Nationality.Code, by = "nationality")
  } 
  
  if("religion" %in% colnames(carbon_pricing_incidence_1)){
    Religion.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Religion.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(religion = as.character(religion))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(religion = as.character(religion))%>%
      left_join(Religion.Code, by = "religion")
  } 
  
  if("language" %in% colnames(carbon_pricing_incidence_1)){
    Language.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Language.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(language = as.character(language))
    
    carbon_pricing_incidence_1 <- carbon_pricing_incidence_1 %>%
      mutate(language = as.character(language))%>%
      left_join(Language.Code, by = "language")
  }

  # Collate data
  
  test_0 <- ncol(data_joint_0)
  
  data_joint_0 <- data_joint_0 %>%
    bind_rows(carbon_pricing_incidence_1)
  
  test_1 <- ncol(data_joint_0)
  
  if(test_1-test_0 > 0){print(paste0((test_1-test_0)," new column(s) for ", i, ":", colnames(data_joint_0[(test_0+1):test_1])))}
  
  print(i)
  
  rm(list = ls(pattern = ".Code"))
  rm(household_information_0, burden_decomposition_0, carbon_pricing_incidence_0, carbon_pricing_incidence_1)
  if(!i %in% c("Chile", "Morocco", "Kenya", "Europe")){rm(appliances_0_1)}
}

# 1.1   Homogenize Codes ####

Cooking.Codes.All   <- data.frame()
Lighting.Codes.All  <- data.frame()
Heating.Codes.All   <- data.frame()
Education.Codes.All <- data.frame()
Toilet.Codes.All    <- data.frame()
Water.Codes.All     <- data.frame()

for (i in Country.Set){
  path_0                     <- list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  codes_0                    <- list.files(paste0("../0_Data/1_Household Data/", path_0, "/2_Codes"))
  
  # Cooking fuel
  
  if("Cooking.Code.csv" %in% codes_0){
    Cooking.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Cooking.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(cooking_fuel = as.character(cooking_fuel))%>%
      mutate(Country_long = i)
    
    Cooking.Codes.All <- bind_rows(Cooking.Codes.All, Cooking.Code)
  }
  
  # Lighting fuel
  
  if("Lighting.Code.csv" %in% codes_0){
    Lighting.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Lighting.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(lighting_fuel = as.character(lighting_fuel))%>%
      mutate(Country_long = i)
    
    Lighting.Codes.All <- bind_rows(Lighting.Codes.All, Lighting.Code)
  }
  
  # Heating fuel
  
  if("Heating.Code.csv" %in% codes_0){
    Heating.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Heating.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(heating_fuel = as.character(heating_fuel))%>%
      mutate(Country_long = i)
    
    Heating.Codes.All <- bind_rows(Heating.Codes.All, Heating.Code)
  }
  
  # Education
  
  if("Education.Code.csv" %in% codes_0){
    Education.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Education.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(edu_hhh = as.character(edu_hhh))%>%
      mutate(Country_long = i)
    
    Education.Codes.All <- bind_rows(Education.Codes.All, Education.Code)
  }
  
  # Toilet
  
  if("Toilet.Code.csv" %in% codes_0){
    Toilet.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Toilet.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(toilet = as.character(toilet))%>%
      mutate(Country_long = i)
    
    Toilet.Codes.All <- bind_rows(Toilet.Codes.All, Toilet.Code)
  }
  
  # Water
  
  if("Water.Code.csv" %in% codes_0){
    Water.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Water.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(water = as.character(water))%>%
      mutate(Country_long = i)
    
    Water.Codes.All <- bind_rows(Water.Codes.All, Water.Code)
  }
  
}

Cooking.Codes.All.1 <- Cooking.Codes.All %>%
  select(Country_long, cooking_fuel, Cooking_Fuel, CF)%>%
  mutate(Cooking_Fuel = iconv(Cooking_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Charcoal", "3. Charcoal", "charcoal", "CHARCOAL", "3. CHARCOAL", "CarbÃ³n"), "Charcoal", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Electricity", "electricity", "electricity?", "Publicly-provided electricity/City Power", "Household generator", "9. Electricity", "10. Solar energy", "Electricity form public network", "Electricity from shared generator", "Electricity from private generator", "ELECTRICITY", "16. ELECTRIC", "Electricity from EUCL", "Other electricity distributors", "Solar panel", "Batteries+ Bulb", "Torch/Phone", "Rechargeable battery", "Other source of electricity", "Solar energy system", "Electricity-National grid", "Electricity- Solar", "Electricity- Personal Generator", "Electricity Community/ thermal plant", "Energía eléctrica", "Generator", "5. SOLAR", "Solar energy", "Electric", "Electricity  Community/ thermal plant", "Solar", "Electricidad"), "Electricity", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Mains gas?", "Bulk gas (zeppelin)?", "Gas in tube?", "Gas", "Gas propano","11. Bio gas", "Gobar gas", "Gas por cañería", "Bio Gas", "GAS", "City gas", "biogas", "15. PIPED NATURAL GAS", "13. BIOGAS", "Biogas", "Natural Gas", "Natural gas", "Biogas"), "Gas", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Gas in a carafe?", "Liquefied petroleum gas LPG", "LPG", "8. Butane / gas", "Supergás", "Gasl", "LIQUIFIED PETROLUM", "Liquid gas cylinders", "LPG 3 kg", "LPG 12 kg", "Elpiji 5.5 kg / blue gaz", "14. LPG/ COOKING GAS", "Liquified  petroleum  gas (LPG)", "LPG (bottled gas)", "LPG & Coal"), "LPG", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("kerosene / firewood / charcoal ?", "Kerosene", "Kerosene (gas)","7. Kerosene", "Paraffin-Stove", "Queroseno", "Paraffin", "kerosene", "Kerosine", "PARAFFIN", "1. KEROSENE", "Kerosene / firewood / charcoal?"), "Kerosene", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("other ?", "Other", "Unknown", "None/donâ€™t cook", "Other (Specify)", "", "Does not cook", "Ninguno", "12. None", "13. Other specify", "Others","No cooking arrangement", "Don't cook at home", "OTHER (SPECIFY)", "Not stated", "OTHER(specify)", "Otro Cuál?","No cocinan","N/S","Ignorado", "18. OTHER (SPECIFY)", "ninguno no cocina", "nr", "No Cooking", "Lantern _Agatadowa_", "Other _specify_", "Ninguna", "Candle", "17. GARBAGE/PLASTIC", "Candles", "Oil Lamp", "other?", "None,No Cooking", "Other (specify)", "Other Fuel", "Other, specify", "None", "Unspecified", "No cooking", "No Fuel", "Otro"), "Unknown", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Firewood", "1. Collecting fire wood", "LeÃ±a","2. Purchase fire wood", "Wood", "Firewood and chips", "Leña", "firewood", "COLLECTED FIREWOOD", "PURCHASED FIREWOOD", "4. WOOD", "Wood/Charcoal", "Firewood of Coal", "Firewood, LPG, Coal", "Firewood & LPG", "Firewood & Coal", "Firewood & Kerosene", "Firewood or Coal"), "Firewood", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Animal waste", "4. Crop residue / leaves", "5. Dung / manure", "6. Sawdust", "Crop residue","Sawdust", "Dung cake", "1Grass (reeds)", "Dried cow dung", "6. ANIMAL WASTE/DUNG","7. CROP RESIDUE/PLANT BIOMASS","8. SAW DUST", "STRAW/GRASS", "CROP RESIDUE","SAW DUST","ANIMAL WASTE", "Straw/shrubs/grass","Animal dung","Agricultural crop residue", "Dung of animals", "Wood, coal, plant-sources", "10. BIOMASS BRIQUETTE", "11. PROCESSED BIOMASS(PELLETS)/ WOODCHIPS", "Aninmal Waste", "Cow Dung"), "Other biomass", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Petrol", "12. ETHANOL", "Petroleum", "Other (Oil, Kerosene)"), "Liquid fuel", CF))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Coke, Coal", "briquette", "coal", "Briquettes", "9. COAL BRIQUETTE", "2. COAL/LIGNITE", "Coal"), "Coal", CF))%>%
  mutate(CF = ifelse(CF == "Biomass" | CF == "Other Biomass", "Other biomass", CF))%>%
  mutate(CF = ifelse(CF == "No Fuel" | CF == "Other" | CF == "No cooking" | CF == "No Cooking" | CF == "No cocinan", "Unknown", CF))%>%
  mutate(CF = ifelse(CF == "Firewood Charcoal", "Firewood", CF))%>%
  mutate(CF = ifelse(is.na(CF) & (Country_long == "Togo" | Country_long == "Cote dIvoire" | Country_long == "Maldives" | Country_long == "Ecuador" | Country_long == "Colombia"), "Unknown",CF))%>%
  rename(CF_new = CF)

Lighting.Codes.All.1 <- Lighting.Codes.All %>%
  select(Country_long, lighting_fuel, Lighting_Fuel, LF)%>%
  mutate(Lighting_Fuel = iconv(Lighting_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Charcoal", "3. Charcoal", "charcoal", "CHARCOAL", "3. CHARCOAL"), "Charcoal", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Electricity", "electricity", "electricity?", "Publicly-provided electricity/City Power", "Household generator", "9. Electricity", "10. Solar energy", "Electricity form public network", "Electricity from shared generator", "Electricity from private generator", "ELECTRICITY", "16. ELECTRIC", "Electricity from EUCL", "Other electricity distributors", "Solar panel", "Batteries+ Bulb", "Torch/Phone", "Rechargeable battery", "Other source of electricity", "Solar energy system", "Electricity-National grid", "Electricity- Solar", "Electricity- Personal Generator", "Electricity Community/ thermal plant", "Energía eléctrica", "Generator", "5. SOLAR", "Solar energy", "Electric", "Electricity  Community/ thermal plant", "Solar",
                                                      "Electricité (générateur)", "Electricité réseau", "Plaque solaire", "Publicly-provided electricity/City power", "Electricity (public)", "Electricity (private)", "Solar Panel", "1. Electricity meter - private", "2. Electricity meter - shared", "3. Electricity from generator", "4. Solar energy", "Eletricidade da rede", "Electricidade (gerador)", "PLN electricity with meter", "PLN electricity without meter", "Non PLN electricity", "Electricity connection from Mains", "Solar Energy", "Electricit", "Electricity directle from EBS", "Electricity directly from the government (NH/RO)", "Electricity through the neighbor's dwelling", "Other Source of Electricitcy", "Solar system energy", "Placa solar", "Power Plant"), "Electricity", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Mains gas?", "Bulk gas (zeppelin)?", "Gas in tube?", "Gas", "11. Bio gas", "Gobar gas", "Gas por cañería", "Bio Gas", "GAS", "City gas", "biogas", "15. PIPED NATURAL GAS", "13. BIOGAS", "Biogas", "Natural Gas", "Natural gas", "Biogas", "5. Bio gas", "Gas Lamp", "Gas lamp"), "Gas", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Gas in a carafe?", "Liquefied petroleum gas LPG", "LPG", "8. Butane / gas", "Supergás", "Gasl", "LIQUIFIED PETROLUM", "Liquid gas cylinders", "LPG 3 kg", "LPG 12 kg", "Elpiji 5.5 kg / blue gaz", "14. LPG/ COOKING GAS", "Liquified  petroleum  gas (LPG)", "LPG (bottled gas)", "LPG & Coal", "Gas (Propan)"), "LPG", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("kerosene / firewood / charcoal ?", "Kerosene", "7. Kerosene", "Paraffin-Stove", "Queroseno", "Paraffin", "kerosene", "Kerosine", "PARAFFIN", "1. KEROSENE", "Kerosene / firewood / charcoal?", "Paraffine/Bois/Planche",
                                                      "Kerosene lamp", "9. Kerosene light lamp (imported)", "10. Kerosene lamp (local kuraz)", "Kerosene Lamp", "Parafina/Lenha/Madeira", "Paraffin Lantern", "ParaffinTin lamp", "Paraffin Pressure Lamp", "Paraffin lantern", "Paraffin Tadooba", "Supergás o queroseno"), "Kerosene", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("other ?", "Other", "Unknown", "None/donâ€™t cook", "Other (Specify)", "", "Does not cook", "Ninguno", "12. None", "13. Other specify", "Others","No cooking arrangement", "Don't cook at home", "OTHER (SPECIFY)", "Not stated", "OTHER(specify)", "Otro Cuál?","No cocinan","N/S","Ignorado", "18. OTHER (SPECIFY)", "ninguno no cocina", "nr", "No Cooking", "Other _specify_", "Ninguna", "17. GARBAGE/PLASTIC", "other?", "None,No Cooking", "Other (specify)", "Other Fuel", "Other, specify", "None", "Unspecified", "No cooking", "No Fuel",
                                                      "Autre", "OTHER(SPECIFY)", "Outros", "missing", "No lighting", "Not electricity", "No type of lighting"), "Unknown", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Firewood", "1. Collecting fire wood", "2. Purchase fire wood", "Wood", "Firewood and chips", "Leña", "firewood", "COLLECTED FIREWOOD", "PURCHASED FIREWOOD", "4. WOOD", "Wood/Charcoal", "Firewood of Coal", "Firewood, LPG, Coal", "Firewood & LPG", "Firewood & Coal", "Firewood & Kerosene", "Firewood or Coal", "12. Fire wood", "Fuel wood"), "Firewood", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Animal waste", "4. Crop residue / leaves", "5. Dung / manure", "6. Sawdust", "Crop residue","Sawdust", "Dung cake", "1Grass (reeds)", "Dried cow dung", "6. ANIMAL WASTE/DUNG","7. CROP RESIDUE/PLANT BIOMASS","8. SAW DUST", "STRAW/GRASS", "CROP RESIDUE","SAW DUST","ANIMAL WASTE", "Straw/shrubs/grass","Animal dung","Agricultural crop residue", "Dung of animals", "Wood, coal, plant-sources", "10. BIOMASS BRIQUETTE", "11. PROCESSED BIOMASS(PELLETS)/ WOODCHIPS", "Aninmal Waste", "Cow Dung", "Cow dung", "Grass (reeds)", "GRASS", "Crop Residue"), "Other biomass", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Petrol", "12. ETHANOL", "Petroleum", "Other (Oil, Kerosene)", "Lampe à pétrole", "Candeeiro a petróleo", "Other oil", "Oil Lamp", "Palm Oil"), "Liquid fuel", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Coke, Coal", "briquette", "coal", "Briquettes", "9. COAL BRIQUETTE", "2. COAL/LIGNITE", "Coal"), "Coal", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Lighting_Fuel %in% c("Lampe", "Lampe à pile", "Lampe à pile, grosse torche", "Candle", "7. Lantern", "6. Electrical battery", "8. Light from dry cell with switch", "11. Candle/wax", "Flash Light", "Candles", "Battery", "Vela", "Battery Lamp/ Torch", "Chinese Lamp", "Torchlight", "CANDLES", "BATTERY/DRY CELL(TORCH)", "CANDLES", "Velas", "Batteries", "Lantern _Agatadowa_", "Cargador de batería", "Candeeiro a pilhas", "Petroleum light/candle/dia", "Private light engine", "Water mill"), "Other lighting", LF))%>%
  mutate(LF = ifelse(is.na(LF) & Country_long == "Guatemala", "Unknown", LF))%>%
  mutate(LF = ifelse(LF == "No Lighting" | LF == "Other" | LF == "No lighting", "Unknown", LF))%>%
  rename(LF_new = LF)


Heating.Codes.All.1 <- Heating.Codes.All %>%
  select(Country_long, heating_fuel, Heating_Fuel)%>%
  mutate(HF = ifelse(Heating_Fuel %in% c("Mains gas?", "Gas in tube?", "Bulk gas (zeppelin)?", "Central heating", "Natural gas", "Gas", "Natural Gas", "Gas por cañería"), "Gas", 
                     ifelse(Heating_Fuel %in% c("electricity?", "Electricity", "Solar", "Electricity form public network", "Electricity from shared generator", "Electricity from private generator", "Other source of electricity", "Solar energy", "Electric", "Energía eléctrica"), "Electricity", 
                            ifelse(Heating_Fuel %in% c("Gas in a carafe?", "Liquefied gas", "Liquid gas cylinders", "Supergás"), "LPG",
                                   ifelse(Heating_Fuel %in% c("Wood", "Firewood or Coal", "Wood, coal, plant-sources", "Leña"), "Firewood",
                                          ifelse(Heating_Fuel %in% c("Dung of animals", "Animal dung", "Dried cow dung"), "Other biomass", 
                                                 ifelse(Heating_Fuel %in% c("Kerosene / firewood / charcoal?", "Paraffin", "Kerosene", "Queroseno"), "Kerosene",
                                                        ifelse(Heating_Fuel %in% c("Oil and diesel"), "Liquid fuels",
                                                               ifelse(Heating_Fuel %in% c("Coal"), "Coal","Unknown")))))))))%>%
  rename(HF_new = HF)

# Education.Codes.All.1 <- Education.Codes.All %>%
#   select(Country_long, edu_hhh, Education, ISCED)%>%
#   mutate(Education = iconv(Education, "UTF-8", "UTF-8", sub = ''))%>%
#   write.xlsx(., "0_Data/9_Supplementary Information/Education.Codes.All.xlsx")

Education.Codes.All.1 <- read.xlsx("0_Data/9_Supplementary Information/Education.Codes.All_mapped.xlsx")%>%
  rename(ISCED_new = ISCED)

# Toilet.Codes.All.1 <- Toilet.Codes.All %>%
#   select(Country_long, toilet, Toilet, TLT)%>%
#   mutate(Toilet = ifelse(is.na(Toilet) & !is.na(TLT),TLT,Toilet))%>%
#   mutate(TLT = ifelse(TLT != "Basic" & TLT != "No Service" & TLT != "Limited", NA, TLT))%>%
#   mutate(Toilet = iconv(Toilet, "UTF-8", "UTF-8", sub = ''))%>%
#   write.xlsx(., "0_Data/9_Supplementary Information/Toilet.Codes.All.xlsx")

Toilet.Codes.All.1 <- read.xlsx("0_Data/9_Supplementary Information/Toilet.Codes.All_mapped.xlsx")%>%
  rename(TLT_new = TLT)

# Water.Codes.All.1 <- Water.Codes.All %>%
#   select(Country_long, water, Water, WTR)%>%
#   mutate(Water = ifelse(is.na(Water) & !is.na(WTR),WTR,Water))%>%
#   mutate(WTR = ifelse(WTR != "Basic" & WTR != "No Service" & WTR != "Limited", NA, WTR))%>%
#   mutate(Water = iconv(Water, "UTF-8", "UTF-8", sub = ''))%>%
#   write.xlsx(., "0_Data/9_Supplementary Information/Water.Codes.All.xlsx")

Water.Codes.All.1 <- read.xlsx("0_Data/9_Supplementary Information/Water.Codes.All_mapped.xlsx")%>%
  rename(WTR_new = WTR)

# 1.2   Shape Collated Data ####

data_joint_1 <- data_joint_0 %>%
  # For Guatemala and Nicaragua
  mutate(Religion            = ifelse(is.na(Religion)& !is.na(Ethnicity_0), Ethnicity_0, Religion))%>%
  mutate(share_other_binning = ifelse(is.na(share_other_binning),0,share_other_binning))%>%
  # Ethiopia has no transport fuels
  mutate(exp_s_transport_fuels = ifelse(is.na(exp_s_transport_fuels),0, exp_s_transport_fuels))%>%
  # Confirmed for ARG, BGD, BRA, DOM, GHA, GTM, IND, MNG, NGA, NIC, PAK, PER, PRY, RWA, URY, ZAF
  mutate(exp_s_other_energy = ifelse(is.na(exp_s_other_energy), 0, exp_s_other_energy))%>%
  mutate_at(vars(starts_with("exp_USD_")), list(~ ifelse(is.na(.),0,.)))%>%
  # Remove 13 households from Nigeria without information on hh_weights
  filter(!is.na(hh_weights))%>%
  filter(!is.na(hh_expenditures_USD_2014))%>%
  select(- truck1.01, -pump.01, -solar.heater.01, -video.01, -cooker.01, -air.cooler.01, -cooler.01, -sewing.machine.01, -sewing_machine.01,
         - region, -ocu_hhh, -vacuum.01, -internet.access, -municipality, -clust, -printer.01, -density, -alphabetism, -freezer.01, -heater.01,
         - inc_capital_rents, -inc_deleted, -inc_other_income, -inc_labour, - "inc_other income", -income_year, -iron.01, -bicycle.01,
         - gas_subsidy, -ely_subsidy, -ind_hhh_b, - lat_cen, -long_cen, -country_of_birth, - year, - month, - day, - hh_type)%>%
  left_join(Cooking.Codes.All.1,   by = c("cooking_fuel",  "Country_long"))%>%
  left_join(Lighting.Codes.All.1,  by = c("lighting_fuel", "Country_long"))%>%
  left_join(Heating.Codes.All.1,   by = c("heating_fuel",  "Country_long"))%>%
  left_join(Education.Codes.All.1, by = c("edu_hhh",       "Country_long"))%>%
  left_join(Water.Codes.All.1,     by = c("water",         "Country_long"))%>%
  left_join(Toilet.Codes.All.1,    by = c("toilet",        "Country_long"))%>%
  #select(-lighting_fuel, -cooking_fuel, -heating_fuel, -water, -toilet, -edu_hhh, -ethnicity, -nationality, -language, -religion) %>%
  select(-Education.x,-edu_hhh, -Education.y, - ISCED,
         -Ethnicity_0, 
         -Cooking_Fuel.x, -Cooking_Fuel.y, -CF,
         -Lighting_Fuel.y, -Lighting_Fuel.x, -LF,
         -Heating_Fuel.y, -Heating_Fuel.x, -Country_long,
         -Toilet.y, - Toilet.x, -Water.x, -Water.y, -TLT,-WTR)%>%
  select(hh_id, Country, hh_weights, hh_size, adults, children,
         province, district, village, urban_01, Province, District,
         age_hhh, sex_hhh, ind_hhh, ISCED_new, Nationality, Ethnicity, Language, Religion, religiosity,
         nationality, ethnicity, language, religion, cooking_fuel, heating_fuel, lighting_fuel, heating_fuel,
         water, toilet, # check
         CF_new, HF_new, LF_new, WTR_new, TLT_new, electricity.access,
         hh_expenditures_USD_2014, hh_expenditures, hh_expenditures_pc,
         inc_gov_cash, inc_gov_monetary, Income_Group_5, Income_Group_10,
         starts_with("share_"), starts_with("exp_USD_"),
         starts_with("CO2_"), starts_with("exp_CO2"), starts_with("burden_CO2"), ends_with("_national"), starts_with("exp_s"),
         ends_with(".01"),
         everything())%>%
  rename(CF = CF_new, HF = HF_new, LF = LF_new, ISCED = ISCED_new, WTR = WTR_new, TLT = TLT_new)

rm(data_joint_0, Cooking.Codes.All.1, Lighting.Codes.All.1, Heating.Codes.All.1, Cooking.Codes.All, Lighting.Codes.All, Heating.Codes.All, Heating.Code, Lighting.Code, Cooking.Code, Education.Code, Education.Codes.All, Education.Codes.All.1, Toilet.Code, Toilet.Codes.All, Toilet.Codes.All.1, Water.Code, Water.Codes.All, Water.Codes.All.1)

# Electrification rate TBD

#t <- filter(data_joint_1, is.na(electricity.access))%>%
#  mutate(exp_USD_Electricity = ifelse(exp_USD_Electricity == 0, NA, exp_USD_Electricity))%>%
#  group_by(Country)%>%
#  summarise_all(list(NAs = ~ sum(is.na(.)),
#                     Obs = ~ n()))%>%
#  select(Country, starts_with("exp_USD_Electricity"))%>%
#  mutate(electrification_rate = 1 - (exp_USD_Electricity_NAs/exp_USD_Electricity_Obs))

# 2.    Overview GTAP-CO2-Intensities ####

Country.Set.B <- c(Country.Set, "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
                                  "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
                                  "Portugal", "Romania", "Sweden", "Slovak Republic", "Vietnam")
Country.Set.B <- Country.Set.B[Country.Set.B!="Europe"]

carbon_intensities_0 <- data.frame()

GTAP.Code <- read_delim("../0_Data/2_IO Data/GTAP_10_MRIO/GTAP10.csv", ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

for (i in Country.Set.B){
  
  if(!i %in% c("Barbados", "Liberia", "Suriname", "Mali", "Niger", "Myanmar", "Maldives", "Iraq")){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = i)
  }
  if(i == "Barbados"){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of the Caribbean")}
  if(i == "Suriname"){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of South America")}
  if(i %in% c("Liberia","Mali", "Niger")){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of Western Africa")}
  if(i == "Myanmar"){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of Southeast Asia")}
  if(i == "Maldives"){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of South Asia")}
  if(i == "Iraq"){
    carbon_intensities_1 <- read.xlsx("../0_Data/2_IO Data/GTAP_10_MRIO/Carbon_Intensities_Full_All_incl_Gas_Coal_PC_direct.xlsx", sheet = "Rest of Western Asia")}

  carbon_intensities   <- left_join(GTAP.Code, carbon_intensities_1, by = c("Number"="GTAP"))%>%
    select(-Explanation, - Number)%>%
    mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
    group_by(GTAP)%>%
    summarise(across(CO2_Mt:Total_HH_Consumption_MUSD, ~ sum(.)))%>%
    ungroup()%>%
    mutate(CO2_t_per_dollar_global      = CO2_Mt/            Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_national    = CO2_Mt_within/     Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_electricity = CO2_Mt_Electricity/Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_transport   = CO2_Mt_Transport/  Total_HH_Consumption_MUSD,
           CO2_t_per_dollar_direct      = CO2_direct/        Total_HH_Consumption_MUSD,
           CH4_t_per_dollar_national    = CH4_MtCO2_within/  Total_HH_Consumption_MUSD,
           FGAS_t_per_dollar_national   = FGAS_MtCO2_within/ Total_HH_Consumption_MUSD,
           N2O_t_per_dollar_national    = N2O_MtCO2_within/  Total_HH_Consumption_MUSD)%>%
    select(GTAP, starts_with("CO2_t"), ends_with("national"))%>%
    mutate(Country = i)
  
  path_0   <-list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  if(!i %in% c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
               "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
               "Portugal", "Romania", "Sweden", "Slovak Republic")){
    matching <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_GTAP_Concordance_%s.xlsx", path_0, i))
  }
 
  if(i %in% c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
              "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
              "Portugal", "Romania", "Sweden", "Slovak Republic")){
    matching <- read.xlsx("../0_Data/1_Household Data/4_Europe_EU27/3_Matching_Tables/Item_GTAP_Concordance_EU_incl_Artificial.xlsx")
  }
  
  matching <- matching %>%
    select(1,3)%>%
    mutate(included = ifelse(!is.na(X3),1,0),
           GTAP     = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt",GTAP))%>%
    group_by(GTAP)%>%
    summarise(first_item = first(X3),
              included   = max(included))%>%
    ungroup()%>%
    mutate(first_item = as.character(first_item))
  
  carbon_intensities <- carbon_intensities %>%
    left_join(matching, by = "GTAP")
  
  carbon_intensities_0 <- bind_rows(carbon_intensities_0, carbon_intensities)
}

# 2.1   Analyse and systematically compare carbon intensities ####

carbon_intensities_2.1 <- carbon_intensities_0 %>%
  select(everything())%>%
  filter(CO2_t_per_dollar_national != "Inf")%>%
  group_by(GTAP)%>%
  mutate(cutoff_90 = quantile(CO2_t_per_dollar_national, probs = 0.9),
         max_value = max(CO2_t_per_dollar_national))%>%
  ungroup()%>%
  mutate(top_10 = ifelse(CO2_t_per_dollar_national > cutoff_90, 1,0))%>%
  mutate(code = countrycode(Country, origin = "country.name", destination = "iso3c"))%>%
  mutate(label  = ifelse(top_10 == 1, code, ""))%>%
  arrange(max_value)%>%
  left_join(select(GTAP.Code, GTAP, Explanation), by = "GTAP")%>%
  mutate(Explanation_0 = sub("\\:.*", "",Explanation))%>%
  mutate(Explanation_0 = ifelse(is.na(Explanation_0), "Gas extraction, manufacture, distribution", Explanation_0))%>%
  mutate(Explanation_1 = paste0(Explanation_0, " (", GTAP, ")"))%>%
  mutate(GTAP_1 = factor(Explanation_1, levels = unique(Explanation_1[order(max_value, decreasing = TRUE)]), ordered = TRUE))%>%
  select(-starts_with("Explanation"))

carbon_intensities_2.1.1 <- carbon_intensities_0 %>%
  select(everything())%>%
  filter(CO2_t_per_dollar_national != "Inf")%>%
  filter(included == 1)%>%
  group_by(GTAP)%>%
  mutate(cutoff_90 = quantile(CO2_t_per_dollar_national, probs = 0.9),
         max_value = max(CO2_t_per_dollar_national))%>%
  ungroup()%>%
  mutate(top_10 = ifelse(CO2_t_per_dollar_national > cutoff_90, 1,0))%>%
  mutate(code = countrycode(Country, origin = "country.name", destination = "iso3c"))%>%
  mutate(label  = ifelse(top_10 == 1, code, ""))%>%
  arrange(max_value)%>%
  left_join(select(GTAP.Code, GTAP, Explanation), by = "GTAP")%>%
  mutate(Explanation_0 = sub("\\:.*", "",Explanation))%>%
  mutate(Explanation_0 = ifelse(is.na(Explanation_0), "Gas extraction, manufacture, distribution", Explanation_0))%>%
  mutate(Explanation_1 = paste0(Explanation_0, " (", GTAP, ")"))%>%
  mutate(GTAP_1 = factor(Explanation_1, levels = unique(Explanation_1[order(max_value, decreasing = TRUE)]), ordered = TRUE))%>%
  select(-starts_with("Explanation"))
  
carbon_intensities_2.1.2 <- carbon_intensities_0 %>%
  select(everything())%>%
  filter(CO2_t_per_dollar_direct != "NaN")%>%
  filter(included == 1)%>%
  group_by(GTAP)%>%
  mutate(cutoff_90 = quantile(CO2_t_per_dollar_direct, probs = 0.9),
         max_value = max(CO2_t_per_dollar_direct))%>%
  ungroup()%>%
  mutate(top_10 = ifelse(CO2_t_per_dollar_direct > cutoff_90, 1,0))%>%
  mutate(code = countrycode(Country, origin = "country.name", destination = "iso3c"))%>%
  mutate(label  = ifelse(top_10 == 1, code, ""))%>%
  arrange(max_value)%>%
  left_join(select(GTAP.Code, GTAP, Explanation), by = "GTAP")%>%
  mutate(Explanation_0 = sub("\\:.*", "",Explanation))%>%
  mutate(Explanation_0 = ifelse(is.na(Explanation_0), "Gas extraction, manufacture, distribution", Explanation_0))%>%
  mutate(Explanation_1 = paste0(Explanation_0, " (", GTAP, ")"))%>%
  mutate(GTAP_1 = factor(Explanation_1, levels = unique(Explanation_1[order(max_value, decreasing = TRUE)]), ordered = TRUE))%>%
  select(-starts_with("Explanation"))%>%
  filter(GTAP == "gasgdt" | GTAP == "p_c" | GTAP == "coa")

P_2.1 <- ggplot(carbon_intensities_2.1)+
  geom_point(aes(y = CO2_t_per_dollar_national, x = 1, fill = factor(included)), position = position_jitter(width = 0.4, seed = 2022), size = 1.5, shape = 21)+
  geom_text_repel(aes(x = 1, label = label, y = CO2_t_per_dollar_national), 
                  size = 2.5, segment.size = 0.3, max.overlaps = Inf, position = position_jitter(width = 0.4, seed = 2022))+
  facet_wrap(. ~ GTAP_1, scales = "free_y", labeller = label_wrap_gen(width = 35))+
  theme_bw()+
  scale_fill_nejm(labels = c("No item matched", "Items matched"))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  labs(fill = "")+
  ylab(expression(paste("Carbon intensity in t", CO[2], sep = " per USD")))+
  xlab("")+
  ggtitle("Sectoral carbon intensity")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title  = element_text(size = 6),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_2.1.1 <- ggplot(carbon_intensities_2.1.1)+
  geom_point(aes(y = CO2_t_per_dollar_national, x = 1, fill = factor(included)), position = position_jitter(width = 0.4, seed = 2022), size = 1.5, shape = 21)+
  geom_text_repel(aes(x = 1, label = label, y = CO2_t_per_dollar_national), 
                  size = 2.5, segment.size = 0.3, max.overlaps = Inf, position = position_jitter(width = 0.4, seed = 2022))+
  facet_wrap(. ~ GTAP_1, scales = "free_y", labeller = label_wrap_gen(width = 30))+
  theme_bw()+
  scale_fill_manual(values = "#0072B5FF", guide = "none")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  labs(fill = "")+
  ylab(expression(paste("Carbon intensity in t", CO[2], sep = " per USD")))+
  xlab("")+
  ggtitle("Sectoral carbon intensity - included items")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title  = element_text(size = 6),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_2.1.2 <- ggplot(carbon_intensities_2.1.2)+
  geom_point(aes(y = CO2_t_per_dollar_direct, x = 1, fill = factor(included)), position = position_jitter(width = 0.4, seed = 2022), size = 1.5, shape = 21)+
  geom_text_repel(aes(x = 1, label = label, y = CO2_t_per_dollar_direct), 
                  size = 2.5, segment.size = 0.3, max.overlaps = Inf, position = position_jitter(width = 0.4, seed = 2022))+
  facet_wrap(. ~ GTAP_1, scales = "free_y", labeller = label_wrap_gen(width = 30))+
  theme_bw()+
  scale_fill_manual(values = "#0072B5FF", guide = "none")+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  labs(fill = "")+
  ylab(expression(paste("Carbon intensity in t", CO[2], sep = " per USD")))+
  xlab("")+
  coord_cartesian(ylim = c(0,0.045))+
  ggtitle("Sectoral carbon intensity - direct emissions - included items")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title  = element_text(size = 6),
        plot.title  = element_text(size = 10),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Figure_2.1.0.jpg", width = 40, height = 40, unit = "cm", res = 400)
print(P_2.1)
dev.off()

jpeg("1_Figures/Figure_2.1.1.jpg", width = 40, height = 40, unit = "cm", res = 400)
print(P_2.1.1)
dev.off()

jpeg("1_Figures/Figure_2.1.2.jpg", width = 20, height = 20, unit = "cm", res = 400)
print(P_2.1.2)
dev.off()

rm(carbon_intensities, carbon_intensities_0, carbon_intensities_1, carbon_intensities_2.1, carbon_intensities_2.1.1, carbon_intensities_2.1.2, GTAP.Code,
   P_2.1, P_2.1.1, P_2.1.2)

# 3     Miscellaneous ####

# 3.1   Checking fuel assignment ####

Country.Set <- c("Argentina", "Armenia", "Bangladesh", "Barbados", "Benin","Bolivia", "Brazil", "Burkina Faso", "Cambodia", "Chile",
                 "Colombia", "Costa Rica", "Cote dIvoire", "Dominican Republic", "Ecuador", "El Salvador", "Ethiopia", 
                 "Ghana","Guatemala", 
                 "Guinea-Bissau", "India", "Indonesia", "Iraq", "Israel", "Kenya", "Liberia", "Malawi", "Maldives", "Mali", 
                 "Mexico", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Niger", "Nigeria", "Norway", "Pakistan", "Paraguay", 
                 "Peru", "Philippines", "Rwanda", "Senegal", "South Africa", "Suriname", "Thailand", "Togo", "Turkey", "Uganda", 
                 "Uruguay"
                 #, 
                 #"Vietnam"
)

Item.Codes.All <- data.frame()

for(i in Country.Set){
  path_0                     <- list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  Item_Codes <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Codes_Description_%s.xlsx", path_0, i))
  
  if(!"item_code" %in% colnames(Item_Codes)){print(paste0("No item_code:", i))}
  if(!"item_name" %in% colnames(Item_Codes)){print(paste0("No item_name:",i))}
  
  matching <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_GTAP_Concordance_%s.xlsx", path_0, i))
  
  if(i == "Thailand" | i == "Maldives" | i == "Pakistan" | i == "Iraq"){
    matching <- matching %>%
      mutate_at(vars(-GTAP, -Explanation),~ as.numeric(.))
  }
  
  if(i == "Colombia"){
    matching <- matching %>%
      mutate(X41 = as.character(X41),
             X42 = as.character(X42),
             X43 = as.character(X43))}
  
  if(i == "Bolivia" | i == "Armenia" | i == "Bangladesh"){
    matching <- matching %>%
      mutate_at(.vars = vars(-GTAP), .funs = list(~ as.character(.)))
  }
  
  matching <- matching %>%
    select (-Explanation) %>%
    pivot_longer(-GTAP, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    select(GTAP, item_code)%>%
    mutate(GTAP = ifelse(GTAP == "gas" | GTAP == "gdt", "gasgdt", GTAP))%>%
    mutate(item_code = as.character(item_code))
  
  fuels <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Fuel_Concordance_%s.xlsx", path_0, i), colNames = FALSE)
  if(i == "Thailand" | i == "Maldives" | i == "Iraq"){
    fuels <- fuels %>%
      mutate_at(vars(-X1),~ as.numeric(.))
  }
  fuels <- fuels %>%
    pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    rename(fuel = X1)%>%
    select(fuel, item_code)%>%
    mutate(item_code = as.character(item_code))
  
  categories <- read.xlsx(sprintf("../0_Data/1_Household Data/%s/3_Matching_Tables/Item_Categories_Concordance_%s.xlsx", path_0, i), colNames = FALSE)
  
  if(i == "Thailand" | i == "Maldives" | i == "Iraq"){
    categories <- categories %>%
      mutate_at(vars(-X1),~ as.numeric(.))
  }
  
  if(i == "Bolivia" | i == "Armenia" | i == "Bangladesh"){
    categories <- categories %>%
      mutate_at(.vars = vars(-X1), .funs = list(~ as.character(.)))
  }
  
  categories <- categories %>%
    pivot_longer(-X1, names_to = "drop", values_to = "item_code")%>%
    filter(!is.na(item_code))%>%
    select(X1, item_code)%>%
    rename(category = X1)%>%
    distinct(category, item_code)%>%
    mutate(item_code = as.character(item_code))
  
  item_codes <- Item_Codes %>%
    select(item_code, item_name)%>%
    mutate(item_code = as.character(item_code))%>%
    left_join(matching, by = "item_code")%>%
    left_join(fuels, by = "item_code")%>%
    left_join(categories, by = "item_code")%>%
    mutate(Country = i)
  
  Item.Codes.All <- Item.Codes.All %>%
    bind_rows(item_codes)
  
}

Item.Codes.All.1 <- Item.Codes.All %>%
  filter(category == "energy" | is.na(category) | !is.na(fuel) | GTAP == "p_c" | GTAP == "ely" | GTAP == "gasgdt" | GTAP == "coa")

rm(Item.Codes.All, Item_Codes, item_codes, matching, fuels, categories)

# 3.2   Track NAs over observations ####

NAs_over_obs <- data_joint_1 %>%
  select(everything())%>%
  group_by(Country)%>%
  summarise_all(list(NAs = ~ sum(is.na(.)),
                     Obs = ~ n()))%>%
  ungroup()%>%
  pivot_longer(-Country, names_to = "Var", values_to = "Val")%>%
  mutate(Type_A = str_sub(Var,-3,-1),
         Type_B = str_sub(Var,1,-5))%>%
  select(-Var)%>%
  pivot_wider(names_from = "Type_A", values_from = "Val")%>%
  mutate(share = NAs/Obs)%>%
  arrange(Type_B, share, Country)%>%
  filter(NAs != 0)

write.xlsx(NAs_over_obs, "0_Data/9_Supplementary Information/NAs_over_Observations_0.xlsx")

# 4     Output ####

# 4.1   Create and save codes ####

dir.create("0_Data/9_Supplementary Information/2_Codes", showWarnings = FALSE)

# Define Codes

Cooking.Codes.Aggregate <- distinct(data_joint_1, CF)%>%
  arrange(CF)%>%
  mutate(CF_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Cooking.Code.All.csv")

Lighting.Codes.Aggregate <- distinct(data_joint_1, LF)%>%
  arrange(LF)%>%
  mutate(LF_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Lighting.Code.All.csv")

Heating.Codes.Aggregate <- distinct(data_joint_1, HF)%>%
  arrange(HF)%>%
  mutate(HF_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Heating.Code.All.csv")

Water.Codes.Aggregate <- distinct(data_joint_1, WTR)%>%
  arrange(WTR)%>%
  mutate(WTR_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Water.Code.All.csv")

Toilet.Codes.Aggregate <- distinct(data_joint_1, TLT)%>%
  arrange(TLT)%>%
  mutate(TLT_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Toilet.Code.All.csv")

Education.Codes.Aggregate <- distinct(data_joint_1, ISCED)%>%
  arrange(ISCED)%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Education.Code.All.csv")

# Country-specific codes

Province.Codes.Aggregate <- distinct(data_joint_1, province, Province, Country)%>%
  arrange(Country, Province)%>%
  mutate(Province_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Province.Code.All.csv")

District.Codes.Aggregate <- distinct(data_joint_1, district, District, Country)%>%
  arrange(Country, District)%>%
  mutate(District_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/District.Code.All.csv")

Nationality.Codes.Aggregate <- distinct(data_joint_1, Country, Nationality, nationality)%>%
  arrange(Country, nationality)%>%
  mutate(Nationality_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Nationality.Code.All.csv")

Ethnicity.Codes.Aggregate <- distinct(data_joint_1, Country, Ethnicity, ethnicity)%>%
  arrange(Country, ethnicity)%>%
  mutate(Ethnicity_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Ethnicity.Code.All.csv")

Language.Codes.Aggregate <- distinct(data_joint_1, Country, Language, language)%>%
  arrange(Country, language)%>%
  mutate(Language_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Language.Code.All.csv")

Religion.Codes.Aggregate <- distinct(data_joint_1, Country, Religion, religion)%>%
  arrange(Country, religion)%>%
  mutate(Religion_code = 1:n())%>%
  write_csv(., "0_Data/9_Supplementary Information/2_Codes/Religion.Code.All.csv")

# 4.2   Split and save data ####

data_joint_split <- data_joint_1 %>%
  left_join(Cooking.Codes.Aggregate,     by = "CF")%>%
  left_join(Lighting.Codes.Aggregate,    by = "LF")%>%
  left_join(Heating.Codes.Aggregate,     by = "HF")%>%
  left_join(Water.Codes.Aggregate,       by = "WTR")%>%
  left_join(Toilet.Codes.Aggregate,      by = "TLT")%>%
  left_join(Education.Codes.Aggregate,   by = "ISCED")%>%
  # Country-specific
  left_join(Province.Codes.Aggregate,    by = c("Country", "province", "Province"))%>%
  left_join(District.Codes.Aggregate,    by = c("Country", "district", "District"))%>%
  left_join(Nationality.Codes.Aggregate, by = c("Country", "nationality", "Nationality"))%>%
  left_join(Ethnicity.Codes.Aggregate,   by = c("Country", "ethnicity", "Ethnicity"))%>%
  left_join(Language.Codes.Aggregate,    by = c("Country", "language", "Language"))%>%
  left_join(Religion.Codes.Aggregate,    by = c("Country", "religion", "Religion"))%>%
  select(-CF,-LF,-HF,-WTR,-TLT,-province,-Province,-district,-District,-nationality,-Nationality,
         -ethnicity,-Ethnicity,-language,-Language,-religion,-Religion,
         -cooking_fuel,-heating_fuel,-lighting_fuel,-water,-toilet)%>%
  select(hh_id, Country, hh_weights, hh_size, adults, children,
         Province_code, District_code, village, urban_01,
         age_hhh, sex_hhh, ind_hhh, ISCED, Nationality_code, Ethnicity_code, Language_code, Religion_code, religiosity,
         CF_code, LF_code, HF_code, WTR_code, TLT_code, electricity.access,
         hh_expenditures_USD_2014, hh_expenditures, hh_expenditures_pc,
         inc_gov_cash, inc_gov_monetary, Income_Group_5, Income_Group_10,
         starts_with("share_"), starts_with("exp_USD_"),
         starts_with("CO2_"), starts_with("exp_CO2"), starts_with("burden_CO2"), ends_with("_national"), starts_with("exp_s"),
         ends_with(".01"),
         everything())%>%
  # delete derived values 
  select(-exp_CO2_global,-exp_CO2_national,-exp_CO2_electricity,-exp_CO2_transport,
         -burden_CO2_global,-burden_CO2_national,-burden_CO2_electricity,-burden_CO2_transport)%>%
  mutate_at(vars(hh_size,adults,children,hh_id,urban_01,age_hhh,sex_hhh,
                 Income_Group_5, Income_Group_10, electricity.access, ISCED, ends_with(".01")), list(~ as.integer(.)))

rm(Cooking.Codes.Aggregate,Lighting.Codes.Aggregate,Heating.Codes.Aggregate,Water.Codes.Aggregate,Toilet.Codes.Aggregate,
   Province.Codes.Aggregate,District.Codes.Aggregate,Nationality.Codes.Aggregate,Ethnicity.Codes.Aggregate,Language.Codes.Aggregate,Religion.Codes.Aggregate)

dir.create("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database", showWarnings = FALSE)

write_rds(data_joint_split, "../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")  
