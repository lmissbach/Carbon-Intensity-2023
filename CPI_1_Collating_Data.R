# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "VennDiagram","xtable")

options(scipen=999)

# 1     Loading Data ####

Country.Set <- c("Argentina", "Armenia", "Bangladesh", "Barbados", "Benin","Bolivia", "Brazil", "Burkina Faso", "Cambodia", "Chile",
                 "Colombia", "Costa Rica", "Cote dIvoire", "Dominican Republic", "Ecuador", "El Salvador", "Ethiopia", 
                 #"Europe", 
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
  
  carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", i), show_col_types = FALSE)
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", i), show_col_types = FALSE)
  
  burden_decomposition_0     <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Sectoral_Burden_%s.csv", i), show_col_types = FALSE)
  
  # fuel_expenditures          <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/2_Fuel_Expenditure_Data/fuel_expenditures_%s.csv", i))
  
  if(!i %in% c("Chile", "Morocco", "Kenya", "Europe")) appliances_0_1 <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/appliances_0_1_new_%s.csv", path_0, i), show_col_types = FALSE)

  carbon_pricing_incidence_1 <- left_join(household_information_0, carbon_pricing_incidence_0, by = "hh_id")%>%
    left_join(burden_decomposition_0, by = "hh_id")
  
  if(!i %in% c("Chile", "Morocco", "Kenya", "Europe")) {carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, appliances_0_1, by = "hh_id")}
  
  # Ad codes
  
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
  if(!i %in% c("Chile", "Marocco", "Kenya", "Europe")){rm(appliances_0_1)}
}

# 1.1  Homogenize Codes ####

Cooking.Codes.All   <- data.frame()
Lighting.Codes.All  <- data.frame()
Heating.Codes.All   <- data.frame()
Education.Codes.All <- data.frame()

for (i in Country.Set){
  path_0                     <- list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  codes_0                    <- list.files(paste0("../0_Data/1_Household Data/", path_0, "/2_Codes"))
  
  # Cooking fuel
  
  if("Cooking.Code.csv" %in% codes_0){
    Cooking.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Cooking.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(cooking_fuel = as.character(cooking_fuel))%>%
      mutate(Country = i)
    
    Cooking.Codes.All <- bind_rows(Cooking.Codes.All, Cooking.Code)
  }
  
  # Lighting fuel
  
  if("Lighting.Code.csv" %in% codes_0){
    Lighting.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Lighting.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(lighting_fuel = as.character(lighting_fuel))%>%
      mutate(Country = i)
    
    Lighting.Codes.All <- bind_rows(Lighting.Codes.All, Lighting.Code)
  }
  
  # Heating fuel
  
  if("Heating.Code.csv" %in% codes_0){
    Heating.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Heating.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(heating_fuel = as.character(heating_fuel))%>%
      mutate(Country = i)
    
    Heating.Codes.All <- bind_rows(Heating.Codes.All, Heating.Code)
  }
  
  # Education
  
  if("Education.Code.csv" %in% codes_0){
    Education.Code <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Education.Code.csv", path_0), show_col_types = FALSE)%>%
      mutate(edu_hhh = as.character(edu_hhh))%>%
      mutate(Country = i)
    
    Education.Codes.All <- bind_rows(Education.Codes.All, Education.Code)
  }
  
}

Cooking.Codes.All.1 <- Cooking.Codes.All %>%
  select(Country, cooking_fuel, Cooking_Fuel, CF)%>%
  mutate(Cooking_Fuel = iconv(Cooking_Fuel, "UTF-8", "UTF-8", sub = ''))%>%
  mutate(CF = ifelse(is.na(CF) & Cooking_Fuel %in% c("Charcoal", "3. Charcoal"), "Charcoal", CF))
# TBA Marlene
  #write.xlsx(., "0_Data/9_Supplementary Information/Cooking.Codes.All.xlsx")

Lighting.Codes.All.1 <- Lighting.Codes.All %>%
  select(Country, lighting_fuel, Lighting_Fuel, LF)%>%
  mutate(Lighting_Fuel = iconv(Lighting_Fuel, "UTF-8", "UTF-8", sub = ''))#%>%
# TBA Marlene
  #write.xlsx(., "0_Data/9_Supplementary Information/Lighting.Codes.All.xlsx")

Heating.Codes.All.1 <- Heating.Codes.All %>%
  select(Country, heating_fuel, Heating_Fuel, HF)# %>%
# TBA Marlene
  #write.xlsx(., "0_Data/9_Supplementary Information/Heating.Codes.All.xlsx")

Education.Codes.All.1 <- Education.Codes.All %>%
  select(Country, edu_hhh, Education, ISCED)%>%
  mutate(Education = iconv(Education, "UTF-8", "UTF-8", sub = ''))#%>%
# TBA Marlene
  #write.xlsx(., "0_Data/9_Supplementary Information/Education.Codes.All.xlsx")


# 1.2   Shape Collated Data ####

data_joint_1 <- data_joint_0 %>%
  select(- truck1.01, -pump.01, -solar.heater.01, -video.01, -cooker.01, -air.cooler.01, -cooler.01, -sewing.machine.01, -sewing_machine.01,
         - region, -ocu_hhh, -vacuum.01, -internet.access, -municipality, -clust, -printer.01, -density, -alphabetism, -freezer.01, -heater.01,
         - inc_capital_rents, -inc_deleted, -inc_other_income, -inc_labour, - language.b, - "inc_other income", -income_year, -iron.01,
         - gas_subsidy, -ely_subsidy, -ind_hhh_b, - lat_cen, -long_cen, -country_of_birth)%>%
  select(-lighting_fuel, -cooking_fuel, -heating_fuel, -water,-toilet,-edu_hhh, -ethnicity)
  # eventually add updated fuel codes

# write_rds(data_joint_1, "../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")  

carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_1, burden_decomposition_0)%>%
  mutate(burden_s_cooking_fuels   = exp_s_cooking_fuels  /hh_expenditures_USD_2014,
         burden_s_transport_fuels = exp_s_transport_fuels/hh_expenditures_USD_2014,
         burden_s_Goods           = exp_s_Goods          /hh_expenditures_USD_2014,
         burden_s_Services        = exp_s_Services       /hh_expenditures_USD_2014,
         burden_s_Food            = exp_s_Food           /hh_expenditures_USD_2014,
         burden_s_Electricity     = exp_s_Electricity    /hh_expenditures_USD_2014,
         burden_s_other_energy    = exp_s_other_energy   /hh_expenditures_USD_2014
  )%>%
  select(-starts_with("exp_s_"))
  

  

  
data_joint_0 <- data_joint_0 %>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014, everything())%>%
  mutate(hh_expenditures_USD_2014_pc     = hh_expenditures_USD_2014/hh_size,
         log_hh_expenditures_USD_2014    = log(hh_expenditures_USD_2014),
         log_hh_expenditures_USD_2014_pc = log(hh_expenditures_USD_2014_pc))%>%
  mutate(electricity.access = ifelse(Country == "Chile" & exp_USD_Electricity == 0,0,
                                     ifelse(Country == "Chile" & exp_USD_Electricity > 0,1,electricity.access)))%>%
  select(hh_id, hh_weights, hh_size, Country, hh_expenditures_USD_2014,
         urban_01, province, district, village, municipality,
         adults, children, age_hhh, sex_hhh, ind_hhh, ISCED,
         ethnicity, religion, language, 
         # cooking_fuel, lighting_fuel, heating_fuel, water, toilet, edu_hhh
         CF, LF, HF, WTR, TLT, electricity.access,
         starts_with("CO2"), starts_with("exp_"), starts_with("burden_"),
         starts_with("hh_exp"), starts_with("log_hh"), starts_with("Income_Group"), starts_with("share_"),
         starts_with("exp_USD_"), starts_with("inc_"), ends_with(".01"), everything())%>%
  select(-boiler.01, -iron.01, -pump.01, -solar.heater.01, -radio.01, -cooker.01, -vacuum.01, -video.01, -bicycle.01,-sewing.machine.01,
         -sewing_machine.01, -printer.01, -vaccum.01, - mobile.01, -stove.01a,
         -lighting_fuel, -heating_fuel, -cooking_fuel, -water, -toilet, edu_hhh)%>%
  mutate(share_other_binning = ifelse(is.na(share_other_binning),0, share_other_binning))%>%
  mutate(car.01          = ifelse(Country != "Chile" & is.na(car.01),0,car.01),
         refrigerator.01 = ifelse(Country != "Chile" & is.na(refrigerator.01),0,refrigerator.01),
         CF = ifelse(is.na(CF), "Unknown", CF),
         LF = ifelse(is.na(LF), "Unknown", LF),
         ISCED = ifelse(is.na(ISCED), 9, ISCED),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Guatemala","No Indica",Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Barbados", "Other",Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Costa Rica", "Otro(a)", Ethnicity),
         Ethnicity = ifelse(is.na(ethnicity) & Country == "Peru", "no sabe/no responde", Ethnicity))

if(!"exp_s_other_energy" %in% colnames(burden_decomposition_0)){
  burden_decomposition_0 <- burden_decomposition_0 %>%
    mutate(exp_s_other_energy = 0)
}

# 1.1   Screen Data ####

# Ethnicity    <- count(data_joint_0, Country, Ethnicity)       # Okay
# Refrigerator <- count(data_joint_0, Country, refrigerator.01) # Okay
# Car          <- count(data_joint_0, Country, car.01)          # Okay
# Urban        <- count(data_joint_0, Country, urban_01)        # Okay
# Cooking      <- count(data_joint_0, Country, CF)              # Okay

# 1.2   Several Summary Statistics ####

# General Summary Statistics

Summary_1.2 <- data.frame()

for(i in Country.Set){
  
  sum_1.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(number                   = n(),
              weights                  = sum(hh_weights),
              hh_size                  = wtd.mean(hh_size, weights = hh_weights),
              urban_01                 = wtd.mean(urban_01, weights = hh_weights),
              electricity.access       = wtd.mean(electricity.access, weights = hh_weights),
              hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              car.01                   = wtd.mean(car.01, weights = hh_weights))%>%
    mutate(urban_01                    = ifelse(i == "Argentina",1,urban_01))%>%
    mutate(Country = i)%>%
    select(Country, number, hh_size, urban_01, electricity.access, hh_expenditures_USD_2014, car.01)%>%
    mutate(hh_expenditures_USD_2014 = round(hh_expenditures_USD_2014,0),
           electricity.access = paste0(round(electricity.access*100,1),"%"),
           car.01   = ifelse(!is.na(car.01), paste0(round(car.01*100,0),"%"),""),
           urban_01 = ifelse(!is.na(urban_01), paste0(round(urban_01*100,0),"%"), ""),
           hh_size = round(hh_size,2))
  
  sum_1.2.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    mutate(Firewood_Biomass_Consumption = ifelse((!is.na(exp_USD_Firewood) & exp_USD_Firewood > 0 ) | (!is.na(exp_USD_Biomass) & exp_USD_Biomass > 0) | 
                                                   CF == "Biomass" | CF == "Charcoal" | CF == "Firewood" | CF == "Firewood Charcoal" | CF == "Firewood Kerosene" |
                                                   CF == "Firewood LPG" | CF == "Firewood LPG Charcoal" | CF == "Other Biomass",hh_weights,0))%>%
    mutate(sum_hh_weights = sum(hh_weights))%>%
    summarise(Firewood_Biomass_Consumption = sum(Firewood_Biomass_Consumption),
              sum_hh_weights               = sum(hh_weights))%>%
    mutate(share_Firewood = paste0(round((Firewood_Biomass_Consumption/sum_hh_weights)*100,0),"%"))%>%
    select(share_Firewood)
  
  sum_1.2 <- bind_cols(sum_1.2, sum_1.2.1)
  
  
  Summary_1.2 <- Summary_1.2 %>%
    bind_rows(sum_1.2)
}

colnames(Summary_1.2) <- c("Country", "Observations", "Average \nHousehold Size", "Urban \nPopulation", "Electricity \nAccess", "Average \nHousehold \nExpenditures [USD]", "Car \nOwnership", "Share of \nFirewood Cons.")

kbl(mutate_all(Summary_1.2,linebreak), format = "latex", linesep = "", booktabs = T,
    caption = "Summary Statistics", format.args = list(big.mark = ",", scientific = FALSE), align = "lrcccrcc")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  footnote(general = "This table provides summary statistics for households in our sample. All values (except observations) are household-weighted averages. The Argentinian sample comprises urban households only.", threeparttable = T)%>%
  column_spec(column = 1,    width = "3.5 cm", border_right = T)%>%
  column_spec(column =  2:8, width = "2.3 cm")%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A1/Table_Summary_A1.tex")

# Average Expenditure und Energy Expenditure Share Income Groups

Summary_1.3 <- data.frame()

for(i in Country.Set){
  
  sum_1.3.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))
  
  sum_1.3.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
              share_energy             = wtd.mean(share_energy, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("share_energy", "hh_expenditures_USD_2014"))
  
  sum_1.3.3 <- bind_cols(sum_1.3.1, sum_1.3.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("hh_expenditures_USD_2014"), starts_with("share_energy"))
  
  Summary_1.3 <- Summary_1.3 %>%
    bind_rows(sum_1.3.3)
  
}

Summary_1.3 <- Summary_1.3 %>%
  mutate_at(vars(starts_with("hh_expenditures_USD_2014")), list(~ round(.,0)))%>%
  mutate_at(vars(starts_with("share_energy")), list(~ paste0(round(.*100,1), "%")))

colnames(Summary_1.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(Summary_1.3, format = "latex", caption = "Average Expenditures and Average Expenditure Shares per Expenditure Quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.13 cm")%>%
  column_spec(8:13, width = "1.04 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average household expenditures [USD]" = 6, "Average energy expenditure shares" = 6))%>%
  footnote(general = "This table shows average household expenditures and average energy expenditure shares for households in 16 countries of Latin America and the Caribbean. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A2/Table_Summary_A2.tex")

# Footprint und Burden National

Summary_1.4 <- data.frame()

for(i in Country.Set){
  
  sum_1.4.1 <- data_joint_0 %>%
    filter(Country == i)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))
  
  sum_1.4.2 <- data_joint_0 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
              burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
    ungroup()%>%
    pivot_wider(names_from = "Income_Group_5", values_from = c("CO2_t_national", "burden_CO2_national"))
  
  sum_1.4.3 <- bind_cols(sum_1.4.1, sum_1.4.2)%>%
    mutate(Country = i)%>%
    select(Country, starts_with("CO2_t_national"), starts_with("burden_CO2_national"))
  
  Summary_1.4 <- Summary_1.4 %>%
    bind_rows(sum_1.4.3)
  
}

Summary_1.4 <- Summary_1.4 %>%
  mutate_at(vars(starts_with("CO2_t_national")), list(~ round(.,1)))%>%
  mutate_at(vars(starts_with("burden_CO2_national")), list(~ paste0(round(.*100,2), "%")))

colnames(Summary_1.4) <- c("Country", rep(c("All","EQ1","EQG2","EQ3","EQ4","EQ5"),2))

kbl(Summary_1.4, format = "latex", caption = "Average Carbon Footprint and Average USD/tCO$_{2}$ Carbon Price Incidence per Expenditure Quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.05 cm")%>%
  column_spec(8:13, width = "1.15 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average carbon footprint [tCO$_{2}$]" = 6, "Average incidence from USD 40/tCO$_{2}$ carbon price" = 6), escape = FALSE)%>%
  footnote(general = "This table shows average carbon footprints in tCO$_{2}$ and average levels of carbon price incidence for households in 16 countries of Latin America and the Caribbean. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_Summary_A3/Table_Summary_A3.tex")

# Electricity-Table

LAC_Electricity <- read_excel("../0_Data/9_Supplementary Data/LAC_Electricity.xlsx")

LAC_Electricity_2 <- LAC_Electricity %>%
  mutate_at(vars("Coal":"Other"),list(~ paste0(round(.*100,1), "\\%")))%>%
  rename("Cons. [TWh]" = "total Electricity Consumption in TWh (2020)", "Cons. pc. [MWh]" = "Electricity Consumption MWh / per capita (2020)")%>%
  rename_at(vars("Coal":"Cons. pc. [MWh]"), funs(str_replace(.,"^", "\\\\rotatebox{90}{")))%>%
  rename_at(vars(2:13), funs(str_replace(., "$","}")))

kbl(mutate_all(LAC_Electricity_2, linebreak), format = "latex", caption = "Electricity Generation in 16 Countries of Latin America and the Caribbean", 
    booktabs = T, align = "l|rrrrrrrrrr|r|r", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", escape = FALSE)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1,     width = "3.15 cm")%>%
  column_spec(2:10,  width = "1.3 cm")%>%
  column_spec(11:12, width = "1.4 cm")%>%
  add_header_above(c(" " = 1, "Share of Electricity Generation by Source in Percent (2020)" = 10, " " = 1, " " = 1))%>%
  footnote(general = "This table provides summary statistics for electricity generation in 16 different countries of Latin America and the Caribbean. It reports the share of electricity generated by each source in each country in 2020 [\\\\%] as well as the total annual electricity consumption [TWh] and per capita [Mwh]. Source: \\\\textcite{IEA.2021} and Our World in Data \\\\autocite{HannahRitchie.2020} for Barbados. Annual electricity consumption for Peru refers to 2019. ", threeparttable = T, escape = FALSE)%>%
  save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A7/Table_A7.tex")

# Supplement for Uruguay, Paraguay, Nicaragua and Guatemala

Summary_1.5 <- data_joint_0 %>%
  filter(Country == "Uruguay" | Country == "Paraguay" | Country == "Nicaragua" | Country == "Guatemala")%>%
  mutate(exp_USD_energy = hh_expenditures_USD_2014*share_energy)%>%
  filter(exp_USD_energy > 0)%>%
  mutate(share_Firewood = exp_USD_Firewood/exp_USD_energy,
         share_Electricity = exp_USD_Electricity/exp_USD_energy)%>%
  group_by(Country)%>%
  summarise(mean_share_Firewood    = mean(share_Firewood),
            mean_share_Electricity = mean(share_Electricity))%>%
  ungroup()
