# 0       General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1     Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "countrycode", "cowplot", "DALEXtra", "eulerr","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "marginaleffects", "margins", "Metrics",
       "openxlsx", "pdp","rattle", "scales", "SHAPforxgboost","tidymodels", "tidyverse", "vip", "xgboost", "xtable")

options(scipen=999,
        dplyr.summarise.inform = FALSE)

set.seed(2023)

GTAP_year <- 2017

# 1       Loading data ####

if(GTAP_year == 2014){
  data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")
}

if(GTAP_year == 2017){
  data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database_2017.rds")
}

# Codes

Cooking.Codes.Aggregate     <- read_csv("0_Data/9_Supplementary Information/2_Codes/Cooking.Code.All.csv", show_col_types = FALSE)
Lighting.Codes.Aggregate    <- read_csv("0_Data/9_Supplementary Information/2_Codes/Lighting.Code.All.csv", show_col_types = FALSE)
Heating.Codes.Aggregate     <- read_csv("0_Data/9_Supplementary Information/2_Codes/Heating.Code.All.csv", show_col_types = FALSE)
Water.Codes.Aggregate       <- read_csv("0_Data/9_Supplementary Information/2_Codes/Water.Code.All.csv", show_col_types = FALSE)
Toilet.Codes.Aggregate      <- read_csv("0_Data/9_Supplementary Information/2_Codes/Toilet.Code.All.csv", show_col_types = FALSE)
Province.Codes.Aggregate    <- read_csv("0_Data/9_Supplementary Information/2_Codes/Province.Code.All.csv", show_col_types = FALSE)
District.Codes.Aggregate    <- read_csv("0_Data/9_Supplementary Information/2_Codes/District.Code.All.csv", show_col_types = FALSE)
Nationality.Codes.Aggregate <- read_csv("0_Data/9_Supplementary Information/2_Codes/Nationality.Code.All.csv", show_col_types = FALSE)
Ethnicity.Codes.Aggregate   <- read_csv("0_Data/9_Supplementary Information/2_Codes/Ethnicity.Code.All.csv", show_col_types = FALSE)
Language.Codes.Aggregate    <- read_csv("0_Data/9_Supplementary Information/2_Codes/Language.Code.All.csv", show_col_types = FALSE)
Religion.Codes.Aggregate    <- read_csv("0_Data/9_Supplementary Information/2_Codes/Religion.Code.All.csv", show_col_types = FALSE)

# 1.1     Combining data ####

data_1 <- data_0 %>%
  left_join(Province.Codes.Aggregate,    by = c("Country", "Province_code"))%>%
  left_join(District.Codes.Aggregate,    by = c("Country", "District_code"))%>%
  left_join(Cooking.Codes.Aggregate,     by = "CF_code")%>%
  left_join(Heating.Codes.Aggregate,     by = "HF_code")%>%
  left_join(Lighting.Codes.Aggregate,    by = "LF_code")%>%
  left_join(Toilet.Codes.Aggregate,      by = "TLT_code")%>%
  left_join(Water.Codes.Aggregate,       by = "WTR_code")%>%
  left_join(Religion.Codes.Aggregate,    by = c("Country", "Religion_code"))%>%
  left_join(Nationality.Codes.Aggregate, by = c("Country", "Nationality_code"))%>%
  left_join(Ethnicity.Codes.Aggregate,   by = c("Country", "Ethnicity_code"))%>%
  left_join(Language.Codes.Aggregate,    by = c("Country", "Language_code"))%>%
  select(-province,-district,-religion,-ethnicity,-nationality,-language)
  
rm(Province.Codes.Aggregate, District.Codes.Aggregate, Cooking.Codes.Aggregate, Heating.Codes.Aggregate, Lighting.Codes.Aggregate, Language.Codes.Aggregate,
   Toilet.Codes.Aggregate, Water.Codes.Aggregate, Religion.Codes.Aggregate, Nationality.Codes.Aggregate, Ethnicity.Codes.Aggregate, data_0)

# 2.      Edit data ####

carbon.price <- 40

data_2 <- data_1 %>%
  mutate(exp_CO2_global                  = CO2_t_global*carbon.price,
         exp_CO2_national                = CO2_t_national*carbon.price,
         exp_CO2_electricity             = CO2_t_electricity*carbon.price,
         exp_CO2_transport               = CO2_t_transport*carbon.price)%>%
  mutate(hh_expenditures_USD_2014_pc     = hh_expenditures_USD_2014/hh_size,
         log_hh_expenditures_USD_2014    = log(hh_expenditures_USD_2014),
         log_hh_expenditures_USD_2014_pc = log(hh_expenditures_USD_2014_pc))%>%
  mutate(burden_CO2_global               = exp_CO2_global/     hh_expenditures_USD_2014,
         burden_CO2_national             = exp_CO2_national/   hh_expenditures_USD_2014,
         burden_CO2_electricity          = exp_CO2_electricity/hh_expenditures_USD_2014,
         burden_CO2_transport            = exp_CO2_transport/  hh_expenditures_USD_2014)%>%
  mutate(burden_s_cooking_fuels          = exp_s_cooking_fuels  /hh_expenditures_USD_2014,
         burden_s_transport_fuels        = exp_s_transport_fuels/hh_expenditures_USD_2014,
         burden_s_Goods                  = exp_s_Goods          /hh_expenditures_USD_2014,
         burden_s_Services               = exp_s_Services       /hh_expenditures_USD_2014,
         burden_s_Food                   = exp_s_Food           /hh_expenditures_USD_2014,
         burden_s_Electricity            = exp_s_Electricity    /hh_expenditures_USD_2014,
         burden_s_other_energy           = exp_s_other_energy   /hh_expenditures_USD_2014)%>%
  select(-starts_with("exp_s_"))%>%
  mutate(carbon_intensity_kg_per_USD_national = CO2_t_national*1000/hh_expenditures_USD_2014)

rm(data_1)

# _______ ####
# 3.      Summary statistics ####

Country.Set <- distinct(data_2, Country)%>%
  mutate(Country_long = countrycode(Country, origin = "iso3c", destination = "country.name"))%>%
  left_join(ungroup(summarise(group_by(data_2, Country), hh_expenditures_USD_2014_mean = wtd.mean(hh_expenditures_USD_2014),
                              sample = n())))%>%
  arrange(hh_expenditures_USD_2014_mean)%>%
  mutate(Group_Income = c(rep("A", 20),
                          rep("B", 20),
                          rep("C", 20),
                          rep("D", nrow(.)-60)))

# 3.1     General summary statistics ####

sum_3.1.1 <- data_2 %>%
  group_by(Country)%>%
  summarise(number                   = n(),
            weights               = sum(hh_weights),
            hh_size                  = wtd.mean(hh_size,                  weights = hh_weights),
            urban_01                 = wtd.mean(urban_01,                 weights = hh_weights),
            electricity.access       = wtd.mean(electricity.access,       weights = hh_weights),
            hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
            car.01                   = wtd.mean(car.01,                   weights = hh_weights))%>%
  ungroup()%>%
  select(Country, number, hh_size, urban_01, electricity.access, hh_expenditures_USD_2014, car.01)%>%
  mutate(hh_expenditures_USD_2014 = round(hh_expenditures_USD_2014,0),
         electricity.access       = ifelse(!is.na(electricity.access), paste0(round(electricity.access*100,1),"%"), ""),
         car.01                   = ifelse(!is.na(car.01), paste0(round(car.01*100,0),"%"),""),
         urban_01                 = ifelse(!is.na(urban_01), paste0(round(urban_01*100,0),"%"), ""),
         hh_size                  = round(hh_size,2))
  
sum_3.1.2 <- data_2 %>%
  mutate(Firewood_Biomass_Consumption = ifelse((!is.na(exp_USD_Firewood) & exp_USD_Firewood > 0 ) | (!is.na(exp_USD_Biomass) & exp_USD_Biomass > 0) | (!is.na(exp_USD_Charcoal) & exp_USD_Charcoal > 0) |
                                                 !is.na(CF) & (CF == "Other biomass" | CF == "Charcoal" | CF == "Firewood"), hh_weights, 0))%>%
  group_by(Country)%>%
  summarise(Firewood_Biomass_Consumption = sum(Firewood_Biomass_Consumption),
            sum_hh_weights               = sum(hh_weights))%>%
  ungroup()%>%
  mutate(share_Firewood = paste0(round((Firewood_Biomass_Consumption/sum_hh_weights)*100,0),"%"))%>%
  select(Country, share_Firewood)

sum_3.1.3 <- left_join(sum_3.1.1, sum_3.1.2, by = "Country")%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)
  
colnames(sum_3.1.3) <- c("Country", "Observations", "Average \nHousehold Size", "Urban \nPopulation", "Electricity \nAccess", "Average \nHousehold \nExpenditures [USD]", "Car \nOwnership", "Share of \nFirewood or \n Charcoal Cons.")

kbl(mutate_all(sum_3.1.3,linebreak), format = "latex", linesep = "", booktabs = T, longtable = T,
    caption = "Summary statistics", format.args = list(big.mark = ",", scientific = FALSE), align = "lrcccccc", label = "A1")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 9)%>%
  footnote(general = "This table provides summary statistics for households in our sample. All values (except observations) are household-weighted averages.", threeparttable = T)%>%
  column_spec(column = 1,    width = "1.5 cm", border_right = T)%>%
  column_spec(column =  2:8, width = "1.5 cm")%>%
  save_kable(., "2_Tables/Table_Summary_A1.tex")

rm(sum_3.1.1, sum_3.1.2, sum_3.1.3)

# 3.2     Average expenditure and energy expenditure share income groups ####

sum_3.2.1 <- data_2 %>%
  group_by(Country)%>%
  summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
            share_energy             = wtd.mean(share_energy, weights = hh_weights))%>%
  ungroup()

sum_3.2.2 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, weights = hh_weights),
            share_energy             = wtd.mean(share_energy, weights = hh_weights))%>%
  ungroup()%>%
  pivot_wider(names_from = "Income_Group_5", values_from = c("share_energy", "hh_expenditures_USD_2014"))

sum_3.2.3 <- left_join(sum_3.2.1, sum_3.2.2, by = "Country")%>%
  select(Country, starts_with("hh_expenditures_USD_2014"), starts_with("share_energy"))%>%
  mutate_at(vars(starts_with("hh_expenditures_USD_2014")), list(~ round(.,0)))%>%
  mutate_at(vars(starts_with("share_energy")), list(~ paste0(round(.*100,0), "%")))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(sum_3.2.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(sum_3.2.3, format = "latex", caption = "Average household expenditures and average energy expenditure shares per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T, label = "A2")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 7)%>%
  column_spec(1, width = "3.12 cm")%>%
  column_spec(2:7, width = "1.08 cm")%>%
  column_spec(8:13, width = "0.9 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average household expenditures [USD]" = 6, "Average energy expenditure shares" = 6))%>%
  footnote(general = "This table shows average household expenditures and average energy expenditure shares for households in our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Summary_A2.tex")

rm(sum_3.2.1, sum_3.2.2, sum_3.2.3)

# 3.3     Carbon footprint and burden national ####

sum_3.3.1 <- data_2 %>%
  group_by(Country)%>%
  summarise(CO2_t_national                       = wtd.mean(CO2_t_national,      weights = hh_weights),
            carbon_intensity_kg_per_USD_national = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()

sum_3.3.2 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
            carbon_intensity_kg_per_USD_national = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()%>%
  pivot_wider(names_from = "Income_Group_5", values_from = c("CO2_t_national", "carbon_intensity_kg_per_USD_national"))

sum_3.3.3 <- left_join(sum_3.3.1, sum_3.3.2, by = "Country")%>%
  select(Country, starts_with("CO2_t_national"), starts_with("carbon_intensity_kg_per_USD_national"))%>%
  mutate_at(vars(starts_with("CO2_t_national")), list(~ round(.,1)))%>%
  mutate_at(vars(starts_with("carbon_intensity_kg_per_USD_national")), list(~ round(.,2)))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(sum_3.3.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(sum_3.3.3, format = "latex", caption = "Average carbon footprint and average carbon intensity per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", linesep = "",
    longtable = T, label = "A3")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 9)%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.05 cm")%>%
  column_spec(8:13, width = "1.15 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average carbon footprint [tCO$_{2}$]" = 6, "Average carbon intensity [kgCO$_{2}$/USD]" = 6), escape = FALSE)%>%
  footnote(general = "This table shows average carbon footprints in tCO$_{2}$ and average carbon intensity for households in all countries of our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Summary_A3.tex")

rm(sum_3.3.1, sum_3.3.2, sum_3.3.3)

# 3.4     Cooking and lighting fuels ####

sum_3.4.1 <- data_2 %>%
  mutate(CF_agg = ifelse(CF == "Charcoal" | CF == "Coal" | CF == "Firewood" | CF == "Other biomass", "Solid fuels",
                         ifelse(CF == "LPG" | CF == "Gas" | CF == "Kerosene" | CF == "Liquid fuel", "Liquid and gaseous fuels",
                                ifelse(CF == "Electricity", "Electricity", "Unknown"))))%>%
  mutate(CF_agg = ifelse(is.na(CF_agg), "Unknown", CF_agg))%>%
  group_by(Country, Income_Group_5)%>%
  mutate(hhs = sum(hh_weights))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5, CF_agg)%>%
  summarise(CF_agg_hhs = sum(hh_weights),
            hhs        = first(hhs))%>%
  ungroup()%>%
  mutate(share = CF_agg_hhs/hhs)%>%
  select(Country, Income_Group_5, CF_agg, share)%>%
  filter(CF_agg != "Unknown")%>%
  unite(IG_CF_agg, c(CF_agg, Income_Group_5))%>%
  pivot_wider(names_from = "IG_CF_agg", values_from = "share")%>%
  select(Country, starts_with("Solid"), starts_with("Liquid"), starts_with("Electricity"))%>%
  mutate_at(vars(-Country), list(~ paste0(round(.*100,0), "%")))%>%
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(sum_3.4.1) <- c("Country", rep(c("EQ1","EQG2","EQ3","EQ4","EQ5"),3))

kbl(sum_3.4.1, format = "latex", caption = "Share of households using cooking fuels", booktabs = T, align = "l|rrrrr|rrrrr|rrrrr", vline = "", linesep = "", label = "A4_CF")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:16, width = "1.00 cm")%>%
  add_header_above(c(" " = 1, "Expenditure quintile" = 5,"Expenditure quintile" = 5, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Solid fuels" = 5, "Liquid or gaseous fuels" = 5, "Electricity" = 5), escape = FALSE)%>%
  footnote(general = "This table shows the share of households using different cooking fuels, such as solid fuels (e.g., firewood, charcoal, coal, biomass), liquid fuels (e.g., LPG, natural gas, kerosene), or electricity over expenditure quintiles.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Summary_A4_Cooking_Fuels.tex")  

sum_3.4.2 <- data_2 %>%
  mutate(LF_agg = ifelse(LF == "Electricity", "Electricity", 
                         ifelse(LF == "Kerosene", "Kerosene",
                                ifelse(LF %in% c("Firewood", "Gas", "Liquid fuel", "LPG", "Other biomass", "Other lighting"), "Other lighting", "Unknown"))))%>%
  mutate(LF_agg = ifelse(is.na(LF_agg), "Unknown", LF_agg))%>%
  group_by(Country, Income_Group_5)%>%
  mutate(hhs = sum(hh_weights))%>%
  ungroup()%>%
  group_by(Country, Income_Group_5, LF_agg)%>%
  summarise(LF_agg_hhs = sum(hh_weights),
            hhs        = first(hhs))%>%
  ungroup()%>%
  mutate(share = LF_agg_hhs/hhs)%>%
  select(Country, Income_Group_5, LF_agg, share)%>%
  filter(LF_agg != "Unknown")%>%
  unite(IG_LF_agg, c(LF_agg, Income_Group_5))%>%
  pivot_wider(names_from = "IG_LF_agg", values_from = "share")%>%
  select(Country, starts_with("Kerosene"), starts_with("Electricity"), starts_with("Other lighting"))%>%
  mutate_at(vars(-Country), list(~ paste0(round(.*100,0), "%")))%>%
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(sum_3.4.2) <- c("Country", rep(c("EQ1","EQ2","EQ3","EQ4","EQ5"),3))

kbl(sum_3.4.2, format = "latex", caption = "Share of households using lighting fuels", booktabs = T, align = "l|rrrrr|rrrrr|rrrrr", vline = "", linesep = "", label = "A5_LF")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:16, width = "1.00 cm")%>%
  add_header_above(c(" " = 1, "Expenditure quintile" = 5,"Expenditure quintile" = 5, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Kerosene" = 5, "Electricity" = 5, "Other lighting fuels" = 5), escape = FALSE)%>%
  footnote(general = "This table shows the share of households using different lighting fuels over expenditure quintiles.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Summary_A4_Lighting_Fuels.tex")  

rm(sum_3.4.1, sum_3.4.2)

# 3.5     Appliances ####

sum_3.5.1 <- data_2 %>%
  select(hh_id, Country, hh_weights, tv.01, ac.01, car.01, washing_machine.01, refrigerator.01, Income_Group_5)

sum_3.5.2 <- sum_3.5.1 %>%
  mutate(Income_Group_5 = "All")%>%
  bind_rows(filter(mutate(sum_3.5.1, Income_Group_5 = as.character(Income_Group_5)), Income_Group_5 == "1" | Income_Group_5 == "5"))%>%
  mutate_at(vars(ends_with(".01")), list(~ ifelse(. == 1, hh_weights,0)))%>%
  group_by(Country, Income_Group_5)%>%
  mutate(hhs = sum(hh_weights))%>%
  ungroup()%>%
  filter(!is.na(tv.01) | !is.na(car.01))%>%
  group_by(Country, Income_Group_5)%>%
  summarise(tv.01              = sum(tv.01),
            refrigerator.01    = sum(refrigerator.01),
            washing_machine.01 = sum(washing_machine.01),
            car.01             = sum(car.01),
            ac.01              = sum(ac.01),
            hhs                = first(hhs))%>%
  ungroup()%>%
  mutate_at(vars(ends_with(".01")), list(~ ./hhs))%>%
  select(-hhs)%>%
  pivot_longer(ends_with(".01"), names_to = "appliance", values_to = "share")%>%
  unite(appliance_IG, c("appliance", "Income_Group_5"))%>%
  pivot_wider(names_from = "appliance_IG", values_from = "share")%>%
  select(Country, ends_with("_All"), everything())%>%
  select(Country, starts_with("car.01"), starts_with("tv.01"), starts_with("refrigerator.01"), starts_with("ac.01"), starts_with("washing"))%>%
  mutate_at(vars(-Country), list(~ paste0(round(.*100,0), "%")))%>%
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(sum_3.5.2) <- c("Country", rep(c("All","EQ1","EQ5"),5))

kbl(sum_3.5.2, format = "latex", caption = "Share of households possessing different assets", booktabs = T, align = "l|rrr|rrr|rrr|rrr|rrr", vline = "", linesep = "", label = "A6")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:16, width = "1.00 cm")%>%
  add_header_above(c(" " = 1,  "Car" = 3, "TV" = 3, "Refrigerator" = 3, "AC" = 3, "Washing machine" = 3), escape = FALSE)%>%
  footnote(general = "This table shows the share of households possessing differents assets for all households (first and fifth expenditure quintile, respectively) in different countries.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Summary_A5_Appliances.tex")  

rm(sum_3.5.1, sum_3.5.2)

# 3.6     Electricity-table (TBA) ####

# LAC_Electricity <- read_excel("../0_Data/9_Supplementary Data/LAC_Electricity.xlsx")
# 
# LAC_Electricity_2 <- LAC_Electricity %>%
#   mutate_at(vars("Coal":"Other"),list(~ paste0(round(.*100,1), "\\%")))%>%
#   rename("Cons. [TWh]" = "total Electricity Consumption in TWh (2020)", "Cons. pc. [MWh]" = "Electricity Consumption MWh / per capita (2020)")%>%
#   rename_at(vars("Coal":"Cons. pc. [MWh]"), funs(str_replace(.,"^", "\\\\rotatebox{90}{")))%>%
#   rename_at(vars(2:13), funs(str_replace(., "$","}")))
# 
# kbl(mutate_all(LAC_Electricity_2, linebreak), format = "latex", caption = "Electricity Generation in 16 Countries of Latin America and the Caribbean", 
#     booktabs = T, align = "l|rrrrrrrrrr|r|r", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", escape = FALSE)%>%
#   kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
#   column_spec(1,     width = "3.15 cm")%>%
#   column_spec(2:10,  width = "1.3 cm")%>%
#   column_spec(11:12, width = "1.4 cm")%>%
#   add_header_above(c(" " = 1, "Share of Electricity Generation by Source in Percent (2020)" = 10, " " = 1, " " = 1))%>%
#   footnote(general = "This table provides summary statistics for electricity generation in 16 different countries of Latin America and the Caribbean. It reports the share of electricity generated by each source in each country in 2020 [\\\\%] as well as the total annual electricity consumption [TWh] and per capita [Mwh]. Source: \\\\textcite{IEA.2021} and Our World in Data \\\\autocite{HannahRitchie.2020} for Barbados. Annual electricity consumption for Peru refers to 2019. ", threeparttable = T, escape = FALSE)%>%
#   save_kable(., "../1_Carbon_Pricing_Incidence/3_Analyses/1_LAC_2021/6_App/Latin-America-Paper/Tables/Table_A7/Table_A7.tex")

# 3.7     Missing information ####

Obs_1 <- data_2 %>%
  select(everything())%>%
  group_by(Country)%>%
  summarise(Obs = n())%>%
  ungroup()

NAs_over_obs_1 <- data_2 %>%
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
  select(Country, Type_B, share)%>%
  #mutate(share = percent(share, accuracy = 1))%>%
  pivot_wider(names_from = "Type_B", values_from = "share")%>%
  left_join(Obs_1, by = "Country")%>%
  select(Country, Obs, everything())

write.xlsx(NAs_over_obs_1, "0_Data/9_Supplementary Information/NAs_final_dataset.xlsx")

# Missing information even though codes exist

codes_1 <- data.frame()

for (i in Country.Set$Country_long){
  path_0                     <- list.files("../0_Data/1_Household Data/")[grep(i, list.files("../0_Data/1_Household Data/"), ignore.case = T)][1]
  
  if(is.na(path_0)) print(i)
  
  codes_0 <- data.frame("Code_0" = list.files(sprintf("../0_Data/1_Household Data/%s/2_Codes", path_0)))%>%
    mutate(Country = Country.Set$Country[Country.Set$Country_long == i])%>%
    filter(str_detect(Code_0, ".csv"))%>%
    mutate(value = "Yes")%>%
    pivot_wider(names_from = "Code_0", values_from = "value")
  
  codes_1 <- bind_rows(codes_1, codes_0)
    
}

codes_1 <- codes_1 %>%
  bind_rows(data.frame(Country = "USA", District.Code.csv = "Yes", Education.Code.csv = "Yes", Ethnicity.Code.csv = "Yes", Gender.Code.csv = "Yes", Industry.Code.csv = "Yes", Province.Code.csv = "Yes"))%>%
  bind_rows(data.frame(Country = "CIV", Cooking.Code.csv = "Yes", District.Code.csv = "Yes", Education.Code.csv = "Yes", Ethnicity.Code.csv = "Yes", Gender.Code.csv = "Yes", Industry.Code.csv = "Yes", Lighting.Code.csv = "Yes", Nationality.Code.csv = "Yes", Province.Code.csv = "Yes", Religion.Code.csv = "Yes", Water.Code.csv = "Yes", Toilet.Code.csv = "Yes"))%>%
  bind_rows(data.frame(Country = "MMR", Cooking.Code.csv = "Yes", District.Code.csv = "Yes", Education.Code.csv = "Yes", Gender.Code.csv = "Yes", Industry.Code.csv = "Yes", Language.Code.csv = "Yes", Lighting.Code.csv = "Yes", Province.Code.csv = "Yes", Religion.Code.csv = "Yes", Water.Code.csv = "Yes", Toilet.Code.csv = "Yes", Village.Code.csv = "Yes"))


NAs_over_obs_2 <- NAs_over_obs_1 %>%
  select(Country, village, urban_01, ind_hhh, ISCED, Province, District, CF, HF, LF, TLT, WTR, Religion, Nationality, Ethnicity, Language)%>%
  pivot_longer(-Country, names_to = "Variable", values_to = "Missing_share")%>%
  filter(Missing_share != 0)%>%
  left_join(mutate_at(codes_1, vars(-Country), ~ ifelse(is.na(.),"No",.)))%>%
  mutate_at(vars(District.Code.csv, Education.Code.csv, Gender.Code.csv, Industry.Code.csv), list(~ ifelse(is.na(.), "Yes",.)))%>%
  mutate(Error = ifelse(Variable == "village" & Village.Code.csv == "Yes", "Village + ", ""))%>%
  mutate(Error = ifelse(Variable == "Province" & Province.Code.csv == "Yes", paste0("Province + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "District" & District.Code.csv == "Yes", paste0("District + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "ind_hhh"  & Industry.Code.csv == "Yes", paste0("Industry + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "ISCED"    & Education.Code.csv == "Yes", paste0("Education + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "CF" & Cooking.Code.csv == "Yes", paste0("Cooking + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "HF" & Heating.Code.csv == "Yes", paste0("Heating + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "LF" & Lighting.Code.csv == "Yes", paste0("Lighting + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "TLT" & Toilet.Code.csv == "Yes", paste0("Toilet + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "WTR" & Water.Code.csv == "Yes", paste0("Water + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "Religion" & Religion.Code.csv == "Yes", paste0("Religion + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "Nationality" & Nationality.Code.csv == "Yes", paste0("Nationality + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "Ethnicity"   & Ethnicity.Code.csv == "Yes", paste0("Ethnicity + ", Error), Error))%>%
  mutate(Error = ifelse(Variable == "Language"    & Language.Code.csv == "Yes", paste0("Language + ", Error), Error))%>%
  filter(!is.na(Error) & Error != "")%>%
  arrange(Error)

write.xlsx(NAs_over_obs_2, "0_Data/9_Supplementary Information/NAs_codes_matching_final_dataset.xlsx")

# _______ ####
# 4.      Descriptive analysis ####

# 4.1     Correlational analyses ####

# 4.2     k-Means clustering ####

dir.create("1_Figures/Analysis_k_means", showWarnings = FALSE)

data_4.2.0 <- data.frame()
data_4.2.00 <- data.frame()

for(i in Country.Set$Country){
  
  start <- Sys.time()
  
  data_4.2 <- data_2 %>%
    filter(Country == i)%>%
    select(hh_id, burden_CO2_national, hh_expenditures_USD_2014, hh_weights)%>%
    mutate(mean_burden_CO2_national        = wtd.mean(burden_CO2_national,          weights = hh_weights),
           sd_burden_CO2_national          = sqrt(wtd.var(burden_CO2_national,      weights = hh_weights)),
           mean_hh_expenditures_USD_2014   = wtd.mean(hh_expenditures_USD_2014,     weights = hh_weights),
           sd_hh_expenditures_USD_2014     = sqrt(wtd.var(hh_expenditures_USD_2014, weights = hh_weights)))%>%
    mutate(scaled_burden_CO2_national      = (burden_CO2_national - mean_burden_CO2_national)/sd_burden_CO2_national,
           scaled_hh_expenditures_USD_2014 = (hh_expenditures_USD_2014 - mean_hh_expenditures_USD_2014)/sd_hh_expenditures_USD_2014)%>%
    select(hh_id, scaled_burden_CO2_national, scaled_hh_expenditures_USD_2014, everything())

  # removing few severe outliers
  if(i  %in% c("ARM", "BGR", "ETH","PRY", "IRQ", "ROU")){
    data_4.2 <- data_4.2 %>%
      filter(scaled_burden_CO2_national < 10)
  }
  
  model <- kmeans(select(data_4.2, scaled_burden_CO2_national, scaled_hh_expenditures_USD_2014), centers = 10)

  gc()
# test <- map_dbl(1:10, function(k){
#   model <- kmeans(select(data_4.2, scaled_burden_CO2_national, scaled_hh_expenditures_USD_2014), centers = k)
#   model$tot.withinss
#   
# })
# 
# elbow_df <- data.frame(k = 1:10,
#                        tot_withinss = test)
# 
# ggplot(elbow_df, aes(x = k, y = tot_withinss))+
#   geom_line()+
#   scale_x_continuous(breaks = 1:20)

# dist_matrix <- dist(select(data_4.2, scaled_burden_CO2_national, scaled_hh_expenditures_USD_2014), method = "euclidean")
# hc_test     <- hclust(dist_matrix, method = "complete")
# assign      <- cutree(hc_test, k = 100)

  data_4.2$cluster_kmeans_10 <- model$cluster
  
  data_4.2.001 <- select(data_4.2, hh_id, cluster_kmeans_10)%>%
    mutate(Country = i)
  
  data_4.2.1 <- data_4.2 %>%
    # mutate(cluster = assign)%>%
    mutate(kmeans_cluster = model$cluster)%>%
    group_by(kmeans_cluster)%>%
    summarise(hh_expenditures_USD_2014 = wtd.mean(hh_expenditures_USD_2014, hh_weights),
              burden_CO2_national      = wtd.mean(burden_CO2_national, hh_weights),
              pop                      = sum(hh_weights))%>%
    ungroup()%>%
    mutate(Country = i)

  data_4.2.0  <- bind_rows(data_4.2.0, data_4.2.1)
  data_4.2.00 <- bind_rows(data_4.2.00, data_4.2.001)
  
  print(i)
  
  end <- Sys.time()
  rm(data_4.2, data_4.2.1)
  print(end-start)
}

data_2 <- data_2 %>%
  left_join(data_4.2.00)

for(i in Country.Set$Country){
  data_4.2.1 <- data_4.2.0 %>%
    filter(Country == i)
  
  data_4.2.2 <- data_2 %>%
    filter(Country == i)%>%
    filter(!is.na(cluster_kmeans_10))
  
  upper_bound   <- plyr::round_any(max(data_4.2.1$burden_CO2_national), 0.05, f = ceiling)
  righter_bound <- plyr::round_any(max(data_4.2.1$hh_expenditures_USD_2014), 5000, f = ceiling)
  
  if(righter_bound == 0) righter_bound <- 10000
  
  P_4.2.2 <- ggplot(data = filter(data_4.2.1, Country == i), aes(x = hh_expenditures_USD_2014, y = burden_CO2_national))+
    geom_point(data = data_4.2.2, aes(fill = factor(cluster_kmeans_10)), alpha = 0.05, shape = 21, colour = "black")+
    geom_point(aes(fill = factor(kmeans_cluster), size = pop), shape = 22, colour = "black")+
    #scale_size_continuous(range = c(1,5))+
    theme_bw()+
    xlab("Total household expenditures")+ ylab("Carbon pricing incidence")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,upper_bound), expand = c(0,0))+
    scale_x_continuous(limits = c(0, righter_bound), labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
    scale_size_continuous(range = c(1,5), guide = "none")+
    scale_fill_discrete(guide = "none")+
    ggtitle(i)+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_k_means/Exp_CPI_%s.jpg", i), width = 15.5, height = 15, unit = "cm", res = 200)
  print(P_4.2.2)
  dev.off()
  
  print(i)
  
  rm(data_4.2.1, data_4.2.2)
}

data_4.2.3 <- data_4.2.0 %>%
  arrange(Country, burden_CO2_national)%>%
  group_by(Country)%>%
  mutate(kmeans_cluster = 1:n())%>%
  ungroup()

P_4.2.2 <- ggplot(data = data_4.2.3, aes(x = hh_expenditures_USD_2014, y = burden_CO2_national))+
  geom_point(aes(fill = factor(kmeans_cluster), size = pop), shape = 22, colour = "black")+
  #scale_size_continuous(range = c(1,5))+
  theme_bw()+
  xlab("Total household expenditures")+ ylab("Carbon pricing incidence")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.15), expand = c(0,0))+
  scale_x_continuous(limits = c(0, 80000), labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_size_continuous(range = c(1,5), guide = "none")+
  scale_fill_discrete(guide = "none")+
  #ggtitle(i)+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Analysis_k_means/Exp_CPI_All.jpg", width = 15.5, height = 15, unit = "cm", res = 200)
print(P_4.2.2)
dev.off()

rm(data_4.2.3, data_4.2.0, data_4.2.00, data_4.2.001, P_4.2.1, P_4.2.2)

# 4.3     Engel-curves ####

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
          stats.title = "\\midrule", model.format = "",
          fontsize = "small")

dict_latex <- c(hh_expenditures_USD_2014 = "HH Exp.", "log(hh_size)" = "HH size (log)", share_energy = "Energy",
                share_food = "Food", share_goods = "Goods", share_services = "Service")

# 4.3.1   Expenditure shares ####

# 4.3.1.1 Parametric Engel Curves - Tables #### 

dir.create("2_Tables/1_Parametric_Engel_Curves", showWarnings = FALSE)

for (i in Country.Set$Country){
  data_4.3.1.1 <- data_2 %>%
    filter(Country == i)
  
  model_1 <- feols(c(share_energy, share_food, share_goods, share_services) ~ hh_expenditures_USD_2014 + hh_expenditures_USD_2014^2 + log(hh_size),
        data = data_4.3.1.1, weights = data_4.3.1.1$hh_weights, vcov = "hetero")
  
  etable(model_1, dict = dict_latex, tex = TRUE, 
         file = sprintf("2_Tables/1_Parametric_Engel_Curves/Table_Parametric_Engel_Curve_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "r2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("Engel curve: Coefficients from OLS-Regression for %s", i),  
         label = sprintf("tab:Engel_parametric_%s",i), 
         adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
         placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}"),
         headers = "Expenditure share on")
}

rm(data_4.3.1.1, model_1)

# 4.3.1.2 Parametric Engel Curves - Figures ####

dir.create("1_Figures/Analysis_Parametric_Engel_Curves", showWarnings = FALSE)

for(i in Country.Set$Country){
  data_4.3.1.2 <- data_2 %>%
    filter(Country == i)%>%
    select(hh_id, hh_weights, Country, starts_with("share_"), hh_expenditures_USD_2014_pc)%>%
    select(-share_other_binning)%>%
    rename(share_Energy = share_energy, share_Food = share_food, share_Services = share_services, share_Goods = share_goods)%>%
    pivot_longer(starts_with("share"), names_to = "Type", values_to = "Share", names_prefix = "share_")
  
  data_4.3.1.2.1 <- data_2 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014, hh_weights))%>%
    ungroup()
  
  upper_bound   <- plyr::round_any(max(data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc), 10000, f = ceiling)
  
  P_4.3.1.2 <- ggplot()+
    geom_vline(xintercept = data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc[1])+
    geom_vline(xintercept = data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc[2])+
    geom_vline(xintercept = data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc[3])+
    geom_vline(xintercept = data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc[4])+
    geom_vline(xintercept = data_4.3.1.2.1$mean_hh_expenditures_USD_2014_pc[5])+
    stat_smooth(data = data_4.3.1.2, 
                aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                    y = Share, fill = Type, colour = Type),
                    level = 0.99, method = "lm", formula = y ~ x + I(x^2) + I(x^3), fullrange = TRUE)+
    theme_bw()+
    xlab("Household expenditures per capita in US-$ (2014)") + ylab("Share of total expenditures")+
    scale_fill_npg()+
    scale_colour_npg()+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,0.65))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
    ggtitle(Country.Set$Country_long[Country.Set$Country == i])+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_Parametric_Engel_Curves/Parametric_EC_%s.jpg", i), width = 15.5, height = 15, unit = "cm", res = 200)
  print(P_4.3.1.2)
  dev.off()
  
  print(i)
  
  rm(data_4.3.1.2, data_4.3.1.2.1, P_4.3.1.2)
  
}

# For Publication

data_4.3.1.2.2 <- data_2 %>%
  left_join(Country.Set)%>%
  mutate(Country_long = fct_reorder(Country_long,hh_expenditures_USD_2014_mean, min))%>%
  select(hh_id, hh_weights, Country, starts_with("share_"), hh_expenditures_USD_2014_pc, Group_Income, Country_long)%>%
  select(-share_other_binning)%>%
  rename(share_Energy = share_energy, share_Food = share_food, share_Services = share_services, share_Goods = share_goods)%>%
  pivot_longer(starts_with("share"), names_to = "Type", values_to = "Share", names_prefix = "share_")

data_4.3.1.2.3 <- data_2 %>%
  left_join(Country.Set)%>%
  mutate(Country_long = fct_reorder(Country_long,hh_expenditures_USD_2014_mean, min))%>%
  group_by(Country_long, Income_Group_5, Group_Income)%>%
  summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014_pc, hh_weights))%>%
  ungroup()

for(Group_0 in c("A", "B", "C", "D")){
  
  if(Group_0 == "A") upper_bound <- 7000 else if(Group_0 == "B") upper_bound <- 12000 else if(Group_0 == "C") upper_bound <- 27000 else upper_bound <- 74000
  
  breaks <- plyr::round_any(seq(0,upper_bound, length.out = 3), 5000, f = floor)
  
  data_4.3.1.2.4 <- data_4.3.1.2.2 %>%
    filter(Group_Income == Group_0)
  
  data_4.3.1.2.5 <- data_4.3.1.2.3 %>%
    filter(Group_Income == Group_0)
  
  P_4.3.1.3 <- ggplot()+
    #geom_point(data = filter(data_4.3.1.2.4, Type == "Energy"), aes(x = hh_expenditures_USD_2014_pc,
    #                                    y = Share), 
    #           alpha = 0.01, shape = 21, colour = "black", fill = "#E64B35FF", size = 0.3)+
    geom_vline(data = data_4.3.1.2.5, aes(xintercept = mean_hh_expenditures_USD_2014_pc), size = 0.5)+
    stat_smooth(data = data_4.3.1.2.4, 
                aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                    y = Share, fill = Type, colour = Type),
                level = 0.95, method = "lm", formula = y ~ x + I(x^2), fullrange = TRUE, size = 0.5)+
    theme_bw()+
    facet_wrap(. ~ Country_long, ncol = 4)+
    xlab("Household expenditures per capita in US-$ (2014)") + ylab("Share of total expenditures")+
    scale_fill_npg()+
    scale_colour_npg()+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,0.8))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0),
                       breaks = breaks)+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
 #P_4.3.1.4 <- ggplot()+
 #  geom_vline(data = data_4.3.1.2.5, aes(xintercept = mean_hh_expenditures_USD_2014_pc))+
 #  #geom_vline(xintercept = data_4.3.1.2.3$mean_hh_expenditures_USD_2014_pc[1])+
 #  # geom_vline(xintercept = data_4.3.1.2.3$mean_hh_expenditures_USD_2014_pc[2])+
 #  # geom_vline(xintercept = data_4.3.1.2.3$mean_hh_expenditures_USD_2014_pc[3])+
 #  # geom_vline(xintercept = data_4.3.1.2.3$mean_hh_expenditures_USD_2014_pc[4])+
 #  # geom_vline(xintercept = data_4.3.1.2.3$mean_hh_expenditures_USD_2014_pc[5])+
 #  stat_smooth(data = data_4.3.1.2.4, 
 #              aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
 #                  y = Share, fill = Type, colour = Type),
 #              level = 0.95, method = "loess", formula = y ~ x, fullrange = TRUE)+
 #  theme_bw()+
 #  facet_wrap(. ~ Country_long, ncol = 4)+
 #  xlab("Household expenditures per capita in US-$ (2014)") + ylab("Share of total expenditures")+
 #  scale_fill_npg()+
 #  scale_colour_npg()+
 #  labs(colour = "", fill = "")+
 #  coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,0.8))+
 #  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
 #  scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0),
 #                     breaks = breaks)+
 #  ggtitle("")+
 #  theme(axis.text.y = element_text(size = 7), 
 #        axis.text.x = element_text(size = 7),
 #        axis.title  = element_text(size = 7),
 #        plot.title = element_text(size = 11),
 #        legend.position = "bottom",
 #        strip.text = element_text(size = 7),
 #        strip.text.y = element_text(angle = 180),
 #        #panel.grid.major = element_blank(),
 #        panel.grid.minor = element_blank(),
 #        axis.ticks = element_line(size = 0.2),
 #        legend.text = element_text(size = 7),
 #        legend.title = element_text(size = 7),
 #        plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
 #         panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_Parametric_Engel_Curves/Parametric_EC_0_%s.jpg", Group_0), width = 15.5, height = 19.375, unit = "cm", res = 600)
  print(P_4.3.1.3)
  dev.off()
  
  # peg(sprintf("1_Figures/Analysis_Parametric_Engel_Curves/Parametric_EC_0_NP_%s.jpg", Group_0), width = 15.5, height = 19.375, unit = "cm", res = 200)
  # rint(P_4.3.1.4)
  # ev.off()
  
  rm(data_4.3.1.2.4, data_4.3.1.2.5, P_4.3.1.3)
  
}


# 4.3.1.3 Non-Parametric Engel Curves - Figures ####

dir.create("1_Figures/Analysis_Non_Parametric_Engel_Curves", showWarnings = FALSE)

for(i in Country.Set$Country){
  
  start <- Sys.time()
  
  data_4.3.1.3 <- data_2 %>%
    filter(Country == i)%>%
    select(hh_id, hh_weights, Country, starts_with("share_"), hh_expenditures_USD_2014_pc)%>%
    mutate(Income_Group_250 = as.numeric(binning(hh_expenditures_USD_2014_pc, 
                                                 bins = 250, method = c("wtd.quantile"), labels = seq(1,250,length.out=250), weights = hh_weights)))%>%
    select(-share_other_binning)%>%
    rename(share_Energy = share_energy, share_Food = share_food, share_Services = share_services, share_Goods = share_goods)%>%
    pivot_longer(starts_with("share"), names_to = "Type", values_to = "Share", names_prefix = "share_")%>%
    group_by(Income_Group_250, Type)%>%
    summarise(Share = wtd.mean(Share, hh_weights),
              hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014_pc, hh_weights),
              hh_weights = sum(hh_weights))%>%
    ungroup()
  
  data_4.3.1.3.1 <- data_2 %>%
    filter(Country == i)%>%
    group_by(Income_Group_5)%>%
    summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014_pc, hh_weights))%>%
    ungroup()
  
  upper_bound   <- plyr::round_any(max(data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc), 10000, f = ceiling)
  
  P_4.3.1.3 <- ggplot()+
    geom_vline(xintercept = data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc[1])+
    geom_vline(xintercept = data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc[2])+
    geom_vline(xintercept = data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc[3])+
    geom_vline(xintercept = data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc[4])+
    geom_vline(xintercept = data_4.3.1.3.1$mean_hh_expenditures_USD_2014_pc[5])+
    geom_smooth(data = data_4.3.1.3, 
                aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                    y = Share, fill = Type, colour = Type), method = "loess", formula = y ~ x, span = 0.75)+
    theme_bw()+
    xlab("Household expenditures per capita in US-$ (2014)") + ylab("Share of total expenditures")+
    scale_fill_npg()+
    scale_colour_npg()+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,0.65))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
    ggtitle(Country.Set$Country_long[Country.Set$Country == i])+
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 7),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_Non_Parametric_Engel_Curves/Nonparametric_EC_%s.jpg", i), width = 15.5, height = 15, unit = "cm", res = 200)
  print(P_4.3.1.3)
  dev.off()
  
  end <- Sys.time()
  print(end-start)
  
  print(i)
  
  rm(data_4.3.1.3, data_4.3.1.3.1, P_4.3.1.3)
  
}

# 4.3.2   Carbon intensity of consumption ####

# 4.3.2.1 Carbon intensity of consumption - Figures ####

dir.create("1_Figures/Analysis_Carbon_Intensity_Curve", showWarnings = FALSE)

data_4.3.2.1 <- data_2 %>%
  select(hh_id, hh_weights, Country, hh_size, 
         hh_expenditures_USD_2014_pc, hh_expenditures_USD_2014,
         carbon_intensity_kg_per_USD_national)%>%
  left_join(Country.Set)%>%
  mutate(Country_long = fct_reorder(Country_long,hh_expenditures_USD_2014_mean, min))%>%
  mutate(carbon_intensity_kg_per_USD_national_per_capita = carbon_intensity_kg_per_USD_national/hh_size)

for(Group_0 in c("A", "B", "C", "D")){
  
  if(Group_0 == "A") upper_bound <- 14000 else if(Group_0 == "B") upper_bound <- 24000 else if(Group_0 == "C") upper_bound <- 34000 else upper_bound <- 55000
  
  breaks <- plyr::round_any(seq(0,upper_bound, length.out = 3), 5000, f = floor)
  
  data_4.3.2.2 <- data_4.3.2.1 %>%
    filter(Group_Income == Group_0)
  
  P_4.3.2.1 <- ggplot()+
    geom_point(data = data_4.3.2.2, aes(x = hh_expenditures_USD_2014,
                                        y = carbon_intensity_kg_per_USD_national), 
               alpha = 0.05, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
    geom_smooth(data = data_4.3.2.2, 
                aes(x = hh_expenditures_USD_2014, weight = hh_weights, 
                    y = carbon_intensity_kg_per_USD_national, group = Country),
                level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#E64B35FF", 
                fill  = "#E64B35FF", size = 0.5)+
    theme_bw()+
    facet_wrap(. ~ Country_long, ncol = 4)+
    xlab("Household expenditures in US-$ (2014)") + ylab("Carbon intensity of consumption [kgCO2/USD]")+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,3))+
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0),
                       breaks = breaks)+
    scale_fill_discrete(guide = "none")+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_Carbon_Intensity_Curve/All_Panel_%s.jpg", Group_0), width = 15.5, height = 19.375, unit = "cm", res = 400)
  print(P_4.3.2.1)
  dev.off()
}

for(Group_0 in c("A", "B", "C", "D")){
  
  if(Group_0 == "A") upper_bound <- 7000 else if(Group_0 == "B") upper_bound <- 12000 else if(Group_0 == "C") upper_bound <- 17000 else upper_bound <- 27000
  
  breaks <- plyr::round_any(seq(0,upper_bound, length.out = 3), 2500, f = floor)
  
  data_4.3.2.3 <- data_4.3.2.1 %>%
    filter(Group_Income == Group_0)
  
  P_4.3.2.2 <- ggplot()+
    geom_point(data = data_4.3.2.3, aes(x = hh_expenditures_USD_2014_pc,
                                        y = carbon_intensity_kg_per_USD_national_per_capita), 
               alpha = 0.05, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
    geom_smooth(data = data_4.3.2.3, 
                aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                    y = carbon_intensity_kg_per_USD_national_per_capita, group = Country),
                level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#E64B35FF", 
                fill  = "#E64B35FF", size = 0.5)+
    theme_bw()+
    facet_wrap(. ~ Country_long, ncol = 4)+
    xlab("Household expenditures per capita in US-$ (2014)") + ylab("Carbon intensity of consumption per capita [kgCO2/USD]")+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,1.5))+
    scale_y_continuous(expand = c(0,0))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0),
                       breaks = breaks)+
    scale_fill_discrete(guide = "none")+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Analysis_Carbon_Intensity_Curve/All_Panel_%s_PC.jpg", Group_0), width = 15.5, height = 19.375, unit = "cm", res = 400)
  print(P_4.3.2.2)
  dev.off()
}

rm(P_4.3.2.1, data_4.3.2.1, data_4.3.2.2)
  
# 4.3.2.2 Carbon intensity of consumption - Tables ####

for(i in Country.Set$Country){
  data_4.3.2 <- data_2 %>%
    filter(Country == i)
  
  model_4.3.2 <- feols(carbon_intensity_kg_per_USD_national ~ log_hh_expenditures_USD_2014 + I(log_hh_expenditures_USD_2014^2) + I(log_hh_expenditures_USD_2014^3) + log(hh_size),
                   data = data_2, weights = data_2$hh_weights, vcov = "hetero")
  
}

# 4.4     Cooking fuel choices ####

# 4.4.1   Graphically ####

dir.create("1_Figures/Analysis_Cooking_Fuel_Choice_Binomial", showWarnings = FALSE)

for(i in Country.Set$Country){
  
  data_4.4.1 <- data_2 %>%
    filter(Country == i)

  if(sum(is.na(data_4.4.1$CF)) == 0){
    data_4.4.1.0 <- data_4.4.1 %>%
      mutate(Cooking_Liquid_Fuels = ifelse(CF %in% c("Kerosene", "Liquid fuel", "LPG", "Gas"),1,0),
             Cooking_Solid_Fuels  = ifelse(CF %in% c("Coal", "Charcoal", "Other biomass", "Firewood"),1,0),
             Cooking_Electricity  = ifelse(CF %in% c("Electricity"),1,0))%>%
      select(hh_id, hh_weights, starts_with("Cooking_"), hh_expenditures_USD_2014)%>%
      pivot_longer(starts_with("Cooking"), names_to = "Type", values_to = "values", names_prefix = "Cooking_")%>%
      mutate(Type = ifelse(Type == "Liquid_Fuels", "Liquid fuels",
                           ifelse(Type == "Solid_Fuels", "Solid fuels", Type)))%>%
      mutate(Type = factor(Type, levels = c("Solid fuels", "Liquid fuels", "Electricity")))
    
    data_4.4.1.1 <- data_4.4.1 %>%
      group_by(Income_Group_5)%>%
      summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014, hh_weights))%>%
      ungroup()
    
    #upper_bound   <- plyr::round_any(max(data_4.4.1.1$mean_hh_expenditures_USD_2014_pc), 15000, f = ceiling)
    upper_bound <- max(data_4.4.1$hh_expenditures_USD_2014)*0.8
    
    P_4.4.1 <- ggplot(data_4.4.1.0, aes(x = hh_expenditures_USD_2014, y = values))+
      geom_vline(xintercept = data_4.4.1.1$mean_hh_expenditures_USD_2014_pc[1])+
      geom_vline(xintercept = data_4.4.1.1$mean_hh_expenditures_USD_2014_pc[2])+
      geom_vline(xintercept = data_4.4.1.1$mean_hh_expenditures_USD_2014_pc[3])+
      geom_vline(xintercept = data_4.4.1.1$mean_hh_expenditures_USD_2014_pc[4])+
      geom_vline(xintercept = data_4.4.1.1$mean_hh_expenditures_USD_2014_pc[5])+
      geom_smooth(formula = y ~ x + I(x^2), method = "glm",
                  aes(colour = Type, fill = Type), method.args = list(family = "binomial"))+
      theme_bw()+
      xlab("Household expenditures in US-$ (2014)") + ylab("Probability to use cooking fuel")+
      coord_cartesian(ylim = c(0,1), xlim = c(0,upper_bound))+
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
      scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
      ggtitle(paste0("Cooking fuel choice in ", Country.Set$Country_long[Country.Set$Country == i]))+
      scale_fill_npg()+
      scale_colour_npg()+
      labs(colour = "", fill = "")+
      theme(axis.text.y = element_text(size = 7), 
            axis.text.x = element_text(size = 7),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 11),
            legend.position = "bottom",
            strip.text = element_text(size = 7),
            strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))
    
    jpeg(sprintf("1_Figures/Analysis_Cooking_Fuel_Choice_Binomial/Cooking_Fuel_Choice_Binomial_%s.jpg", i), width = 15.5, height = 15, unit = "cm", res = 200)
    print(P_4.4.1)
    dev.off()
    
    print(i)
    
  }
    
}

# Figure for Fuel Price Elasticity Paper

data_4.4.2 <- data_2 %>%
  filter(Country %in% c("IND","KEN", "MMR", "GHA", "KHM"))

data_4.4.2.0 <- data_4.4.2 %>%
  mutate(Cooking_Liquid_Fuels = ifelse(CF %in% c("Kerosene", "Liquid fuel", "LPG", "Gas"),1,0),
         Cooking_Solid_Fuels  = ifelse(CF %in% c("Coal", "Charcoal", "Other biomass", "Firewood"),1,0),
         Cooking_Electricity  = ifelse(CF %in% c("Electricity"),1,0))%>%
  select(hh_id, hh_weights, starts_with("Cooking_"), hh_expenditures_USD_2014, Country)%>%
  pivot_longer(starts_with("Cooking"), names_to = "Type", values_to = "values", names_prefix = "Cooking_")%>%
  mutate(Type = ifelse(Type == "Liquid_Fuels", "Liquid fuels",
                       ifelse(Type == "Solid_Fuels", "Solid fuels", Type)))%>%
  mutate(Type = factor(Type, levels = c("Solid fuels", "Liquid fuels", "Electricity")))

#data_4.4.1.1 <- data_4.4.1 %>%
#  group_by(Income_Group_5)%>%
#  summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014, hh_weights))%>%
#  ungroup()
#

P_4.4.2 <- ggplot(data_4.4.2.0, aes(x = hh_expenditures_USD_2014, y = values))+
  facet_wrap(. ~ Country)+
  geom_smooth(formula = y ~ x + I(x^2), method = "glm",
              aes(colour = Type, fill = Type), method.args = list(family = "binomial"))+
  theme_bw()+
  xlab("Household expenditures in US-$ (2014)") + ylab("Probability to use cooking fuel")+
  coord_cartesian(ylim = c(0,1), xlim = c(0,10000))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
  #ggtitle(paste0("Cooking fuel choice in ", Country.Set$Country_long[Country.Set$Country == i]))+
  scale_fill_npg()+
  scale_colour_npg()+
  labs(colour = "", fill = "")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg(sprintf("1_Figures/Analysis_Cooking_Fuel_Choice_Binomial/Cooking_Fuel_Choice_Binomial_Fuel_Price_Elasticity.jpg", i), width = 15.5, height = 15, unit = "cm", res = 200)
print(P_4.4.2)
dev.off()

print(i)

# 4.5     Vertical vs. horizontal inequality (Tables and Figures) ####

data_4.5 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_carbon_intensity_kg_per_USD_national = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  mutate(dif_q95_q05_carbon_intensity_kg_per_USD_national = q95_carbon_intensity_kg_per_USD_national - q05_carbon_intensity_kg_per_USD_national,
         dif_q80_q20_carbon_intensity_kg_per_USD_national = q80_carbon_intensity_kg_per_USD_national - q20_carbon_intensity_kg_per_USD_national,)%>%
  select(Country, Income_Group_5, dif_q95_q05_carbon_intensity_kg_per_USD_national, dif_q80_q20_carbon_intensity_kg_per_USD_national, median_carbon_intensity_kg_per_USD_national)%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(median_carbon_intensity_kg_per_USD_national, dif_q95_q05_carbon_intensity_kg_per_USD_national, dif_q80_q20_carbon_intensity_kg_per_USD_national))%>%
  mutate(median_1_5    = median_carbon_intensity_kg_per_USD_national_1/median_carbon_intensity_kg_per_USD_national_5,
         dif_95_05_1_5 = dif_q95_q05_carbon_intensity_kg_per_USD_national_1/dif_q95_q05_carbon_intensity_kg_per_USD_national_5,
         dif_80_20_1_5 = dif_q80_q20_carbon_intensity_kg_per_USD_national_1/dif_q80_q20_carbon_intensity_kg_per_USD_national_5)

# Table Output

data_4.5.1 <- data_4.5 %>%
  # mutate_at(vars(median_burden_CO2_national_1:dif_q80_q20_burden_CO2_national_5), list(~ label_percent(accuracy = 0.01)(.)))%>%
  mutate_at(vars(median_carbon_intensity_kg_per_USD_national_1:dif_80_20_1_5), list(~ round(.,2)))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Country_long, everything(), - Country)%>%
  arrange(Country_long)

colnames(data_4.5.1) <- c("Country", "$\\overline{AC}_{r}^{1}$", "MAC5", "H1", "H5", "H1A", "H5A", "MAC 1/5", "H 1/5", "H 1/5 A")

kbl(mutate_all(data_4.5.1, linebreak), format = "latex", caption = "Comparing Median Additional Costs (AC) and Horizontal Spread between first and fifth Expenditure Quintile", 
    booktabs = T, align = "l|cc|cccc|ccc", vline = "", linesep = "", longtable = T,
    col.names = NULL, label = "A7")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 9)%>%
  add_header_above(c("Country" = 1, 
                     "$\\\\overline{AC}_{r}^{1}$" = 1, 
                     "$\\\\overline{AC}_{r}^{5}$" = 1, 
                     "$\\\\overline{H}_{r}^{1}$" = 1, 
                     "$\\\\overline{H}_{r}^{5}$" = 1,
                     "$\\\\overline{H}_{r}^{1*}$" = 1,
                     "$\\\\overline{H}_{r}^{5*}$" = 1,
                     "$\\\\widehat{AC}_{r}^{1}$" = 1,
                     "$\\\\widehat{H}_{r}^{1}$" = 1,
                     "$\\\\widehat{H}_{r}^{1*}$" = 1), escape = FALSE, align = "c")%>%
  column_spec(1, width = "2.88 cm")%>%
  column_spec(2:10, width = "1.46 cm")%>%
  footnote(general = "This table shows the median carbon intensity in the first expenditure quintile ($\\\\overline{AC}_{r}^{1}$) and in the fifth quintile ($\\\\overline{AC}_{r}^{5}$). It displays the difference between the 5$^{th}$ (20$^{th}$) and 95$^{th}$ (80$^{th}$) within quintile percentile incidence for the first ($\\\\overline{H}_{r}^{1}$ and $\\\\overline{H}_{r}^{1*}$) and the fifth quintile ($\\\\overline{H}_{r}^{5}$ and $\\\\overline{H}_{r}^{5*}$). It also compares median carbon intensity in the first income quintile to that in the fifth quintile ($\\\\hat{AC}$$_{r}^{1}$). Lastly it displays our comparison index faciltiating the comparison of within quintile variation between the first and fifth quintile ($\\\\hat{H}_{r}^{1}$ and $\\\\hat{H}_{r}^{1*}$ respectively).",
           threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Vertical_Horizontal.tex")

# From Latin America paper

poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,2.95,
                                                                         2.96,2.96,1.06,
                                                                         2.95,1.05,1.05,2.95,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,2.95,2.95,1.05,
                         1.06,2.96,2.96,
                         2.95,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", "FAIL")))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,3.19,3.19,1.01,
                           1.01,3.19,3.19,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,3.19,3.19,
                           1.01,1.01,3.19,3.19),
                     label = c(rep("Progressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ1",4),
                               rep("Progressive and more heterogeneous in IQ1",4)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,3.18,3.18,
                           0.02,3.17,0.02),
                     x = c(0.02,0.02,3.17,
                           0.03,3.18,3.18))

poly_4 <- data.frame(text = c("Horizontal Differences > Vertical Differences",
                              "Vertical Differences > Horizontal Differences"),
                     x = c(2,1),
                     y = c(0.5,2.5))

data_4.5.2 <- data_4.5 

P.4.5.1 <- ggplot()+
  #geom_polygon(data = poly, aes(x = y, y = x, group = g), colour = "black", alpha = 0.5, fill = NA)+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  geom_text(data = poly_4, aes(label = text, x = x, y = y))+
  #geom_text(data = poly, aes(x = z_4, y = z_3, group = g, label = label))+
  theme_bw()+
  geom_point(data = data_4.5.2, aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  geom_text_repel(data = data_4.5.2, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
                  direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  #geom_abline(intercept = 0, slope = 1)+
  scale_fill_npg(guide = guide_legend(nrow = 2))+
  #scale_shape_manual(values = c(15,15,15,15,17,17,17,17,18,18,18,18,19,19,19,19))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Analysis_Vertical_Horizontal/Figure_VH.jpg", width = 15.75, height = 16, unit = "cm", res = 600)
print(P.4.5.1)
dev.off()

rm(poly, poly_2, poly_3, poly_4, data_4.5, data_4.5.1, data_4.5.2, P.4.5.1)

# _______ ####
# 5.      Econometric analysis ####

# Overhead Overleaf

tex.style <- style.tex(model.title = "Model", fixef.title = "\\midrule",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small")

dict_latex <- c(Income_Group_5 = "Expenditure Quintile", 
                log_hh_expenditures_USD_2014 = "HH Exp. (log)", 
                car.01 = "Car Ownership",
                hh_size = "HH Size", 
                refrigerator.01 = "Refrigerator Own.", 
                urban_01 = "Urban Area", 
                burden_CO2_national = "Carbon Price Incidence",
                affected_more_than_80q_CO2n ="Log-Odds of Expecting Higher Additional Costs than 80% of Population",
                affected_upper_80 = "Upper 20%",
                affected_lower_80 = "Lower 20%",
                affected_80_no_transfers = "Log-Odds of Higher Incidence than 80% of Pop. and No Access to Transfers",
                electricity.access = "Electricity Acc.",
                Ethnicity = "ETH",
                scaled_CO2_t_national = "CO$_{2}$ footprint",
                scaled_carbon_intensity_kg_per_USD_national = "CO$_{2}$ intensity",
                Liquidfuel = "Liquid fuel",
                ISCED_0 = "ISCED")

reference.list_5.0.1.1 <- data.frame(Country = Country.Set$Country)%>%
  mutate(REF_ISCED = ifelse(Country != "FIN", "Reference group for education (\\textit{ISCED}) is ISCED-level 0-1 (primary or no education)",
                            "Reference group for education (\\textit{ISCED} is ISCED-level 2-5 (secondary education)"))%>%
  mutate(REF_CF    = ifelse(Country %in% c("GTM", "DOM"), "\\textit{LPG} for cooking fuel (\\textit{CF})",
                            ifelse(Country %in% c("BEN","BFA","TGO","NER","NGA","GNB", "MLI"), "\\textit{Charcoal} for cooking fuel (\\textit{CF})",
                                   ifelse(Country %in% c("ARM", "BEL", "BGD", "BGR", "CHL", "CYP", "CZE", "DEU", "DNK", "ESP", "EST",
                                                         "FIN", "FRA", "GRC", "HRV", "HUN", "IRL", "ISR", "ITA", "LTU", "LUX", "LVA",
                                                         "MAR", "MNG", "NLD", "NOR", "PAK", "PHL", "POL", "PRT", "ROU", "SVK", "SWE"),"", "\\textit{Electricity} for cooking fuel (\\textit{CF})"))))%>%
  # To be updated - think about ethnicity
  #left_join(read.xlsx("2_Tables/2_Logit_Models/Reference_Categories_Logit.xlsx"))%>%
  # for now
  #mutate(ref = tolower(ref))%>%
  mutate(ref = NA)%>%
  mutate(REF_ETH   = ifelse(!is.na(ref), c(paste0("\\textit{",ref,"}")), NA))%>%
  mutate(REF_ETH_0 = "for ethnicitiy (\\textit{ETH})")%>%
  mutate(REF       = ifelse(is.na(REF_ETH) & REF_CF != "", paste0(REF_ISCED, " and ", REF_CF, "."),
                            ifelse(is.na(REF_ETH) & REF_CF == "", paste0(REF_ISCED, "."),
                                   paste0(REF_ISCED, ", ", REF_CF, " and ", REF_ETH, " ", REF_ETH_0, "."))))

# 5.1     OLS ####

data_5.1 <- data_2 %>%
  group_by(Country)%>%
  mutate(mean_carbon_intensity_kg_per_USD_national   = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights),
         sd_carbon_intensity_kg_per_USD_national = sqrt(wtd.var(carbon_intensity_kg_per_USD_national, weights = hh_weights)),
         mean_CO2_t_national        = wtd.mean(CO2_t_national,      weights = hh_weights),
         sd_CO2_t_national      = sqrt(wtd.var(CO2_t_national,      weights = hh_weights)))%>%
  ungroup()%>%
  mutate(scaled_carbon_intensity_kg_per_USD_national = (carbon_intensity_kg_per_USD_national - mean_carbon_intensity_kg_per_USD_national)/sd_carbon_intensity_kg_per_USD_national,
         scaled_CO2_t_national      = (CO2_t_national      - mean_CO2_t_national)/sd_CO2_t_national)%>%
  select(-mean_carbon_intensity_kg_per_USD_national, - mean_CO2_t_national, - sd_carbon_intensity_kg_per_USD_national, - sd_CO2_t_national)%>%
  mutate(ISCED_0 = ifelse(ISCED == 0 | ISCED == 1, "0-1",
                          ifelse(ISCED == 2 | ISCED == 3 | ISCED == 4 | ISCED == 5, "2-5",
                                 ifelse(ISCED == 6 | ISCED == 7 | ISCED == 8, "6-8", 
                                        ifelse(ISCED == 9, "9", ISCED)))))
  
# 5.1.1   OLS (Tables) ####

# Overhead

list_5.1.1.1       <- list()
data_frame_5.1.1.1 <- data.frame()

for(i in Country.Set$Country){
  
  data_5.1.1.1 <- data_5.1 %>%
    filter(Country == i)
  
  formula_0 <- " ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if(sum(is.na(data_5.1.1.1$urban_01)) == 0)           formula_0 <- paste0(formula_0, " + urban_01")
  if(sum(is.na(data_5.1.1.1$electricity.access))==0)   formula_0 <- paste0(formula_0, " + electricity.access")
  if(sum(is.na(data_5.1.1.1$car.01))==0)               formula_0 <- paste0(formula_0, " + car.01")
  if(sum(is.na(data_5.1.1.1$CF))==0 & i != "JOR"){
    if(!i %in% c("BEN","BFA","GTM","DOM","TGO","NER","NGA","GNB","MLI")) formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i  %in% c("GTM","DOM"))                                           formula_0 <- paste0(formula_0, ' + i(CF, ref = "LPG")')
    if(i  %in% c("BEN","BFA","TGO","NER","NGA","GNB","MLI"))             formula_0 <- paste0(formula_0, ' + i(CF, ref = "Charcoal")')
  }
  if(sum(is.na(data_5.1.1.1$ISCED_0))==0 & !i %in% c("SWE", "NLD")){
    if(i != "FIN") formula_0 <- paste0(formula_0, ' + i(ISCED_0, ref = "0-1")')
    if(i == "FIN") formula_0 <- paste0(formula_0, ' + i(ISCED_0, ref = "2-5")')
  }                
  # Leave out for now
  #if(sum(is.na(data_5.1.1.1$Ethnicity))==0 & i != "BOL"){
  #  ref_0 <- count(data_5.1.1.1, Ethnicity)$Ethnicity[which.max(count(data_5.1.1.1, Ethnicity)$n)]
  #  
  #  data_frame_5.1.1.2 <- bind_rows(data_frame_5.1.1.2, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
  #  
  #  formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  #}
  #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(paste0("scaled_carbon_intensity_kg_per_USD_national", formula_0))
  formula_2 <- as.formula(paste0("scaled_CO2_t_national", formula_0))
  
  model_5.1.1.1 <- feols(formula_1, 
                         data    = data_5.1.1.1, 
                         weights = data_5.1.1.1$hh_weights,  
                         se = "hetero")
  
  model_5.1.1.2 <- feols(formula_2, 
                         data    = data_5.1.1.1, 
                         weights = data_5.1.1.1$hh_weights, 
                         se = "hetero")
  
  REF_0 <- reference.list_5.0.1.1$REF[reference.list_5.0.1.1$Country == i]
  
  etable(model_5.1.1.1, dict = dict_latex, tex = TRUE, file = sprintf("2_Tables/3a_OLS_Burden/Table_OLS_Burden_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("OLS-regression coefficients for carbon intensity of consumers in %s", Country.Set$Country_long[Country.Set$Country == i]),  
         label = sprintf("tab:OLS_1_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation OLS on the carbon intensity of consumption in  ", sprintf("in %s",Country.Set$Country_long[Country.Set$Country == i]), " as the dependent variable. ", REF_0)))
  
  etable(model_5.1.1.2, dict = dict_latex, tex = TRUE, file = sprintf("2_Tables/3b_OLS_CO2_t_national/Table_OLS_CO2_t_national_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("OLS-regression coefficients for carbon footprint in %s", Country.Set$Country_long[Country.Set$Country == i]),  
         label = sprintf("tab:OLS_2_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation OLS on the carbon footprint of consumption in  ", sprintf("in %s",i), " as the dependent variable.  ", REF_0)))
  
  tidy_5.1.1.1 <- tidy(model_5.1.1.1)%>%
    mutate(Country = i)%>%
    mutate(Type = "carbon_intensity_kg_per_USD_national")

  tidy_5.1.1.2 <- tidy(model_5.1.1.2)%>%
    mutate(Country = i)%>%
    mutate(Type = "CO2_t_national")
  
  data_frame_5.1.1.1 <- data_frame_5.1.1.1 %>%
    bind_rows(tidy_5.1.1.1)%>%
    bind_rows(tidy_5.1.1.2)
  
  list_5.1.1.1[[paste0(i, "_CI")]] <- model_5.1.1.1
  list_5.1.1.1[[paste0(i, "_CF")]] <- model_5.1.1.2
  
  print(i)
  
  rm(data_5.1.1.1, tidy_5.1.1.1, tidy_5.1.1.2, model_5.1.1.1, model_5.1.1.2)
}

# may also display results for multiple countries in one table one day

# 5.1.2   OLS (Figures / average marginal effects) ####

data_frame_5.1.2.1 <- data_frame_5.1.1.1 %>%
  filter(term != "(Intercept)")%>%
  mutate(Type_B = "OLS")%>%
  mutate(CF_base = ifelse(Country %in% c("GTM", "DOM"), "LPG",
                          ifelse(Country %in% c("BEN","BFA","TGO","NER","NGA","GNB","MLI"), "Charcoal", "Electricity")))

for (Type_0 in c("carbon_intensity_kg_per_USD_national", "CO2_t_national")){

  for (Term_0 in c("urban_01", "car.01", "electricity.access", "hh_size", "log_hh_expenditures_USD_2014", 
                   "ISCED_0::2-5", "ISCED_0::6-8")){
    data_frame_5.1.2.2 <- data_frame_5.1.2.1 %>%
      filter(Type == Type_0)%>%
      filter(term == Term_0)%>%
      left_join(select(Country.Set, Country, hh_expenditures_USD_2014_mean), by = "Country")%>%
      mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
      mutate(conf.low  = estimate - 1.96*std.error,
             conf.high = estimate + 1.96*std.error)
    
    labels_data_frame <- expand_grid(Term_0 = data_frame_5.1.2.1$term,
                                     Type_0 = data_frame_5.1.2.1$Type)%>%
      unique()%>%
      mutate(title_0 = ifelse(Term_0 == "urban_01", "Urban citizenship",
                              ifelse(Term_0 == "car.01", "Car ownership",
                                     ifelse(Term_0 == "electricity.access", "Electricity access",
                                            ifelse(Term_0 == "log_hh_expenditures_USD_2014", "Household expenditures",
                                                   ifelse(Term_0 == "hh_size", "Household size", 
                                                          ifelse(Term_0 == "ISCED_0::2-5", "Secondary education",
                                                                 ifelse(Term_0 == "ISCED_0::6-8", "Tertiary education", NA))))))))%>%
      mutate(legend_0 = tolower(title_0))%>%
      mutate(bound_0 = ifelse(Term_0 == "log_hh_expenditures_USD_2014", -1.5,
                              ifelse(Type_0 == "carbon_intensity_kg_per_USD_national" & Term_0 == "electricity.access", -2, -1)))%>%
      mutate(bound_1 = ifelse(Type_0 == "carbon_intensity_kg_per_USD_national" & Term_0 == "electricity.access", 2,
                              ifelse(Term_0 == "car.01" & Type_0 == "carbon_intensity_kg_per_USD_national", 4,
                                     ifelse(Term_0 == "car.01" & Type_0 == "CO2_t_national", 6.5,
                                            ifelse(Term_0 == "log_hh_expenditures_USD_2014", 1.5,
                                                   ifelse(Term_0 == "ISCED_0::6-8", 1.5, 1))))))
                                     
    bound_0  <- labels_data_frame$bound_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    bound_1  <- labels_data_frame$bound_1[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    title_0  <- labels_data_frame$title_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    legend_0 <- labels_data_frame$legend_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    
    if(Type_0 == "carbon_intensity_kg_per_USD_national") state_0 <- "carbon intensity of consumption [SD]" else state_0 <- "carbon footprint of consumption [SD]"
    
    P_5.1.2.1 <- ggplot(data = data_frame_5.1.2.2, aes(x = estimate, y = reorder(Country, desc(estimate))))+
      geom_vline(aes(xintercept = 0))+
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5, size = 0.3)+
      geom_point(shape = 21, aes (fill = Colour_Type), size = 1.5)+
      theme_bw()+
      xlab(paste0("Average marginal effect of ",legend_0," on ", state_0))+ 
      ylab("Country")+
      labs(colour = "", fill = "")+
      coord_cartesian(xlim = c(bound_0, bound_1))+
      scale_x_continuous(expand = c(0,0), breaks = seq(round(bound_0), round(bound_1), 1))+
      scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
      ggtitle(title_0)+
      theme(axis.text.y = element_text(size = 6), 
            axis.text.x = element_text(size = 6),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 11),
            legend.position = "bottom",
            # strip.text = element_text(size = 7),
            #strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))
    
    if(Term_0 == "ISCED_0::2-5") Term_0 <- "secondary_education" else if(Term_0 == "ISCED_0::6-8") Term_0 <- "higher_education" 
    
    if(Type_0 == "carbon_intensity_kg_per_USD_national"){
      jpeg(sprintf("1_Figures/Analysis_OLS_ME_Carbon_Intensity/AME_OLS_CI_%s.jpg", Term_0), width = 15.5, height = 16, unit = "cm", res = 600)
      print(P_5.1.2.1)
      dev.off()
    }
    
    if(Type_0 == "CO2_t_national"){
      jpeg(sprintf("1_Figures/Analysis_OLS_ME_Carbon_Footprint/AME_OLS_FP_%s.jpg", Term_0), width = 15.5, height = 16, unit = "cm", res = 600)
      print(P_5.1.2.1)
      dev.off()
    }
    
    }
  
  rm(P_5.1.2.1, data_frame_5.1.2.2, labels_data_frame, bound_0, bound_1, legend_0, state_0, Term_0, title_0, Type_0)
}

# Separately for cooking fuels

for (Type_0 in c("carbon_intensity_kg_per_USD_national", "CO2_t_national")){
  
  data_frame_5.1.2.3 <- data_frame_5.1.2.1 %>%
    filter(Type == Type_0)%>%
    separate(term, c("term_0", "term_1"), sep = "::")%>%
    filter(term_0 == "CF")%>%
    left_join(select(Country.Set, Country, hh_expenditures_USD_2014_mean), by = "Country")%>%
    mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
    filter(term_1 != "Unknown")%>%
    group_by(CF_base, term_1)%>%
    arrange(desc(estimate))%>%
    ungroup()%>%
    mutate(number = 1:n())%>%
    mutate(test = fct_reorder(interaction(Country,CF_base,term_1), number))%>%
    mutate(Category = ifelse(CF_base == "Electricity" & term_1 %in% c("Charcoal", "Firewood", "Coal", "Other biomass"), "Electricity A",
                             ifelse(CF_base == "Electricity", "Electricity B", CF_base)))%>%
    mutate(conf.low  = estimate - 1.96*std.error,
           conf.high = estimate + 1.96*std.error)
  
  for (B_0 in c("Electricity A", "Electricity B", "LPG", "Charcoal")){
    
    labels_data_frame <- expand_grid(B      = data_frame_5.1.2.3$Category,
                                     Type_0 = data_frame_5.1.2.3$Type)%>%
      unique()%>%
      arrange(B)%>%
      mutate(title_0 = ifelse(B == "Electricity A", "Cooking fuel choice compared to electricity - solid fuels",
                              ifelse(B == "LPG", "Cooking fuel choice compared to LPG",
                                     ifelse(B == "Charcoal", "Cooking fuel choice compared to charcoal", 
                                            ifelse(B == "Electricity B", "Cooking fuel choice compared to electricity - liquid fuels", NA)))))%>%
      mutate(legend_0 = ifelse(B == "Electricity A" | B == "Electricity B", "cooking fuel choice compared to electricity",
                               ifelse(B == "LPG", "cooking fuel choice compared to LPG", title_0)))%>%
      mutate(bound_0 = ifelse(Type_0 == "carbon_intensity_kg_per_USD_national", c(-1,-2.5,-3,-2), c(-1,-2,-1,-2)),
             bound_1 = ifelse(Type_0 == "carbon_intensity_kg_per_USD_national", c(3,4,7,1), c(2,2,2.5,1)))
    
    bound_0   <- labels_data_frame$bound_0[labels_data_frame$B == B_0 & labels_data_frame$Type_0 == Type_0]
    bound_1   <- labels_data_frame$bound_1[labels_data_frame$B == B_0 & labels_data_frame$Type_0 == Type_0]
    title_0   <- labels_data_frame$title_0[labels_data_frame$B == B_0 & labels_data_frame$Type_0 == Type_0]
    legend_0 <- labels_data_frame$legend_0[labels_data_frame$B == B_0 & labels_data_frame$Type_0 == Type_0]
    if(Type_0 == "carbon_intensity_kg_per_USD_national") state_0 <- "carbon intensity of consumption [SD]" else state_0 <- "carbon footprint of consumption [SD]"
    if(B_0 == "Electricity A" | B_0 == "Electricity B") ATY <- element_text(size = 4) else ATY <- element_text(size = 6)
    
    data_frame_5.1.2.4 <- data_frame_5.1.2.3 %>%
      filter(Category == B_0)
    
    P_5.1.2.2 <- ggplot(data = data_frame_5.1.2.4, aes(x = estimate, y = test))+
      geom_vline(aes(xintercept = 0))+
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5, size = 0.3)+
      geom_point(shape = 21, aes (fill = Colour_Type), size = 1.5)+
      theme_bw()+
      facet_wrap(. ~ term_1, scales = "free_y", ncol = 1)+
      xlab(paste0("Average marginal effect of ",legend_0," on ", state_0))+
      ylab("Country")+
      labs(colour = "", fill = "")+
      coord_cartesian(xlim = c(bound_0, bound_1))+
      scale_y_discrete(labels = function(x) str_sub(x,1,3))+
      scale_x_continuous(expand = c(0,0), breaks = seq(round(bound_0), round(bound_1), 1))+
      scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
      ggtitle(title_0)+
      theme(axis.text.y = ATY, 
            axis.text.x = element_text(size = 6),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 11),
            legend.position = "bottom",
            # strip.text = element_text(size = 7),
            #strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))+
      ggforce::facet_col(vars(term_1), scales = "free", space = "free")
    
    if(Type_0 == "carbon_intensity_kg_per_USD_national"){
      jpeg(sprintf("1_Figures/Analysis_OLS_ME_Carbon_Intensity/AME_OLS_CI_CF_%s.jpg", B_0), width = 15.5, height = 16, unit = "cm", res = 600)
      print(P_5.1.2.2)
      dev.off()
    }
    
    if(Type_0 == "CO2_t_national"){
      jpeg(sprintf("1_Figures/Analysis_OLS_ME_Carbon_Footprint/AME_OLS_FP_CF_%s.jpg", B_0), width = 15.5, height = 16, unit = "cm", res = 600)
      print(P_5.1.2.2)
      dev.off()
    }
    
  }
  
}

rm(ATY, B_0, bound_0, bound_1, legend_0, state_0, title_0, Type_0, formula_0, formula_1, formula_2, P_5.1.2.2, 
   data_frame_5.1.2.3, data_frame_5.1.2.4, labels_data_frame, list_5.1.1.1)

# 5.2     Inequality decomposition ####

# 5.3     Logit-Model ####

data_5.3 <- data_2 %>%
  group_by(Country)%>%
  mutate(barrier_upper_80 = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.8, weights = hh_weights),
         barrier_lower_80 = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.2, weights = hh_weights))%>%
  ungroup()%>%
  mutate(affected_upper_80 = ifelse(carbon_intensity_kg_per_USD_national > barrier_upper_80,1,0),
         affected_lower_80 = ifelse(carbon_intensity_kg_per_USD_national < barrier_lower_80,1,0))%>%
  mutate(ISCED_0 = ifelse(ISCED == 0 | ISCED == 1, "0-1",
                          ifelse(ISCED == 2 | ISCED == 3 | ISCED == 4 | ISCED == 5, "2-5",
                                 ifelse(ISCED == 6 | ISCED == 7 | ISCED == 8, "6-8", 
                                        ifelse(ISCED == 9, "9", ISCED)))))

# 5.3.1   Logit-Model (Tables) ####

# Overhead

list_5.3.1.1       <- list()
data_frame_5.3.1.1 <- data.frame()
data_frame_5.3.1.2 <- data.frame()

for(i in Country.Set$Country){
  
  data_5.3.1.1 <- data_5.3 %>%
    filter(Country == i)
  
  formula_0 <- " ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if(sum(is.na(data_5.3.1.1$urban_01)) == 0)           formula_0 <- paste0(formula_0, " + urban_01")
  if(sum(is.na(data_5.3.1.1$electricity.access))==0)   formula_0 <- paste0(formula_0, " + electricity.access")
  if(sum(is.na(data_5.3.1.1$car.01))==0)               formula_0 <- paste0(formula_0, " + car.01")
  if(sum(is.na(data_5.3.1.1$CF))==0){
    if(!i %in% c("BEN","BFA","GTM","DOM","TGO","NER","NGA","GNB","MLI")) formula_0 <- paste0(formula_0, ' + i(CF, ref = "Electricity")')
    if(i  %in% c("GTM","DOM"))                                           formula_0 <- paste0(formula_0, ' + i(CF, ref = "LPG")')
    if(i  %in% c("BEN","BFA","TGO","NER","NGA","GNB","MLI"))             formula_0 <- paste0(formula_0, ' + i(CF, ref = "Charcoal")')
  }
  if(sum(is.na(data_5.3.1.1$ISCED_0))==0 & !i %in% c("SWE", "NLD")){
    if(i != "FIN") formula_0 <- paste0(formula_0, " + i(ISCED_0, ref = '0-1')")
    if(i == "FIN") formula_0 <- paste0(formula_0, " + i(ISCED_0, ref = '2-5')")
  }                
  # Leave out for now
  #if(sum(is.na(data_5.3.1.1$Ethnicity))==0 & i != "BOL"){
  #  ref_0 <- count(data_5.3.1.1, Ethnicity)$Ethnicity[which.max(count(data_5.3.1.1, Ethnicity)$n)]
  #  
  #  data_frame_5.3.1.2 <- bind_rows(data_frame_5.3.1.2, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
  #  
  #  formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  #}
  #if("religion" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$religion))==0)           formula_0 <- paste0(formula_0, " + factor(religion)")
  #if("district" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$district))==0)           formula_0 <- paste0(formula_0, " + factor(district)")
  #if("province" %in% colnames(household_information_0) & sum(is.na(data_2.1.2.1$province))==0)           formula_0 <- paste0(formula_0, " + factor(province)")
  
  formula_1 <- as.formula(paste0("affected_upper_80", formula_0))
  formula_2 <- as.formula(paste0("affected_lower_80", formula_0))
  
  model_5.3.1.1 <- feglm(formula_1, 
                         data    = data_5.3.1.1, 
                         weights = data_5.3.1.1$hh_weights, 
                         family = quasibinomial("logit"), 
                         se = "hetero")
  
  model_5.3.1.2 <- feglm(formula_2, 
                         data    = data_5.3.1.1, 
                         weights = data_5.3.1.1$hh_weights, 
                         family = quasibinomial("logit"), 
                         se = "hetero")

  REF_0 <- reference.list_5.0.1.1$REF[reference.list_5.0.1.1$Country == i]

  etable(model_5.3.1.1, model_5.3.1.2, dict = dict_latex, tex = TRUE, file = sprintf("2_Tables/2_Logit_Models/Table_Logit_Burden_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("Logit-model coefficients for carbon-intensive consumers in %s", Country.Set$Country_long[Country.Set$Country == i]),  
         label = sprintf("tab:Logit_1_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation LOGIT on the log-odds transformed probability of higher (lower) additional costs than 80\\% of the population ", sprintf("in %s",Country.Set$Country_long[Country.Set$Country == i]), " as the dependent variable. ", REF_0)))
  
  # etable(model_5.3.1.2, dict = dict_latex, tex = TRUE, file = sprintf("2_Tables/2_Logit_Models/Table_Logit_Burden_lower_%s.tex", i),
  #        digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
  #        title = sprintf("Logit-Model Coefficients Hardship Cases in %s", Country.Set$Country_long[Country.Set$Country == i]),  
  #        label = sprintf("tab:Logit_1_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
  #        notes = c("\\medskip \\textit{Note:}",
  #                  paste0("This table displays regression results from equation LOGIT on the log-odds transformed probability of lower additional costs than 80\\% of the population ", sprintf("in %s",i), " as the dependent variable.  ", REF_0)))
  
  # Additional stuff done for Latin America - check out if necessary 
  
  tidy_5.3.1.1 <- tidy(model_5.3.1.1)%>%
    mutate(Country = i)%>%
    mutate(Type = "affected_upper_80")
  
  tidy_5.3.1.2 <- tidy(model_5.3.1.2)%>%
    mutate(Country = i)%>%
    mutate(Type = "affected_lower_80")
  
  data_frame_5.3.1.1 <- data_frame_5.3.1.1 %>%
    bind_rows(tidy_5.3.1.1)%>%
    bind_rows(tidy_5.3.1.2)
  
  list_5.3.1.1[[paste0(i, "_U")]] <- model_5.3.1.1
  list_5.3.1.1[[paste0(i, "_L")]] <- model_5.3.1.2
  print(i)
  
  rm(data_5.3.1.1, tidy_5.3.1.1, tidy_5.3.1.2, model_5.3.1.1, model_5.3.1.2)
}

# may also display results for multiple countries in one table one day

data_frame_5.3.1.3 <- data_frame_5.3.1.1 %>%
  filter(term != "(Intercept)")%>%
  mutate(Type_B = "Logit")

write.xlsx(data_frame_5.3.1.2, "2_Tables/2_Logit_Models/Reference_Categories_Logit.xlsx")

# 5.3.1.0 Joint tables with OLS output ####

for(i in Country.Set$Country){
  model_5.3.1.0.A <- list_5.1.1.1[[paste0(i, "_CF")]]
  model_5.3.1.0.B <- list_5.1.1.1[[paste0(i, "_CI")]]
  model_5.3.1.0.C <- list_5.3.1.1[[paste0(i, "_U")]]
  model_5.3.1.0.D <- list_5.3.1.1[[paste0(i, "_L")]]
  
  REF_0 <- reference.list_5.0.1.1$REF[reference.list_5.0.1.1$Country == i]
  
  etable(model_5.3.1.0.A, model_5.3.1.0.B, model_5.3.1.0.C, model_5.3.1.0.D, 
         dict = dict_latex, 
         tex = TRUE, family = FALSE, headers = c("OLS", "OLS", "Logit", "Logit"),
         file = sprintf("2_Tables/3c_OLS_Logit_combined/Table_CF_CI_UL20_%s.tex", i),
         digits = 3, replace = TRUE, fitstat = c("n", "cor2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
         title = sprintf("Model coefficients: carbon-intensive consumers in %s", Country.Set$Country_long[Country.Set$Country == i]),  
         label = sprintf("tab:Logit_1_%s",i), adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", placement = "htbp!",
         notes = c("\\medskip \\textit{Note:}",
                   paste0("This table displays regression results from equation OLS on the carbon emissions embedded in consumption [SD] (1) and on the carbon intensity of consumption [SD] (2), respectively. 
                          Column (3) ((4) respectively) shows regression results from equation LOGIT on the log-odds transformed probability of higher (lower) additional costs than 80\\% of the population ", sprintf("in %s",Country.Set$Country_long[Country.Set$Country == i]), " as the dependent variable. ", REF_0)))
  
}

rm(list_5.1.1.1, list_5.3.1.1, model_5.3.1.0.A, model_5.3.1.0.B, model_5.3.1.0.C, model_5.3.1.0.D)

# 5.3.2   Logit-Model (Figures / average marginal effects) ####

data_frame_5.3.2.1 <- data.frame()

for(i in Country.Set$Country){
  
  start_5.3.2.1 <- Sys.time()
  
  data_5.3.2.1 <- data_5.3 %>%
    filter(Country == i)%>%
    mutate(CF = ifelse(CF == "Electricity", "A_Electricity",
                       ifelse(CF == "LPG", "B_LPG", CF)))%>%
    mutate(ISCED              = factor(ISCED))
  
  formula_0 <- " ~ log_hh_expenditures_USD_2014 + hh_size"
  
  if(sum(is.na(data_5.3.2.1$urban_01)) == 0 & i != "ISR" & i != "BRA" & i != "SUR")           formula_0 <- paste0(formula_0, " + urban_01")
  if(sum(is.na(data_5.3.2.1$electricity.access))==0 & i != "GHA" & i != "SLV" & i != "IND")   formula_0 <- paste0(formula_0, " + electricity.access")
  if(sum(is.na(data_5.3.2.1$car.01))==0)               formula_0 <- paste0(formula_0, " + car.01")
  if(sum(is.na(data_5.3.2.1$motorcycle.01))==0)               formula_0 <- paste0(formula_0, " + motorcycle.01")
  if(sum(is.na(data_5.3.2.1$CF))==0){
    if(!i %in% c("BEN","BFA","GTM","DOM","TGO","NER","NGA","GNB","MLI", "GEO", "JOR")) formula_0 <- paste0(formula_0, ' + i(CF, ref = "A_Electricity")')
    if(i  %in% c("GTM","DOM", "GEO"))                                           formula_0 <- paste0(formula_0, ' + i(CF, ref = "B_LPG")')
    if(i  %in% c("BEN","BFA","TGO","NER","NGA","GNB","MLI"))             formula_0 <- paste0(formula_0, ' + i(CF, ref = "Charcoal")')
  }
  if(sum(is.na(data_5.3.2.1$ISCED_0))==0 & i != "NLD" & i != "SWE" & i != "MNG" & i != "CHE" & i != "GBR"){
    if(i != "FIN") formula_0 <- paste0(formula_0, " | ISCED_0")
    if(i == "FIN") formula_0 <- paste0(formula_0, " | ISCED_0")
  }                
  if(i %in% c("NLD", "SWE", "MNG", "CHE", "GBR")){
    formula_0 <- paste0(formula_0, " | ")
  }
  
  if(sum(is.na(data_5.3.2.1$Religion))==0)           formula_0 <- paste0(formula_0, " + Religion")
  if(sum(is.na(data_5.3.2.1$Language))==0)           formula_0 <- paste0(formula_0, " + Language")
  if(sum(is.na(data_5.3.2.1$religiosity))==0)        formula_0 <- paste0(formula_0, " + religiosity")
  if(sum(is.na(data_5.3.2.1$Ethnicity))==0)          formula_0 <- paste0(formula_0, " + Ethnicity")
  if(sum(is.na(data_5.3.2.1$Nationality))==0)        formula_0 <- paste0(formula_0, " + Nationality")
  if(sum(is.na(data_5.3.2.1$HF))==0)                 formula_0 <- paste0(formula_0, " + HF")
  if(sum(is.na(data_5.3.2.1$LF))==0)                 formula_0 <- paste0(formula_0, " + LF")
  if(sum(is.na(data_5.3.2.1$refrigerator.01))==0)    formula_0 <- paste0(formula_0, " + refrigerator.01")
  if(sum(is.na(data_5.3.2.1$washing_machine.01))==0) formula_0 <- paste0(formula_0, " + washing_machine.01")
  if(sum(is.na(data_5.3.2.1$ac.01))==0)              formula_0 <- paste0(formula_0, " + ac.01")
  if(sum(is.na(data_5.3.2.1$District))==0)           formula_0 <- paste0(formula_0, " + District")
  if(sum(is.na(data_5.3.2.1$Province))==0)           formula_0 <- paste0(formula_0, " + Province")
  
  # Leave out for now
  #if(sum(is.na(data_5.3.2.1$Ethnicity))==0 & i != "BOL"){
  #  ref_0 <- count(data_5.3.2.1, Ethnicity)$Ethnicity[which.max(count(data_5.3.2.1, Ethnicity)$n)]
  #  
  #  data_frame_5.3.2.2 <- bind_rows(data_frame_5.3.2.2, data.frame(Country = i, Type = "Ethnicity", ref = ref_0))
  #  
  #  formula_0 <- paste0(formula_0, ' + i(Ethnicity, ref = "', ref_0,'")')
  #}
  #if(sum(is.na(data_5.3.2.1$Religion))==0)           formula_0 <- paste0(formula_0, " + Religion")
  #if(sum(is.na(data_5.3.2.1$District))==0)           formula_0 <- paste0(formula_0, " + District")
  #if(sum(is.na(data_5.3.2.1$Province))==0)           formula_0 <- paste0(formula_0, " + Province")
  
  formula_1 <- as.formula(paste0("affected_upper_80", formula_0))
  formula_2 <- as.formula(paste0("affected_lower_80", formula_0))
  
  model_5.3.2.1 <- feglm(formula_1, 
                         data    = data_5.3.2.1, 
                         weights = data_5.3.2.1$hh_weights, 
                         family = quasibinomial("logit"), 
                         se = "hetero")
  
  # model_5.3.2.2 <- feglm(formula_2, 
  #                        data    = data_5.3.2.1, 
  #                        weights = data_5.3.2.1$hh_weights, 
  #                        family = quasibinomial("logit"), 
  #                        se = "hetero")
  
  tidy_5.3.2.1 <- tidy(marginaleffects(model_5.3.2.1, wts = data_5.3.2.1$hh_weights))%>%
    mutate(Country = i)%>%
    mutate(Type = "affected_upper_80")
  
  # tidy_5.3.2.2 <- tidy(marginaleffects(model_5.3.2.2, wts = data_5.3.2.1$hh_weights))%>%
  #   mutate(Country = i)%>%
  #   mutate(Type = "affected_lower_80")
  
  data_frame_5.3.2.1 <- data_frame_5.3.2.1 %>%
    bind_rows(tidy_5.3.2.1)#%>%
    # bind_rows(tidy_5.3.2.2)
  
  end_5.3.2.1 <- Sys.time()
  
  print(paste0(i, " ", round(end_5.3.2.1 - start_5.3.2.1,1), "secs"))
  
  rm(data_5.3.2.1, tidy_5.3.2.1, model_5.3.2.1, start_5.3.2.1, end_5.3.2.1, formula_0, formula_1, formula_2)
}

# write.xlsx(data_frame_5.3.2.1, "1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_Logit_2017.xlsx")

data_frame_5.3.2.3 <- read.xlsx("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_Logit_2017.xlsx")#%>%
  # mutate(term = ifelse((contrast == "6-8 - 2-5" | contrast == "6-8 - 0-1") & !is.na(contrast), "higher_education",
  #                      ifelse(contrast == "2-5 - 0-1" & !is.na(contrast), "secondary_education", term)))

for (Type_0 in c("affected_upper_80")){
  # Add education - questionable - maybe cluster higher education / lower education
  for (Term_0 in c("urban_01", "car.01", "electricity.access", "hh_size", "log_hh_expenditures_USD_2014", "motorcycle.01")){
    data_frame_5.3.2.4 <- data_frame_5.3.2.3 %>%
      filter(Type == Type_0)%>%
      filter(term == Term_0)%>%
      left_join(select(Country.Set, Country, hh_expenditures_USD_2014_mean), by = "Country")%>%
      mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))
    
    labels_data_frame <- expand_grid(Term_0 = data_frame_5.3.2.3$term,
                                     Type_0 = data_frame_5.3.2.3$Type)%>%
      unique()%>%
      mutate(title_0 = ifelse(Term_0 == "urban_01", "Urban citizenship",
                              ifelse(Term_0 == "car.01", "Car ownership",
                                     ifelse(Term_0 == "electricity.access", "Electricity access",
                                            ifelse(Term_0 == "log_hh_expenditures_USD_2014", "Household expenditures",
                                                   ifelse(Term_0 == "hh_size", "Household size", 
                                                          ifelse(Term_0 == "secondary_education", "Secondary education",
                                                                 ifelse(Term_0 == "higher_education", "Higher education", 
                                                                        ifelse(Term_0 == "motorcycle.01", "Motorcycle ownership", NA)))))))))%>%
      mutate(legend_0 = tolower(title_0))%>%
      mutate(bound_0 = ifelse(Term_0 == "urban_01", -0.4,
                              ifelse(Term_0 == "car.01" & Type_0 == "affected_lower_80",-0.5,
                                     ifelse(Term_0 == "car.01" & Type_0 == "affected_upper_80",-0.3,
                                            ifelse(Term_0 == "electricity.access" & Type_0 == "affected_upper_80", -0.4,
                                                   ifelse(Term_0 == "electricity.access" & Type_0 == "affected_lower_80", -0.85,
                                                          ifelse(Term_0 == "log_hh_expenditures_USD_2014", -0.6,
                                                                 ifelse(Term_0 == "hh_size" & Type_0 == "affected_upper_80", -0.04,
                                                                        ifelse(Term_0 == "hh_size" & Type_0 == "affected_lower_80", -0.12,
                                                                               ifelse(Term_0 == "secondary_education",-0.5,
                                                                                      ifelse(Term_0 == "motorcycle.01", -0.25 ,-0.75)))))))))))%>%
      mutate(bound_1 = ifelse(Term_0 == "urban_01", 0.22,
                              ifelse(Term_0 == "car.01" & Type_0 == "affected_lower_80",0.25,
                                     ifelse(Term_0 == "car.01" & Type_0 == "affected_upper_80",1,
                                            ifelse(Term_0 == "electricity.access" & Type_0 == "affected_upper_80", 0.5,
                                                   ifelse(Term_0 == "electricity.access" & Type_0 == "affected_lower_80", 0.5,
                                                          ifelse(Term_0 == "log_hh_expenditures_USD_2014" & Type_0 == "affected_upper_80", 0.4,
                                                                 ifelse(Term_0 == "log_hh_expenditures_USD_2014" & Type_0 == "affected_lower_80", 0.3,
                                                                        ifelse(Term_0 == "hh_size" & Type_0 == "affected_upper_80", 0.12,
                                                                               ifelse(Term_0 == "hh_size" & Type_0 == "affected_lower_80", 0.05,
                                                                                      ifelse(Term_0 == "motorcycle.01", 1, NA)))))))))))
    
    bound_0  <- labels_data_frame$bound_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    bound_1  <- labels_data_frame$bound_1[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    title_0  <- labels_data_frame$title_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    legend_0 <- labels_data_frame$legend_0[labels_data_frame$Term_0 == Term_0 & labels_data_frame$Type_0 == Type_0]
    if(Type_0 == "affected_upper_80") state_0 <- "higher" else state_0 <- "lower" 
    if(Term_0 == "log_hh_expenditures_USD_2014" | Term_0 == "hh_size") size_0 <- 5 else size_0 <- 6
    
    P_5.3.2.4 <- ggplot(data = data_frame_5.3.2.4, aes(x = estimate, y = reorder(Country, desc(estimate))))+
      geom_vline(aes(xintercept = 0))+
      geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5, size = 0.3)+
      geom_point(shape = 21, aes (fill = Colour_Type), size = 1.5)+
      theme_bw()+
      xlab(paste0("Average marginal effect of ",legend_0," on probability of ", state_0 ," 20% of carbon intensity"))+ 
      ylab("Country")+
      labs(colour = "", fill = "")+
      coord_cartesian(xlim = c(bound_0, bound_1))+
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
      scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
      ggtitle(title_0)+
      theme(axis.text.y = element_text(size = size_0), 
            axis.text.x = element_text(size = 6),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 11),
            legend.position = "bottom",
            # strip.text = element_text(size = 7),
            #strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))
    
    jpeg(sprintf("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_%s_%s_2017.jpg", Type_0, Term_0), width = 15.5, height = 16, unit = "cm", res = 600)
    print(P_5.3.2.4)
    dev.off()

  }
  
  rm(P_5.3.2.4, data_frame_5.3.2.4, labels_data_frame, bound_0, bound_1, legend_0, state_0, Term_0, title_0, Type_0)
}

for (Type_0 in c("affected_upper_80")){
  # Add education - questionable - maybe cluster higher education / lower education
    data_frame_5.3.2.4 <- data_frame_5.3.2.3 %>%
      filter(Type == Type_0)%>%
      filter(term == "CF")%>%
      left_join(select(Country.Set, Country, hh_expenditures_USD_2014_mean), by = "Country")%>%
      mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
      separate(contrast, c("A", "B"), sep = " - ")%>%
      mutate(B = ifelse(B == "A_Electricity", "Electricity",
                        ifelse(B == "B_LPG", "LPG", B)))%>%
      filter(A != "Unknown")%>%
      mutate(A = ifelse(A == "B_LPG", "LPG", A))%>%
      group_by(B, A)%>%
      arrange(desc(estimate))%>%
      ungroup()%>%
      mutate(number = 1:n())%>%
      mutate(test = fct_reorder(interaction(Country,B,A), number))%>%
      mutate(Category = ifelse(B == "Electricity" & A %in% c("Charcoal", "Firewood", "Coal", "Other biomass"), "Electricity A",
                               ifelse(B == "Electricity", "Electricity B", B)))
    
    for (B_0 in c("Electricity A", "Electricity B", "LPG", "Charcoal")){
      
      labels_data_frame <- expand_grid(B      = data_frame_5.3.2.4$Category,
                                       Type_0 = data_frame_5.3.2.4$Type)%>%
        unique()%>%
        mutate(title_0 = ifelse(B == "Electricity A", "Cooking fuel choice compared to electricity - solid fuels",
                                ifelse(B == "LPG", "Cooking fuel choice compared to LPG",
                                       ifelse(B == "Charcoal", "Cooking fuel choice compared to charcoal", 
                                              ifelse(B == "Electricity B", "Cooking fuel choice compared to electricity - liquid fuels", NA)))))%>%
        mutate(legend_0 = ifelse(B == "Electricity A" | B == "Electricity B", "cooking fuel choice compared to electricity",
                                 ifelse(B == "LPG", "cooking fuel choice compared to LPG", title_0)))%>%
        mutate(bound_0  = ifelse(B == "Electricity A", -0.5,
                                 ifelse(B == "LPG", -0.4,
                                        ifelse(B == "Charcoal",-0.35,
                                               ifelse(B == "Electricity B", -0.5, NA)))))%>%
        mutate(bound_1  = ifelse(B == "Electricity A", 0.8,
                                 ifelse(B == "LPG", 0.5,
                                        ifelse(B == "Charcoal",0.65,
                                               ifelse(B == "Electricity B", 1.1, NA)))))
      
      bound_0   <- labels_data_frame$bound_0[labels_data_frame$B == B_0 & labels_data_frame$B == B_0]
      bound_1   <- labels_data_frame$bound_1[labels_data_frame$B == B_0 & labels_data_frame$B == B_0]
      title_0   <- labels_data_frame$title_0[labels_data_frame$B == B_0 & labels_data_frame$B == B_0]
      legend_0 <- labels_data_frame$legend_0[labels_data_frame$B == B_0 & labels_data_frame$B == B_0]
      if(Type_0 == "affected_upper_80") state_0 <- "higher" else state_0 <- "lower" 
      if(B_0 == "Electricity A" | B_0 == "Electricity B") ATY <- element_text(size = 4) else ATY <- element_text(size = 6)
      
      data_frame_5.3.2.5 <- data_frame_5.3.2.4 %>%
        filter(Category == B_0)
      
      P_5.3.2.5 <- ggplot(data = data_frame_5.3.2.5, aes(x = estimate, y = test))+
        geom_vline(aes(xintercept = 0))+
        geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5, size = 0.3)+
        geom_point(shape = 21, aes (fill = Colour_Type), size = 1.5)+
        theme_bw()+
        facet_wrap(. ~ A, scales = "free_y", ncol = 1)+
        xlab(paste0("Average marginal effect of ",legend_0," on probability of ", state_0 ," 20% of carbon intensity"))+ 
        ylab("Country")+
        labs(colour = "", fill = "")+
        coord_cartesian(xlim = c(bound_0, bound_1))+
        scale_y_discrete(labels = function(x) str_sub(x,1,3))+
        scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
        scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
        ggtitle(title_0)+
        theme(axis.text.y = ATY, 
              axis.text.x = element_text(size = 6),
              axis.title  = element_text(size = 7),
              plot.title = element_text(size = 11),
              legend.position = "bottom",
              # strip.text = element_text(size = 7),
              #strip.text.y = element_text(angle = 180),
              #panel.grid.major = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_line(size = 0.2),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
              panel.border = element_rect(size = 0.3))+
        ggforce::facet_col(vars(A), scales = "free", space = "free")
      
      jpeg(sprintf("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_%s_CF_%s_2017.jpg", Type_0, B_0), width = 15.5, height = 16, unit = "cm", res = 600)
      print(P_5.3.2.5)
      dev.off()
      
    }
    
    rm(P_5.3.2.5, data_frame_5.3.2.5, ATY, B_0, bound_0, bound_1, legend_0, state_0, title_0, Type_0, labels_data_frame, data_frame_5.3.2.4)
    
}

for(i in Country.Set$Country){
  data_frame_5.3.2.6 <- data_frame_5.3.2.3 %>%
    filter(Country == i)%>%
    mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
    separate(contrast, c("A", "B"), sep = " - ")%>%
    group_by(term)%>%
    mutate(min_0 = min(estimate))%>%
    ungroup()%>%
    arrange(min_0, estimate)%>%
    mutate(B = ifelse(B == "A_Electricity", "Electricity",
                      ifelse(B == "B_LPG", "LPG", B)))%>%
    mutate(A = ifelse(A == "A_Electricity", "Electricity",
                      ifelse(A == "B_LPG", "LPG", A)))%>%
    filter(A != "Unknown" | is.na(A))%>%
    mutate(number = 1/1:n())%>%
    mutate(Label = ifelse(term == "urban_01", "Urban citizenship", 
                          ifelse(term == "car.01", "Car ownership",
                                 ifelse(term == "hh_size", "Household size",
                                        ifelse(term == "electricity.access", "Electricity access",
                                               ifelse(term == "motorcycle.01", "Motorcycle own.",
                                                      ifelse(term == "log_hh_expenditures_USD_2014", "Household expenditures",
                                                             ifelse(term == "CF", paste0("CF: ", A), term))))))))%>%
    mutate(order_0 = fct_reorder(Label, number))
  
  bound_0 <- -0.7
  bound_1 <- 1
  
  P_5.3.2.6 <- ggplot(data = data_frame_5.3.2.6, aes(x = estimate, y = order_0))+
    geom_vline(aes(xintercept = 0))+
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5, size = 0.3)+
    geom_point(shape = 21, aes (fill = Colour_Type), size = 1.5)+
    theme_bw()+
    xlab(paste0("Average marginal effect on probability of upper 20% of carbon intensity"))+ 
    ylab("Variable")+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(bound_0, bound_1))+
    # scale_y_discrete(labels = function(x) str_sub(x,1,3))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
    scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
    ggtitle(Country.Set$Country_long[Country.Set$Country == i])+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6),
          axis.title  = element_text(size = 6),
          plot.title = element_text(size = 9),
          legend.position = "bottom",
          # strip.text = element_text(size = 7),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  if(nrow(data_frame_5.3.2.6) < 6) height_0 <- 4 else height_0 <- 8
  
  jpeg(sprintf("1_Figures/Analysis_Logit_Models_Marginal_Effects/Country/Average_Marginal_Effects_2017_%s.jpg", i), width = 10, height = height_0, unit = "cm", res = 600)
  print(P_5.3.2.6)
  dev.off()
  
  print(i)
  
}

rm(data_frame_5.3.2.1, data_frame_5.3.2.3, data_5.3, Type_0, i)

# _______ ####
# 6       ML-supported analysis ####

# 6.1     Boosted Regression trees on carbon intensity of consumption ####

Country.Set.Test.1 <- c("BEL", "NOR", "LBR")

track <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT.xlsx")

set.seed(2023)

options(warn = 1)

# Takes seven hours

for (i in Country.Set$Country){
  tryCatch({
    
    track_0 <- data.frame(Country = i, date = date())
    
    run_ID <- if(i %in% track$Country) paste0(i, "_", max(track$number[track$Country == i])+1) else paste0(i, "_",1)
    
    print(paste0("Start ", i, ": ", run_ID))
    
    # Filter only observations for country of interest  
    data_6.1 <- filter(data_2, Country == i)
    
    track_0$observations_sample = nrow(data_6.1)
    
    # Feature engineering - Step 1 (with dplyr)
    
    data_6.1.1 <- data_6.1 %>%
      # select relevant variables
      select(Country, hh_id, hh_weights, hh_size,
             Province, urban_01, District,
             sex_hhh, ISCED, Ethnicity, Religion, Nationality, Language, religiosity,
             electricity.access, HF, LF, CF, 
             hh_expenditures_USD_2014, 
             car.01, motorcycle.01, refrigerator.01, ac.01, tv.01, washing_machine.01, 
             carbon_intensity_kg_per_USD_national)%>%
      # remove redundant variables
      select(-Country, -hh_id, -hh_weights)%>%
      # can be included for "sanity check"
      # mutate(noise = rnorm(n(),0,1))%>%
      # include hh_weights later in the process
      # factors instead of characters
      mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
      mutate_at(vars(sex_hhh, ISCED, religiosity, urban_01, ends_with(".01"), electricity.access), list(~ as.factor(.)))
    
    # Country-specific edits 
    if(i == "SWE"){data_6.1.1 <- select(data_6.1.1, -ISCED, -sex_hhh)}
    if(i == "NLD"){data_6.1.1 <- select(data_6.1.1, -ISCED)}
    if(i == "ARG" | i == "GEO"){data_6.1.1 <- select(data_6.1.1, -electricity.access)}
    if(!i %in% c("ARG", "ARM", "AUT", "BEL", "BOL", "CHE", "DEU", "ESP", "FIN", "FRA",
                 "GRC", "HUN", "ITA", "JOR", "NLD", "POL", "PRT", "ROU", "SUR", "SWE")){data_6.1.1 <- select(data_6.1.1, -District)}
    
    rm(data_6.1)
    
    # Splitting the sample, but no strata
    
    prop_0 = 0.80
    # if(i == "IDN"){prop_0 <- 0.2}
    # if(i == "IND"){prop_0 <- 0.3}
    # if(i == "MEX"){prop_0 <- 0.5}
    
    data_6.1.2 <- data_6.1.1 %>%
      initial_split(prop = prop_0)
    
    # Data for training
    data_6.1.2.train <- data_6.1.2 %>%
      training()
    
    # Data for testing
    data_6.1.2.test <- data_6.1.2 %>%
      testing()
    
    rm(data_6.1.1, data_6.1.2)
    
    # Feature engineering - Step 2 (with recipe)
    
    recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                           data = data_6.1.2.train)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), threshold = 0.05)
    
    data_6.1.2.training <- recipe_6.1.0 %>%
      prep(training = data_6.1.2.train)%>%
      bake(new_data = NULL)
    
    data_6.1.2.testing <- recipe_6.1.0 %>%
      prep(training = data_6.1.2.test)%>%
      bake(new_data = NULL) 
    
    # Five-fold cross-validation
    
    folds_6.1 <- vfold_cv(data_6.1.2.training, v = 5)
    
    # Setup model to be tuned
    
    model_brt <- boost_tree(
      trees         = 1000,
      tree_depth    = tune(), # maximum depth of tree
      learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
      # min_n       = tune(),
      mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
      # stop_iter   = tune(),
      # sample_size = tune()
    )%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    # Create a tuning grid - 16 different models for the tuning space
    
    grid_0 <- grid_latin_hypercube(
      tree_depth(),
      learn_rate(c(-3,-0.5)),# tuning parameters
      mtry(c(round((ncol(data_6.1.2.training)-1)/2,0), ncol(data_6.1.2.training)-1)),
      size = 29)%>%
      # default parameters
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_6.1.2.training)-1))
    
    # Tune the model - cover the entire parameter space without running every combination
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    model_brt_1 <- tune_grid(model_brt,
                             carbon_intensity_kg_per_USD_national ~ .,
                             resamples = folds_6.1,
                             grid      = grid_0,
                             metrics   = metric_set(mae, rmse, rsq))
    
    time_2 <- Sys.time()
    
    doParallel::stopImplicitCluster()
    
    track_0$tuning_time <- as.integer(difftime(time_1, time_2, units = "min"))
    
    # Collect metrics of tuned models
    
    metrics_1 <- collect_metrics(model_brt_1)
    
    model_brt_1.1 <- select_best(model_brt_1, metric = "mae")
    
    metrics_1.1 <- metrics_1 %>%
      filter(.config == model_brt_1.1$.config[1])
    
    # Output: best model after tuning
    track_0 <- bind_cols(track_0, model_brt_1.1)%>%
      rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate, mtry_best = mtry)%>%
      select(-.config)%>%
      mutate(mae_mean_cv_5_train  = metrics_1.1$mean[metrics_1.1$.metric == "mae"],
             rmse_mean_cv_5_train = metrics_1.1$mean[metrics_1.1$.metric == "rmse"],
             rsq_mean_cv_5_train  = metrics_1.1$mean[metrics_1.1$.metric == "rsq"])
    
    track <- track %>%
      bind_rows(track_0)
    
    rm(track_0, run_ID, time_1, time_2, grid_0,
       model_brt, model_brt_1, model_brt_1.1, metrics_1.1, metrics_1,
       data_6.1.2.test, data_6.1.2.testing, data_6.1.2.train, data_6.1.2.training,
       folds_6.1, recipe_6.1.0, prop_0)
    
    gc()
    
    print(paste0("End: ", i, " ", Sys.time()))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

track_1 <- track %>%
  group_by(Country)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  mutate(number_ob = paste0(Country, "_", number))%>%
  select(number_ob, everything())%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT_2017.xlsx")

rm(track)

# 6.1.1   Calculate model evaluation and SHAP-values ####

# SHAP expresses feature importance based on the marginal contribution of each predictor for each observation. Has local explanation and consistency.

Country.Set.Test.2 <- c("ARG", "JOR")

Country.Set.Test.3 <- c("BOL", "PRT", "BEL", "CHE", "AUT", "NLD", "SWE", "FRA", "ITA", "ESP", "GRC", "SUR")

# eval_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation.xlsx")
eval_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")
# eval_0 <- data.frame()
# shap_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail.xlsx") 
shap_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD_2017.xlsx") 
# shap_0 <- data.frame()
# shap_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification.xlsx")
# shap_1 <- data.frame()
shap_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification_VFOLD_2017.xlsx")

track_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT_2017.xlsx")

for (i in Country.Set.Test.2){
  tryCatch({
    
    print(paste0("Start: ", i, " ", Sys.time()))

    track <- track_0 %>%
      filter(Country == i)%>%
      #dplyr::slice(which.min(mae_mean_cv_5_train))
      dplyr::slice(which.max(number))
    
    # if(i %in% c("MEX", "IND", "IDN")){
    #   track <- track_0 %>%
    #     filter(Country == i)%>%
    #     filter(number == max(number))%>%
    #     ungroup()
    # }
    # 
    # if(i %in% c("BEL", "NOR", "LBR")){
    #   track <- track_0 %>%
    #     filter(Country == i)%>%
    #     filter(tree_depth_best != 1)%>%
    #     filter(number == max(number))%>%
    #     ungroup()
    # }
    
    data_6.1 <- filter(data_2, Country == i)
    # data_6.1 <- read_csv("C:/Users/misl/Desktop/Israel_Test_Set.csv")
    
    # Feature engineering - Part I
    
    data_6.1.1 <- data_6.1 %>%
      # select relevant variables
      select(Country, hh_id, hh_weights, hh_size,
             Province, urban_01, District,
             sex_hhh, ISCED, Ethnicity, Religion, Nationality, Language, religiosity,
             electricity.access, HF, LF, CF, 
             hh_expenditures_USD_2014, 
             car.01, motorcycle.01, refrigerator.01, ac.01, tv.01, washing_machine.01, 
             carbon_intensity_kg_per_USD_national)%>%
      # remove redundant variables
      select(-Country, -hh_id, -hh_weights)%>%
      # can be included for "sanity check"
      # mutate(noise = rnorm(n(),0,1))%>%
      # include hh_weights later in the process
      # factors instead of characters
      mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
      mutate_at(vars(sex_hhh, ISCED, religiosity, urban_01, ends_with(".01"), electricity.access), list(~ as.factor(.)))
    
    # Country-specific edits 
    if(i == "SWE"){data_6.1.1 <- select(data_6.1.1, -ISCED, -sex_hhh)}
    if(i == "NLD"){data_6.1.1 <- select(data_6.1.1, -ISCED)}
    if(i == "ARG" | i == "GEO"){data_6.1.1 <- select(data_6.1.1, -electricity.access)}
    if(!i %in% c("ARG", "ARM", "AUT", "BEL", "BOL", "CHE", "DEU", "ESP", "FIN", "FRA",
                 "GRC", "HUN", "ITA", "JOR", "NLD", "POL", "PRT", "ROU", "SUR", "SWE")){data_6.1.1 <- select(data_6.1.1, -District)}
    
    rm(data_6.1)
    
    # Feature engineering - Step 2 (with recipe) - with dummification
    
    recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                           data = data_6.1.1)%>%
      # Deletes all columns with any NA
      step_filter_missing(all_predictors(), threshold = 0)%>%
      # Remove minimum number of columns such that correlations are less than 0.9
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), -ends_with("electricity.access"), -ends_with("ISCED"), threshold = 0.05)%>%
      # including dummification
      step_dummy(all_nominal())
    
    if(i %in% c("MMR", "GHA", "CIV", "GTM", "BRB", "TGO")){
      recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                             data = data_6.1.1)%>%
        # Deletes all columns with any NA
        step_filter_missing(all_predictors(), threshold = 0)%>%
        # Remove minimum number of columns such that correlations are less than 0.9
        step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
        # should have very few unique observations for factors
        step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), -ends_with("electricity.access"), -ends_with("ISCED"), threshold = 0.05)%>%
        # including dummification
        step_dummy(all_nominal())
    }
    
    if(i %in% c("PER")){
      recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                             data = data_6.1.1)%>%
        # Deletes all columns with any NA
        step_filter_missing(all_predictors(), threshold = 0)%>%
        # Remove minimum number of columns such that correlations are less than 0.9
        step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
        # should have very few unique observations for factors
        step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), -ends_with("electricity.access"), -ends_with("ISCED"), -ends_with("LF"), -ends_with("CF"), threshold = 0.1)%>%
        step_other("CF", "LF", threshold = 0.05)%>%
        # including dummification
        step_dummy(all_nominal())
    }
    
    data_6.1.2 <- recipe_6.1.0 %>%
      prep(training = data_6.1.1)%>%
      bake(new_data = NULL)%>%
      # for cross-validation
      mutate(id = 1:n())
    
    folds_6.1.2 <- vfold_cv(data_6.1.2, v = 5)
    
    data_6.1.3 <- data_6.1.2 %>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[1]]$in_id), in1 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[2]]$in_id), in2 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[3]]$in_id), in3 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[4]]$in_id), in4 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[5]]$in_id), in5 = 1), by = c("id"))%>%
      mutate_at(vars(in1:in5), ~ ifelse(is.na(.),0,.))%>%
      mutate(fold_test = ifelse(in1 == 0, 1,
                                ifelse(in2 == 0, 2,
                                       ifelse(in3 == 0, 3,
                                              ifelse(in4 == 0, 4,
                                                     ifelse(in5 == 0, 5, NA))))))%>%
      select(-in1, -in2, -in3, -in4, -in5, -id)
    
    data_6.1.2.raw <- data_6.1.1 %>%
      mutate(id = 1:n())%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[1]]$in_id), in1 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[2]]$in_id), in2 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[3]]$in_id), in3 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[4]]$in_id), in4 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.1.2$splits[[5]]$in_id), in5 = 1), by = c("id"))%>%
      mutate_at(vars(in1:in5), ~ ifelse(is.na(.),0,.))%>%
      mutate(fold_test = ifelse(in1 == 0, 1,
                                ifelse(in2 == 0, 2,
                                       ifelse(in3 == 0, 3,
                                              ifelse(in4 == 0, 4,
                                                     ifelse(in5 == 0, 5, NA))))))%>%
      select(-in1, -in2, -in3, -in4, -in5, -id)
      
    # Model to be tuned
    model_brt <- boost_tree(
      trees         = 1000,
      tree_depth    = track$tree_depth_best[1],
      learn_rate    = track$learn_rate_best[1], # the higher the learning rate the faster - default 0.3
      mtry          = track$mtry_best[1]
    )%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    shap_6.1.2.0 <- data.frame()
    
    for(v in c(1:5)){
      print(v)
      
      # Training dataset
      data_6.1.2.training <- data_6.1.3 %>%
        filter(fold_test != v)%>%
        select(-fold_test)
      
      # Testing dataset
      data_6.1.2.testing <- data_6.1.3 %>%
        filter(fold_test == v)%>%
        select(-fold_test)
      
      data_6.1.2.test <- data_6.1.2.raw %>%
        filter(fold_test == v)%>%
        select(-fold_test)
      
      # Entire dataset
      # data_6.1.2.all <- recipe_6.1.0 %>%
      #   prep(training = data_6.1.2.train)%>%
      #   bake(new_data = data_6.1.1)
      
      # Fit model on training dataset
      model_brt_1 <- model_brt %>%
        fit(carbon_intensity_kg_per_USD_national ~ .,
            data = data_6.1.2.training)
      
      # # Evaluation on training dataset 
      # predictions_6.1.1 <- augment(model_brt_1, new_data = data_6.1.2.training)
      # mae_6.1.1  <- mae(predictions_6.1.1,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # rmse_6.1.1 <- rmse(predictions_6.1.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # rsq_6.1.1  <- rsq(predictions_6.1.1,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # eval_6.1.1 <- bind_rows(mae_6.1.1, rmse_6.1.1, rsq_6.1.1)%>%
      #   mutate(Type = "Training")
      
      # Evalutation on testing dataset
      predictions_6.1.2 <- augment(model_brt_1, new_data = data_6.1.2.testing)
      mae_6.1.2  <- mae(predictions_6.1.2,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      rmse_6.1.2 <- rmse(predictions_6.1.2, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      rsq_6.1.2  <- rsq(predictions_6.1.2,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      eval_6.1.2 <- bind_rows(mae_6.1.2, rmse_6.1.2, rsq_6.1.2)%>%
        mutate(Type = "Testing")%>%
        mutate(fold = v)%>%
        mutate(number_ob = track$number_ob[1])%>%
        select(-.estimator)%>%
        pivot_wider(names_from = "Type", values_from = ".estimate", names_prefix = "Sample_")%>%
        mutate(Country = i)%>%
        select(Country, number_ob, .metric, fold, Sample_Testing)
      
      eval_0 <- eval_0 %>%
        filter(Country != i | fold != v)%>%
        bind_rows(eval_6.1.2)
      
      rm(predictions_6.1.2, mae_6.1.2, rmse_6.1.2, rsq_6.1.2, eval_6.1.2)
      
      # SHAP - use testing data for evaluation
      
      data_6.1.2.testing_matrix <- data_6.1.2.testing %>%
        #sample_n(fraction)%>%
        select(-carbon_intensity_kg_per_USD_national)%>%
        as.matrix()
      
      # data_6.1.2.all_matrix <- data_6.1.2.all %>%
      #   select(-carbon_intensity_kg_per_USD_national)%>%
      #   as.matrix()
      
      shap_6.1.2 <- predict(extract_fit_engine(model_brt_1),
                            data_6.1.2.testing_matrix,
                            predcontrib = TRUE,
                            approxcontrib = FALSE)
      
      shap_6.1.2.1 <- shap_6.1.2 %>%
        as_tibble()%>%
        summarise_all(~ mean(abs(.)))%>%
        select(-BIAS)%>%
        pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
        arrange(desc(SHAP_contribution))%>%
        mutate(tot_contribution = sum(SHAP_contribution))%>%
        mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
        select(-tot_contribution)
      
      # Edit variable names 
      
      shap_6.1.2.2 <- shap_6.1.2.1 %>%
        mutate(var_1 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), NA, str_remove(variable, "^[^_]*")))%>%
        mutate(var_1 = str_remove(var_1, "_X"))%>%
        mutate(var_1 = str_remove(var_1, "_"))%>%
        mutate(var_1 = str_replace_all(var_1, "\\.", " "))%>%
        mutate(var_0 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), str_remove(variable, "_X."), str_remove(variable, "_.*")))%>%
        select(var_0, var_1, everything(), -variable)%>%
        rename(Variable = var_0)%>%
        mutate(Var_0 = ifelse(grepl("District", Variable), "District", 
                              ifelse(grepl("Province", Variable), "Province", 
                                     ifelse(grepl("ISCED", Variable), "ISCED", 
                                            ifelse(grepl("Ethnicity", Variable), "Ethnicity", 
                                                   ifelse(grepl("Religion", Variable), "Religion", 
                                                          ifelse(Variable == "hh_expenditures_USD_2014", "HH expenditures",
                                                                 ifelse(Variable == "hh_size", "HH size",
                                                                        ifelse(grepl("car.01", Variable), "Car own.",
                                                                               ifelse(grepl("urban_01", Variable), "Urban",
                                                                                      ifelse(grepl("sex_hhh", Variable), "Gender HHH",
                                                                                             ifelse(grepl("CF_", Variable), "Cooking", 
                                                                                                    ifelse(grepl("HF_", Variable), "Heating", 
                                                                                                           ifelse(grepl("LF_", Variable), "Lighting", 
                                                                                                                  ifelse(Variable == "electricity.access", "Electricity access", Variable)))))))))))))))%>%
        mutate(Var_0 = ifelse(Var_0 == "religiosity", "Religiosity",
                              ifelse(Var_0 %in% c("refrigerator.01", "ac.01", "tv.01", "washing_machine.01"), "Appliance own.", 
                                     ifelse(Var_0 == "motorcycle.01", "Motorcycle own.", Var_0))))%>%
        mutate(order_number = 1:n())%>%
        group_by(Var_0)%>%
        mutate(order_number_2 = min(order_number))%>%
        ungroup()%>%
        arrange(order_number_2, order_number)%>%
        mutate(var_1 = ifelse(Variable == "refrigerator.01", "Refrigerator",
                              ifelse(Variable == "washing_machine.01", "Washing machine",
                                     ifelse(Variable == "ac.01", "AC",
                                            ifelse(Variable == "tv.01", "TV", 
                                                   ifelse(Variable == "sex_hhh", "Female HHH",
                                                          ifelse(Variable == "motorcycle.01", NA, var_1)))))))%>%
        rename(Var_1 = var_1)%>%
        select(Var_0, Var_1, everything(), - Variable)%>%
        select(-order_number, -order_number_2)%>%
        mutate(Country = i)%>%
        mutate(number_ob = track$number_ob[1])%>%
        mutate(fold = v)
      
      shap_0 <- shap_0 %>%
        filter(Country != i | fold != v)%>%
        bind_rows(shap_6.1.2.2)
      
      # Sum up and aggregate for output
      
      shap_6.1.2.3 <- shap_6.1.2.2 %>%
        group_by(Var_0)%>%
        summarise_at(vars(share_SHAP), ~ sum(.))%>%
        ungroup()%>%
        mutate(help_0 = ifelse(share_SHAP < 0.03,1,0))%>%
        arrange(share_SHAP)
      
      shap_6.1.2.3.1 <- shap_6.1.2.3 %>%
        filter(help_0 == 1)%>%
        summarise(share_SHAP = sum(share_SHAP))%>%
        mutate(Var_0 = "Other features (Sum)")
      
      shap_6.1.2.3.2 <- shap_6.1.2.3 %>%
        filter(help_0 == 0)%>%
        arrange(desc(share_SHAP))%>%
        bind_rows(shap_6.1.2.3.1)%>%
        select(-help_0)%>%
        mutate(Country = i)%>%
        select(Country, everything())%>%
        mutate(number_ob = track$number_ob[1])%>%
        mutate(fold = v)
      
      shap_1 <- shap_1 %>%
        filter(Country != i | fold != v)%>%
        bind_rows(shap_6.1.2.3.2)
      
      # SHAP hh_expenditures_USD_2014
      
      shap_6.1.2.4 <- shap_6.1.2 %>% 
        as_tibble()%>%
        select(hh_expenditures_USD_2014)%>%
        rename(SHAP_hh_expenditures_USD_2014 = hh_expenditures_USD_2014)%>%
        bind_cols(select(data_6.1.2.testing, hh_expenditures_USD_2014))%>%
        mutate(Country = i)%>%
        mutate(id = 1:n())%>%
        select(Country, id, everything())
      
      VOI <- shap_6.1.2.3.2 %>%
        slice_head(n = 8)%>%
        filter(Var_0 != "HH expenditures")
      
      shap_6.1.2.5 <- shap_6.1.2 %>%
        as_tibble()%>%
        pivot_longer(everything(), names_to = "variable", values_to = "SHAP")%>%
        mutate(var_1 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), NA, str_remove(variable, "^[^_]*")))%>%
        mutate(var_1 = str_remove(var_1, "_X"))%>%
        mutate(var_1 = str_remove(var_1, "_"))%>%
        mutate(var_1 = str_replace_all(var_1, "\\.", " "))%>%
        mutate(var_0 = ifelse(variable %in% c("hh_size", "hh_expenditures_USD_2014") | grepl("sex_hhh", variable) | grepl(".01", variable), str_remove(variable, "_X."), str_remove(variable, "_.*")))%>%
        select(var_0, var_1, everything(), -variable)%>%
        rename(Variable = var_0)%>%
        mutate(Var_0 = ifelse(grepl("District", Variable), "District", 
                              ifelse(grepl("Province", Variable), "Province", 
                                     ifelse(grepl("ISCED", Variable), "ISCED", 
                                            ifelse(grepl("Ethnicity", Variable), "Ethnicity", 
                                                   ifelse(grepl("Religion", Variable), "Religion", 
                                                          ifelse(Variable == "hh_expenditures_USD_2014", "HH expenditures",
                                                                 ifelse(Variable == "hh_size", "HH size",
                                                                        ifelse(grepl("car.01", Variable), "Car own.",
                                                                               ifelse(grepl("urban_01", Variable), "Urban",
                                                                                      ifelse(grepl("sex_hhh", Variable), "Gender HHH",
                                                                                             ifelse(grepl("CF_", Variable), "Cooking", 
                                                                                                    ifelse(grepl("HF_", Variable), "Heating", 
                                                                                                           ifelse(grepl("LF_", Variable), "Lighting", 
                                                                                                                  ifelse(Variable == "electricity.access", "Electricity access", Variable)))))))))))))))%>%
        mutate(Var_0 = ifelse(Var_0 == "religiosity", "Religiosity",
                              ifelse(Var_0 %in% c("refrigerator.01", "ac.01", "tv.01", "washing_machine.01"), "Appliance own.", 
                                     ifelse(Var_0 == "motorcycle.01", "Motorcycle own.", Var_0))))%>%
        mutate(var_1 = ifelse(Variable == "refrigerator.01", "Refrigerator",
                              ifelse(Variable == "washing_machine.01", "Washing machine",
                                     ifelse(Variable == "ac.01", "AC",
                                            ifelse(Variable == "tv.01", "TV", 
                                                   ifelse(Variable == "sex_hhh", "Female HHH",
                                                          ifelse(Variable == "motorcycle.01", NA, var_1)))))))%>%
        rename(Var_1 = var_1)%>%
        select(Var_0, Var_1, everything(), - Variable)%>%
        filter(Var_0 %in% VOI$Var_0)
      
      shap_6.1.2.6 <- data.frame(id = 1:nrow(data_6.1.2.testing))
      
      # Binaere bzw. numerische Variablen
      
      for(j in c("HH size", "Urban", "Car own.", "Motorcycle own.", "Electricity access")){
        
        if(j %in% VOI$Var_0){
          shap_6.1.2.5.1 <- shap_6.1.2.5 %>%
            filter(Var_0 == j)%>%
            select(-Var_1)%>%
            mutate(id      = 1:n())%>%
            pivot_wider(names_from = "Var_0", values_from = "SHAP", names_prefix = "SHAP_")
          
          if(j == "HH size"){
            shap_6.1.2.5.1 <- bind_cols(shap_6.1.2.5.1, select(data_6.1.2.testing, hh_size))
          }
          
          if(j == "Electricity access"){
            shap_6.1.2.5.1 <- bind_cols(shap_6.1.2.5.1, select(data_6.1.2.testing, electricity.access_X1))%>%
              rename(electricity.access = electricity.access_X1)
          }
          
          if(j == "Urban"){
            shap_6.1.2.5.1 <- bind_cols(shap_6.1.2.5.1, select(data_6.1.2.testing, urban_01_X1))%>%
              rename(urban_01 = urban_01_X1)
          }
          
          if(j == "Car own."){
            shap_6.1.2.5.1 <- bind_cols(shap_6.1.2.5.1, select(data_6.1.2.testing, car.01_X1))%>%
              rename(car.01 = car.01_X1)
          }
          
          if(j == "Motorcycle own."){
            shap_6.1.2.5.1 <- bind_cols(shap_6.1.2.5.1, select(data_6.1.2.testing, motorcycle.01_X1))%>%
              rename(motorcycle.01 = motorcycle.01_X1)
          }
          
          shap_6.1.2.6 <- left_join(shap_6.1.2.6, shap_6.1.2.5.1, by = "id")
        }
      }
      
      # Kategorische Variablen / Faktorvariablen
      
      shap_6.1.2.7 <- data.frame(id = 1:nrow(data_6.1.2.testing))
      
      for(k in c("Province", "CF", "Appliance own.", "District", "ISCED", "Ethnicity", "Gender HHH", "LF", "HF", "Religion", "Nationality", "Religiosity")){
        if(k %in% VOI$Var_0){
          shap_6.1.2.5.2 <- shap_6.1.2.5 %>%
            filter(Var_0 == k)%>%
            mutate(id = rep(1:nrow(data_6.1.2.testing), each = n()/nrow(data_6.1.2.testing), length.out = nrow(.)))
          
          if(k == "Province"){
            if(i == "ISR"){
              data_6.1.2.test <- data_6.1.2.test %>%
                mutate(Province = str_replace(Province, "\\(", " "))%>%
                mutate(Province = str_replace(Province, "\\)"," "))
            }
            
            if(i == "BRB" | i == "NGA"){
              data_6.1.2.test <- data_6.1.2.test %>%
                mutate(Province = str_replace(Province, "\\.", " "))
            }
            
            if(i == "MAR" | i == "GEO" | i == "JOR"){
              data_6.1.2.test <- data_6.1.2.test %>%
                mutate(Province = str_replace_all(Province, "-", " "))
            }
            
            shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              filter(Province == Var_1)%>%
              select(-Var_0, -Var_1)%>%
              rename(SHAP_Province = SHAP)%>%
              select(id, everything())%>%
              mutate(Province = as.character(Province))
            
            # shap_6.1.2.5.4 <- select(data_6.1.2.test, starts_with(k))%>%
            #   mutate(id = 1:n())%>%
            #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #   distinct(Province)%>%
            #   mutate(Province = as.character(Province))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, Province), Province_0 = as.character(Province)))%>%
              mutate(Province = ifelse(is.na(Province), Province_0, Province))%>%
              select(-Province_0)
          }
          
          if(k == "CF"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              mutate(rows = n())%>%
              mutate(CF = as.character(CF))%>%
              group_by(CF)%>%
              mutate(share_CF = n()/rows)%>%
              ungroup()%>%
              mutate(CF = ifelse(share_CF < 0.05, "other", CF))%>%
              filter(CF == Var_1)%>%
              select(-CF, -Var_0)%>%
              rename(SHAP_CF = SHAP, CF = Var_1)%>%
              select(id, everything())%>%
              mutate(CF = as.character(CF))
            
            # shap_6.1.2.5.4 <- select(data_6.1.2.test, starts_with(k))%>%
            #   mutate(id = 1:n())%>%
            #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #   distinct(CF)%>%
            #   mutate(CF = as.character(CF))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, CF), CF_0 = as.character(CF)))%>%
              mutate(CF = ifelse(is.na(CF), CF_0, CF))%>%
              select(-CF_0, - share_CF, -rows)
          }
          
          if(k == "HF"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              mutate(rows = n())%>%
              mutate(HF = as.character(HF))%>%
              group_by(HF)%>%
              mutate(share_HF = n()/rows)%>%
              ungroup()%>%
              mutate(HF = ifelse(share_HF < 0.05, "other", HF))%>%
              filter(HF == Var_1)%>%
              select(-HF, -Var_0)%>%
              rename(SHAP_HF = SHAP, HF = Var_1)%>%
              select(id, everything())%>%
              mutate(HF = as.character(HF))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, HF), HF_0 = as.character(HF)))%>%
              mutate(HF = ifelse(is.na(HF), HF_0, HF))%>%
              select(-HF_0, - share_HF, -rows)
          }
          
          if(k == "LF"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              mutate(rows = n())%>%
              mutate(LF = as.character(LF))%>%
              group_by(LF)%>%
              mutate(share_LF = n()/rows)%>%
              ungroup()%>%
              mutate(LF = ifelse(share_LF < 0.05, "other", LF))%>%
              filter(LF == Var_1)%>%
              select(-LF, -Var_0)%>%
              rename(SHAP_LF = SHAP, LF = Var_1)%>%
              select(id, everything())%>%
              mutate(LF = as.character(LF))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, LF), LF_0 = as.character(LF)))%>%
              mutate(LF = ifelse(is.na(LF), LF_0, LF))%>%
              select(-LF_0,- share_LF, -rows)
          }
          
          if(k == "Appliance own."){
            shap_6.1.2.5.3 <- select(data_6.1.2.test, ends_with("refrigerator.01"), ends_with("ac.01"), ends_with("tv.01"), ends_with("washing_machine.01"))%>%
              mutate(id = 1:n())%>%
              select_if(function(x) any(!is.na(x)))%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              mutate(Var_1 = ifelse(Var_1 == "Washing machine", "Washing_machine", Var_1))%>%
              pivot_wider(names_from = "Var_1", values_from = "SHAP", names_prefix = "SHAP_")%>%
              select(-Var_0)%>%
              select(id, everything())
            
            shap_6.1.2.5.5 <- shap_6.1.2.5.3
          }
          
          if(k == "District"){
            
            #if(){
            #  shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
            #    mutate(id = 1:n())%>%
            #    left_join(shap_6.1.2.5.2, by = "id")%>%
            #    pivot_longer(starts_with("District_"), names_to = "District", values_to = "values", names_prefix = "District_")%>%
            #    filter(values == 1)%>%
            #    filter(District == Var_1)%>%
            #    select(-Var_0, -Var_1, - values)%>%
            #    rename(SHAP_District = SHAP)%>%
            #    select(id, everything())%>%
            #    mutate(District = as.character(District))
            #  
            #  # shap_6.1.2.5.4 <- select(data_6.1.2.test, starts_with(k))%>%
            #  #   mutate(id = 1:n())%>%
            #  #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #  #   distinct(District)%>%
            #  #   mutate(District = as.character(District))
            #  
            #  shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
            #    bind_cols(transmute(select(data_6.1.2.test, District), District_0 = as.character(District)))%>%
            #    mutate(District = ifelse(is.na(District), District_0, District))%>%
            #    select(-District_0)
            #}
            
            if(k == "District"){
              data_6.1.2.test <- data_6.1.2.test %>%
                mutate(District = str_replace(District, "\\.", " "))%>%
                mutate(District = str_replace_all(District, "-", " "))
              
              shap_6.1.2.5.3 <- select(data_6.1.2.test, starts_with(k))%>%
                mutate(id = 1:n())%>%
                left_join(shap_6.1.2.5.2, by = "id")%>%
                filter(District == Var_1)%>%
                select(-Var_0, -Var_1)%>%
                rename(SHAP_District = SHAP)%>%
                select(id, everything())%>%
                mutate(District = as.character(District))
              
              shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
                bind_cols(transmute(select(data_6.1.2.test, District), District_0 = as.character(District)))%>%
                mutate(District = ifelse(is.na(District), District_0, District))%>%
                select(-District_0)
              
            }
            
            
          }
          
          if(k == "ISCED"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.testing, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              pivot_longer(starts_with("ISCED_"), names_to = "ISCED", values_to = "values", names_prefix = "ISCED_")%>%
              mutate(ISCED = str_remove(ISCED, "X"))%>%
              filter(values == 1)%>%
              filter(ISCED == Var_1)%>%
              select(-Var_0, -Var_1, -values)%>%
              rename(SHAP_ISCED = SHAP)%>%
              select(id, everything())
            
            # shap_6.1.2.5.4 <- select(data_6.1.2.test, starts_with(k))%>%
            #   mutate(id = 1:n())%>%
            #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #   distinct(ISCED)
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, ISCED), ISCED_0 = as.character(ISCED)))%>%
              mutate(ISCED = ifelse(is.na(ISCED), ISCED_0, ISCED))%>%
              select(-ISCED_0)
          }
          
          if(k == "Ethnicity"){
            
            if(i == "BOL"){
              data_6.1.2.test <- data_6.1.2.test %>%
                mutate(Ethnicity = str_replace_all(Ethnicity, "\\.", " "))
            }
            
            shap_6.1.2.5.3 <- select(data_6.1.2.testing, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              pivot_longer(starts_with("Ethnicity_"), names_to = "Ethnicity", values_to = "values", names_prefix = "Ethnicity_")%>%
              mutate(Ethnicity = str_remove(Ethnicity, "X"))%>%
              filter(values == 1)%>%
              mutate(Ethnicity = str_replace_all(Ethnicity, "\\.", " "))%>%
              filter(Ethnicity == Var_1)%>%
              select(-Ethnicity, -Var_0, -values)%>%
              rename(SHAP_Ethnicity = SHAP, Ethnicity = Var_1)%>%
              select(id, everything())%>%
              mutate(Ethnicity = as.character(Ethnicity))
            
            # shap_6.1.2.5.4 <- select(data_6.1.2.test, starts_with(k))%>%
            #   mutate(id = 1:n())%>%
            #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #   distinct(Ethnicity)
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, Ethnicity), Ethnicity_0 = as.character(Ethnicity)))%>%
              mutate(Ethnicity = ifelse(is.na(Ethnicity), Ethnicity_0, Ethnicity))%>%
              select(-Ethnicity_0)
            
          }
          
          if(k == "Religion"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.testing, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              pivot_longer(starts_with("Religion_"), names_to = "Religion", values_to = "values", names_prefix = "Religion_")%>%
              mutate(Religion = str_replace_all(Religion, "\\.", " "))%>%
              mutate(Religion = str_remove(Religion, "X"))%>%
              filter(values == 1)%>%
              filter(Religion == Var_1)%>%
              select(-Religion, -Var_0, -values)%>%
              rename(SHAP_Religion = SHAP, Religion = Var_1)%>%
              select(id, everything())%>%
              mutate(Religion = as.character(Religion))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, Religion), Religion_0 = as.character(Religion)))%>%
              mutate(Religion = ifelse(is.na(Religion), Religion_0, Religion))%>%
              select(-Religion_0)
            
          }
          
          if(k == "Religiosity"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.testing, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              pivot_longer(starts_with("Religiosity"), names_to = "Religiosity", values_to = "values", names_prefix = "religiosity_")%>%
              mutate(Religiosity = str_remove(Religiosity, "X"))%>%
              filter(values == 1)%>%
              filter(Religiosity == Var_1)%>%
              select(-Religiosity, -Var_0, -values)%>%
              rename(SHAP_Religiosity = SHAP, Religiosity = Var_1)%>%
              select(id, everything())%>%
              mutate(Religiosity = as.character(Religiosity))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, religiosity), Religiosity_0 = as.character(religiosity)))%>%
              mutate(Religiosity = ifelse(is.na(Religiosity), Religiosity_0, Religiosity))%>%
              select(-Religiosity_0)
            
          }
          
          if(k == "Nationality"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.testing, starts_with(k))%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              pivot_longer(starts_with("Nationality_"), names_to = "Nationality", values_to = "values", names_prefix = "Nationality_")%>%
              filter(values == 1)%>%
              filter(Nationality == Var_1)%>%
              select(-Nationality, -Var_0, -values)%>%
              rename(SHAP_Nationality = SHAP, Nationality = Var_1)%>%
              select(id, everything())%>%
              mutate(Nationality = as.character(Nationality))
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              bind_cols(transmute(select(data_6.1.2.test, Nationality), Nationality_0 = as.character(Nationality)))%>%
              mutate(Nationality = ifelse(is.na(Nationality), Nationality_0, Nationality))%>%
              select(-Nationality_0)
            
          }
          
          if(k == "Gender HHH"){
            
            shap_6.1.2.5.3 <- select(data_6.1.2.test, "sex_hhh")%>%
              mutate(id = 1:n())%>%
              left_join(shap_6.1.2.5.2, by = "id")%>%
              mutate(sex_hhh = as.numeric(sex_hhh))%>%
              filter(sex_hhh == max(sex_hhh))%>%
              select(-sex_hhh, -Var_0)%>%
              rename(SHAP_Gender = SHAP, Gender = Var_1)%>%
              select(id, everything())
            
            # shap_6.1.2.5.4 <- select(data_6.1.2.test, sex_hhh)%>%
            #   mutate(id = 1:n())%>%
            #   filter(!id %in% shap_6.1.2.5.3$id)%>%
            #   distinct(sex_hhh)
            
            shap_6.1.2.5.5 <- left_join(data.frame(id = 1:nrow(data_6.1.2.testing)), shap_6.1.2.5.3, by = "id") %>%
              mutate(Gender = ifelse(is.na(Gender), "Male HHH", Gender)) # some bugs here, can be improved
          }
          
          
          shap_6.1.2.7 <- left_join(shap_6.1.2.7, shap_6.1.2.5.5, by = "id")
        }
      }
      
      shap_6.1.2.8 <- shap_6.1.2.4 %>%
        select(Country, id, ends_with("hh_expenditures_USD_2014"))%>%
        mutate(sd_exp   = sd(hh_expenditures_USD_2014),
               mean_exp = mean(hh_expenditures_USD_2014))%>%
        mutate(z_score_exp = (hh_expenditures_USD_2014-mean_exp)/sd_exp)%>%
        select(-mean_exp, -sd_exp)
      
      # Working with the following dataframe
      
      shap_6.1.2.9 <- shap_6.1.2.6 %>%
        left_join(shap_6.1.2.7, by = "id")%>%
        left_join(shap_6.1.2.8, by = "id")%>%
        mutate(Country = i)%>%
        mutate(fold = v)
      
      shap_6.1.2.0 <- bind_rows(shap_6.1.2.0, shap_6.1.2.9)
  }

    # to be exported

    write_rds(shap_6.1.2.0, sprintf("../0_Data/9_Supplementary Data/BRT-Tracking/SHAP-Values en detail/2017/SHAP_wide_%s.rds", i))
    
    # removals to be updated
    rm(shap_6.1.2.1, shap_6.1.2.2, shap_6.1.2.3,
       shap_6.1.2.3.1, shap_6.1.2.3.2, shap_6.1.2.4, shap_6.1.2.5,
       shap_6.1.2.5.1, shap_6.1.2.5.2, shap_6.1.2.5.3, shap_6.1.2.5.5,
       shap_6.1.2.6, shap_6.1.2.7, shap_6.1.2.8, shap_6.1.2.9,
       data_6.1.1,
       data_6.1.2.test, data_6.1.2.testing, data_6.1.2.training, data_6.1.2.testing_matrix,
       recipe_6.1.0, model_brt, model_brt_1, VOI, shap_6.1.2, shap_6.1.2.0, data_6.1.2, folds_6.1.2, data_6.1.2.raw, data_6.1.3)
    
    gc()
    
    print(paste0("End: ", i, " ", Sys.time()))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Output Evaluation metrics to LaTeX

eval_0.1 <- eval_0 %>%
  distinct()%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")

# Save shap_0 for classification exercise

shap_0.1 <- shap_0 %>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD_2017.xlsx")

# Save shap_1 as output

shap_1.1 <- shap_1 %>%
  write.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification_VFOLD_2017.xlsx")

# Part b: Output of tables

eval_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")
# eval_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD.xlsx")

eval_1.0 <- eval_1 %>%
  filter(.metric == "mae")%>%
  mutate(number = 1:n())%>%
  group_by(Country, number_ob, fold)%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  ungroup()%>%
  arrange(Country)%>%
  select(Country, number_ob)

eval_1.1 <- eval_1 %>%
  filter(number_ob %in% eval_1.0$number_ob)%>%
  mutate(number = 1:n())%>%
  group_by(Country, number_ob, .metric, fold)%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob, .metric)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  ungroup()%>%
  left_join(Country.Set)%>%
  select(Country_long, .metric, starts_with("Sample"), Country)%>%
  mutate_at(vars(starts_with("Sample")), ~ round(., 2))%>%
  arrange(Country_long)%>%
  pivot_wider(names_from = ".metric", values_from = c("Sample_Testing"))

eval_1.2 <- data_2 %>%
  group_by(Country)%>%
  summarise(mean_carbon_intensity = wtd.mean(carbon_intensity_kg_per_USD_national, hh_weights))%>%
  ungroup()%>%
  mutate(mean_carbon_intensity = round(mean_carbon_intensity, 2))

eval_1.3 <- left_join(eval_1.1, eval_1.2)%>%
  select(Country_long, mean_carbon_intensity, everything(), -Country, -sample)

colnames(eval_1.3) <- c("Country", "Mean", rep(c("MAE", "RMSE", "$R^{2}$"),1))

kbl(eval_1.3, format = "latex", caption = "Evaluation of boosted regression tree models", booktabs = T, align = "l|r|rrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T, escape = FALSE, label = "A8")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 8)%>%
  #column_spec(1, width = "3.15 cm")%>%
  # add_header_above(c("Country" = 1, rep(c("MAE", "RMSE", "R^{2}"),3) ))%>%
  #add_header_above(c(" " = 2, "Test sample" = 3, "Training sample" = 3, "Entire sample" = 3))%>%
  footnote(general = "This table shows performance metrics for boosted regression tree models. MAE is the mean absolute error of predictions; RMSE is the root mean squared error of predictions; $R^{2}$ is the squared correlation of prediction errors. Unit of MAE and RMSE is $kgCO_{2}$ per US-\\\\$. We show MAE, RMSE and $R^{2}$ for predictions on the testing set, on the training set and on the entire dataset. ", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_SHAP_Summary.tex")

rm(eval_1, eval_1.0, eval_1.1, eval_1.2, eval_1.3)

# 6.2     Classification exercise ####

# Pick best-fitting model

# eval_6.2 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD.xlsx")
eval_6.2 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")

eval_6.2.1 <- eval_6.2 %>%
  filter(.metric == "mae")%>%
  group_by(Country, number_ob, fold)%>%
  mutate(number = 1:n())%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  filter(Sample_Testing == min(Sample_Testing))%>%
  ungroup()%>%
  arrange(Country)%>%
  select(Country, number_ob)

# Part I: Normalized SHAP values

# data_6.2.0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD.xlsx")
data_6.2.0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD_2017.xlsx")

# Confirmed

data_6.2.1 <- data_6.2.0 %>%
  filter(number_ob %in% eval_6.2.1$number_ob)%>%
  mutate(number_2 = ifelse(Country != lag(Country), 1:n(),NA))%>%
  fill(number_2)%>%
  mutate(number_2 = ifelse(is.na(number_2),1,number_2))%>%
  group_by(Country)%>%
  filter(number_2 == max(number_2))%>%
  ungroup()%>%
  group_by(Country, number_ob, Var_0, Var_1)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  arrange(Country)%>%
  mutate(Var_0 = ifelse(Var_0 == "ISCED", "Education",
                        ifelse(Var_0 == "HF", "Heating fuel",
                               ifelse(Var_0 == "LF", "Lighting fuel",
                                      ifelse(Var_0 == "CF", "Cooking fuel", Var_0)))))%>%
  group_by(Country, Var_0)%>%
  summarise(#SHAP_contribution = sum(SHAP_contribution),
            share_SHAP        = sum(share_SHAP))%>%
  ungroup()%>%
  pivot_wider(names_from = "Var_0", values_from = "share_SHAP")%>%
  mutate_at(vars(-Country), ~ ifelse(is.na(.),0,.))%>%
  rename(Gender = "Gender HHH", 
         #Appliance = "Appliance own.", Electr = "Electricity access",
         # Car = "Car own.", Motorcycle = "Motorcycle own.", 
         Size = "HH size",
         # CF = "Cooking fuel", LF = "Lighting fuel", HF = "Heating fuel"
         )%>%
  mutate(Sociodemographic = Ethnicity + Nationality + Religion + Language + Religiosity + Gender + Education + Size,
         Spatial          = District + Province + Urban,
         # Electricity      = Electr + Appliance,
         # "Vehicle own."         = Car + Motorcycle,
         # Energy           = CF + LF + HF
         )%>%
  select(-Ethnicity, -Nationality, -Religion, -Language, -Religiosity, 
         -Gender, -Education, 
         -District, -Province,
         #-Electr, -Appliance,
         # -Motorcycle, -Car, 
         - Size,
         - Urban,
         # -CF, -HF, -LF,
         )

# Part Ia: Correcting for larger or smaller R2

#r2_6.2.0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD.xlsx")
r2_6.2.0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")%>%
  filter(number_ob %in% eval_6.2.1$number_ob)%>%
  filter(.metric == "rsq")%>%
  group_by(Country, fold)%>%
  mutate(number = 1:n())%>%
  filter(number == max(number))%>%
  ungroup()%>%
  select(Country, Sample_Testing)%>%
  group_by(Country)%>%
  summarise(rsq = mean(Sample_Testing))%>%
  ungroup()

# With and without correction for r2

data_6.2.1_r2 <- data_6.2.1 %>%
  left_join(r2_6.2.0)%>%
  mutate_at(vars(-Country, -rsq), ~ .*rsq)%>%
  select(-rsq)

# Part II: Vertical vs. horizontal inequality

data_6.2.2 <- data_2 %>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_burden_CO2_national = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.5, weights = hh_weights),
            q95_burden_CO2_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.95, weights = hh_weights),
            q05_burden_CO2_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.05, weights = hh_weights))%>%
  ungroup()%>%
  mutate(dif_q95_q05_burden_CO2_national = q95_burden_CO2_national - q05_burden_CO2_national)%>%
  select(Country, Income_Group_5, dif_q95_q05_burden_CO2_national, median_burden_CO2_national)%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(median_burden_CO2_national, dif_q95_q05_burden_CO2_national))%>%
  mutate(median_1_5    = median_burden_CO2_national_1/median_burden_CO2_national_5,
         dif_95_05_1_5 = dif_q95_q05_burden_CO2_national_1/dif_q95_q05_burden_CO2_national_5)%>%
  select(Country, median_1_5, dif_95_05_1_5)

# Part III: Carbon intensity of consumption

data_6.2.3 <- data_2 %>%
  group_by(Country)%>%
  summarise(mean_carbon_intensity = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()

# Performing clustering on the following dataframe:

# data_6.2.4.0 <- left_join(data_6.2.1, data_6.2.2)%>%
#   left_join(data_6.2.3)%>%
#   mutate_at(vars(median_1_5, dif_95_05_1_5), ~ as.numeric(.))

# No correction for r2

data_6.2.4.0 <- left_join(data_6.2.1, data_6.2.2, by = "Country")%>%
  left_join(data_6.2.3, by = "Country")%>%
  mutate_at(vars(median_1_5, dif_95_05_1_5), ~ as.numeric(.))

# Correction for r2
  
data_6.2.4.0_r2 <- left_join(data_6.2.1_r2, data_6.2.2, by = "Country")%>%
  left_join(data_6.2.3, by = "Country")%>%
  mutate_at(vars(median_1_5, dif_95_05_1_5), ~ as.numeric(.))

data_6.2.4 <- data_6.2.4.0 %>%
  mutate_at(vars(-Country), ~ (. - mean(.))/sd(.))%>%
  select(-Country)

data_6.2.4_r2 <- data_6.2.4.0_r2 %>%
  mutate_at(vars(-Country), ~ (. - mean(.))/sd(.))%>%
  select(-Country)

# Unsupervised machine learning algorithm
# Each cluster is represented by its centroid which corresponds to the mean of points assigned to the cluster

set.seed(2023)

data_6.2.5 <- data.frame()

for(k in 2:20){
  model_6.2.0 <- kmeans(data_6.2.4, centers = k, nstart = 1000)
  
  # total within-cluster sum of squares
  
  tot_within_ss <- model_6.2.0$tot.withinss
  
  # Silhouette
  
  silhouette_1 <- mean((cluster::silhouette(model_6.2.0$cluster, dist(data_6.2.4)))[,3])
  
  data_6.2.5 <- bind_rows(data_6.2.5,
                          data.frame(k_0             = k,
                                     tot_within_ss_0 = tot_within_ss,
                                     silhouette_0    = silhouette_1))
  
  # PAM more robust
}

data_6.2.5_r2 <- data.frame()

for(k in 2:20){
  model_6.2.0 <- kmeans(data_6.2.4_r2, centers = k, nstart = 1000)
  
  # total within-cluster sum of squares
  
  tot_within_ss <- model_6.2.0$tot.withinss
  
  # Silhouette
  
  silhouette_1 <- mean((cluster::silhouette(model_6.2.0$cluster, dist(data_6.2.4_r2)))[,3])
  
  data_6.2.5_r2 <- bind_rows(data_6.2.5_r2,
                          data.frame(k_0             = k,
                                     tot_within_ss_0 = tot_within_ss,
                                     silhouette_0    = silhouette_1))
  
  # PAM more robust
}

# Ellbow-Plot:

# ggplot(data_6.2.5)+
#   geom_line(aes(x = k_0, y = tot_within_ss_0))+
#   geom_point(aes(x = k_0, y = tot_within_ss_0))

# Silhouette-Plot:

# Non-corrected
data_6.2.5.1 <- data_6.2.5 %>%
  bind_rows(data.frame(k_0 = 1, silhouette_0 = 0))%>%
  mutate(help_0 = ifelse(silhouette_0 == max(silhouette_0),1,0))

# Corrected
data_6.2.5.2 <- data_6.2.5_r2 %>%
  bind_rows(data.frame(k_0 = 1, silhouette_0 = 0))%>%
  mutate(help_0 = ifelse(silhouette_0 == max(silhouette_0),1,0))

P_6.2.5.1 <- ggplot(data_6.2.5.1)+
  geom_vline(aes(xintercept = data_6.2.5.1$k[silhouette_0 == max(silhouette_0)]))+
  geom_line(aes(x = k_0, y = silhouette_0))+
  geom_point(aes(x = k_0,y = silhouette_0, fill = factor(help_0)), shape = 21, size = 1.5)+
  scale_fill_manual(values = c("#0072B5FF", "#BC2C29FF"))+
  guides(fill = "none")+
  xlab("Number of clusters (k)")+
  ylab("Average silhouette width")+
  coord_cartesian(ylim = c(min(data_6.2.5.1$silhouette_0)*0.8, 0.31),
                  xlim = c(0,20.5))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,20,5), minor_breaks = seq(0,20,1))+
  scale_y_continuous(expand = c(0,0.01))+
  theme_bw()+
  ggtitle("Silhouette plot (non-corrected)")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        # panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_6.2.5.2 <- ggplot(data_6.2.5.2)+
  geom_vline(aes(xintercept = data_6.2.5.2$k[silhouette_0 == max(silhouette_0)]))+
  geom_line(aes(x = k_0, y = silhouette_0))+
  geom_point(aes(x = k_0,y = silhouette_0, fill = factor(help_0)), shape = 21, size = 1.5)+
  scale_fill_manual(values = c("#0072B5FF", "#BC2C29FF"))+
  guides(fill = "none")+
  xlab("Number of clusters (k)")+
  ylab("Average silhouette width")+
  coord_cartesian(ylim = c(min(data_6.2.5.2$silhouette_0)*0.8, 0.31),
                  xlim = c(0,20.5))+
  scale_x_continuous(expand = c(0,0), breaks = seq(0,20,5), minor_breaks = seq(0,20,1))+
  scale_y_continuous(expand = c(0,0.01))+
  theme_bw()+
  ggtitle("Silhouette plot")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Figures_Appendix/Figure_Silhouette_%d.jpg", width = 12, height = 10, unit = "cm", res = 400)
print(P_6.2.5.1)
print(P_6.2.5.2)
dev.off()

# Silhouette-method 

model_6.2.1 <- kmeans(data_6.2.4,    centers = 13, nstart = 2000)
model_6.2.2 <- kmeans(data_6.2.4_r2, centers = 6, nstart = 2000)

silhouette_6.2.1 <- cluster::silhouette(model_6.2.1$cluster, dist(data_6.2.4))[,3]
silhouette_6.2.2 <- cluster::silhouette(model_6.2.2$cluster, dist(data_6.2.4_r2))[,3]

data_6.2.6 <- data_6.2.4.0 %>%
  mutate(cluster_13_means    = model_6.2.1$cluster,
         silhouette_13_means = silhouette_6.2.1)%>%
  group_by(cluster_13_means)%>%
  mutate(number = n())%>%
  ungroup()%>%
  arrange(number, cluster_13_means, silhouette_13_means)%>%
  mutate(order = 1:n())%>%
  mutate(order_2 = ifelse(cluster_13_means != lag(cluster_13_means), 1:n(), NA))%>%
  fill(order_2)%>%
  mutate(order_2 = ifelse(is.na(order_2),1, 1/order_2))

data_6.2.6_r2 <- data_6.2.4.0_r2 %>%
  mutate(cluster_6_means    = model_6.2.2$cluster,
         silhouette_6_means = silhouette_6.2.2)%>%
  group_by(cluster_6_means)%>%
  mutate(number = n())%>%
  ungroup()%>%
  arrange(number, cluster_6_means, silhouette_6_means)%>%
  mutate(order = 1:n())%>%
  mutate(order_2 = ifelse(cluster_6_means != lag(cluster_6_means), 1:n(), NA))%>%
  fill(order_2)%>%
  mutate(order_2 = ifelse(is.na(order_2),1, 1/order_2))

data_6.2.6.1 <- data_6.2.6 %>%
  arrange(desc(number), cluster_13_means)%>%
  distinct(cluster_13_means)%>%
  mutate(cluster = LETTERS[1:n()])

data_6.2.6.1_r2 <- data_6.2.6_r2 %>%
  arrange(desc(number), cluster_6_means)%>%
  distinct(cluster_6_means)%>%
  mutate(cluster = LETTERS[1:n()])

data_6.2.6.2 <- left_join(data_6.2.6, data_6.2.6.1, by = "cluster_13_means")%>%
  select(cluster, Country, everything())%>%
  arrange(cluster, desc(silhouette_13_means))%>%
  mutate(order = 1:n())%>%
  select(-order_2, -number, -cluster_13_means)%>%
  # Information on best-fitting countries
  group_by(cluster)%>%
  mutate(best_fit = ifelse(silhouette_13_means == max(silhouette_13_means),1,0))%>%
  ungroup()%>%
  left_join(select(Country.Set, Country, sample), by = "Country")%>%
  group_by(cluster)%>%
  mutate(largest_country = ifelse(sample == max(sample),1,0))%>%
  ungroup()%>%
  select(-sample)

data_6.2.6.2_r2 <- left_join(data_6.2.6_r2, data_6.2.6.1_r2, by = "cluster_6_means")%>%
  select(cluster, Country, everything())%>%
  arrange(cluster, desc(silhouette_6_means))%>%
  mutate(order = 1:n())%>%
  select(-order_2, -number, -cluster_6_means)%>%
  # Information on best-fitting countries
  group_by(cluster)%>%
  mutate(best_fit = ifelse(silhouette_6_means == max(silhouette_6_means),1,0))%>%
  ungroup()%>%
  left_join(select(Country.Set, Country, sample), by = "Country")%>%
  group_by(cluster)%>%
  mutate(largest_country = ifelse(sample == max(sample),1,0))%>%
  ungroup()%>%
  select(-sample)

write_csv(data_6.2.6.2, "../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv")
write_csv(data_6.2.6.2_r2, "../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv")

P_6.2.5.3 <- ggplot(data_6.2.6)+
  geom_bar(aes(y = silhouette_13_means, x = factor(order), fill = factor(order_2)), 
           stat = "identity", 
           colour = "black",
           width = 0.6, size = 0.1)+
  scale_x_discrete(labels = data_6.2.6$Country, guide = guide_axis(n.dodge=2))+
  scale_y_continuous(expand = c(0,0.01))+
  scale_fill_viridis_d()+
  ylab("Silhouette width")+
  xlab("Country")+
  ggtitle("Silhouette plot for 13 clusters")+
  coord_flip()+
  theme_bw()+
  guides(fill = "none")+
  theme(axis.text.y = element_text(size = 5), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_6.2.5.4 <- ggplot(data_6.2.6_r2)+
  geom_bar(aes(y = silhouette_6_means, x = factor(order), fill = factor(order_2)), 
           stat = "identity", 
           colour = "black",
           width = 0.6, size = 0.1)+
  scale_x_discrete(labels = data_6.2.6_r2$Country, guide = guide_axis(n.dodge=2))+
  scale_y_continuous(expand = c(0,0.01))+
  scale_fill_viridis_d()+
  ylab("Silhouette width")+
  xlab("Country")+
  ggtitle("Silhouette plot for 6 clusters")+
  coord_flip()+
  theme_bw()+
  guides(fill = "none")+
  theme(axis.text.y = element_text(size = 5), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Figures_Appendix/Figure_Silhouette_Clusters_%d.jpg", width = 15.5, height = 10, unit = "cm", res = 400)
print(P_6.2.5.3)
print(P_6.2.5.4)
dev.off()

data_6.2.7 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv")%>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars(-Country), ~ mean(.))%>%
  ungroup()%>%
  # potentially relevant criteria
  rename(car_01 = "Car own.", ely = "Electricity access", exp = "HH expenditures",
         app = "Appliance own.", moto = "Motorcycle own.", cook = "Cooking fuel",
         light = "Lighting fuel", heat = "Heating fuel", size = "HH size", gender = "Gender HHH")%>%
  mutate(CAR         = ifelse(car_01 > 0.07, "Yes", "No"),
         APPLIANCE   = ifelse(app > 0.05, "Yes", "No"),
         COOKING     = ifelse(cook > 0.05, "Yes", "No"),
         ELECTRICITY = ifelse(ely > 0.1, "Yes", "No"),
         EDUCATION   = ifelse(Education > 0.1, "Yes", "No"),
         MOTORCYCLE  = ifelse(moto > 0.07, "Yes", "No"),
         PROVINCE    = ifelse(Province > 0.04, "Yes", "No"),
         DISTRICT    = ifelse(District > 0.04, "Yes", "No"),
         URBAN       = ifelse(Urban > 0.04, "Yes", "No"),
         LIGHTING    = ifelse(light > 0.05, "Yes", "No"),
         HEATING     = ifelse(heat > 0.05, "Yes", "No"),
         'HH SIZE'   = ifelse(size > 0.1, "Yes", "No"),
         GENDER      = ifelse(gender > 0.1, "Yes", "No"),
         SOCIODEMOGRAPHIC = ifelse((Sociodemographic + gender + Education) > 0.05, "Yes", "No"),
         VERTICAL    = ifelse(median_1_5 < 1, "Progressive",
                              ifelse(median_1_5 > 1, "Regressive", NA)),
         HORIZONTAL  = ifelse(dif_95_05_1_5 > 1, "Heterogeneous in poor",
                              ifelse(dif_95_05_1_5 < 1, "Heterogeneous in rich", NA)),
         ABSOLUTE    = ifelse(mean_carbon_intensity < 0.6, "Less carbon intensive",
                              ifelse(mean_carbon_intensity > 0.6 & mean_carbon_intensity < 1, "More carbon intensive",
                                     ifelse(mean_carbon_intensity > 1, "Very carbon intensive", NA))),
         EXPENDITURES = ifelse(exp < 0.05, "Less important",
                               ifelse(exp < 0.1, "Relatively more important",
                                      ifelse(exp > 0.1, "Very more important", NA))))%>%
  select(cluster, number, CAR:EXPENDITURES)%>%
  arrange(desc(number))

data_6.2.8 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv") %>%
  select("Car own.", "Electricity access", "HH expenditures", "Appliance own.",
         "Cooking fuel", Education, Province, District, Urban, "Heating fuel", "Lighting fuel",
         Sociodemographic, median_1_5, dif_95_05_1_5, mean_carbon_intensity, cluster, Country)%>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars(-Country), ~ mean(.))%>%
  ungroup()%>%
  mutate_at(vars(-cluster, - number), ~ (. - mean(.))/sd(.))

rm(data_6.2.0, data_6.2.1, data_6.2.2, data_6.2.3, data_6.2.4, 
   data_6.2.4.0, data_6.2.5, data_6.2.5.1, data_6.2.4_r2, data_6.2.4.0_r2, data_6.2.5_r2, data_6.2.5.2,
   data_6.2.6_r2, data_6.2.6.1_r2, data_6.2.6.2_r2, model_6.2.2, silhouette_6.2.2,
   data_6.2.6, data_6.2.6.1, data_6.2.6.2, data_6.2.7, data_6.2.1_r2, r2_6.2.0,
   data_6.2.8, model_6.2.0, model_6.2.1, P_6.2.5.1, P_6.2.5.2, P_6.2.5.3, P_6.2.5.4, k, silhouette_1, silhouette_6.2.1, tot_within_ss,
   eval_6.2, eval_6.2.1)


# 6.3     Boosted Regression trees on carbon intensity of consumption (with expenditures only) ####

Country.Set.Test.1 <- c("BEL", "NOR", "LBR")

track <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT_2017_EXP.xlsx")
# track <- data.frame(Country = c())

set.seed(2023)

options(warn = 1)

# Takes seven hours

for (i in Country.Set$Country){
  tryCatch({
    
    track_0 <- data.frame(Country = i, date = date())
    
    run_ID <- if(i %in% track$Country) paste0(i, "_", max(track$number[track$Country == i])+1) else paste0(i, "_",1)
    
    print(paste0("Start ", i, ": ", run_ID))
    
    # Filter only observations for country of interest  
    data_6.3 <- filter(data_2, Country == i)
    
    track_0$observations_sample = nrow(data_6.3)
    
    # Feature engineering - Step 1 (with dplyr)
    
    data_6.3.1 <- data_6.3 %>%
      # select relevant variables
      select(hh_expenditures_USD_2014, carbon_intensity_kg_per_USD_national)

    rm(data_6.3)
    
    # Splitting the sample, but no strata
    
    prop_0 = 0.80

    data_6.3.2 <- data_6.3.1 %>%
      initial_split(prop = prop_0)
    
    # Data for training
    data_6.3.2.train <- data_6.3.2 %>%
      training()
    
    # Data for testing
    data_6.3.2.test <- data_6.3.2 %>%
      testing()
    
    rm(data_6.3.1, data_6.3.2)
    
    # Feature engineering - Step 2 (with recipe)
    
    recipe_6.3.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                           data = data_6.3.2.train)
    
    data_6.3.2.training <- recipe_6.3.0 %>%
      prep(training = data_6.3.2.train)%>%
      bake(new_data = NULL)
    
    data_6.3.2.testing <- recipe_6.3.0 %>%
      prep(training = data_6.3.2.test)%>%
      bake(new_data = NULL) 
    
    # Five-fold cross-validation
    
    folds_6.3 <- vfold_cv(data_6.3.2.training, v = 5)
    
    # Setup model to be tuned
    
    model_brt <- boost_tree(
      trees         = 1000,
      tree_depth    = tune(), # maximum depth of tree
      learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
      # min_n       = tune(),
      #mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
      # stop_iter   = tune(),
      # sample_size = tune()
    )%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    # Create a tuning grid - 16 different models for the tuning space
    
    grid_0 <- grid_latin_hypercube(
      tree_depth(),
      learn_rate(c(-3,-0.5)),# tuning parameters
      #mtry(c(round((ncol(data_6.3.2.training)-1)/2,0), ncol(data_6.3.2.training)-1)),
      size = 29)%>%
      # default parameters
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3))
    
    # Tune the model - cover the entire parameter space without running every combination
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    model_brt_1 <- tune_grid(model_brt,
                             carbon_intensity_kg_per_USD_national ~ .,
                             resamples = folds_6.3,
                             grid      = grid_0,
                             metrics   = metric_set(mae, rmse, rsq))
    
    time_2 <- Sys.time()
    
    doParallel::stopImplicitCluster()
    
    track_0$tuning_time <- as.integer(difftime(time_1, time_2, units = "min"))
    
    # Collect metrics of tuned models
    
    metrics_1 <- collect_metrics(model_brt_1)
    
    model_brt_1.1 <- select_best(model_brt_1, metric = "mae")
    
    metrics_1.1 <- metrics_1 %>%
      filter(.config == model_brt_1.1$.config[1])
    
    # Output: best model after tuning
    track_0 <- bind_cols(track_0, model_brt_1.1)%>%
      rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate)%>%
      select(-.config)%>%
      mutate(mae_mean_cv_5_train  = metrics_1.1$mean[metrics_1.1$.metric == "mae"],
             rmse_mean_cv_5_train = metrics_1.1$mean[metrics_1.1$.metric == "rmse"],
             rsq_mean_cv_5_train  = metrics_1.1$mean[metrics_1.1$.metric == "rsq"])
    
    track <- track %>%
      bind_rows(track_0)
    
    rm(track_0, run_ID, time_1, time_2, grid_0,
       model_brt, model_brt_1, model_brt_1.1, metrics_1.1, metrics_1,
       data_6.3.2.test, data_6.3.2.testing, data_6.3.2.train, data_6.3.2.training,
       folds_6.3, recipe_6.3.0, prop_0)
    
    gc()
    
    print(paste0("End: ", i, " ", Sys.time()))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

track_1 <- track %>%
  group_by(Country)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  mutate(number_ob = paste0(Country, "_", number))%>%
  select(number_ob, everything())%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT_2017_EXP.xlsx")

rm(track)

# 6.3.1   Calculate model evaluation and SHAP-values ####

Country.Set.Test.2 <- c("UGA")

Country.Set.Test.3 <- c("BOL", "PRT", "BEL", "CHE", "AUT", "NLD", "SWE", "FRA", "ITA", "ESP", "GRC", "SUR")

# eval_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017_EXP.xlsx")
eval_0 <- data.frame(Country = c("A"), fold = c(1))

track_0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT_2017_EXP.xlsx")

for (i in Country.Set$Country){
  tryCatch({
    
    print(paste0("Start: ", i, " ", Sys.time()))
    
    track <- track_0 %>%
      filter(Country == i)%>%
      #dplyr::slice(which.min(mae_mean_cv_5_train))
      dplyr::slice(which.max(number))
    
    data_6.3 <- filter(data_2, Country == i)
    
    # Feature engineering - Part I
    
    data_6.3.1 <- data_6.3 %>%
      # select relevant variables
      select(hh_expenditures_USD_2014, carbon_intensity_kg_per_USD_national)
    
    rm(data_6.3)
    
    # Feature engineering - Step 2 (with recipe) - with dummification
    
    recipe_6.3.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                           data = data_6.3.1)

    data_6.3.2 <- recipe_6.3.0 %>%
      prep(training = data_6.3.1)%>%
      bake(new_data = NULL)%>%
      # for cross-validation
      mutate(id = 1:n())
    
    folds_6.3.2 <- vfold_cv(data_6.3.2, v = 5)
    
    data_6.3.3 <- data_6.3.2 %>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[1]]$in_id), in1 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[2]]$in_id), in2 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[3]]$in_id), in3 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[4]]$in_id), in4 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[5]]$in_id), in5 = 1), by = c("id"))%>%
      mutate_at(vars(in1:in5), ~ ifelse(is.na(.),0,.))%>%
      mutate(fold_test = ifelse(in1 == 0, 1,
                                ifelse(in2 == 0, 2,
                                       ifelse(in3 == 0, 3,
                                              ifelse(in4 == 0, 4,
                                                     ifelse(in5 == 0, 5, NA))))))%>%
      select(-in1, -in2, -in3, -in4, -in5, -id)
    
    data_6.3.2.raw <- data_6.3.1 %>%
      mutate(id = 1:n())%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[1]]$in_id), in1 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[2]]$in_id), in2 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[3]]$in_id), in3 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[4]]$in_id), in4 = 1), by = c("id"))%>%
      left_join(mutate(data.frame(id = folds_6.3.2$splits[[5]]$in_id), in5 = 1), by = c("id"))%>%
      mutate_at(vars(in1:in5), ~ ifelse(is.na(.),0,.))%>%
      mutate(fold_test = ifelse(in1 == 0, 1,
                                ifelse(in2 == 0, 2,
                                       ifelse(in3 == 0, 3,
                                              ifelse(in4 == 0, 4,
                                                     ifelse(in5 == 0, 5, NA))))))%>%
      select(-in1, -in2, -in3, -in4, -in5, -id)
    
    # Model to be tuned
    model_brt <- boost_tree(
      trees         = 1000,
      tree_depth    = track$tree_depth_best[1],
      learn_rate    = track$learn_rate_best[1] # the higher the learning rate the faster - default 0.3
    )%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    for(v in c(1:5)){
      print(v)
      
      # Training dataset
      data_6.3.2.training <- data_6.3.3 %>%
        filter(fold_test != v)%>%
        select(-fold_test)
      
      # Testing dataset
      data_6.3.2.testing <- data_6.3.3 %>%
        filter(fold_test == v)%>%
        select(-fold_test)
      
      data_6.3.2.test <- data_6.3.2.raw %>%
        filter(fold_test == v)%>%
        select(-fold_test)

      # Fit model on training dataset
      model_brt_1 <- model_brt %>%
        fit(carbon_intensity_kg_per_USD_national ~ .,
            data = data_6.3.2.training)
      
      # # Evaluation on training dataset 
      # predictions_6.1.1 <- augment(model_brt_1, new_data = data_6.1.2.training)
      # mae_6.1.1  <- mae(predictions_6.1.1,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # rmse_6.1.1 <- rmse(predictions_6.1.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # rsq_6.1.1  <- rsq(predictions_6.1.1,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      # eval_6.1.1 <- bind_rows(mae_6.1.1, rmse_6.1.1, rsq_6.1.1)%>%
      #   mutate(Type = "Training")
      
      # Evalutation on testing dataset
      predictions_6.3.2 <- augment(model_brt_1, new_data = data_6.3.2.testing)
      mae_6.3.2  <- mae(predictions_6.3.2,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      rmse_6.3.2 <- rmse(predictions_6.3.2, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      rsq_6.3.2  <- rsq(predictions_6.3.2,  truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
      eval_6.3.2 <- bind_rows(mae_6.3.2, rmse_6.3.2, rsq_6.3.2)%>%
        mutate(Type = "Testing")%>%
        mutate(fold = v)%>%
        mutate(number_ob = track$number_ob[1])%>%
        select(-.estimator)%>%
        pivot_wider(names_from = "Type", values_from = ".estimate", names_prefix = "Sample_")%>%
        mutate(Country = i)%>%
        select(Country, number_ob, .metric, fold, Sample_Testing)
      
      eval_0 <- eval_0 %>%
        filter(Country != i | fold != v)%>%
        bind_rows(eval_6.3.2)
      
      rm(predictions_6.3.2, mae_6.3.2, rmse_6.3.2, rsq_6.3.2, eval_6.3.2)
      
    }
    
    gc()
    
    print(paste0("End: ", i, " ", Sys.time()))
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Output Evaluation metrics to LaTeX

eval_0.1 <- eval_0 %>%
  distinct()%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017_EXP.xlsx")

# Part b: Output of tables

eval_2 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017_EXP.xlsx")
eval_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")

eval_1.0 <- eval_1 %>%
  filter(.metric == "mae")%>%
  mutate(number = 1:n())%>%
  group_by(Country, number_ob, fold)%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  ungroup()%>%
  arrange(Country)%>%
  select(Country, number_ob)

eval_1.1 <- eval_1 %>%
  filter(number_ob %in% eval_1.0$number_ob)%>%
  mutate(number = 1:n())%>%
  group_by(Country, number_ob, .metric, fold)%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob, .metric)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  ungroup()%>%
  left_join(Country.Set)%>%
  select(Country_long, .metric, starts_with("Sample"), Country)%>%
  mutate_at(vars(starts_with("Sample")), ~ round(., 2))%>%
  arrange(Country_long)%>%
  pivot_wider(names_from = ".metric", values_from = c("Sample_Testing"))

eval_2.1 <- eval_2 %>%
  filter(Country != "A")%>%
  group_by(Country, .metric)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  ungroup()%>%
  left_join(Country.Set)%>%
  select(Country_long, .metric, starts_with("Sample"), Country)%>%
  mutate_at(vars(starts_with("Sample")), ~ round(., 2))%>%
  arrange(Country_long)%>%
  pivot_wider(names_from = ".metric", values_from = c("Sample_Testing"))%>%
  select(Country_long, mae, rmse, rsq)%>%
  rename(mae_wo = mae, rmse_wo = rmse, rsq_wo = rsq)

eval_1.2 <- data_2 %>%
  group_by(Country)%>%
  summarise(mean_carbon_intensity = wtd.mean(carbon_intensity_kg_per_USD_national, hh_weights))%>%
  ungroup()%>%
  mutate(mean_carbon_intensity = round(mean_carbon_intensity, 2))

eval_1.3 <- left_join(eval_1.1, eval_1.2)%>%
  left_join(eval_2.1)%>%
  select(Country_long, mean_carbon_intensity, everything(), -Country, -sample)

colnames(eval_1.3) <- c("Country", "Mean", rep(c("MAE", "RMSE", "$R^{2}$"),1))

kbl(eval_1.3, format = "latex", caption = "Evaluation of boosted regression tree models", booktabs = T, align = "l|r|rrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T, escape = FALSE, label = "A8")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 8)%>%
  #column_spec(1, width = "3.15 cm")%>%
  # add_header_above(c("Country" = 1, rep(c("MAE", "RMSE", "R^{2}"),3) ))%>%
  #add_header_above(c(" " = 2, "Test sample" = 3, "Training sample" = 3, "Entire sample" = 3))%>%
  footnote(general = "This table shows performance metrics for boosted regression tree models including only total household expenditures as feature. MAE is the mean absolute error of predictions; RMSE is the root mean squared error of predictions; $R^{2}$ is the squared correlation of prediction errors. Unit of MAE and RMSE is $kgCO_{2}$ per US-\\\\$. We show MAE, RMSE and $R^{2}$ for predictions on the testing set, on the training set and on the entire dataset. ", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_SHAP_Summary_EXP.tex")

rm(eval_1, eval_1.0, eval_1.1, eval_1.2, eval_1.3)

# 6.1.X   BRT on carbon intensity of consumption (regression model) ####

# We wish to detect the importance of variables in modelling and non-linear relationships

Country.Set.Test.1 <- c("ITA", "ESP", "GRC", "RUS", "MOZ", "VNM")

track <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT.xlsx")

pdp_data <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/PDP_Data.xlsx")

vip_data <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/VIP_Data.xlsx")

set.seed(2023)

options(warn = 1)

for (i in Country.Set.Test.1){
  tryCatch({
    
    track_0 <- data.frame(Country = i, date = date())
    
    run_ID <- if(i %in% track$Country) paste0(i, "_", max(track$number[track$Country == i])+1) else paste0(i, "_",1)
    
    print(paste0("Start: ", i))
    
    # Filter only observations for country of interest  
    data_6.1 <- filter(data_2, Country == i)
    
    track_0$observations_sample = nrow(data_6.1)
    
    # Feature Engineering
    # Possibly do it with recipe
    
    data_6.1.1 <- data_6.1 %>%
      # select relevant variables
      select(Country, hh_id, hh_weights, hh_size,
             Province, urban_01, 
             # District,
             sex_hhh, ISCED, Ethnicity, Religion, Nationality, Language, religiosity,
             electricity.access, HF, LF, CF, 
             hh_expenditures_USD_2014, 
             car.01, motorcycle.01, refrigerator.01, ac.01, tv.01, washing_machine.01, 
             carbon_intensity_kg_per_USD_national)%>%
      # remove redundant variables
      select(-Country, -hh_id, -hh_weights)%>%
      # include hh_weights later in the process
      # factors instead of characters
      mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
      mutate_at(vars(sex_hhh, ISCED, religiosity, urban_01, ends_with(".01")), list(~ as.factor(.)))%>%
      mutate(noise = rnorm(n(),0,1))
    # dataset should have no NA
    #select_if(~ !any(is.na(.)))
    
    # Country-specific edits addressing bugs
    if(i == "SWE"){data_6.1.1 <- select(data_6.1.1, -ISCED, -sex_hhh)}
    if(i == "NLD"){data_6.1.1 <- select(data_6.1.1, -ISCED)}

    rm(data_6.1)
    
    # Splitting the sample, but no strata
    
    prop_0 = 0.75
    if(i == "IDN"){prop_0 <- 0.2}
    if(i == "IND"){prop_0 <- 0.3}
    if(i == "MEX"){prop_0 <- 0.5}
    
    data_6.1.2 <- data_6.1.1 %>%
      initial_split(prop = prop_0)
    
    # Data for training
    data_6.1.2.train <- data_6.1.2 %>%
      training()
    
    # Data for testing
    data_6.1.2.test <- data_6.1.2 %>%
      testing()
    
    rm(data_6.1.1, data_6.1.2)
    
    # Feature engineering with recipe()
    
    recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                           data = data_6.1.2.train)%>%
      step_filter_missing(all_predictors(), threshold = 0)%>%
      step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
      # should have very few unique observations for factors
      step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), threshold = 0.05) # %>%
      #step_novel(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))%>%
      #step_dummy(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))
    
    data_6.1.2.training <- recipe_6.1.0 %>%
      prep(training = data_6.1.2.train)%>%
      bake(new_data = NULL)
    
    data_6.1.2.testing <- recipe_6.1.0 %>%
      prep(training = data_6.1.2.test)%>%
      bake(new_data = NULL)
    
    # Setup model - Standard version
    
    model_brt <- boost_tree()%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    # Fitting the model on training data
    
    model_brt_1 <- model_brt %>%
      fit(carbon_intensity_kg_per_USD_national ~ .,
          data = data_6.1.2.training)
    
    # What can we do with this fitted model?
    # tidy() ?
    # glance() ?
    # augment()
    
    # Use augment to derive predictions for training and test set
    
    mae_predictions_6.1.1 <- augment(model_brt_1, new_data = data_6.1.2.testing)%>%
      mae(truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
    
    rsq_predictions_6.1.1 <- augment(model_brt_1, new_data = data_6.1.2.testing)%>%
      rsq(truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
    
    # Also
    # predictions_6.1.1 <- model_brt_1 %>%
    #  predict(new_data = data_6.1.2.testing)%>%
    #  bind_cols(data_6.1.2.testing)
    # mae(predictions_6.1.1, estimate = .pred, truth = carbon_intensity_kg_per_USD_national)
    
    mae_predictions_6.1.2 <- augment(model_brt_1, new_data = data_6.1.2.training)%>%
      mae(truth = carbon_intensity_kg_per_USD_national, estimate = .pred) 
    
    rsq_predictions_6.1.2 <- augment(model_brt_1, new_data = data_6.1.2.training)%>%
      rsq(truth = carbon_intensity_kg_per_USD_national, estimate = .pred) 
    
    # Output: Evaluation on training data
    track_0$mae_1.1 <- mae_predictions_6.1.2$.estimate[1]
    
    track_0$rsq_1.1 <- rsq_predictions_6.1.2$.estimate[1]
    
    # Output: Evaluation on testing data
    track_0$mae_1.2 <- mae_predictions_6.1.1$.estimate[1]
    
    track_0$rsq_1.2 <- rsq_predictions_6.1.1$.estimate[1]
    
    rm(mae_predictions_6.1.1, mae_predictions_6.1.2, 
       rsq_predictions_6.1.1, rsq_predictions_6.1.2,
       model_brt_1)
    
    # Ten-fold cross-validation
    
    folds_6.1 <- vfold_cv(data_6.1.2.training, v = 10)
    
    model_brt_2 <- fit_resamples(model_brt,
                                 carbon_intensity_kg_per_USD_national ~.,
                                 resamples = folds_6.1,
                                 metrics = metric_set(mae, rmse))
    
    mae_predictions_6.1.3 <- collect_metrics(model_brt_2)
    
    # Output: Evaluation with cross-fold validation
    track_0$mae_2.1  <- mae_predictions_6.1.3$mean[1]
    track_0$rmse_2.1 <- mae_predictions_6.1.3$mean[2]
    
    rm(mae_predictions_6.1.3, model_brt_2, model_brt)
    
    # Optimize the ensemble by tuning
    
    model_brt_3 <- boost_tree(
      trees = 1000,
      tree_depth = tune(), # maximum depth of tree - tuning not necessarily necessariy
      learn_rate = tune(), # the higher the learning rate the faster - default 0.3
      # min_n = tune(),
      mtry = tune(), # possibly 1/0.7/0.5
      # stop_iter = tune(),
      # sample_size = tune()
    )%>%
      set_mode("regression")%>%
      set_engine("xgboost")
    
    # Create a tuning grid
    
    grid_0 <- grid_latin_hypercube(
      tree_depth(),
      learn_rate(c(-4,-1)),# tuning parameters
      size = 10)%>%
      bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3))
    
    doParallel::registerDoParallel()
    
    time_1 <- Sys.time()
    
    # This is what takes long
    
    model_brt_3.1 <- tune_grid(model_brt_3,
                               carbon_intensity_kg_per_USD_national ~ .,
                               resamples = folds_6.1,
                               grid      = grid_0,
                               metrics   = metric_set(mae, rmse))
    
    time_2 <- Sys.time()
    
    track_0$tuning_time <- as.integer(difftime(time_1, time_2, units = "min"))
    
    metrics_3.1 <- collect_metrics(model_brt_3.1)
    
    # get the best model available
    
    model_brt_3.1.1 <- select_best(model_brt_3.1, metric = "mae") 
    
    # Output: best model after tuning
    track_0 <- bind_cols(track_0, model_brt_3.1.1)%>%
      rename(tree_depth_best = tree_depth, learn_rate_best = learn_rate)%>%
      select(-.config)
    
    # Final model
    model_brt_3.1.2 <- finalize_model(model_brt_3, model_brt_3.1.1)%>%
      # runs model with parameters as selected in model_brt_3.1.1
      fit(carbon_intensity_kg_per_USD_national ~ ., 
          data = data_6.1.2.training)
    
    # With testing data
    
    model_brt_3.1.3 <- finalize_model(model_brt_3, model_brt_3.1.1)%>%
      fit(carbon_intensity_kg_per_USD_national ~ .,
          data = data_6.1.2.testing)
    
    predictions_6.2.1 <- augment(model_brt_3.1.2, new_data = data_6.1.2.training)
    
    mae_predictions_6.2.1 <- mae(predictions_6.2.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
    
    rsq_predictions_6.2.1 <- rsq(predictions_6.2.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
    
    mae_predictions_6.2.2 <- mae(augment(model_brt_3.1.2, new_data = data_6.1.2.testing),
                                 truth = carbon_intensity_kg_per_USD_national,
                                 estimate = .pred)
    
    rsq_predictions_6.2.2 <- rsq(augment(model_brt_3.1.2, new_data = data_6.1.2.testing),
                                 truth = carbon_intensity_kg_per_USD_national,
                                 estimate = .pred)
    
    # Output: Evaluation of tuned model on training and testing data
    track_0$mae_3.1 <- mae_predictions_6.2.1$.estimate[1]
    track_0$mae_3.2 <- mae_predictions_6.2.2$.estimate[1]
    
    track_0$rsq_3.1 <- rsq_predictions_6.2.1$.estimate[1]
    track_0$rsq_3.2 <- rsq_predictions_6.2.2$.estimate[1]
    
    rm(predictions_6.2.1, 
       mae_predictions_6.2.1, mae_predictions_6.2.2,
       rsq_predictions_6.2.1, rsq_predictions_6.2.2, 
       folds_6.1, grid_0,
       metrics_3.1, model_brt_3.1.1, model_brt_3.1, model_brt_3)
    
    # Most important variables 
    
    vi_3.2 <- vi(model_brt_3.1.2)%>%
      mutate(Country = i)%>%
      mutate(run_ID = run_ID)%>%
      mutate(Type = "Training")
    
    vi_3.3 <- vi(model_brt_3.1.3)%>%
      mutate(Country = i)%>%
      mutate(run_ID = run_ID)%>%
      mutate(Type = "Testing")
    
    vip_data <- vip_data %>%
      bind_rows(vi_3.2) %>%
      bind_rows(vi_3.3)
    
    rm(vi_3.2, vi_3.3)
    
    # Preparation for partial dependence plot
    
    explainer_6.1 <- explain_tidymodels(model_brt_3.1.2,
                                        data = select(data_6.1.2.training, - carbon_intensity_kg_per_USD_national),
                                        y    = select(data_6.1.2.training, carbon_intensity_kg_per_USD_national),
                                        label = "BRT")
    # TBA
    pdp_6.1 <- model_profile(
      explainer = explainer_6.1,
      variables = c("hh_expenditures_USD_2014"),
      #variable_type = "numerical",
      N = NULL
    )
    
    pdp_6.2.1 <- model_profile(
      explainer = explainer_6.1,
      variable_type = "categorical",
      N = NULL
    )
    
    pdp_6.2.2 <- model_profile(
      explainer = explainer_6.1,
      variable_type = "numerical",
      N = NULL
    )
    
    # Aggregated profile
    
    pdp_6.1.1 <- as_tibble(pdp_6.1$agr_profiles)%>%
      mutate(Country = i)%>%
      mutate(run_ID = run_ID)%>%
      rename("hh_expenditures_USD_2014" = "_x_")
    
    # Ceteris paribus profiles
    
    # pdp_6.1.2 <- as_tibble(pdp_6.1$cp_profiles)%>%
    #   mutate(Country = i)%>%
    #   mutate(run_ID = run_ID)
    
    # Aggregated profile
    
    pdp_6.2.3 <- as_tibble(pdp_6.2.1$agr_profiles)%>%
      mutate(Country = i)%>%
      mutate(run_ID = run_ID)%>%
      rename()
    
    pdp_6.2.4 <- as_tibble(pdp_6.2.2$agr_profiles)%>%
     mutate(Country = i)%>%
     mutate(run_ID = run_ID)%>%
     rename("numeric" = "_x_")
    
    pdp_6.3 <- bind_rows(pdp_6.1.1, pdp_6.2.3, pdp_6.2.4)
    
    pdp_data <- pdp_data %>%
      bind_rows(pdp_6.3)
    
    rm(explainer_6.1, pdp_6.1, data_6.1.2.testing, data_6.1.2.training, pdp_6.2.1, pdp_6.2.2, pdp_6.2.3, pdp_6.2.4,
       pdp_6.1.1, pdp_6.3, model_brt_3.1.2, model_brt_3.1.3, data_6.1.2.test, data_6.1.2.train, recipe_6.1.0)
    
    # SHAP-values: TBA
    
    track <- track %>%
      bind_rows(track_0)
    
    rm(track_0, run_ID, time_1, time_2)
    
    gc()
    
    print(paste0("End: ", i, " ", Sys.time()))
    
    #parallel::stopCluster()
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

# Export information on session

track_1 <- track %>%
  group_by(Country)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  mutate(number_ob = paste0(Country, "_", number))%>%
  select(number_ob, everything())%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT.xlsx")

pdp_data_1 <- pdp_data %>%
  #rename(x ="_x_")%>%
  #mutate(x = ifelse(Country == "MEX", iconv(x, "UTF-8", "UTF-8",sub=''),x))%>%
  write.xlsx(., "../0_Data/9_Supplementary Data/BRT-Tracking/PDP_Data.xlsx")

vip_data_1 <- vip_data %>%
  write.xlsx(.,"../0_Data/9_Supplementary Data/BRT-Tracking/VIP_Data.xlsx")

rm(track, pdp_data, vip_data, track_1, pdp_data_1, vip_data_1)

# 6.1.1   Variable Importance Plots ####

# Variable importance: Which predictor has the largest effect on model outcomes?
# Fractional contribution of each feature based on the gain feature's splits where gain is the improvement
# to accuracy brought by a feature to its branches --> Does splitting by features increase accuracy?
# Relative contribution of feature to the model = each feature's contribution for each tree in the model.
# Improvement in accuracy.
# Measures sum up to one !
# Variables that do not influence model's predicitons may be excluded from model
# Domain-knowledge-based validation: Identification of most important variables may be helpful in assessing the validity of the model based on domain knowledge.
# based on the number of times variables are selected for splitting.

# vip(model_brt_3.1.2)

# Provisional: To be changed later

for(i in c("ISR", "KHM", "ZAF", "BOL")){
  
  data_6.1.1.0 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/VIP_Data.xlsx")%>%
    filter(Country == i)%>%
    # Missing: Of course - select model with best performance on fitting test set
    mutate(Var_0 = ifelse(grepl("District", Variable), "District", 
                          ifelse(grepl("Province", Variable), "Province", 
                                 ifelse(grepl("ISCED", Variable), "ISCED", 
                                        ifelse(grepl("Ethnicity", Variable), "Ethnicity", 
                                               ifelse(grepl("Religion", Variable), "Religion", 
                                                      ifelse(Variable == "hh_expenditures_USD_2014", "HH exp.",
                                                             ifelse(Variable == "hh_size", "HH size",
                                                                    ifelse(grepl("car.01", Variable), "Car own.",
                                                                           ifelse(grepl("urban_01", Variable), "Urban",
                                                                                  ifelse(grepl("sex_hhh", Variable), "Gender HHH",
                                                                                         Variable)))))))))),
           Var_1 = ifelse(grepl("District", Variable), gsub("District?","", Variable), 
                          ifelse(grepl("Province", Variable), gsub("Province?","", Variable), 
                                 ifelse(grepl("ISCED", Variable), gsub("ISCED?","", Variable),
                                        ifelse(grepl("Ethnicity", Variable), gsub("Ethnicity?","", Variable),
                                               ifelse(grepl("Religion", Variable), gsub("Religion?","", Variable),
                                                      ifelse(grepl("urban_01", Variable), gsub("urban_01?","", Variable),
                                                             ifelse(grepl("sex_hhh", Variable), gsub("sex_hhh?","", Variable), 
                                                                    ifelse(grepl("car.01", Variable), gsub("car.01","", Variable), NA)))))))))%>%
    mutate(help_0 = ifelse(Importance < 0.01,1,0))
  
  data_6.1.1.1 <- filter(data_6.1.1.0, help_0 == 1)%>%
    summarise(Importance = sum(Importance))%>%
    mutate(Var_0 = "Other features (Sum)")%>%
    bind_rows(filter(data_6.1.1.0, help_0 == 0))%>%
    arrange(desc(Importance))%>%
    mutate(number_order = ifelse(Var_0 != "Other features (Sum)",1:n(), n()+1))%>%
    mutate(Var_2 = ifelse(!is.na(Var_1), paste0(Var_0, ": ", Var_1), Var_0))
  
  plot_6.1.1 <- ggplot(data_6.1.1.1)+
    geom_col(aes(x = Importance, y = reorder(Var_2, desc(number_order))), width = 0.7, colour = "black", fill = "#E64B35FF")+
    theme_bw()+
    coord_cartesian(xlim = c(0,0.81))+
    scale_x_continuous(labels = scales::percent_format(), expand = c(0,0))+
    xlab("Variable importance")+
    ylab("")+
    ggtitle(sprintf("Feature importance plot for %s", i))
  
  jpeg(sprintf("1_Figures/Test/VIP_Test_%s.jpg",i), width = 15.5, height = 16, unit = "cm", res = 600)
  print(plot_6.1.1)
  dev.off()
  
}

# 6.1.2   Partial Dependence Plots ####

# Continuous variables

pdp_6.1.1.1 <- pdp_6.1.1 %>%
  rename(Variable = "_vname_", x = "_x_", y = "_yhat_", id = "_ids_")%>%
  filter(Variable == "hh_expenditures_USD_2014")

pdp_6.1.2.1 <- pdp_6.1.2 %>%
  rename(Variable = "_vname_", x = "hh_expenditures_USD_2014", y = "_yhat_", id = "_ids_")

plot_6.1.2.1 <- ggplot()+
  geom_line(data = pdp_6.1.2.1, aes(x = x, y = y, group = id), colour = "lightgrey", size = 0.05)+
  geom_line(data = pdp_6.1.1.1, aes(x = x, y = y), colour = "#E64B35FF", size = 1)+
  theme_bw()+
  ylab("Prediction carbon intensity of consumption")+
  xlab("Household expenditures in USD (2014)")+
  coord_cartesian(xlim = c(0,55000))+
  scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  ggtitle(sprintf("Partial dependence plot for %s - household expenditures", i))

jpeg("1_Figures/Test/PDP_Test_1.jpg", width = 15.5, height = 16, unit = "cm", res = 600)
print(plot_6.1.2.1)
dev.off()

# Categorical variables - could potentially be contrasted with marginal effects from OLS

pdp_6.2.1.1 <- pdp_6.2.1 %>%
  rename(Variable ="_vname_", x = "_x_", y = "_yhat_")%>%
  filter(!Variable %in% c("sex_hhh", "Religion", "refrigerator.01"))

plot_6.1.2.2 <- ggplot()+
  geom_col(data = pdp_6.2.1.1, aes(x = x, y = y), width = 0.7, colour = "black", fill = "#E64B35FF")+
  facet_grid(. ~ Variable, scales = "free", space = "free")+
  coord_cartesian(ylim = c(0.3,0.6))+
  theme_bw()+
  ylab("Prediction carbon intensity of consumption")+
  xlab("")+
  ggtitle(sprintf("Partial dependence plot for %s",i))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        strip.text = element_text(angle = 90))

jpeg("1_Figures/Test/PDP_Test_2.jpg", width = 30, height = 16, unit = "cm", res = 600)
print(plot_6.1.2.2)
dev.off()

# 6.X     TBA: BRT on hardship cases (classification model) ####
# _______ ####
# 7       Figures Presentation ####
# 7.1     Figure 1: Scatterplot and friends ####

data_7.1 <- data_2 %>%
  filter(Country == "ZAF")

data_7.1.1 <- data_7.1 %>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))

data_7.1.2 <- data_7.1 %>%
  group_by(Income_Group_5)%>%
  summarise(max_exp = max(hh_expenditures_USD_2014_pc))

P.7.1.0 <- ggplot()+
  geom_boxplot(data = data_7.1.1, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = 1), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3)+
  geom_point(data = data_7.1.1, aes(y = mean, x = 1), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  xlab("") + ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(ylim = c(0,5))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = "none")+
  scale_fill_discrete(guide = "none")+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.1.1 <- ggplot()+
  geom_point(data = data_7.1, aes(x = hh_expenditures_USD_2014_pc,
                                      y = carbon_intensity_kg_per_USD_national), 
             alpha = 0.05, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + ylab("")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 22000), ylim = c(0,5))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("South Africa")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.1.2 <- P.7.1.1 +
  geom_smooth(data = data_7.1, 
              aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                  y = carbon_intensity_kg_per_USD_national, group = Country),
              level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#E64B35FF", 
              fill  = "#E64B35FF", size = 0.5)

P.7.1.3 <- ggplot()+
  geom_vline(aes(xintercept = 543.4191),  size = 0.2)+
  geom_vline(aes(xintercept = 981.5637),  size = 0.2)+
  geom_vline(aes(xintercept = 1753.5873), size = 0.2)+
  geom_vline(aes(xintercept = 3911.4171), size = 0.2)+
  geom_point(data = data_7.1, aes(x = hh_expenditures_USD_2014_pc,
                                  y = carbon_intensity_kg_per_USD_national, fill = factor(Income_Group_5)), 
             alpha = 0.05, shape = 21, colour = "black",  size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + ylab("")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 22000), ylim = c(0,5))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("South Africa")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.1.4 <- P.7.1.1 +
  geom_vline(aes(xintercept = 543.4191),  size = 0.2)+
  geom_vline(aes(xintercept = 981.5637),  size = 0.2)+
  geom_vline(aes(xintercept = 1753.5873), size = 0.2)+
  geom_vline(aes(xintercept = 3911.4171), size = 0.2)

P.7.1.5 <- align_plots(P.7.1.0, P.7.1.1, P.7.1.2, P.7.1.3, P.7.1.4, P.7.2.1, align = "hv")

P.7.1.6  <- ggdraw(P.7.1.5[[1]])
P.7.1.7  <- ggdraw(P.7.1.5[[2]])
P.7.1.8  <- ggdraw(P.7.1.5[[3]])
P.7.1.9  <- ggdraw(P.7.1.5[[4]])
P.7.1.10 <- ggdraw(P.7.1.5[[5]])
P.7.1.11 <- ggdraw(P.7.1.5[[6]])

jpeg("4_Presentations/Figures/Figure 1/Figure_1_0_%d.jpg", width = 4, height = 10, unit = "cm", res = 600)
print(P.7.1.6)
dev.off()

jpeg("4_Presentations/Figures/Figure 1/Figure_1_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.7.1.7)
print(P.7.1.8)
print(P.7.1.9)
print(P.7.1.10)
dev.off()

# 7.2     Figure 2: Boxplot and friends ####

data_7.2 <- data_7.1 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_7.2.1 <- data_7.2 %>%
  summarise(min_median = min(y50),
            max_median = max(y50))
  
P.7.2.1 <- ggplot(data_7.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(Income_Group_5)), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 0.7) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab("")+
  geom_point(aes(y = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("")+
  scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 2/Figure_2_a_%d.jpg", width = 8.5, height = 10, unit = "cm", res = 600)
print(P.7.1.11)
dev.off()

P.7.2.2 <- ggplot(data_7.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("South Africa")+
  scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.2.3 <- ggplot(data_7.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("South Africa")+
  scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.2.4 <- ggplot(data_7.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_7.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("South Africa")+
  scale_fill_discrete(guide = "none")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.2.5 <- ggplot(data_7.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_7.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("South Africa")+
  scale_fill_manual(guide = "none", values = c("grey", "lightgrey"))+
  scale_size_manual(guide = "none", values = c(0.3, 0.7))+
  scale_colour_manual(guide = "none", values = c("black", "darkred"))+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 2/Figure_2_b_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.7.2.2)
print(P.7.2.3)
print(P.7.2.4)
print(P.7.2.5)
dev.off()

rm(data_7.2, data_7.1, data_7.2.1, data_7.1.1, data_7.1.2, 
   P.7.1.0, P.7.1.1, P.7.1.2, P.7.1.3, P.7.1.4, P.7.1.5, P.7.1.6, P.7.1.7, P.7.1.8, P.7.1.9, P.7.1.10, P.7.1.11,
   P.7.2.1, P.7.2.2, P.7.2.3, P.7.2.4, P.7.2.5)

# 7.2.1   Figure 2.1: Horizontal exceed vertical differences ####

data_7.2.2 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_7.2.3 <- data_7.2.2 %>%
  group_by(Country)%>%
  summarise(min_median = min(y50),
            max_median = max(y50))%>%
  ungroup()

data_7.2.4 <- left_join(data_7.2.2, data_7.2.3)%>%
  filter(Income_Group_5 == 1)%>%
  sample_n(20)%>%
  arrange(min_median)%>%
  mutate(new_col = 1:n())
  
P_7.2.6 <- ggplot(data = data_7.2.4)+
  geom_rect(aes(xmin = new_col - 0.45,
                xmax = new_col + 0.45,
                ymin = min_median, ymax = max_median), 
            alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = new_col, group = new_col), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.4, alpha = 0.85, fill = "grey", size = 0.2)+
  geom_point(aes(y = mean, x = new_col), shape = 23, size = 0.5 ,fill = "white", stroke = 0.2)+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = data_7.2.4$new_col, labels = data_7.2.4$Country)+
  ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  xlab("Country")+
  coord_flip(ylim = c(0,5))+
  ggtitle("Vertical and horizontal differences")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 2/Figure_2_c_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.2.6)
dev.off()

rm(data_7.2.2, data_7.2.3, data_7.2.4, P_7.2.6)

# 7.3     Figure 3: Vertical over horizontal effects ####

data_7.3.0 <- read_csv("../0_Data/9_Supplementary Data/WDI/2021_08_17_WDI.csv") %>%
  rename(Country.Name = "Country Name",
         Country.Code = "Country Code",
         Type         = "Series Name")%>%
  select(-'Series Code')%>%
  rename_at(vars(ends_with("]")), list(~ str_replace(., "..YR.....", "")))%>%
  pivot_longer(-("Country.Name":"Type"), names_to = "year", values_to = "value")%>%
  filter(value != "..")%>%
  mutate(value = as.numeric(value))%>%
  filter(year == 2018 & Type == "GDP per capita (constant 2010 US$)")

data_7.3 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_burden_CO2_national = wtd.quantile(burden_CO2_national, probs = 0.5, weights = hh_weights),
            q95_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.95, weights = hh_weights),
            q05_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.05, weights = hh_weights),
            q20_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.20, weights = hh_weights),
            q80_burden_CO2_national    = wtd.quantile(burden_CO2_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  mutate(dif_q95_q05_burden_CO2_national = q95_burden_CO2_national - q05_burden_CO2_national,
         dif_q80_q20_burden_CO2_national = q80_burden_CO2_national - q20_burden_CO2_national,)%>%
  select(Country, Income_Group_5, dif_q95_q05_burden_CO2_national, dif_q80_q20_burden_CO2_national, median_burden_CO2_national)%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(median_burden_CO2_national, dif_q95_q05_burden_CO2_national, dif_q80_q20_burden_CO2_national))%>%
  mutate(median_1_5    = median_burden_CO2_national_1/median_burden_CO2_national_5,
         dif_95_05_1_5 = dif_q95_q05_burden_CO2_national_1/dif_q95_q05_burden_CO2_national_5,
         dif_80_20_1_5 = dif_q80_q20_burden_CO2_national_1/dif_q80_q20_burden_CO2_national_5)%>%
  left_join(data_4.5.0, by = c("Country" = "Country.Code"))%>%
  mutate(value = ifelse(Country == "TWN", 20388.2761, value))%>%
  mutate(interest2 = ifelse(Country %in% c("RWA", "DEU", "ZAF", "USA", "CAN", "MWI", "LBR"),1,0.5))

poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,2.95,
                                                                         2.96,2.96,1.06,
                                                                         2.95,1.05,1.05,2.95,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,2.95,2.95,1.05,
                         1.06,2.96,2.96,
                         2.95,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", "FAIL")))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,3.19,3.19,1.01,
                           1.01,3.19,3.19,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,3.19,3.19,
                           1.01,1.01,3.19,3.19),
                     label = c(rep("Progressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ1",4),
                               rep("Progressive and more heterogeneous in IQ1",4)),
                     g2 = c(rep(1,4), rep(2,4), rep(1,4), rep(2,4)),
                     label2 = c(rep("Progressive",4),  rep("Regressive",4), rep("Regressive",4), rep("Progressive",4)),
                     label3 = c(rep("More heterogeneous in IQ5", 8), rep("More heterogeneous in IQ1", 8)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,3.18,3.18,
                           0.02,3.17,0.02),
                     x = c(0.02,0.02,3.17,
                           0.03,3.18,3.18))

poly_4 <- data.frame(text = c("Horizontal Differences > Vertical Differences",
                              "Vertical Differences > Horizontal Differences"),
                     x = c(2,1),
                     y = c(0.5,2.5))

# First only figure without anything

P.7.3.1 <- ggplot()+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = 1))+
  theme_bw()+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.3.2 <- ggplot()+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = 1))+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label2), alpha = 0.5)+
  theme_bw()+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_manual(values = c("#6F99ADFF", "#FFDC91FF"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical distribution Coefficient")+
  xlab("Horizontal distribution Coefficient")+
  labs(fill = "")+
  #guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

L.7.3.2 <- ggdraw(get_legend(P.7.3.2))

P.7.3.2 <- P.7.3.2 +
  guides(fill = "none")

P.7.3.3 <- ggplot()+
  geom_hline(aes(yintercept = 1))+
  geom_vline(aes(xintercept = 1))+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label3), alpha = 0.5)+
  theme_bw()+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_manual(values = c("#6F99ADFF", "#FFDC91FF"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  #guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

L.7.3.3 <- ggdraw(get_legend(P.7.3.3))

P.7.3.3 <- P.7.3.3 +
  guides(fill = "none")

P.7.3.4 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  #geom_text(data = poly_4, aes(label = text, x = x, y = y))+
  theme_bw()+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_manual(values = c("#FFDC91FF", "#6F99ADFF", "#0072B5FF", "#E18727FF"), guide = guide_legend(nrow = 2))+
  #scale_shape_manual(values = c(15,15,15,15,17,17,17,17,18,18,18,18,19,19,19,19))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  #guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

L.7.3.4 <- ggdraw(get_legend(P.7.3.4))

P.7.3.4 <- P.7.3.4 +
  guides(fill = "none")

# Just South Africa

P.7.3.5 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  theme_bw()+
  geom_point(data = filter(data_7.3, Country == "ZAF"), aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  geom_text_repel(data = filter(data_7.3, Country == "ZAF"), aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
                  direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_manual(values = c("#FFDC91FF", "#6F99ADFF", "#0072B5FF", "#E18727FF"), guide = guide_legend(nrow = 2))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

# All countries

P.7.3.6 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = poly_2, aes(x = x, y = y, group = g, fill = label), alpha = 0.5)+
  theme_bw()+
  geom_point(data = data_7.3, aes(y = median_1_5, x = dif_95_05_1_5), shape = 17, colour = "black", size = 2)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_manual(values = c("#FFDC91FF", "#6F99ADFF", "#0072B5FF", "#E18727FF"), guide = guide_legend(nrow = 2))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.3.7 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  theme_bw()+
  geom_point(data = data_7.3, aes(y = median_1_5, x = dif_95_05_1_5, fill = log(value)), shape = 24, colour = "black", size = 2)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

L.7.3.7 <- ggdraw(get_legend(P.7.3.7))

P.7.3.7 <- P.7.3.7 +
  guides(fill = "none")

P.7.3.8 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  theme_bw()+
  geom_point(data = data_7.3, aes(y = median_1_5, x = dif_95_05_1_5, fill = log(value), alpha = interest2), shape = 24, colour = "black", size = 2)+
  geom_text_repel(data = filter(data_7.3, Country %in% c("RWA", "DEU", "ZAF", "USA", "CAN", "MWI", "LBR")), 
                  aes(label = Country.Name, y = median_1_5, x = dif_95_05_1_5),
                  direction = "both", size = 2, max.overlaps = 100)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ylab("Vertical Distribution Coefficient")+
  xlab("Horizontal Distribution Coefficient")+
  labs(fill = "")+
  guides(fill = "none", alpha = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.3.10 <- align_plots(P.7.3.1,P.7.3.2,P.7.3.3,P.7.3.4,P.7.3.5,P.7.3.6,P.7.3.7,P.7.3.8, align = "hv")
P.7.3.11 <- ggdraw(P.7.3.10[[1]])
P.7.3.12 <- ggdraw(P.7.3.10[[2]])
P.7.3.13 <- ggdraw(P.7.3.10[[3]])
P.7.3.14 <- ggdraw(P.7.3.10[[4]])
P.7.3.15 <- ggdraw(P.7.3.10[[5]])
P.7.3.16 <- ggdraw(P.7.3.10[[6]])
P.7.3.17 <- ggdraw(P.7.3.10[[7]])
P.7.3.18 <- ggdraw(P.7.3.10[[8]])

jpeg("4_Presentations/Figures/Figure 3/Figure_3_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.7.3.11)
print(P.7.3.12)
print(P.7.3.13)
print(P.7.3.14)
print(P.7.3.15)
print(P.7.3.16)
print(P.7.3.17)
print(P.7.3.18)
dev.off()

jpeg("4_Presentations/Figures/Figure 3/Figure_3_Legend_a_%d.jpg", width = 12, height = 2, unit = "cm", res = 600)
print(L.7.3.4)
dev.off()

jpeg("4_Presentations/Figures/Figure 3/Figure_3_Legend_b_%d.jpg", width = 8, height = 2, unit = "cm", res = 600)
print(L.7.3.2)
print(L.7.3.3)
print(L.7.3.7)
dev.off()

rm(poly, poly_2, poly_3, poly_4, data_7.3, data_7.3.0,
   P.7.3.1, P.7.3.2, P.7.3.3, P.7.3.4, P.7.3.5, P.7.3.6, P.7.3.7, P.7.3.8,
   P.7.3.10, P.7.3.11, P.7.3.12, P.7.3.13, P.7.3.14, P.7.3.15, P.7.3.16, P.7.3.17, P.7.3.18, L.7.3.2, L.7.3.3, L.7.3.4, L.7.3.7)

# 7.4     Figure 4: Logit-Model approach ####

data_7.4 <- data_2 %>%
  filter(Country == "ZAF")%>%
  mutate(border_80 = wtd.quantile(carbon_intensity_kg_per_USD_national, weight = hh_weights, probs = 0.87))%>%
  mutate(interest  = ifelse(carbon_intensity_kg_per_USD_national > border_80, "20% most carbon-intensive consumers",
                            "80% least carbon-intensive consumers"))

data_7.4.1 <- data_7.4 %>%
  group_by(interest)%>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()

P.7.4.1 <- ggplot()+
  geom_point(data = data_7.4, aes(x = hh_expenditures_USD_2014_pc,
                                  y = carbon_intensity_kg_per_USD_national), 
             alpha = 0.05, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + 
  ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 22000), ylim = c(0,7))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("South Africa")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.4.2 <- ggplot()+
  geom_point(data = data_7.4, aes(x = hh_expenditures_USD_2014_pc,
                                  y = carbon_intensity_kg_per_USD_national,
                                  fill = factor(interest)), 
             alpha = 0.05, shape = 21, colour = "black", size = 0.5)+
  geom_hline(aes(yintercept = data_7.4$border_80[1]), size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + 
  ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 22000), ylim = c(0,7))+
  scale_y_continuous(expand = c(0,0))+
  guides(fill = "none")+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_fill_manual(values = c("#BC3C29FF","#4DBBD5FF"))+
  ggtitle("South Africa")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.4.3 <- ggplot(data_7.4.1, aes(x = as.factor(interest)))+
  geom_hline(aes(yintercept = data_7.4$border_80[1]), size = 0.5)+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(interest)), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 0.9) +
  theme_bw()+
  xlab("")+ ylab("")+
  geom_point(aes(y = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(limits = rev, labels = c(expression(paste(HC[ZAF], "=0", sep = "")), expression(paste(HC[ZAF], "=1", sep = ""))))+
  coord_cartesian(ylim = c(0,7))+
  ggtitle("")+
  guides(fill = "none")+
  scale_fill_manual(values = c("#BC3C29FF","#4DBBD5FF"))+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P.7.4.4 <- align_plots(P.7.4.1, P.7.4.2, P.7.4.3, align = "h")

P.7.4.5 <- ggdraw(P.7.4.4[[1]])
P.7.4.6 <- ggdraw(P.7.4.4[[2]])
P.7.4.7 <- ggdraw(P.7.4.4[[3]])

jpeg("4_Presentations/Figures/Figure 4/Figure_4_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.7.4.5)
print(P.7.4.6)
dev.off()

jpeg("4_Presentations/Figures/Figure 4/Figure_4_b_%d.jpg", width = 5, height = 10, unit = "cm", res = 600)
print(P.7.4.7)
dev.off()

rm(data_7.4, data_7.4.1, P.7.4.1, P.7.4.2, P.7.4.3, P.7.4.4, P.7.4.5, P.7.4.6, P.7.4.7)

# 7.5     Figure 5: Average Marginal effects ####

data_7.5.0 <- read.xlsx("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_Logit.xlsx")%>%
  mutate(term = ifelse((contrast == "6-8 - 2-5" | contrast == "6-8 - 0-1") & !is.na(contrast), "higher_education",
                       ifelse(contrast == "2-5 - 0-1" & !is.na(contrast), "secondary_education", term)))

# 7.5.1   Figure 5.1: Average marginal effects for South Africa ####

data_7.5.1 <- data_7.5.0 %>%
  filter(Country == "ZAF")%>%
  filter(Type == "affected_upper_80")%>%
  mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
  filter(term != "ISCED_0")%>%
  arrange(estimate)%>%
  mutate(help = c(1,2,3,4,5,8,10,6,9,7,11,12,13))%>%
  arrange(help)%>%
  filter(contrast != "Unknown - A_Electricity")%>%
  mutate(Term = c("Expenditures (log)", "CF: Firewood", "CF: Other biomass", "CF: Kerosene", "CF: Coal", "CF: Gas",
                  "Secondary \n education", "Higher \n education", "HH size", "Urban", "Car own.", "Electricity access"))
  

P_7.5.1 <- ggplot(data = data_7.5.1, aes(x = estimate, y = reorder(Term, desc(help))))+
  geom_vline(aes(xintercept = 0))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.3, size = 0.2)+
  geom_point(shape = 22, aes (fill = Colour_Type), size = 1.5, stroke = 0.3)+
  theme_bw()+
  xlab(expression(paste("Average marginal effect on probability of ", HC[ZAF], "=1", sep = "")))+ 
  ylab("Household characteristics")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(-0.25, 0.52))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
  ggtitle("South Africa")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 5/Figure_5_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.5.1)
dev.off()

rm(data_7.5.1, P_7.5.1)

# 7.5.2   Figure 5.2: Average marginal effects for selected countries ####

data_7.5.2 <- data_7.5.0 %>%
  filter(Type == "affected_upper_80")%>%
  filter(term == "urban_01" | term == "car.01" | (term == "CF" & (contrast == "B_LPG - A_Electricity" | contrast == "Firewood - A_Electricity")))%>%
  mutate(Colour_Type = ifelse(estimate > 0, "A", "B"))%>%
  arrange(estimate)%>%
  group_by(term, contrast)%>%
  mutate(help = 1:n())%>%
  ungroup()

# Urban/rural

P_7.5.2.1 <- ggplot(data = filter(data_7.5.2, term == "urban_01"), aes(x = estimate, y = reorder(Country, desc(estimate))))+
  geom_vline(aes(xintercept = 0))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.7, size = 0.2)+
  geom_point(shape = 22, aes (fill = Colour_Type), size = 1, stroke = 0.3)+
  theme_bw()+
  xlab(expression(paste("Average marginal effect on probability of ", HC[i], "=1", sep = "")))+ 
  ylab("Country")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(-0.25, 0.25))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
  ggtitle("Urban citizenship")+
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Car ownership

P_7.5.2.2 <- ggplot(data = filter(data_7.5.2, term == "car.01"), aes(x = estimate, y = reorder(Country, desc(estimate))))+
  geom_vline(aes(xintercept = 0))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.7, size = 0.2)+
  geom_point(shape = 22, aes (fill = Colour_Type), size = 1, stroke = 0.3)+
  theme_bw()+
  xlab(expression(paste("Average marginal effect on probability of ", HC[i], "=1", sep = "")))+ 
  ylab("")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(-0.2, 0.53))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
  ggtitle("Car ownership")+
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Cooking fuel - Firewood

P_7.5.2.3 <- ggplot(data = filter(data_7.5.2, term == "CF" & contrast == "Firewood - A_Electricity"), aes(x = estimate, y = reorder(Country, desc(estimate))))+
  geom_vline(aes(xintercept = 0))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.7, size = 0.2)+
  geom_point(shape = 22, aes (fill = Colour_Type), size = 1, stroke = 0.3)+
  theme_bw()+
  xlab(expression(paste("Average marginal effect on probability of ", HC[i], "=1", sep = "")))+ 
  ylab("")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(-0.5, 0.5))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
  ggtitle("Cooking with firewood")+
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Cooking fuel - LPG

P_7.5.2.4 <- ggplot(data = filter(data_7.5.2, term == "CF" & contrast == "B_LPG - A_Electricity"), aes(x = estimate, y = reorder(Country, desc(estimate))))+
  geom_vline(aes(xintercept = 0))+
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.7, size = 0.2)+
  geom_point(shape = 22, aes (fill = Colour_Type), size = 1, stroke = 0.3)+
  theme_bw()+
  xlab(expression(paste("Average marginal effect on probability of ", HC[i], "=1", sep = "")))+ 
  ylab("")+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(-0.5, 0.5))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_manual(guide = "none", values = c("#4DBBD5FF", "#E64B35FF"))+
  ggtitle("Cooking with LPG")+
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 5/Figure_5_2_%d.jpg", width = 7, height = 10, unit = "cm", res = 600)
print(P_7.5.2.1)
print(P_7.5.2.2)
print(P_7.5.2.3)
print(P_7.5.2.4)
dev.off()

rm(data_7.5.0, data_7.5.2, P_7.5.2.1, P_7.5.2.2, P_7.5.2.3, P_7.5.2.4)

# 7.6     Figure 6: Variable Importance Plot and Partial Dependence Plots ####

# 7.6.1   Figure 6.1: VIP for South Africa ####

data_7.6.1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/VIP_Data.xlsx")%>%
  filter(Country == "ZAF")%>%
  mutate(Var_0 = ifelse(grepl("District", Variable), "District", 
                        ifelse(grepl("Province", Variable), "Province", 
                               ifelse(grepl("ISCED", Variable), "ISCED", 
                                      ifelse(grepl("Ethnicity", Variable), "Ethnicity", 
                                             ifelse(grepl("Religion", Variable), "Religion", 
                                                    ifelse(Variable == "hh_expenditures_USD_2014", "HH expenditures",
                                                           ifelse(Variable == "hh_size", "HH size",
                                                                  ifelse(grepl("car.01", Variable), "Car own.",
                                                                         ifelse(grepl("urban_01", Variable), "Urban",
                                                                                ifelse(grepl("sex_hhh", Variable), "Gender HHH",
                                                                                       ifelse(grepl("CF_", Variable), "Cooking", 
                                                                                              ifelse(grepl("HF_", Variable), "Heating", 
                                                                                                     ifelse(grepl("LF_", Variable), "Lighting", 
                                                                                                            ifelse(Variable == "electricity.access", "Electricity access", Variable)))))))))))))),
         Var_1 = ifelse(grepl("District", Variable), gsub("District?","", Variable), 
                        ifelse(grepl("Province", Variable), gsub("Province_?","", Variable), 
                               ifelse(grepl("ISCED", Variable), gsub("ISCED_?","", Variable),
                                      ifelse(grepl("Ethnicity", Variable), gsub("Ethnicity_?","", Variable),
                                             ifelse(grepl("Religion", Variable), gsub("Religion?","", Variable),
                                                    ifelse(grepl("urban_01", Variable), gsub("urban_01?","", Variable),
                                                           ifelse(grepl("sex_hhh", Variable), gsub("sex_hhh_?","", Variable), 
                                                                  ifelse(grepl("car.01", Variable), gsub("car.01","", Variable), 
                                                                         ifelse(grepl("CF_", Variable), gsub("CF_?", "", Variable), 
                                                                                ifelse(grepl("LF_", Variable), gsub("LF_?", "", Variable), 
                                                                                       ifelse(grepl("HF_", Variable), gsub("HF_?", "", Variable), NA))))))))))))%>%
  mutate(help_0 = ifelse(Importance < 0.01,1,0))

data_7.6.1.1 <- data_7.6.1 %>%
  filter(help_0 == 1)%>%
  summarise(Importance = sum(Importance))%>%
  mutate(Var_0 = "Other features (Sum)")

data_7.6.1.2 <- data_7.6.1 %>%
  filter(help_0 == 0)%>%
  bind_rows(data_7.6.1.1)%>%
  arrange(desc(Importance))%>%
  mutate(number_order = ifelse(Var_0 != "Other features (Sum)",1:n(), n()+1))%>%
  mutate(Var_2 = ifelse(!is.na(Var_1), paste0(Var_0, ": ", Var_1), Var_0))%>%
  mutate(Var_2 = str_remove(Var_2, "\\."))%>%
  mutate(Var_2 = ifelse(Var_2 == "Car own: 0", "Car own.",
                        ifelse(Var_2 == "Ethnicity: IndianAsian", "Ethnicity: Indian/Asian",
                               ifelse(Var_2 == "washing_machine010", "Washing machine",
                                      ifelse(Var_2 == "Urban: 0", "Urban / rural", 
                                             ifelse(Var_2 == "Province: KwazuluNatal", "Province: Kzwazulu Natal", Var_2))))))%>%
  mutate(help_2 = ifelse(number_order > 3,1,0))

P_7.6.1 <- ggplot(data_7.6.1.2)+
  geom_col(aes(x = Importance, y = reorder(Var_2, desc(number_order))), width = 0.7, colour = "black", fill = "#6F99ADFF", size = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.52))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  xlab("Variable importance")+
  ylab("Variable")+
  ggtitle("South Africa")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_7.6.2 <- ggplot(data_7.6.1.2)+
  geom_col(aes(x = Importance, y = reorder(Var_2, desc(number_order)), fill = factor(help_2)), width = 0.7, colour = "black", size = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.52))+
  scale_fill_manual(values = c("#E64B35FF","#6F99ADFF"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  guides(fill = "none")+
  xlab("Variable importance")+
  ylab("Variable")+
  ggtitle("South Africa")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))


jpeg("4_Presentations/Figures/Figure 6/Figure_6_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.6.1)
print(P_7.6.2)
dev.off()

rm(data_7.6.1, data_7.6.1.1, data_7.6.1.2, P_7.6.1, P_7.6.2)


# 7.6.2   Figure 6.2: PDP for South Africa ####

data_7.6.2 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/PDP_Data.xlsx")%>%
  filter(Country == "ZAF")%>%
  filter(run_ID == "ZAF_2")%>%
  rename(Variable = "_vname_", label = "_label_", yhat = "_yhat_", id = "_ids_")

# Expenditures 

# Get other households

# Filter only observations for country of interest  
data_7.6.2.1 <- filter(data_2, Country == "ZAF")

data_7.6.2.2 <- data_7.6.2.1 %>%
  # select relevant variables
  select(Country, hh_id, hh_weights, hh_size,
         Province, urban_01, 
         # District,
         sex_hhh, ISCED, Ethnicity, Religion, Nationality, Language, religiosity,
         electricity.access, HF, LF, CF, 
         hh_expenditures_USD_2014, 
         car.01, motorcycle.01, refrigerator.01, ac.01, tv.01, washing_machine.01, 
         carbon_intensity_kg_per_USD_national)%>%
  # remove redundant variables
  select(-Country, -hh_id, -hh_weights)%>%
  # include hh_weights later in the process
  # factors instead of characters
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  mutate_at(vars(sex_hhh, ISCED, religiosity, urban_01, ends_with(".01"), electricity.access), list(~ as.factor(.)))

data_7.6.2.3 <- data_7.6.2.2 %>%
  initial_split(prop = 0.75)

# Data for training
data_7.6.2.3.train <- data_7.6.2.3 %>%
  training()

# Data for testing
data_7.6.2.3.test <- data_7.6.2.3 %>%
  testing()

recipe_7.6.2.3 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                       data = data_7.6.2.3.train)%>%
  step_filter_missing(all_predictors(), threshold = 0)%>%
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
  # should have very few unique observations for factors
  step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), threshold = 0.05) # %>%
#step_novel(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))%>%
#step_dummy(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))

data_7.6.2.3.training <- recipe_7.6.2.3 %>%
  prep(training = data_7.6.2.3.train)%>%
  bake(new_data = NULL)

data_7.6.2.3.testing <- recipe_7.6.2.3 %>%
  prep(training = data_7.6.2.3.test)%>%
  bake(new_data = NULL)

track_1 <- track %>%
  filter(number_ob == "ZAF_2")

model_brt_3 <- boost_tree(
  trees = 1000,
  tree_depth = track_1$tree_depth_best[1],
  learn_rate = track_1$learn_rate_best[1], # the higher the learning rate the faster - default 0.3
  # min_n = tune(),
  # mtry = tune(),
  # stop_iter = tune(),
  # sample_size = tune()
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")%>%
  fit(carbon_intensity_kg_per_USD_national ~ .,
      data = data_7.6.2.3.training)

# Preparation for partial dependence plot

explainer_7.6.2 <- explain_tidymodels(model_brt_3,
                                    data = select(data_7.6.2.3.testing, - carbon_intensity_kg_per_USD_national),
                                    y    = select(data_7.6.2.3.testing, carbon_intensity_kg_per_USD_national),
                                    label = "BRT")
# TBA
pdp_7.6.2 <- model_profile(
  explainer = explainer_7.6.2,
  variables = c("hh_expenditures_USD_2014"),
  #variable_type = "numerical",
  N = NULL
)

# Aggregated profile

pdp_7.6.2.1 <- as_tibble(pdp_7.6.2$agr_profiles)%>%
  rename("hh_expenditures_USD_2014" = "_x_")%>%
  rename(Variable = "_vname_", label = "_label_", yhat = "_yhat_", id = "_ids_")

# Ceteris paribus profiles

pdp_7.6.2.2 <- as_tibble(pdp_7.6.2$cp_profiles)%>%
  rename(Variable = "_vname_", label = "_label_", yhat = "_yhat_", id = "_ids_")

data_7.6.2.0 <- data_7.6.2 %>%
  filter(Variable == "hh_expenditures_USD_2014")

P_7.6.2.1 <- ggplot()+
  geom_line(data = pdp_7.6.2.2, aes(x = hh_expenditures_USD_2014, y = yhat, group = id), colour = "lightgrey", size = 0.03)+
  geom_line(data = pdp_7.6.2.1, aes(x = hh_expenditures_USD_2014, y = yhat), colour = "#E64B35FF", size = 0.5)+
  #geom_line(data = data_7.6.2.0, aes(x = hh_expenditures_USD_2014, y = yhat), colour = "blue", size = 1)+
  theme_bw()+
  ylab(expression(paste("Prediction carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  xlab("Household expenditures in USD (2014)")+
  coord_cartesian(xlim = c(0,32000), ylim = c(0,5))+
  scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ggtitle("Partial dependence plot for South Africa - Part A)")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# With linear model included

P_7.6.2.1.1 <- ggplot()+
  geom_point(data = data_7.1, aes(x = hh_expenditures_USD_2014,
                                  y = carbon_intensity_kg_per_USD_national), 
             alpha = 0.05, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
  geom_line(data = pdp_7.6.2.1, aes(x = hh_expenditures_USD_2014, y = yhat), colour = "#E64B35FF", size = 0.5)+
  #geom_line(data = data_7.6.2.0, aes(x = hh_expenditures_USD_2014, y = yhat), colour = "blue", size = 1)+
  theme_bw()+
  ylab(expression(paste("Prediction carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  xlab("Household expenditures in USD (2014)")+
  coord_cartesian(xlim = c(0,32000), ylim = c(0,5))+
  scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  ggtitle("Partial dependence plot for South Africa - Part A)")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        # strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_7.6.2.1.2 <- P_7.6.2.1.1 +
  geom_smooth(data = data_7.1, 
              aes(x = hh_expenditures_USD_2014, weight = hh_weights, 
                  y = carbon_intensity_kg_per_USD_national, group = Country),
              level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#0072B5FF", 
              fill  = "#E64B35FF", size = 0.5)

jpeg("4_Presentations/Figures/Figure 6/Figure_6_b_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.6.2.1)
print(P_7.6.2.1.1)
print(P_7.6.2.1.2)
dev.off()

# Other variables

pdp_7.6.2.3 <- model_profile(
  explainer = explainer_7.6.2,
  variable_type = "categorical",
  N = NULL
)

data_7.6.2.3.0 <- as_tibble(pdp_7.6.2.3$agr_profiles)%>%
  rename(Variable = "_vname_", label = "_label_", yhat = "_yhat_", id = "_ids_", xvar = "_x_")%>%
  filter(Variable %in% c("car.01", "electricity.access", "Ethnicity", "Province", "urban_01"))%>%
  mutate(Variable = ifelse(Variable == "car.01", "Has car?",
                           ifelse(Variable == "electricity.access", "Electricity?",
                                  ifelse(Variable == "urban_01", "Urban?", Variable))))%>%
  mutate(newvar = ifelse(Variable %in% c("Electricity?", "Has car?") & xvar == "0", "No",
                    ifelse(Variable %in% c("Electricity?", "Has car?") & xvar == "1", "Yes",
                           ifelse(Variable == "Urban?" & xvar == "0", "Rural",
                                  ifelse(Variable == "Urban?" & xvar == "1", "Urban", NA)))))%>%
  mutate(newvar = ifelse(is.na(newvar), as.character(xvar), newvar))%>%
  mutate(newvar = ifelse(newvar == "other", "Indian/Asian", newvar))%>%
  mutate(interest = ifelse((Variable == "Electricity?" & newvar == "Yes") | (Variable == "Has car?" & newvar == "Yes") | (newvar == "Indian/Asian"), "1", "0"))

data_7.6.2.3 <- data_7.6.2 %>%
  filter(Variable != "hh_expenditures_USD_2014")

P_7.6.2.2 <- ggplot()+
  geom_col(data = data_7.6.2.3.0, aes(x = newvar, y = yhat, fill = factor(interest)), width = 0.7, colour = "black", size = 0.3, fill = "#6F99ADFF")+
  facet_grid(. ~ Variable, scales = "free", space = "free")+
  coord_cartesian(ylim = c(0,4))+
  theme_bw()+
  ylab(expression(paste("Prediction carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  ggtitle("Partial dependence plot for South Africa - Part B)")+
  xlab("")+
  guides(fill = "none")+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1), 
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7, angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_7.6.2.3 <- ggplot()+
  geom_col(data = data_7.6.2.3.0, aes(x = newvar, y = yhat, fill = factor(interest)), width = 0.7, colour = "black", size = 0.3)+
  facet_grid(. ~ Variable, scales = "free", space = "free")+
  coord_cartesian(ylim = c(0,4))+
  theme_bw()+
  ylab(expression(paste("Prediction carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  ggtitle("Partial dependence plot for South Africa - Part B)")+
  xlab("")+
  guides(fill = "none")+
  scale_fill_manual(values = c("#6F99ADFF", "#E64B35FF"))+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1), 
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7, angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))


jpeg("4_Presentations/Figures/Figure 6/Figure_6_c_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.6.2.2)
print(P_7.6.2.3)
dev.off()

rm(data_7.6.2.1, data_7.6.2.2, data_7.6.2.3, data_7.6.2.3.test, data_7.6.2.3.train, data_7.6.2.3.testing, data_7.6.2.3.training,
   recipe_7.6.2.3, track_1, model_brt_3, explainer_7.6.2, pdp_7.6.2, pdp_7.6.2.1, pdp_7.6.2.2, P_7.6.2.1,
   data_7.6.2, data_7.6.2.3.0, P_7.6.2.2, P_7.6.2.3, pdp_7.6.2.3)

# 7.6.3   Figure 6.3: Shapley-Values for South Africa ####

data_7.6.3 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/PDP_Data.xlsx")%>%
  filter(Country == "ZAF")%>%
  filter(run_ID == "ZAF_2")%>%
  rename(Variable = "_vname_", label = "_label_", yhat = "_yhat_", id = "_ids_")

# Get other households

# Filter only observations for country of interest  
data_7.6.3.1 <- filter(data_2, Country == "ZAF")

data_7.6.3.2 <- data_7.6.3.1 %>%
  # select relevant variables
  select(Country, hh_id, hh_weights, hh_size,
         Province, urban_01, 
         # District,
         sex_hhh, ISCED, Ethnicity, Religion, Nationality, Language, religiosity,
         electricity.access, HF, LF, CF, 
         hh_expenditures_USD_2014, 
         car.01, motorcycle.01, refrigerator.01, ac.01, tv.01, washing_machine.01, 
         carbon_intensity_kg_per_USD_national)%>%
  # remove redundant variables
  select(-Country, -hh_id, -hh_weights)%>%
  # include hh_weights later in the process
  # factors instead of characters
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  mutate_at(vars(sex_hhh, ISCED, religiosity, urban_01, ends_with(".01"), electricity.access), list(~ as.factor(.)))

data_7.6.3.3 <- data_7.6.3.2 %>%
  initial_split(prop = 0.75)

# Data for training
data_7.6.3.3.train <- data_7.6.3.3 %>%
  training()

# Data for testing
data_7.6.3.3.test <- data_7.6.3.3 %>%
  testing()

recipe_7.6.3.3 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                         data = data_7.6.3.3.train)%>%
  step_filter_missing(all_predictors(), threshold = 0)%>%
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
  # should have very few unique observations for factors
  step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), threshold = 0.05)%>%
  step_dummy(all_nominal())# %>%
#step_novel(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))%>%
#step_dummy(all_nominal(), -ends_with(".01"), -ends_with("urban_01"))

data_7.6.3.3.training <- recipe_7.6.3.3 %>%
  prep(training = data_7.6.3.3.train)%>%
  bake(new_data = NULL)

data_7.6.3.3.testing <- recipe_7.6.3.3 %>%
  prep(training = data_7.6.3.3.test)%>%
  bake(new_data = NULL)

track_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT.xlsx") %>%
  filter(number_ob == "ZAF_2")

model_brt_3 <- boost_tree(
  trees = 1000,
  tree_depth = track_1$tree_depth_best[1],
  learn_rate = track_1$learn_rate_best[1], # the higher the learning rate the faster - default 0.3
  # min_n = tune(),
  # mtry = tune(),
  # stop_iter = tune(),
  # sample_size = tune()
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")%>%
  fit(carbon_intensity_kg_per_USD_national ~ .,
      data = data_7.6.3.3.training)

# Preparation for shapley values plot

# explainer_7.6.3 <- explain_tidymodels(model_brt_3,
#                                       data = select(data_7.6.3.3.testing, - carbon_intensity_kg_per_USD_national),
#                                       y    = select(data_7.6.3.3.testing, carbon_intensity_kg_per_USD_national),
#                                       label = "BRT")
# 
# test_shap <- predict_parts(explainer = explainer_7.6.3,
#                            new_observation = data_7.6.3.3.testing,
#                            type = "shap")

# potentially required one-hot-encoding

test <- data_7.6.3.3.training %>%
  select(-carbon_intensity_kg_per_USD_national)%>%
  as.matrix()

# better

shap_contrib <- predict(extract_fit_engine(model_brt_3), test, predcontrib = TRUE, approxcontrib = F)

test <- as_tibble(shap_contrib)%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(),names_to = "variable", values_to = "contribution")

shap <- shap.prep(
  xgb_model = extract_fit_engine(model_brt_3),
  X_train = test
)

shap1 <- shap.values(
  xgb_model = extract_fit_engine(model_brt_3),
  X_train = test
)

shap.test <- shap %>%
  distinct(variable, mean_value)%>%
  mutate(sum_value = sum(mean_value))%>%
  mutate(share_shap = mean_value/sum_value)

# Global feature importance. --> Could be rebuild from package documentation: https://github.com/liuyanguu/SHAPforxgboost/blob/master/R/SHAP_funcs.R

# This is what might be worth working with
P.1 <- shap.plot.summary(shap)

jpeg("4_Presentations/Figures/Figure_Test.jpg", width = 20, height = 20, unit = "cm", res = 600)
print(P.1)
dev.off()

# Schöne Spielerei, aber eigentlich nicht hilfreich
# shap.values1 <- shap.values(xgb_model = extract_fit_engine(model_brt_3), X_train = test)

# plot <- shap.prep.stack.data(shap_contrib = shap.values1$shap_score, top_n = 6, n_groups = 10)

# shap.plot.force_plot(plot)
 
# This is what might be worth working with
shap.plot.dependence(data_long = shap, x = "urban_01_X1",
                     y = "urban_01_X1", color_feature = "value")

shap.plot.dependence(data_long = shap, x = "hh_expenditures_USD_2014", color_feature = "hh_expenditures_USD_2014")

test <- predict_parts(explainer = explainer_7.6.3,
                      new_observation = data_7.6.3.3.testing,
                      type = "shap", B = 20)

rm(data_7.6.3, data_7.6.3.1, data_7.6.3.2, data_7.6.3.3,
   data_7.6.3.3.test, data_7.6.3.3.testing, data_7.6.3.3.train, data_7.6.3.3.training, model_brt_3,
   recipe_7.6.3.3, track, track_1)

# 7.7     Figure 7: Overview of countries ####

data_7.7 <- data_6.3.5 %>%
  #select(-median_1_5, -dif_95_05_1_5, -mean_carbon_intensity)%>%
  pivot_longer(-Country, names_to = "names", values_to = "values")%>%
  mutate(continent = countrycode(Country, origin = "iso3c", destination = "continent"))%>%
  # For now
  filter(continent == "Africa")

# First horizontal and vertical indicators

data_7.7.1 <- filter(data_7.7, names %in% c("median_1_5", "dif_95_05_1_5"))%>%
  mutate(values_rescaled = ifelse(values > 1, values/2, values))

P_7.7.1 <- ggplot(data_7.7.1)+
  geom_point(aes(y = Country, x = names, fill = values_rescaled), shape = 22, size = 3, stroke = 0.003)+
  theme_bw()+
  scale_fill_gradient2(na.value = NA, limits = c(0,1.5), low = "#0072B5FF", high = "#BC3C29FF", breaks = c(0,0.5,1,1.5), labels = c(0,1,2,3),
                       midpoint = 0.5)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(widehat(H)[r]^{1}), expression(widehat(V)[r]^{1})))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("Country")+
  ggtitle("Classification")+
  theme(axis.text.y = element_text(size = 3), 
        axis.text.x = element_text(size = 6),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

# Carbon intensity of consumption 

data_7.7.2 <- filter(data_7.7, names %in% c("mean_carbon_intensity"))

P_7.7.2 <- ggplot(data_7.7.2)+
  geom_point(aes(y = Country, x = names, fill = values), shape = 22, size = 3, stroke = 0.003)+
  theme_bw()+
  scale_fill_gradient2(na.value = NA, limits = c(0,2.2), low = "#0072B5FF", high = "#BC3C29FF", 
                       midpoint = 0.55)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(CI[r])))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("")+
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

# Features

data_7.7.3 <- filter(data_7.7, !names %in% c("mean_carbon_intensity", "median_1_5", "dif_95_05_1_5"))%>%
  mutate(help = ifelse(is.na(values), NA, "1"))

P_7.7.3 <- ggplot(data_7.7.3)+
  geom_point(aes(y = Country, x = names, fill = values, colour = help), shape = 22, size = 3, stroke = 0.003)+
  theme_bw()+
  scale_colour_manual(na.value = NA, values = c("black"))+
  scale_fill_gradient2(na.value = NA, limits = c(0,1), low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0.2)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete()+
  xlab("")+ 
  guides(fill = "none", colour = "none")+
  ylab("")+
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 4, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

P_7.7.4 <- align_plots(P_7.7.1, P_7.7.2, P_7.7.3, align = "h")

P_7.7.5 <- ggdraw(P_7.7.4[[1]])
P_7.7.6 <- ggdraw(P_7.7.4[[2]])
P_7.7.7 <- ggdraw(P_7.7.4[[3]])

jpeg("4_Presentations/Figures/Figure 7/Figure_7_a_Africa_%d.jpg", width = 4, height = 10, unit = "cm", res = 600)
print(P_7.7.5)
dev.off()

jpeg("4_Presentations/Figures/Figure 7/Figure_7_b_Africa_%d.jpg", width = 2, height = 10, unit = "cm", res = 600)
print(P_7.7.6)
dev.off()

jpeg("4_Presentations/Figures/Figure 7/Figure_7_c_Africa_%d.jpg", width = 8, height = 10, unit = "cm", res = 600)
print(P_7.7.7)
dev.off()

rm(data_7.7, data_7.7.1, data_7.7.2, data_7.7.3, P_7.7.1,  P_7.7.2,  P_7.7.3,  P_7.7.4,  P_7.7.5,  P_7.7.6,  P_7.7.7)

# 7.8     Figure 8: Classification of countries ####

data_7.8.1 <- data_6.3.8 %>%
  arrange(desc(number))%>%
  mutate(cluster = 1:n())%>%
  select(-number, - cluster_kmeans_11)%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", "Education" = ISCED, "Mean CO2 intensity" = "mean_carbon_intensity",
         "Vertical inequality" = "median_1_5")%>%
  pivot_longer(-cluster, names_to = "names", values_to = "values")%>%
  mutate(names = factor(names, levels = c("Mean CO2 intensity", "Vertical inequality", "Horizontal inequality",
                                          "HH expenditures", "Car own.", "Electricity access", "Urban", "Province",
                                          "Cooking", "Lighting", "Education")))

P_7.8.1 <- ggplot(data_7.8.1)+
  geom_point(aes(y = factor(cluster), x = names, fill = values), shape = 22, size = 4, stroke = 0.01)+
  theme_bw()+
  #scale_fill_viridis_c(direction = -1)+
  scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete()+
  xlab("Variable")+ 
  guides(fill = "none", colour = "none")+
  ylab("Cluster")+
  ggtitle("Clustering of countries")+
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major = element_line(size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("4_Presentations/Figures/Figure 8/Figure_8_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.8.1)
dev.off()

rm(data_7.8.1, P_7.8.1)

# 7.9     Figure 9: Venn-Diagram ####

# For South Africa

data_7.9.1 <- data_2 %>%
  filter(Country == "ZAF")%>%
  mutate(barrier_0 = wtd.quantile(burden_CO2_national, probs = 0.8, weights = hh_weights))%>%
  mutate(poorest_20_percent  = ifelse(Income_Group_5 == 1,1,0),
         access_to_transfers = ifelse((!is.na(inc_gov_cash)|!is.na(inc_gov_monetary))&(inc_gov_cash > 0 | inc_gov_monetary > 0),1,0),
         most_affected       = ifelse(burden_CO2_national > barrier_0,1,0))%>%
  filter(poorest_20_percent == 1 | most_affected == 1 | access_to_transfers == 1)%>%
  select(Country, access_to_transfers, poorest_20_percent, most_affected, hh_weights) %>%
  mutate(A = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 0, hh_weights,0),
         B = ifelse(most_affected == 0 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         C = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         
         D = ifelse(most_affected == 1 & access_to_transfers == 0 & poorest_20_percent == 1, hh_weights,0),
         E = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 0, hh_weights,0),
         G = ifelse(most_affected == 0 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0),
         
         H = ifelse(most_affected == 1 & access_to_transfers == 1 & poorest_20_percent == 1, hh_weights,0))

data_7.9.2 <- data_7.9.1 %>%
  summarise("Most Affected"       = sum(A),
            "The Poorest"         = sum(B),
            "Access to Transfers" = sum(C),
            "Most Affected&The Poorest"         = sum(D),
            "Most Affected&Access to Transfers" = sum(E),
            "The Poorest&Access to Transfers"   = sum(G),
            "Most Affected&The Poorest&Access to Transfers" = sum(H))

data_7.9.3 <- c(
  "Most Affected"                                 = data_7.9.2$'Most Affected',
  "The Poorest"                                   = data_7.9.2$'The Poorest',
  "Access to Transfers"                           = data_7.9.2$'Access to Transfers',
  "Most Affected&The Poorest"                     = data_7.9.2$'Most Affected&The Poorest',
  "Most Affected&Access to Transfers"             = data_7.9.2$'Most Affected&Access to Transfers',
  "The Poorest&Access to Transfers"               = data_7.9.2$'The Poorest&Access to Transfers',
  "The Poorest&Access to Transfers&Most Affected" = data_7.9.2$'Most Affected&The Poorest&Access to Transfers'
)

P_9.1 <- plot(euler(data_7.9.3, shape = "ellipse"), labels = FALSE,
               quantities = list(type = "percent", fontsize = 7), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
               main = list(label = "South Africa", fontsize = 7)
               #legend = list(side = "bottom", nrow = 1, ncol = 3)
               )

pop <- sum(data_7.9.1$hh_weights)

data_7.9.4 <- rownames_to_column(as.data.frame(data_7.9.3))%>%
  rename(Type = rowname, value = data_7.9.3)%>%
  mutate(total = pop)%>%
  mutate(percent = round(value/total,2))%>%
  mutate(label = paste0(percent*100, "%"))

P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- data_7.9.4$label[data_7.9.4$Type == "Most Affected"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- data_7.9.4$label[data_7.9.4$Type == "The Poorest"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- data_7.9.4$label[data_7.9.4$Type == "Access to Transfers"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- data_7.9.4$label[data_7.9.4$Type == "Most Affected&The Poorest"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.5$children$tag.quantity.5$label <- data_7.9.4$label[data_7.9.4$Type == "Most Affected&Access to Transfers"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.6$children$tag.quantity.6$label <- data_7.9.4$label[data_7.9.4$Type == "The Poorest&Access to Transfers"]
P_9.1$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.7$children$tag.quantity.7$label <- data_7.9.4$label[data_7.9.4$Type == "The Poorest&Access to Transfers&Most Affected"]

# Optimum

data_7.9.5 <- c(
  "Most Affected"                                 = 0,
  "The Poorest"                                   = 0,
  "Access to Transfers"                           = data_7.9.2$'Access to Transfers',
  "Most Affected&The Poorest"                     = 0,
  "Most Affected&Access to Transfers"             = data_7.9.2$'Most Affected&Access to Transfers' + data_7.9.2$'Most Affected',
  "The Poorest&Access to Transfers"               = data_7.9.2$'The Poorest&Access to Transfers' + data_7.9.2$'The Poorest',
  "The Poorest&Access to Transfers&Most Affected" = data_7.9.2$'Most Affected&The Poorest&Access to Transfers' + data_7.9.2$'Most Affected&The Poorest'
)

P_9.2 <- plot(euler(data_7.9.5, shape = "ellipse"), labels = FALSE,
              quantities = list(type = "percent", fontsize = 7), fills = list(fill = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"), alpha = 0.8),
              main = list(label = "South Africa", fontsize = 7)
              #legend = list(side = "bottom", nrow = 1, ncol = 3)
)

data_7.9.6 <- rownames_to_column(as.data.frame(data_7.9.5))%>%
  rename(Type = rowname, value = data_7.9.5)%>%
  mutate(total = pop)%>%
  mutate(percent = round(value/total,2))%>%
  mutate(label = paste0(percent*100, "%"))%>%
  filter(value != 0)

P_9.2$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.1$children$tag.quantity.1$label <- data_7.9.6$label[data_7.9.6$Type == "Access to Transfers"]
P_9.2$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.2$children$tag.quantity.2$label <- data_7.9.6$label[data_7.9.6$Type == "The Poorest&Access to Transfers"]
P_9.2$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.3$children$tag.quantity.3$label <- data_7.9.6$label[data_7.9.6$Type == "Most Affected&Access to Transfers"]
P_9.2$children$canvas.grob$children$diagram.grob.1$children$tags$children$tag.number.4$children$tag.quantity.4$label <- data_7.9.6$label[data_7.9.6$Type == "The Poorest&Access to Transfers&Most Affected"]

data_7.9.5 <- data.frame(A = c("20% most affected", "Access to transfers", "Poorest 20%"),
                     B = c(1,2,3),
                     C = c(1,2,3))
data_7.9.5$A <- factor(data_7.9.5$A, levels = c("20% most affected", "Poorest 20%", "Access to transfers"))

Legend <- ggplot(data_7.9.5, aes(x = B, y = C, fill = A))+
  geom_point(shape = 21, alpha = 0.8, size = 2)+
  scale_fill_manual(values = c("#BC3C29FF", "#FFDC91FF", "#6F99ADFF"))+
  guides(fill = guide_legend(nrow = 1, override.aes = c(size = 5)))+
  theme_bw()+
  labs(fill = "")

Legend <- ggdraw(get_legend(Legend))

jpeg("4_Presentations/Figures/Figure 9/Figure_9_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_9.1)
print(P_9.2)
dev.off()

jpeg("4_Presentations/Figures/Figure 9/Figure_9_a_L.jpg", width = 10, height = 1, unit = "cm", res = 600)
print(Legend)
dev.off()

rm(data_7.9.1, data_7.9.2, data_7.9.3, data_7.9.4, data_7.9.5, data_7.9.6, Legend, P_9.1, P_9.2, pop)

# 7.10    Figure 10: World-Map ####

world <- map_data("world")
prep <- c("Austria","Germany", "Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Spain",
          "France", "Finland", "Croatia", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands",
          "Poland", "Portugal", "Romania", "Serbia", "Sweden", "Switzerland", "Slovakia", "Greece", "UK", "Norway",
          "Argentina", "Barbados", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic",
          "Ecuador", "El Salvador", "Guatemala", "Mexico", "Nicaragua", "Paraguay", "Peru", "Suriname" ,"Uruguay",
          "Canada", "USA",
          "Israel", "Iraq", "India", "Indonesia", "Jordan", "Philippines", "Turkey", "Bangladesh", "Vietnam", "Thailand", "Pakistan",
          "Mongolia", "Bhutan", "Armenia", "Myanmar", "Maldives", "Cambodia", "Taiwan", "Russia", "Malaysia",
          "Benin", "Burkina Faso", "Ethiopia", "Egypt", "Ghana", "Guinea-Bissau" ,"Ivory Coast" ,"Kenya", "Liberia", "Malawi",
          "Mali", "Morocco", "Niger", "Nigeria", "Rwanda", "Senegal", "Serbia", "South Africa", "Switzerland", "Taiwan" ,"Tanzania", "Togo", "Tunisia", "Uganda",
          "Georgia")

not_available <- c("China", "Azerbaijan", "Sri Lanka", "Malaysia", "Qatar", "Greenland")

on_radar <- c("Australia",
              #"New Zealand", "Namibia", "Burundi", "Cameroon", "Papua New Guinea", "Zimbabwe", "Democratic Republic of the Congo", "Japan", "South Korea",
              "Mozambique")

regions <- distinct(world, region)%>%
  mutate(Status = ifelse(region %in% prep, "Data collected",
                         ifelse(region %in% not_available, "Data not available", 
                                ifelse(region %in% on_radar, "Data available", NA))))

regions$Status <- factor(regions$Status, levels = c("Data collected", "Data available", "Data not available"  ))

world <- left_join(world, regions)%>%
  filter(region != "Antarctica")

#world$Status <- factor(world$Status, levels = c("Distributional analysis published", "Distributional analysis in preparation", ""))

P.7.10 <- ggplot()+
  geom_map(data = world, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region, fill = Status), colour = "black", size = 0.1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.15,0.15),
        legend.background = element_rect(fill = NA),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 9),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  guides(fill = guide_legend(override.aes = list(size = 0.1)))+
  scale_fill_manual(values = c("#0072B5FF", "#6F99ADFF", "lightgrey"), na.translate = FALSE, na.value = "lightgrey")+
  labs(fill = "")+
  guides(colour = "none")+
  ggtitle("Coverage of global carbon pricing incidence map")

jpeg("4_Presentations/Figures/Figure 10/Figure_10.jpg", width = 15.5, height = 10, unit = "cm", res = 600)
print(P.7.10)
dev.off()

rm(P.7.10, regions, world, not_available, on_radar, prep)

# _______ ####
# 8       Figures Publication #####

# 8.1     Figure 1: Horizontal exceeds vertical differences ####

data_8.1.0 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_8.1.1 <- data_8.1.0 %>%
  group_by(Country)%>%
  summarise(min_median = min(y50),
            max_median = max(y50))%>%
  ungroup()

data_8.1.2 <- left_join(data_8.1.0, data_8.1.1)%>%
  filter(Income_Group_5 == 1)%>%
  arrange(min_median)%>%
  mutate(new_col = 1:n())%>%
  mutate(new_row = c(rep(1,22), rep(2,22), rep(3,22), rep(4,21)))%>%
  mutate(ymax    = c(rep(2.2,22), rep(2.5,22), rep(4,22), rep(5,21)))%>%
  mutate(ymin    = c(rep(0,87)))

data_8.1.3 <- data_8.1.0 %>%
  filter(interest == 1)%>%
  group_by(Country)%>%
  mutate(help = ifelse(y50 == max(y50), 1,0))%>%
  ungroup()%>%
  mutate(Type = ifelse(Income_Group_5 == 1 & help == 1, "Regressive", "Progressive"))%>%
  filter(Income_Group_5 == 1)%>%
  select(Country, Type)%>%
  mutate(Type = factor(Type, levels = c("Regressive", "Progressive")))

data_8.1.2 <- left_join(data_8.1.2, data_8.1.3)

P_8.1 <- ggplot(data = data_8.1.2)+
  geom_point(aes(y = ymin, x = new_col), alpha = 0)+
  geom_point(aes(y = ymax, x = new_col), alpha = 0)+
  geom_rect(aes(xmin = new_col - 0.45,
                xmax = new_col + 0.45,
                ymin = min_median, ymax = max_median, fill = Type), 
            alpha = 0.5, inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = new_col, group = new_col), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.4, alpha = 0.85, fill = "grey", size = 0.2)+
  geom_point(aes(y = mean, x = new_col), shape = 23, size = 1 ,fill = "white", stroke = 0.2)+
  theme_bw()+
  facet_wrap(. ~ new_row, scales = "free", nrow = 1)+
  scale_fill_nejm()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), breaks = data_8.1.2$new_col, labels = data_8.1.2$Country)+
  ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  xlab("Country")+
  coord_flip()+
  guides(fill = "none")+
  # ggtitle("Vertical and horizontal differences")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid.major.x = element_line(size = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3, fill = NA))

jpeg("1_Figures/Figure 1/Figure_1_2017.jpg", width = 15.5, height = 10, unit = "cm", res = 600)
print(P_8.1)
dev.off()

# For Appendix

data_8.1.4 <- data_8.1.0 %>%
  left_join(data_8.1.1)%>%
  left_join(data_8.1.3)%>%
  arrange(min_median)%>%
  mutate(min_median = ifelse(Income_Group_5 != 1, NA, min_median))%>%
  mutate(new_col = rep(1,435))%>%
  mutate(new_row = c(rep(1,150), rep(2,150), rep(3,135)))%>%
  group_by(Country)%>%
  mutate(ymax = max(y95)*1.1)%>%
  ungroup()%>%
  # mutate(ymax    = c(rep(2.2,110), rep(2.5,110), rep(4,110), rep(5,)))%>%
  mutate(ymin    = c(rep(0,435)))%>%
  left_join(Country.Set)

for (i in c(1,2,3)){
  
  P_8.1.App <- ggplot(filter(data_8.1.4, new_row == i), aes(x = as.character(Income_Group_5)))+
    geom_point(aes(y = ymin, x = new_col), alpha = 0)+
    geom_point(aes(y = ymax, x = new_col), alpha = 0)+
    geom_rect(data = filter(data_8.1.4, new_row == i), aes(ymin = min_median, ymax = max_median, fill = Type), xmin = 0, xmax = 6, alpha = 0.5, inherit.aes = FALSE)+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
                 stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.25, alpha = 1, fill = "grey") +
    theme_bw()+
    facet_wrap(. ~ Country_long, scales = "free_x")+
    xlab("Expenditure quintiles")+ ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
    geom_point(aes(y = mean), shape = 23, size = 1, fill = "white")+
    scale_y_continuous(expand = c(0,0), labels = scales::number_format(accuracy = 0.1), breaks = trans_breaks(identity, identity, 2))+
    scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
    coord_flip()+
    # ggtitle("South Africa")+
    scale_fill_nejm(guide = "none")+
    theme(axis.text.y = element_text(size = 4), 
          axis.text.x = element_text(size = 4),
          axis.title  = element_text(size = 5),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_text(size = 5),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Figures_Appendix/Figure_1_2017_Appendix_%s.jpg", i), width = 15.5, height = 15.5, unit = "cm", res = 600)
  print(P_8.1.App)
  dev.off()
  
}

rm(data_8.1.0, data_8.1.1, data_8.1.2, P_8.1, data_8.1.3, data_8.1.4, P_8.1.App, i)

# 8.2     Figure 2: Horizontal vs. vertical differences ####

data_8.2.0 <- read_csv("../0_Data/9_Supplementary Data/WDI/2021_08_17_WDI.csv") %>%
  rename(Country.Name = "Country Name",
         Country.Code = "Country Code",
         Type         = "Series Name")%>%
  select(-'Series Code')%>%
  rename_at(vars(ends_with("]")), list(~ str_replace(., "..YR.....", "")))%>%
  pivot_longer(-("Country.Name":"Type"), names_to = "year", values_to = "value")%>%
  filter(value != "..")%>%
  mutate(value = as.numeric(value))%>%
  filter(year == 2018 & Type == "GDP per capita (constant 2010 US$)")

data_8.2.1 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_carbon_intensity_kg_per_USD_national = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.80, weights = hh_weights))%>%
  ungroup()%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  mutate(dif_q95_q05_carbon_intensity_kg_per_USD_national = q95_carbon_intensity_kg_per_USD_national - q05_carbon_intensity_kg_per_USD_national,
         dif_q80_q20_carbon_intensity_kg_per_USD_national = q80_carbon_intensity_kg_per_USD_national - q20_carbon_intensity_kg_per_USD_national,)%>%
  select(Country, Income_Group_5, dif_q95_q05_carbon_intensity_kg_per_USD_national, dif_q80_q20_carbon_intensity_kg_per_USD_national, median_carbon_intensity_kg_per_USD_national)%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(median_carbon_intensity_kg_per_USD_national, dif_q95_q05_carbon_intensity_kg_per_USD_national, dif_q80_q20_carbon_intensity_kg_per_USD_national))%>%
  mutate(median_1_5    = median_carbon_intensity_kg_per_USD_national_1/median_carbon_intensity_kg_per_USD_national_5,
         dif_95_05_1_5 = dif_q95_q05_carbon_intensity_kg_per_USD_national_1/dif_q95_q05_carbon_intensity_kg_per_USD_national_5,
         dif_80_20_1_5 = dif_q80_q20_carbon_intensity_kg_per_USD_national_1/dif_q80_q20_carbon_intensity_kg_per_USD_national_5)%>%
  left_join(data_8.2.0, by = c("Country" = "Country.Code"))%>%
  mutate(value = ifelse(Country == "TWN", 20388.2761, value))
  # mutate(interest2 = ifelse(Country %in% c("RWA", "DEU", "ZAF", "USA", "CAN", "MWI", "LBR"),1,0.5))

# For Manuscript

data_8.2.2 <- data_8.2.1 %>%
  mutate(regressive = ifelse(median_1_5 > 1,1,0))%>%
  mutate(progressive = ifelse(median_1_5 < 1,1,0))%>%
  arrange(desc(value))%>%
  mutate(income_higher = 1:n())%>%
  mutate(income_lower = n():1)%>%
  mutate(regressive_income  = ifelse(regressive == 1 & income_higher < 21,1,0))%>%
  mutate(progressive_income = ifelse(progressive == 1 & income_lower < 21,1,0))%>%
  mutate(horizontal = ifelse(dif_95_05_1_5 > 1,1,0))%>%
  mutate(hor_ver = ifelse(dif_95_05_1_5 > median_1_5,1,0))

poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,2.95,
                                                                         2.96,2.96,1.06,
                                                                         2.95,1.05,1.05,2.95,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,2.95,2.95,1.05,
                         1.06,2.96,2.96,
                         2.95,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", NA)))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,2.5,2.5,1.01,
                           1.01,2.5,2.5,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,3.19,3.19,
                           1.01,1.01,3.19,3.19),
                     label = c(rep("Progressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ1",4),
                               rep("Progressive and more heterogeneous in IQ1",4)),
                     g2 = c(rep(1,4), rep(2,4), rep(1,4), rep(2,4)),
                     label2 = c(rep("Progressive",4),  rep("Regressive",4), rep("Regressive",4), rep("Progressive",4)),
                     label3 = c(rep("More heterogeneous in IQ5", 8), rep("More heterogeneous in IQ1", 8)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,3.18,3.18,
                           0.02,3.18,0.02),
                     x = c(0.02,0.02,3.17,
                           0.03,3.18,3.18))

poly_4 <- data.frame(text = c("Horizontal differences > Vertical differences",
                              "Vertical differences > Horizontal differences"),
                     x = c(2.1,1.1),
                     y = c(0.3,2.2))

poly_5 <- data.frame(text = c("A", "B", "C", "D"),
                     x = c(0.9,1.1,0.9,1.1),
                     y = c(2.4,2.4,0.1,0.1))

P_8.2 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  geom_label(data = poly_4, aes(label = text, x = x, y = y), alpha = 0.5, size = 2.5)+
  geom_label(data = poly_5, aes(label = text, x = x, y = y), alpha = 0.2, size = 2.5)+
  theme_bw()+
  geom_point(data = data_8.2.1, aes(y = median_1_5, x = dif_95_05_1_5, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,3.2), ylim = c(0,2.5))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2))+
  ylab("Vertical distribution coefficient")+
  xlab("Horizontal distribution coefficient")+
  labs(fill = "")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("1_Figures/Figure 2/Figure_2_2017.jpg", width = 15.5, height = 10, unit = "cm", res = 600)
print(P_8.2)
dev.off()

# Policy-design

data_8.2.2 <- data_2 %>%
  mutate(carbon_intensity_kg_per_USD_global      = CO2_t_global*1000/hh_expenditures_USD_2014,
         carbon_intensity_kg_per_USD_electricity = CO2_t_electricity*1000/hh_expenditures_USD_2014,
         carbon_intensity_kg_per_USD_transport   = CO2_t_transport*1000/hh_expenditures_USD_2014)%>%
  group_by(Country, Income_Group_5)%>%
  summarise(median_carbon_intensity_kg_per_USD_national    = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_national       = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_national       = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_national       = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_national       = wtd.quantile(carbon_intensity_kg_per_USD_national, probs = 0.80, weights = hh_weights),
            
            median_carbon_intensity_kg_per_USD_global      = wtd.quantile(carbon_intensity_kg_per_USD_global, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_global         = wtd.quantile(carbon_intensity_kg_per_USD_global, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_global         = wtd.quantile(carbon_intensity_kg_per_USD_global, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_global         = wtd.quantile(carbon_intensity_kg_per_USD_global, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_global         = wtd.quantile(carbon_intensity_kg_per_USD_global, probs = 0.80, weights = hh_weights),
            
            median_carbon_intensity_kg_per_USD_electricity = wtd.quantile(carbon_intensity_kg_per_USD_electricity, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_electricity    = wtd.quantile(carbon_intensity_kg_per_USD_electricity, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_electricity    = wtd.quantile(carbon_intensity_kg_per_USD_electricity, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_electricity    = wtd.quantile(carbon_intensity_kg_per_USD_electricity, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_electricity    = wtd.quantile(carbon_intensity_kg_per_USD_electricity, probs = 0.80, weights = hh_weights),
            
            median_carbon_intensity_kg_per_USD_transport   = wtd.quantile(carbon_intensity_kg_per_USD_transport, probs = 0.5, weights = hh_weights),
            q95_carbon_intensity_kg_per_USD_transport      = wtd.quantile(carbon_intensity_kg_per_USD_transport, probs = 0.95, weights = hh_weights),
            q05_carbon_intensity_kg_per_USD_transport      = wtd.quantile(carbon_intensity_kg_per_USD_transport, probs = 0.05, weights = hh_weights),
            q20_carbon_intensity_kg_per_USD_transport      = wtd.quantile(carbon_intensity_kg_per_USD_transport, probs = 0.20, weights = hh_weights),
            q80_carbon_intensity_kg_per_USD_transport      = wtd.quantile(carbon_intensity_kg_per_USD_transport, probs = 0.80, weights = hh_weights))%>%
  ungroup()%>%
  filter(Income_Group_5 == 1 | Income_Group_5 == 5)%>%
  mutate(dif_q95_q05_carbon_intensity_kg_per_USD_national    = q95_carbon_intensity_kg_per_USD_national - q05_carbon_intensity_kg_per_USD_national,
         dif_q80_q20_carbon_intensity_kg_per_USD_national    = q80_carbon_intensity_kg_per_USD_national - q20_carbon_intensity_kg_per_USD_national,
         dif_q95_q05_carbon_intensity_kg_per_USD_global      = q95_carbon_intensity_kg_per_USD_global - q05_carbon_intensity_kg_per_USD_global,
         dif_q80_q20_carbon_intensity_kg_per_USD_global      = q80_carbon_intensity_kg_per_USD_global - q20_carbon_intensity_kg_per_USD_global,
         dif_q95_q05_carbon_intensity_kg_per_USD_electricity = q95_carbon_intensity_kg_per_USD_electricity - q05_carbon_intensity_kg_per_USD_electricity,
         dif_q80_q20_carbon_intensity_kg_per_USD_electricity = q80_carbon_intensity_kg_per_USD_electricity - q20_carbon_intensity_kg_per_USD_electricity,
         dif_q95_q05_carbon_intensity_kg_per_USD_transport   = q95_carbon_intensity_kg_per_USD_transport - q05_carbon_intensity_kg_per_USD_transport,
         dif_q80_q20_carbon_intensity_kg_per_USD_transport   = q80_carbon_intensity_kg_per_USD_transport - q20_carbon_intensity_kg_per_USD_transport)%>%
  select(Country, Income_Group_5, 
         starts_with("dif"), starts_with("median"))%>%
  pivot_wider(names_from = Income_Group_5, values_from = c(starts_with("dif"), starts_with("median")))%>%
  mutate(median_1_5_national       = median_carbon_intensity_kg_per_USD_national_1/median_carbon_intensity_kg_per_USD_national_5,
         dif_95_05_1_5_national    = dif_q95_q05_carbon_intensity_kg_per_USD_national_1/dif_q95_q05_carbon_intensity_kg_per_USD_national_5,
         dif_80_20_1_5_national    = dif_q80_q20_carbon_intensity_kg_per_USD_national_1/dif_q80_q20_carbon_intensity_kg_per_USD_national_5,
         median_1_5_global         = median_carbon_intensity_kg_per_USD_global_1/median_carbon_intensity_kg_per_USD_global_5,
         dif_95_05_1_5_global      = dif_q95_q05_carbon_intensity_kg_per_USD_global_1/dif_q95_q05_carbon_intensity_kg_per_USD_global_5,
         dif_80_20_1_5_global      = dif_q80_q20_carbon_intensity_kg_per_USD_global_1/dif_q80_q20_carbon_intensity_kg_per_USD_global_5,
         median_1_5_electricity    = median_carbon_intensity_kg_per_USD_electricity_1/median_carbon_intensity_kg_per_USD_electricity_5,
         dif_95_05_1_5_electricity = dif_q95_q05_carbon_intensity_kg_per_USD_electricity_1/dif_q95_q05_carbon_intensity_kg_per_USD_electricity_5,
         dif_80_20_1_5_electricity = dif_q80_q20_carbon_intensity_kg_per_USD_electricity_1/dif_q80_q20_carbon_intensity_kg_per_USD_electricity_5,
         median_1_5_transport      = median_carbon_intensity_kg_per_USD_transport_1/median_carbon_intensity_kg_per_USD_transport_5,
         dif_95_05_1_5_transport   = dif_q95_q05_carbon_intensity_kg_per_USD_transport_1/dif_q95_q05_carbon_intensity_kg_per_USD_transport_5,
         dif_80_20_1_5_transport   = dif_q80_q20_carbon_intensity_kg_per_USD_transport_1/dif_q80_q20_carbon_intensity_kg_per_USD_transport_5)%>%
  left_join(data_8.2.0, by = c("Country" = "Country.Code"))%>%
  mutate(value = ifelse(Country == "TWN", 20388.2761, value))

poly <- data.frame(g = c(1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,5,5,6,6,6), x = c(0.05,0.05,0.95,
                                                                         0.05,0.05,0.95,0.95,
                                                                         1.05,1.05,4.45,
                                                                         4.45,4.45,1.06,
                                                                         4.45,1.05,1.05,4.45,
                                                                         0.06,0.96,0.96), 
                   y = c(0.06,0.96,0.96,
                         1.05,4.45,4.45,1.05,
                         1.06,4.45,4.45,
                         4.45,1.05,1.05,
                         0.95,0.95,0.05,0.05,
                         0.05,0.95,0.05))%>%
  mutate(x_1 = ifelse(g == 1,0.25,
                      ifelse(g == 2,0.5,
                             ifelse(g == 3,1.75,
                                    ifelse(g == 4,2.25,
                                           ifelse(g == 5,2,
                                                  ifelse(g == 6,0.75,0)))))))%>%
  mutate(y_1 = ifelse(g == 1,0.75,
                      ifelse(g == 2,2,
                             ifelse(g == 3,2.25,
                                    ifelse(g == 4,1.75,
                                           ifelse(g == 5,0.5,
                                                  ifelse(g == 6,0.25,0)))))))%>%
  mutate(z_1 = ifelse(g == 6 & x_1 == lag(x_1), NA,x_1),
         z_2 = ifelse(x_1 == lead(x_1), NA, x_1))%>%
  mutate(z_3 = ifelse(g == 6, z_1, z_2))%>%
  mutate(z_1 = ifelse(g == 6 & y_1 == lag(y_1), NA,y_1),
         z_2 = ifelse(y_1 == lead(y_1), NA, y_1))%>%
  mutate(z_4 = ifelse(g == 6, z_1, z_2))%>%
  mutate(label = ifelse(g == 1, "Regressive and homogeneous (Horizontal)",
                        ifelse(g == 2, "Regressive and heterogeneous", 
                               ifelse(g == 3, "Progressive and heterogeneous (Horizontal)",
                                      ifelse(g == 4, "Progressive and heterogeneous (Vertical)",
                                             ifelse(g == 5, "Progressive and homogeneous",
                                                    ifelse(g == 6, "Regressive and heterogeneous (Vertical)", NA)))))))

poly_2 <- data.frame(g = c(1,1,1,1,
                           2,2,2,2,
                           3,3,3,3,
                           4,4,4,4),
                     y = c(0.01,0.99,0.99,0.01,
                           1.01,3.19,3.19,1.01,
                           1.01,3.19,3.19,1.01,
                           0.01,0.99,0.99,0.01),
                     x = c(0.01,0.01,0.99,0.99,
                           0.01,0.01,0.99,0.99,
                           1.01,1.01,4.49,4.49,
                           1.01,1.01,4.49,4.49),
                     label = c(rep("Progressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ5",4),
                               rep("Regressive and more heterogeneous in IQ1",4),
                               rep("Progressive and more heterogeneous in IQ1",4)),
                     g2 = c(rep(1,4), rep(2,4), rep(1,4), rep(2,4)),
                     label2 = c(rep("Progressive",4),  rep("Regressive",4), rep("Regressive",4), rep("Progressive",4)),
                     label3 = c(rep("More heterogeneous in IQ5", 8), rep("More heterogeneous in IQ1", 8)))

poly_3 <- data.frame(g = c(1,1,1,
                           2,2,2),
                     y = c(0.03,4.48,4.48,
                           0.02,4.48,0.02),
                     x = c(0.02,0.02,4.46,
                           0.03,4.48,4.48))

poly_4 <- data.frame(text = c("Horizontal differences > Vertical differences",
                              "Vertical differences > Horizontal differences"),
                     x = c(3.1,1.3),
                     y = c(0.3,2.8))

poly_5 <- data.frame(text = c("A", "B", "C", "D"),
                     x = c(0.8,1.2,0.8,1.2),
                     y = c(3.1,3.1,0.1,0.1))

P_8.0 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  geom_label(data = poly_4, aes(label = text, x = x, y = y), alpha = 0.5, size = 2.5)+
  geom_label(data = poly_5, aes(label = text, x = x, y = y), alpha = 0.2, size = 2.5)+
  theme_bw()+
  # geom_segment(data = data_8.2.2, aes(y = median_1_5_national, yend = median_1_5_global, x = dif_95_05_1_5_national, xend = dif_95_05_1_5_global), size = 0.1, colour = "lightgrey")+
  geom_point(data = data_8.2.2, aes(y = median_1_5_national, x = dif_95_05_1_5_national, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  # geom_point(data = data_8.2.2, aes(y = median_1_5_global, x = dif_95_05_1_5_global, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,4.5), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2,3))+
  ylab("Vertical distribution coefficient")+
  xlab("Horizontal distribution coefficient")+
  labs(fill = "")+
  ggtitle("National climate policy")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.3 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  geom_label(data = poly_4, aes(label = text, x = x, y = y), alpha = 0.5, size = 2.5)+
  geom_label(data = poly_5, aes(label = text, x = x, y = y), alpha = 0.2, size = 2.5)+
  theme_bw()+
  # geom_segment(data = data_8.2.2, aes(y = median_1_5_national, yend = median_1_5_global, x = dif_95_05_1_5_national, xend = dif_95_05_1_5_global), size = 0.1, colour = "lightgrey")+
  geom_point(data = data_8.2.2, aes(y = median_1_5_national, x = dif_95_05_1_5_national, fill = log(value)), shape = 21, colour = "black", size = 2.5, alpha = 0.1)+
  geom_point(data = data_8.2.2, aes(y = median_1_5_global, x = dif_95_05_1_5_global, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,4.5), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2,3))+
  ylab("Vertical distribution coefficient")+
  xlab("Horizontal distribution coefficient")+
  labs(fill = "")+
  ggtitle("International climate policy")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.4 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  geom_label(data = poly_4, aes(label = text, x = x, y = y), alpha = 0.5, size = 2.5)+
  geom_label(data = poly_5, aes(label = text, x = x, y = y), alpha = 0.2, size = 2.5)+
  theme_bw()+
  # geom_segment(data = data_8.2.2, aes(y = median_1_5_national, yend = median_1_5_global, x = dif_95_05_1_5_national, xend = dif_95_05_1_5_global), size = 0.1, colour = "lightgrey")+
  geom_point(data = data_8.2.2, aes(y = median_1_5_national, x = dif_95_05_1_5_national, fill = log(value)), shape = 21, colour = "black", size = 2.5, alpha = 0.1)+
  geom_point(data = data_8.2.2, aes(y = median_1_5_transport, x = dif_95_05_1_5_transport, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,4.5), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2,3))+
  ylab("Vertical distribution coefficient")+
  xlab("Horizontal distribution coefficient")+
  labs(fill = "")+
  ggtitle("Transport sector policy")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.5 <- ggplot()+
  geom_polygon(data = poly_3, aes(x = x, y = y, group = g), colour = "black", fill = NA, size = 0.3)+
  geom_polygon(data = filter(poly_2, g == 1), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#6F99ADFF")+
  geom_polygon(data = filter(poly_2, g == 2), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#E18727FF")+
  geom_polygon(data = filter(poly_2, g == 3), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#0072B5FF")+
  geom_polygon(data = filter(poly_2, g == 4), aes(x = x, y = y, group = g), alpha = 0.5, fill = "#FFDC91FF")+
  geom_label(data = poly_4, aes(label = text, x = x, y = y), alpha = 0.5, size = 2.5)+
  geom_label(data = poly_5, aes(label = text, x = x, y = y), alpha = 0.2, size = 2.5)+
  theme_bw()+
  # geom_segment(data = data_8.2.2, aes(y = median_1_5_national, yend = median_1_5_global, x = dif_95_05_1_5_national, xend = dif_95_05_1_5_global), size = 0.1, colour = "lightgrey")+
  geom_point(data = data_8.2.2, aes(y = median_1_5_national, x = dif_95_05_1_5_national, fill = log(value)), shape = 21, colour = "black", size = 2.5, alpha = 0.1)+
  geom_point(data = data_8.2.2, aes(y = median_1_5_electricity, x = dif_95_05_1_5_electricity, fill = log(value)), shape = 21, colour = "black", size = 2.5)+
  #geom_text_repel(data = data_7.3, aes(label = Country, y = median_1_5, x = dif_95_05_1_5),
  #                direction = "both", size = 2, max.overlaps = 100)+
  coord_cartesian(xlim = c(0,4.5), ylim = c(0,3.2))+
  scale_fill_viridis_c(breaks = c(7,11), labels = c("Poorer", "Richer"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,1,2,3))+
  ylab("Vertical distribution coefficient")+
  xlab("Horizontal distribution coefficient")+
  labs(fill = "")+
  ggtitle("Electricity sector policy")+
  # guides(fill = "none")+
  #guides(fill = guide_legend(nrow = 2))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title  = element_text(size = 7),
        legend.position = "right",
        strip.text = element_text(size = 7),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.6 <- ggarrange(P_8.0, P_8.3, P_8.4, P_8.5, common.legend = TRUE, legend = "right")

jpeg("1_Figures/Figure 2/Figure_2_2017_Policy.jpg", width = 23, height = 21, unit = "cm", res = 600)
print(P_8.6)
dev.off()

# For Manuscript

data_8.2.3 <- data_8.2.2 %>%
  mutate(regressive_national     = ifelse(median_1_5_national > 1,1,0),
         progressive_national    = ifelse(median_1_5_national < 1,1,0),
         regressive_global       = ifelse(median_1_5_global > 1,1,0),
         progressive_global      = ifelse(median_1_5_global < 1,1,0),
         regressive_electricity  = ifelse(median_1_5_electricity > 1,1,0),
         progressive_electricity = ifelse(median_1_5_electricity < 1,1,0),
         regressive_transport    = ifelse(median_1_5_transport > 1,1,0),
         progressive_transport   = ifelse(median_1_5_transport < 1,1,0),
         
         horizontal_national    = ifelse(dif_95_05_1_5_national > 1,1,0),
         horizontal_global      = ifelse(dif_95_05_1_5_global > 1,1,0),
         horizontal_electricity = ifelse(dif_95_05_1_5_electricity > 1,1,0),
         horizontal_transport   = ifelse(dif_95_05_1_5_transport > 1,1,0),
         
         hor_ver_national    = ifelse(dif_95_05_1_5_national > median_1_5_national,1,0),
         hor_ver_global      = ifelse(dif_95_05_1_5_global > median_1_5_global,1,0),
         hor_ver_electricity = ifelse(dif_95_05_1_5_electricity > median_1_5_electricity,1,0),
         hor_ver_transport   = ifelse(dif_95_05_1_5_transport > median_1_5_transport,1,0))%>%
  arrange(desc(value))%>%
  mutate(income_higher = 1:n())%>%
  mutate(income_lower = n():1)%>%
  mutate(regressive_income  = ifelse(regressive_national == 1 & income_higher < 21,1,0))%>%
  mutate(progressive_income = ifelse(progressive_national == 1 & income_lower < 21,1,0))

rm(data_8.2.0, data_8.2.1, poly, poly_2, poly_3, poly_4, poly_5, P_8.2, data_8.2.2,
   P_8.0, P_8.3, P_8.4, P_8.5, P_8.6, data_8.2.3)

# 8.3     Figure 3: Clustering and features ####

data_8.3.0 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE) %>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars("Appliance own.":"silhouette_6_means", number), ~ mean(.))%>%
  ungroup()%>%
  mutate_at(vars(-cluster, - number, - dif_95_05_1_5, -median_1_5), ~ (. - mean(.))/sd(.))%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5")%>%
  pivot_longer("Appliance own.":"silhouette_6_means", names_to = "names", values_to = "values")%>%
  filter(names != "silhouette_6_means")%>%
  mutate(names = factor(names, levels = c("Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
                                          "HH expenditures", "Sociodemographic",
                                          "Spatial", 
                                          "Electricity access", "Cooking fuel", "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")))

# Need to split up because of different scaling required

data_8.3.1 <- data_8.3.0 %>%
  filter(names != "Vertical inequality" & names != "Horizontal inequality" & names != "Mean carbon intensity")%>%
  # group_by(names)%>%
  mutate(values_rescaled = rescale(values))

data_8.3.2 <- data_8.3.0 %>%
  filter(names == "Vertical inequality" | names == "Horizontal inequality")%>%
  group_by(names)%>%
  mutate(values_new = (values - 1))%>%
  mutate(values_new = values_new/sd(values_new))%>%
  ungroup()%>%
  bind_rows(data.frame(values = 1, values_new = 0))%>%
  mutate(values_rescaled     = rescale(values, c(0,1)),
         values_rescaled_new = rescale(values_new, c(0,1)))%>%
  filter(!is.na(cluster))

data_8.3.3 <- data_8.3.1 %>%
  filter(!names %in% c("Mean carbon intensity", "Vertical inequality", "Horizontal inequality"))

data_8.3.4 <- data_8.3.0 %>%
  filter(names == "Mean carbon intensity")%>%
  mutate(values_rescaled = rescale(values))

P_8.3.1 <- ggplot(data_8.3.4)+
  geom_point(aes(y = cluster, x = names, fill = values), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.44,0.88,1)))+
  
  # scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 1)+
  #scale_fill_gradient2(na.value = NA, limits = c(0,1.5), low = "#0072B5FF", high = "#BC3C29FF", breaks = c(0,0.5,1,1.5), labels = c(0,1,2,3),
  #                     midpoint = 0.5)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(paste("Mean ", CO[2],"-intensity (", CI[r], ")", sep = ""))))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("")+
  ggtitle("")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_blank(),
        strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.3.2 <- ggplot(data_8.3.2)+
  geom_point(aes(y = cluster, x = names, fill = values_new), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.43,0.85,1)))+theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(paste("Horizontal inequality (", widehat(H)[r]^{1} ,")", sep = "")), 
                              expression(paste("Vertical inequality (", widehat(V)[r]^{1} ,")", sep = ""))))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("Country cluster")+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_blank(),
        strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.3.3 <- ggplot(data_8.3.3)+
  geom_point(aes(y = cluster, x = names, fill = values), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.27,0.54,1)))+
  scale_y_discrete(limits = rev)+
  scale_x_discrete()+
  xlab("")+ 
  guides(fill = "none")+
  ylab("")+
  # ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        #strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

L.1 <- get_legend(ggplot(data_8.3.3)+
                    geom_point(aes(x = values, y = values, fill = values_rescaled), shape = 22)+
                    scale_fill_gradientn(na.value = NA,
                                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                                         values = scales::rescale(c(0,0.22,0.54,1)),
                                         breaks = c(0,0.22,0.54,1),
                                         labels = c("Rather low", "Neutral","Rather high","High"),
                                         name = "Feature value",
                                         guide = guide_colorbar(barwidth = 10, barheight = 0.8, ticks.colour = NA))+
                    theme(legend.position = "bottom",
                          legend.title    = element_text(size = 6, vjust = 0.75, hjust = 0, margin = margin(r = 3)),
                          legend.text     = element_text(size = 5)))

P_8.3.4 <- ggarrange(P_8.3.2, P_8.3.1, P_8.3.3, nrow = 1, align = "h", widths = c(2,1,6),
                     legend.grob = L.1,
                     legend = "bottom")

jpeg("1_Figures/Figure 3/Figure_3_Corrected.jpg", width = 15.5, height = 12, unit = "cm", res = 600)
print(P_8.3.4)
dev.off()

data_8.3.0 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE) %>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars("Appliance own.":"silhouette_13_means", number), ~ mean(.))%>%
  ungroup()%>%
  mutate_at(vars(-cluster, - number, - dif_95_05_1_5, -median_1_5), ~ (. - mean(.))/sd(.))%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5")%>%
  pivot_longer("Appliance own.":"silhouette_13_means", names_to = "names", values_to = "values")%>%
  filter(names != "silhouette_13_means")%>%
  mutate(names = factor(names, levels = c("Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
                                          "HH expenditures", "Sociodemographic",
                                          "Spatial", "Electricity access", "Cooking fuel",
                                          "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")))

# Need to split up because of different scaling required

data_8.3.1 <- data_8.3.0 %>%
  filter(names != "Vertical inequality" & names != "Horizontal inequality")%>%
  # group_by(names)%>%
  mutate(values_rescaled = rescale(values))

data_8.3.2 <- data_8.3.0 %>%
  filter(names == "Vertical inequality" | names == "Horizontal inequality")%>%
  group_by(names)%>%
  mutate(values_new = (values - 1))%>%
  mutate(values_new = values_new/sd(values_new))%>%
  ungroup()%>%
  bind_rows(data.frame(values = 1, values_new = 0))%>%
  mutate(values_rescaled     = rescale(values, c(0,1)),
         values_rescaled_new = rescale(values_new, c(0,1)))%>%
  filter(!is.na(cluster))

data_8.3.3 <- data_8.3.1 %>%
  filter(!names %in% c("Mean carbon intensity", "Vertical inequality", "Horizontal inequality"))

data_8.3.1 <- data_8.3.1 %>%
  filter(names == "Mean carbon intensity")

P_8.3.1 <- ggplot(data_8.3.1)+
  geom_point(aes(y = cluster, x = names, fill = values), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.30,0.56,1)))+
  
  # scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 1)+
  #scale_fill_gradient2(na.value = NA, limits = c(0,1.5), low = "#0072B5FF", high = "#BC3C29FF", breaks = c(0,0.5,1,1.5), labels = c(0,1,2,3),
  #                     midpoint = 0.5)+
  theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(paste("Mean ", CO[2],"-intensity (", CI[r], ")", sep = ""))))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("")+
  # ggtitle("")+
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_blank(),
        strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.3.2 <- ggplot(data_8.3.2)+
  geom_point(aes(y = cluster, x = names, fill = values_new), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.33,0.66,1)))+theme_bw()+
  scale_y_discrete(limits = rev)+
  scale_x_discrete(labels = c(expression(paste("Horizontal inequality (", widehat(H)[r]^{1} ,")", sep = "")), 
                              expression(paste("Vertical inequality (", widehat(V)[r]^{1} ,")", sep = ""))))+
  xlab("")+ 
  guides(fill = "none")+
  ylab("Country cluster")+
  ggtitle("")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title  = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_blank(),
        strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_8.3.3 <- ggplot(data_8.3.3)+
  geom_point(aes(y = cluster, x = names, fill = values), shape = 22, size = 4, stroke = 0.2)+
  theme_bw()+
  scale_fill_gradientn(na.value = NA,
                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                       values = scales::rescale(c(0,0.28,0.56,1)))+
  scale_y_discrete(limits = rev)+
  scale_x_discrete()+
  xlab("")+ 
  guides(fill = "none")+
  ylab("")+
  ggtitle("")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
        axis.title.x  = element_text(size = 7),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        #strip.background = element_blank(),
        #strip.text.y = element_text(angle = 180),
        #panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(size = 0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

L.1 <- get_legend(ggplot(data_8.3.3)+
                    geom_point(aes(x = values, y = values, fill = values_rescaled), shape = 22)+
                    scale_fill_gradientn(na.value = NA,
                                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                                         values = scales::rescale(c(0,0.36,0.72,1)),
                                         breaks = c(0,0.36,0.72,1),
                                         labels = c("Rather low", "Neutral","Rather high","High"),
                                         name = "Feature value",
                                         guide = guide_colorbar(barwidth = 10, barheight = 0.8, ticks.colour = NA))+
                    theme(legend.position = "bottom",
                          legend.title    = element_text(size = 6, vjust = 0.75, hjust = 0, margin = margin(r = 3)),
                          legend.text     = element_text(size = 5)))

P_8.3.4 <- ggarrange(P_8.3.2, P_8.3.1, P_8.3.3, nrow = 1, align = "h", widths = c(2,1,6),
                     legend.grob = L.1,
                     legend = "bottom")

jpeg("1_Figures/Figure 3/Figure_3_Uncorrected.jpg", width = 15.5, height = 18, unit = "cm", res = 600)
print(P_8.3.4)
dev.off()

rm(data_8.3.0, data_8.3.1, data_8.3.2, data_8.3.3, data_8.3.4,
   P_8.3.1, P_8.3.2, P_8.3.3, P_8.3.4, L.1)

# 8.3.1   Figure 3: Supplementing table ####

data_8.3.0 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE) %>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars("Appliance own.":"silhouette_6_means", number), ~ mean(.))%>%
  ungroup()%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5",
         "Average silhouette width" = "silhouette_6_means")%>%
  select(cluster, number, "Average silhouette width", "Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
          "HH expenditures", "Sociodemographic",
          "Spatial", "Electricity access", "Cooking fuel",
          "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")%>%
  rename(Cluster = cluster,
         Number = number)%>%
  mutate_at(vars("Average silhouette width":"Appliance own."), ~ round(.,2))

kbl(data_8.3.0, format = "latex", caption = "Average feature importance across country clusters", booktabs = T, 
    vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", escape = FALSE, label = "A9")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  row_spec(0, angle = 90)%>%
  #column_spec(1:21, width = "0.5 cm")%>%
  #column_spec(1, width = "3.15 cm")%>%
  # add_header_above(c("Country" = 1, rep(c("MAE", "RMSE", "R^{2}"),3) ))%>%
  footnote(general = "This table shows the average importance of features in percent (based on absolute average SHAP-values per feature) across all countries from each cluster A to F. Feature importance is adjusted for model accuracy. Columns 'Mean carbon intensity', 'Horizontal inequality' and 'Vertical inequality' show average values. Column 'number' refers to the number of countries assigned to this cluster.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Clusters_Summary_Corrected.tex")

data_8.3.0 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE) %>%
  group_by(cluster)%>%
  mutate(number = n())%>%
  summarise_at(vars("Appliance own.":"silhouette_13_means", number), ~ mean(.))%>%
  ungroup()%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5",
         "Average silhouette width" = "silhouette_13_means")%>%
  select(cluster, number, "Average silhouette width", "Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
         "HH expenditures", "Sociodemographic",
         "Spatial", "Electricity access", "Cooking fuel",
         "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")%>%
  rename(Cluster = cluster,
         Number = number)%>%
  mutate_at(vars("Average silhouette width":"Appliance own."), ~ round(.,2))

kbl(data_8.3.0, format = "latex", caption = "Average feature importance across country clusters", booktabs = T, 
    vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "", escape = FALSE, label = "A9_Uncorrected")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  row_spec(0, angle = 90)%>%
  #column_spec(1:21, width = "0.5 cm")%>%
  #column_spec(1, width = "3.15 cm")%>%
  # add_header_above(c("Country" = 1, rep(c("MAE", "RMSE", "R^{2}"),3) ))%>%
  footnote(general = "This table shows the average importance of features in percent (based on absolute average SHAP-values per feature) across all countries from each cluster A to M. Feature importance is unadjusted for model accuracy. Columns 'Mean carbon intensity', 'Horizontal inequality' and 'Vertical inequality' show average values. Column 'number' refers to the number of countries assigned to this cluster.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Clusters_Summary_Uncorrected.tex")

rm(data_8.3.0)  

# 8.4     Figure 4: Country-level feature importance and clusters ####

for(i in c(1,2)){
  if(i == 1){
    cluster_0 <- c("A", "B", "C", "D")
  } else {cluster_0 <- c("E", "F", "G", "H", "I", "J", "K", "L", "M")}
  
  # First horizontal and vertical indicators - scaling not necessary
  
  data_8.4.1 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(cluster, Country, median_1_5, dif_95_05_1_5)%>%
    mutate_at(vars(median_1_5, dif_95_05_1_5), ~ (. - 1))%>%
    mutate_at(vars(median_1_5, dif_95_05_1_5), ~ ./sd(.))%>%
    pivot_longer("median_1_5":"dif_95_05_1_5", names_to = "names", values_to = "values")%>%
    mutate(values_rescaled = rescale(values, c(0,1)))
  
  data_8.4.1.1 <- data_8.4.1 %>%
    filter(cluster %in% cluster_0)
  
  P_8.4.1 <- ggplot(data_8.4.1.1)+
    geom_point(aes(y = Country, x = names, fill = values_rescaled), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.35,0.7,1)))+
    
    # scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 1)+
    #scale_fill_gradient2(na.value = NA, limits = c(0,1.5), low = "#0072B5FF", high = "#BC3C29FF", breaks = c(0,0.5,1,1.5), labels = c(0,1,2,3),
    #                     midpoint = 0.5)+
    theme_bw()+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete(labels = c(expression(paste("Horizontal inequality (", widehat(H)[r]^{1} ,")", sep = "")), 
                                expression(paste("Vertical inequality (", widehat(V)[r]^{1} ,")", sep = ""))))+
    xlab("")+ 
    guides(fill = "none")+
    ylab("Country")+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_blank(),
          strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Carbon intensity of consumption - scaling cannot do harm
  
  data_8.4.2 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(-silhouette_13_means)%>%
    pivot_longer("Appliance own.":"mean_carbon_intensity", names_to = "names", values_to = "values")%>%
    mutate(continent = countrycode(Country, origin = "iso3c", destination = "continent"))%>%
    filter(names %in% c("mean_carbon_intensity"))%>%
    mutate(value = (values - mean(values))/sd(values))%>%
    mutate(values_rescaled = rescale(values, c(0,1)))%>%
    filter(cluster %in% cluster_0)
  
  P_8.4.2 <- ggplot(data_8.4.2)+
    geom_point(aes(y = Country, x = names, fill = value), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    #scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0)+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.3,0.6,1)))+
    theme_bw()+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete(labels = c(expression(paste("Mean ", CO[2],"-intensity (", CI[r], ")", sep = ""))))+
    xlab("")+ 
    guides(fill = "none")+
    ylab("")+
    ggtitle("")+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x  = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_blank(),
          strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Features
  
  # Here we could show NAs as missings
  
  data_8.4.3 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(-silhouette_13_means, -mean_carbon_intensity, -median_1_5, -dif_95_05_1_5)%>%
    mutate_at(vars("Appliance own.":"Sociodemographic"), ~ ifelse(. == 0, NA, .))%>%
    # Now it is normalized
    # mutate_at(vars("Appliance own.":"Sociodemographic"), ~ (. - mean(., na.rm = TRUE))/sd(., na.rm = TRUE))%>%
    pivot_longer("Appliance own.":"Sociodemographic", names_to = "names", values_to = "values")%>%
    mutate(continent = countrycode(Country, origin = "iso3c", destination = "continent"))%>%
    mutate(help = ifelse(is.na(values), NA, "1"))%>%
    mutate(values_rescaled = rescale(values, c(0,1)))%>%
    filter(cluster %in% cluster_0)%>%
    mutate(names = factor(names, levels = c("HH expenditures", "HH size", "Education", "Gender HHH", "Sociodemographic",
                                            "Urban", "Province", "District", "Electricity access", "Cooking fuel",
                                            "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")))
  
  P_8.4.3 <- ggplot(data_8.4.3)+
    geom_point(aes(y = Country, x = names, fill = values, colour = help), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    scale_colour_manual(na.value = NA, values = c("black"))+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.09,0.35,1)))+
    # scale_fill_gradient2(na.value = NA, 
    #                      limits = c(-1.5,1.5), 
    #                      low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0)+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete()+
    xlab("")+ 
    guides(fill = "none", colour = "none")+
    ylab("")+
    ggtitle("")+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x  = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text.y = element_text(size = 6, angle = 0),
          strip.background = element_rect(size = 0.3),
          #strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Legend
  
  L.1 <- get_legend(ggplot(data_8.4.1)+
    geom_point(aes(x = values, y = values, fill = values_rescaled), shape = 22)+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.35,0.70,1)),
                         breaks = c(0,0.36,0.72,1),
                         labels = c("Rather low", "Neutral","Rather high","High"),
                         name = "Feature value",
                         guide = guide_colorbar(barwidth = 10, barheight = 0.8, ticks.colour = NA))+
    theme(legend.position = "bottom",
          legend.title    = element_text(size = 6, vjust = 0.75, hjust = 0, margin = margin(r = 3)),
          legend.text     = element_text(size = 5)))
  
  # P_8.4.4 <- align_plots(P_8.4.1, P_8.4.2, P_8.4.3, align = "h")
  
  P_8.4.4 <- ggarrange(P_8.4.1, P_8.4.2, P_8.4.3, nrow = 1, align = "h", widths = c(2,1,6),
                       legend.grob = L.1, legend = "bottom")
  
  jpeg(sprintf("1_Figures/Figure 4/Figure_4_Uncorrected_%s.jpg",i), width = 15.5, height = 18, unit = "cm", res = 600)
  print(P_8.4.4)
  dev.off()
}

for(i in c(1,2)){
  if(i == 1){
    cluster_0 <- c("A")
  } else if(i == 2){cluster_0 <- c("B","C", "D", "E", "F")}
  
  # First horizontal and vertical indicators - scaling not necessary
  
  data_8.4.1 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(cluster, Country, median_1_5, dif_95_05_1_5)%>%
    mutate_at(vars(median_1_5, dif_95_05_1_5), ~ (. - 1))%>%
    mutate_at(vars(median_1_5, dif_95_05_1_5), ~ ./sd(.))%>%
    pivot_longer("median_1_5":"dif_95_05_1_5", names_to = "names", values_to = "values")%>%
    mutate(values_rescaled = rescale(values, c(0,1)))
  
  data_8.4.1.1 <- data_8.4.1 %>%
    filter(cluster %in% cluster_0)
  
  P_8.4.1 <- ggplot(data_8.4.1.1)+
    geom_point(aes(y = Country, x = names, fill = values_rescaled), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.34,0.68,1)))+
    
    # scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 1)+
    #scale_fill_gradient2(na.value = NA, limits = c(0,1.5), low = "#0072B5FF", high = "#BC3C29FF", breaks = c(0,0.5,1,1.5), labels = c(0,1,2,3),
    #                     midpoint = 0.5)+
    theme_bw()+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete(labels = c(expression(paste("Horizontal inequality (", widehat(H)[r]^{1} ,")", sep = "")), 
                                expression(paste("Vertical inequality (", widehat(V)[r]^{1} ,")", sep = ""))))+
    xlab("")+ 
    guides(fill = "none")+
    ylab("Country")+
    ggtitle("")+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_blank(),
          strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Carbon intensity of consumption - scaling cannot do harm
  
  data_8.4.2 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(-silhouette_6_means)%>%
    pivot_longer("Appliance own.":"mean_carbon_intensity", names_to = "names", values_to = "values")%>%
    mutate(continent = countrycode(Country, origin = "iso3c", destination = "continent"))%>%
    filter(names %in% c("mean_carbon_intensity"))%>%
    mutate(value = (values - mean(values))/sd(values))%>%
    mutate(values_rescaled = rescale(values, c(0,1)))%>%
    filter(cluster %in% cluster_0)
  
  P_8.4.2 <- ggplot(data_8.4.2)+
    geom_point(aes(y = Country, x = names, fill = value), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    #scale_fill_gradient2(na.value = NA, low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0)+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.3,0.6,1)))+
    theme_bw()+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete(labels = c(expression(paste("Mean ", CO[2],"-intensity (", CI[r], ")", sep = ""))))+
    xlab("")+ 
    guides(fill = "none")+
    ylab("")+
    ggtitle("")+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x  = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text = element_blank(),
          strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Features
  
  # Here we could show NAs as missings
  
  data_8.4.3 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE)%>%
    # it is in fact not normalized
    select(cluster, Country, order, best_fit, largest_country, everything())%>%
    select(-silhouette_6_means, -mean_carbon_intensity, -median_1_5, -dif_95_05_1_5)%>%
    mutate_at(vars("Appliance own.":"Sociodemographic"), ~ ifelse(. == 0, NA, .))%>%
    # Now it is normalized
    # mutate_at(vars("Appliance own.":"Sociodemographic"), ~ (. - mean(., na.rm = TRUE))/sd(., na.rm = TRUE))%>%
    pivot_longer("Appliance own.":"Sociodemographic", names_to = "names", values_to = "values")%>%
    mutate(continent = countrycode(Country, origin = "iso3c", destination = "continent"))%>%
    mutate(help = ifelse(is.na(values), NA, "1"))%>%
    mutate(values_rescaled = rescale(values, c(0,1)))%>%
    filter(cluster %in% cluster_0)%>%
    mutate(names = factor(names, levels = c("HH expenditures", "HH size", "Education", "Gender HHH", "Sociodemographic",
                                            "Urban", "Province", "District", "Electricity access", "Cooking fuel",
                                            "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")))
  
  P_8.4.3 <- ggplot(data_8.4.3)+
    geom_point(aes(y = Country, x = names, fill = values, colour = help), shape = 22, size = 3, stroke = 0.2)+
    theme_bw()+
    scale_colour_manual(na.value = NA, values = c("black"))+
    scale_fill_gradientn(na.value = NA,
                         colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                         values = scales::rescale(c(0,0.09,0.30,1)))+
    # scale_fill_gradient2(na.value = NA, 
    #                      limits = c(-1.5,1.5), 
    #                      low = "#0072B5FF", high = "#BC3C29FF", midpoint = 0)+
    facet_grid(cluster ~ ., scales = "free", space = "free")+
    scale_y_discrete(limits = rev)+
    scale_x_discrete()+
    xlab("")+ 
    guides(fill = "none", colour = "none")+
    ylab("")+
    ggtitle("")+
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          axis.title.x  = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          strip.text.y = element_text(size = 6, angle = 0),
          strip.background = element_rect(size = 0.3),
          #strip.background = element_blank(),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.major.y = element_line(size = 0.2),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.1), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # Legend
  
  L.1 <- get_legend(ggplot(data_8.4.1)+
                      geom_point(aes(x = values, y = values, fill = values_rescaled), shape = 22)+
                      scale_fill_gradientn(na.value = NA,
                                           colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
                                           values = scales::rescale(c(0,0.34,0.68,1)),
                                           breaks = c(0,0.34,0.68,1),
                                           labels = c("Rather low", "Neutral","Rather high","High"),
                                           name = "Feature value",
                                           guide = guide_colorbar(barwidth = 10, barheight = 0.8, ticks.colour = NA))+
                      theme(legend.position = "bottom",
                            legend.title    = element_text(size = 6, vjust = 0.75, hjust = 0, margin = margin(r = 3)),
                            legend.text     = element_text(size = 5)))
  
  # P_8.4.4 <- align_plots(P_8.4.1, P_8.4.2, P_8.4.3, align = "h")
  
  P_8.4.4 <- ggarrange(P_8.4.1, P_8.4.2, P_8.4.3, nrow = 1, align = "h", widths = c(2,1,6),
                       legend.grob = L.1, legend = "bottom")
  
  jpeg(sprintf("1_Figures/Figure 4/Figure_4_Corrected_%s.jpg",i), width = 15.5, height = 18, unit = "cm", res = 600)
  print(P_8.4.4)
  dev.off()
}

rm(P_8.4.4, P_8.4.3, P_8.4.2, P_8.4.1,
   data_8.4.1, data_8.4.1.1, data_8.4.2, data_8.4.3, cluster_0, i, L.1)

# 8.4.1   Figure 4: Table outputs ####

# First horizontal and vertical indicators - scaling not necessary

data_8.4.1 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Uncorrected.csv", show_col_types = FALSE)%>%
  # it is in fact not normalized
  select(cluster, Country, order, best_fit, largest_country, everything())%>%
  mutate_at(vars("Appliance own.":"Sociodemographic"), ~ ifelse(. == 0, NA, .))%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5",
         "Silhouette width" = "silhouette_13_means")%>%
  select(cluster, Country, "Silhouette width", "Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
         "HH expenditures", "Sociodemographic",
         "Spatial", "Electricity access", "Cooking fuel",
         "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")%>%
  rename(Cluster = cluster)%>%
  mutate_at(vars("Silhouette width":"Appliance own."), ~ round(.,2))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Cluster, Country, everything(), - Country_long)

options(knitr.kable.NA = "")

kbl(data_8.4.1, format = "latex", caption = "Feature importance across countries by cluster", booktabs = T, 
    vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T, label = "A10_Uncorrected")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 8)%>%
  row_spec(0, angle = 90)%>%
  row_spec(1:87, font_size = 6)%>%
  column_spec(1:21, width = "0.35 cm")%>%
  row_spec(c(17,29,38,47,55,61,67,71,75,78,81,84,87), hline_after = TRUE)%>%
  #collapse_rows(columns = 3:4, valign = "middle")%>%
  footnote(general = "This table shows feature importance in percent (based on absolute average SHAP-values per feature) across all countries and per cluster. Feature importance is unadjusted for model accuracy. Columns 'Mean carbon intensity', 'Horizontal inequality' and 'Vertical inequality' show average values. Column 'number' refers to the number of countries assigned to this cluster.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Countries_SHAP_Summary_Uncorrected.tex")

data_8.4.1 <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE)%>%
  # it is in fact not normalized
  select(cluster, Country, order, best_fit, largest_country, everything())%>%
  mutate_at(vars("Appliance own.":"Sociodemographic"), ~ ifelse(. == 0, NA, .))%>%
  rename("Horizontal inequality" = "dif_95_05_1_5", 
         "Mean carbon intensity" = "mean_carbon_intensity",
         "Vertical inequality"   = "median_1_5",
         "Silhouette width" = "silhouette_6_means")%>%
  select(cluster, Country, "Silhouette width", "Mean carbon intensity", "Horizontal inequality", "Vertical inequality",
         "HH expenditures", "Sociodemographic",
         "Spatial", "Electricity access", "Cooking fuel",
         "Heating fuel", "Lighting fuel", "Car own.", "Motorcycle own.", "Appliance own.")%>%
  rename(Cluster = cluster)%>%
  mutate_at(vars("Silhouette width":"Appliance own."), ~ round(.,2))%>%
  left_join(select(Country.Set, Country, Country_long))%>%
  select(Cluster, Country, everything(), - Country_long)

options(knitr.kable.NA = "")

kbl(data_8.4.1, format = "latex", caption = "Feature importance across countries by cluster", booktabs = T, 
    vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T, label = "A10")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 8)%>%
  row_spec(0, angle = 90)%>%
  row_spec(1:87, font_size = 6)%>%
  column_spec(1:21, width = "0.35 cm")%>%
  row_spec(c(46,65,78,83,85,87), hline_after = TRUE)%>%
  #collapse_rows(columns = 3:4, valign = "middle")%>%
  footnote(general = "This table shows feature importance in percent (based on absolute average SHAP-values per feature) across all countries and per cluster. Feature importance is adjusted for model accuracy. Columns 'Mean carbon intensity', 'Horizontal inequality' and 'Vertical inequality' show average values. Column 'number' refers to the number of countries assigned to this cluster.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Countries_SHAP_Summary_Corrected.tex")

rm(data_8.4.1)

# 8.5     Figure 5: Joint-figures for paper and Appendix ####

eval_8.5 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD_2017.xlsx")
# eval_8.5 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Evaluation_VFOLD.xlsx")

eval_8.5.1 <- eval_8.5 %>%
  filter(.metric == "mae")%>%
  group_by(Country, number_ob, fold)%>%
  mutate(number = 1:n())%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  filter(Sample_Testing == min(Sample_Testing))%>%
  ungroup()%>%
  arrange(Country)%>%
  select(Country, number_ob)

eval_8.5.2 <- eval_8.5 %>%
  filter(.metric == "rsq")%>%
  group_by(Country, number_ob, fold)%>%
  mutate(number = 1:n())%>%
  filter(number == max(number))%>%
  ungroup()%>%
  group_by(Country, number_ob)%>%
  summarise(Sample_Testing = mean(Sample_Testing))%>%
  filter(Sample_Testing == min(Sample_Testing))%>%
  ungroup()%>%
  mutate(Sample_Testing = round(Sample_Testing,2))

 #data_8.5.1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification_VFOLD.xlsx")
data_8.5.1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification_VFOLD_2017.xlsx")%>%
  filter(number_ob %in% eval_8.5.1$number_ob)%>%
  group_by(number_ob, fold)%>%
  mutate(test = cumsum(share_SHAP)-0.00000001)%>%
  ungroup()%>%
  # arrange(Country)%>%
  # mutate(test2 = ceiling(test))%>%
  # group_by(number_ob, fold)%>%
  # filter(test2 == max(test2))%>%
  # ungroup()%>%
  # group_by(Country, fold)%>%
  # mutate(test3 = sum(share_SHAP))%>%
  # ungroup()
  select(Country, Var_0, share_SHAP, fold)%>%
  pivot_wider(names_from = "Var_0", values_from = "share_SHAP", values_fill = 0)%>%
  pivot_longer(c(-Country, -fold), names_to = "Var_0", values_to = "share_SHAP")%>%
  group_by(Country, Var_0)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  # Because of folds, some variables have values lower than 3%
  mutate(Var_0 = ifelse(share_SHAP < 0.03 & share_SHAP > 0, "Other features (Sum)", Var_0))%>%
  group_by(Country, Var_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  arrange(Country)%>%
  mutate(order = ifelse(Var_0 == "Other features (Sum)", n(), 1))%>%
  arrange(Country, order, desc(share_SHAP))%>%
  mutate(order = ifelse(order == 1, 1:n(),order))%>%
  group_by(Country)%>%
  mutate(order_new = 1:n())%>%
  ungroup()%>%
  mutate(help_0 = ifelse(order_new < 5,1,0))%>%
  filter(share_SHAP > 0)%>%
  mutate(Var_0 = ifelse(Var_0 == "ISCED", "Education",
                        ifelse(Var_0 == "HF", "Heating fuel",
                               ifelse(Var_0 == "LF", "Lighting fuel",
                                      ifelse(Var_0 == "CF", "Cooking fuel", Var_0)))))

data_8.5.2.A <- data_8.5.1 %>%
  filter(order_new < 5)

# data_8.5.2.A <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification.xlsx")%>%
#   filter(number_ob %in% eval_8.5.1$number_ob)%>%
#   mutate(lag_country = ifelse(Country != lag(Country), 1:n(),NA))%>%
#   fill(lag_country)%>%
#   mutate(lag_country = ifelse(is.na(lag_country),1,lag_country))%>%
#   group_by(Country)%>%
#   filter(lag_country == max(lag_country))%>%
#   group_by(Country, Var_0)%>%
#   mutate(number = 1:n())%>%
#   ungroup()%>%
#   arrange(Country, desc(share_SHAP))%>%
#   group_by(Country)%>%
#   slice_head(n = 4)%>%
#   ungroup()

# data_8.5.2.B <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD.xlsx")%>%
data_8.5.2.B <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail_VFOLD_2017.xlsx")%>%
  filter(number_ob %in% eval_8.5.1$number_ob)%>%
  mutate(lag_country = ifelse(Country != lag(Country), 1:n(),NA))%>%
  fill(lag_country)%>%
  mutate(lag_country = ifelse(is.na(lag_country),1,lag_country))%>%
  group_by(Country)%>%
  filter(lag_country == max(lag_country))%>%
  ungroup()%>%
  group_by(Country, Var_0, Var_1)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  group_by(Country, Var_0, Var_1)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  arrange(Country, desc(share_SHAP))

data_8.5.2.C <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized_Corrected.csv", show_col_types = FALSE)%>%
  left_join(ungroup(summarise(group_by(mutate(data_2, pop = hh_size*hh_weights), Country), pop = sum(pop))))%>%
  group_by(cluster)%>%
  mutate(most_pop = ifelse(pop == max(pop),1,0))%>%
  ungroup()%>%
  select(-pop)

data_8.5.2.D <- data_8.5.2.B %>%
  group_by(Country, Var_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  arrange(Country, desc(share_SHAP))

# for-Loop for all countries

list_C <- list()

for (i in c(data_8.5.2.C$Country)){
  
    list_A <- list()
    list_B <- list()
  
    data_8.5.2 <- data_8.5.1 %>%
      filter(Country == i)%>%
      mutate(help_0 = ifelse(!"HH expenditures" %in% filter(data_8.5.2.A, Country == i)$Var_0 & order_new == 4,0,
                             ifelse(!"HH expenditures" %in% filter(data_8.5.2.A, Country == i)$Var_0 & Var_0 == "HH expenditures",1,help_0)))
    
    title_0 <- paste0("Cluster ", data_8.5.2.C$cluster[data_8.5.2.C$Country == i],": ",
                      Country.Set$Country_long[Country.Set$Country == i], " (")
    
    if(i == "DOM"){
      title_0 <- paste0("Cluster ", data_8.5.2.C$cluster[data_8.5.2.C$Country == i],": ",
                        "Dom. Republic", " (")
    }
    
    title_1 <- paste0("=", format(eval_8.5.2$Sample_Testing[eval_8.5.2$Country == i], nsmall = 2),")")
    
    P_8.5 <- ggplot(data_8.5.2)+
      geom_col(aes(x = share_SHAP, y = reorder(Var_0, desc(order)), fill = factor(help_0)), width = 0.7, colour = "black", size = 0.3)+
      theme_bw()+
      coord_cartesian(xlim = c(0,max(signif(max(data_8.5.2$share_SHAP) + 0.1,1),0.3000001)))+
      scale_fill_manual(values = c("#E64B35FF","#6F99ADFF"))+
      scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
      guides(fill = "none")+
      xlab("Feature importance (SHAP)")+
      ylab("Feature")+
      ggtitle(bquote(.(title_0) *R^2* .(title_1)))+
      # ggtitle(paste0("Cluster ", data_8.5.2.C$cluster[data_8.5.2.C$Country == i],
      #                ": ", Country.Set$Country_long[Country.Set$Country == i]), " (")+
      theme(axis.text.y = element_text(size = 6), 
            axis.text.x = element_text(size = 6),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 9),
            plot.title.position = "plot",
            legend.position = "bottom",
            # strip.text = element_text(size = 7),
            #strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))
    
    list_A[[i]] <- P_8.5
    
    data_8.5.2.A.1 <- data_8.5.2.A %>%
      filter(Country == i)%>%
      mutate(Var_1 = ifelse(Var_0 == "Car own.", "car.01", 
                            ifelse(Var_0 == "HH size", "hh_size",
                                   ifelse(Var_0 == "Electricity access", "electricity.access",
                                          ifelse(Var_0 == "Motorcycle own.", "motorcycle.01",
                                                 ifelse(Var_0 == "Gender HHH", "sex_hhh",
                                                        ifelse(Var_0 %in% c("CF", "HF", "LF", "HH expenditures", "District", "Province",
                                                                            "Ethnicity", "Nationality", "Religion", "Appliance own."), Var_0, 
                                                               ifelse(Var_0 == "Urban", "urban_01", 
                                                                      ifelse(Var_0 == "ISCED", "ISCED",NA)))))))))
    
    data_8.5.2.D.1 <- data_8.5.2.D %>%
      filter(Country == i)%>%
      mutate(number = ifelse(Var_0 == "HH expenditures", 0, 1:n()))%>%
      arrange(number)%>%
      slice_head(n = 4)%>%
      ungroup()%>%
      mutate(Var_1 = ifelse(Var_0 == "Car own.", "car.01", 
                            ifelse(Var_0 == "HH size", "hh_size",
                                   ifelse(Var_0 == "Electricity access", "electricity.access",
                                          ifelse(Var_0 == "Motorcycle own.", "motorcycle.01",
                                                 ifelse(Var_0 == "Gender HHH", "sex_hhh",
                                                        ifelse(Var_0 %in% c("CF", "HF", "LF", "HH expenditures", "District", "Province",
                                                                            "Ethnicity", "Nationality", "Religion", "Appliance own."), Var_0, 
                                                               ifelse(Var_0 == "Urban", "urban_01", 
                                                                      ifelse(Var_0 == "ISCED", "ISCED",
                                                                             ifelse(Var_0 == "Religiosity", "Religiosity", NA))))))))))
    
    labels_dataframe <- data.frame(Var_1 = c("car.01", "electricity.access", "hh_size", "motorcycle.01", "sex_hhh", "CF", "HF", "LF",
                                             "HH expenditures", "urban_01", "District", "Province", "Appliance own.", "Ethnicity", "ISCED", "Nationality", "Religion", "Religiosity"),
                                   XLAB = c("Car ownership", "Electricity access", "Household size", "Motorcycle ownership",
                                            "Gender of household head", "Cooking fuel", "Heating fuel", "Lighting fuel", "Household expenditures", "Urban",
                                            "District", "Province", "Appliance ownership", "Ethnicity", "Education of household head", "Nationality", "Religion", "Religiosity"),
                                   YLAB = c("SHAP value for car ownership", "SHAP value for electricity access", "SHAP value for household size",
                                            "SHAP value for motorcycle ownership", "SHAP value for gender of household head", 
                                            "SHAP value for cooking fuel", "SHAP value for heating fuel", "SHAP value for lighting fuel", NA, "SHAP for urban/rural",
                                            "SHAP value for district", "SHAP value for province", "SHAP value for appliance ownership", 
                                            "SHAP value for ethnicity", "SHAP value for education of hosuehold head", "SHAP value for nationality", "SHAP value for religion", "SHAP value for religiosity"))%>%
      left_join(select(data_8.5.2.D.1, Var_1, share_SHAP), by = c("Var_1" = "Var_1"))%>%
      mutate(share_SHAP = round(share_SHAP,2))%>%
      mutate(help = ifelse(Var_1 %in% c("HH expenditures", "Appliance own."), Var_1, 
                           ifelse(Var_1 == "ISCED", "Education",
                                  ifelse(Var_1 == "hh_size", "HH size",
                                         ifelse(Var_1 == "motorcycle.01", "Motorcycle",
                                                ifelse(Var_1 == "sex_hhh", "Gender HHH", 
                                                       ifelse(Var_1 == "electricity.access", "Electricity", 
                                                              ifelse(Var_1 == "car.01", "Car own.", XLAB))))))))%>%
      mutate(title = paste0(help, " (Importance: ", share_SHAP*100,"%)"))
    
    data_8.5.2.D.1 <- data_8.5.2.D.1 %>%
      filter(Var_0 != "HH expenditures")
    
    data_8.5.2.0 <- read_rds(sprintf("../0_Data/9_Supplementary Data/BRT-Tracking/SHAP-Values en detail/2017/SHAP_wide_%s.rds",i))
    
    if(nrow(data_8.5.2.0)<5000){
      alpha_0 <- 0.75
    } else if (nrow(data_8.5.2.0)<10000){
      alpha_0 <- 0.5
    } else if (nrow(data_8.5.2.0)<20000){
      alpha_0 <- 0.3
    } else if (nrow(data_8.5.2.0)<50000){
      alpha_0 <- 0.2
    } else alpha_0 <- 0.1
    
    # colnames(data_8.5.2.0)
    
    # Expenditures dataframe
    
    data_8.5.2.1 <- data_8.5.2.0 %>%
      select(SHAP_hh_expenditures_USD_2014, hh_expenditures_USD_2014, z_score_exp)%>%
      filter(z_score_exp < 3)
    
    P_8.5.2.1 <- ggplot(data_8.5.2.1)+
      geom_hline(aes(yintercept = 0))+
      geom_point(aes(x = hh_expenditures_USD_2014,
                     y = SHAP_hh_expenditures_USD_2014,
                     colour = z_score_exp),
                 size = 0.5, alpha = alpha_0)+
      geom_smooth(aes(x     = hh_expenditures_USD_2014,
                      y     = SHAP_hh_expenditures_USD_2014),
                  method = "loess", color = "black", size = 0.4, se = FALSE,
                  formula = y ~ x)+
      theme_bw()+
      scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
      scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
      # scale_fill_viridis_c()+
      # scale_colour_viridis_c()+
      #scale_color_gradientn(na.value = NA,
      #                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
      #                       values = scales::rescale(c(0,0.3,0.5,1)))+
      guides(colour = "none", fill = "none")+
      ggtitle(paste0(labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"]))+
      coord_cartesian(xlim = c(0,max(data_8.5.2.1$hh_expenditures_USD_2014)))+ # TBA
      scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0), n.breaks = 4)+
      xlab("Household expenditures in US-$ (2014)")+
      ylab("SHAP value for household expenditures")+
      theme(axis.text.y = element_text(size = 6), 
            axis.text.x = element_text(size = 6),
            axis.title  = element_text(size = 7),
            plot.title = element_text(size = 8),
            plot.subtitle = element_text(size = 6),
            legend.position = "bottom",
            # strip.text = element_text(size = 7),
            #strip.text.y = element_text(angle = 180),
            #panel.grid.major = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size = 7),
            legend.title = element_text(size = 7),
            plot.margin = unit(c(0.3,0.4,0.3,0.3), "cm"),
            panel.border = element_rect(size = 0.3))
    
    list_B[[paste0(i,"_0")]] <- P_8.5.2.1
    list_C[[paste0(i, "_B_0")]] <- P_8.5.2.1
    
    list_0 <- list()
    
    # Easily computable variables
    
    for(j in c("car.01", "electricity.access", "hh_size", "motorcycle.01", "sex_hhh", 
               "CF", "HF", "LF", "urban_01")){
      if(j %in% data_8.5.2.D.1$Var_1 | (j == "sex_hhh" & "Gender" %in% data_8.5.2.D.1$Var_1)){
        data_8.5.2.2 <- data_8.5.2.0 %>%
          rename_if(str_detect(names(.), "SHAP_HH size"), ~paste0("SHAP_hh_size"))%>%
          rename_if(str_detect(names(.), "SHAP_Car own."), ~paste0("SHAP_car.01"))%>%
          rename_if(str_detect(names(.), "SHAP_Motorcycle own."), ~paste0("SHAP_motorcycle.01"))%>%
          rename_if(str_detect(names(.), "SHAP_Gender"), ~paste0("SHAP_sex_hhh"))%>%
          rename_if(str_detect(names(.), "Gender"), ~paste0("sex_hhh"))%>%
          rename_if(str_detect(names(.), "SHAP_Urban"), ~paste0("SHAP_urban_01"))%>%
          rename_if(str_detect(names(.), "SHAP_Electricity access"), ~paste0("SHAP_electricity.access"))%>%
          select(ends_with(j), z_score_exp)%>%
          rename_if(str_detect(names(.), "SHAP"), ~paste0("SHAP"))%>%
          rename(Variable = j)%>%
          filter(z_score_exp < 3)%>%
          mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))
        
        if(j == "motorcycle.01"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(Variable == 0, "No motorcycle", "Owns a motorcycle"))%>%
            mutate(Variable = factor(Variable, levels = c("No motorcycle", "Owns a motorcycle")))
        }
        
        if(j == "car.01"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(Variable == 0, "No car", "Owns a car"))%>%
            mutate(Variable = factor(Variable, levels = c("No car", "Owns a car")))
        }
        
        if(j == "LF"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse((Variable %in% c("Firewood", "Gas", "Other biomass")) & SHAP == 0 | i == "ETH" & Variable == "other" | i == "KEN" & Variable == "other", "Other lighting", Variable))
        
          if(i == "RWA"){
            data_8.5.2.2 <- data_8.5.2.2 %>%
              mutate(Variable = ifelse(Variable == "other", "Fuelwood or liquid", Variable))
          }
          
          }
        
        if(j == "CF"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(i == "PER" & (Variable %in% c("Unknown", "Other biomass", "Coal")) & SHAP == 0, "Other cooking", Variable))
          
        }
        
        if(j == "electricity.access"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(Variable == 0, "No access", "Access"))
        }
        
        if(j == "urban_01"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(Variable == 0, "Rural", "Urban"))
        }
        
        if(j == "sex_hhh"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate()
        }
        
        if(j == "hh_size"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            filter(Variable < 6)
        }
        
        text_size_0 <- if(j %in% c("CF", "HF", "LF")){text_size_0 <- 5}else{text_size_0 <- 6}
        
        P_8.5.2.2 <- ggplot(data_8.5.2.2)+
          geom_hline(aes(yintercept = 0))+
          # geom_violin(aes(x = Variable,
          #                 y = SHAP),
          #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
          #ggforce::geom_sina(aes(x = Variable,
          #                       y = SHAP,
          #                       colour = z_score_exp,
          #                       fill = z_score_exp),
          #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
          geom_jitter(aes(x = as.factor(Variable),
                          y = SHAP,
                          colour = z_score_exp,
                          fill = z_score_exp),
                      size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
          theme_bw()+
          scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          # scale_colour_viridis_c()+
          # scale_fill_viridis_c()+
          guides(colour = "none", fill = "none")+
          ggtitle(labels_dataframe$title[labels_dataframe$Var_1 == j])+
          xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
          ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
          theme(axis.text.y = element_text(size = 6), 
                axis.text.x = element_text(size = text_size_0),
                axis.title  = element_text(size = 7),
                plot.title = element_text(size = 8),
                plot.subtitle = element_text(size = 6),
                legend.position = "bottom",
                # strip.text = element_text(size = 7),
                #strip.text.y = element_text(angle = 180),
                #panel.grid.major = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.ticks = element_line(size = 0.2),
                legend.text = element_text(size = 7),
                legend.title = element_text(size = 7),
                plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
                panel.border = element_rect(size = 0.3))
        
        list_0[[j]] <- P_8.5.2.2
        
      }
      
    }
    
    # Other variables
    
    for(j in c("District", "Province", "Ethnicity", "ISCED", "Nationality", "Religion", "Religiosity")){
      if(j %in% data_8.5.2.D.1$Var_1){
        data_8.5.2.2 <- data_8.5.2.0 %>%
          #rename_if(str_detect(names(.), "SHAP_HH size"), ~paste0("SHAP_hh_size"))%>%
          select(ends_with(j), z_score_exp)%>%
          rename_if(str_detect(names(.), "SHAP"), ~paste0("SHAP"))%>%
          rename(Variable = j)%>%
          filter(z_score_exp < 3)%>%
          mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))
        
        # Variable-specific adjustment
        
        data_8.5.2.B.1 <- data_8.5.2.B %>%
          filter(Country == i)%>%
          filter(Var_0 == j)%>%
          arrange(desc(share_SHAP))%>%
          slice_head(n = 5)%>%
          mutate(order_no = 1:n())
        
        data_8.5.2.B.2 <- data_8.5.2.2 %>%
          group_by(Variable)%>%
          summarise(number = n())%>%
          ungroup()%>%
          arrange(desc(number))%>%
          slice_head(n = 5)
        
        if(j == "Province" & i == "EGY"){
          data_8.5.2.2 <- data_8.5.2.2 %>%
            mutate(Variable = ifelse(Variable == "Rural Lower Egypt", "Rur. Lower",
                                     ifelse(Variable == "Rural Upper Egypt", "Rur. Upper",
                                            ifelse(Variable == "Urban Lower Egypt", "Urb. Lower",
                                                   ifelse(Variable == "Urban Upper Egypt", "Urb. Upper",
                                                          ifelse(Variable == "Urban Governorates", "Urb. Gov.", 
                                                                 ifelse(Variable == "Frontier Governorates", "Other", Variable)))))))
        }
        
        data_8.5.2.3 <- data_8.5.2.2 %>%
          mutate(Variable = str_replace(Variable, "-"," "))%>%
          mutate(Variable = str_replace(Variable, "\\."," "))%>%
          mutate(Var_imp = ifelse(Variable %in% data_8.5.2.B.1$Var_1, "1","0"),
                 Var_big = ifelse(Variable %in% data_8.5.2.B.2$Variable, "1","0"))%>%
          left_join(select(data_8.5.2.B.1, Var_1, share_SHAP, order_no), by = c("Variable" = "Var_1"))%>%
          arrange(desc(share_SHAP))%>%
          mutate(Variable = ifelse(i == "SUR" & Variable == "Mixed", "other", Variable))%>%
          mutate(order_no = ifelse(Var_imp == "0",1,order_no+1))%>%
          group_by(Variable)%>%
          mutate(var_ID = cur_group_id())%>%
          ungroup()%>%
          mutate(Variable = ifelse((Var_imp == "0" & max(order_no)>5 & i != "SUR" & i != "EGY") | (Var_imp == "0" & max(var_ID)>5 & i != "SUR" & i != "EGY"), "Other", Variable))%>%
          mutate(Variable = ifelse(i == "SUR" & (j == "District") & Var_imp == "0", "Other", 
                                   ifelse(i == "SUR" & (j == "District") & Var_imp == "1", str_sub(Variable,4,-1), Variable)))%>%
          mutate(Variable = ifelse(i == "SUR" & (j == "Religion") & Var_imp == "0", "Other", 
                                   ifelse(i == "SUR" & (j == "Religion") & Var_imp == "1", str_sub(Variable,1,-1), Variable)))%>%
          mutate(Variable = ifelse(i == "SUR" & j == "Ethnicity" & is.na(Variable), "Other", Variable))%>%
          mutate(Variable = ifelse(i == "NGA" & Variable != "Other", str_sub(Variable, 5,-1), Variable))%>%
          mutate(Variable = ifelse(i == "BFA" & Variable == "Centre Est", "Cen. Est", Variable))%>%
          mutate(Variable = ifelse(nchar(Variable) > 6, paste0(str_sub(Variable, 1,6),"."), Variable))
        
        text_size_0 <- if(j == "Province" | j == "District"){text_size_0 <- 5}else{text_size_0 <- 6}

        if(j == "Province" | j == "Ethnicity"){
          data_8.5.2.3 <- data_8.5.2.3 %>%
            mutate(Variable = str_to_title(Variable))}
        
        if(j == "Province" & i == "UGA"){
          data_8.5.2.3 <- data_8.5.2.3 %>%
            mutate(Variable = ifelse(Variable == 1, "Kampala",
                                     ifelse(Variable == 2, "Central 1",
                                            ifelse(Variable == 3, "Central 2",
                                                   ifelse(Variable == 5, "Bukedi",
                                                          ifelse(Variable == 10, "Ancholi", Variable))))))
        }
        
        if(j == "Province" & i == "IND"){
          data_8.5.2.3 <- data_8.5.2.3 %>%
            mutate(Variable = ifelse(Variable == "09", "Uttar P.",
                                     ifelse(Variable == "14", "Manipur",
                                            ifelse(Variable == "18", "Assam",
                                                   ifelse(Variable == "19", "West Ben.",
                                                          ifelse(Variable == "20", "Jhark.", Variable))))))
        }
        
        if(j == "Religiosity"){
          data_8.5.2.3 <- data_8.5.2.3 %>%
            mutate(Variable = ifelse(Variable == 1, "Secular",
                                     ifelse(Variable == 2, "Trad.",
                                            ifelse(Variable == 3, "Religious",
                                                   ifelse(Variable == 4, "Orthodox", "other")))))
        }
        
        
        P_8.5.2.3 <- ggplot(data_8.5.2.3)+
          geom_hline(aes(yintercept = 0))+
          # geom_violin(aes(x = Variable,
          #                 y = SHAP),
          #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
          #ggforce::geom_sina(aes(x = Variable,
          #                       y = SHAP,
          #                       colour = z_score_exp,
          #                       fill = z_score_exp),
          #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
          geom_jitter(aes(x = fct_reorder(as.factor(Variable),order_no),
                          y = SHAP,
                          colour = z_score_exp,
                          fill = z_score_exp),
                      size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
          theme_bw()+
          scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          # scale_colour_viridis_c()+
          # scale_fill_viridis_c()+
          guides(colour = "none", fill = "none")+
          ggtitle(labels_dataframe$title[labels_dataframe$Var_1 == j])+
          xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
          ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
          theme(axis.text.y = element_text(size = 6), 
                axis.text.x = element_text(size = text_size_0),
                axis.title  = element_text(size = 7),
                plot.title = element_text(size = 8),
                plot.subtitle = element_text(size = 6),
                legend.position = "bottom",
                # strip.text = element_text(size = 7),
                #strip.text.y = element_text(angle = 180),
                #panel.grid.major = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.ticks = element_line(size = 0.2),
                legend.text = element_text(size = 7),
                legend.title = element_text(size = 7),
                plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
                panel.border = element_rect(size = 0.3))
        
        list_0[[j]] <- P_8.5.2.3
      }
    }
    
    for(j in c("Appliance own.")){
      if(j %in% data_8.5.2.D.1$Var_1){
        data_8.5.2.2 <- data_8.5.2.0 %>%
          rename_if(str_detect(names(.), "SHAP_Washing_machine"), ~paste0("SHAP_washing_machine.01"))%>%
          rename_if(str_detect(names(.), "SHAP_Refrigerator"), ~paste0("SHAP_refrigerator.01"))%>%
          rename_if(str_detect(names(.), "SHAP_TV"), ~paste0("SHAP_tv.01"))%>%
          rename_if(str_detect(names(.), "SHAP_AC"), ~paste0("SHAP_ac.01"))%>%
          select(ends_with("washing_machine.01"), ends_with("refrigerator.01"), ends_with("tv.01"), ends_with("ac.01"), z_score_exp)%>%
          mutate(id = 1:n())
        
        if("washing_machine.01" %in% colnames(data_8.5.2.2)){
          data_8.5.2.2.1 <- data_8.5.2.2 %>%
            select(ends_with("washing_machine.01"), id)%>%
            rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
            rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
            mutate(Variable = ifelse(Variable == 0, "No WM", "Has WM"))%>%
            mutate(Var_1 = "Washing machine")
        } else data_8.5.2.2.1 <- select(data_8.5.2.2, id)
        
        
        if("refrigerator.01" %in% colnames(data_8.5.2.2)){
          data_8.5.2.2.2 <- data_8.5.2.2 %>%
            select(ends_with("refrigerator.01"), id)%>%
            rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
            rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
            mutate(Variable = ifelse(Variable == 0, "No refrig.", "Has refrig."))%>%
            mutate(Var_1 = "Refrigerator")
        } else data_8.5.2.2.2 <- select(data_8.5.2.2, id)
        
        if("tv.01" %in% colnames(data_8.5.2.2)){
          data_8.5.2.2.3 <- data_8.5.2.2 %>%
            select(ends_with("tv.01"), id)%>%
            rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
            rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
            mutate(Variable = ifelse(Variable == 0, "No TV", "Has TV"))%>%
            mutate(Var_1 = "TV")
        }else data_8.5.2.2.3 <- select(data_8.5.2.2, id)
        
        if("ac.01" %in% colnames(data_8.5.2.2)){
          data_8.5.2.2.4 <- data_8.5.2.2 %>%
            select(ends_with("ac.01"), id)%>%
            rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
            rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
            mutate(Variable = ifelse(Variable == 0, "No AC", "Has AC"))%>%
            mutate(Var_1 = "AC")
        } else data_8.5.2.2.4 <- select(data_8.5.2.2, id)
        
        data_8.5.2.2.5 <- data.frame()%>%
          bind_rows(data_8.5.2.2.1)%>%
          bind_rows(data_8.5.2.2.2)%>%
          bind_rows(data_8.5.2.2.3)%>%
          bind_rows(data_8.5.2.2.4)%>%
          left_join(select(data_8.5.2.2, id, z_score_exp), by = "id")
        
        # Variable-specific adjustment
        
        data_8.5.2.B.1 <- data_8.5.2.B %>%
          filter(Country == i)%>%
          filter(Var_0 == j)%>%
          arrange(desc(share_SHAP))%>%
          slice_head(n = 2)%>%
          mutate(order_no = 1:n())
        
        data_8.5.2.3 <- data_8.5.2.2.5 %>%
          mutate(Var_imp = ifelse(Var_1 %in% data_8.5.2.B.1$Var_1, "1","0"))%>%
          filter(Var_imp == "1")%>%
          arrange(Var_1, desc(Variable))%>%
          mutate(order_no = 1:n())%>%
          group_by(Var_1, Variable)%>%
          mutate(order_no = min(order_no))%>%
          ungroup()
        
        P_8.5.2.3 <- ggplot(data_8.5.2.3)+
          geom_hline(aes(yintercept = 0))+
          # geom_violin(aes(x = Variable,
          #                 y = SHAP),
          #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
          #ggforce::geom_sina(aes(x = Variable,
          #                       y = SHAP,
          #                       colour = z_score_exp,
          #                       fill = z_score_exp),
          #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
          geom_jitter(aes(x = fct_reorder(as.factor(Variable),order_no),
                          y = SHAP,
                          colour = z_score_exp,
                          fill = z_score_exp),
                      size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
          theme_bw()+
          scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
          # scale_colour_viridis_c()+
          # scale_fill_viridis_c()+
          guides(colour = "none", fill = "none")+
          ggtitle(labels_dataframe$title[labels_dataframe$Var_1 == j])+
          xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
          ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
          theme(axis.text.y = element_text(size = 6), 
                axis.text.x = element_text(size = 6),
                axis.title  = element_text(size = 7),
                plot.title = element_text(size = 8),
                plot.subtitle = element_text(size = 6),
                legend.position = "bottom",
                # strip.text = element_text(size = 7),
                #strip.text.y = element_text(angle = 180),
                #panel.grid.major = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.ticks = element_line(size = 0.2),
                legend.text = element_text(size = 7),
                legend.title = element_text(size = 7),
                plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
                panel.border = element_rect(size = 0.3))
        
        list_0[[j]] <- P_8.5.2.3
      }
    }
    

    list_B[[paste0(i,"_1")]] <- list_0[[data_8.5.2.D.1$Var_1[1]]]
    list_B[[paste0(i,"_2")]] <- list_0[[data_8.5.2.D.1$Var_1[2]]]
    list_B[[paste0(i,"_3")]] <- list_0[[data_8.5.2.D.1$Var_1[3]]]
    
    P_8.5.0 <- ggarrange(list_A[[1]], list_B[[1]], list_B[[2]], list_B[[3]], list_B[[4]], align = "h", nrow = 1)
    
    list_C[[i]] <- P_8.5.0
    list_C[[paste0(i, "_A")]] <- P_8.5
    list_C[[paste0(i, "_B_1")]] <- list_0[[data_8.5.2.D.1$Var_1[1]]]
    list_C[[paste0(i, "_B_2")]] <- list_0[[data_8.5.2.D.1$Var_1[2]]]
    list_C[[paste0(i, "_B_3")]] <- list_0[[data_8.5.2.D.1$Var_1[3]]]
    
    print(i)

}

P_8.5.0.I   <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "A"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "A"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "A"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "A"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "A"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "B"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "B"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "B"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "B"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "B"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "C"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "C"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "C"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "C"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "C"],"_B_3")[1]]],
                         
                         ncol = 5, nrow = 3, align = "h")
P_8.5.0.II  <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "D"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "D"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "D"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "D"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "D"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "E"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "E"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "E"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "E"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "E"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "F"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "F"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "F"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "F"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "F"],"_B_3")[1]]],
                         
                         ncol = 5, nrow = 3, align = "h")
P_8.5.0.III <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "G"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "G"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "G"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "G"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "G"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "H"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "H"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "H"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "H"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "H"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "I"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "I"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "I"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "I"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "I"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "J"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "J"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "J"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "J"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "J"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "K"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "K"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "K"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "K"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "K"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "L"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "L"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "L"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "L"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "L"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "M"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "M"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "M"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "M"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "M"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "N"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "N"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "N"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "N"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "N"],"_B_3")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "O"],"_A")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "O"],"_B_0")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "O"],"_B_1")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "O"],"_B_2")[1]]],
                         # list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$best_fit == 1 & data_8.5.2.C$cluster == "O"],"_B_3")[1]]],
                         ncol = 5, nrow = 3, align = "h")

P_8.5.0.L   <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "A"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "A"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "A"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "A"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "A"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "B"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "B"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "B"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "B"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "B"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "C"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "C"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "C"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "C"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "C"],"_B_3")[1]]],
                         
                         ncol = 5, nrow = 3, align = "h")
P_8.5.0.L2  <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "D"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "D"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "D"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "D"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "D"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "E"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "E"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "E"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "E"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "E"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "F"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "F"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "F"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "F"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "F"],"_B_3")[1]]],
                         
                         ncol = 5, nrow = 3, align = "h")
P_8.5.0.L3 <- ggarrange(list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "G"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "G"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "G"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "G"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "G"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "H"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "H"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "H"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "H"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "H"],"_B_3")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "I"],"_A")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "I"],"_B_0")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "I"],"_B_1")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "I"],"_B_2")[1]]],
                         list_C[[paste0(data_8.5.2.C$Country[data_8.5.2.C$largest_country == 1 & data_8.5.2.C$cluster == "I"],"_B_3")[1]]],
                         
                         ncol = 5, nrow = 3, align = "h")

jpeg("1_Figures/Figure 5/Figures_joint_%d.jpg", width = 30, height = 28, unit = "cm", res = 400)
print(P_8.5.0.I)
print(P_8.5.0.II)
print(P_8.5.0.III)
dev.off()

jpeg("1_Figures/Figure 5/Figures_joint_large_%d.jpg", width = 30, height = 28, unit = "cm", res = 400)
print(P_8.5.0.L)
print(P_8.5.0.L2)
print(P_8.5.0.L3)
dev.off()

# jpeg("1_Figures/Figure 5/Figures_joint_3.jpg", width = 30, height = 30, unit = "cm", res = 300)
# print(P_8.5.0.III)
# dev.off()

for (i in c(data_8.5.2.C$Country)){
  P_8.5.1 <- list_C[[paste0(i)[1]]]
  
  jpeg(sprintf("1_Figures/Figure 5b/Figure_5b_%s.jpg", i), width = 30, height = 10, unit = "cm", res = 300)
  print(P_8.5.1)
  dev.off()
}

# Supplementary analysis

data_8.5.2.E <- data_8.5.2.D %>%
  group_by(Country)%>%
  mutate(max_0 = ifelse(share_SHAP == max(share_SHAP),1,0))%>%
  ungroup()

rm(list_A, list_B, list_C,
   P_8.5.0.I, P_8.5.0.II, P_8.5.0.III, P_8.5, data_8.5.1, data_8.5.2,
   P_8.5.0.L, P_8.5.0.L2, P_8.5.0.L3,
   P_8.5.2.1, P_8.5.2.2, P_8.5.2.3, P_8.5.0, P_8.5.1,
   data_8.5.2.A, data_8.5.2.B, data_8.5.2.C, data_8.5.2.D,
   data_8.5.2.A.1, data_8.5.2.B.1, data_8.5.2.B.2, data_8.5.2.D.1,
   data_8.5.2.0, data_8.5.2.1, data_8.5.2.2,
   data_8.5.2.2.1, data_8.5.2.2.2, data_8.5.2.2.3, data_8.5.2.2.4, data_8.5.2.2.5,
   data_8.5.2.3, labels_dataframe, list_0, alpha_0, i, j, text_size_0, eval_8.5, eval_8.5.1, eval_8.5.2, title_0, title_1)

# 8.5.1   Figure 5a: Feature importance (SHAP) on the country-level ####

# Introduce for-loop for each country that loads in shap_1

data_8.5.1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification.xlsx")%>%
  filter(number_ob %in% eval_8.5.1$number_ob)%>%
  group_by(number_ob, Var_0)%>%
  mutate(number = 1:n())%>%
  filter(number == max(number))%>%
  ungroup()%>%
  filter(Country != "SRB" | Var_0 != "Province")%>%
  group_by(number_ob)%>%
  mutate(test = sum(share_SHAP))%>%
  ungroup()%>%
  select(Country, Var_0, share_SHAP)%>%
  arrange(Country)%>%
  mutate(order = ifelse(Var_0 == "Other features (Sum)", n(), 1))%>%
  arrange(Country, order, desc(share_SHAP))%>%
  mutate(order = ifelse(order == 1, 1:n(),order))%>%
  group_by(Country)%>%
  mutate(order_new = 1:n())%>%
  ungroup()%>%
  mutate(help_0 = ifelse(order_new < 5,1,0))%>%
  filter(share_SHAP > 0)%>%
  mutate(Var_0 = ifelse(Var_0 == "ISCED", "Education",
                        ifelse(Var_0 == "HF", "Heating fuel",
                               ifelse(Var_0 == "LF", "Lighting fuel",
                                      ifelse(Var_0 == "CF", "Cooking fuel", Var_0)))))

for(i in Country.Set$Country){
  data_8.5.2 <- data_8.5.1 %>%
    filter(Country == i)
  
  P_8.5 <- ggplot(data_8.5.2)+
    geom_col(aes(x = share_SHAP, y = reorder(Var_0, desc(order)), fill = factor(help_0)), width = 0.7, colour = "black", size = 0.3)+
    theme_bw()+
    coord_cartesian(xlim = c(0,signif(max(data_8.5.2$share_SHAP) + 0.1,1)))+
    scale_fill_manual(values = c("#E64B35FF","#6F99ADFF"))+
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    guides(fill = "none")+
    xlab("Feature importance (SHAP)")+
    ylab("Feature")+
    ggtitle(Country.Set$Country_long[Country.Set$Country == i])+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 11),
          legend.position = "bottom",
          # strip.text = element_text(size = 7),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("1_Figures/Figure 5a/Figure_5a_%s.jpg", i), width = 10, height = 10, unit = "cm", res = 200)
  print(P_8.5)
  dev.off()
  
}

rm(P_8.5, data_8.5.1, data_8.5.2)

# 8.5.2   Figure 5b: Partial dependence plots (SHAP) ####

data_8.5.2.A <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Classification.xlsx")%>%
  filter(number_ob %in% eval_8.5.1$number_ob)%>%
  mutate(lag_country = ifelse(Country != lag(Country), 1:n(),NA))%>%
  fill(lag_country)%>%
  mutate(lag_country = ifelse(is.na(lag_country),1,lag_country))%>%
  group_by(Country)%>%
  filter(lag_country == max(lag_country))%>%
  group_by(Country, Var_0)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  arrange(Country, desc(share_SHAP))%>%
  group_by(Country)%>%
  slice_head(n = 4)%>%
  ungroup()

data_8.5.2.B <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_SHAP_Detail.xlsx")%>%
  filter(number_ob %in% eval_8.5.1$number_ob)%>%
  mutate(lag_country = ifelse(Country != lag(Country), 1:n(),NA))%>%
  fill(lag_country)%>%
  mutate(lag_country = ifelse(is.na(lag_country),1,lag_country))%>%
  group_by(Country)%>%
  filter(lag_country == max(lag_country))%>%
  group_by(Country, Var_0, Var_1)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  arrange(Country, desc(share_SHAP))

data_8.5.2.C <- read_csv("../0_Data/9_Supplementary Data/BRT-Tracking/Clusters_Normalized.csv", show_col_types = FALSE)

data_8.5.2.D <- data_8.5.2.B %>%
  group_by(Country, Var_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  arrange()%>%
  arrange(Country, desc(share_SHAP))

list_all <- list()

 for (i in c(Country.Set$Country)){
  
  data_8.5.2.A.1 <- data_8.5.2.A %>%
    filter(Country == i)%>%
    mutate(Var_1 = ifelse(Var_0 == "Car own.", "car.01", 
                          ifelse(Var_0 == "HH size", "hh_size",
                                 ifelse(Var_0 == "Electricity access", "electricity.access",
                                        ifelse(Var_0 == "Motorcycle own.", "motorcycle.01",
                                               ifelse(Var_0 == "Gender HHH", "sex_hhh",
                                                      ifelse(Var_0 %in% c("CF", "HF", "LF", "HH expenditures", "District", "Province",
                                                                          "Ethnicity", "Nationality", "Religion", "Appliance own."), Var_0, 
                                                             ifelse(Var_0 == "Urban", "urban_01", 
                                                                    ifelse(Var_0 == "ISCED", "ISCED",NA)))))))))
  
  data_8.5.2.D.1 <- data_8.5.2.D %>%
    filter(Country == i)%>%
    slice_head(n = 4)%>%
    ungroup()%>%
    mutate(Var_1 = ifelse(Var_0 == "Car own.", "car.01", 
                          ifelse(Var_0 == "HH size", "hh_size",
                                 ifelse(Var_0 == "Electricity access", "electricity.access",
                                        ifelse(Var_0 == "Motorcycle own.", "motorcycle.01",
                                               ifelse(Var_0 == "Gender HHH", "sex_hhh",
                                                      ifelse(Var_0 %in% c("CF", "HF", "LF", "HH expenditures", "District", "Province",
                                                                          "Ethnicity", "Nationality", "Religion", "Appliance own."), Var_0, 
                                                             ifelse(Var_0 == "Urban", "urban_01", 
                                                                    ifelse(Var_0 == "ISCED", "ISCED",NA)))))))))
  
  labels_dataframe <- data.frame(Var_1 = c("car.01", "electricity.access", "hh_size", "motorcycle.01", "sex_hhh", "CF", "HF", "LF",
                                           "HH expenditures", "urban_01", "District", "Province", "Appliance own.", "Ethnicity", "ISCED", "Nationality", "Religion"),
                                 XLAB = c("Car ownership", "Electricity access", "Household size", "Motorcycle ownership",
                                          "Gender of household head", "Cooking fuel", "Heating fuel", "Lighting fuel", "Household expenditures", "Urban",
                                          "District", "Province", "Appliance ownership", "Ethnicity", "Education of household head", "Nationality", "Religion"),
                                 YLAB = c("SHAP value for car ownership", "SHAP value for electricity access", "SHAP value for household size",
                                          "SHAP value for motorcycle ownership", "SHAP value for gender of household head", 
                                          "SHAP value for cooking fuel", "SHAP value for heating fuel", "SHAP value for lighting fuel", NA, "SHAP for urban/rural",
                                          "SHAP value for district", "SHAP value for province", "SHAP value for appliance ownership", 
                                          "SHAP value for ethnicity", "SHAP value for education of hosuehold head", "SHAP value for nationality", "SHAP value for religion"))%>%
    left_join(select(data_8.5.2.D.1, Var_1, share_SHAP), by = c("Var_1" = "Var_1"))%>%
    mutate(share_SHAP = round(share_SHAP,2))%>%
    mutate(title = paste0(XLAB, " (norm. abs. SHAP: ", share_SHAP,")"))
  
  data_8.5.2.D.1 <- data_8.5.2.D.1 %>%
    filter(Var_0 != "HH expenditures")
   
  data_8.5.2.0 <- read_rds(sprintf("../0_Data/9_Supplementary Data/BRT-Tracking/SHAP-Values en detail/SHAP_wide_%s.rds",i))

  if(nrow(data_8.5.2.0)<500){
    alpha_0 <- 0.75
  } else if (nrow(data_8.5.2.0)<5000){
      alpha_0 <- 0.6
  } else if (nrow(data_8.5.2.0)<10000){
        alpha_0 <- 0.4
  } else {
          alpha_0 <- 0.3}
  
  # colnames(data_8.5.2.0)
  
  # Expenditures dataframe
  
  data_8.5.2.1 <- data_8.5.2.0 %>%
    select(SHAP_hh_expenditures_USD_2014, hh_expenditures_USD_2014, z_score_exp)%>%
    filter(z_score_exp < 3)
  
  P_8.5.2.1 <- ggplot(data_8.5.2.1)+
    geom_hline(aes(yintercept = 0))+
    geom_smooth(aes(x     = hh_expenditures_USD_2014,
                    y     = SHAP_hh_expenditures_USD_2014),
                method = "loess", color = "black", size = 0.15, se = FALSE,
                formula = y ~ x)+
    geom_point(aes(x = hh_expenditures_USD_2014,
                   y = SHAP_hh_expenditures_USD_2014,
                   colour = z_score_exp),
               size = 0.5, alpha = alpha_0)+
    theme_bw()+
    scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
    scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
    # scale_fill_viridis_c()+
    # scale_colour_viridis_c()+
    #scale_color_gradientn(na.value = NA,
    #                       colors = c("#0072B5FF", "white","#BC3C29FF", "#631879FF"),
    #                       values = scales::rescale(c(0,0.3,0.5,1)))+
    guides(colour = "none", fill = "none")+
    ggtitle(paste0(Country.Set$Country_long[Country.Set$Country == i], " (Cluster ",
                   data_8.5.2.C$cluster[data_8.5.2.C$Country == i],"): "), 
                   labels_dataframe$title[labels_dataframe$Var_1 == "HH expenditures"])+
    coord_cartesian(xlim = c(0,max(data_8.5.2.1$hh_expenditures_USD_2014)))+ # TBA
    scale_x_continuous(labels = scales::dollar_format(), expand = c(0,0))+
    xlab("Household expenditures in US-$ (2014)")+
    ylab("SHAP value for household expenditures")+
    theme(axis.text.y = element_text(size = 6), 
          axis.text.x = element_text(size = 6),
          axis.title  = element_text(size = 7),
          plot.title = element_text(size = 8),
          plot.subtitle = element_text(size = 6),
          legend.position = "bottom",
          # strip.text = element_text(size = 7),
          #strip.text.y = element_text(angle = 180),
          #panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))
  
  # jpeg(sprintf("1_Figures/Figure 5b/Figure_5b_%s.jpg", "ISRAEL"), width = 10, height = 10, unit = "cm", res = 500)
  # print(P_8.5.2.1)
  # dev.off()
  
  list_0 <- list()
  
  # Easily computable variables
  
  for(j in c("car.01", "electricity.access", "hh_size", "motorcycle.01", "sex_hhh", 
               "CF", "HF", "LF", "urban_01")){
    if(j %in% colnames(data_8.5.2.0) | (j == "sex_hhh" & "Gender" %in% colnames(data_8.5.2.0))){
      data_8.5.2.2 <- data_8.5.2.0 %>%
        rename_if(str_detect(names(.), "SHAP_HH size"), ~paste0("SHAP_hh_size"))%>%
        rename_if(str_detect(names(.), "SHAP_Car own."), ~paste0("SHAP_car.01"))%>%
        rename_if(str_detect(names(.), "SHAP_Motorcycle own."), ~paste0("SHAP_motorcycle.01"))%>%
        rename_if(str_detect(names(.), "SHAP_Gender"), ~paste0("SHAP_sex_hhh"))%>%
        rename_if(str_detect(names(.), "Gender"), ~paste0("sex_hhh"))%>%
        rename_if(str_detect(names(.), "SHAP_Urban"), ~paste0("SHAP_urban_01"))%>%
        rename_if(str_detect(names(.), "SHAP_Electricity access"), ~paste0("SHAP_electricity.access"))%>%
        select(ends_with(j), z_score_exp)%>%
        rename_if(str_detect(names(.), "SHAP"), ~paste0("SHAP"))%>%
        rename(Variable = j)%>%
        filter(z_score_exp < 3)%>%
        mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))
        
      if(j == "motorcycle.01"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse(Variable == 0, "No motorcycle", "Owns a motorcycle"))%>%
          mutate(Variable = factor(Variable, levels = c("No motorcycle", "Owns a motorcycle")))
      }
      
      if(j == "car.01"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse(Variable == 0, "No car", "Owns a car"))%>%
          mutate(Variable = factor(Variable, levels = c("No car", "Owns a car")))
      }
      
      if(j == "LF"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse((Variable %in% c("Firewood", "Gas", "Other biomass")) & SHAP == 0, "Other lighting", Variable))
      }
      
      if(j == "CF"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse(i == "PER" & (Variable %in% c("Unknown", "Other biomass", "Coal")) & SHAP == 0, "Other cooking", Variable))
          
      }
      
      if(j == "electricity.access"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse(Variable == 0, "No access", "Access"))
      }
      
      if(j == "urban_01"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate(Variable = ifelse(Variable == 0, "Rural", "Urban"))
      }
      
      if(j == "sex_hhh"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          mutate()
      }
      
      if(j == "hh_size"){
        data_8.5.2.2 <- data_8.5.2.2 %>%
          filter(Variable < 6)
      }
      
      P_8.5.2.2 <- ggplot(data_8.5.2.2)+
        geom_hline(aes(yintercept = 0))+
        # geom_violin(aes(x = Variable,
        #                 y = SHAP),
        #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
        #ggforce::geom_sina(aes(x = Variable,
        #                       y = SHAP,
        #                       colour = z_score_exp,
        #                       fill = z_score_exp),
        #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
        geom_jitter(aes(x = as.factor(Variable),
                        y = SHAP,
                        colour = z_score_exp,
                        fill = z_score_exp),
                    size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
        theme_bw()+
        scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        # scale_colour_viridis_c()+
        # scale_fill_viridis_c()+
        guides(colour = "none", fill = "none")+
        ggtitle("",labels_dataframe$title[labels_dataframe$Var_1 == j])+
        xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
        ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
        theme(axis.text.y = element_text(size = 6), 
              axis.text.x = element_text(size = 6),
              axis.title  = element_text(size = 7),
              plot.title = element_text(size = 8),
              plot.subtitle = element_text(size = 6),
              legend.position = "bottom",
              # strip.text = element_text(size = 7),
              #strip.text.y = element_text(angle = 180),
              #panel.grid.major = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_line(size = 0.2),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
              panel.border = element_rect(size = 0.3))
      
      list_0[[j]] <- P_8.5.2.2
      
    }
    
  }

  # Other variables
  
  for(j in c("District", "Province", "Ethnicity", "ISCED", "Nationality", "Religion")){
    if(j %in% colnames(data_8.5.2.0)){
      data_8.5.2.2 <- data_8.5.2.0 %>%
        #rename_if(str_detect(names(.), "SHAP_HH size"), ~paste0("SHAP_hh_size"))%>%
        select(ends_with(j), z_score_exp)%>%
        rename_if(str_detect(names(.), "SHAP"), ~paste0("SHAP"))%>%
        rename(Variable = j)%>%
        filter(z_score_exp < 3)%>%
        mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))
 
      # Variable-specific adjustment
      
      data_8.5.2.B.1 <- data_8.5.2.B %>%
        filter(Country == i)%>%
        filter(Var_0 == j)%>%
        arrange(desc(share_SHAP))%>%
        slice_head(n = 5)%>%
        mutate(order_no = 1:n())
      
      data_8.5.2.B.2 <- data_8.5.2.2 %>%
        group_by(Variable)%>%
        summarise(number = n())%>%
        ungroup()%>%
        arrange(desc(number))%>%
        slice_head(n = 5)
      
      data_8.5.2.3 <- data_8.5.2.2 %>%
        mutate(Variable = str_replace(Variable, "-"," "))%>%
        mutate(Variable = str_replace(Variable, "\\."," "))%>%
        mutate(Var_imp = ifelse(Variable %in% data_8.5.2.B.1$Var_1, "1","0"),
               Var_big = ifelse(Variable %in% data_8.5.2.B.2$Variable, "1","0"))%>%
        left_join(select(data_8.5.2.B.1, Var_1, share_SHAP, order_no), by = c("Variable" = "Var_1"))%>%
        arrange(desc(share_SHAP))%>%
        mutate(Variable = ifelse(i == "SUR" & Variable == "Mixed", "other", Variable))%>%
        mutate(order_no = ifelse(Var_imp == "0",1,order_no+1))%>%
        group_by(Variable)%>%
        mutate(var_ID = cur_group_id())%>%
        ungroup()%>%
        mutate(Variable = ifelse((Var_imp == "0" & max(order_no)>5 & i != "SUR") | (Var_imp == "0" & max(var_ID)>5 & i != "SUR"), "Other", Variable))
      
      P_8.5.2.3 <- ggplot(data_8.5.2.3)+
        geom_hline(aes(yintercept = 0))+
        # geom_violin(aes(x = Variable,
        #                 y = SHAP),
        #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
        #ggforce::geom_sina(aes(x = Variable,
        #                       y = SHAP,
        #                       colour = z_score_exp,
        #                       fill = z_score_exp),
        #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
        geom_jitter(aes(x = fct_reorder(as.factor(Variable),order_no),
                        y = SHAP,
                        colour = z_score_exp,
                        fill = z_score_exp),
                    size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
        theme_bw()+
        scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        # scale_colour_viridis_c()+
        # scale_fill_viridis_c()+
        guides(colour = "none", fill = "none")+
        ggtitle("",labels_dataframe$title[labels_dataframe$Var_1 == j])+
        xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
        ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
        theme(axis.text.y = element_text(size = 6), 
              axis.text.x = element_text(size = 6),
              axis.title  = element_text(size = 7),
              plot.title = element_text(size = 8),
              plot.subtitle = element_text(size = 6),
              legend.position = "bottom",
              # strip.text = element_text(size = 7),
              #strip.text.y = element_text(angle = 180),
              #panel.grid.major = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_line(size = 0.2),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
              panel.border = element_rect(size = 0.3))
      
      list_0[[j]] <- P_8.5.2.3
    }
  }
  
  for(j in c("Appliance own.")){
    if(j %in% data_8.5.2.D.1$Var_1){
      data_8.5.2.2 <- data_8.5.2.0 %>%
        rename_if(str_detect(names(.), "SHAP_Washing_machine"), ~paste0("SHAP_washing_machine.01"))%>%
        rename_if(str_detect(names(.), "SHAP_Refrigerator"), ~paste0("SHAP_refrigerator.01"))%>%
        rename_if(str_detect(names(.), "SHAP_TV"), ~paste0("SHAP_tv.01"))%>%
        rename_if(str_detect(names(.), "SHAP_AC"), ~paste0("SHAP_ac.01"))%>%
        select(ends_with("washing_machine.01"), ends_with("refrigerator.01"), ends_with("tv.01"), ends_with("ac.01"), z_score_exp)%>%
        mutate(id = 1:n())
      
      if("washing_machine.01" %in% colnames(data_8.5.2.2)){
        data_8.5.2.2.1 <- data_8.5.2.2 %>%
          select(ends_with("washing_machine.01"), id)%>%
          rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
          rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
          mutate(Variable = ifelse(Variable == 0, "No washing machine", "Has washing machine"))%>%
          mutate(Var_1 = "Washing machine")
      } else data_8.5.2.2.1 <- select(data_8.5.2.2, id)
      
      
      if("refrigerator.01" %in% colnames(data_8.5.2.2)){
        data_8.5.2.2.2 <- data_8.5.2.2 %>%
          select(ends_with("refrigerator.01"), id)%>%
          rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
          rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
          mutate(Variable = ifelse(Variable == 0, "No refrigerator", "Has refrigerator"))%>%
          mutate(Var_1 = "Refrigerator")
      } else data_8.5.2.2.2 <- select(data_8.5.2.2, id)
      
      if("tv.01" %in% colnames(data_8.5.2.2)){
        data_8.5.2.2.3 <- data_8.5.2.2 %>%
          select(ends_with("tv.01"), id)%>%
          rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
          rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
          mutate(Variable = ifelse(Variable == 0, "No TV", "Has TV"))%>%
          mutate(Var_1 = "TV")
      }else data_8.5.2.2.3 <- select(data_8.5.2.2, id)
      
      if("ac.01" %in% colnames(data_8.5.2.2)){
        data_8.5.2.2.4 <- data_8.5.2.2 %>%
          select(ends_with("ac.01"), id)%>%
          rename_if(str_detect(names(.), "SHAP"), ~ paste0("SHAP"))%>%
          rename_if(str_detect(names(.), ".01"), ~ paste0("Variable"))%>%
          mutate(Variable = ifelse(Variable == 0, "No AC", "Has AC"))%>%
          mutate(Var_1 = "AC")
      } else data_8.5.2.2.4 <- select(data_8.5.2.2, id)
      
      data_8.5.2.2.5 <- data.frame()%>%
        bind_rows(data_8.5.2.2.1)%>%
        bind_rows(data_8.5.2.2.2)%>%
        bind_rows(data_8.5.2.2.3)%>%
        bind_rows(data_8.5.2.2.4)%>%
        left_join(select(data_8.5.2.2, id, z_score_exp), by = "id")
      
      # Variable-specific adjustment
      
      data_8.5.2.B.1 <- data_8.5.2.B %>%
        filter(Country == i)%>%
        filter(Var_0 == j)%>%
        arrange(desc(share_SHAP))%>%
        slice_head(n = 2)%>%
        mutate(order_no = 1:n())

      data_8.5.2.3 <- data_8.5.2.2.5 %>%
        mutate(Var_imp = ifelse(Var_1 %in% data_8.5.2.B.1$Var_1, "1","0"))%>%
        filter(Var_imp == "1")%>%
        arrange(Var_1, desc(Variable))%>%
        mutate(order_no = 1:n())%>%
        group_by(Var_1, Variable)%>%
        mutate(order_no = min(order_no))%>%
        ungroup()
      
      P_8.5.2.3 <- ggplot(data_8.5.2.3)+
        geom_hline(aes(yintercept = 0))+
        # geom_violin(aes(x = Variable,
        #                 y = SHAP),
        #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
        #ggforce::geom_sina(aes(x = Variable,
        #                       y = SHAP,
        #                       colour = z_score_exp,
        #                       fill = z_score_exp),
        #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
        geom_jitter(aes(x = fct_reorder(as.factor(Variable),order_no),
                        y = SHAP,
                        colour = z_score_exp,
                        fill = z_score_exp),
                    size = 1, alpha = alpha_0, height = 0, width = 0.25, shape = 21)+
        theme_bw()+
        scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
        # scale_colour_viridis_c()+
        # scale_fill_viridis_c()+
        guides(colour = "none", fill = "none")+
        ggtitle("", labels_dataframe$title[labels_dataframe$Var_1 == j])+
        xlab(labels_dataframe$XLAB[labels_dataframe$Var_1 == j])+
        ylab(labels_dataframe$YLAB[labels_dataframe$Var_1 == j])+
        theme(axis.text.y = element_text(size = 6), 
              axis.text.x = element_text(size = 6),
              axis.title  = element_text(size = 7),
              plot.title = element_text(size = 8),
              plot.subtitle = element_text(size = 6),
              legend.position = "bottom",
              # strip.text = element_text(size = 7),
              #strip.text.y = element_text(angle = 180),
              #panel.grid.major = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.ticks = element_line(size = 0.2),
              legend.text = element_text(size = 7),
              legend.title = element_text(size = 7),
              plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
              panel.border = element_rect(size = 0.3))
      
      list_0[[j]] <- P_8.5.2.3
    }
  }
  
  P_8.5.2.2 <- list_0[[data_8.5.2.D.1$Var_1[1]]]
  P_8.5.2.3 <- list_0[[data_8.5.2.D.1$Var_1[2]]]
  P_8.5.2.4 <- list_0[[data_8.5.2.D.1$Var_1[3]]]
  P_8.5.2.5 <- ggarrange(P_8.5.2.1, P_8.5.2.2, P_8.5.2.3, P_8.5.2.4, align = "h", nrow = 1)
  
  jpeg(sprintf("1_Figures/Figure 5b/Figure_5b_%s.jpg", i), width = 30, height = 10, unit = "cm", res = 500)
  print(P_8.5.2.5)
  dev.off()
  
  list_all[[i]] <- P_8.5.2.5
    
}

rm(P_8.5.2.1, P_8.5.2.2, P_8.5.2.3, P_8.5.2.4, P_8.5.2.5,
   data_8.5.2.A, data_8.5.2.B, data_8.5.2.D,
   data_8.5.2.A.1, data_8.5.2.B.1, data_8.5.2.B.2, data_8.5.2.D.1,
   data_8.5.2.0, data_8.5.2.1, data_8.5.2.2,
   data_8.5.2.2.1, data_8.5.2.2.2, data_8.5.2.2.3, data_8.5.2.2.4, data_8.5.2.2.5,
   data_8.5.2.3, labels_dataframe, list_0, alpha_0, i, j)

# Groups ABCD

P_8.5.2.5.A <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "A" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.B <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "B" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.C <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "C" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.D <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "D" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.6 <- ggarrange(P_8.5.2.5.A,P_8.5.2.5.B,P_8.5.2.5.C,P_8.5.2.5.D, align = "v", ncol = 1)

# Groups EFGH

P_8.5.2.5.E <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "E" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.F <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "F" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.G <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "G" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.H <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "H" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.I <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "I" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.7 <- ggarrange(P_8.5.2.5.E,P_8.5.2.5.F,P_8.5.2.5.G,P_8.5.2.5.H,P_8.5.2.5.I, align = "v", ncol = 1)

# Groups IJKLMN

P_8.5.2.5.J <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "J" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.K <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "K" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.L <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "L" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.M <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "M" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.5.N <- list_all[[data_8.5.2.C$Country[data_8.5.2.C$cluster == "N" & data_8.5.2.C$best_fit == 1]]]
P_8.5.2.8 <- ggarrange(P_8.5.2.5.J, P_8.5.2.5.K, P_8.5.2.5.L, P_8.5.2.5.M, P_8.5.2.5.N, align = "v", ncol = 1)

jpeg("1_Figures/Figure 5b/Figures_joint_%d.jpg", width = 30, height = 30, unit = "cm", res = 500)
print(P_8.5.2.6)
print(P_8.5.2.7)
print(P_8.5.2.8)
dev.off()

rm(P_8.5.2.5.A, P_8.5.2.5.B, P_8.5.2.5.C, P_8.5.2.5.D, 
   P_8.5.2.5.E, P_8.5.2.5.F, P_8.5.2.5.G, P_8.5.2.5.H, 
   P_8.5.2.5.I, P_8.5.2.5.J, P_8.5.2.5.K, P_8.5.2.5.L, P_8.5.2.5.M, 
   P_8.5.2.6, P_8.5.2.7, P_8.5.2.8 )

# 8.5.3   Information 5c: Direction of effects ####

data_8.5.3.0 <- data.frame()

for (i in c(Country.Set$Country)){
  
  data_8.5.3.1 <- read_rds(sprintf("../0_Data/9_Supplementary Data/BRT-Tracking/SHAP-Values en detail/2017/SHAP_wide_%s.rds",i))%>%
    mutate(Country = i)
  
  data_8.5.3.0 <- data_8.5.3.0 %>%
    bind_rows(data_8.5.3.1)
  
  print(i)
}

# Urban

data_8.5.3.2 <- data_8.5.3.0 %>%
  select(Country, SHAP_Urban, urban_01)%>%
  filter(!is.na(urban_01))%>%
  group_by(Country, urban_01)%>%
  summarise(mean_SHAP_urban = mean(SHAP_Urban))%>%
  ungroup()%>%
  pivot_wider(names_from = "urban_01", values_from = "mean_SHAP_urban", names_prefix = "urban_")%>%
  mutate(Urban = ifelse(urban_1 > urban_0,1,0))

# Provinces

data_8.5.3.3 <- data_8.5.3.0 %>%
  select(Country, SHAP_Province, Province)%>%
  filter(!is.na(Province))%>%
  group_by(Country, Province)%>%
  summarise(mean_SHAP_Province = mean(SHAP_Province))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_Province))

# District

data_8.5.3.4 <- data_8.5.3.0 %>%
  select(Country, SHAP_District, District)%>%
  filter(!is.na(District))%>%
  group_by(Country, District)%>%
  summarise(mean_SHAP_District = mean(SHAP_District))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_District))

# CF

data_8.5.3.5 <- data_8.5.3.0 %>%
  select(Country, SHAP_CF, CF)%>%
  filter(!is.na(CF))%>%
  group_by(Country, CF)%>%
  summarise(mean_SHAP_CF = mean(SHAP_CF))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_CF))

# LF

data_8.5.3.6 <- data_8.5.3.0 %>%
  select(Country, SHAP_LF, LF)%>%
  filter(!is.na(LF))%>%
  group_by(Country, LF)%>%
  summarise(mean_SHAP_LF = mean(SHAP_LF))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_LF))

# HF

data_8.5.3.7 <- data_8.5.3.0 %>%
  select(Country, SHAP_HF, HF)%>%
  filter(!is.na(HF))%>%
  group_by(Country, HF)%>%
  summarise(mean_SHAP_HF = mean(SHAP_HF))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_HF))

# ISCED

data_8.5.3.8 <- data_8.5.3.0 %>%
  select(Country, SHAP_ISCED, ISCED)%>%
  filter(!is.na(ISCED))%>%
  group_by(Country, ISCED)%>%
  summarise(mean_SHAP_ISCED = mean(SHAP_ISCED))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_ISCED))

# Religiosity

data_8.5.3.9 <- data_8.5.3.0 %>%
  select(Country, SHAP_Religiosity, Religiosity)%>%
  filter(!is.na(Religiosity))%>%
  group_by(Country, Religiosity)%>%
  summarise(mean_SHAP_Religiosity = mean(SHAP_Religiosity))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_Religiosity))

# Religion

data_8.5.3.10 <- data_8.5.3.0 %>%
  select(Country, SHAP_Religion, Religion)%>%
  filter(!is.na(Religion))%>%
  group_by(Country, Religion)%>%
  summarise(mean_SHAP_Religion = mean(SHAP_Religion))%>%
  ungroup()%>%
  arrange(Country, desc(mean_SHAP_Religion))

test <- data_8.4.1 %>%
  mutate(gender = ifelse(is.na(`Gender HHH`),0,`Gender HHH`),
         sociodemographic = ifelse(is.na(Sociodemographic),0,Sociodemographic),
         education   = ifelse(is.na(Education),0,Education))%>%
  rowwise()%>%
  mutate(aggregate = sum(gender + sociodemographic + education))%>%
  mutate(agg_2 = ifelse(aggregate > 0.03,1,0))

# 8.6.0   Figures for Appendix ####
# 8.6.1   Figure parametric Engel-curves ####

Country.Set.8.6.1 <- Country.Set %>%
  mutate(Group_1 = c(rep("A",30), rep("B",30), rep("C",27)))

data_8.6.1.1 <- data_2 %>%
  left_join(Country.Set.8.6.1)%>%
  mutate(Country_long = fct_reorder(Country_long,hh_expenditures_USD_2014_mean, min))%>%
  select(hh_id, hh_weights, Country, starts_with("share_"), hh_expenditures_USD_2014_pc, Group_1, Country_long)%>%
  select(-share_other_binning)%>%
  rename(share_Energy = share_energy, share_Food = share_food, share_Services = share_services, share_Goods = share_goods)%>%
  pivot_longer(starts_with("share"), names_to = "Type", values_to = "Share", names_prefix = "share_")

data_8.6.1.2 <- data_2 %>%
  left_join(Country.Set.8.6.1)%>%
  mutate(Country_long = fct_reorder(Country_long,hh_expenditures_USD_2014_mean, min))%>%
  group_by(Country_long, Income_Group_5, Group_1)%>%
  summarise(mean_hh_expenditures_USD_2014_pc = wtd.mean(hh_expenditures_USD_2014_pc, hh_weights))%>%
  ungroup()

for(Group_0 in c("A", "B", "C")){
  
    data_8.6.1.4 <- data_8.6.1.2 %>%
      filter(Group_1 == Group_0)
  
  if(Group_0 == "A"){
    upper_bound <- 5500
    breaks      <- c(0,2500,5000)
    binwidth_0    <- 250
    scale_0       <- 400
    accuracy_2nd  <- 0.01
  }
  
  if(Group_0 == "B"){
    upper_bound   <- 11500
    breaks        <- c(0,5000,10000)
    binwidth_0    <- 500
    scale_0       <- 1200
    accuracy_2nd  <- 0.01
  }
  
  if(Group_0 == "C"){
    upper_bound <- 75000
    breaks      <- c(0,25000,50000)
    binwidth_0    <- 2000
    scale_0       <- 5000
    accuracy_2nd  <- 0.01
  }
    
  data_8.6.1.3 <- data_8.6.1.1 %>%
    filter(Group_1 == Group_0)%>%
    mutate(Share = Share/scale_0)
  
  P_8.6.1.1 <- ggplot()+
    geom_vline(data = data_8.6.1.4, aes(xintercept = mean_hh_expenditures_USD_2014_pc), size = 0.2)+
    geom_histogram(data = data_8.6.1.3, aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, group = Country_long, y = ..density..),
                   binwidth = binwidth_0, fill = "lightgrey", colour = "black", alpha = 0.6, size = 0.05)+
    stat_smooth(data = data_8.6.1.3, 
                aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                    y = Share, fill = Type, colour = Type),
                level = 0.95, method = "lm", formula = y ~ x + I(x^2), size = 0.3, fullrange = FALSE)+
    theme_bw()+
    facet_wrap(. ~ Country_long, ncol = 5)+
    xlab("Household expenditures per capita in US-$ (2017)") + ylab("Share of total expenditures")+
    scale_fill_nejm()+
    scale_colour_nejm()+
    labs(colour = "", fill = "")+
    coord_cartesian(xlim = c(0, upper_bound), ylim = c(0,0.00022*4000/scale_0))+
    scale_y_continuous(expand = c(0,0),
                       labels = scales::percent_format(accuracy = 1, scale = scale_0*100),
                       sec.axis = sec_axis(~ ., labels = scales::percent_format(accuracy = accuracy_2nd), name = "Density of households"))+
    scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0),
                       breaks = breaks)+
    theme(axis.text.y = element_text(size = 5), 
          axis.text.x = element_text(size = 5),
          axis.title  = element_text(size = 6),
          plot.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 6),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.2),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
          panel.border = element_rect(size = 0.3))

  jpeg(sprintf("1_Figures/Analysis_Parametric_Engel_Curves/Parametric_EC_0_%s.jpg", Group_0), width = 15.5, height = 18, unit = "cm", res = 600)
  print(P_8.6.1.1)
  dev.off()
  
  rm(data_8.6.1.3, data_8.6.1.4, P_8.6.1.1, accuracy_2nd, binwidth_0, breaks, scale_0, upper_bound, Group_0)
  
}

rm(data_8.6.1.1, data_8.6.1.2, Country.Set.8.6.1)
