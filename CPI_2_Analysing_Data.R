# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1     Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "countrycode","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "xtable")

options(scipen=999,
        dplyr.summarise.inform = FALSE)

# 1       Loading data ####

data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")

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

# 3.      Summary statistics ####

Country.Set <- distinct(data_2, Country)%>%
  mutate(Country_long = countrycode(Country, origin = "iso3c", destination = "country.name"))%>%
  left_join(ungroup(summarise(group_by(data_2, Country), hh_expenditures_USD_2014_mean = wtd.mean(hh_expenditures_USD_2014))))%>%
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

sum_3.1.3 <- left_join(sum_3.1.1, sum_3.1.2, by = "Country")
  
colnames(sum_3.1.3) <- c("Country", "Observations", "Average \nHousehold Size", "Urban \nPopulation", "Electricity \nAccess", "Average \nHousehold \nExpenditures [USD]", "Car \nOwnership", "Share of \nFirewood or \n Charcoal Cons.")

kbl(mutate_all(sum_3.1.3,linebreak), format = "latex", linesep = "", booktabs = T, longtable = T,
    caption = "Summary statistics", format.args = list(big.mark = ",", scientific = FALSE), align = "lrcccccc")%>%
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
  mutate_at(vars(starts_with("share_energy")), list(~ paste0(round(.*100,1), "%")))

colnames(sum_3.2.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(sum_3.2.3, format = "latex", caption = "Average household expenditures and average energy expenditure shares per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "",
    longtable = T)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 8)%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.13 cm")%>%
  column_spec(8:13, width = "1.04 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average household expenditures [USD]" = 6, "Average energy expenditure shares" = 6))%>%
  footnote(general = "This table shows average household expenditures and average energy expenditure shares for households in our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Summary_A2.tex")

rm(sum_3.2.1, sum_3.2.2, sum_3.2.3)

# 3.3     Carbon footprint and burden national ####

sum_3.3.1 <- data_2 %>%
  group_by(Country)%>%
  summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
            burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()

sum_3.3.2 <- data_2 %>%
  group_by(Country, Income_Group_5)%>%
  summarise(CO2_t_national      = wtd.mean(CO2_t_national,      weights = hh_weights),
            burden_CO2_national = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  pivot_wider(names_from = "Income_Group_5", values_from = c("CO2_t_national", "burden_CO2_national"))

sum_3.3.3 <- left_join(sum_3.3.1, sum_3.3.2, by = "Country")%>%
  select(Country, starts_with("CO2_t_national"), starts_with("burden_CO2_national"))%>%
  mutate_at(vars(starts_with("CO2_t_national")), list(~ round(.,1)))%>%
  mutate_at(vars(starts_with("burden_CO2_national")), list(~ paste0(round(.*100,2), "%")))

colnames(sum_3.3.3) <- c("Country", rep(c("All","EQ1","EQ2","EQ3","EQ4","EQ5"),2))

kbl(sum_3.3.3, format = "latex", caption = "Average carbon footprint and average USD/tCO$_{2}$ carbon price incidence per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", linesep = "",
    longtable = T)%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "repeat_header"), font_size = 9)%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.05 cm")%>%
  column_spec(8:13, width = "1.15 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average carbon footprint [tCO$_{2}$]" = 6, "Average incidence from USD 40/tCO$_{2}$ carbon price" = 6), escape = FALSE)%>%
  footnote(general = "This table shows average carbon footprints in tCO$_{2}$ and average levels of carbon price incidence for households in all countries of our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T, escape = FALSE)%>%
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
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))

colnames(sum_3.4.1) <- c("Country", rep(c("EQ1","EQG2","EQ3","EQ4","EQ5"),3))

kbl(sum_3.4.1, format = "latex", caption = "Share of households using cooking fuels", booktabs = T, align = "l|rrrrr|rrrrr|rrrrr", vline = "", linesep = "")%>%
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
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))

colnames(sum_3.4.2) <- c("Country", rep(c("EQ1","EQ2","EQ3","EQ4","EQ5"),3))

kbl(sum_3.4.2, format = "latex", caption = "Share of households using lighting fuels", booktabs = T, align = "l|rrrrr|rrrrr|rrrrr", vline = "", linesep = "")%>%
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
  mutate_at(vars(-Country), list(~ ifelse(. == "NA%","-",.)))

colnames(sum_3.5.2) <- c("Country", rep(c("All","EQ1","EQ5"),5))

kbl(sum_3.5.2, format = "latex", caption = "Share of households possessing different assets", booktabs = T, align = "l|rrr|rrr|rrr|rrr|rrr", vline = "", linesep = "")%>%
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

# 5.      Econometric analysis ####

# 5.1     OLS ####

# 5.2     Inequality decomposition ####

# 5.3     Logit-Model ####