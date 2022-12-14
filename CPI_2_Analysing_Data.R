# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1   Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "countrycode","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "xtable")

options(scipen=999)

# 1     Loading data ####

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

# 1.1   Combining data ####

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

# 2.    Edit data ####

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
  select(-starts_with("exp_s_"))

# 3.    Summary statistics ####

Country.Set <- distinct(data_2, Country)%>%
  mutate(Country_long = countrycode(Country, origin = "iso3c", destination = "country.name"))

# 3.1   General summary statistics ####

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
         electricity.access       = paste0(round(electricity.access*100,1),"%"),
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

kbl(mutate_all(sum_3.1.3,linebreak), format = "latex", linesep = "", booktabs = T,
    caption = "Summary statistics", format.args = list(big.mark = ",", scientific = FALSE), align = "lrcccrcc")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  footnote(general = "This table provides summary statistics for households in our sample. All values (except observations) are household-weighted averages.", threeparttable = T)%>%
  column_spec(column = 1,    width = "3.5 cm", border_right = T)%>%
  column_spec(column =  2:8, width = "2.3 cm")%>%
  save_kable(., "2_Tables/Table_Summary_A1.tex")

rm(sum_3.1.1, sum_3.1.2, sum_3.1.3)

# 3.2   Average expenditure and energy expenditure share income groups ####

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

kbl(sum_3.2.3, format = "latex", caption = "Average expenditures and average energy expenditure shares per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", format.args = list(big.mark = ",", scientific = FALSE), linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.13 cm")%>%
  column_spec(8:13, width = "1.04 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average household expenditures [USD]" = 6, "Average energy expenditure shares" = 6))%>%
  footnote(general = "This table shows average household expenditures and average energy expenditure shares for households in our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T)%>%
  save_kable(., "2_Tables/Table_Summary_A2.tex")

rm(sum_3.2.1, sum_3.2.2, sum_3.2.3)

# 3.3   Carbon footprint and burden national ####

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

colnames(sum_3.3.3) <- c("Country", rep(c("All","EQ1","EQG2","EQ3","EQ4","EQ5"),2))

kbl(sum_3.3.3, format = "latex", caption = "Average carbon footprint and average USD/tCO$_{2}$ carbon price incidence per expenditure quintile", booktabs = T, align = "l|rrrrrr|rrrrrr", vline = "", linesep = "")%>%
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down"))%>%
  column_spec(1, width = "3.15 cm")%>%
  column_spec(2:7, width = "1.05 cm")%>%
  column_spec(8:13, width = "1.15 cm")%>%
  add_header_above(c(" " = 2, "Expenditure quintile" = 5, " " = 1, "Expenditure quintile" = 5))%>%
  add_header_above(c(" " = 1, "Average carbon footprint [tCO$_{2}$]" = 6, "Average incidence from USD 40/tCO$_{2}$ carbon price" = 6), escape = FALSE)%>%
  footnote(general = "This table shows average carbon footprints in tCO$_{2}$ and average levels of carbon price incidence for households in all countries of our sample. We estimate household-weighted averages for the whole population and per expenditure quintile.", threeparttable = T, escape = FALSE)%>%
  save_kable(., "2_Tables/Table_Summary_A3.tex")

rm(sum_3.3.1, sum_3.3.2, sum_3.3.3)

# 3.4   Electricity-table (TBA) ####

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

# 4.    Descriptive analysis ####

# 4.1   Correlational analyses ####

# 5.    Econometric analysis ####

# 5.1   OLS ####

# 5.2   Inequality decomposition ####

# 5.3   Logit-Model ####