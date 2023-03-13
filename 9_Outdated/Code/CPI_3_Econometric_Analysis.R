# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("broom")
library("cowplot")
library("fixest")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

Summary_DF <- data.frame(Country.Name = c(""), Difference_Vertical = c(0), Difference_Horizontal = c(0))

Countries_List_A <- c("Argentina", "Bolivia", "Ecuador", "South_Africa", "Nigeria", "Ethiopia", "India", "Indonesia", "Vietnam")

# 1   Loading Data ####
for(Country.Name in Countries_List_A){
#Country.Name <- "Ecuador"

path_0 <-list.files("../0_Data/1_Household Data/")[grep(Country.Name, list.files("../0_Data/1_Household Data/"), ignore.case = T)]

carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", Country.Name),
                                       col_types = cols(exp_LCU_Kerosene = col_double(),
                                                        exp_LCU_Gas      = col_double(),
                                                        exp_LCU_Charcoal = col_double()))

if(!"exp_LCU_Biomass"  %in% colnames(carbon_pricing_incidence_0)) carbon_pricing_incidence_0$exp_LCU_Biomass  <- 0
if(!"exp_LCU_Charcoal" %in% colnames(carbon_pricing_incidence_0)) carbon_pricing_incidence_0$exp_LCU_Charcoal <- 0
if(!"exp_LCU_Firewood" %in% colnames(carbon_pricing_incidence_0)) carbon_pricing_incidence_0$exp_LCU_Firewood <- 0

  
household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", Country.Name))
  
appliances_0               <- read_csv(sprintf("../0_Data/1_Household Data/%s/1_Data_Clean/appliances_0_1_new_%s.csv", path_0, Country.Name))

# 1.1  Unified Dataframe ####

data_0 <- left_join(carbon_pricing_incidence_0, household_information_0)%>%
  left_join(appliances_0)%>%
  mutate(log_total_expenditures = log(hh_expenditures_USD_2014))

# 1.2 Several Summary Statistics ####

if(Country.Name == "South_Africa") Country.Name <- "South Africa"

# 1.2.1 Vertical Difference between Quintiles - National Carbon Tax

data_121 <- data_0 %>%
  group_by(Income_Group_5)%>%
  summarise(q20 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.2),
            q50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
            q80 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.8),
            qav = wtd.mean(    burden_CO2_national, weights = hh_weights))%>%
  ungroup()

# Difference between first and fifth quintile - average
dif_a <- unname(data_121$qav[data_121$Income_Group_5 == 1] - data_121$qav[data_121$Income_Group_5 == 5])
dif_b <- unname(data_121$q80[data_121$Income_Group_5 == 1] - data_121$q20[data_121$Income_Group_5 == 1])


Summary_AP <- data.frame(Country.Name = Country.Name, Difference_Vertical = dif_a, Difference_Horizontal = dif_b)

Summary_DF <- bind_rows(Summary_DF, Summary_AP)


# Better solution than the following - extract numbers directly from WDI
# # 1.2.2 Share of Households using Biomass
# 
# cooking.code                 <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Cooking.Code.csv", path_0))
# 
# data_122 <- data_0 %>%
#   select(hh_id, hh_weights, ends_with("_Firewood"), ends_with("_Charcoal"), ends_with("_Biomass"), ends_with("cooking_fuel"))%>%
#   left_join(cooking.code)%>%
#   mutate_at(.vars = vars(starts_with("exp_LCU")), .funs = list(~ ifelse(is.na(.), 0, .)))%>%
#   mutate(uses_biomass  = ifelse(exp_LCU_Firewood > 0 | exp_LCU_Charcoal > 0 | exp_LCU_Biomass > 0, 1, 0),
#          cooks_biomass = ifelse(CF == "Firewood",1,0),
#          biomass = ifelse(uses_biomass == 1 | cooks_biomass == 1, 1, 0))
# 
# data_1221 <- data_122 %>%
#   group_by(biomass)%>%
#   summarise(weights = sum(hh_weights))%>%
#   ungroup()%>%
#   mutate(share = weights/sum(weights))
# 
# biomass_share <- data_1221$share[data_1221$biomass == 1]



rm(Summary_AP, data_121, dif_a, dif_b, data_0)

}

Summary_DF <- Summary_DF %>%
  arrange(Country.Name)

write.xlsx(Summary_DF, "../1_Carbon_Pricing_Incidence/2_Figures/Figures_joint/Cesifo_Paper_2021/Vertical_horizontal_Disparity_Cesifo.xlsx")

# 2.00 Test-Analysis for Adrien ####

education.code               <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Education.Code.csv", path_0))%>%
  rename(Education = Label)
urban.code                   <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Urban.Code.csv", path_0))
ethnicity.code               <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/Ethnicity.Code.csv", path_0))%>%
  rename(Ethnicity = Label)
district.code                <- read_csv(sprintf("../0_Data/1_Household Data/%s/2_Codes/District.Code.csv", path_0)) 

data_0 <- left_join(data_0, education.code, by = c("edu_hhh" = "education"))%>%
  mutate(Urban = ifelse(urban_01 == 1, "Urban", "Rural"))%>%
  left_join(ethnicity.code)%>%
  #left_join(district.code)%>%
  mutate(mean_burden   = wtd.mean(burden_CO2_national, weights = hh_weights),
         dev_burden    = burden_CO2_national - mean_burden)%>%
  mutate(more_than_5   = ifelse(burden_CO2_national > 0.05,1,0),
         define_top_30 = wtd.quantile(burden_CO2_national, probs = 0.7, weights = hh_weights),
         top_30        = ifelse(burden_CO2_national > define_top_30,1,0))


# 2.01 Test-Analysis for Adrien ####

model_1 <- lm(burden_CO2_national ~ factor(Income_Group_5), data = data_0, weights = hh_weights)

model_1 <- feols(c(burden_CO2_national, dev_burden) ~ factor(Income_Group_5),                          data = data_0, weights = data_0$hh_weights)
# model_2 <- feols(burden_CO2_national ~ i(Income_Group_5) + log_total_expenditures, data = data_0, weights = data_0$hh_weights)
model_3 <- feols(burden_CO2_national ~ log_total_expenditures,                     data = data_0, weights = data_0$hh_weights)
model_4 <- feols(burden_CO2_national ~ log_total_expenditures + Urban + Education +
                   hh_size + Ethnicity + car.01, data = data_0, weights = data_0$hh_weights)
model_5 <- feols(burden_CO2_national ~ factor(Income_Group_5) + Urban + Education +
                   hh_size + Ethnicity + car.01 + i(province, ref = "Guayas"), data = data_0, weights = data_0$hh_weights)

t_1 <- tidy(model_1)
t_3 <- tidy(model_3)
t_4 <- tidy(model_4)
t_5 <- tidy(model_5)
t_6 <- tidy(model_6)

t_all <- list("Income Group only" = t_1, "Log Expenditures only" = t_3, "Income Group + Covariates" = t_4, "Log Expenditures + Covariates" = t_5)

write.xlsx(t_all, sprintf("../1_Carbon_Pricing_Incidence/3_Analyses/9_Adrien_Regressions/Multiple_Regression_%s.xlsx", Country.Name))

model_6 <- glm(top_30 ~ log_total_expenditures + Urban + Education +  hh_size + car.01, data = data_0, family = quasibinomial(logit))
summary(model_6)

model_7 <- feols(share_energy ~ log_total_expenditures + Urban + Education + hh_size +  car.01, data = data_0, weights = data_0$hh_weights)
summary(model_7)

# 2.1 Analysis of Distributional Impacts ####
# 2.2 Correlations of Incidence with Factors ####
# 2.3 Explaining correlating Factors ####
# 2.4 Modelling different compensation policies ####

# 3   Cross-Sectional Analysis
# 3.1 Regressions across regions ####
# 3.2 Maps ####