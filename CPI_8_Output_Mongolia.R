# 0     General ####
# Author: L. Missbach (missbach@mcc-berlin.net)

# 0.1     Packages ####

if(!require("pacman")) install.packages("pacman")

p_load("boot", "broom", "countrycode","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "marginaleffects", "margins",
       "openxlsx", "rattle", "scales", "tidyverse", "xtable")

options(scipen=999,
        dplyr.summarise.inform = FALSE)

# 1       Loading data ####

# data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")

data_0.1 <- read_csv("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/2017_11B/Carbon_Pricing_Incidence_Mongolia_2023.csv")
data_0.2 <- read_csv("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/2017_11B/household_information_Mongolia_new_2023.csv")
data_0.3 <- read_csv("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/2017_11B/Sectoral_Burden_Mongolia_2023.csv")

data_0 <- left_join(data_0.1, data_0.2)%>%
  left_join(data_0.3)

# Codes

Water.Codes    <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Water.Code.csv",    show_col_types = FALSE)
Toilet.Codes   <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Toilet.Code.csv",   show_col_types = FALSE)
Province.Codes <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Province.Code.csv", show_col_types = FALSE)
District.Codes <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/District.Code.csv", show_col_types = FALSE)
Industry.Codes <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Industry.Code.csv", show_col_types = FALSE)
Housing.Codes  <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Housing.Code.csv",  show_col_types = FALSE)
Gender.Codes   <- read_csv("../0_Data/1_Household Data/1_Mongolia/2_Codes/Gender.Code.csv",   show_col_types = FALSE)

# 1.1     Combining data ####

data_1 <- data_0 %>%
  left_join(Province.Codes,    by = c("province"))%>%
  left_join(District.Codes,    by = c("district"))%>%
  left_join(Toilet.Codes,      by = "toilet")%>%
  left_join(Water.Codes,       by = "water")%>%
  left_join(Industry.Codes,    by = c("ind_hhh"))%>%
  left_join(Housing.Codes,     by = c("dwelling"))%>%
  left_join(Gender.Codes,      by = c("sex_hhh"))%>%
  select(-province,-district,-water,-toilet,-ind_hhh,-dwelling,-sex_hhh)
  
rm(Province.Codes, District.Codes, Toilet.Codes, Water.Codes, Industry.Codes, Housing.Codes, Gender.Codes, data_0, data_0.1, data_0.2, data_0.3)

# 2.      Edit data ####

carbon.price    <- 2
carbon.price.35 <- 35
carbon.price.ad <- carbon.price/40

data_2 <- data_1 %>%
  mutate(# exp_CO2_global                  = CO2_t_global*carbon.price,
         exp_CO2_national                = CO2_t_national*carbon.price,
         exp_CO2_electricity             = CO2_t_electricity*carbon.price,
         # exp_CO2_transport               = CO2_t_transport*carbon.price,
         exp_CO2_without_coal            = CO2_t_without_coal*carbon.price,
         # Carbon price 35
         # exp_CO2_global_35               = CO2_t_global*carbon.price.35,
         exp_CO2_national_35             = CO2_t_national*carbon.price.35,
         exp_CO2_electricity_35          = CO2_t_electricity*carbon.price.35,
         # exp_CO2_transport_35            = CO2_t_transport*carbon.price.35,
         exp_CO2_without_coal_35         = CO2_t_without_coal*carbon.price.35)%>%
  mutate(hh_expenditures_USD_2014_pc     = hh_expenditures_USD_2014/hh_size,
         log_hh_expenditures_USD_2014    = log(hh_expenditures_USD_2014),
         log_hh_expenditures_USD_2014_pc = log(hh_expenditures_USD_2014_pc))%>%
  mutate(exp_CO2_without_electricity     = exp_CO2_national-exp_CO2_electricity,
         exp_CO2_without_electricity_35  = exp_CO2_national_35-exp_CO2_electricity_35)%>%
  mutate(# burden_CO2_global               = exp_CO2_global/     hh_expenditures_USD_2014,
         burden_CO2_national             = exp_CO2_national/   hh_expenditures_USD_2014,
         # burden_CO2_electricity          = exp_CO2_electricity/hh_expenditures_USD_2014,
         # burden_CO2_transport            = exp_CO2_transport/  hh_expenditures_USD_2014,
         burden_CO2_national_without_coal = exp_CO2_without_coal/hh_expenditures_USD_2014,
         burden_CO2_national_without_electricity = exp_CO2_without_electricity/hh_expenditures_USD_2014,
         # burden_CO2_global_35            = exp_CO2_global_35/     hh_expenditures_USD_2014,
         burden_CO2_national_35          = exp_CO2_national_35/   hh_expenditures_USD_2014,
         # burden_CO2_electricity_35       = exp_CO2_electricity_35/hh_expenditures_USD_2014,
         # burden_CO2_transport_35         = exp_CO2_transport_35/  hh_expenditures_USD_2014,
         burden_CO2_national_without_coal_35        = exp_CO2_without_coal_35/hh_expenditures_USD_2014,
         burden_CO2_national_without_electricity_35 = exp_CO2_without_electricity_35/hh_expenditures_USD_2014)%>%
  mutate(burden_s_cooking_fuels          = exp_s_cooking_fuels*carbon.price.ad  /hh_expenditures_USD_2014,
         burden_s_transport_fuels        = exp_s_transport_fuels*carbon.price.ad/hh_expenditures_USD_2014,
         burden_s_Goods                  = exp_s_Goods*carbon.price.ad          /hh_expenditures_USD_2014,
         burden_s_Services               = exp_s_Services*carbon.price.ad       /hh_expenditures_USD_2014,
         burden_s_Food                   = exp_s_Food*carbon.price.ad           /hh_expenditures_USD_2014,
         burden_s_Electricity            = exp_s_Electricity*carbon.price.ad    /hh_expenditures_USD_2014,
         burden_s_other_energy           = exp_s_other_energy*carbon.price.ad   /hh_expenditures_USD_2014)%>%
  select(-starts_with("exp_s_"))%>%
  mutate(carbon_intensity_kg_per_USD_national = CO2_t_national*1000/hh_expenditures_USD_2014)

rm(data_1)

# 3       Output GIZ ####

# Carbon footprint
data_3.1.0 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   CO2_t_national, weights = hh_weights))%>%
  ungroup()

# Relative burden national carbon price
data_3.1.1 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

# Absolute burden national carbon price
data_3.1.2 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(exp_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(exp_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(exp_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(exp_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(exp_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   exp_CO2_national, weights = hh_weights))%>%
  ungroup()

# Relative burden national carbon price of 35
data_3.1.3 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_35, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_35, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_35, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_35, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_35, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_35, weights = hh_weights))%>%
  ungroup()

# Absolute burden national carbon price of 35
data_3.1.4 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(exp_CO2_national_35, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(exp_CO2_national_35, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(exp_CO2_national_35, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(exp_CO2_national_35, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(exp_CO2_national_35, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   exp_CO2_national_35, weights = hh_weights))%>%
  ungroup()

# Absolute burden national carbon price without electricity sector
data_3.1.5 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(exp_CO2_without_electricity, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(exp_CO2_without_electricity, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(exp_CO2_without_electricity, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(exp_CO2_without_electricity, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(exp_CO2_without_electricity, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   exp_CO2_without_electricity, weights = hh_weights))%>%
  ungroup()

# Absolute burden national carbon price without coal
data_3.1.6 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(exp_CO2_without_coal, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(exp_CO2_without_coal, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(exp_CO2_without_coal, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(exp_CO2_without_coal, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(exp_CO2_without_coal, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   exp_CO2_without_coal, weights = hh_weights))%>%
  ungroup()

# Relative burden national carbon price without electricity sector
data_3.1.7 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_without_electricity, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_without_electricity, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_without_electricity, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_without_electricity, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_without_electricity, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_without_electricity, weights = hh_weights))%>%
  ungroup()

# Relative burden national carbon price without coal
data_3.1.8 <- data_2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national_without_coal, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national_without_coal, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national_without_coal, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national_without_coal, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national_without_coal, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national_without_coal, weights = hh_weights))%>%
  ungroup()

# Figures ####

# Step 1 ####

P_3.1.0 <- ggplot(data_3.1.0, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon footprint (in tCO2)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,20))+
  ggtitle("Carbon footprint - Mongolia")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.1 <- ggplot(data_3.1.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in % of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.01))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.3 <- ggplot(data_3.1.3, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in % of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1501))+
  ggtitle(expression(paste("Carbon price of US-$ 35 per ", tCO[2], sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.7 <- ggplot(data_3.1.7, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in % of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.01))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - without electricity sector", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.8 <- ggplot(data_3.1.8, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in % of total expenditures)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.01))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - without heating sector", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_relative_%d.jpg", width = 8, height = 8, unit = "cm", res = 400)
print(P_3.1.1)
print(P_3.1.3)
print(P_3.1.7)
print(P_3.1.8)
dev.off()

P_3.1.2 <- ggplot(data_3.1.2, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in USD)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,30))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.4 <- ggplot(data_3.1.4, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in USD)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,500))+
  ggtitle(expression(paste("Carbon price of US-$ 35 per ", tCO[2], sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.5 <- ggplot(data_3.1.5, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in USD)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,30))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - without electricity sector", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.1.6 <- ggplot(data_3.1.6, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3,
               fill = "lightgrey") +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon price incidence (in USD)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,30))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - without heating sector", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_absolute_%d.jpg", width = 8, height = 8, unit = "cm", res = 400)
print(P_3.1.2)
print(P_3.1.4)
print(P_3.1.5)
print(P_3.1.6)
dev.off()

# Step 2 ####

# Heterogenität entlang ausgewählter Dimensionen

data_3.2.1 <- data_2 %>%
  group_by(urban_01)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Urban = ifelse(urban_01 == 0, "Rural", "Urban"))

data_3.2.2 <- data_2 %>%
  group_by(Province)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

data_3.2.3 <- data_2 %>%
  group_by(District)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

data_3.2.4 <- data_2 %>%
  group_by(Dwelling)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Dwelling = tolower(Dwelling))%>%
  mutate(Dwelling = str_to_title(Dwelling))%>%
  mutate(Dwelling = ifelse(Dwelling == "Convenient Single-Family House", "Single-family house",
                           ifelse(Dwelling == "Public Accomodation, Dormitory", "Public accomodation", 
                                  ifelse(Dwelling == "Non-Living Quarters", "Non-living quarters",
                                         ifelse(Dwelling == "Appartment", "Apartment",
                                                ifelse(Dwelling == "Separate Apartment", "Separate apartment", Dwelling))))))%>%
  filter(Dwelling != "Other")

P_3.2.1 <- ggplot(data_3.2.1, aes(x = Urban))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Urban), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  #scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.01))+
  guides(fill = "none")+
  scale_fill_nejm()+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - Urban/Rural", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.2.2 <- ggplot(data_3.2.2, aes(x = reorder(Province, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Province), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  #scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.01))+
  guides(fill = "none")+
  scale_fill_nejm()+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - Region", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.2.3 <- ggplot(data_3.2.3, aes(x = reorder(District, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = reorder(District, desc(y50))), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  #scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.012))+
  guides(fill = "none")+
  #scale_fill_nejm()+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - Aimags", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.2.4 <- ggplot(data_3.2.4, aes(x = reorder(Dwelling, desc(y50))))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Dwelling), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  #scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.01))+
  guides(fill = "none")+
  scale_fill_nejm()+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - Dwelling", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

P_3.2.5 <- ggplot(data_2, aes(x = area, y = burden_CO2_national))+
  geom_smooth(method = "loess", colour = "black", fill = "lightgrey", span = 0.3, linewidth = 0.5)+
  geom_point(shape = 21, fill = "#BC3C29FF", alpha = 0.5, size = 0.6)+
  theme_bw()+
  xlab("")+ ylab("Carbon pricing incidence")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.02), xlim = c(0,150))+
  guides(fill = "none")+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " - Area of living", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1, vjust = 0.25),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_horizontal_%d.jpg", width = 9, height = 8, unit = "cm", res = 400)
print(P_3.2.1)
print(P_3.2.2)
dev.off()

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_horizontal_3.jpg", width = 12, height = 8, unit = "cm", res = 400)
print(P_3.2.3)
dev.off()

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_horizontal_4.jpg", width = 12, height = 8, unit = "cm", res = 400)
print(P_3.2.4)
dev.off()

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_horizontal_5.jpg", width = 12, height = 8, unit = "cm", res = 400)
print(P_3.2.5)
dev.off()

# Step 3 ####

# Kompensation

data_3.3.1 <- data_2 %>%
  mutate(exp_carbon_tax_0 = burden_CO2_national*hh_expenditures_USD_2014)%>%
  mutate(exp_carbon_tax   = exp_carbon_tax_0*hh_weights,
         population       = hh_size*hh_weights)%>%
  mutate(sum_exp_carbon_tax = sum(exp_carbon_tax),
         population         = sum(population))%>%
  # Redistribute 50% of revenues back to population
  mutate(per_capita_dividend = sum_exp_carbon_tax*0.5/population)%>%
  mutate(net_burden = (exp_carbon_tax_0 - per_capita_dividend*hh_size)/hh_expenditures_USD_2014)%>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(net_burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   net_burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price + equal per capita transfer")%>%
  bind_rows(mutate(data_3.1.1, Type = "Carbon price"))%>%
  mutate_at(vars(-Income_Group_5, - Type), list(~ .*-1))

P_3.3.1 <- ggplot(data_3.3.1, aes(x = factor(Income_Group_5), group = interaction(Income_Group_5, Type)))+
  geom_hline(aes(yintercept = 0))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), stat = "identity", position = position_dodge(0.6), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Net budget change")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white", position = position_dodge(0.6))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0), breaks = c(seq(-0.008,0.004,0.002)))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.009,0.004))+
  scale_fill_npg()+
  guides(fill = guide_legend(nrow = 1, title = ""))+
  ggtitle(expression(paste("Carbon price of US-$ 2 per ", tCO[2], " with revenue recycling", sep = "")))+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 7),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.border = element_rect(size = 0.3),
        panel.spacing = unit(1,"lines"))

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Mongolia/Figure_MNG_recycling_%d.jpg", width = 12, height = 8, unit = "cm", res = 400)
print(P_3.3.1)
dev.off()

