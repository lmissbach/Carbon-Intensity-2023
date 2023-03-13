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


# 3       Output GIZ ####

data_7.1 <- data_2 %>%
  filter(Country == "MEX")

data_7.2 <- data_2 %>%
  filter(Country == "UGA")

data_7.1.0 <- data_7.1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   CO2_t_national, weights = hh_weights))%>%
  ungroup()

data_7.1.1 <- data_7.1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

data_7.1.2 <- data_7.1 %>%
  group_by(urban_01)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

P_7.1.0 <- ggplot(data_7.1.0, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon footprint (in tCO2)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,20))+
  ggtitle("Carbon footprint - Mexico")+
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

P_7.1.1 <- ggplot(data_7.1.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.065))+
  ggtitle("Carbon pricing incidence - Mexico")+
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

P_7.1.2 <- ggplot(data_7.1.2, aes(x = factor(urban_01)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(urban_01)), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.065))+
  guides(fill = "none")+
  scale_fill_nejm()+
  ggtitle("Carbon pricing incidence - Mexico")+
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

data_7.1.3 <- data_7.1 %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price")

data_7.1.4 <- data_7.1 %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_global, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_global, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_global, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_global, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_global, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_global, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price including carbon border adjustment")

data_7.1.5 <- data_7.1 %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_transport, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_transport, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price (transport sector)")

data_7.1.6 <- data_7.1 %>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_electricity, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_electricity, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price (electricity sector)")

data_7.1.36 <- bind_rows(data_7.1.3, data_7.1.4, data_7.1.5, data_7.1.6)

data_7.1.7 <- data_7.1 %>%
  mutate(exp_carbon_tax_0 = burden_CO2_national*hh_expenditures_USD_2014)%>%
  mutate(exp_carbon_tax   = exp_carbon_tax_0*hh_weights,
         population       = hh_size*hh_weights)%>%
  mutate(sum_exp_carbon_tax = sum(exp_carbon_tax),
         population         = sum(population))%>%
  mutate(per_capita_dividend = sum_exp_carbon_tax/population)%>%
  mutate(net_burden = (exp_carbon_tax_0 - per_capita_dividend*hh_size)/hh_expenditures_USD_2014)%>%
  group_by(Income_Group_10)%>%
  summarise(
    y5  = wtd.quantile(net_burden, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(net_burden, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   net_burden, weights = hh_weights))%>%
  ungroup()%>%
  mutate(Type = "Carbon price + equal per capita transfer")

data_7.1.37 <- bind_rows(data_7.1.3, data_7.1.7)%>%
  mutate_at(vars(-Income_Group_10, - Type), list(~ .*-1))

P_7.1.36 <- ggplot(data_7.1.36, aes(x = factor(Income_Group_10), group = interaction(Income_Group_10, Type)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  facet_wrap(. ~ Type)+
  xlab("Expenditure deciles")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white", position = position_dodge(0.5))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9","10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.07))+
  guides(fill = "none")+
  scale_fill_npg()+
  ggtitle("Carbon pricing incidence - Mexico")+
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

P_7.1.4 <- ggplot(data_7.1.3, aes(x = factor(Income_Group_10)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", 
               position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, fill = "#4DBBD5FF") +
  theme_bw()+
  xlab("Expenditure deciles")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.7, stroke = 0.5, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9", "10\n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(0,0.065))+
  ggtitle("Carbon pricing incidence - Mexico")+
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

P_7.1.37 <- ggplot(data_7.1.37, aes(x = factor(Income_Group_10), group = interaction(Income_Group_10, Type)))+
  geom_hline(aes(yintercept = 0))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), stat = "identity", position = position_dodge(0.6), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure deciles")+ ylab("Net budget change")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.5, fill = "white", position = position_dodge(0.6))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(seq(-0.05,0.22,.025)))+
  scale_x_discrete(labels = c("1 \n Poorest \n 10 Percent", "2", "3", "4", "5", "6", "7", "8", "9","10 \n Richest \n 10 Percent"))+
  coord_cartesian(ylim = c(-0.07,0.22))+
  scale_fill_npg()+
  guides(fill = guide_legend(nrow = 1, title = ""))+
  ggtitle("Carbon pricing incidence - Mexico")+
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

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Figure_MEX_%d.jpg", width = 8, height = 8, unit = "cm", res = 400)
print(P_7.1.0)
print(P_7.1.1)
print(P_7.1.2)
dev.off()

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Figure_MEX_add_%d.jpg", width = 16, height = 16, unit = "cm", res = 400)
print(P_7.1.4)
print(P_7.1.36)
print(P_7.1.37)
dev.off()

data_7.2.0 <- data_7.2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(CO2_t_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   CO2_t_national, weights = hh_weights))%>%
  ungroup()

data_7.2.1 <- data_7.2 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

data_7.2.2 <- data_7.2 %>%
  group_by(urban_01)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

P_7.2.0 <- ggplot(data_7.2.0, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon footprint (in tCO2)")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,5))+
  ggtitle("Carbon footprint - Uganda")+
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

P_7.2.1 <- ggplot(data_7.2.1, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.04))+
  ggtitle("Carbon pricing incidence - Uganda")+
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

P_7.2.2 <- ggplot(data_7.2.2, aes(x = factor(urban_01)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(urban_01)), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Expenditure quintile")+ ylab("Carbon pricing incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  scale_x_discrete(labels = c("Rural", "Urban"))+
  coord_cartesian(ylim = c(0,0.04))+
  guides(fill = "none")+
  scale_fill_nejm()+
  ggtitle("Carbon pricing incidence - Uganda")+
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

jpeg("C:/Users/misl/OwnCloud/Distributional_Map/1_Slides/Graphics/Figure_UGA_%d.jpg", width = 8, height = 8, unit = "cm", res = 400)
print(P_7.2.0)
print(P_7.2.1)
print(P_7.2.2)
dev.off()

