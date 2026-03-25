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

GTAP_version <- "11B"

# 1       Loading data ####

if(GTAP_year == 2014){
  data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database.rds")
}

if(GTAP_year == 2017 & GTAP_version == "11A"){
  data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database_2017.rds")
}

if(GTAP_year == 2017 & GTAP_version == "11B"){
  data_0 <- read_rds("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database_2017_11B.rds")
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

# _______ PHD-Seminar ####

# 4       Figures Presentation ####
# 4.1     Figure 1: Scatterplot and friends ####

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

# 4.2     Figure 2: Boxplot and friends ####

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

# 4.2.1   Figure 2.1: Horizontal exceed vertical differences ####

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

# 4.3     Figure 3: Vertical over horizontal effects ####

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

# 4.4     Figure 4: Logit-Model approach ####

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

# 4.5     Figure 5: Average Marginal effects ####

data_7.5.0 <- read.xlsx("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_Logit.xlsx")%>%
  mutate(term = ifelse((contrast == "6-8 - 2-5" | contrast == "6-8 - 0-1") & !is.na(contrast), "higher_education",
                       ifelse(contrast == "2-5 - 0-1" & !is.na(contrast), "secondary_education", term)))

# 4.5.1   Figure 5.1: Average marginal effects for South Africa ####

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

# 4.5.2   Figure 5.2: Average marginal effects for selected countries ####

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

# 4.6     Figure 6: Variable Importance Plot and Partial Dependence Plots ####

# 4.6.1   Figure 6.1: VIP for South Africa ####

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


# 4.6.2   Figure 6.2: PDP for South Africa ####

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

# 4.6.3   Figure 6.3: Shapley-Values for South Africa ####

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

# 4.7     Figure 7: Overview of countries ####

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

# 4.8     Figure 8: Classification of countries ####

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

# 4.9     Figure 9: Venn-Diagram ####

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

# 4.10    Figure 10: World-Map ####

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

# _______ Envecon 2024 ####

# 5       Figures Presentation ####
# 5.1     Figure 1: Scatterplot and friends ####

data_5.1 <- data_2 %>%
  filter(Country == "GBR")

data_5.1.1 <- data_5.1 %>%
  summarise(y5  = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(carbon_intensity_kg_per_USD_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(carbon_intensity_kg_per_USD_national, weights = hh_weights))

data_5.1.2 <- data_5.1 %>%
  group_by(Income_Group_5)%>%
  summarise(max_exp = max(hh_expenditures_USD_2014_pc))

P.5.1.0 <- ggplot()+
  geom_boxplot(data = data_5.1.1, aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, x = 1), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3)+
  geom_point(data = data_5.1.1, aes(y = mean, x = 1), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  theme_bw()+
  xlab("") + ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(ylim = c(0,2.2))+
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

P.5.1.1 <- ggplot()+
  geom_point(data = data_5.1, aes(x = hh_expenditures_USD_2014_pc,
                                  y = burden_CO2_national), 
             alpha = 0.2, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + 
  ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 45000), ylim = c(0,0.06))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("United Kingdom")+
  theme(axis.text = element_text(size = 6),
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

P.5.1.2 <- P.5.1.1 +
  geom_smooth(data = data_5.1, 
              aes(x = hh_expenditures_USD_2014_pc, weight = hh_weights, 
                  y = burden_CO2_national, group = Country),
              level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#E64B35FF", 
              fill  = "#E64B35FF", size = 0.5)

P.5.1.3 <- ggplot()+
  geom_vline(aes(xintercept = 543.4191),  size = 0.2)+
  geom_vline(aes(xintercept = 981.5637),  size = 0.2)+
  geom_vline(aes(xintercept = 1753.5873), size = 0.2)+
  geom_vline(aes(xintercept = 3911.4171), size = 0.2)+
  geom_point(data = data_5.1, aes(x = hh_expenditures_USD_2014_pc,
                                  y = carbon_intensity_kg_per_USD_national, fill = factor(Income_Group_5)), 
             alpha = 0.2, shape = 21, colour = "black",  size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in US-$ (2014)") + ylab(expression(paste("Carbon intensity of consumption [kg", CO[2], "/USD]", sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 45000), ylim = c(0,2.2))+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1),  expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("United Kingdom")+
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

P.5.1.4 <- P.5.1.1 +
  geom_vline(aes(xintercept = 543.4191),  size = 0.2)+
  geom_vline(aes(xintercept = 981.5637),  size = 0.2)+
  geom_vline(aes(xintercept = 1753.5873), size = 0.2)+
  geom_vline(aes(xintercept = 3911.4171), size = 0.2)

P.5.1.5 <- align_plots(P.5.1.0, P.5.1.1, P.5.1.2, P.5.1.3, P.5.1.4, P.5.2.1, align = "hv")

P.5.1.6  <- ggdraw(P.5.1.5[[1]])
P.5.1.7  <- ggdraw(P.5.1.5[[2]])
P.5.1.8  <- ggdraw(P.5.1.5[[3]])
P.5.1.9  <- ggdraw(P.5.1.5[[4]])
P.5.1.10 <- ggdraw(P.5.1.5[[5]])
P.5.1.11 <- ggdraw(P.5.1.5[[6]])

jpeg("4_Presentations/2024_Envecon/Figures/Figure 1/Figure_1_0_%d.jpg", width = 4, height = 10, unit = "cm", res = 600)
print(P.5.1.6)
dev.off()

jpeg("4_Presentations/2024_Envecon/Figures/Figure 1/Figure_1_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.5.1.7)
print(P.5.1.8)
print(P.5.1.9)
print(P.5.1.10)
dev.off()

# 5.2     Figure 2: Boxplot and friends ####

data_5.2 <- data_5.1 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_5.2.1 <- data_5.2 %>%
  summarise(min_median = min(y50),
            max_median = max(y50))

P.5.2.1 <- ggplot(data_5.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = factor(Income_Group_5)), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 0.7) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab("")+
  geom_point(aes(y = mean), shape = 23, size = 2, stroke = 0.3, fill = "white")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,2.2))+
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

jpeg("4_Presentations/2024_Envecon/Figures/Figure 2/Figure_2_a_%d.jpg", width = 8.5, height = 10, unit = "cm", res = 600)
print(P.5.1.11)
dev.off()

P.5.2.2 <- ggplot(data_5.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("United Kingdom")+
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

P.5.2.3 <- ggplot(data_5.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("United Kingdom")+
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

P.5.2.4 <- ggplot(data_5.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_5.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("United Kingdom")+
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

P.5.2.5 <- ggplot(data_5.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_5.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("United Kingdom")+
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

jpeg("4_Presentations/2024_Envecon/Figures/Figure 2/Figure_2_b_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.5.2.2)
print(P.5.2.3)
print(P.5.2.4)
print(P.5.2.5)
dev.off()

rm(data_7.2, data_7.1, data_7.2.1, data_7.1.1, data_7.1.2, 
   P.7.1.0, P.7.1.1, P.7.1.2, P.7.1.3, P.7.1.4, P.7.1.5, P.7.1.6, P.7.1.7, P.7.1.8, P.7.1.9, P.7.1.10, P.7.1.11,
   P.7.2.1, P.7.2.2, P.7.2.3, P.7.2.4, P.7.2.5)


# 5.2.0  Figure 2 with Lump-Sum-Transfers ####

data_5.2.0 <- data_2 %>%
  filter(Country == "GBR")%>%
  mutate(pop = hh_size*hh_weights,
         sum_exp = exp_CO2_national*hh_weights)

sum_exp_all <- sum(data_5.2.0$sum_exp)*0.5
sum_exp_all_pc <- sum_exp_all/sum(data_5.2.0$pop)

data_5.2.0 <- data_5.2.0 %>%
  mutate(transfer = sum_exp_all_pc*hh_size)%>%
  mutate(exp_CO2_national_transfer = exp_CO2_national - transfer)%>%
  mutate(burden_CO2_national_transfer = exp_CO2_national_transfer/hh_expenditures_USD_2014)

data_5.2.0.1 <- data_5.2.0 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.95),
            mean    = wtd.mean(burden_CO2_national_transfer, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))%>%
  mutate(Type = "Carbon pricing and lump-sum transfer (50%)")%>%
  bind_rows(data_5.2)%>%
  mutate(Type = ifelse(!is.na(Type), Type, "Carbon pricing"))

P.5.2.X <- ggplot(data_5.2.0.1, aes(x = as.character(Income_Group_5), group = interaction(Income_Group_5, Type)))+
  geom_hline(aes(yintercept = 0))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), 
               stat = "identity", position = position_dodge(0.6), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 US-$/", tCO[2], " (net budget change)", sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white", position = position_dodge(0.6))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.08,0.1))+
  scale_fill_nejm()+
  labs(fill = " ")+
  ggtitle("United Kingdom")+
  #scale_fill_discrete(guide = "none")+
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

jpeg("4_Presentations/2024_Envecon/Figures/Figure 2/Figure_2_boxplot.jpg", width = 14, height = 12, unit = "cm", res = 600)
print(P.5.2.X)
dev.off()


# 5.2.1   Figure 2.1: Horizontal exceed vertical differences ####

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

jpeg("4_Presentations/Envecon_2024/Figures/Figure 2/Figure_2_c_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P_7.2.6)
dev.off()

rm(data_7.2.2, data_7.2.3, data_7.2.4, P_7.2.6)

# 5.3     Figure 3: Vertical over horizontal effects ####

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
  mutate(interest2 = ifelse(Country %in% c("RWA", "GBR", "ZAF", "USA", "CAN", "MWI", "LBR"),1,0.5))

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

# 5.4     Figure 4: Logit-Model approach ####

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

# 5.5     Figure 5: Average Marginal effects ####

data_7.5.0 <- read.xlsx("1_Figures/Analysis_Logit_Models_Marginal_Effects/Average_Marginal_Effects_Logit.xlsx")%>%
  mutate(term = ifelse((contrast == "6-8 - 2-5" | contrast == "6-8 - 0-1") & !is.na(contrast), "higher_education",
                       ifelse(contrast == "2-5 - 0-1" & !is.na(contrast), "secondary_education", term)))

# 5.5.1   Figure 5.1: Average marginal effects for South Africa ####

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

# 5.5.2   Figure 5.2: Average marginal effects for selected countries ####

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

# 5.6     Figure 6: Variable Importance Plot and Partial Dependence Plots ####

# 5.6.1   Figure 6.1: VIP for South Africa ####

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


# 5.6.2   Figure 6.2: PDP for South Africa ####

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

# 5.6.3   Figure 6.3: Shapley-Values for South Africa ####

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

# 5.7     Figure 7: Overview of countries ####

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

# 5.8     Figure 8: Classification of countries ####

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

# 5.9     Figure 9: Venn-Diagram ####

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

# 5.10    Figure 10: World-Map ####

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


# _______ C3A 2024 ####

data_0 <- read_rds("H:/6_Citizen_Survey/2_Data/Microdata/Microdata_Transformed_France.rds")

data_0.1 <- data_0 %>%
  mutate(carbon_intensity_kg_per_EURO = CO2_t_national/hh_expenditures_EURO_2018,
         hh_expenditures_EURO_2018_pc = hh_expenditures_EURO_2018/hh_size,
         burden_CO2_national = CO2_t_national*40/hh_expenditures_EURO_2018)%>%
  mutate(Income_Group_5 = as.numeric(binning(hh_expenditures_EURO_2018_pc, bins = 5, "wtd.quantile", weights = hh_weights)))

# 6.1     Figure 1: Scatterplot ####

P.5.1.1 <- ggplot()+
  geom_point(data = data_0.1, aes(x = hh_expenditures_EURO_2018_pc,
                                  y = burden_CO2_national), 
             alpha = 0.2, shape = 21, colour = "black", fill = "#4DBBD5FF", size = 0.5)+
  theme_bw()+
  xlab("Household expenditures per capita in € (2018)") + 
  ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], sep = "")))+
  labs(colour = "", fill = "")+
  coord_cartesian(xlim = c(0, 35000), ylim = c(0,0.06))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1), expand = c(0,0))+
  scale_fill_discrete(guide = "none")+
  ggtitle("France")+
  theme(axis.text = element_text(size = 6),
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

P.5.1.2 <- P.5.1.1 +
  geom_smooth(data = data_0.1, 
              aes(x = hh_expenditures_EURO_2018_pc, weight = hh_weights, 
                  y = burden_CO2_national),
              level = 0.95, method = "lm", formula = y ~ x + I(x^2), colour = "#E64B35FF", 
              fill  = "#E64B35FF", linewidth = 0.5)

jpeg("6_Versions/C3A/Figures/Figure_1_a_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.5.1.1)
print(P.5.1.2)
dev.off()

# 6.2     Figure 2: Boxplot ####

data_6.2 <- data_0.1 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
            mean = wtd.mean(burden_CO2_national, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))

data_6.2.1 <- data_6.2 %>%
  summarise(min_median = min(y50),
            max_median = max(y50))

P.6.2.2 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("France")+
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

P.6.2.3 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("France")+
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

P.6.2.4 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_6.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1, fill = "grey") +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("France")+
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

P.6.2.5 <- ggplot(data_6.2, aes(x = as.character(Income_Group_5)))+
  geom_rect(data = data_6.2.1, aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.5, fill = "#0072B5FF", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = interest, colour = interest, size = interest), 
               stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], sep = "")))+
  geom_point(aes(y = mean, colour = interest), shape = 23, size = 2, fill = "white")+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,0.1))+
  ggtitle("France")+
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

jpeg("6_Versions/C3A/Figures/Figure_2_b_%d.jpg", width = 10, height = 10, unit = "cm", res = 600)
print(P.6.2.2)
print(P.6.2.3)
print(P.6.2.4)
print(P.6.2.5)
dev.off()

# 6.3     Figure 3: Lump Sum Transfers ####

data_6.2.0 <- data_0.1 %>%
  mutate(exp_CO2_national = CO2_t_national*40)%>%
  mutate(pop     = hh_size*hh_weights,
         sum_exp = exp_CO2_national*hh_weights)

sum_exp_all <- sum(data_6.2.0$sum_exp)*0.5
sum_exp_all_pc <- sum_exp_all/sum(data_6.2.0$pop)

data_6.2.0 <- data_6.2.0 %>%
  mutate(transfer = sum_exp_all_pc*hh_size)%>%
  mutate(exp_CO2_national_transfer = exp_CO2_national - transfer)%>%
  mutate(burden_CO2_national_transfer = exp_CO2_national_transfer/hh_expenditures_EURO_2018)

data_6.2.0.1 <- data_6.2.0 %>%
  group_by(Income_Group_5)%>%
  summarise(y5  = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.05),
            y25 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.25),
            y50 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.5),
            y75 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.75),
            y95 = wtd.quantile(burden_CO2_national_transfer, weights = hh_weights, probs = 0.95),
            mean    = wtd.mean(burden_CO2_national_transfer, weights = hh_weights))%>%
  ungroup()%>%
  mutate(interest = ifelse(Income_Group_5 == 1 | Income_Group_5 == 5,"1", "0"))%>%
  mutate(Type = "Carbon pricing and lump-sum transfer (50%)")%>%
  bind_rows(data_6.2)%>%
  mutate(Type = ifelse(!is.na(Type), Type, "Carbon pricing"))

P.6.2.X <- ggplot(data_6.2.0.1, aes(x = as.character(Income_Group_5), group = interaction(Income_Group_5, Type)))+
  geom_hline(aes(yintercept = 0))+
  #geom_rect(aes(ymin = min_median, ymax = max_median), xmin = 0, xmax = 6, alpha = 0.2, fill = "lightblue", inherit.aes = FALSE)+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95, fill = Type), 
               stat = "identity", position = position_dodge(0.6), outlier.shape = NA, width = 0.5, size = 0.3, alpha = 1) +
  theme_bw()+
  xlab("Expenditure quintiles")+ ylab(expression(paste("Incidence for a carbon price of 40 €/", tCO[2], " (net budget change)", sep = "")))+
  geom_point(aes(y = mean), shape = 23, size = 2, fill = "white", position = position_dodge(0.6))+
  scale_y_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(-0.06,0.1))+
  scale_fill_nejm()+
  labs(fill = " ")+
  ggtitle("France")+
  #scale_fill_discrete(guide = "none")+
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

jpeg("6_Versions/C3A/Figures/Figure_2_boxplot.jpg", width = 14, height = 12, unit = "cm", res = 600)
print(P.6.2.X)
dev.off()

# 6.4     Figure 4: SHAP ####

data_6.4 <- data_0.1 %>%
  select(-hh_id, -hh_weights, -CO2_t_gas, -starts_with("Exp"), -Expenditure_Group_5, -Expenditure_Group_10)%>%
  # TBA
  mutate(nationality = ifelse(nationality %in% c("Nationalité de l'Union européenne des 15 (sauf France)", "Nationalité des pays entrés en 2004 dans l'Union européenne"), "Nationalité de l'Union européenne", nationality))%>%
  mutate(housing_type_2 = ifelse(housing_type_2 == "Ne sait pas", "Other", housing_type_2))%>%
  mutate(construction_year = ifelse(construction_year %in% c("Ne sait pas", "Other", "Refus"), "Ne sait pas", construction_year),
         wall  = ifelse(wall %in% c("Dur (Pierre, brique, parpaing)", "Tôle"), wall, "Other"),
         roof  = ifelse(roof %in% c("Béton (maison en cours d'agrandissement)", "Tôle"), roof, "Other"),
         floor = ifelse(floor %in% c("Carrelage", "Revêtement plastique (lino...)"), floor, "Other"),
         heating_fuel = ifelse(heating_fuel %in% c("Autre","Ne sait pas", "Other"), "Other", heating_fuel),
         urban_type = ifelse(urban_type %in% c("Commune appartenant à la couronne d'un grand pôle", "Commune appartenant à un grand pôle (10 000 emplois ou plus)"), "Grand pôle",
                             ifelse(urban_type %in% c("Commune appartenant à la couronne d'un moyen pôle", "Commune appartenant à un moyen pôle (5 000 à moins de 10 000 emplois)"), "Moyen pôle",
                                    ifelse(urban_type %in% c("Commune appartenant à la couronne d'un petit pôle", "Commune appartenant à un petit pôle (de 1 500 à moins de 5 000 emplois)"), "Petit pôle",
                                           ifelse(urban_type %in% c("Autre commune multipolarisée", "Commune multipolarisée des grandes aires urbaines", "Other"), "Commune multipolarisée", "Other")))))%>%
  mutate(area = ifelse(is.na(area),999,area))%>%
  mutate(tenant = ifelse(tenant %in% c("Locataire", "Sous-locataire, co-locataire"), "Locataire", 
                         ifelse(tenant %in% c("Logé gratuitement, avec paiement éventuel de charges", "Usufruitier, y compris en viager"), "Other", tenant)))%>%
  mutate(housing_type = ifelse(housing_type %in% c("Un logement dans un immeuble collectif", "Un logement dans un immeuble collectif à usage autre que d'habitation (usine, bureaux, commerce, bâtiment public...)"), "Logement dans un immeuble collectif",
                               ifelse(housing_type == "Une maison individuelle", "Une maison individuelle", "Other")))%>%
  mutate(construction_year = ifelse(construction_year %in% c("En 1948 ou avant"), construction_year,
                                    ifelse(construction_year %in% c("De 1949 à 1961", "De 1962 à 1967", "De 1968 à 1974"), "De 1949 à 1974",
                                           ifelse(construction_year %in% c("De 1975 à 1981", "De 1982 à 1989"), "De 1975 à 1989",
                                                  ifelse(construction_year %in% c("De 1990 à 1998", "De 1999 à 2003"), "De 1990 à 2003",
                                                         ifelse(construction_year %in% c("En 2004 et après", "En construction"), "En 2004 et après", construction_year))))))%>%
  # to watch: urban_type
  mutate(exp_interest   = CO2_t_national*40)%>% # in €
  mutate(burden_interest = exp_interest/hh_expenditures_EURO_2018)%>%
  select(-CO2_t_transport, -CO2_t_gas_direct, -exp_interest, -CO2_t_national)%>%
  mutate_if(vars(is.character(.)), list(~ as.factor(.)))%>%
  select(burden_interest, any_of(c("hh_expenditures_EURO_2018", "number_of_cars", "heating_fuel", "tenant", "urban_type", "housing_type", "construction_year", "province", "occupation")))


data_0.1 <- data_6.4 %>%
  # Create noise parameter
  mutate(noise = rnorm(nrow(.),0,1))

data_0.2 <- data_0.1 %>%
  initial_split(prop = 0.8)

# Data for training
data_0.2.train <- data_0.2 %>%
  training()

# Data for testing
data_0.2.test <- data_0.2 %>%
  testing()

rm(data_0.1, data_0.2)

# Feature engineering

  recipe_0 <- recipe(burden_interest ~ .,
                     data = data_0.2.train)%>%
    # Deletes all columns with any NA
    step_filter_missing(all_predictors(), threshold = 0)%>%
    # Remove minimum number of columns such that correlations are less than 0.9
    step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
    # should have very few unique observations for factors
    step_other(all_nominal(), -province, -urban_type, threshold = 0.05)%>%
    step_dummy(all_nominal())

data_0.2.training <- recipe_0 %>%
  prep(training = data_0.2.train)%>%
  bake(new_data = NULL)

data_0.2.testing <- recipe_0 %>%
  prep(training = data_0.2.test)%>%
  bake(new_data = NULL) 

# Five-fold cross-validation

folds_1 <- vfold_cv(data_0.2.training, v = 5)

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
  mtry(c(round((ncol(data_0.2.training)-1)/2,0), ncol(data_0.2.training)-1)),
  size = 15)%>%
  # default parameters
  bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_0.2.training)-1))

# Tune the model - cover the entire parameter space without running every combination

print("Start computing")

doParallel::registerDoParallel()

time_1 <- Sys.time()

model_brt_1 <- tune_grid(model_brt,
                         burden_interest ~ .,
                         resamples = folds_1,
                         grid      = grid_0,
                         metrics   = metric_set(mae, rmse, rsq))

time_2 <- Sys.time()

doParallel::stopImplicitCluster()

print("End computing")

# Collect metrics of tuned models

metrics_1 <- collect_metrics(model_brt_1)

model_brt_1.1 <- select_best(model_brt_1, metric = "mae")

metrics_1.1 <- metrics_1 %>%
  filter(.config == model_brt_1.1$.config[1])

# Fit best model after tuning
model_brt <- boost_tree(
  trees         = 1000,
  tree_depth    = metrics_1.1$tree_depth[1], # 3
  learn_rate    = metrics_1.1$learn_rate[1], # 0.0411
  mtry          = metrics_1.1$mtry[1]        # 42
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")

model_brt_2 <- model_brt %>%
  fit(burden_interest ~ .,
      data = data_0.2.training)

predictions_0 <- augment(model_brt_2, new_data = data_0.2.testing)
rsq_0  <- rsq(predictions_0,  truth = burden_interest, estimate = .pred)

data_0.2.testing_matrix <- data_0.2.testing %>%
  select(-burden_interest)%>%
  as.matrix()

shap_1 <- predict(extract_fit_engine(model_brt_2),
                  data_0.2.testing_matrix,
                  predcontrib = TRUE,
                  approxcontrib = FALSE)

shap_1.1 <- shap_1 %>%
  as_tibble()%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
  arrange(desc(SHAP_contribution))%>%
  mutate(tot_contribution = sum(SHAP_contribution))%>%
  mutate(share_SHAP       = SHAP_contribution/tot_contribution)%>%
  select(-tot_contribution)

shap_1.2 <- shap_1.1 %>%
  mutate(VAR_0 = ifelse(grepl("education", variable), "Education", 
                        ifelse(grepl("heating_fuel", variable), "Heating fuel", 
                               ifelse(grepl("building_type", variable), "Building_Type", 
                                      ifelse(grepl("urban_type", variable), "Urban/Rural", 
                                             ifelse(grepl("renting", variable), "Renting", 
                                                    ifelse(grepl("employment", variable), "Employment", 
                                                           ifelse(grepl("bundesland", variable), "Bundesland", 
                                                                  ifelse(grepl("heating_type", variable), "Heating_Type", 
                                                                         ifelse(grepl("building_year", variable), "Building_Year", 
                                                                                ifelse(grepl("industry", variable), "Industry", 
                                                                                       ifelse(grepl("ausbildung", variable), "Ausbildung", 
                                                                                              ifelse(grepl("cooking_fuel", variable), "Cooking_Fuel", 
                                                                                                     ifelse(grepl("housing_type", variable), "Housing type", 
                                                                                                            ifelse(grepl("construction_year", variable), "Construction year", 
                                                                                                                   ifelse(grepl("occupation", variable), "Occupation", 
                                                                                                                          ifelse(grepl("province", variable), "Province", 
                                                                                                                                 ifelse(grepl("nationality", variable), "Nationality", variable))))))))))))))))))%>%
  mutate(VAR_0 = ifelse(grepl("district", variable), "District",
                        ifelse(grepl("house_age", variable), "House Age",
                               ifelse(grepl("tenant", variable), "Tenant",
                                      ifelse(grepl("urban_identif_2", variable), "Urban_Identif_2",
                                             ifelse(grepl("urban_identif", variable) & VAR_0 != "Urban_Identif_2", "Urban_Identif",
                                                    ifelse(grepl("water_energy", variable), "Water energy", 
                                                           ifelse(grepl("house_type", variable), "House Type", VAR_0))))))))%>%
  mutate(VAR_0 = ifelse(VAR_0 == "number_of_cars", "Number of cars",
                        ifelse(VAR_0 == "hh_expenditures_EURO_2018", "HH expenditures", VAR_0)))%>%
  group_by(VAR_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  arrange(desc(share_SHAP))%>%
  mutate(help_0 = c(1,1,1,1, rep(0,6)))%>%
  mutate(order = 1:n())%>%
  filter(VAR_0 != "noise")

P_6.4.1 <- ggplot(shap_1.2)+
  geom_col(aes(x = share_SHAP, y = reorder(VAR_0, desc(order)), fill = factor(help_0)), width = 0.7, colour = "black", size = 0.3)+
  theme_bw()+
  coord_cartesian(xlim = c(0,0.28))+
  scale_fill_manual(values = c("#E64B35FF","#6F99ADFF"))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  guides(fill = "none")+
  xlab("Feature importance (SHAP)")+
  ylab("Feature")+
  ggtitle(bquote(.("France (") *R^2* .("= 0.2)")))+
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

shap_1.1 <- shap_1 %>%
  as_tibble()%>%
  select(hh_expenditures_EURO_2018)%>%
  rename(SHAP_hh_expenditures_EURO_2018 = hh_expenditures_EURO_2018)%>%
  bind_cols(select(data_0.2.testing, hh_expenditures_EURO_2018))%>%
  mutate(id = 1:n())%>%
  mutate(sd_exp   = sd(hh_expenditures_EURO_2018),
         mean_exp = mean(hh_expenditures_EURO_2018))%>%
  mutate(z_score_exp = (hh_expenditures_EURO_2018-mean_exp)/sd_exp)%>%
  select(-mean_exp, -sd_exp)%>%
  filter(z_score_exp < 3)

P_6.4.2 <- ggplot(shap_1.1)+
  geom_hline(aes(yintercept = 0))+
  geom_point(aes(x = hh_expenditures_EURO_2018,
                 y = SHAP_hh_expenditures_EURO_2018,
                 colour = z_score_exp),
             size = 0.5, alpha = 0.2)+
  geom_smooth(aes(x     = hh_expenditures_EURO_2018,
                  y     = SHAP_hh_expenditures_EURO_2018),
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
  ggtitle("HH expenditures (Importance: 15%)")+
  coord_cartesian(xlim = c(0,65000), ylim = c(-0.01,0.03))+ # TBA
  scale_x_continuous(labels = scales::dollar_format(prefix ="€"), expand = c(0,0), n.breaks = 4)+
  xlab("Household expenditures in EURO (2018)")+
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

shap_1.3 <- shap_1 %>%
  as_tibble()%>%
  pivot_longer(everything(), names_to = "variable", values_to = "SHAP")%>%
  mutate(Var_0 = ifelse(grepl("tenant", variable), "Tenant",
                        ifelse(grepl("heating_fuel", variable), "Heating Fuel",
                               ifelse(grepl("number_of_cars", variable), "Number of cars", NA))))%>%
  filter(!is.na(Var_0))

shap_1.3.1 <- shap_1.3 %>%
  filter(Var_0 == "Number of cars")%>%
  mutate(id      = 1:n())%>%
  bind_cols(select(data_0.2.testing, number_of_cars))%>%
  left_join(select(shap_1.1, id, z_score_exp))%>%
  filter(number_of_cars < 4)%>%
  filter(z_score_exp < 3)%>%
  mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))


P_6.4.3 <- ggplot(shap_1.3.1)+
  geom_hline(aes(yintercept = 0))+
  # geom_violin(aes(x = Variable,
  #                 y = SHAP),
  #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
  #ggforce::geom_sina(aes(x = Variable,
  #                       y = SHAP,
  #                       colour = z_score_exp,
  #                       fill = z_score_exp),
  #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
  geom_jitter(aes(x = as.factor(number_of_cars),
                  y = SHAP,
                  colour = z_score_exp,
                  fill = z_score_exp),
              size = 1, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
  theme_bw()+
  coord_cartesian(ylim = c(-0.015, 0.01))+
  scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  # scale_colour_viridis_c()+
  # scale_fill_viridis_c()+
  guides(colour = "none", fill = "none")+
  ggtitle("Car own. (Importance: 14%)")+
  xlab("Number of cars")+
  ylab("SHAP values for car ownership")+
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 7),
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

shap_1.3.0 <- shap_1.3 %>%
  filter(Var_0 == "Heating Fuel")%>%
  mutate(id = rep(1:nrow(data_0.2.testing), each = n()/nrow(data_0.2.testing), length.out = nrow(.)))

shap_1.3.2 <- select(data_0.2.test, "heating_fuel")%>%
  mutate(id = 1:n())%>%
  left_join(shap_1.3.0, by = "id")%>%
  mutate(rows = n())%>%
  mutate(heating_fuel = as.character(heating_fuel))%>%
  group_by(heating_fuel)%>%
  mutate(share_HF = n()/rows)%>%
  ungroup()%>%
  mutate(heating_fuel = ifelse(share_HF < 0.03, "other", heating_fuel))%>%
  mutate(variable = str_remove(variable, "heating_fuel_"))%>%
  mutate(Yes = ifelse(variable == heating_fuel,1,
                      ifelse(variable == "Fuel..mazout..pétrole" & heating_fuel == "Fuel, mazout, pétrole",1,
                             ifelse(variable == "Gaz.de.ville" & heating_fuel == "Gaz de ville",1,0))))%>%
  filter(Yes == 1)

shap_1.3.2.1 <- left_join(data.frame(id = 1:nrow(data_0.2.testing)), shap_1.3.2, by = "id")%>%
  bind_cols(transmute(select(data_0.2.test, heating_fuel), heating_fuel_0 = as.character(heating_fuel)))%>%
  mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))%>%
  mutate(HF = ifelse(is.na(heating_fuel), heating_fuel_0, heating_fuel))%>%
  mutate(HF = ifelse(HF == "other", "Other", 
                     ifelse(HF == "Bois", "Wood",
                            ifelse(HF == "Electricité", "Electr.",
                                   ifelse(HF == "Gaz de ville", "City gas",
                                          ifelse(HF == "Fuel, mazout, pétrole", "Liquid fuel", HF))))))%>%
  select(id, SHAP, HF)%>%
  left_join(select(shap_1.1, id, z_score_exp))%>%
  filter(z_score_exp < 3)%>%
  mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))

P_6.4.4 <- ggplot(shap_1.3.2.1)+
  geom_hline(aes(yintercept = 0))+
  # geom_violin(aes(x = Variable,
  #                 y = SHAP),
  #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
  #ggforce::geom_sina(aes(x = Variable,
  #                       y = SHAP,
  #                       colour = z_score_exp,
  #                       fill = z_score_exp),
  #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
  geom_jitter(aes(x = as.factor(HF),
                  y = SHAP,
                  colour = z_score_exp,
                  fill = z_score_exp),
              size = 1, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
  theme_bw()+
  coord_cartesian(ylim = c(-0.01, 0.005))+
  scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  # scale_colour_viridis_c()+
  # scale_fill_viridis_c()+
  guides(colour = "none", fill = "none")+
  ggtitle("Heating fuel (Importance: 26%)")+
  xlab("Heating fuel")+
  ylab("SHAP values for heating fuel")+
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

shap_1.3.5 <- shap_1.3 %>%
  filter(Var_0 == "Tenant")%>%
  mutate(id = rep(1:nrow(data_0.2.testing), each = n()/nrow(data_0.2.testing), length.out = nrow(.)))

shap_1.3.6 <- select(data_0.2.test, "tenant")%>%
  mutate(id = 1:n())%>%
  left_join(shap_1.3.5, by = "id")%>%
  mutate(variable = str_remove(variable, "tenant_"))%>%
  mutate(Yes = ifelse(variable == tenant,1,
                      ifelse(variable == "Propriétaire.ou.copropriétaire..y.compris.en.indivision....vous.n.avez.pas.de.remboursement.de.prêt.sur.votre.habitation" & tenant == "Propriétaire ou copropriétaire (y compris en indivision) : vous n'avez pas de remboursement de prêt sur votre habitation",1,0)))%>%
  filter(Yes == 1)

shap_1.3.6.1 <- left_join(data.frame(id = 1:nrow(data_0.2.testing)), shap_1.3.6, by = "id")%>%
  bind_cols(transmute(select(data_0.2.test, tenant), tenant_0 = as.character(tenant)))%>%
  mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))%>%
  mutate(Tenant = ifelse(tenant_0 == "Other", "Other",
                         ifelse(tenant_0 == "Locataire", "Tenant",
                                ifelse(tenant_0 == "Accédant à la propriété : vous avez des remboursements de prêts en cours", "Owner - loan repay",
                                   ifelse(tenant_0 == "Propriétaire ou copropriétaire (y compris en indivision) : vous n'avez pas de remboursement de prêt sur votre habitation", "Owner", tenant_0)))))%>%
  select(id, SHAP, Tenant)%>%
  left_join(select(shap_1.1, id, z_score_exp))%>%
  filter(z_score_exp < 3)%>%
  mutate(SHAP = ifelse(is.na(SHAP),0,SHAP))%>%
  filter(Tenant != "Other")

P_6.4.5 <- ggplot(shap_1.3.6.1)+
  geom_hline(aes(yintercept = 0))+
  # geom_violin(aes(x = Variable,
  #                 y = SHAP),
  #             size = 0.1, fill = "grey", alpha = 0.1, scale = "count")+
  #ggforce::geom_sina(aes(x = Variable,
  #                       y = SHAP,
  #                       colour = z_score_exp,
  #                       fill = z_score_exp),
  #                   size = 0.75, alpha = 0.75, scale = "count", shape = 21)+
  geom_jitter(aes(x = as.factor(Tenant),
                  y = SHAP,
                  colour = z_score_exp,
                  fill = z_score_exp),
              size = 1, alpha = 0.2, height = 0, width = 0.25, shape = 21)+
  theme_bw()+
  scale_colour_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  scale_fill_gradient(low = "#0072B5FF", high = "#BC3C29FF")+
  # scale_colour_viridis_c()+
  # scale_fill_viridis_c()+
  guides(colour = "none", fill = "none")+
  ggtitle("House ownership (Importance: 13%)")+
  xlab("House ownership")+
  ylab("SHAP values for house ownership")+
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

P_joint <- ggarrange(P_6.4.1, P_6.4.4, P_6.4.2, P_6.4.3, P_6.4.5, align = "h", nrow = 1)

jpeg("6_Versions/C3A/Figures/Figure_5b.jpg", width = 30, height = 10, unit = "cm", res = 300)
print(P_joint)
dev.off()
