if(!require("pacman")) install.packages("pacman")

p_load("arrow", "boot", "broom", "countrycode","fixest", "ggpubr", "ggrepel",
       "ggsci", "Hmisc", "knitr", "kableExtra", "openxlsx", "rattle", "readxl", "scales", "tidyverse", "VennDiagram","xtable")

options(scipen=999)

data_0 <- read_parquet("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/3_Collated_Database/Collated_Database_2017_11C.parquet")

inflation_0 <- read_excel("H:/6_Citizen_Survey/2_Data/Supplementary/imf-dm-export-20250527.xls")%>%
  rename(Country = "Inflation rate, average consumer prices (Annual percent change)")%>%
  # filter(Country %in% c("United States"))%>%
  pivot_longer(-Country, names_to = "year", values_to = "rate")%>%
  filter(year < 2026)

list_0 <- list()
n <- 1
for(i in c("USA", "ARG", "AUS", "CAN", "CHL", "COL", "FRA", "DEU", "IND", "MEX", "NGA", "PHL", "ZAF", "ESP", "GBR")){
  data_0.1 <- filter(data_0, Country == i)%>%
    select(hh_id, hh_size, hh_weights, hh_expenditures_USD_2014, hh_expenditures, CO2_t_national, Income_Group_5, Income_Group_10)
  
  t <- data.frame(i_0 = c("USA", "ARG", "AUS", "CAN", "CHL", "COL", "FRA", "DEU", "IND", "MEX", "NGA", "PHL", "ZAF", "ESP", "GBR"),
                  Country = c("United States", "Argentina", "Australia", "Canada", "Chile", "Colombia", "France", "Germany", "Indonesia", "Mexico", "Nigeria", "Philippines", "South Africa", "Spain", "United Kingdom"))
  
  Country_0 <- t %>%
    filter(i_0 == i)%>%
    pull(Country)
  
  inflation_1 <- inflation_0 %>%
    filter(Country == Country_0)%>%
    mutate(rate = case_when(Country == "United Kingdom" & year < 2019 ~ "0",
                            Country == "United States" & year < 2020 ~ "0",
                            Country == "Argentina" & year < 2018 ~ "0",
                            Country == "Australia" & year < 2016 ~ "0",
                            Country == "Canada" & year < 2018 ~ "0",
                            Country == "Chile" & year < 2018 ~ "0",
                            Country == "Colombia" & year < 2017 ~ "0",
                            Country == "France" & year < 2018 ~ "0",
                            Country == "Germany" & year < 2019 ~ "0",
                            Country == "Indonesia" & year < 2019 ~ "0",
                            Country == "Mexico" & year < 2019 ~ "0",
                            Country == "Nigeria" & year < 2019 ~ "0",
                            Country == "Philippines" & year < 2016 ~ "0",
                            Country == "South Africa" & year < 2015 ~ "0",
                            Country == "Spain" & year < 2020 ~ "0",
                            TRUE ~ rate))%>%
    # filter((Country == "Germany" & year > 2018)|(Country == "France")|(Country == "Romania" & year > 2019)|(Country == "Spain" & year > 2018))%>%
    mutate(inflation_rate = 1+as.numeric(rate)/100)%>%
    group_by(Country)%>%
    summarise(inflation_rate = prod(inflation_rate))%>%
    ungroup()%>%
    pull(inflation_rate)
  
  print(inflation_1)
  
  print(i)
  
  data_0.1.1 <- data_0.1 %>%
    mutate(Income_Group_10_new = as.numeric(binning(hh_expenditures, bins = 10, method = "wtd.quantile", weights = hh_weights)))%>%
    group_by(Income_Group_10_new)%>%
    summarise(min = min(hh_expenditures),
              max = max(hh_expenditures),
              CO2_t_national = wtd.mean(CO2_t_national, hh_weights))%>%
    ungroup()%>%
    mutate(min = round(min*inflation_1,-2),
           max = round(max*inflation_1,-2))%>%
    mutate(min_monthly = round(min/12,-2),
           max_monthly = round(max/12,-2))%>%
    select(Income_Group_10_new, min, max, min_monthly, max_monthly, CO2_t_national)%>%
    mutate(CO2_t_national = round(CO2_t_national,1))
  
  list_0[[n]] <- data_0.1.1
  
  n <- n +1
  
}

names(list_0) <- c("USA", "ARG", "AUS", "CAN", "CHL", "COL", "FRA", "DEU", "IND", "MEX", "NGA", "PHL", "ZAF", "ESP", "GBR")

write.xlsx(list_0, "C:/Users/misl/Desktop/20251128_Country_Brackets.xlsx")

