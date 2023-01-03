# 0   General ####

# Author: L. Missbach, missbach@mcc-berlin.net

# 0.1 Packages ####

library("cowplot")
library("ggpubr")
library("ggsci")
library("haven")
library("Hmisc")
library("openxlsx")
library("rattle")
library("scales")
library("tidyverse")
options(scipen=999)

# 1   Loading Data ####

for(Country.Name in c("Argentina", "Bangladesh", "Bolivia", "Ecuador", "Ethiopia", "Europe", "India", "Indonesia", "Israel", "Nigeria", "Peru", "Philippines", "South_Africa", "Thailand", "Turkey", "Vietnam")) {

# Country.Name <- "Ecuador"

carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", Country.Name))

household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", Country.Name))

#fuel_expenditures_0       <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/2_Fuel_Expenditure_Data/fuel_expenditures_%s.csv", Country.Name))

if(Country.Name == "South_Africa") Country.Name <- "South Africa"

if(Country.Name == "Europe"){
  household_information_0    <- household_information
  
  carbon_pricing_incidence_0 <- final_incidence_information
}

# 2   Graphics Individual ####

carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)

# 2.1 Cumulative Curves ####

adjust_0 <- 0.3

add_on_df <- expand_grid(Income_Group_5 = c(1,2,3,4,5), burden_CO2_national = c(seq(0,0.1,0.001)))%>%
  mutate(hh_weights = 0)

add_on_weights <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(IG_weights = sum(hh_weights))%>%
  ungroup()

carbon_pricing_incidence_2.1 <- carbon_pricing_incidence_1 %>%
  mutate(burden_CO2_national = round(burden_CO2_national,3))%>%
  filter(!is.na(burden_CO2_national))%>%
  bind_rows(add_on_df)%>%
  group_by(Income_Group_5, burden_CO2_national)%>%
  summarise(hh_weights = sum(hh_weights))%>%
  ungroup()%>%
  left_join(add_on_weights, by = "Income_Group_5")%>%
  mutate(share = hh_weights/IG_weights)

add_on_median <- carbon_pricing_incidence_2.1 %>%
  group_by(Income_Group_5)%>%
  mutate(cumsum_shares = cumsum(share))%>%
  filter(cumsum_shares >= 0.5)%>%
  slice(which.min(cumsum_shares))%>%
  ungroup()%>%
  rename(median = burden_CO2_national)%>%
  select(Income_Group_5, median)

add_on_median <- ggplot_build(ggplot(carbon_pricing_incidence_2.1, aes(y = share, x = burden_CO2_national, group = factor(Income_Group_5)))+
               geom_smooth(method = "loess", span = adjust_0, se = FALSE))$data[[1]]%>%
  select(x,y,group)%>%
  left_join(add_on_median, by = c("group" = "Income_Group_5"))%>%
  mutate(help = median - x)%>%
  mutate(help_0 = ifelse(help <0, help*-1, help))%>%
  group_by(group)%>%
  filter(help_0 == min(help_0))%>%
  ungroup()%>%
  rename(Income_Group_5 = group, median.x = x, median.y = y)%>%
  select(-median, -help, -help_0)%>%
  select(median.x, median.y, Income_Group_5)

carbon_pricing_incidence_2.1 <- left_join(carbon_pricing_incidence_2.1, add_on_median)

plot_figure_1 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Carbon Price Incidence",
                          YLAB = "Share of Households per Quintile", 
                          fill0 = "none"){

P_X <- ggplot(carbon_pricing_incidence_2.1, aes(group = factor(Income_Group_5), colour = factor(Income_Group_5), linetype = factor(Income_Group_5)))+
  theme_bw()+
  theme(axis.text.y = ATY, 
        axis.text.x = ATX,
        axis.title  = ATT,
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))+
  #annotate("rect", xmin = min_median, xmax = max_median, ymin = 0, ymax = 0.11, alpha = 0.5, fill = "grey")+
  #annotate("segment", x = min_median, xend = max_median, y = 0.078, yend = 0.078, arrow = arrow(ends = "both", angle = 90, length = unit (.05, "cm")), size = 0.2)+
  #annotate("text", x = (min_median + max_median)/2, y = 0.081, label = "paste(Delta, V)", parse = TRUE, size = 1.5)+
  geom_smooth(aes(x = burden_CO2_national, y = share), size = 0.3, method = "loess", n = 160, span = adjust_0, se = FALSE, fullrange = TRUE)+
  geom_point(aes(x = median.x, y = median.y, group = factor(Income_Group_5), fill = factor(Income_Group_5)), shape = 21, size = 1.3, stroke = 0.2, colour = "black")+
  xlab(XLAB) +
  ylab(YLAB) +
  labs(colour = "", linetype = "", fill = "")+
  scale_y_continuous(breaks = c(0,0.025,0.05,0.075), expand = c(0,0), labels = scales::percent_format(accuracy = 0.1))+
  scale_x_continuous(expand = c(0,0), labels = scales::percent_format(accuracy = 1), breaks = seq(0,0.08, 0.02))+
  coord_cartesian(xlim = c(0,0.085), ylim = c(0,0.085))+
  #geom_segment(aes(x = median, xend = median, y = 0, yend = 100, colour = factor(Income_Group_5), linetype = factor(Income_Group_5)), size = 1)+
  scale_colour_manual(  values = c("#BC3C29FF","#00A087FF","#000000","#E18727FF",   "#0072B5FF"))+
  scale_fill_manual(    values = c("#BC3C29FF","#00A087FF","#000000","#E18727FF",   "#0072B5FF"))+
  scale_linetype_manual(values = c("solid", "longdash", "dotdash", "solid", "solid"))+
  ggtitle(Country.Name)+
  #guides(fill = guide_legend("Expenditure Quintile"), colour = guide_legend("Expenditure Quintile"), linetype = guide_legend("Expenditure Quintile"))
  guides(fill = fill0, colour = fill0, linetype = fill0)

return(P_X)
}

P_1 <- plot_figure_1()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_1_Distribution_National_Carbon_Price/Figure_1_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_1)
dev.off()

# L_1 <- ggdraw(get_legend(P_1))
# jpeg("../1_Carbon_Pricing_Incidence/2_Figures/Figure_1_Distribution_National_Carbon_Price/Legend_1.jpg", width = 8*400, height = 2*400, res = 400)
# L_1
# dev.off()

# 2.2 Boxplots ####

if(Country.Name != "Europe"){
carbon_pricing_incidence_2.2 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()
}

# Default Y-Axis
ylim0 <- 0.085

if(Country.Name == "Ethiopia") ylim0 <- 0.02  
if(Country.Name == "Argentina" | Country.Name == "Turkey" | Country.Name == "South Africa") ylim0 <- 0.205

plot_figure_2 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          accuracy_0 = 1,
                          data_0 = carbon_pricing_incidence_2.2,
                          title_0 = Country.Name){

P_2 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab(XLAB)+ ylab(YLAB)+
  geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = accuracy_0), expand = c(0,0))+
  scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
  coord_cartesian(ylim = c(0,ylim0))+
  ggtitle(title_0)+
  theme(axis.text.y = ATY, 
        axis.text.x = ATX,
        axis.title  = ATT,
        plot.title = element_text(size = 7),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        panel.border = element_rect(size = 0.3))

return(P_2)
}

P_2 <- plot_figure_2()

if(Country.Name == "Ethiopia") P_2 <- plot_figure_2(accuracy_0 = 0.1)

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_2)
dev.off()

# 2.2.1 Boxplots in Europe ####

if(Country.Name == "Europe"){
  carbon_pricing_incidence_2.2 <- carbon_pricing_incidence_1 %>%
    group_by(Income_Group_5, Country)%>%
    summarise(
      y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
      y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
      y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
      y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
      y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
      mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
    ungroup()
  
  for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
             "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
             "Portugal", "Romania", "Sweden", "Slovak Republic")){
    carbon_pricing_incidence_2.2.1 <- carbon_pricing_incidence_2.2 %>%
      filter(Country == i)
    
    ylim0 <- 0.1
    
    P_2 <- plot_figure_2(data_0 = carbon_pricing_incidence_2.2.1, title_0 = i)
    
    jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_%s.jpg", i), width = 6, height = 6, unit = "cm", res = 400)
    print(P_2)
    dev.off()
    
    
  }
  
  P_3 <- ggplot(carbon_pricing_incidence_2.2, aes(x = factor(Income_Group_5)))+
    geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
    theme_bw()+
    xlab("Expenditure Quintiles")+ ylab("Carbon Price Incidence")+
    facet_wrap(. ~ Country)+
    geom_point(aes(y = mean), shape = 23, size = 1.3, stroke = 0.2, fill = "white")+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
    scale_x_discrete(labels = c("1 \n Poorest \n 20 Percent", "2", "3", "4", "5 \n Richest \n 20 Percent"))+
    coord_cartesian(ylim = c(0,0.1))+
    ggtitle("")+
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
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))
  
  jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_2_Boxplot_National_Carbon_Price/Figure_2_Europe.jpg", i), width = 30, height = 30, unit = "cm", res = 400)
  print(P_3)
  dev.off()
  
}


# 2.3 Vertical Distribution across Instruments ####

if(Country.Name != "Europe"){

carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_1 %>%
  group_by(Income_Group_5)%>%
  summarise(
    wtd.median_CO2_global   = wtd.quantile(burden_CO2_global,   weight = hh_weights, probs = 0.5),
    wtd.median_CO2_national = wtd.quantile(burden_CO2_national, weight = hh_weights, probs = 0.5),
    wtd.median_transport    = wtd.quantile(burden_CO2_transport,    weight = hh_weights, probs = 0.5),
    wtd.median_electricity  = wtd.quantile(burden_CO2_electricity,  weight = hh_weights, probs = 0.5)
  )%>%
  ungroup()

carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_2.3 %>%
  mutate(CO2_global       = wtd.median_CO2_global  /carbon_pricing_incidence_2.3$wtd.median_CO2_global[1],
         CO2_national     = wtd.median_CO2_national/carbon_pricing_incidence_2.3$wtd.median_CO2_national[1],
         transport        = wtd.median_transport   /carbon_pricing_incidence_2.3$wtd.median_transport[1],
         electricity      = wtd.median_electricity /carbon_pricing_incidence_2.3$wtd.median_electricity[1])%>%
  select(-starts_with("wtd."))%>%
  pivot_longer(-Income_Group_5, names_to = "type", values_to = "Value")%>%
  unite(help, c("type", "Income_Group_5"), sep = "_", remove = FALSE)
}

plot_figure_3 <- function(ATT  = element_text(size = 7), ATX = element_text(size = 7), ATY = element_text(size = 7),
                          XLAB = "Expenditure Quintiles",
                          YLAB = "Carbon Price Incidence", 
                          fill0 = "none",
                          data_0 = carbon_pricing_incidence_2.3,
                          title_0 = Country.Name){
  P_3 <- ggplot(data_0, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, colour = "black", size = 0.3)+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    #geom_label_repel(aes(y = 1,    group = type,  label = label),   size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
    #geom_label_repel(aes(y = 3,    group = type,  label = Label_2), size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = -0.6)+
    #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
    geom_line(aes( y = Value, group = type, colour = type, alpha = type), size = 0.4, position = position_dodge(0.2))+
    geom_point(aes(y = Value, group = type, fill = type, shape = type, alpha = type), size = 1.5, colour = "black", position = position_dodge(0.2), stroke = 0.2)+
    scale_colour_npg(  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price")) +
    scale_fill_npg  (  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"))+
    scale_shape_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(21,22,23,24,25))+
    scale_alpha_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(1,1,1,1,1))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    #scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
    theme(axis.text.y = ATY, 
          axis.text.x = ATX,
          axis.title  = ATT,
          plot.title = element_text(size = 7),
          legend.position = "bottom",
          strip.text = element_text(size = 7),
          strip.text.y = element_text(angle = 180),
          panel.grid.major = element_line(size = 0.3),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(size = 0.2),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7),
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))+
    coord_cartesian(ylim = c(0.5,2.5))+
    #guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    guides(fill = fill0, colour = fill0, shape = fill0, size = fill0, alpha = fill0)+
    xlab(XLAB)+
    ylab(YLAB)+ 
    ggtitle(title_0)
  
  return(P_3)
  
}

P_3 <- plot_figure_3()

jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_3_Vertical_Effects/Figure_3_%s.jpg", Country.Name), width = 6, height = 6, unit = "cm", res = 400)
print(P_3)
dev.off()

# 2.3.1 Vertical Distribution Across Instruments in Europe ####

if(Country.Name == "Europe"){
  
  carbon_pricing_incidence_2.3.0 <- data.frame()
  
  carbon_pricing_incidence_2.3 <- carbon_pricing_incidence_1 %>%
    group_by(Income_Group_5, Country)%>%
    summarise(
      wtd.median_CO2_global   = wtd.quantile(burden_CO2_global,       weight = hh_weights, probs = 0.5),
      wtd.median_CO2_national = wtd.quantile(burden_CO2_national,     weight = hh_weights, probs = 0.5),
      wtd.median_transport    = wtd.quantile(burden_CO2_transport,    weight = hh_weights, probs = 0.5),
      wtd.median_electricity  = wtd.quantile(burden_CO2_electricity,  weight = hh_weights, probs = 0.5)
    )%>%
    ungroup()
  
  for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
             "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
             "Portugal", "Romania", "Sweden", "Slovak Republic")){
    
    carbon_pricing_incidence_2.3.1 <- carbon_pricing_incidence_2.3 %>%
      filter(Country == i)
    
    carbon_pricing_incidence_2.3.1 <- carbon_pricing_incidence_2.3.1 %>%
      mutate(CO2_global       = wtd.median_CO2_global  /carbon_pricing_incidence_2.3.1$wtd.median_CO2_global[1],
             CO2_national     = wtd.median_CO2_national/carbon_pricing_incidence_2.3.1$wtd.median_CO2_national[1],
             transport        = wtd.median_transport   /carbon_pricing_incidence_2.3.1$wtd.median_transport[1],
             electricity      = wtd.median_electricity /carbon_pricing_incidence_2.3.1$wtd.median_electricity[1])%>%
      select(-starts_with("wtd."))%>%
      pivot_longer(CO2_global:electricity, names_to = "type", values_to = "Value")%>%
      unite(help, c("type", "Income_Group_5", "Country"), sep = "_", remove = FALSE)
    
    carbon_pricing_incidence_2.3.0 <- carbon_pricing_incidence_2.3.0 %>%
      bind_rows(carbon_pricing_incidence_2.3.1)
  }
  
  for(i in c("Belgium", "Bulgaria", "Cyprus", "Czech Republic", "Germany", "Denmark", "Estonia", "Greece", "Spain", "Finland",
             "France", "Croatia", "Hungary" ,"Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland",
             "Portugal", "Romania", "Sweden", "Slovak Republic")){
    carbon_pricing_incidence_2.3.2 <- carbon_pricing_incidence_2.3.0 %>%
      filter(Country == i)
    
    P_3 <- plot_figure_3(data_0 = carbon_pricing_incidence_2.3.2, title_0 = i)
    
    jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_3_Vertical_Effects/Figure_3_%s.jpg", i), width = 6, height = 6, unit = "cm", res = 400)
    print(P_3)
    dev.off()
    
    
  }
  
  P_3.1 <- ggplot(carbon_pricing_incidence_2.3.0, aes(x = factor(Income_Group_5)))+
    geom_hline(yintercept = 1, colour = "black", size = 0.3)+
    #geom_ribbon(aes(ymin = low, ymax = upper, group = type, fill = type), alpha = 0.2)+
    #geom_label_repel(aes(y = 1,    group = type,  label = label),   size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = nudge_0)+
    #geom_label_repel(aes(y = 3,    group = type,  label = Label_2), size = 1.6, segment.linetype = 1, segment.size = 0.1, box.padding = 0.00, label.padding = 0.10, label.r = 0.05, direction = "y", min.segment.length = 0, nudge_y = -0.6)+
    #geom_label_repel(aes(y = pure, group = type, segment.linetype = 1, label = label_emissions_coverage, segment.size = 1, size = 15), min.segment.length = 0, hjust = 1, force_pull = 0, nudge_x = 1)+
    geom_line(aes( y = Value, group = type, colour = type, alpha = type), size = 0.4, position = position_dodge(0.2))+
    geom_point(aes(y = Value, group = type, fill = type, shape = type, alpha = type), size = 1.5, colour = "black", position = position_dodge(0.2), stroke = 0.2)+
    scale_colour_npg(  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price")) +
    scale_fill_npg  (  labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"))+
    scale_shape_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(21,22,23,24,25))+
    scale_alpha_manual(labels = c("International Carbon Price","National Carbon Price", "Electricity Sector Carbon Price", "Liquid Fuel Carbon Price"), values = c(1,1,1,1,1))+
    labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
    facet_wrap(. ~ Country)+
    theme_bw() + 
    scale_x_discrete(labels = c("1","2","3","4","5"))+
    #scale_y_continuous(breaks = seq(limit_low, limit_up, step_0))+
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
          plot.margin = unit(c(0.1,0.1,0,0), "cm"),
          panel.border = element_rect(size = 0.3))+
    coord_cartesian(ylim = c(0.5,2.5))+
    guides(fill = guide_legend(nrow = 2, order = 1), colour = guide_legend(nrow = 2, order = 1), shape = guide_legend(nrow = 2, order = 1), alpha = FALSE, size = FALSE)+
    #guides(fill = fill0, colour = fill0, shape = fill0, size = fill0, alpha = fill0)+
    xlab("Expenditure Quintiles")+
    ylab("Carbon Price Incidence")+ 
    ggtitle("")
  
  jpeg(sprintf("../1_Carbon_Pricing_Incidence/2_Figures/Figure_3_Vertical_Effects/Figure_3_Europe.jpg", i), width = 30, height = 30, unit = "cm", res = 400)
  print(P_3.1)
  dev.off()
  
}

# 2.4 Correlations with Incidence - Energy, Fuels etc. ####
# 2.5 Tax Policies ####
# 2.6 Maps ####
# 2.7 
# 3.X ####

print(paste0("End ", Country.Name))

rm(list = ls())
}

# 4.  Special Figures ####
# 4.1 Green Fiscal Policy Network ####


carbon_pricing_4.1.0 <- data.frame()
carbon_pricing_4.1.2 <- data.frame()
carbon_pricing_4.1.3 <- data.frame()
# Check Vietnam
for(i in c("India", "Bangladesh", "Indonesia", "Pakistan", "Vietnam", "Turkey", "Philippines", "Thailand")){
  
  Country.Name <- i
  
  if(Country.Name == "Pakistan") carbon_pricing_incidence_1   <- read_csv("C:/Users/misl/OwnCloud/Distributional Paper/Incidence_Analysis_11_2020_GTAP/Incidence.Analysis.Pakistan_11_2020.csv")%>%
      rename(burden_CO2_national = burden_CO2_per_capita)%>%
      mutate(exp_CO2_national = exp_pc_CO2_within*hh_size,
             hh_expenditures_USD_2014 = hh_size*hh_expenditure_USD_pc)
  if(Country.Name != "Pakistan"){
  
  carbon_pricing_incidence_0 <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/Carbon_Pricing_Incidence_%s.csv", Country.Name))
  
  household_information_0    <- read_csv(sprintf("../1_Carbon_Pricing_Incidence/1_Data_Incidence_Analysis/1_Transformed_and_Modeled/household_information_%s_new.csv", Country.Name))
  
  carbon_pricing_incidence_1 <- left_join(carbon_pricing_incidence_0, household_information_0)
  }
  carbon_pricing_4.1.1 <- carbon_pricing_incidence_1 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_CO2_national = wtd.quantile(burden_CO2_national, probs = 0.5, weight = hh_weights))%>%
    ungroup()%>%
    mutate(Country = Country.Name)%>%
    mutate(relative_burden_CO2_national = burden_CO2_national/burden_CO2_national[1])
  
  carbon_pricing_4.1.0 <- carbon_pricing_4.1.0 %>%
    bind_rows(carbon_pricing_4.1.1)
  
  carbon_pricing_4.1.2.1 <- carbon_pricing_incidence_1 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_5  = wtd.quantile(burden_CO2_national, probs = 0.05, weight = hh_weights),
              burden_25 = wtd.quantile(burden_CO2_national, probs = 0.25, weight = hh_weights),
              burden_50 = wtd.quantile(burden_CO2_national, probs = 0.50, weight = hh_weights),
              burden_75 = wtd.quantile(burden_CO2_national, probs = 0.75, weight = hh_weights),
              burden_95 = wtd.quantile(burden_CO2_national, probs = 0.95, weight = hh_weights),
              burden_me = wtd.mean(burden_CO2_national, weight = hh_weights))%>%
    ungroup()%>%
    mutate(Country = Country.Name)%>%
    mutate(min_median = min(burden_50),
           max_median = max(burden_50))
  
  carbon_pricing_4.1.2 <- carbon_pricing_4.1.2 %>%
    bind_rows(carbon_pricing_4.1.2.1)
  
  carbon_pricing_4.1.3.1 <- carbon_pricing_incidence_1 %>%
    mutate(revenues = exp_CO2_national*hh_weights,
           population = hh_weights*hh_size)
  
  total_revenues     <- sum(carbon_pricing_4.1.3.1$revenues)
  population         <- sum(carbon_pricing_4.1.3.1$population)
  revenue_per_capita <- total_revenues/population
  
  carbon_pricing_4.1.3.1 <- carbon_pricing_4.1.3.1 %>%
    mutate(revenue_hh = hh_size*revenue_per_capita,
           net_budget = revenue_hh-exp_CO2_national,
           net_burden = net_budget/hh_expenditures_USD_2014,
           burden_0   = -burden_CO2_national)
  
  carbon_pricing_4.1.3.1.1 <- carbon_pricing_4.1.3.1 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_5  = wtd.quantile(net_burden, probs = 0.05, weight = hh_weights),
              burden_25 = wtd.quantile(net_burden, probs = 0.25, weight = hh_weights),
              burden_50 = wtd.quantile(net_burden, probs = 0.50, weight = hh_weights),
              burden_75 = wtd.quantile(net_burden, probs = 0.75, weight = hh_weights),
              burden_95 = wtd.quantile(net_burden, probs = 0.95, weight = hh_weights),
              burden_me = wtd.mean(net_burden, weight = hh_weights))%>%
    ungroup()%>%
    mutate(type = "net")
  
  carbon_pricing_4.1.3.1.2 <- carbon_pricing_4.1.3.1 %>%
    group_by(Income_Group_5)%>%
    summarise(burden_5  = wtd.quantile(burden_0, probs = 0.05, weight = hh_weights),
              burden_25 = wtd.quantile(burden_0, probs = 0.25, weight = hh_weights),
              burden_50 = wtd.quantile(burden_0, probs = 0.50, weight = hh_weights),
              burden_75 = wtd.quantile(burden_0, probs = 0.75, weight = hh_weights),
              burden_95 = wtd.quantile(burden_0, probs = 0.95, weight = hh_weights),
              burden_me = wtd.mean(burden_0, weight = hh_weights))%>%
    ungroup()%>%
    mutate(type = "0")%>%
    bind_rows(carbon_pricing_4.1.3.1.1)%>%
    mutate(Country = Country.Name)%>%
    filter(Income_Group_5 == 1 | Income_Group_5 == 5)
  
  carbon_pricing_4.1.3 <- carbon_pricing_4.1.3 %>%
    bind_rows(carbon_pricing_4.1.3.1.2)
  
  print(Country.Name)
  print(round(revenue_per_capita,3))
}

# Vertical Distribution of National Carbon Pricing Across Countries
P_4.1.0 <- ggplot(carbon_pricing_4.1.0, aes(x = factor(Income_Group_5), y = relative_burden_CO2_national, group = Country))+
            geom_hline(yintercept = 1, colour = "black", size = 0.3)+
            geom_line(aes(colour = Country), size = 0.4, position = position_dodge(0.1))+
            geom_point(aes(fill = Country, shape = Country), size = 1.5, position = position_dodge(0.1), stroke = 0.2)+
  theme_bw() +          
  theme(axis.text.y = element_text(size = 7), 
                  axis.text.x = element_text(size = 7),
                  axis.title  = element_text(size = 7),
                  plot.title = element_text(size = 7),
                  strip.text = element_text(size = 7),
                  strip.text.y = element_text(angle = 180),
                  panel.grid.major = element_line(size = 0.3),
                  panel.grid.minor = element_blank(),
                  axis.ticks = element_line(size = 0.2),
                  legend.text = element_text(size = 7),
                  legend.title = element_text(size = 7),
                  plot.margin = unit(c(0.1,0.1,0,0), "cm"),
                  panel.border = element_rect(size = 0.3))+
            scale_colour_npg() +
            scale_fill_npg()+
            scale_shape_manual(values = c(21,22,21,22,21,22,21,22))+
            labs(fill = "", colour = "", shape = "", alpha = "", linetype = "")+
            scale_x_discrete(labels = c("1","2","3","4","5"))+
            coord_cartesian(ylim = c(0.5,2))+
            guides(colour = "none", size = "none", alpha = "none")+
            ylab("Additional median costs normalised by first quintile")+
            xlab("Income Quintile")
  

jpeg("C:/Users/misl/ownCloud/Distributional Paper/00000_GFPN_Report/Figures/Figure_2.jpeg", width = 15.5, height = 9, unit = "cm", res = 400)
print(P_4.1.0)
dev.off()

# Distributional Effects with implemented vertical difference
plot_4.1.2 <- function(Country.Name, YLAB = "Carbon Price Incidence", XLAB = "Income Quintile", 
                       ATX = element_text(size = 5), ATY = element_text(size = 7), ATT = element_text(size = 7)){
t <- filter(carbon_pricing_4.1.2, Country == Country.Name)

min_median <- t$min_median[1]
max_median <- t$max_median[1]

P.1 <- ggplot(t, aes(x = Income_Group_5, group = interaction(Country, Income_Group_5)))+
  annotate("rect", ymin = min_median, ymax = max_median, xmin = 0, xmax = 11, alpha = 0.5, fill = "grey")+
  geom_boxplot(aes(ymin = burden_5, lower = burden_25, middle = burden_50, upper = burden_75, ymax = burden_95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  coord_flip(xlim = c(0.5,5.5), ylim = c(0,0.105))+
  geom_point(aes(y = burden_me), shape = 23, size = 1, stroke = 0.4, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  ggtitle(Country.Name)+
  theme(axis.text.y      = ATY, 
        axis.text.x      = ATX,
        axis.title       = ATT,
        plot.title       = element_text(size = 7),
        legend.position  = "bottom",
        strip.text       = element_text(size = 7),
        strip.text.y     = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks       = element_line(size = 0.2),
        legend.text      = element_text(size = 7),
        legend.title     = element_text(size = 7),
        plot.margin      = unit(c(0.1,0.1,0,0), "cm"),
        panel.border     = element_rect(size = 0.3))+
  xlab(XLAB)+
  ylab(YLAB)

return(P.1)

}

P.1.BA <- plot_4.1.2("Bangladesh", YLAB = "", ATX = element_blank())
P.1.II <- plot_4.1.2("India",      YLAB = "", ATX = element_blank(), XLAB = "", ATY = element_blank())
P.1.IO <- plot_4.1.2("Indonesia",  YLAB = "", ATX = element_blank(), XLAB = "", ATY = element_blank())
P.1.PK <- plot_4.1.2("Pakistan",   YLAB = "", ATX = element_blank(), XLAB = "", ATY = element_blank())
P.1.PH <- plot_4.1.2("Philippines",)
P.1.TH <- plot_4.1.2("Thailand",   XLAB = "", ATY = element_blank())
P.1.TK <- plot_4.1.2("Turkey",     XLAB = "", ATY = element_blank())
P.1.VT <- plot_4.1.2("Vietnam",    XLAB = "", ATY = element_blank())

P.4.1.2 <- ggarrange(P.1.BA, P.1.II, P.1.IO, P.1.PK, P.1.PH, P.1.TH, P.1.TK, P.1.VT, nrow = 2, ncol = 4, align = "hv")
#P.4.1.2

jpeg("C:/Users/misl/ownCloud/Distributional Paper/00000_GFPN_Report/Figures/Figure_3.jpeg", width = 15.5, height = 9, unit = "cm", res = 400)
print(P.4.1.2)
dev.off()

# Differences for first quintiles with lump-sum redistribution

P.4.1.3 <- ggplot(carbon_pricing_4.1.3, aes(x = factor(Income_Group_5)))+
  geom_hline(aes(yintercept = 0))+
  facet_wrap(. ~ Country, nrow = 1)+
  geom_boxplot(aes(ymin = burden_5, lower = burden_25, middle = burden_50, upper = burden_75, ymax = burden_95, fill = type), stat = "identity", position = position_dodge(0.7), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  geom_point(aes(y = burden_me, group = type), shape = 23, size = 1, stroke = 0.4, fill = "white", position = position_dodge(0.7), stat = "identity")+
  scale_y_continuous(labels = c("-10%", "-5%", "0%", "+5%", "+10%", "+15%", "+20%"), expand = c(0,0), breaks = seq(-0.1,0.2,0.05))+
  scale_x_discrete()+
  scale_fill_npg(labels = c("Carbon Price", "Carbon Price + Revenue Recycling"))+
  coord_cartesian(ylim = c(-0.1, 0.2))+
  theme(axis.text.y      = element_text(size = 7), 
        axis.text.x      = element_text(size = 7),
        axis.title       = element_text(size = 7),
        plot.title       = element_text(size = 7),
        legend.position  = "bottom",
        strip.text       = element_text(size = 7),
        strip.text.y     = element_text(angle = 180),
        panel.grid.major = element_line(size = 0.3),
        panel.grid.minor = element_blank(),
        axis.ticks       = element_line(size = 0.2),
        legend.text      = element_text(size = 7),
        legend.title     = element_text(size = 7),
        plot.margin      = unit(c(0.1,0.1,0,0), "cm"),
        panel.border     = element_rect(size = 0.3))+
  xlab("Income Quintile")+
  ylab("Household budget change")+
  labs(fill = "")

jpeg("C:/Users/misl/ownCloud/Distributional Paper/00000_GFPN_Report/Figures/Figure_4.jpeg", width = 15.5, height = 9, unit = "cm", res = 400)
print(P.4.1.3)
dev.off()

# 1.4.1 Average footprints per Country ####

data_1.4.1 <- data_2 %>%
  filter(!is.na(CO2_t_national))%>%
  group_by(Country)%>%
  summarise(
    mean_CO2_t_national = mean(CO2_t_national),
    mean_CO2_t_global   = mean(CO2_t_global)
  )%>%
  ungroup()

world <- map_data("world")%>%
  mutate(Country = countrycode(region, origin = "country.name", destination = "iso3c"))%>%
  left_join(data_1.4.1)%>%
  filter(region != "Antarctica")

P.1 <- ggplot()+
  #geom_map(data = world, map = world, aes(map_id = region), fill = "lightgrey")+
  geom_map(data = world, map = world,
           aes(long, lat, map_id = region, fill = mean_CO2_t_national), colour = "black", size = 0.1)+
  theme_bw()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 9),
        axis.ticks = element_blank(),
        panel.grid = element_blank())+
  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  #scale_fill_manual(values = c("#6F99ADFF", "#0072B5FF", "lightgrey"), na.translate = FALSE, na.value = "lightgrey")+
  labs(fill = "")+
  guides(colour = "none")+
  ggtitle("Average carbon footprint in t")

jpeg("C:/Users/misl/OwnCloud/Papiere und Stuff/PhD-Seminar 07.12.2022/Figure_0.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P.1)
dev.off()

# 1.4.2 Boxplots for all countries with 40 USD carbon tax ####

data_1.4.2 <- data_joint_1 %>%
  group_by(Country)%>%
  summarise(
    y5  = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.05),
    y25 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.25),
    y50 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.5),
    y75 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.75),
    y95 = wtd.quantile(burden_CO2_national, weights = hh_weights, probs = 0.95),
    mean = wtd.mean(   burden_CO2_national, weights = hh_weights))%>%
  ungroup()

P_2 <- ggplot(data_1.4.2, aes(x = reorder(Country, mean)))+
  geom_boxplot(aes(ymin = y5, lower = y25, middle = y50, upper = y75, ymax = y95), stat = "identity", position = position_dodge(0.5), outlier.shape = NA, width = 0.5, size = 0.3) +
  theme_bw()+
  xlab("Country")+ ylab("Carbon Pricing Incidence")+
  geom_point(aes(y = mean), shape = 23, size = 1.1, stroke = 0.4, fill = "white")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.2))+
  ggtitle("Additional costs for 40 USD/tCO2 carbon price")+
  theme(axis.text.y = element_text(size = 7), 
        axis.text.x = element_text(size = 5, angle = 90),
        axis.title  = element_text(size = 7),
        plot.title = element_text(size = 11),
        legend.position = "bottom",
        strip.text = element_text(size = 7),
        strip.text.y = element_text(angle = 180),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(size = 0.2),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        panel.border = element_rect(size = 0.3))

jpeg("C:/Users/misl/OwnCloud/Papiere und Stuff/PhD-Seminar 07.12.2022/Figure_1.jpg", width = 15.5, height = 15, unit = "cm", res = 400)
print(P_2)
dev.off()
