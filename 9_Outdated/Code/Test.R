# Fit best model on entire set

track_1 <- read.xlsx("../0_Data/9_Supplementary Data/BRT-Tracking/Tracking_BRT.xlsx") %>%
  filter(Country == i)%>%
  dplyr::slice(which.min(mae_mean_cv_5_train))

data_6.1 <- filter(data_2, Country == i)

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
if(!i %in% c("ARG", "ARM", "AUT", "BEL", "BOL", "CHE", "DEU", "ESP", "FIN", "FRA",
             "GRC", "HUN", "ITA", "JOR", "NLD", "POL", "PRT", "ROU", "SUR", "SWE")){data_6.1.1 <- select(data_6.1.1, -District)}

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

rm(data_6.1.2)

# Feature engineering - Step 2 (with recipe) - with dummification

recipe_6.1.0 <- recipe(carbon_intensity_kg_per_USD_national ~ .,
                       data = data_6.1.2.train)%>%
  # Deletes all columns with any NA
  step_filter_missing(all_predictors(), threshold = 0)%>%
  # Remove minimum number of columns such that correlations are less than 0.9
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
  # should have very few unique observations for factors
  step_other(all_nominal(), -ends_with(".01"), -ends_with("urban_01"), -ends_with("District"), -ends_with("Province"), threshold = 0.05)%>%
  # including dummification
  step_dummy(all_nominal())

data_6.1.2.training <- recipe_6.1.0 %>%
  prep(training = data_6.1.2.train)%>%
  bake(new_data = NULL)

data_6.1.2.testing <- recipe_6.1.0 %>%
  prep(training = data_6.1.2.test)%>%
  bake(new_data = NULL) 

data_6.1.2.all <- recipe_6.1.0 %>%
  prep(training = data_6.1.2.train)%>%
  bake(new_data = data_6.1.1) 

# Setup model to be tuned

model_brt <- boost_tree(
  trees         = 1000,
  tree_depth    = track_1$tree_depth_best[1],
  learn_rate    = track_1$learn_rate_best[1], # the higher the learning rate the faster - default 0.3
  mtry          = track_1$mtry_best[1]
)%>%
  set_mode("regression")%>%
  set_engine("xgboost")

model_brt_1 <- model_brt %>%
  fit(carbon_intensity_kg_per_USD_national ~ .,
      data = data_6.1.2.training)

# Evalutation on training set
predictions_6.2.1 <- augment(model_brt_1, new_data = data_6.1.2.training)
# Evalutation on testing set
predictions_6.2.2 <- augment(model_brt_1, new_data = data_6.1.2.testing)
# Evaluation on entire set
predictions_6.2.3 <- augment(model_brt_1, new_data = data_6.1.2.all)

mae_6.2.1 <- mae(predictions_6.2.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
mae_6.2.2 <- mae(predictions_6.2.2, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
mae_6.2.3 <- mae(predictions_6.2.3, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
rmse_6.2.1 <- rmse(predictions_6.2.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
rmse_6.2.2 <- rmse(predictions_6.2.2, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
rsq_6.2.1 <- rsq(predictions_6.2.1, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)
rsq_6.2.2 <- rsq(predictions_6.2.2, truth = carbon_intensity_kg_per_USD_national, estimate = .pred)


# VIP

# PDP

# SHAP

test_1 <- data_6.1.2.training %>%
  select(-carbon_intensity_kg_per_USD_national)%>%
  as.matrix()

shap_contrib <- predict(extract_fit_engine(model_brt_1), 
                        test_1, 
                        predcontrib = TRUE, approxcontrib = F)

shap_contrib <- shap.prep(extract_fit_engine(model_brt_1), 
                        X_train = test_1)

shap_contrib_1 <- as_tibble(shap_contrib)%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(),names_to = "variable", values_to = "contribution")%>%
  arrange(desc(contribution))%>%
  mutate(tot_contribution = sum(contribution))%>%
  mutate(share = contribution/tot_contribution)

test_2 <- data_6.1.2.testing %>%
  select(-carbon_intensity_kg_per_USD_national)%>%
  as.matrix()

shap_contrib <- predict(extract_fit_engine(model_brt_1), 
                        test_2, 
                        predcontrib = TRUE, approxcontrib = F)

shap_contrib_2 <- as_tibble(shap_contrib)%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(),names_to = "variable", values_to = "contribution")%>%
  arrange(desc(contribution))%>%
  mutate(tot_contribution = sum(contribution))%>%
  mutate(share = contribution/tot_contribution)%>%
  rename(contribution_2 = contribution,
         tot_contribution_2 = tot_contribution,
         share_2 = share)

test_3 <- data_6.1.2.all %>%
  select(-carbon_intensity_kg_per_USD_national)%>%
  as.matrix()

shap_contrib <- predict(extract_fit_engine(model_brt_1), 
                        test_3, 
                        predcontrib = TRUE, approxcontrib = F)

shap_contrib_3 <- as_tibble(shap_contrib)%>%
  summarise_all(~ mean(abs(.)))%>%
  select(-BIAS)%>%
  pivot_longer(everything(),names_to = "variable", values_to = "contribution")%>%
  arrange(desc(contribution))%>%
  mutate(tot_contribution = sum(contribution))%>%
  mutate(share = contribution/tot_contribution)%>%
  rename(contribution_3 = contribution,
         tot_contribution_3 = tot_contribution,
         share_3 = share)

shap_contrib_4 = left_join(shap_contrib_1, shap_contrib_2, by = "variable")%>%
  left_join(shap_contrib_3, by = "variable")%>%
  select(variable, starts_with("share"))

# Add up shap-values --> later

shap_contrib_4.1 <- shap_contrib_4 %>%
  mutate(Variable = ifelse(variable %in% c("washing_machine.01_X1", "hh_size", "urban_01_X1",
                                           "hh_expenditures_USD_2014"), str_remove(variable, "_X1"), 
                           ifelse(variable == "sex_hhh_X2", "sex_hhh", str_extract(variable, "^[^_]*"))))

shap_contrib_4.1.1 <- shap_contrib_4.1 %>%
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
  mutate(Var_0 = ifelse(Variable %in% c("refrigerator.01", "ac.01", "tv.01", "washing_machine.01"), "Appliance own.", 
                        ifelse(Var_0 == "religiosity", "Religiosity", Var_0)))%>%
  group_by(Var_0)%>%
  summarise_at(vars(starts_with("share")), ~ sum(.))%>%
  ungroup()%>%
  mutate(help_0 = ifelse(share < 0.02,1,0))

# This is more like an aggregated output

shap_contrib_4.1.1.1 <- shap_contrib_4.1.1 %>%
  filter(help_0 == 1)%>%
  summarise(share = sum(share),
            share_2 = sum(share_2),
            share_3 = sum(share_3))%>%
  mutate(Var_0 = "Other features (Sum)")

shap_contrib_4.1.1.2 <- shap_contrib_4.1.1 %>%
  filter(help_0 == 0)%>%
  bind_rows(shap_contrib_4.1.1.1)%>%
  arrange(desc(share))%>%
  mutate(number_order = 1:n())

shap.prep_0 <- shap.prep(extract_fit_engine(model_brt_1), 
                       X_train = data_6.1.2.testing_matrix)


# shap.plot.summary(as_tibble(shap.prep_0))

ggplot(as_tibble(shap.prep_0)) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue),
                     method = "counts", maxwidth = 0.7, alpha = 0.7)

test <- filter(shap.prep_0, variable == "hh_expenditures_USD_2014")

ggplot(test,
       aes(x = rfvalue,
           y = value,
           color = rfvalue))+
  geom_point(
    size = 0.3, 
    width = 0,
    height = 0,
    alpha = 0.5,
  )+
  scale_color_gradient(low="#FFCC33", high="#6600CC",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  coord_cartesian(xlim = c(0,60000))
  
test <- shap.prep_0 %>%
  group_by(variable)%>%
  # rfvalue: raw feature value
  # value: SHAP value
  mutate(min_A = min(rfvalue),
         max_A = max(rfvalue))%>%
  ungroup()%>%
  mutate(test = (rfvalue - min_A)/(max_A - min_A))


test1 <- shap.prep_0 %>%
  group_by(variable)%>%
  mutate(mean_rf = mean(rfvalue),
         sd_rf   = sd(rfvalue))%>%
  ungroup()%>%
  mutate(z_score_rf = (rfvalue-mean_rf)/sd_rf)%>%
  filter(variable == "hh_expenditures_USD_2014")

test2 <- shap.prep_0 %>%
  group_by(variable)%>%
  mutate(mean_rf = mean(rfvalue),
         sd_rf   = sd(rfvalue))%>%
  ungroup()%>%
  mutate(z_score_rf = (rfvalue-mean_rf)/sd_rf)%>%
  filter(variable == "car.01_X1")%>%
  select(ID, value, rfvalue)%>%
  rename(value_car = value, rfvalue_car = rfvalue)%>%
  left_join(test1)

ggplot(test1,
       aes(x = rfvalue,
           y = value,
           color = z_score_rf))+
  geom_hline(aes(yintercept = 0))+
  geom_point(
    size = 0.3, 
    width = 0,
    height = 0,
    alpha = 0.5,
  )+
  scale_color_gradient(low="#FFCC33", high="#6600CC", 
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))+
  coord_cartesian(xlim = c(0,60000))

ggplot(test2,
       aes(x = as.factor(rfvalue_car),
           y = value,
           color = z_score_rf))+
  geom_hline(aes(yintercept = 0))+
  ggforce::geom_sina(alpha = 0.5, size = 0.6, maxwidth = 0.7)+
  #geom_jitter(
  #  size = 0.6, 
  #  width = 0.3,
  #  height = 0,
  #  alpha = 0.5,
  #)+
  scale_color_gradient(low="#FFCC33", high="#6600CC",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.3)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))
