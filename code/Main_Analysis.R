library(tidyverse)
library(car)
library(glmnet)
library(lmtest)

# Load the consolidated dataset
data <- read_csv("Processed Data Master File.csv")
names(data)
# Convert distance variables from metres to kilometres
data <- data %>%
  mutate(
    dist_water_km = dist_water_m / 1000,
    dist_road_km = dist_road_track_m / 1000,
    dist_urban_km = dist_urban_m / 1000,
    dist_urban_new_km = dist_urban_new / 1000
  )

# Group countries for fixed-effects models
data <- data %>%
  mutate(
    Country_group = case_when(
      Country == "Brazil" ~ "Brazil",
      Country == "Colombia" ~ "Colombia",
      Country == "Peru" ~ "Peru",
      TRUE ~ "Rest"
    )
  )

######## Thematic Block 1 - Location and land pressure ###########
# test if all values each variable has sufficient variability
table(data$driver_illegal_logging) #switch out for all variable

#model with all variables
model_b1_ols <- lm(diff_annual_norm ~ dist_road_km + dist_urban_km +dist_urban_new_km + dist_water_km +
                     driver_illegal_logging +
                     driver_smallholder_ag +
                     driver_industrial_ag +
                     driver_livestock +
                     driver_wood_extraction +
                     driver_infrastructure + 
                     driver_mining+
                     driver_other+
                     driver_local_livelihoods,
                   data = data)

summary(model_b1_ols)
library(car)
vif(model_b1_ols)

#run a correlation test between x-variables - check for MC
data %>%
  select(dist_road_track_m, dist_urban_m, driver_illegal_logging, driver_smallholder_ag,
         driver_industrial_ag, driver_livestock, driver_wood_extraction,
         driver_infrastructure, driver_local_livelihoods) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor() %>%
  round(2)

#>0.5 correlation between driver_indsutrial_ag and driver_livestock - so i merged them into one new variable
data <- data %>%
  mutate(driver_ag_combined = as.numeric(driver_industrial_ag == TRUE | 
                                           driver_livestock == TRUE))
#check multi-collinearity again
data %>%
  select(dist_road_track_m, dist_urban_m,dist_water_m, driver_illegal_logging, driver_smallholder_ag,
         driver_ag_combined, driver_wood_extraction,
         driver_infrastructure, driver_local_livelihoods) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor() %>%
  round(2)

model_b2_ols <- lm(diff_annual_norm ~ dist_road_km + dist_urban_new_km + dist_urban_km +dist_water_km+
                     driver_illegal_logging +
                     driver_smallholder_ag +
                     driver_ag_combined +
                     driver_wood_extraction + driver_mining+driver_other+
                     driver_infrastructure +
                     driver_local_livelihoods,
                   data = data)

summary(model_b2_ols)
vif(model_b2_ols)

#lets add country fixed effects
model_b2_ols_fe <- lm(diff_annual_norm ~ dist_road_km + dist_urban_new_km + dist_urban_km + dist_water_km +
                        driver_illegal_logging +
                        driver_smallholder_ag +
                        driver_ag_combined +
                        driver_wood_extraction + driver_mining + driver_other +
                        driver_infrastructure +
                        driver_local_livelihoods + 
                        relevel(as.factor(Country_group), ref = "Brazil"),
                      data = data)
summary(model_b2_ols_fe)

# Backward selection forcing fixed effects to stay - so they don't get kicked out based on AIC criterion
model_b2_step_fe <- step(model_b2_ols_fe, 
                         direction = "backward",
                         scope = list(lower = ~ relevel(as.factor(Country_group), ref = "Brazil")))
summary(model_b2_step_fe)

#checking OLS assumptions
shapiro.test(residuals(model_b2_step_fe)) #report this one
hist(residuals(model_b2_step_fe)) #check this out
qqnorm(residuals(model_b2_step_fe)) #report this
qqline(residuals(model_b2_step_fe)) #this goes with one above

#prepare data for lasso and ridge regression
x_b2_ols <- model.matrix(~ driver_ag_combined + driver_smallholder_ag +
                           driver_wood_extraction + driver_illegal_logging + 
                           driver_infrastructure + driver_mining +
                           driver_local_livelihoods + driver_other +
                           dist_road_km + dist_urban_km +dist_water_km +dist_urban_new_km+
                           relevel(as.factor(Country_group), ref = "Brazil"),
                         data = data)[, -1]
y_b2_ols <- data$diff_annual_norm

library(glmnet)

# i want country fixed effects to not be shrunk in ridge and lasso

make_penalty_factors <- function(x_matrix) {
  col_names <- colnames(x_matrix)
  penalty_factors <- rep(1, length(col_names))
  penalty_factors[grepl("Country_group", col_names)] <- 0
  penalty_factors[grepl("log_area", col_names)] <- 0  # also protect area control
  return(penalty_factors)
}

# Lasso
lasso_b2 <- cv.glmnet(x_b2_ols, y_b2_ols, alpha = 1, family = "gaussian", standardize=TRUE, penalty.factor = make_penalty_factors(x_b2_ols))
cat("LASSO coefficients:\n")
coef(lasso_b2, s = "lambda.min")

# Ridge
ridge_b2 <- cv.glmnet(x_b2_ols, y_b2_ols, alpha = 0, family = "gaussian", standardize=TRUE, penalty.factor = make_penalty_factors(x_b2_ols))
coef(ridge_b2, s = "lambda.min")

#Interaction Term

# Does the effect of distance vary by community wealth?

#water x wealth
model_dist_wealth_water <- lm(diff_annual_norm ~ dist_water_km * wealth_income_avg+ 
                                relevel(as.factor(Country_group), ref = "Brazil"),
                              data = data)
summary(model_dist_wealth_water)

#road x wealth
model_dist_wealth_road <- lm(diff_annual_norm ~ dist_road_km * wealth_income_avg
                             +relevel(as.factor(Country_group), ref = "Brazil"),
                             data = data)
summary(model_dist_wealth_road)

#city x wealth
model_dist_wealth_urban <- lm(diff_annual_norm ~ dist_urban_km * wealth_income_avg
                              + relevel(as.factor(Country_group), ref = "Brazil"),
                              data = data)
summary(model_dist_wealth_urban)

#town x wealth
model_dist_wealth_town <- lm(diff_annual_norm ~ dist_urban_new_km * wealth_income_avg
                                  + relevel(as.factor(Country_group), ref = "Brazil"),
                                  data = data)

summary(model_dist_wealth_town)

#lets see if significance of town x wealth interaction holds up when you add other variables from Block 1
model_dist_wealth_town_int <- lm(diff_annual_norm ~ dist_urban_new_km * wealth_income_avg
                                  +  dist_road_km +dist_urban_km+dist_water_km+
                                    driver_illegal_logging +
                                    driver_smallholder_ag +
                                    driver_ag_combined +
                                    driver_wood_extraction + driver_mining + driver_other +
                                    driver_infrastructure +
                                    driver_local_livelihoods + 
                                    relevel(as.factor(Country_group), ref = "Brazil"),
                                  data = data)
summary(model_dist_wealth_town_int) #holds up under normal standard errors
coeftest(model_dist_wealth_town_int, vcov = vcovHC(model_dist_wealth_town_int, type = "HC3")) #holds up under HC3

#let plot to see 
# Create distance sequence
dist_seq <- seq(min(data$dist_urban_new_km, na.rm = TRUE),
                max(data$dist_urban_new_km, na.rm = TRUE),
                length.out = 100)

# Define wealth levels
wealth_low <- quantile(data$wealth_income_avg, 0.25, na.rm = TRUE)
wealth_med <- quantile(data$wealth_income_avg, 0.50, na.rm = TRUE)
wealth_high <- quantile(data$wealth_income_avg, 0.75, na.rm = TRUE)

#where do the lines intersect
b <- coef(model_dist_wealth_town)

intercept <- function(w) {
  b["(Intercept)"] + b["wealth_income_avg"] * w
}

slope <- function(w) {
  b["dist_urban_new_km"] +
    b["dist_urban_new_km:wealth_income_avg"] * w
}

intersection <- function(w1, w2) {
  (intercept(w2) - intercept(w1)) /
    (slope(w1) - slope(w2))
}

# X values (km) - where does intersection happen
intersection(wealth_low, wealth_med)
intersection(wealth_low, wealth_high)
intersection(wealth_med, wealth_high)

# Predictions with confidence intervals
pred_low <- predict(model_dist_wealth_town,
                    newdata = data.frame(dist_urban_new_km = dist_seq,
                                         wealth_income_avg = wealth_low),
                    interval = "confidence")

pred_med <- predict(model_dist_wealth_town,
                    newdata = data.frame(dist_urban_new_km = dist_seq,
                                         wealth_income_avg = wealth_med),
                    interval = "confidence")

pred_high <- predict(model_dist_wealth_town,
                     newdata = data.frame(dist_urban_new_km = dist_seq,
                                          wealth_income_avg = wealth_high),
                     interval = "confidence")

# Plot
plot(dist_seq, pred_low[, "fit"],
     type = "l", col = "blue", lwd = 2,
     ylim = range(c(pred_low, pred_med, pred_high)),
     xlab = "Distance to Nearest Town (km)",
     ylab = "Predicted Forest Loss (% of project area)")

# Confidence intervals - low wealth
polygon(c(dist_seq, rev(dist_seq)),
        c(pred_low[, "lwr"], rev(pred_low[, "upr"])),
        col = adjustcolor("blue", alpha.f = 0.2), border = NA)

# Medium wealth line and CI
lines(dist_seq, pred_med[, "fit"], col = "darkgreen", lwd = 2)
polygon(c(dist_seq, rev(dist_seq)),
        c(pred_med[, "lwr"], rev(pred_med[, "upr"])),
        col = adjustcolor("darkgreen", alpha.f = 0.2), border = NA)

# High wealth line and CI
lines(dist_seq, pred_high[, "fit"], col = "red", lwd = 2)
polygon(c(dist_seq, rev(dist_seq)),
        c(pred_high[, "lwr"], rev(pred_high[, "upr"])),
        col = adjustcolor("red", alpha.f = 0.2), border = NA)

abline(h = 0, lty = 2, col = "grey")

legend("bottomleft",
       legend = c("Low wealth (25th)", "Medium wealth (50th)", "High wealth (75th)"),
       col = c("blue", "darkgreen", "red"),
       lwd = 1)

####### Thematic Block Regression 2 - governance of REDD+ project ##############
#prep data
# Recode governance variables as factors, keeping ND as a category
data <- data %>%
  mutate(
    fpic_f = factor(
      case_when(
        FPIC == "yes" ~ "Yes",
        FPIC == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    ),
    
    protected_area_f = factor(
      case_when(
        `Protected Area` == "yes" ~ "Yes",
        `Protected Area` == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    ),
    
    contested_f = factor(
      case_when(
        Contested == "yes" ~ "Yes",
        Contested == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    )
  )


# Create numeric tenure indicator - convert the singular ND value to 0; insufficient variation to save as a factor
data <- data %>%
  mutate(
    tenure = case_when(
      tenure_yn == "yes" ~ 1,
      tenure_yn == "no" ~ 0,
      TRUE ~ 0
    )
  )

#checking sufficient variation
table(data$fpic_f) #manual investigation

model_b3 <- lm(diff_annual_norm ~ fpic_f + protected_area_f + contested_f +
                 cu_private + cu_communities + cu_state +
                 lt_public + lt_private + lt_communities +
                 tenure, data=data)
summary(model_b3)
vif(model_b3)

#look at MC between x non-factor variables 
data %>%
  select(
    cu_private, cu_communities , cu_state ,
    lt_public , lt_private , lt_communities 
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor() %>%
  round(2)

#merging to create new merged variables to address multi-collinearity because correlation isright at 0.5 or over 0.5 correlation between cu and lt private/community/public&state
data <- data %>%
  mutate(
    rights_private = as.numeric(cu_private == TRUE | lt_private == TRUE),
    rights_communities = as.numeric(cu_communities == TRUE | lt_communities == TRUE),
    rights_public_state = as.numeric(cu_state == TRUE | lt_public == TRUE)
  )

model_b3_new <- lm(diff_annual_norm ~ fpic_f + protected_area_f + contested_f +
                     rights_private + rights_communities + rights_public_state +tenure
                   , data = data)
summary(model_b3_new)
vif(model_b3_new)

# add country fixed effects
model_b3_new_fe <- lm(diff_annual_norm ~ fpic_f + protected_area_f + contested_f +
                        rights_private + rights_communities + rights_public_state +tenure+
                        relevel(as.factor(Country_group), ref = "Brazil"),
                      data = data)
summary(model_b3_new_fe)

# Backward selection forcing fixed effects to stay - so they don't get kicked out based on AIC criterion
model_b3_step_fe <- step(model_b3_new_fe, 
                         direction = "backward",
                         scope = list(lower = ~ relevel(as.factor(Country_group), ref = "Brazil")))
summary(model_b3_step_fe)

#checking OLS
shapiro.test(residuals(model_b3_step_fe)) #report this one
hist(residuals(model_b3_step_fe)) #check this out
qqnorm(residuals(model_b3_step_fe)) #report this
qqline(residuals(model_b3_step_fe)) #this goes with one above

#preparing for lasso and ridge regression
x_b3 <- model.matrix(~ fpic_f + protected_area_f + contested_f +
                       rights_private + rights_communities + rights_public_state +
                       tenure +relevel(as.factor(Country_group), ref = "Brazil"),
                     data = data)[, -1]

y_b3_ols <- data$diff_annual_norm

lasso_b3_ols <-cv.glmnet(x_b3,y_b3_ols, alpha=1, family="gaussian", standardize=TRUE, penalty.factor = make_penalty_factors(x_b3))
coef(lasso_b3_ols, s="lambda.min")

ridge_b3_ols <-cv.glmnet(x_b3,y_b3_ols, alpha=0, family="gaussian",standardize=TRUE, penalty.factor = make_penalty_factors(x_b3))
coef(ridge_b3_ols, s = "lambda.min")

#Interaction effect - tenure (y/n) x contested
model_int_tenure <- lm(diff_annual_norm ~ contested_f + tenure+
                         relevel(as.factor(Country_group), ref = "Brazil"),
                       data = data)
summary(model_int_tenure)


########### Thematic Block 3-Socio-economic #####################
#prep data
# Recode benefit and employment variables as factors, keeping ND as a category
data <- data %>%
  mutate(
    direct_cash_f = factor(
      case_when(
        direct_cash_yn == "yes" ~ "Yes",
        direct_cash_yn == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    ),
    
    employment_f = factor(
      case_when(
        employment_yn == "yes" ~ "Yes",
        employment_yn == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    ),
    
    education_f = factor(
      case_when(
        education_yn == "yes" ~ "Yes",
        education_yn == "no" ~ "No",
        TRUE ~ "ND"
      ),
      levels = c("No", "Yes", "ND")
    ),
    
    conditionality_f = factor(
      case_when(
        monetary_benefit_type_conditional %in% c("conditional", "both") ~ "Conditional",
        monetary_benefit_type_conditional == "non-conditional" ~ "Non-conditional",
        TRUE ~ "ND"
      ),
      levels = c("Non-conditional", "Conditional", "ND")
    ),
    
    # Same variable name used later in the interaction model
    direct_cash_factor = direct_cash_f
  )

#check for sufficient variation
table(data$mb_PES) #manually check

model_b4 <- lm(diff_annual_norm ~ wealth_income_avg + mb_PES + mb_job + mb_carbon +
                 conditionality_f + direct_cash_f +
                 ncb_infrastructure + ncb_microfinance + ncb_health +
                 ncb_livelihood + ncb_water +
                 employment_f + education_f +
                 ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                 ea_agriculture + ea_fishing + ea_tree_planting,
               data = data)

summary(model_b4)
vif(model_b4)

# For binary variables, use chi-squared


vars_b4 <- c("mb_PES", "mb_job", "mb_carbon", "conditionality_f", "direct_cash_f",
             "ncb_infrastructure", "ncb_microfinance", "ncb_health",
             "ncb_livelihood", "ncb_water", "employment_f", "education",
             "ea_processing", "ea_agroforestry", "ea_microenterprise", "ea_ecotourism",
             "ea_agriculture", "ea_fishing", "ea_tree_planting")

chi_data_b4 <- data %>%
  select(all_of(vars_b4)) %>%
  mutate(across(everything(), as.factor))

var_pairs_b4 <- combn(vars_b4, 2, simplify = FALSE)

chi_results_b4 <- map_dfr(var_pairs_b4, function(pair) {
  tbl <- table(chi_data_b4[[pair[1]]], chi_data_b4[[pair[2]]])
  test <- chisq.test(tbl, simulate.p.value = TRUE)
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl)) - 1
  cramers_v <- round(sqrt(test$statistic / (n * k)), 3)
  tibble(var1 = pair[1], var2 = pair[2],
         chi_sq = round(test$statistic, 3),
         p_value = round(test$p.value, 4),
         cramers_v = cramers_v)
})

# Show matrix
chi_results_b4 %>%
  select(var1, var2, cramers_v) %>%
  bind_rows(rename(., var1 = var2, var2 = var1)) %>%
  pivot_wider(names_from = var2, values_from = cramers_v) %>%
  column_to_rownames("var1")



# For wealth_income_avg vs binary variables
cor_wealth <- data %>%
  summarise(across(all_of(vars_b4), ~cor(wealth_income_avg, as.numeric(.), 
                                         use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(cor_wealth)


# Base model without any mb variable - using anova to see how each improve the model: this tells us that mb_carbon improves the model, the others don't by themselves
model_base <- lm(diff_annual_norm ~ wealth_income_avg +
                   conditionality_f + direct_cash_f +
                   ncb_infrastructure + ncb_microfinance + ncb_health +
                   ncb_livelihood + ncb_water +
                   employment_f + education_f +
                   ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                   ea_agriculture + ea_fishing + ea_tree_planting,
                 data = data)

# Test each mb variable against base model 
mb_vars <- c("mb_PES", "mb_job", "mb_carbon")

map(mb_vars, function(var) {
  model <- update(model_base, paste(". ~ . +", var))
  cat("\n---", var, "---\n")
  print(anova(model_base, model))
})

#now we have a new version of the model only with mb_job --> other two were dropped because of high MC and because they give less individual explanatory power according to ANOVA
model_b4 <- lm(diff_annual_norm ~ wealth_income_avg + mb_job +
                 conditionality_f + direct_cash_f +
                 ncb_infrastructure + ncb_microfinance + ncb_health +
                 ncb_livelihood + ncb_water +
                 employment_f + education_f +
                 ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                 ea_agriculture + ea_fishing + ea_tree_planting,
               data = data)

vif(model_b4)

# Create numeric conditionality indicator used below
data <- data %>%
  mutate(
    conditionality = case_when(
      monetary_benefit_type_conditional %in% c("conditional", "both") ~ 1,
      monetary_benefit_type_conditional == "non-conditional" ~ 0,
      TRUE ~ 0
    )
  )
#direct cash and conditionality still have high MC
#create combined variable so as to not drop either
data <- data %>%
  mutate(cash_conditionality = case_when(
    direct_cash_yn == "yes" & conditionality == 1 ~ "Conditional_cash",
    direct_cash_yn == "yes" & conditionality == 0 ~ "Unconditional_cash",
    direct_cash_yn == "no" ~ "No_cash",
    TRUE ~ "ND"
  ) %>% factor(levels = c("No_cash", "Conditional_cash", 
                          "Unconditional_cash", "ND")))

table(data$cash_conditionality)

#try new model
model_b4_final <- lm(diff_annual_norm ~ wealth_income_avg + mb_job+
                       cash_conditionality +
                       ncb_infrastructure + ncb_microfinance + ncb_health +
                       ncb_livelihood + ncb_water +
                       employment_f + education_f +
                       ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                       ea_agriculture + ea_fishing + ea_tree_planting,
                     data = data)

vif(model_b4_final)
table(data$mb_job, data$cash_conditionality)
# Base model without either mb_job or cash_conditionality - see which one matters most since high MC
model_base_clean <- lm(diff_annual_norm ~ wealth_income_avg +
                         ncb_infrastructure + ncb_microfinance + ncb_health +
                         ncb_livelihood + ncb_water +
                         employment_f + education_f +
                         ea_processing + ea_agroforestry + ea_microenterprise + 
                         ea_ecotourism + ea_agriculture + ea_fishing + ea_tree_planting,
                       data = data)

# Test each separately
anova(model_base_clean, update(model_base_clean, ". ~ . + mb_job"))
anova(model_base_clean, update(model_base_clean, ". ~ . + cash_conditionality")) #lower p-value
#lets do backwards variable selection

model_b4_final <- lm(diff_annual_norm ~ wealth_income_avg + cash_conditionality +
                       ncb_infrastructure + ncb_microfinance + ncb_health +
                       ncb_livelihood + ncb_water +
                       employment_f + education +
                       ea_processing + ea_agroforestry + ea_microenterprise + 
                       ea_ecotourism + ea_agriculture + ea_fishing + ea_tree_planting,
                     data = data)

vif(model_b4_final)
model_b4_step <- step(model_b4_final, direction = "backward")
summary(model_b4_step)

#add fixed effects
model_b4_final_fe <- lm(diff_annual_norm ~ wealth_income_avg + cash_conditionality +
                          ncb_infrastructure + ncb_microfinance + ncb_health +
                          ncb_livelihood + ncb_water +
                          employment_f + education_f +
                          ea_processing + ea_agroforestry + ea_microenterprise + 
                          ea_ecotourism + ea_agriculture + ea_fishing + ea_tree_planting 
                        +
                          relevel(as.factor(Country_group), ref = "Brazil"),
                        data = data)

model_b4_step_fe <- step(model_b4_final_fe, 
                         direction = "backward",
                         scope = list(lower = ~ relevel(as.factor(Country_group), ref = "Brazil")))
summary(model_b4_step_fe)
#checking OLS
shapiro.test(residuals(model_b4_step_fe)) #report this one
hist(residuals(model_b4_step_fe)) #check this out
qqnorm(residuals(model_b4_step_fe)) #report this
qqline(residuals(model_b4_step_fe)) #this goes with one above


x_b4 <- model.matrix(~ wealth_income_avg +cash_conditionality +
                       ncb_infrastructure + ncb_microfinance + ncb_health +
                       ncb_livelihood + ncb_water +
                       employment_f + education +
                       ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                       ea_agriculture + ea_fishing + ea_tree_planting +relevel(as.factor(Country_group), ref = "Brazil"),
                     data = data)[, -1]
y_b4_ols <- data$diff_annual_norm
lasso_b4_ols <-cv.glmnet(x_b4,y_b4_ols, alpha=1, family="gaussian",standardize=TRUE, penalty.factor = make_penalty_factors(x_b4))
coef(lasso_b4_ols, s="lambda.min")

ridge_b4_ols <-cv.glmnet(x_b4,y_b4_ols, alpha=0, family="gaussian",standardize=TRUE, penalty.factor = make_penalty_factors(x_b4))
coef(ridge_b4_ols, s = "lambda.min")

#Interaction#
#direct cash x wealth
model_cash_factor_wealth <- lm(diff_annual_norm ~ direct_cash_factor * wealth_income_avg+
                               relevel(as.factor(Country_group), ref = "Brazil"),
                               data = data)
summary(model_cash_factor_wealth)

# look at interaction with direct cash and wealth; and then does it hold up when you add other values within the category
model_cash_factor_wealth_int <- lm(diff_annual_norm ~ direct_cash_factor * wealth_income_avg
                               + conditionality_f+employment_f +education_f +mb_PES + mb_job + mb_carbon
                               +ncb_infrastructure + ncb_microfinance + ncb_health +
                                 ncb_livelihood + ncb_water + ea_processing + ea_agroforestry + ea_microenterprise + ea_ecotourism +
                                 ea_agriculture + ea_fishing + ea_tree_planting
                               +relevel(as.factor(Country_group), ref = "Brazil"),
                               data = data)
summary(model_cash_factor_wealth_int) #holds up under normal standard errors
coeftest(model_cash_factor_wealth_int, vcov = vcovHC(model_cash_factor_wealth_int, type = "HC3")) #doesn't hold up under conservative HC3

#lets re-plot with direct_cash as a factor and diff_annual_norm

wealth_seq <- seq(min(data$wealth_income_avg, na.rm = TRUE),
                  max(data$wealth_income_avg, na.rm = TRUE),
                  length.out = 100)
#finding where lines intersect
b <- coef(model_cash_factor_wealth)

# Intercept and slope for "No direct cash"
a_no <- b["(Intercept)"]
m_no <- b["wealth_income_avg"]

# Intercept and slope for "Direct cash"
a_yes <- b["(Intercept)"] + b["direct_cash_factorYes"]
m_yes <- b["wealth_income_avg"] +
  b["direct_cash_factorYes:wealth_income_avg"]

# Wealth score (X) where the two lines intersect
x_intersection <- (a_yes - a_no) / (m_no - m_yes)

x_intersection

# Predictions with confidence intervals - using factor levels for direct_cash
pred_no_cash <- predict(model_cash_factor_wealth,
                        newdata = data.frame(
                          wealth_income_avg = wealth_seq,
                          direct_cash_factor = factor("No", levels = c("No", "Yes", "ND")),
                          Country_group = "Brazil"),
                        interval = "confidence")

pred_cash <- predict(model_cash_factor_wealth,
                     newdata = data.frame(
                       wealth_income_avg = wealth_seq,
                       direct_cash_factor = factor("Yes", levels = c("No", "Yes", "ND")),
                       Country_group = "Brazil"),
                     interval = "confidence")

# Plot
plot(wealth_seq, pred_no_cash[, "fit"],
     type = "l", col = "blue", lwd = 2,
     ylim = range(c(pred_no_cash, pred_cash)),
     xlab = "Wealth/income (1-5)",
     ylab = "Predicted Forest Loss (% of project area)")

polygon(c(wealth_seq, rev(wealth_seq)),
        c(pred_no_cash[, "lwr"], rev(pred_no_cash[, "upr"])),
        col = adjustcolor("blue", alpha.f = 0.2), border = NA)

lines(wealth_seq, pred_cash[, "fit"], col = "red", lwd = 2)

polygon(c(wealth_seq, rev(wealth_seq)),
        c(pred_cash[, "lwr"], rev(pred_cash[, "upr"])),
        col = adjustcolor("red", alpha.f = 0.2), border = NA)

abline(h = 0, lty = 2, col = "grey")

legend("topleft",
       legend = c("No direct cash", "Direct cash"),
       col = c("blue", "red"),
       lwd = 2)

################Thematic Block 4 - Project design ###############
table(data$tp_community) #reject
table(data$obj_return)#reject
table(data$std_FSC) #reject
table(data$std_VCS) #reject
table(data$obj_conservation) #reject

data <- data %>%
  mutate(vcs_methodology = relevel(as.factor(`VCS Methodology`), ref = "VM0015")) #important because it's most frequent

model_b5 <- lm(diff_annual ~ std_CCB + obj_forest_production + obj_social + 
                 obj_climate + local_developer + tp_private + tp_ngo + tp_public +
                 vcs_methodology,
               data = data)

summary(model_b5)
vif(model_b5)

#check for MC looking at chi-squared values. I see some over 0.4 (below 0.5) but they are all theoretically distinct and VIF is not too high
vars_b5 <- c("std_CCB", "obj_forest_production", "obj_social", "obj_climate",
             "local_developer", "tp_private", "tp_ngo", "tp_public")

chi_data_b5 <- data %>%
  select(all_of(vars_b5)) %>%
  mutate(across(everything(), as.factor))

var_pairs_b5 <- combn(vars_b5, 2, simplify = FALSE)

chi_results_b5 <- map_dfr(var_pairs_b5, function(pair) {
  tbl <- table(chi_data_b5[[pair[1]]], chi_data_b5[[pair[2]]])
  test <- chisq.test(tbl, simulate.p.value = TRUE)
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl)) - 1
  cramers_v <- round(sqrt(test$statistic / (n * k)), 3)
  tibble(var1 = pair[1], var2 = pair[2],
         chi_sq = round(test$statistic, 3),
         p_value = round(test$p.value, 4),
         cramers_v = cramers_v)
})

chi_results_b5 %>%
  select(var1, var2, cramers_v) %>%
  bind_rows(rename(., var1 = var2, var2 = var1)) %>%
  pivot_wider(names_from = var2, values_from = cramers_v) %>%
  column_to_rownames("var1")

#back to variable selection
model_b5_step <- step(model_b5, direction = "backward")
summary(model_b5_step)

#add fixed effects
model_b5_fe <- lm(diff_annual_norm ~ std_CCB + obj_forest_production + obj_social + 
                    obj_climate + local_developer + tp_private + tp_ngo + tp_public +
                    vcs_methodology +
                    relevel(as.factor(Country_group), ref = "Brazil"),
                  data = data)

model_b5_step_fe <- step(model_b5_fe, 
                         direction = "backward",
                         scope = list(lower = ~ relevel(as.factor(Country_group), ref = "Brazil")))
summary(model_b5_step_fe)
summary(model_b5_step)

#checking OLS assumptions
shapiro.test(residuals(model_b5_step_fe)) #report this one
hist(residuals(model_b5_step_fe)) #check this out
qqnorm(residuals(model_b5_step_fe)) #report this
qqline(residuals(model_b5_step_fe)) #this goes with one above

x_b5 <- model.matrix(~ std_CCB + obj_forest_production + obj_social + 
                       obj_climate + local_developer + tp_private + 
                       tp_ngo + tp_public + vcs_methodology +relevel(as.factor(Country_group), ref = "Brazil"),
                     data = data)[, -1]
y_b5_ols <- data$diff_annual_norm

lasso_b5_ols <- cv.glmnet(x_b5, y_b5_ols, alpha = 1, family = "gaussian",standardize=TRUE, penalty.factor = make_penalty_factors(x_b5))
coef(lasso_b5_ols, s = "lambda.min")

ridge_b5_ols <- cv.glmnet(x_b5, y_b5_ols, alpha = 0, family = "gaussian", standardize=TRUE, penalty.factor = make_penalty_factors(x_b5))
coef(ridge_b5_ols, s = "lambda.min")

#figuring out what's going on

#i'm curious-for successful projects with tp_ngo, how many have CCB certifications
data_success <- data %>%
  filter(diff_annual < 0)

data_success %>%
  filter(tp_ngo == TRUE) %>%
  select(ID, Country, tp_ngo, std_CCB)

#lets see if there is location bias playing out in tp_NGO projects
data %>%
  group_by(tp_ngo) %>%
  summarise(across(c(dist_water_km, dist_road_km, dist_urban_km, dist_urban_new_km), 
                   median, na.rm = TRUE))
table(data$tp_ngo, data$vcs_methodology)
#check to see if there are any systematic differences in location in tp_NGO projects
kruskal.test(dist_road_km~tp_ngo, data=data)
kruskal.test(dist_urban_km~tp_ngo, data=data)
kruskal.test(dist_urban_new_km~tp_ngo, data=data)


####### Combined Model #############
combo <- lm(diff_annual_norm~ dist_water_km + rights_public_state + 
              tp_ngo +
              #direct_cash_factor *wealth_income_avg+
              relevel(as.factor(Country_group), ref = "Brazil"), data = data )
summary(combo)
#look at residuals of combined model
shapiro.test(residuals(combo)) #report this one
hist(residuals(combo)) #check this out
qqnorm(residuals(combo)) #report this
qqline(residuals(combo)) #this goes with one above

#run anova to see
combo_no_int <- lm(diff_annual_norm ~ dist_water_km + rights_public_state + 
                     tp_ngo +
                     relevel(as.factor(Country_group), ref = "Brazil"), data = data)
combo_int <- lm(diff_annual_norm ~ dist_water_km + rights_public_state + 
                  tp_ngo +
                  direct_cash_factor * wealth_income_avg +
                  relevel(as.factor(Country_group), ref = "Brazil"), data = data)

# ANOVA test comparing the two models
anova(combo_no_int, combo_int)
summary(combo_int)

#look at residuals of combined model
shapiro.test(residuals(combo_int)) #report this one
hist(residuals(combo_int)) #check this out
qqnorm(residuals(combo_int)) #report this
qqline(residuals(combo_int)) #this goes with one above


########## Running PCA/FAMD ###############
#running FAMD
continuous_vars <- c(
  "wealth_income_avg",
  "dist_road_km",
  "dist_urban_km",
  "dist_urban_new_km",
  "dist_water_km"
)

famd_vars <- data %>%
  select(
    # Block 1 - Land Pressure
    dist_road_km, dist_urban_km, dist_urban_new_km, dist_water_km,
    driver_illegal_logging, driver_smallholder_ag, driver_industrial_ag,
    driver_livestock, driver_wood_extraction, driver_infrastructure,
    driver_mining, driver_other, driver_local_livelihoods,
    
    # Block 2 - Governance
    fpic_f, protected_area_f, contested_f,
    cu_private, cu_communities, cu_state,
    lt_public, lt_private, lt_communities,
    tenure,
    
    # Block 3 - Socioeconomic
    wealth_income_avg, mb_PES, mb_job, mb_carbon,
    conditionality_f, direct_cash_f,
    ncb_infrastructure, ncb_microfinance, ncb_health,
    ncb_livelihood, ncb_water,
    employment_f, education,
    ea_processing, ea_agroforestry, ea_microenterprise, ea_ecotourism,
    ea_agriculture, ea_fishing, ea_tree_planting,
    
    # Block 4 - Design
    std_CCB, obj_forest_production, obj_social,
    obj_climate, local_developer, tp_private, tp_ngo, tp_public,
    vcs_methodology
  )

#convert non-continuous variables to factors
famd_vars <- famd_vars %>%
  mutate(
    across(
      -all_of(continuous_vars),
      as.factor
    )
  )

str(famd_vars)
library(FactoMineR)
library(factoextra)
library(dplyr)

# remove rows with missing values - none have NA
famd_vars_clean <- famd_vars %>%
  na.omit()

# remove variables with no variation
famd_vars_clean <- famd_vars_clean %>%
  select(where(~ length(unique(.)) > 1))

# run FAMD
famd_result <- FAMD(famd_vars_clean, graph = FALSE)
summary(famd_result)
fviz_screeplot(famd_result, addlabels = TRUE)
fviz_famd_var(famd_result, repel = TRUE)
sort(famd_result$var$contrib[, 1], decreasing = TRUE)[1:20]
sort(famd_result$var$contrib[,2], decreasing = TRUE)[1:20]

# Make factor levels unique by adding the variable name to each level
famd_vars_clean2 <- famd_vars_clean %>%
  mutate(
    across(
      where(is.factor),
      ~ factor(paste0(cur_column(), "_", as.character(.)))
    )
  )

# Re-run FAMD
famd_result2 <- FAMD(famd_vars_clean2, graph = FALSE)
fviz_famd_var(
  famd_result2,
  repel = TRUE
)
sort(famd_result2$var$contrib[, 1], decreasing = TRUE)[1:20]
sort(famd_result2$var$contrib[, 2], decreasing = TRUE)[1:20]

###########Model Diagnostics##########
vif(model_b2_step_fe)
vif(model_b3_step_fe)
#vif(model_b4_step_fe)
vif(model_b5_step_fe)
vif(combo)
vif(combo_int)
bptest(model_b2_step_fe)
bptest(model_b3_step_fe)
bptest(model_b4_step_fe) 
bptest(model_b5_step_fe)
bptest(combo)
bptest(combo_int)
summary(model_b2_step_fe) 
summary(model_b3_step_fe) 
summary(model_b4_step_fe) 
summary(model_b5_step_fe) 
summary(combo)
summary(combo_int)
library(sandwich)
coeftest(model_b2_step_fe, vcov = vcovHC(model_b2_step_fe, type = "HC0"))
coeftest(model_b2_step_fe, vcov = vcovHC(model_b2_step_fe, type = "HC3"))

coeftest(model_b3_step_fe, vcov = vcovHC(model_b3_step_fe, type = "HC0"))
coeftest(model_b3_step_fe, vcov = vcovHC(model_b3_step_fe, type = "HC3"))

coeftest(model_b4_step_fe, vcov = vcovHC(model_b4_step_fe, type = "HC0"))
coeftest(model_b4_step_fe, vcov = vcovHC(model_b4_step_fe, type = "HC3"))

coeftest(model_b5_step_fe, vcov = vcovHC(model_b5_step_fe, type = "HC0"))
coeftest(model_b5_step_fe, vcov = vcovHC(model_b5_step_fe, type = "HC3"))

coeftest(combo, vcov = vcovHC(combo, type = "HC0"))
coeftest(combo, vcov = vcovHC(combo, type = "HC3"))

coeftest(combo_int, vcov = vcovHC(combo_int, type = "HC0"))
coeftest(combo_int, vcov = vcovHC(combo_int, type = "HC3"))

names(data)

#### Addressing Limitation - Varying start date between 2000-2022 ####
start_year <- data$Start_year
combo_start <- lm(diff_annual_norm~ dist_water_km + rights_public_state + 
              tp_ngo + start_year+
              relevel(as.factor(Country_group), ref = "Brazil"), data = data )
summary(combo_start)


### Making Figures #####
#Map - Figure 1
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70", linewidth = 0.2) +
  geom_point(data = data, aes(x = longitude, y = latitude), size = 0.02) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")


#Bar plot - Figure 2
barplot(data$diff_annual_norm,
        names.arg = data$ID,
        las=2,
        cex.names=0.4,
        col = ifelse(data$diff_annual_norm<0, "blue","red"),
        ylab="Annual Difference (% of project area)",
        xlab="Project ID")
legend("bottomright", legend = c("Additional Project", "Not Additional Project"),
       col = c("blue", "red"), lwd=2, cex=0.8)

#Bar Plot - Figure 3
data_success %>%
  mutate(local_developer = as.factor(local_developer),
         local_developer = ifelse(local_developer == "TRUE", "Yes", "No")) %>%
  mutate(Country_group = ifelse(Country_group == "Rest", "Africa & Asia", Country_group)) %>%
  select(Country_group, fpic_f, direct_cash_f,local_developer
         #conditionality_f, direct_cash_f, employment_f, education_f, local_developer
  ) %>%
  pivot_longer(-Country_group,
               names_to = "variable",
               values_to = "value") %>%
  mutate(
    variable = dplyr::case_when(
      variable == "direct_cash_f" ~ "Direct Cash",
      variable == "fpic_f" ~ "FPIC",
      variable == "local_developer" ~ "Local Developer",
      TRUE ~ variable
    ),
    value = as.character(value),
    value = dplyr::case_when(
      value %in% c("yes", "YES", "TRUE") ~ "Yes",
      value %in% c("no", "NO", "FALSE") ~ "No",
      value %in% c("nd", "ND") ~ "ND",
      TRUE ~ value
    ),
    value = factor(value, levels = c("Yes", "No", "ND"))
  )%>%
  filter(!is.na(value)) %>%
  group_by(Country_group, variable, value) %>%
  summarise(n = n()) %>%
  group_by(Country_group, variable) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = Country_group, y = proportion, fill = value)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("ND" = "darkgreen",
                               "Yes" = "blue",
                               "No" = "red" 
  )
  ) +
  facet_wrap(~variable, ncol = 4) + #number here tells you how many in a row you want
  labs(x = "Country Group", y = "% of Additional Projects",
       fill = "",)+
  #title = "Block 2 Variables Among Successful Projects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Descriptive Analysis Tests ####
library(dunn.test)

# Shapiro-Wilk tests for continuous variables.
# A p-value below 0.05 indicates evidence against normality
shapiro.test(data$dist_water_km)
shapiro.test(data$dist_road_km)
shapiro.test(data$dist_urban_km)
shapiro.test(data$dist_urban_new_km)
shapiro.test(data$wealth_income_avg)

# Block 1 Exploration
dist_vars <- c("dist_water_km", "dist_road_km", "dist_urban_km", "dist_urban_new_km")
for(var in dist_vars) {
  cat("\n---", var, "---\n")
  kruskal.test(data[[var]] ~ data$Country_group)
  dunn.test(data[[var]], 
            data$Country_group,
            method = "bonferroni")
}

driver_vars <- c("driver_smallholder_ag", "driver_industrial_ag",
                 "driver_livestock", "driver_wood_extraction", 
                 "driver_illegal_logging", "driver_infrastructure",
                 "driver_mining", "driver_oil_extraction",
                 "driver_local_livelihoods", "driver_other")

for(var in driver_vars) {
  cat("\n---", var, "---\n")
  print(chisq.test(table(data[[var]], data$Country_group)))
}
table(data$driver_oil_extraction, data$Country_group)

#gives you median values
data %>%
  group_by(Country_group) %>%
  summarise(median_dist_urban = median(dist_urban_km, na.rm = TRUE))

#Block 2 Exploration
gov_vars <- c("fpic_f", "protected_area_f", "contested_f",
              "cu_private","cu_communities" , "cu_state",
              "lt_public" , "lt_private" , "lt_communities", "tenure")

for(var in gov_vars) {
  cat("\n---", var, "---\n")
  print(chisq.test(table(data[[var]], data$Country_group)))
}

#Block 3 Exploration
kruskal.test(data$wealth_income_avg ~ data$Country_group)
dunn.test(data$wealth_income_avg, data$Country_group, method="bonferroni")

se_vars <- c("mb_PES", "mb_job", "mb_carbon",
             "conditionality_f", "direct_cash_f",
             "ncb_infrastructure", "ncb_microfinance", "ncb_health",
             "ncb_livelihood", "ncb_water",
             "employment_f", "education_f",
             "ea_processing", "ea_agroforestry", "ea_microenterprise", 
             "ea_ecotourism", "ea_agriculture", "ea_fishing", 
             "ea_tree_planting")

for(var in se_vars) {
  cat("\n---", var, "---\n")
  print(chisq.test(table(data[[var]], data$Country_group)))
}

#Block 4
design_vars <- c("std_CCB" , "obj_forest_production", "obj_social" , 
                 "obj_climate" ,"local_developer" , "tp_private" , "tp_ngo" , "tp_public" ,
                 "vcs_methodology")
for(var in design_vars) {
  cat("\n---", var, "---\n")
  print(chisq.test(table(data[[var]], data$Country_group)))
}

# now let's just look at percent successes in each country group
data %>%
  group_by(Country_group) %>%        # group data by country group
  summarise(
    n_total = n(),                    # count total projects in each group
    n_success = sum(deforestation_binary, na.rm = TRUE),  # sum the 1s (successes) - works because binary is 0/1
    pct_success = round(n_success / n_total * 100, 1),     # divide successes by total, multiply by 100 for percentage, round to 1 decimal
    percent_diff=round(mean(diff_annual_norm, na.rm=TRUE), 3),
  )
