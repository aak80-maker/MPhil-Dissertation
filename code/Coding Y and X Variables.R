#The data sets loaded here can be found in data/intermediary
#Relevant data sets are: 'data_x_variables.csv', 'data_y_variable_ha.csv', 'VCS Methodology.csv'
#These data set were created from the original data sets found in data/original

library(tidyverse)
#install.packages(c("tidyverse", "ggplot2", "rnaturalearth", "rnaturalearthdata", "sf"))
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(corrplot)
#install.packages(c("corrplot"))

#loading x variables

data<-read_csv("data_x_variables.csv")
data <- data %>% rename(index = ID)
data <- data %>% rename(ID = VCS)
data <- data %>% mutate(ID = as.character(ID))

#lets add VCS methodologies to this
methodologies <-read_csv("VCS Methodology.csv")
methodologies <- methodologies %>% mutate(ID = as.character(ID))

data_all <- data %>%
  left_join(methodologies, by = "ID")

#now lets just load the forest loss data
ha <- read_csv('data_y_variable_ha.csv')
#lets remove the all NA rows
ha <- ha %>%
  filter(!if_all(everything(), is.na))

#now lets create 2 categories for binary logit, for cumulative deforestation
library(dplyr)

ha <- ha %>%
  mutate(
    deforestation_outcome = case_when(
      `Difference Cumulative (ha)` > 0 ~ "Failure", #when observed forest loss is greater than the counterfactual 
      `Difference Cumulative (ha)` < 0 ~ "Success", #when observed forest loss is less than the counterfactual 
      TRUE ~ NA_character_
    ),
    
    deforestation_outcome = factor(
      deforestation_outcome,
      levels = c(
        "Success",
        "Failure"
      )
    )
  )

# lets merge them into one, first change column names in ha and credits
ha <- ha %>% rename(ID = `Tang et al./VCS (Verra) ID Number`)
#credits <- credits %>% rename(ID = `Tang et al./VCS (Verra) ID Number`)
ha <- ha %>% mutate(ID = as.character(ID))
#credits <- credits %>% mutate(ID = as.character(ID))

#this gives us merged data set with x and y together
merged <- data_all %>%
  left_join(ha, by = "ID")
  

# lets visualise the data a bit

#we made deforestation outcome a 1 or a 0 based on whether or not it exceeded SC baseline
merged <- merged %>%
  mutate(
    deforestation_binary = ifelse(
      deforestation_outcome == "Success",
      1,
      0
    )
  )


#Here i want to run Cramer's V test to understand relationship between my categorical variables
install.packages("rcompanion")
library(rcompanion)
library(tidyr)

#cramerV(table(merged$deforestation_driver, merged$deforestation_binary))
#table(merged$deforestation_driver, merged$deforestation_binary)


# Get all unique individual drivers (split by semicolon first)
library(tidyr)
library(dplyr)

merged %>%
  separate_rows(deforestation_driver, sep = ";") %>%
  mutate(deforestation_driver = trimws(deforestation_driver)) %>%
  distinct(deforestation_driver) %>%
  arrange(deforestation_driver)

library(rcompanion)

# here we merge some of the categories into 10
merged <- merged %>%
  mutate(
    driver_smallholder_ag = grepl("transient agriculture|slash and burn|^agriculture$", 
                                  deforestation_driver, ignore.case = TRUE),
    driver_industrial_ag = grepl("industrial agriculture", 
                                 deforestation_driver, ignore.case = TRUE),
    driver_livestock = grepl("cattle grazing|cattle ranching|industrial agriculture or cattle ranching", 
                             deforestation_driver, ignore.case = TRUE),
    driver_wood_extraction = grepl("charcoal production|energy wood|industrial wood exploitation", 
                                   deforestation_driver, ignore.case = TRUE),
    driver_illegal_logging = grepl("illegal logging", 
                                   deforestation_driver, ignore.case = TRUE),
    driver_infrastructure = grepl("infrastructure", 
                                  deforestation_driver, ignore.case = TRUE),
    driver_mining = grepl("mining", 
                          deforestation_driver, ignore.case = TRUE),
    driver_oil_extraction = grepl("oil extraction", 
                                  deforestation_driver, ignore.case = TRUE),
    driver_local_livelihoods = grepl("local livelihoods", 
                                     deforestation_driver, ignore.case = TRUE),
    driver_other = grepl("other land speculation|other policy|population increase|fire", 
                         deforestation_driver, ignore.case = TRUE)
  )

# drivers <- c("driver_smallholder_ag", "driver_industrial_ag", "driver_livestock",
#              "driver_wood_extraction", "driver_illegal_logging", "driver_infrastructure",
#              "driver_mining", "driver_oil_extraction", "driver_local_livelihoods",
#              "driver_other")

#lets do this for other x variables too
unique(merged$Customary_use)
unique(merged$legal_tenure)
unique(merged$monetary_benefit_type...15)
merged <- merged %>% rename(monetary_benefit_type = `monetary_benefit_type...15`)
merged <- merged %>% rename(monetary_benefit_type_conditional = `monetary_benefit_type...14`)

merged <- merged %>%
  mutate(
    # Customary use
    cu_private = grepl("private", Customary_use, ignore.case = TRUE),
    cu_communities = grepl("communities", Customary_use, ignore.case = TRUE),
    cu_state = grepl("state", Customary_use, ignore.case = TRUE),
    
    # Legal tenure
    lt_public = grepl("state|public", legal_tenure, ignore.case = TRUE),
    lt_private = grepl("private", legal_tenure, ignore.case = TRUE),
    lt_communities = grepl("communities", legal_tenure, ignore.case = TRUE),
    
    # Monetary benefit type
    mb_PES = grepl("PES", monetary_benefit_type, ignore.case = TRUE),
    mb_job = grepl("Job", monetary_benefit_type, ignore.case = TRUE),
    mb_carbon = grepl("Carbon revenue sharing", monetary_benefit_type, ignore.case = TRUE),
    mb_pension = grepl("Pension", monetary_benefit_type, ignore.case = TRUE),
    mb_grant = grepl("Grant", monetary_benefit_type, ignore.case = TRUE),
    mb_ecotourism = grepl("Ecotourism", monetary_benefit_type, ignore.case = TRUE),
    mb_indirect = grepl("Indirect income", monetary_benefit_type, ignore.case = TRUE),
    mb_none = grepl("None", monetary_benefit_type, ignore.case = TRUE)
  )

# vars <- c("cu_private", "cu_communities", "cu_state",
#           "lt_state", "lt_private", "lt_communities", "lt_public",
#           "mb_PES", "mb_job", "mb_carbon", "mb_pension", 
#           "mb_grant", "mb_ecotourism", "mb_indirect", "mb_none")
# 

# now lets do for a few more x variables
unique(merged$non_cash_benefits_type)
unique(merged$economic_activities_list)
unique(merged$standards)
unique(merged$project_objectives)

# Get unique individual values for each column
merged %>%
  separate_rows(non_cash_benefits_type, sep = ";") %>%
  mutate(non_cash_benefits_type = trimws(non_cash_benefits_type)) %>%
  distinct(non_cash_benefits_type)

merged %>%
  separate_rows(economic_activities_list, sep = ";") %>%
  mutate(economic_activities_list = trimws(economic_activities_list)) %>%
  distinct(economic_activities_list)

merged %>%
  separate_rows(standards, sep = ";") %>%
  mutate(standards = trimws(standards)) %>%
  distinct(standards)

merged %>%
  separate_rows(project_objectives, sep = ";") %>%
  mutate(project_objectives = trimws(project_objectives)) %>%
  distinct(project_objectives)

merged <- merged %>%
  mutate(
    # Non-cash benefits
    ncb_training = grepl("Training & Capacity Building", non_cash_benefits_type, ignore.case = TRUE),
    ncb_infrastructure = grepl("Infrastructure & Equipment", non_cash_benefits_type, ignore.case = TRUE),
    ncb_microfinance = grepl("Microfinance & Enterprise Support", non_cash_benefits_type, ignore.case = TRUE),
    ncb_health = grepl("Health Services", non_cash_benefits_type, ignore.case = TRUE),
    ncb_livelihood = grepl("Agricultural & Livelihood Support", non_cash_benefits_type, ignore.case = TRUE),
    ncb_land_rights = grepl("Legal & Land Rights", non_cash_benefits_type, ignore.case = TRUE),
    ncb_water = grepl("Water & Sanitation", non_cash_benefits_type, ignore.case = TRUE),
    
    # Economic activities (merged)
    ea_processing = grepl("processing and commercialization", economic_activities_list, ignore.case = TRUE),
    ea_agroforestry = grepl("agroforestry|silvopastoral", economic_activities_list, ignore.case = TRUE),
    ea_microenterprise = grepl("microenterprise|micro-credits|economic interest groups", economic_activities_list, ignore.case = TRUE),
    ea_ecotourism = grepl("ecotourism", economic_activities_list, ignore.case = TRUE),
    ea_timber = grepl("plantation forestry|sustainable timber harvesting", economic_activities_list, ignore.case = TRUE),
    ea_agriculture = grepl("agriculture", economic_activities_list, ignore.case = TRUE),
    ea_cookstoves = grepl("fuel efficient stoves|fuel efficient cookstoves", economic_activities_list, ignore.case = TRUE),
    ea_ntfp = grepl("ntfp", economic_activities_list, ignore.case = TRUE),
    ea_fishing = grepl("fishing", economic_activities_list, ignore.case = TRUE),
    ea_tree_planting = grepl("tree planting", economic_activities_list, ignore.case = TRUE),
    ea_mining = grepl("sustainable mining", economic_activities_list, ignore.case = TRUE),
    
    # Standards
    std_VCS = grepl("VCS", standards, ignore.case = TRUE),
    std_CCB = grepl("CCB", standards, ignore.case = TRUE),
    std_FSC = grepl("FSC", standards, ignore.case = TRUE),
    
    # Project objectives (merged)
    obj_forest_production = grepl("timber production|non timber production|participatory forest management", project_objectives, ignore.case = TRUE),
    obj_social = grepl("social development|community|indigenous peoples", project_objectives, ignore.case = TRUE),
    obj_conservation = grepl("biodiversity conservation|ecosystem restoration", project_objectives, ignore.case = TRUE),
    obj_climate = grepl("climate", project_objectives, ignore.case = TRUE),
    obj_return = grepl("return on investment", project_objectives, ignore.case = TRUE)
  )

# 
# new_vars <- c("ncb_training", "ncb_infrastructure", "ncb_microfinance", "ncb_health",
#               "ncb_livelihood", "ncb_land_rights", "ncb_water",
#               "ea_processing", "ea_agroforestry", "ea_microenterprise", "ea_ecotourism",
#               "ea_timber", "ea_agriculture", "ea_cookstoves", "ea_ntfp",
#               "ea_fishing", "ea_tree_planting", "ea_mining",
#               "std_VCS", "std_CCB", "std_FSC",
#               "obj_forest_production", "obj_social", "obj_conservation",
#               "obj_climate", "obj_forest_mgmt", "obj_return")
# 


# lets try for partner information
head(merged$Country)
head(merged$nationality)
unique(merged$type_partner)

#here we generate a new column called local_developer, where we have a TRUE if one of 
#the project developers has the same nationality as the country where the project is located
merged <- merged %>%
  mutate(local_developer = mapply(function(country, nat) {
    grepl(country, nat, ignore.case = TRUE)
  }, Country, nationality))

#here we create dummy columns for type of partner
merged <- merged %>%
  mutate(
    tp_private = grepl("private for.profit", type_partner, ignore.case = TRUE),
    tp_ngo = grepl("ngo|non for profit", type_partner, ignore.case = TRUE),
    tp_public = grepl("public", type_partner, ignore.case = TRUE),
    tp_community = grepl("local communities|landowners|cooperatives", type_partner, ignore.case = TRUE)
  )

#lets look at remaining x variables
unique(merged$FPIC)
unique(merged$`Protected Area`)
unique(merged$`Forest Type`)
unique(merged$Contested)
unique(merged$direct_cash_yn)
unique(merged$employment_yn)
unique(merged$tenure_type)
unique(merged$education_yn)
unique(merged$standards)

merged <- merged %>%
  mutate(
    # Binary yes/no variables (ND → NA)
    fpic = case_when(FPIC == "yes" ~ 1, FPIC == "no" ~ 0, TRUE ~ NA_real_),
    protected_area = case_when(`Protected Area` == "yes" ~ 1, 
                               `Protected Area` == "no" ~ 0, TRUE ~ NA_real_),
    contested = case_when(Contested == "yes" ~ 1, 
                          Contested == "no" ~ 0, TRUE ~ NA_real_),
    direct_cash = case_when(direct_cash_yn == "yes" ~ 1, 
                            direct_cash_yn == "no" ~ 0, TRUE ~ NA_real_),
    employment = case_when(employment_yn == "yes" ~ 1, 
                           employment_yn == "no" ~ 0, TRUE ~ NA_real_),
    education = case_when(education_yn == "yes" ~ 1, 
                          education_yn == "no" ~ 0, TRUE ~ NA_real_),
    
    # Forest type dummies
    ft_humid = grepl("humid", `Forest Type`, ignore.case = TRUE),
    ft_dry = grepl("dry", `Forest Type`, ignore.case = TRUE),
    ft_other = grepl("other", `Forest Type`, ignore.case = TRUE),
    
    # Tenure type dummies
    tt_titling = grepl("Land titling & tenure security", tenure_type, ignore.case = TRUE),
    tt_planning = grepl("Land use planning & demarcation", tenure_type, ignore.case = TRUE)
  )

#now lets explore correlations
# new_vars3 <- c("fpic", "protected_area", "contested", "direct_cash", 
#                "employment", "education", "ft_humid", "ft_dry", "ft_other",
#                "tt_titling", "tt_planning")
# 