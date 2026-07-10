library(tidyverse)

# clean and prepare the analyis dataset

data<-read_csv("all_data.csv")


# Remove duplicate identifier columns and tidy the dataset structure
data <-data %>%
  select(-iso_code.y) %>%
  relocate(`Name of Project`, .after = 3)

data <-data %>%
  select(-project_name)


# Recode VCS methodology as a factor using the most common methodology (VM0015) as the reference category
table(data$`VCS Methodology`)
data <- data %>%
  mutate(vcs_methodology = relevel(as.factor(`VCS Methodology`), ref = "VM0015"))
data$vcs_methodology

typeof(data$vcs_methodology)

# Verify the recoding
data%>% select(direct_cash,direct_cash_yn) %>% print(n = Inf)

# Identify variables containing "ND" values before recoding
data %>%
  summarise(across(everything(), ~sum(. == "ND", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "nd_count") %>%
  filter(nd_count > 0) %>%
  arrange(desc(nd_count))%>% print(n = Inf)

# Identify the project with a missing certification entry
data %>% filter(standards == "ND") %>% select(ID, `Name of Project`, standards)

# Recode missing certification entry - done by checking Verra registry 
data <- data %>%
  mutate(standards = ifelse(standards == "ND", "VCS;CCB", standards))

#fixed an ND in the standards column
data <- data %>%
mutate(
std_VCS = grepl("VCS", standards, ignore.case = TRUE),
std_CCB = grepl("CCB", standards, ignore.case = TRUE),
std_FSC = grepl("FSC", standards, ignore.case = TRUE))
data %>% select(standards, std_VCS, std_CCB, std_FSC) %>% print(n = Inf)
table(data$std_CCB, data$std_VCS)

# Recreate certification indicator variables after correcting the standards field
data <- data %>%
  mutate(
    vcs_methodology = relevel(as.factor(`VCS Methodology`), ref = "VM0015"),
    forest_type = relevel(as.factor(`Forest Type`), ref = "humid")
  )


# Merge project area data and create size-adjusted outcome variables
# Import project area information
area <- read.csv('project_area.csv')
# Merge project area into the analysis dataset
data <- merge(data, area, by = "ID", all.x = TRUE)
hist(data$Area)
data$log_area <- log(data$Area)
hist(data$log_area)
# Calculate annual avoided deforestation as a percentage of project area
data <- data %>%
  mutate (diff_annual_norm = (diff_annual/Area)*100)
data$diff_annual_norm

plot(data$diff_annual,data$diff_annual_norm)
abline(lm(data$diff_annual_norm~data$diff_annual))

# Save the final analysis-ready dataset
write_csv(data, "Processed Data Master File.csv")

