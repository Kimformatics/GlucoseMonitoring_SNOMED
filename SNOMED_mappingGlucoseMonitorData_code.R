# Load libraries
library(dplyr)

# Load datasets
glucose_data <- read.csv('glucose_monitor_data_100_patients.csv', stringsAsFactors = FALSE)
snomed_ct_data <- read.csv('snomed_ct_mapping.csv', stringsAsFactors = FALSE)

# Create mapping dictionary
snomed_dict <- setNames(snomed_ct_data$SNOMED_CT_Code, snomed_ct_data$Variable)

# Add SNOMED codes to glucose data variables
glucose_data_mapped <- glucose_data %>%
  mutate(
    Glucose_Level_SNOMED = snomed_dict['Glucose Level'],
    Meal_Status_SNOMED = snomed_dict['Meal Status'],
    Insulin_Dose_SNOMED = snomed_dict['Insulin Dose'],
    Exercise_Level_SNOMED = snomed_dict['Exercise Level'],
    HbA1c_SNOMED = snomed_dict['HbA1c']
  )

# Save data
write.csv(glucose_data_mapped, 'SNOMEDmapped_glucoseMonitorData.csv', row.names = FALSE)
print(head(glucose_data_mapped))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Viz
# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load data
mapped_glucose_data <- read_csv("SNOMEDmapped_glucoseMonitorData.csv")

# Histogram of Glucose Levels
ggplot(glucose_data_mapped, aes(x = Glucose_Level_mg_dL)) +
  geom_histogram(bins = 15, fill = "blue", color = "yellow", alpha = 0.7) +
  labs(title = "Distribution of Glucose Levels",
       x = "Glucose Level (mg/dL)",
       y = "Frequency") +
  theme_minimal()

# Boxplot of A1c Status by Meal Status 
ggplot(glucose_data_mapped, aes(x = Meal_Status, y = HbA1c_Percentage, fill = Meal_Status)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(title = "HbA1c Levels by Meal Status",
       x = "Meal Status",
       y = "HbA1c Percentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  


# Line Chart of Glucose Levels Over Time (First 20 Patients Only)
subset_data <- glucose_data_mapped %>% slice(1:20)
ggplot(subset_data, aes(x = Timestamp, y = Glucose_Level_mg_dL, group = 1)) +
  geom_line(color = "red") +
  geom_point(size = 2, color = "black") +
  labs(title = "Glucose Levels Over Time (Sample Patients)",
       x = "Timestamp",
       y = "Glucose Level (mg/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart of A1c Status by Meal Status 
ggplot(glucose_data_mapped, aes(x = Meal_Status, fill = Meal_Status)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Fasting" = "blue", "Postprandial" = "yellow")) +
  labs(title = "Distribution of Meal Status (Fasting vs. Postprandial)",
       x = "Meal Status",
       y = "Count") +
  theme_minimal()
