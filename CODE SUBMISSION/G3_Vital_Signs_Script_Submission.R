#Packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Dataset
Vital_Signs_Data <- read.csv("C:/Users/ASUS/Documents/Portfolio Dataset/Exp 3/1_Vital_signs_diagnosis_data_Group_003.csv")

#Pre-processing Data 
names(Vital_Signs_Data)

clean_data <- Vital_Signs_Data %>%
  filter(!is.na(BMI)) %>%
  mutate(
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 24.9 ~ "Normal",
      BMI >= 25 & BMI < 29.9 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ NA_character_),
    
    Sex_Legend = case_when(
      Sex_Legend == "0 - Female" ~ "Female",
      Sex_Legend == "1 - Male" ~ "Male",
      TRUE ~ NA_character_),
    
    Smoking_status_Legend = case_when(
      Smoking_status_Legend == "non-smoker" ~ "Non-Smoker",
      Smoking_status_Legend == "occasional" ~ "Occasional",
      Smoking_status_Legend == "Chainsmoker" ~ "Chainsmoker",
      Smoking_status_Legend == "" ~ NA_character_,
      TRUE ~ Smoking_status_Legend),
    
    Hypertension_Group = case_when(
      grepl("normal|hypotensive", Hypertension_Legend, ignore.case = TRUE) ~ "No",
      grepl("elevated|hypertension|stage", Hypertension_Legend, ignore.case = TRUE) ~ "Yes",
      is.na(Hypertension_Legend) | Hypertension_Legend == "" ~ "Unknown",
      TRUE ~ "Unknown"),
    
    Physical_Activity_Category = case_when(
      Physical_Activity_Hours_Week < 4 ~ "Low",
      Physical_Activity_Hours_Week >= 4 & Physical_Activity_Hours_Week < 10 ~ "Moderate",
      Physical_Activity_Hours_Week >= 10 ~ "High",
      TRUE ~ NA_character_))

clean_data <- clean_data %>%
  mutate(Physical_Activity_Category = case_when(
    Physical_Activity_Hours_Week < 4 ~ "Low",
    Physical_Activity_Hours_Week >= 4 & Physical_Activity_Hours_Week < 10 ~ "Moderate",
    Physical_Activity_Hours_Week >= 10 ~ "High",
    TRUE ~ NA_character_))

summary(clean_data)

#Descriptive statistics of the dataset (mean, median, standard deviation, etc.)

Vital_Signs_Data <- Vital_Signs_Data %>%
  mutate(
    Systolic_BP = as.numeric(Systolic_BP),
    Diastolic_BP = as.numeric(Diastolic_BP))

descriptive_stats <- Vital_Signs_Data %>%
  summarise(
    mean_BMI = mean(BMI, na.rm = TRUE),
    mean_BP_Sys = mean(Systolic_BP, na.rm = TRUE),
    mean_BP_Dia = mean(Diastolic_BP, na.rm = TRUE),
    mean_Cholesterol = mean(Cholesterol_mg.dL, na.rm = TRUE),
    mean_Glucose = mean(Glucose_mg.dL, na.rm = TRUE),
    mean_Stress = mean(Stress_Level, na.rm = TRUE),
    mean_Activity = mean(Physical_Activity_Hours_Week, na.rm = TRUE))

print(descriptive_stats)

#Density Plot: BMI with WHO Classification
bmi_categories <- data.frame( category = factor(c("Underweight", "Normal", "Overweight", "Obese"),
                                                levels = c("Underweight", "Normal", "Overweight", "Obese")),
                              xmin = c(0, 18.5, 25, 30),
                              xmax = c(18.5, 24.9, 29.9, 60))

ggplot(clean_data, aes(x = BMI)) + geom_rect(data = bmi_categories,
                                             aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf, fill = category),
                                             inherit.aes = FALSE, alpha = 0.2) + geom_density(fill = "steelblue", alpha = 0.4, color = "darkblue") + 
  geom_vline(aes(xintercept = mean(BMI, na.rm = TRUE)), color = "red", linetype = "dashed", linewidth = 1) + labs(
    title = "BMI Density Plot with Risk Classifications",
    subtitle = "WHO-based BMI categories",
    x = "BMI",
    y = "Density",
    fill = "Legend") + 
  scale_fill_manual(values = c(
    "Underweight" = "lightblue",
    "Normal" = "#B5EAD7", 
    "Overweight" = "#FFE066",                  
    "Obese" = "#FEC8D8")) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12))

##ADVANCED INSIGHT | One-sample t-test
t_test_result <- t.test(clean_data$BMI, mu = 25, alternative = "two.sided", na.rm = TRUE)
print(t_test_result)

#Scatter Plot: Physical Activity vs. BMI 
plot_data <- clean_data %>%
  filter(!is.na(Physical_Activity_Hours_Week), !is.na(BMI))

ggplot(plot_data, aes(x = Physical_Activity_Hours_Week, y = BMI)) +
  geom_point(aes(color = BMI_Category), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    title = "Physical Activity vs. BMI",
    x = "Physical Activity (Hours/Week)",
    y = "BMI",
    color = "BMI Category"
  ) +
  scale_color_manual(values = c("Underweight" = "lightblue", "Normal" = "#B5EAD7", "Overweight" = "#FFE066", "Obese" = "#FEC8D8")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

##ADVANCED INSIGHT | Pearson Correlation Test
correlation_result <- cor.test(clean_data$Physical_Activity_Hours_Week, clean_data$BMI, method = "pearson", use = "complete.obs")
print(correlation_result)

#Boxplots: Stress Levels by BMI / Cholesterol / Systolic BP 
ggplot(clean_data, aes(x = BMI_Category, y = Stress_Level, fill = BMI_Category)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  labs(
    title = "Stress Levels by BMI Category",
    x = "BMI Category",
    y = "Stress Level"
  ) +
  scale_fill_manual(values = c("Underweight" = "lightblue", "Normal" = "#B5EAD7", "Overweight" = "#FFE066", "Obese" = "#FEC8D8")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )
##ADVANCED INSIGHT | ANOVA
anova_result_stress <- aov(Stress_Level ~ BMI_Category, data = clean_data)
print(anova_result_stress)

#Bar Plot: Physical Activity Category vs BMI Category 
ggplot(clean_data, aes(x = Physical_Activity_Category, fill = BMI_Category)) +
  geom_bar(position = "fill", alpha = 0.7) +
  labs(
    title = "Physical Activity Category vs BMI Category",
    x = "Physical Activity Category",
    y = "Proportion",
    fill = "BMI Category"
  ) +
  scale_fill_manual(values = c("Underweight" = "lightblue", "Normal" = "#B5EAD7", "Overweight" = "#FFE066", "Obese" = "#FEC8D8")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

##ADVANCED INSIGHT | Chi-Square Test 
chi_square_result <- chisq.test(table(clean_data$Physical_Activity_Category, clean_data$BMI_Category))
print(chi_square_result)