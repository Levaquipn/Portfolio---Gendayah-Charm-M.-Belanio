#Packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Dataset
Nutritional_Dietry_Data <- read.csv("C:/Users/ASUS/Documents/Portfolio Dataset/Exp 4/3_Nutritional_Dietary_data_Group_003.csv")

#Pre-processing Data
names(Nutritional_Dietry_Data)

clean_data <- Nutritional_Dietry_Data %>%
  filter(!is.na(BMI)) %>%  
  
  mutate(BMI_Category = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 24.9 ~ "Normal",
    BMI >= 25 & BMI < 29.9 ~ "Overweight",
    BMI >= 30 ~ "Obese",
    TRUE ~ NA_character_),
    
    Physical_Activity_Category = case_when(
      Physical_Activity_Hours_Week < 4 ~ "Low",
      Physical_Activity_Hours_Week >= 4 & Physical_Activity_Hours_Week < 10 ~ "Moderate",
      Physical_Activity_Hours_Week >= 10 ~ "High",
      TRUE ~ NA_character_),
    
    Daily_Caloric_Intake_kcal = ifelse(Daily_Caloric_Intake_kcal <= 0, NA, Daily_Caloric_Intake_kcal),
    Protein_intake_g = ifelse(Protein_intake_g <= 0, NA, Protein_intake_g),
    Fat_intake_g = ifelse(Fat_intake_g <= 0, NA, Fat_intake_g),
    Carbohydrate_intake_g = ifelse(Carbohydrate_intake_g <= 0, NA, Carbohydrate_intake_g),
    Water_intake_ml = ifelse(Water_intake_ml <= 0, NA, Water_intake_ml))

macro_data <- clean_data %>%
  select(Patient.ID, Protein_intake_g, Fat_intake_g, Carbohydrate_intake_g) %>%
  pivot_longer(
    cols = c(Protein_intake_g, Fat_intake_g, Carbohydrate_intake_g),
    names_to = "Macronutrient",
    values_to = "Intake_g"
  )
#Descriptive statistics of the dataset (mean, median, standard deviation, etc.)
descriptive_stats <- clean_data %>%
  summarise(mean_BMI = mean(BMI, na.rm = TRUE),
            median_BMI = median(BMI, na.rm = TRUE),
            sd_BMI = sd(BMI, na.rm = TRUE),
            
            mean_BodyFat = mean(Body_Fat_percent, na.rm = TRUE),
            median_BodyFat = median(Body_Fat_percent, na.rm = TRUE),
            sd_BodyFat = sd(Body_Fat_percent, na.rm = TRUE),
            
            mean_MuscleMass = mean(Muscle_Mass_kg, na.rm = TRUE),
            median_MuscleMass = median(Muscle_Mass_kg, na.rm = TRUE),
            sd_MuscleMass = sd(Muscle_Mass_kg, na.rm = TRUE),
            
            mean_Calories = mean(Daily_Caloric_Intake_kcal, na.rm = TRUE),
            median_Calories = median(Daily_Caloric_Intake_kcal, na.rm = TRUE),
            sd_Calories = sd(Daily_Caloric_Intake_kcal, na.rm = TRUE),
            
            mean_Protein = mean(Protein_intake_g, na.rm = TRUE),
            mean_Fat = mean(Fat_intake_g, na.rm = TRUE),
            mean_Carbs = mean(Carbohydrate_intake_g, na.rm = TRUE),
            
            mean_Water = mean(Water_intake_ml, na.rm = TRUE),
            median_Water = median(Water_intake_ml, na.rm = TRUE),
            sd_Water = sd(Water_intake_ml, na.rm = TRUE),
            
            mean_Activity = mean(Physical_Activity_Hours_Week, na.rm = TRUE),
            median_Activity = median(Physical_Activity_Hours_Week, na.rm = TRUE),
            sd_Activity = sd(Physical_Activity_Hours_Week, na.rm = TRUE))

print(descriptive_stats)

#Scatter Plot: Physical Activity vs. Body Fat %
ggplot(clean_data, aes(x = Physical_Activity_Hours_Week, y = Body_Fat_percent, color = BMI_Category)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Physical Activity vs. Body Fat Percentage",
       x = "Physical Activity (Hours/Week)",
       y = "Body Fat Percentage",
       color = "BMI Category") +
  theme_minimal() +
  theme(legend.position = "right")

##ADVANCED INSIGHT | Pearson
pearsons_result <- cor(clean_data$Physical_Activity_Hours_Week, clean_data$Body_Fat_percent, use = "complete.obs")
print(paste("Pearson's correlation coefficient:", pearsons_result))

#Violin + Box Plot: Macronutrient Intake vs. Muscle Mass 
ggplot(macro_data, aes(x = Macronutrient, y = Intake_g, fill = Macronutrient)) +
  geom_violin(alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(title = "Distribution of Macronutrient Intake",
       x = "Macronutrient Type",
       y = "Intake (grams)") +
  theme_minimal(base_size = 14)

##ADVANCED INSIGHT | Multiple Linear Regression
model <- lm(Body_Fat_percent ~ Physical_Activity_Hours_Week + Daily_Caloric_Intake_kcal + Protein_intake_g + Fat_intake_g + Carbohydrate_intake_g, data = clean_data)
summary(model)

#Box Plot: Daily Caloric Intake vs. BMI Categories 
ggplot(clean_data, aes(x = BMI_Category, y = Daily_Caloric_Intake_kcal, fill = BMI_Category)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Daily Calorie Intake by BMI Category",
       x = "BMI Category",
       y = "Daily Calorie Intake (kcal)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

##ADVANCED INSIGHT | ANOVA
anova_result <- aov(Daily_Caloric_Intake_kcal ~ BMI_Category, data = clean_data)
print(anova_result)

#Scatter Plot: Water Intake vs. Muscle Mass 
ggplot(clean_data, aes(x = Water_intake_ml, y = Muscle_Mass_kg)) +
  geom_point(aes(color = BMI_Category), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Water Intake vs. Muscle Mass",
       x = "Water Intake (ml)",
       y = "Muscle Mass (kg)",
       color = "BMI Category") +
  theme_minimal() +
  theme(legend.position = "right")

##ADVANCED INSIGHT | spearman correlation
spearman_result <- cor(clean_data$Water_intake_ml, clean_data$Muscle_Mass_kg, method = "spearman", use = "complete.obs")
print(paste("Spearman's correlation coefficient:", spearman_result))
