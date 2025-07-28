#Packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Dataset
Demographic_Behavioral_Data <- read.csv("C:/Users/ASUS/Documents/Portfolio Dataset/Exp 2/2_Demographic_Behavioral_data_Group_003.csv")

#Pre-processing Data
names(Demographic_Behavioral_Data)

Demographic_Behavioral_Data <- Demographic_Behavioral_Data %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "Unknown", .)))

Demographic_Behavioral_Data <- Demographic_Behavioral_Data %>%
  select(-Smoking_status, -Drinking_status, -Region.1, -Socioeconomic.1,
         -Education.1, -Patient_Satisfaction, -Health_literacy,
         -starts_with("X")) %>%
  rename_with(~ gsub("_", " ", .x) %>% tolower())

#Descriptive statistics of the dataset (mean, median, standard deviation, etc.)
descriptive_stats <- Demographic_Behavioral_Data %>%
  summarise(
    mean_BMI = mean(bmi, na.rm = TRUE),
    mean_Phys_Act = mean('physical activity hours week', na.rm = TRUE),
    mean_Sex = mean('sex', na.rm = TRUE),
    mean_Education = mean('education', na.rm = TRUE),
    mean_Health_Score = mean('health literacy score', na.rm = TRUE),
    mean_Satisfaction_Score = mean('patient satisfaction score', na.rm = TRUE),)
print(descriptive_stats)

#Scatter Plot: BMI vs Physical Activity
ggplot(Demographic_Behavioral_Data, aes(x = `physical activity hours week`, y = bmi)) +
  geom_point(color = "#66CCFF", size = 2.5, alpha = 0.8) +  # Sky blue
  geom_smooth(method = "lm", se = FALSE, color = "#FF6F91", size = 1.2) + 
  labs(title = "BMI vs Physical Activity Hours per Week",
       x = "Physical Activity Hours per Week",
       y = "BMI") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "#444"),
        axis.title = element_text(face = "bold", color = "#555"))

##ADVANCED INSIGHT | PEARSON CORRELATIOKN
pearsons_bmi_activity <- cor(Demographic_Behavioral_Data$bmi, Demographic_Behavioral_Data$`physical activity hours week`, use = "complete.obs")
print(paste("Pearson Correlation between BMI and Physical Activity Hours per Week:", pearsons_bmi_activity))

#Box Plot: BMI by Sex
Demographic_Behavioral_Data$sex <- factor(Demographic_Behavioral_Data$sex, 
                                          levels = c(0, 1), 
                                          labels = c("Male", "Female"))
ggplot(Demographic_Behavioral_Data, aes(x = sex, y = bmi, fill = sex)) +
  geom_boxplot(alpha = 0.85, width = 0.6, outlier.shape = NA) +
  geom_jitter(color = "#444444", size = 1.5, alpha = 0.15, width = 0.2) +
  scale_fill_manual(values = c("#89CFF0", "#FFB6C1")) +  
  labs(title = "BMI Distribution by Sex",
       x = "Sex",
       y = "BMI") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#333"))

##ADVANCED INSIGHT | T.TEST
t.test(bmi ~ sex, data = Demographic_Behavioral_Data)

#Boxplot: Health Literacy by Education
Demographic_Behavioral_Data$education <- factor(Demographic_Behavioral_Data$education,
                                                levels = c(0, 1, 2, 3),
                                                labels = c("None", "Primary", "Secondary", "Tertiary"))

ggplot(Demographic_Behavioral_Data, aes(x = education, y = `health literacy score`, fill = education)) +
  geom_boxplot(alpha = 0.9, width = 0.6) +
  scale_fill_manual(values = c("#FFE066", "#FEC8D8", "#B5EAD7", "#FF9AA2")) +  
  labs(title = "Health Literacy Score by Education Level",
       x = "Education Level",
       y = "Health Literacy Score") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#333"))

##ADVANCED INSIGHT |ANOVA
aov_result <- aov(`health literacy score` ~ education, data = Demographic_Behavioral_Data)
summary(aov_result)

#Violin Plot: Patient Satisfaction by Smoking Status
Demographic_Behavioral_Data$`smoking status` <- as.factor(Demographic_Behavioral_Data$`smoking status`)

ggplot(Demographic_Behavioral_Data, aes(x = `smoking status`, y = `patient satisfaction score`, fill = `smoking status`)) +
  geom_violin(trim = FALSE, alpha = 0.85) +
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
  scale_fill_manual(
    values = c("#FFB6B9", "#A0E7E5", "#B28DFF"), #huntrix
    labels = c("Non-Smoker", "Occasional", "Chainsmoker")
  ) +
  labs(
    title = "Patient Satisfaction by Smoking Status",
    x = "Smoking Status",
    y = "Patient Satisfaction"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#444")
  )

##ADVANCED INSIGHT | Chi-Square Test
Demographic_Behavioral_Data$sat_group <- cut(Demographic_Behavioral_Data$`patient satisfaction score`,
                                             breaks = c(0, 2, 4, 5),
                                             labels = c("Low", "Medium", "High"))
chisq.test(table(Demographic_Behavioral_Data$`smoking status`, Demographic_Behavioral_Data$sat_group))



