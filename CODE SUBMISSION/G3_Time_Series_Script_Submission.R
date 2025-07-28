# Packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Dataset
Time_Series_Data <- read.csv("C:/Users/ASUS/Documents/Portfolio Dataset/Exp 8/4_Time_series_Mointoring_data_Group_003.csv")

candy_palette <- c(
  "Patient_1" = "#B5EAD7",  
  "Patient_2" = "#C7CEEA",  
  "Patient_3" = "#FEC8D8")

# Preprocessing
clean_data <- Time_Series_Data %>%
  pivot_longer(
    cols = starts_with("Patient_"),
    names_to = c("Patient", "Variable"),
    names_pattern = "Patient_(\\d)_(.*)",
    values_to = "Value") %>%
  pivot_wider(
    names_from = Variable,
    values_from = Value) %>%
  mutate(
    Patient = paste0("Patient_", Patient),
    Month_num = (Month_numerical - 1) %% 12 + 1,
    Month_Label = factor(month.abb[Month_num], levels = month.abb)) %>%
  filter(!is.na(avg_steps), !is.na(Stress_Level), !is.na(BMI))

# Descriptive statistics of the dataset (mean, median, standard deviation, etc.)
descriptive_stats <- clean_data %>%
  summarise(
    mean_steps = mean(avg_steps, na.rm = TRUE),
    median_steps = median(avg_steps, na.rm = TRUE),
    sd_steps = sd(avg_steps, na.rm = TRUE),
    
    mean_stress = mean(Stress_Level, na.rm = TRUE),
    median_stress = median(Stress_Level, na.rm = TRUE),
    sd_stress = sd(Stress_Level, na.rm = TRUE),
    
    mean_BMI = mean(BMI, na.rm = TRUE),
    median_BMI = median(BMI, na.rm = TRUE),
    sd_BMI = sd(BMI, na.rm = TRUE))

print(descriptive_stats)

# Line Plot: Steps Over Time
clean_data <- clean_data %>%
  mutate(
    Year = 2020 + (Month_numerical - 1) %/% 12, 
    Month_num = (Month_numerical - 1) %% 12 + 1,
    Month_Label = month.abb[Month_num],
    Month_Year = factor(paste(Month_Label, Year), 
        levels = paste(month.abb[((min(Month_numerical)-1) %% 12 + 1):((max(Month_numerical)-1) %% 12 + 1)],
        2020 + ((min(Month_numerical)-1) %/% 12):((max(Month_numerical)-1) %/% 12))))

ggplot(clean_data, aes(x = Month_Year, y = avg_steps, color = Patient, group = Patient)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Steps Over Time",
    x = "Month and Year",
    y = "Average Steps",
    color = "Patient") +
  
  theme_minimal() +
  scale_color_manual(values = candy_palette) +
  theme(
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

## ADVANCED INSIGHT | Linear Regression
model_steps <- lm(avg_steps ~ Month_numerical + Patient, data = clean_data)
summary(model_steps)

# Bar Plot: Monthly Stress Levels per Patient
month_labels <- c("Jan.", "Feb.", "Mar.", "Apr.", "May.", "Jun.",
                  "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")

ggplot(clean_data, aes(x = Month_Year, y = Stress_Level, fill = Patient)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Monthly Stress Levels per Patient",
    x = "Month",
    y = "Stress Level",
    fill = "Patient") +
  
  scale_fill_manual(values = candy_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 0))

## ADVANCED INSIGHT | Two-way ANOVA
anova_result <- aov(Stress_Level ~ Month * Patient, data = clean_data)
print(anova_result)

# Scatter Plot: BMI vs. Average Steps
ggplot(clean_data, aes(x = BMI, y = avg_steps, color = Patient)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "BMI vs. Average Steps",
    x = "BMI",
    y = "Average Steps",
    color = "Patient") +
  
  theme_minimal() +
  scale_color_manual(values = candy_palette) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 0))

## ADVANCED INSIGHT | Pearson Correlation
pearson_result <- cor(clean_data$BMI, clean_data$avg_steps, use = "complete.obs")
print(pearson_result)

# Box Plot: BMI Distribution by Patient 
ggplot(clean_data, aes(x = Patient, y = BMI, fill = Patient)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "BMI Distribution by Patient",
    x = "Patient",
    y = "BMI") +
  
  theme_minimal() +
  scale_fill_manual(values = candy_palette) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 0))

## ADVANCED INSIGHT | Kruskal-Wallis
kruskal.test(BMI ~ Patient, data = clean_data)
