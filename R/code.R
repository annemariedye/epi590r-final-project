library(tidyverse)
library(gtsummary)
library(dplyr)
library(ggplot2)

strep_tb <- read_csv(here::here("data", "strep_tb.csv")) |>
	mutate(gender_cat = factor(gender, levels = c("M", "F"), labels = c("Male", "Female")),
											baseline_condition_cat = factor(baseline_condition, levels = c("1_Good", "2_Fair", "3_Poor"), labels = c("Good", "Fair", "Poor")),
											baseline_temp_cat = factor(baseline_temp, levels = c("1_<=98.9F/37.2C", "2_99-99.9F/37.3-37.7C", "3_100-100.9F/37.8-38.2C", "4_>=101F/38.3C"), labels = c("<=98.9F/37.2C", "99-99.9F/37.3-37.7C", "100-100.9F/37.8-38.2C", ">=101F/38.3C")),
											baseline_esr_cat = factor(baseline_esr, levels = c("1_1-10", "2_11-20", "3_21-50", "4_51+"), labels = c("1-10", "11-20", "21-50", "51+")),
											baseline_cavitation_cat = factor(baseline_cavitation, levels = c("yes", "no"), labels = c("Yes", "No")),
											strep_resistance_cat = factor(strep_resistance, levels = c("1_sens_0-8", "2_mod_8-99", "3_resist_100+"), labels = c("Sensitive 0-8", "Moderate 8-99", "Resistant 100+")),
											radiologic_6m_cat = factor(radiologic_6m, levels = c("1_Death", "2_Considerable Deterioration", "3_Moderate_deterioration", "4_No_change", "5_Moderate_improvement", "6_Considerable_improvement"), labels = c("Death", "Considerable Deterioration", "Moderate Deterioration", "No Change", "Moderate Improvement", "Considerable Improvement")))

#Table of Descriptive Statistics
tbl_summary(
	strep_tb,
	by = arm,
	include = c(gender_cat, baseline_condition_cat, baseline_temp_cat, baseline_esr_cat,
							baseline_cavitation_cat, strep_resistance_cat, radiologic_6m_cat),
	label = list(
		gender_cat ~ "Gender",
		baseline_condition_cat ~ "Condition of the Patient at Baseline",
		baseline_temp_cat ~ "Oral Temperature at Baseline (Degrees F)",
		baseline_esr_cat ~ "Erythrocyte Sedimentation Rate at Baseline (millimeters per hour)",
		baseline_cavitation_cat ~ "Cavitation of the Lungs on Chest X-ray at Baseline",
		strep_resistance_cat ~ "Resistance to Streptomycin at 6 months",
		radiologic_6m_cat ~ "Radiologic Outcome at 6 months"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels()

#Regression
logistic_model <- glm(gender_cat ~ baseline_condition_cat + baseline_temp_cat +
												baseline_esr_cat + baseline_cavitation_cat + strep_resistance_cat +
												radiologic_6m_cat, data = strep_tb, family = binomial())

tbl_regression(logistic_model, exponentiate = TRUE,
							 label = list(baseline_condition_cat ~ "Condition of the Patient at Baseline",
							 						 baseline_temp_cat ~ "Oral Temperature at Baseline (Degrees F)",
							 						 baseline_esr_cat ~ "Erythrocyte Sedimentation Rate at Baseline (millimeters per hour)",
							 						 baseline_cavitation_cat ~ "Cavitation of the Lungs on Chest X-ray at Baseline",
							 						 strep_resistance_cat ~ "Resistance to Streptomycin at 6 months",
							 						 radiologic_6m_cat ~ "Radiologic Outcome at 6 months"))

#Figure
figure <- ggplot(strep_tb, aes(radiologic_6m_cat)) +
					geom_bar(fill = "#0073C2FF") +
					labs(
						title = "Patient Outcomes at 6 Months",
						x = "Radiologic Outcomes at 6 Months",
						y = "Count") +
					theme_minimal() +
					theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

print(figure)

ggsave(plot = figure, filename = here::here("results", "figures", "figure.pdf"))

#Function
x <- strep_tb$rad_num

range <- function(x) {
	range_formula = max(x) - min(x)
	return(range_formula)
}

range(x)

