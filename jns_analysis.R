# Install packages if necessary
# install.packages("googlesheets4")
# install.packages("dplyr")
# install.packages("finalfit")
# install.packages("pROC")
# install.packages("missForest")
library(mice) 
library(pROC)
library(googlesheets4)
library(dplyr)
library(finalfit)
library(ggplot2)
library(data.table)

para_only_df <- read_sheet("https://docs.google.com/spreadsheets/d/1_Eir3hYTAjuN0rE8Xu8mI5MvBB6wwg4muboauUkjiVE/edit#gid=310140165", sheet = "para_only_df")

para_only_df$junctional <- 
  ifelse(para_only_df$target_c1 == "Checked" | para_only_df$target_c2 == "Checked" | para_only_df$target_c7 == "Checked" | 
           para_only_df$target_t1 == "Checked" | para_only_df$target_t2 == "Checked" | para_only_df$target_t11 == "Checked" | 
           para_only_df$target_t12 == "Checked" | para_only_df$target_l1 == "Checked" | para_only_df$target_l5 == "Checked" | 
           para_only_df$target_s1 == "Checked", 1, 0)

para_only_df$mobile <- 
  ifelse(para_only_df$target_c3 == "Checked" | para_only_df$target_c4 == "Checked" | para_only_df$target_c5 == "Checked" | 
           para_only_df$target_c6 == "Checked" | para_only_df$target_l2 == "Checked" | para_only_df$target_l3 == "Checked" | 
           para_only_df$target_l4 == "Checked", 1, 0)

para_only_df$rigid <- 
  ifelse(para_only_df$target_t3 == "Checked" | para_only_df$target_t4 == "Checked" | para_only_df$target_t5 == "Checked" | 
           para_only_df$target_t6 == "Checked", 1, 0)

para_only_df$semirigid <- 
  ifelse(para_only_df$target_s2 == "Checked" | para_only_df$target_s3 == "Checked" | para_only_df$target_s4 == "Checked" | 
           para_only_df$target_s5 == "Checked", 1, 0)

full_df <- para_only_df 

full_df <- full_df %>%
  mutate(
    any_complication_adjusted = if_else(
      ssi == 1 | necrosis == 1 |
        dehiscence_without_ssi == 1 |
        # hematoma == 1 |
        unintentional_durotomy_csf_leak == 1 |
        symptomatic_seroma == 1,
      1, 0
    )
  )

# Specify the variables we want to change to numeric class
numerics_to_change <- c("age", "preop_weight_lbs", "height_in", "preop_bmi",
                        "hgb", "albumin", "procedure_duration", "levels_incise",
                        "instrumentation_levels", "length_of_closure_cm", 
                        "ebl", "drain_duration_days", "los_days", "target_levels",
                        "days_until_postop_chemo", "days_until_postop_radiation",
                        "fu_period_mo", "number_drains"
)

# Change the class of the variables to numeric
full_df <- full_df %>%
  mutate_at(vars(numerics_to_change), as.numeric)

# Specify the variables we want to change to character class
factors_to_change <- c("any_complication_adjusted",
                       "sex", "htn", "hx_cva", "hx_mi", "hx_cvd", "aca_use", 
                       "hx_chemo", "hx_radiation", "hx_prior_spine",
                       "chronic_steroid", "niddm", "iddm", "asa", "asagtet3",
                       "neoplasm", "t_cervical", "t_thoracic",
                       "t_lumbar", "t_sacral_coccygeal", "lam_hem_lamino",
                       "fac_form", "any_corp", "any_instrumentation",
                       "bone_matrix", "non_vanc_abx_infusion",
                       "junctional", "rigid", "mobile", "semirigid",
                       "sup_and_deep", "prolonged_los", "icu_admit",
                       "ssi", "necrosis",
                       "dehiscence", 'dehiscence_without_ssi',
                       "hematoma", "symptomatic_seroma",
                       "csf_leak", "intentional_durotomy_csf_leak",
                       "unintentional_durotomy_csf_leak", 
                       "reoperation_rt_wound_complication",
                       "mortality_30d", "mortality_90d",
                       "greater_than_20", "greater_than_30",
                       "repeat_region")

# Change the class of the variables to character
full_df <- full_df %>%
  mutate_at(vars(factors_to_change), as.character)

#-----------------------------------------------------------
# Defining the Data Summary  
#-----------------------------------------------------------
# Define variables
dependent = "any_complication_adjusted"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "asagtet3",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "hx_cvd",
                "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "indication",
                "indication",
                "junctional", 
                "mobile", "semirigid", "rigid",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation",
                "ssi", "infection_depth",
                "necrosis", "dehiscence",
                "dehiscence_without_ssi",
                "hematoma", "symptomatic_seroma", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d", "mortality_90d",
                "repeat_region")

# Paraspinous flap closure data summary by any complication, adjusted
para_summary <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(para_summary, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_2")

#-----------------------------------------------------------
# Student T-test Analysis
#-----------------------------------------------------------

numeric_vars <- c("age", "height_in", "preop_weight_lbs", "preop_bmi",                   "target_levels", "levels_incise", "length_of_closure_cm", 
                  "instrumentation_levels", "procedure_duration", 
                  "ebl", "number_drains", "drain_duration_days", "los_days", 
                  "fu_period_mo")

# Create an empty data frame to store results
para_t_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

# Loop through each numeric variable
for (numeric_var in numeric_vars) {
  # Perform t-test
  para_t_test_result <- 
    t.test(get(numeric_var) ~ any_complication_adjusted, 
           data = full_df)
  
  # Extract p-value
  p_value <- para_t_test_result$p.value
  
  # Append results to the data frame
  para_t_table <- 
    rbind(para_t_table, data.frame(variable = numeric_var, 
                                   p_value = p_value))
}

# Write the T-test data frame to the shared Google Drive
write_sheet(para_t_table, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_2.t")

#-----------------------------------------------------------
# Chi-Squared Analysis
#-----------------------------------------------------------
categorical_vars <- c("male", "female",
                      "former_smoker", "current_smoker",
                      "niddm", "iddm", "htn", "hx_cva", "hx_cvd",
                      "hx_mi", "aca_use", "asa", "asagtet3",
                      "chronic_steroid", 
                      "hx_chemo", "hx_radiation",
                      "hx_prior_spine",
                      "neoplasm", "primary_spinal_tumor",
                      "brea_tumor", "chor_tumor",
                      "epen_tumor", "lung_tumor", 
                      "rena_tumor", "meta_tumor",
                      "othe_tumor", "schw_tumor", 
                      "pros_tumor", "mult_tumor", 
                      "spin_tumor",
                      "indication",
                      "junctional", "mobile", "semirigid", "rigid",
                      "t_cervical", "t_thoracic",
                      "t_lumbar", "t_sacral_coccygeal",
                      "lam_hem_lamino", "fac_form", "fusion", 
                      "any_corp", "discectomy", "costotransversectomy",
                      "lateral_extracavitary", "sacrectomy", 
                      "spine_procedure_other", 
                      "greater_than_20", "greater_than_30",
                      "any_instrumentation",
                      "rods", "plates", "cages", 
                      "bone_matrix", 
                      "vancomycin_powder", "vancomycin_non_powder",
                      "non_vanc_abx_infusion",
                      "superficial_depth", 
                      "deep_drain", "sup_and_deep", 
                      "postop_chemo", "postop_radiation",
                      "ssi", "deep_ssi", "superficial_ssi",
                      "drain_ssi","prolonged_los", "icu_admit",
                      "necrosis", "dehiscence",
                      "dehiscence_without_ssi",
                      "hematoma", "symptomatic_seroma", "csf_leak",
                      "intentional_durotomy_csf_leak",
                      "unintentional_durotomy_csf_leak",
                      "reoperation","reoperation_rt_wound_complication",
                      "mortality_30d", "mortality_90d")

#Paraspinous chi-squared
# Create an empty data frame to store results
para_chi_table <- 
  data.frame(variable = character(), 
             p_value = numeric(), 
             stringsAsFactors = FALSE)

# Loop through each categorical variable
for (categorical_var in categorical_vars) {
  # Perform chi-square test
  chi_square_result <- 
    chisq.test(table(full_df[[categorical_var]], 
                     full_df$any_complication_adjusted))
  
  # Extract p-value
  p_value <- chi_square_result$p.value
  
  # Append results to the data frame
  para_chi_table <- 
    rbind(para_chi_table, data.frame(variable = categorical_var, 
                                     p_value = p_value))
}

# Write the chi-squared data frame to the shared Google Drive
write_sheet(para_chi_table, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_2.chi")

#-----------------------------------------------------------
# Multivariate Logistic Regression
full_df <- full_df %>% filter(study_id != 178) #  remove outlier by BMI
full_df <- full_df %>% filter(study_id != 431) #  remove outlier by BMI
#-----------------------------------------------------------

# greater_than_20, greater_than_30

# "any_complication_adjusted", "ssi", "symptomatic_seroma", 
# "hematoma"
# "dehiscence_without_ssi", "prolonged_los", "icu_admit",
# "reoperation_rt_wound_complication", "mortality_30d"

full_df$indication <- as.factor(full_df$indication)

# Relevel 'indication' with 'Degenerative' as the reference
full_df$indication <- relevel(full_df$indication, ref = "Neoplastic") #Set neo for dehiscence

# Refit the logistic regression model
logistic_model <- glm(dehiscence_without_ssi ~ age + sex + niddm + iddm +
                        preop_bmi + 
                        asa + chronic_steroid +
                        htn + hx_mi + hx_cva + 
                        indication + 
                        aca_use + 
                        hx_prior_spine + 
                        junctional +
                        length_of_closure_cm,
                      data = full_df, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# Summarize the logistic regression model
model_summary <- summary(logistic_model)

# Extract coefficients, standard errors, z-values, and p-values
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
z_values <- coef(model_summary)[, "z value"]
p_values <- coef(model_summary)[, "Pr(>|z|)"]

# Extract variable names
variable_names <- rownames(coef(model_summary))

# Create a data frame with Coefficients, Std. Error, z-value, and P value
logistic_regression_table <- data.frame(
  Variables = variable_names,
  Coefficients = coefficients,
  Std_Error = std_errors,
  Z_Value = z_values,
  P_Values = p_values
)

write_sheet(logistic_regression_table, ss = 
              "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "report")

#OTHER ANALYSIS FOR FIGURE 3

# greater_than_20, greater_than_30

# "ssi", "symptomatic_seroma", 
# "dehiscence_without_ssi", "prolonged_los", "icu_admit",
# "reoperation_rt_wound_complication", "mortality_30d"

full_df$indication <- as.factor(full_df$indication)

# Relevel 'indication' with 'Degenerative' as the reference
full_df$indication <- relevel(full_df$indication, ref = "Degenerative")

# Refit the logistic regression model
logistic_model <- glm(reoperation_rt_wound_complication ~ ssi + symptomatic_seroma + 
                        dehiscence_without_ssi +
                        unintentional_durotomy_csf_leak +
                        hematoma,
                      data = full_df, 
                      family = binomial)

# Display summary of the model
summary(logistic_model)

# Summarize the logistic regression model
model_summary <- summary(logistic_model)

# Extract coefficients, standard errors, z-values, and p-values
coefficients <- coef(model_summary)[, "Estimate"]
std_errors <- coef(model_summary)[, "Std. Error"]
z_values <- coef(model_summary)[, "z value"]
p_values <- coef(model_summary)[, "Pr(>|z|)"]

# Extract variable names
variable_names <- rownames(coef(model_summary))

# Create a data frame with Coefficients, Std. Error, z-value, and P value
logistic_regression_table <- data.frame(
  Variables = variable_names,
  Coefficients = coefficients,
  Std_Error = std_errors,
  Z_Value = z_values,
  P_Values = p_values
)

write_sheet(logistic_regression_table, ss = 
              "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "report")


#-----------------------------------------------------------
# Recheck Data Class  
#-----------------------------------------------------------

# Specify the variables we want to change to numeric class
numeric_variables <- c("study_id", "dob", "age", 
                       "preop_weight_lbs", "height_in", "preop_bmi",
                       "hgb", "albumin", "index_surgery_date", "procedure_start", 
                       "procedure_finish", "procedure_duration", "last_nsgy_fu",
                       "fu_period_mo",
                       "levels_incise",
                       "instrumentation_levels", "length_of_closure_cm", 
                       "ebl", "number_drains", "drain_placement_date", 
                       "drain_removal_date", "drain_duration_days", 
                       "discharge_date",
                       "los_days", "target_levels",
                       "days_until_postop_chemo", "days_until_postop_radiation",
                       "fu_period_mo", "number_drains"
)

# Change the class of the variables to numeric
full_df <- full_df %>%
  mutate_at(vars(numeric_variables), as.numeric)

# Specify the variables we want to change to character class
factors_to_change <- c('any_complication_adjusted',
                       "sex", "htn", "hx_cva", "hx_mi", "aca_use", 
                       "hx_chemo", "hx_radiation", "hx_prior_spine",
                       "chronic_steroid", "niddm", "iddm", "asa",
                       "neoplasm", "t_cervical", "t_thoracic",
                       "t_lumbar", "t_sacral_coccygeal", "lam_hem_lamino",
                       "fac_form", "any_corp", "any_instrumentation",
                       "bone_matrix", "non_vanc_abx_infusion",
                       "junctional", "rigid", "mobile", "semirigid",
                       "sup_and_deep", "prolonged_los", "icu_admit",
                       "ssi", "necrosis",
                       "dehiscence", 'dehiscence_without_ssi',
                       "hematoma", "symptomatic_seroma",
                       "csf_leak", "intentional_durotomy_csf_leak",
                       "unintentional_durotomy_csf_leak", 
                       "reoperation_rt_wound_complication",
                       "mortality_30d",
                       "greater_than_20", "greater_than_30")

full_df <- full_df %>%
  mutate_at(vars(factors_to_change), as.factor)

#-----------------------------------------------------------
# Indication Data Summary  
#-----------------------------------------------------------
# Define variables
dependent = "indication"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "junctional", "mobile", "semirigid", "rigid",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation", 
                "any_complication_adjusted",
                "ssi", "infection_depth",
                "necrosis", "dehiscence",
                "dehiscence_without_ssi",
                "hematoma", "symptomatic_seroma", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d",
                "repeat_region")

# Paraspinous flap closure data summary by indication
jns_indication_summary <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(jns_indication_summary, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_1")

# ----------------------------------------------------------------------
# Hot-encoding ANOVA and chi-square analysis for indication (>2 subgroups)
# ----------------------------------------------------------------------

# Create  empty list to store results
full_df_indication_list <- list()

# Loop through each explanatory variable
for (variable in names(full_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("indication")) {
    next
  }
  
  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      # Perform ANOVA
      anova_result <- aov(full_df[[variable]] ~ indication, data = full_df)
      
      # Extract the p-value from ANOVA summary
      p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
      
      # Append results to the list
      full_df_indication_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(full_df[[variable]])
      for (level in levels) {
        contingency_table <- table(full_df[[variable]] == level, full_df$indication)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        full_df_indication_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    # If an error occurs, print NA in the p-value column
    full_df_indication_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
full_df_indication <- do.call(rbind, full_df_indication_list)

write_sheet(full_df_indication, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_1.stats")


# Calculating median and IQR for values e.g. ASA

# Ensure ASA is numeric
full_df$number_drains <- as.numeric(as.character(full_df$number_drains))

# Calculate median and IQR for ASA across all indications
summary_stats <- full_df %>%
  group_by(any_complication_adjusted) %>%
  summarise(
    Median = median(asa, na.rm = TRUE),
    Q1 = quantile(number_drains, 0.25, na.rm = TRUE),
    Q3 = quantile(number_drains, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  )

# View the result
summary_stats

anova_result <- aov(number_drains ~ indication, data = full_df)

# View the summary of the ANOVA
summary(anova_result)

# ----------------------------------------------------------------------
# Table 3 - SSI
# ----------------------------------------------------------------------
# Define variables
dependent = "ssi"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "junctional", "indication",
                "mobile", "semirigid", "rigid",# Create a contingency table for SSI and repeat_region
contingency_table_ssi <- table(full_df$SSI, full_df$repeat_region)

# Perform the chi-squared test
chi_sq_result_ssi <- chisq.test(contingency_table_ssi)

# View the result
chi_sq_result_ssi

                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation", 
                "any_complication_adjusted",
                "infection_depth",
                "necrosis", "dehiscence_without_ssi",
                "dehiscence_without_ssi",
                "hematoma", "symptomatic_seroma", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d",
                "repeat_region")

# Paraspinous flap closure data summary by indication
jns_table_3 <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(jns_table_3, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_3")

# Stats for table 3

# Create empty list to store results
full_df_list <- list()

# Loop through each explanatory variable
for (variable in names(full_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("ssi")) {
    next
  }
  
  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      
      t_test_result <- t.test(full_df[[variable]] ~ full_df$ssi)
      
      # Extract the p-value from the t-test
      p_value <- t_test_result$p.value
      
      # Append results to the list
      full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(full_df[[variable]])
      for (level in levels) {
        contingency_table <- table(full_df[[variable]] == level, full_df$ssi)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        full_df_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
full_df_stats <- do.call(rbind, full_df_list)

# Write the results to Google Sheets
write_sheet(full_df_stats, 
            ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "jns_table_3.stats")


# ----------------------------------------------------------------------
# Table 4 - dehiscence_without_ssi
# ----------------------------------------------------------------------
# Define variables
dependent = "dehiscence_without_ssi"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "junctional", "indication",
                "mobile", "semirigid", "rigid",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation", 
                "any_complication_adjusted",
                "infection_depth",
                "necrosis",
                "ssi",
                "hematoma", "symptomatic_seroma", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d",
                "repeat_region")

# Paraspinous flap closure data summary by indication
jns_table_4 <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(jns_table_4, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_4")

# Stats for table 4

# Create empty list to store results
full_df_list <- list()

# Loop through each explanatory variable
for (variable in names(full_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("dehiscence_without_ssi")) {
    next
  }
  
  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      
      t_test_result <- t.test(full_df[[variable]] ~ full_df$dehiscence_without_ssi)
      
      # Extract the p-value from the t-test
      p_value <- t_test_result$p.value
      
      # Append results to the list
      full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(full_df[[variable]])
      for (level in levels) {
        contingency_table <- table(full_df[[variable]] == level, full_df$dehiscence_without_ssi)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        full_df_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
full_df_stats <- do.call(rbind, full_df_list)

# Write the results to Google Sheets
write_sheet(full_df_stats, 
            ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "jns_table_4.stats")

# ----------------------------------------------------------------------
# Table 5.1 - symptomatic_seroma
# ----------------------------------------------------------------------

# Define variables
dependent = "symptomatic_seroma"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "junctional", "indication",
                "mobile", "semirigid", "rigid",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation", 
                "any_complication_adjusted",
                "infection_depth",
                "necrosis",
                "ssi",
                "hematoma", "dehiscence_without_ssi", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d",
                "repeat_region")

# Paraspinous flap closure data summary by symptomatic seroma
jns_table_5.1 <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(jns_table_5.1, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_5.1")

# Stats for table 5.1

# Create empty list to store results
full_df_list <- list()

# Loop through each explanatory variable
for (variable in names(full_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("symptomatic_seroma")) {
    next
  }
  
  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      
      t_test_result <- t.test(full_df[[variable]] ~ full_df$symptomatic_seroma)
      
      # Extract the p-value from the t-test
      p_value <- t_test_result$p.value
      
      # Append results to the list
      full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(full_df[[variable]])
      for (level in levels) {
        contingency_table <- table(full_df[[variable]] == level, full_df$symptomatic_seroma)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        full_df_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
full_df_stats <- do.call(rbind, full_df_list)

# Write the results to Google Sheets
write_sheet(full_df_stats, 
            ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "jns_table_5.1stats")

# ----------------------------------------------------------------------
# Table 5.2 - hematoma
# ----------------------------------------------------------------------

# Define variables
dependent = "hematoma"
explanatory = c("age", "sex", "height_in",
                "preop_weight_lbs", "preop_bmi", "asa",
                "smoker", "dm",
                "htn", "hx_cva",
                "hx_mi", "aca_use", 
                "steroid_use", 
                "hx_chemo", "hx_radiation",
                "junctional", "indication",
                "mobile", "semirigid", "rigid",
                "radiation_type", 
                "hx_prior_spine",
                "neoplasm", "primary_spinal_tumor",
                "primary_tumor_type",
                "t_cervical", "t_thoracic",
                "t_lumbar", "t_sacral_coccygeal",
                "lam_hem_lamino", "fac_form", "fusion", 
                "any_corp", "discectomy", "costotransversectomy",
                "lateral_extracavitary", "sacrectomy", 
                "spine_procedure_other", 
                "target_levels", 
                "levels_incise", 
                "greater_than_20", "greater_than_30",
                "length_of_closure_cm",
                "instrumentation_levels",
                "any_instrumentation",
                "rods", "plates", "cages", 
                "bone_matrix", 
                "vancomycin_powder", "vancomycin_non_powder",
                "non_vanc_abx_infusion",
                "procedure_duration", "ebl", 
                "drains_placed", "number_drains", "superficial_depth", 
                "deep_drain", "sup_and_deep", 
                "drain_duration_days", "los_days", 
                "prolonged_los", "icu_admit",
                "fu_period_mo", 
                "postop_chemo", "postop_radiation", 
                "any_complication_adjusted",
                "infection_depth",
                "necrosis",
                "ssi",
                "symptomatic_seroma", "dehiscence_without_ssi", "csf_leak",
                "intentional_durotomy_csf_leak",
                "unintentional_durotomy_csf_leak",
                "reoperation_rt_wound_complication",
                "mortality_30d",
                "repeat_region")

# Paraspinous flap closure data summary by symptomatic seroma
jns_table_5.2 <- full_df %>% 
  summary_factorlist(dependent, explanatory,
                     na_include=TRUE, 
                     na_include_dependent = TRUE, 
                     total_col = TRUE, 
                     add_col_totals = TRUE, 
                     col_totals_prefix = "n = ", 
                     p=FALSE, 
                     cont = "mean",
                     cont_cut = 2,
                     orderbytotal = TRUE, 
                     include_row_totals_percent = TRUE,
                     digits = c(1, 1, 3, 1, 0)
  )

# Write the summary data frame to the shared Google Drive
write_sheet(jns_table_5.2, ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", sheet = "jns_table_5.2")

# Stats for table 5.2 - hematoma

# Create empty list to store results
full_df_list <- list()

# Loop through each explanatory variable
for (variable in names(full_df)) {
  # Skip the response variable and non-explanatory variables
  if (variable %in% c("hematoma")) {
    next
  }
  
  tryCatch({
    # Check if the variable is numeric
    if (variable %in% numeric_variables) {
      
      t_test_result <- t.test(full_df[[variable]] ~ full_df$hematoma)
      
      # Extract the p-value from the t-test
      p_value <- t_test_result$p.value
      
      # Append results to the list
      full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = p_value)
    } else {
      # Perform chi-square test for each level
      levels <- unique(full_df[[variable]])
      for (level in levels) {
        contingency_table <- table(full_df[[variable]] == level, full_df$hematoma)
        chi_square_result <- chisq.test(contingency_table)
        p_value <- chi_square_result$p.value
        
        # Append results to the list
        full_df_list[[paste(variable, level, sep = "_")]] <- data.frame(variable = variable, level = as.character(level), p_value = p_value)
      }
    }
  }, error = function(e) {
    full_df_list[[variable]] <- data.frame(variable = variable, level = "NA", p_value = NA)
  })
}

# Combine the list of data frames into one data frame
full_df_stats <- do.call(rbind, full_df_list)

# Write the results to Google Sheets
write_sheet(full_df_stats, 
            ss = "https://docs.google.com/spreadsheets/d/1wpYfoKiOlVMy611F7Ni_YMZLnnQrkol54qy7X6c8ztg/edit?usp=sharing", 
            sheet = "jns_table_5.2stats")


contingency_table <- table(full_df$indication, full_df$hematoma)

# Perform chi-square test
chi_square_result <- chisq.test(contingency_table)

# View the result
chi_square_result
