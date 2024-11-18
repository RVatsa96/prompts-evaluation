###################################################################
### Jacaranda Health PROMPTS Evaluation ###########################
###################################################################

### REQUIRES: PROMPTS_Full.dta 
### - Not provided per ethical approval under which study data were collected

### WRITTEN BY: Rajet Vatsa, Wei Chang

### OVERVIEW: 1. Balance
###           2. Attrition
###           3. Indices
###           4. Outcomes
###           5. P-Value Adjustment

# Load necessary libraries
library(haven); library(lmtest); library(sandwich); library(tidyverse)

# Import cleaned and merged PROMPTS dataset
data_dir <- "~/Dropbox (Harvard University)/JH Eval Data/Harvard Analysis/Data/Clean/"
prompts_data <- read_dta(paste0(data_dir, "PROMPTS_Full.dta"))
anc_data <- prompts_data %>% filter(PAN_final_status == 1) # 3399 observations from antenatal f/u survey
pnc_data <- prompts_data %>% filter(PPN_status == "Completed") # 5509 observations from postpartum f/u survey
outcomes_data <- prompts_data %>% filter(anc_fu == 1 | pnc_fu == 1) # 6139 observations from full sample

### 1. ASSESS BASELINE BALANCE

# Balance characteristics: PBL_age PBL_edu_sec PBL_read_kisw_eng PBL_married PBL_Q706_num_ppl_hh
#                          PBL_Q707_ownland PBL_improved_water PBL_improved_toilet PBL_travel_vehicle
#                          PBL_Q712_minutestohosp PBL_Q713_workforpay PBL_save_med PBL_Q716_own_mobile_phn
#                          PBL_text_freq PBL_Q720_received_preg_sms PBL_gestation_age PBL_Q401_anc_attd
#                          PBL_Q405_anc_total PBL_knowledge_s_f PBL_high_risk PBL_Q501_preg_total
#                          PBL_prev_preg_high_risk PBL_PHQ2_score

# Continuous variables
var_cnt <- c("PBL_age", "PBL_Q706_num_ppl_hh", "PBL_Q712_minutestohosp", "PBL_gestation_age",
             "PBL_Q405_anc_total", "PBL_knowledge_s_f", "PBL_Q501_preg_total", "PBL_PHQ2_score")
for(var in var_cnt){
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  overall_mean_bsl = colMeans(prompts_data[, var], na.rm = T)
  overall_sd_bsl = sapply(prompts_data[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  ctrl_mean_bsl = colMeans(filter(prompts_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd_bsl = sapply(filter(prompts_data, PBL_treat == "Control")[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  trt_mean_bsl = colMeans(filter(prompts_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_sd_bsl = sapply(filter(prompts_data, PBL_treat == "Treated")[, var], sd, na.rm = T)
  
  # Print summary statistics, rounded to 2 decimal points
  print("Balance among full baseline sample:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean (SD): ", round(overall_mean_bsl, 2), " (", round(overall_sd_bsl, 2), ")"))
  print(paste0("Control Mean (SD): ", round(ctrl_mean_bsl, 2), " (", round(ctrl_sd_bsl, 2), ")"))
  print(paste0("Treated Mean (SD): ", round(trt_mean_bsl, 2), " (", round(trt_sd_bsl, 2), ")"))
  
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  # that completed antenatal follow-up
  overall_mean_anc = colMeans(anc_data[, var], na.rm = T)
  overall_sd_anc = sapply(anc_data[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  # that completed antenatal follow-up
  ctrl_mean_anc = colMeans(filter(anc_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd_anc = sapply(filter(anc_data, PBL_treat == "Control")[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  # that completed antenatal follow-up
  trt_mean_anc = colMeans(filter(anc_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_sd_anc = sapply(filter(anc_data, PBL_treat == "Treated")[, var], sd, na.rm = T)
  
  # Print summary statistics, rounded to 2 decimal points
  print("Balance among baseline sample that completed antenatal follow-up:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean (SD): ", round(overall_mean_anc, 2), " (", round(overall_sd_anc, 2), ")"))
  print(paste0("Control Mean (SD): ", round(ctrl_mean_anc, 2), " (", round(ctrl_sd_anc, 2), ")"))
  print(paste0("Treated Mean (SD): ", round(trt_mean_anc, 2), " (", round(trt_sd_anc, 2), ")"))
  
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  # that completed postpartum follow-up
  overall_mean_pnc = colMeans(pnc_data[, var], na.rm = T)
  overall_sd_pnc = sapply(pnc_data[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  # that completed postpartum follow-up
  ctrl_mean_pnc = colMeans(filter(pnc_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd_pnc = sapply(filter(pnc_data, PBL_treat == "Control")[, var], sd, na.rm = T)
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  # that completed postpartum follow-up
  trt_mean_pnc = colMeans(filter(pnc_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_sd_pnc = sapply(filter(pnc_data, PBL_treat == "Treated")[, var], sd, na.rm = T)
  
  # Print summary statistics, rounded to 2 decimal points
  print("Balance among baseline sample that completed postpartum follow-up:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean (SD): ", round(overall_mean_pnc, 2), " (", round(overall_sd_pnc, 2), ")"))
  print(paste0("Control Mean (SD): ", round(ctrl_mean_pnc, 2), " (", round(ctrl_sd_pnc, 2), ")"))
  print(paste0("Treated Mean (SD): ", round(trt_mean_pnc, 2), " (", round(trt_sd_pnc, 2), ")"))
}

# Binary variables
var_bin <- c("PBL_edu_sec", "PBL_read_kisw_eng", "PBL_married", "PBL_Q707_ownland", 
             "PBL_improved_water", "PBL_improved_toilet", "PBL_travel_vehicle", "PBL_Q713_workforpay", 
             "PBL_save_med", "PBL_Q716_own_mobile_phn", "PBL_text_freq", "PBL_Q720_received_preg_sms", 
             "PBL_Q401_anc_attd", "PBL_high_risk", "PBL_prev_preg_high_risk")
for(var in var_bin){
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  overall_mean_bsl = colMeans(prompts_data[, var], na.rm = T)
  overall_frac_bsl = paste0(table(prompts_data[, var])[2], "/", sum(table(prompts_data[, var])))
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  ctrl_mean_bsl = colMeans(filter(prompts_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_frac_bsl = paste0(table(filter(prompts_data, PBL_treat == "Control")[, var])[2], "/", 
                         sum(table(filter(prompts_data, PBL_treat == "Control")[, var])))
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  trt_mean_bsl = colMeans(filter(prompts_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_frac_bsl = paste0(table(filter(prompts_data, PBL_treat == "Treated")[, var])[2], "/", 
                        sum(table(filter(prompts_data, PBL_treat == "Treated")[, var])))
  
  # Print summary statistics, rounded to 1 decimal point
  print("Balance among full baseline sample:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean: ", round(overall_mean_bsl*100, 1), "% [", 
               overall_frac_bsl, "]"))
  print(paste0("Control Mean: ", round(ctrl_mean_bsl*100, 1), "% [", 
               ctrl_frac_bsl, "]"))
  print(paste0("Treated Mean: ", round(trt_mean_bsl*100, 1), "% [", 
               trt_frac_bsl, "]"))
  
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  # that completed antenatal follow up
  overall_mean_anc = colMeans(anc_data[, var], na.rm = T)
  overall_frac_anc = paste0(table(anc_data[, var])[2], "/", sum(table(anc_data[, var])))
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  # that completed antenatal follow up
  ctrl_mean_anc = colMeans(filter(anc_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_frac_anc = paste0(table(filter(anc_data, PBL_treat == "Control")[, var])[2], "/", 
                         sum(table(filter(anc_data, PBL_treat == "Control")[, var])))
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  # that completed antenatal follow up
  trt_mean_anc = colMeans(filter(anc_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_frac_anc = paste0(table(filter(anc_data, PBL_treat == "Treated")[, var])[2], "/", 
                        sum(table(filter(anc_data, PBL_treat == "Treated")[, var])))
  
  # Print summary statistics, rounded to 1 decimal point
  print("Balance among full baseline sample that completed antenatal follow up:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean: ", round(overall_mean_anc*100, 1), "% [", 
               overall_frac_anc, "]"))
  print(paste0("Control Mean: ", round(ctrl_mean_anc*100, 1), "% [", 
               ctrl_frac_anc, "]"))
  print(paste0("Treated Mean: ", round(trt_mean_anc*100, 1), "% [", 
               trt_frac_anc, "]"))
  
  # Baseline summary statistics (mean, SD) for OVERALL sample enrolled and consented at baseline
  # that completed postpartum follow up
  overall_mean_pnc = colMeans(pnc_data[, var], na.rm = T)
  overall_frac_pnc = paste0(table(pnc_data[, var])[2], "/", sum(table(pnc_data[, var])))
  
  # Baseline summary statistics (mean, SD) for CONTROL sample enrolled and consented at baseline
  # that completed postpartum follow up
  ctrl_mean_pnc = colMeans(filter(pnc_data, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_frac_pnc = paste0(table(filter(pnc_data, PBL_treat == "Control")[, var])[2], "/", 
                         sum(table(filter(pnc_data, PBL_treat == "Control")[, var])))
  
  # Baseline summary statistics (mean, SD) for TREATED sample enrolled and consented at baseline
  # that completed postpartum follow up
  trt_mean_pnc = colMeans(filter(pnc_data, PBL_treat == "Treated")[, var], na.rm = T)
  trt_frac_pnc = paste0(table(filter(pnc_data, PBL_treat == "Treated")[, var])[2], "/", 
                        sum(table(filter(pnc_data, PBL_treat == "Treated")[, var])))
  
  # Print summary statistics, rounded to 1 decimal point
  print("Balance among full baseline sample that completed postpartum follow up:")
  print(paste0("Variable: ", var))
  print(paste0("Overall Mean: ", round(overall_mean_pnc*100, 1), "% [", 
               overall_frac_pnc, "]"))
  print(paste0("Control Mean: ", round(ctrl_mean_pnc*100, 1), "% [", 
               ctrl_frac_pnc, "]"))
  print(paste0("Treated Mean: ", round(trt_mean_pnc*100, 1), "% [", 
               trt_frac_pnc, "]"))
}

### 2. ASSESS ATTRITION AT ANTENATAL AND POSTPARTUM FOLLOW-UP

# Attrition among targeted and eligible participants at antenatal follow-up
outcomes_data$PAN_attrition <- case_when(
  is.na(outcomes_data$PAN_status) ~ NA,
  outcomes_data$PAN_status == 13 ~ NA,
  outcomes_data$PAN_status == 1 ~ 0,
  .default = 1)
table(outcomes_data$PAN_attrition, outcomes_data$PBL_treat)
# Attrition rate in CONTROL arm
ctrl_mean_anc = colMeans(filter(outcomes_data, PBL_treat == "Control")[, "PAN_attrition"], na.rm = T)
ctrl_frac_anc = paste0(table(filter(outcomes_data, PBL_treat == "Control")[, "PAN_attrition"])[2], "/", 
                       sum(table(filter(outcomes_data, PBL_treat == "Control")[, "PAN_attrition"])))
# Attrition rate in TREATED arm
trt_mean_anc = colMeans(filter(outcomes_data, PBL_treat == "Treated")[, "PAN_attrition"], na.rm = T)
trt_frac_anc = paste0(table(filter(outcomes_data, PBL_treat == "Treated")[, "PAN_attrition"])[2], "/", 
                      sum(table(filter(outcomes_data, PBL_treat == "Treated")[, "PAN_attrition"])))
# P-value of difference across arms: from regression adjusted for treatment indicator and 
# recruitment-facility-level normal vaginal birth volume tertile at baseline; standard errors
# clustered by recruitment facility
mod_unadj_anc <- lm(PAN_attrition ~ PBL_treatment + factor(PBL_tertile_deliver_nor), data = outcomes_data)
pval_anc <- coeftest(mod_unadj_anc, vcov = vcovCL, cluster = outcomes_data$PBL_Q102_facilityid)["PBL_treatment", 4]

print("Antenatal follow-up attrition:")
print(paste0("Control: ", round(ctrl_mean_anc*100, 1), "% [", 
             ctrl_frac_anc, "]"))
print(paste0("Treated: ", round(trt_mean_anc*100, 1), "% [", 
             trt_frac_anc, "]"))
print(paste0("p-Value of Difference: ", round(pval_anc, 3)))

# Attrition among targeted and eligible participants at postpartum follow-up
outcomes_data$PPN_attrition <- case_when(
  is.na(outcomes_data$PPN_status) ~ NA,
  outcomes_data$PPN_status == "7 weeks or less (rescheduled)" ~ NA,
  outcomes_data$PPN_status == "Completed" ~ 0,
  .default = 1)
table(outcomes_data$PPN_attrition, outcomes_data$PBL_treat)
# Attrition rate in CONTROL arm
ctrl_mean_pnc = colMeans(filter(outcomes_data, PBL_treat == "Control")[, "PPN_attrition"], na.rm = T)
ctrl_frac_pnc = paste0(table(filter(outcomes_data, PBL_treat == "Control")[, "PPN_attrition"])[2], "/", 
                       sum(table(filter(outcomes_data, PBL_treat == "Control")[, "PPN_attrition"])))
# Attrition rate in TREATED arm
trt_mean_pnc = colMeans(filter(outcomes_data, PBL_treat == "Treated")[, "PPN_attrition"], na.rm = T)
trt_frac_pnc = paste0(table(filter(outcomes_data, PBL_treat == "Treated")[, "PPN_attrition"])[2], "/", 
                      sum(table(filter(outcomes_data, PBL_treat == "Treated")[, "PPN_attrition"])))
# P-value of difference across arms: from regression adjusted for treatment indicator and 
# recruitment-facility-level normal vaginal birth volume tertile at baseline; standard errors
# clustered by recruitment facility
mod_unadj_pnc <- lm(PPN_attrition ~ PBL_treatment + factor(PBL_tertile_deliver_nor), data = outcomes_data)
pval_pnc <- coeftest(mod_unadj_pnc, vcov = vcovCL, cluster = outcomes_data$PBL_Q102_facilityid)["PBL_treatment", 4]

print("Postpartum follow-up attrition:")
print(paste0("Control: ", round(ctrl_mean_pnc*100, 1), "% [", 
             ctrl_frac_pnc, "]"))
print(paste0("Treated: ", round(trt_mean_pnc*100, 1), "% [", 
             trt_frac_pnc, "]"))
print(paste0("p-Value of Difference: ", round(pval_pnc, 3)))

### 3. GENERATE SUMMARY INDICES FOR EACH DOMAIN

# Index 1: Knowledge
i1_vars <- c("PAN_labor_sign_n", "PAN_knowledge_s_f", "PPN_postnatal_knowledge_s_f", "PPN_neonatal_knowledge_s_f")
i1_vars_df <- outcomes_data %>% select(caseid, PBL_treat, all_of(i1_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i1_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i1_df <- i1_vars_df %>% select(caseid, PBL_treat)
i1_df_weights <- i1_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i1_vars){
  ctrl_mean = colMeans(filter(i1_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i1_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i1_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i1_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i1_df <- cbind(i1_df, var_df[, var])
  i1_df_weights <- cbind(i1_df_weights, std_var_for_weights)
}
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i1_weights <- solve(cov(select(i1_df_weights, all_of(i1_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i1_df$i1w <- i1_df %>% select(all_of(i1_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i1_weights, na.rm = T)
i1_df <- i1_df %>% select(caseid, i1w)
outcomes_data <- left_join(outcomes_data, i1_df)

# Index 2: Birth Preparedness
i2_vars <- c("PAN_birth_prep", "PAN_bstfeed_plan", "short_intvl_deliv")
i2_vars_df <- outcomes_data %>% select(caseid, PBL_treat, all_of(i2_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i2_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i2_df <- i2_vars_df %>% select(caseid, PBL_treat)
i2_df_weights <- i2_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i2_vars){
  ctrl_mean = colMeans(filter(i2_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i2_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i2_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i2_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i2_df <- cbind(i2_df, var_df[, var])
  i2_df_weights <- cbind(i2_df_weights, std_var_for_weights)
}
# Ensure directional consistency in interpretation of all component outcomes
i2_df$short_intvl_deliv <- i2_df$short_intvl_deliv * -1
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i2_weights <- solve(cov(select(i2_df_weights, all_of(i2_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i2_df$i2w <- i2_df %>% select(all_of(i2_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i2_weights, na.rm = T)
i2_df <- i2_df %>% select(caseid, i2w) # NEW
outcomes_data <- left_join(outcomes_data, i2_df)

# Index 3: Routine Care-Seeking | UPDATED TO OUTCOMES_DATA FOR PAN_ANY_CARE
i3_vars <- c("ANC_visits", "PNC_ownhealth", "ANC_4visits", "PNC_adequate", 
             "PPN_any_baby_care", "PPN_any_mum_care", "PAN_any_care", "PPN_del_facility")
i3_vars_df <- outcomes_data %>% select(caseid, PBL_treat, all_of(i3_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i3_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i3_df <- i3_vars_df %>% select(caseid, PBL_treat)
i3_df_weights <- i3_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i3_vars){
  ctrl_mean = colMeans(filter(i3_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i3_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i3_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i3_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i3_df <- cbind(i3_df, var_df[, var])
  i3_df_weights <- cbind(i3_df_weights, std_var_for_weights)
}
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i3_weights <- solve(cov(select(i3_df_weights, all_of(i3_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i3_df$i3w <- i3_df %>% select(all_of(i3_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i3_weights, na.rm = T)
i3_df <- i3_df %>% select(caseid, i3w)
outcomes_data <- left_join(outcomes_data, i3_df)

# Index 4: Danger Sign Care-Seeking
i4_vars <- c("PAN_danger_care_uncond", "PPN_any_mom_danger_care_uncond", "PPN_any_baby_danger_care_uncond")
i4_vars_df <- outcomes_data %>% select(caseid, PBL_treat, all_of(i4_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i4_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i4_df <- i4_vars_df %>% select(caseid, PBL_treat)
i4_df_weights <- i4_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i4_vars){
  ctrl_mean = colMeans(filter(i4_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i4_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i4_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i4_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i4_df <- cbind(i4_df, var_df[, var])
  i4_df_weights <- cbind(i4_df_weights, std_var_for_weights)
}
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i4_weights <- solve(cov(select(i4_df_weights, all_of(i4_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i4_df$i4w <- i4_df %>% select(all_of(i4_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i4_weights, na.rm = T)
i4_df <- i4_df %>% select(caseid, i4w)
outcomes_data <- left_join(outcomes_data, i4_df)

# Index 5: Newborn Care
i5_vars <- c("exclusive_bf", "PPN_sleep_back", "PPN_sing_talk")
i5_vars_df <- pnc_data %>% select(caseid, PBL_treat, all_of(i5_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i5_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i5_df <- i5_vars_df %>% select(caseid, PBL_treat)
i5_df_weights <- i5_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i5_vars){
  ctrl_mean = colMeans(filter(i5_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i5_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i5_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i5_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i5_df <- cbind(i5_df, var_df[, var])
  i5_df_weights <- cbind(i5_df_weights, std_var_for_weights)
}
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i5_weights <- solve(cov(select(i5_df_weights, all_of(i5_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i5_df$i5w <- i5_df %>% select(all_of(i5_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i5_weights, na.rm = T)
i5_df <- i5_df %>% select(caseid, i5w)
pnc_data <- left_join(pnc_data, i5_df)

# Index 6: Postpartum Care Content
i6_vars <- c("PPN_Q304_checkup_uncond", "PPN_Q308_physi_mom_uncond", "PPN_Q307_fp_mom_uncond",
             "PPN_Q309_cervical_uncond", "PPN_Q305_physi_bb_uncond", "PPN_Q306_bb_imm_uncond")
i6_vars_df <- pnc_data %>% select(caseid, PBL_treat, all_of(i6_vars)) %>% 
  mutate(num_missing = rowSums(is.na(.)),
         all_missing = num_missing == length(i6_vars)) %>%
  filter(all_missing == FALSE) # Exclude observations with missing data for all component variables
i6_df <- i6_vars_df %>% select(caseid, PBL_treat)
i6_df_weights <- i6_vars_df %>% select(caseid, PBL_treat)
# Normalize each component variable
for(var in i6_vars){
  ctrl_mean = colMeans(filter(i6_vars_df, PBL_treat == "Control")[, var], na.rm = T)
  ctrl_sd = sapply(filter(i6_vars_df, PBL_treat == "Control")[, var], sd, na.rm = T)
  trt_mean = colMeans(filter(i6_vars_df, PBL_treat == "Treated")[, var], na.rm = T)
  var_df <- i6_vars_df %>% select(PBL_treat, all_of(var))
  std_var_for_weights <- (var_df[, var] - ctrl_mean)/ctrl_sd
  var_df[, var] <- (var_df[, var] - ctrl_mean)/ctrl_sd
  i6_df <- cbind(i6_df, var_df[, var])
  i6_df_weights <- cbind(i6_df_weights, std_var_for_weights)
}
# For each observation, calculate the inverse-covariance-weighted average across the component variables
i6_weights <- solve(cov(select(i6_df_weights, all_of(i6_vars)), use = "pairwise.complete.obs")) %>% rowSums()
i6_df$i6w <- i6_df %>% select(all_of(i6_vars)) %>% apply(MARGIN = 1, FUN = weighted.mean, w = i6_weights, na.rm = T)
i6_df <- i6_df %>% select(caseid, i6w)
pnc_data <- left_join(pnc_data, i6_df)

### 4. EVALUATE OUTCOMES

# Interventional fidelity: "PAN_Q701_sms_preg"

# i1w (outcomes_data)
# "PAN_labor_sign_n", "PAN_knowledge_s_f", "PPN_postnatal_knowledge_s_f", "PPN_neonatal_knowledge_s_f"

# i2w (outcomes_data)
# "PAN_birth_prep", "PAN_bstfeed_plan", "short_intvl_deliv"

# i3w (outcomes_data)
# "ANC_visits", "PNC_ownhealth", "ANC_4visits", "PNC_adequate", 
# "PPN_del_facility", "PAN_any_care", "PPN_any_mum_care", "PPN_any_baby_care"

# i4w (outcomes_data)
# "PAN_danger_care_uncond", "PPN_any_mom_danger_care_uncond", "PPN_any_baby_danger_care_uncond"

# i5w (pnc_data)
# "exclusive_bf", "PPN_sleep_back", "PPN_sing_talk"

# i6w (pnc_data)
# "PPN_Q304_checkup_uncond", "PPN_Q305_physi_bb_uncond", "PPN_Q306_bb_imm_uncond", 
# "PPN_Q307_fp_mom_uncond", "PPN_Q308_physi_mom_uncond", "PPN_Q309_cervical_uncond"

# Additional Outcomes Not Reported in Main Manuscript
# "knowledge_pooled", "danger_care_uncond_pooled", "PAN_respect", "PPN_respect", "PAN_empowerment", 
# "PPN_questions_answered", "PAN_Q407_ifas"

# Results for outcomes_data dataset
varlist <- c("") # Vector of variables to be evaluated
for(var in varlist){
  # Control and treated means
  filter(outcomes_data, PBL_treat == "Control")[, var] %>% colMeans(na.rm = T) %>%
    round(4) %>% print()
  filter(outcomes_data, PBL_treat == "Treated")[, var] %>% colMeans(na.rm = T) %>%
    round(4) %>% print()
  # Unadjusted treatment effect: only includes covariates that account for  randomization procedure 
  # (e.g., recruitment facility’s baseline normal vaginal birth volume tertile)
  mod_unadj <- lm(pull(outcomes_data[, var]) ~ outcomes_data$PBL_treatment + 
                    factor(outcomes_data$PBL_tertile_deliver_nor))
  # Cluster standard errors by recruitment facility
  coef_unadj_cl <- coeftest(mod_unadj, vcov = vcovCL, 
                            cluster = outcomes_data$PBL_Q102_facilityid)
  ci_unadj_cl <- coefci(mod_unadj, vcov = vcovCL, # 95% CI
                        cluster = outcomes_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_unadj_cl["outcomes_data$PBL_treatment", c(1,4)], 5), nsmall = 5))
  print(format(round(ci_unadj_cl["outcomes_data$PBL_treatment", ], 2), nsmall = 2))
  # Adjusted treatment effect: adds baseline individual- and recruitment-facility-level covariates
  mod_adj <- lm(pull(outcomes_data[, var]) ~ outcomes_data$PBL_treatment + 
                  factor(outcomes_data$PBL_tertile_deliver_nor) + 
                  factor(outcomes_data$PBL_county) + 
                  factor(outcomes_data$PBL_level_num) + outcomes_data$PBL_gestation_age + 
                  outcomes_data$PBL_age + outcomes_data$PBL_first_pregnancy + 
                  outcomes_data$PBL_edu_sec + outcomes_data$PBL_adequate_anc + 
                  outcomes_data$PBL_Q720_received_preg_sms)
  # Cluster standard errors by recruitment facility
  coef_adj_cl <- coeftest(mod_adj, vcov = vcovCL, 
                          cluster = outcomes_data$PBL_Q102_facilityid)
  ci_adj_cl <- coefci(mod_adj, vcov = vcovCL, # 95% CI
                      cluster = outcomes_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_adj_cl["outcomes_data$PBL_treatment", c(1,4)], 5), nsmall = 5))
  print(format(round(ci_adj_cl["outcomes_data$PBL_treatment", ], 2), nsmall = 2))
}

# Results for anc_data dataset
varlist <- c("") # Vector of variables to be evaluated
for(var in varlist){
  # Control and treated means
  filter(anc_data, PBL_treat == "Control")[, var] %>% colMeans(na.rm = T) %>%
    round(3) %>% print()
  filter(anc_data, PBL_treat == "Treated")[, var] %>% colMeans(na.rm = T) %>%
    round(3) %>% print()
  # Unadjusted treatment effect: only includes covariates that account for  randomization procedure 
  # (e.g., recruitment facility’s baseline normal vaginal birth volume tertile)
  mod_unadj <- lm(pull(anc_data[, var]) ~ anc_data$PBL_treatment + 
                    factor(anc_data$PBL_tertile_deliver_nor))
  # Cluster standard errors by recruitment facility
  coef_unadj_cl <- coeftest(mod_unadj, vcov = vcovCL, 
                            cluster = anc_data$PBL_Q102_facilityid)
  ci_unadj_cl <- coefci(mod_unadj, vcov = vcovCL, # 95% CI
                        cluster = anc_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_unadj_cl["anc_data$PBL_treatment", c(1,4)], 5), nsmall = 5))
  print(format(round(ci_unadj_cl["anc_data$PBL_treatment", ], 2), nsmall = 2))
  # Adjusted treatment effect: adds baseline individual- and recruitment-facility-level covariates
  mod_adj <- lm(pull(anc_data[, var]) ~ anc_data$PBL_treatment + 
                  factor(anc_data$PBL_tertile_deliver_nor) + 
                  factor(anc_data$PBL_county) + 
                  factor(anc_data$PBL_level_num) + anc_data$PBL_gestation_age + 
                  anc_data$PBL_age + anc_data$PBL_first_pregnancy + 
                  anc_data$PBL_edu_sec + anc_data$PBL_adequate_anc + 
                  anc_data$PBL_Q720_received_preg_sms)
  # Cluster standard errors by recruitment facility
  coef_adj_cl <- coeftest(mod_adj, vcov = vcovCL, 
                          cluster = anc_data$PBL_Q102_facilityid)
  ci_adj_cl <- coefci(mod_adj, vcov = vcovCL, # 95% CI
                      cluster = anc_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_adj_cl["anc_data$PBL_treatment", c(1,4)], 10), nsmall = 10))
  print(format(round(ci_adj_cl["anc_data$PBL_treatment", ], 3), nsmall = 3))
}

# Results for pnc_data dataset
varlist <- c("") # Vector of variables to be evaluated
for(var in varlist){
  # Control and treated means
  filter(pnc_data, PBL_treat == "Control")[, var] %>% colMeans(na.rm = T) %>%
    round(2) %>% print()
  filter(pnc_data, PBL_treat == "Treated")[, var] %>% colMeans(na.rm = T) %>%
    round(2) %>% print()
  # Unadjusted treatment effect: only includes covariates that account for  randomization procedure 
  # (e.g., recruitment facility’s baseline normal vaginal birth volume tertile)
  mod_unadj <- lm(pull(pnc_data[, var]) ~ pnc_data$PBL_treatment + 
                    factor(pnc_data$PBL_tertile_deliver_nor))
  # Cluster standard errors by recruitment facility
  coef_unadj_cl <- coeftest(mod_unadj, vcov = vcovCL, 
                            cluster = pnc_data$PBL_Q102_facilityid)
  ci_unadj_cl <- coefci(mod_unadj, vcov = vcovCL, # 95% CI
                        cluster = pnc_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_unadj_cl["pnc_data$PBL_treatment", c(1,4)], 5), nsmall = 5))
  print(format(round(ci_unadj_cl["pnc_data$PBL_treatment", ], 2), nsmall = 2))
  # Adjusted treatment effect: adds baseline individual- and recruitment-facility-level covariates
  mod_adj <- lm(pull(pnc_data[, var]) ~ pnc_data$PBL_treatment + 
                  factor(pnc_data$PBL_tertile_deliver_nor) + 
                  factor(pnc_data$PBL_county) + 
                  factor(pnc_data$PBL_level_num) + pnc_data$PBL_gestation_age + 
                  pnc_data$PBL_age + pnc_data$PBL_first_pregnancy + 
                  pnc_data$PBL_edu_sec + pnc_data$PBL_adequate_anc + 
                  pnc_data$PBL_Q720_received_preg_sms)
  # Cluster standard errors by recruitment facility
  coef_adj_cl <- coeftest(mod_adj, vcov = vcovCL, 
                          cluster = pnc_data$PBL_Q102_facilityid)
  ci_adj_cl <- coefci(mod_adj, vcov = vcovCL, # 95% CI
                      cluster = pnc_data$PBL_Q102_facilityid)
  # Print coefficient estimate and corresponding 95% CI/p-value
  print(format(round(coef_adj_cl["pnc_data$PBL_treatment", c(1,4)], 10), nsmall = 10))
  print(format(round(ci_adj_cl["pnc_data$PBL_treatment", ], 3), nsmall = 3))
}

### 5. HOLM-BONFERRONI-ADJUSTED P-VALUES

raw_pvals <- c() # Vector of unadjusted p-values
# Adjust p-values with Holm method to account for multiplicity
format(round(p.adjust(raw_pvals, method = "holm"), 3), nsmall = 3)
