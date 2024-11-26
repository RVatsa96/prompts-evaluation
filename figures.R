###################################################################
### Jacaranda Health PROMPTS Evaluation ###########################
###################################################################

### REQUIRES: PROMPTS_Full.dta 
### - Not provided per ethical approval under which study data were collected

### WRITTEN BY: Rajet Vatsa, Wei Chang

### OVERVIEW: 1. Main Manuscript Figure 4
###           2. Main Manuscript Figure 5

# Load necessary libraries
library(haven); library(lmtest); library(sandwich); library(tidyverse)

# Import cleaned and merged PROMPTS dataset
data_dir <- "~/Dropbox (Harvard University)/JH Eval Data/Harvard Analysis/Data/Clean/"
prompts_data <- read_dta(paste0(data_dir, "PROMPTS_Full.dta"))
anc_data <- prompts_data %>% filter(PAN_final_status == 1) # 3399 observations from antenatal f/u survey
pnc_data <- prompts_data %>% filter(PPN_status == "Completed") # 5509 observations from postpartum f/u survey
outcomes_data <- prompts_data %>% filter(anc_fu == 1 | pnc_fu == 1) # 6139 observations from full sample

### 1. GENERATE FIGURE 4 (HISTOGRAMS OF ANTENATAL AND POSTNATAL CARE UTILIZATION)

# Antenatal care utilization, by intervention arm
ggplot() + 
  # Histogram for CONTROL arm; bin width of 1 visit
  geom_histogram(data = filter(pnc_data, PBL_treatment == 0), 
                 mapping = aes(x = ANC_visits, y = after_stat(count / sum(count))),
                 fill = "#757575", binwidth = 1, boundary = 0, closed = "left", alpha = 0.7) + 
  # Histogram for TREATED arm; bin width of 1 visit
  geom_histogram(data = filter(pnc_data, PBL_treatment == 1), 
                 mapping = aes(x = ANC_visits, y = after_stat(count / sum(count))),
                 fill = "#235597", color = "#235597", binwidth = 1, boundary = 0, closed = "left", alpha = 0.5) + 
  annotate("text", x = 0.25, y = 0.35, label = "Treatment", color = "#235597", size = 3.5, fontface = 2,
           hjust = 0) + 
  annotate("text", x = 0.25, y = 0.315, label = "Control", color = "#757575", size = 3.5, fontface = 2,
           hjust = 0) + 
  geom_vline(xintercept = 4, color = "black", lty = 2) + 
  theme_classic() + labs(x = "Number of ANC Visits",
                         y = "Frequency") + 
  scale_x_continuous(breaks=seq(0,16,4),labels=seq(0,16,4)) + 
  theme(axis.text.x = element_text(colour = c('black', 'black','black', 'black', 'black')),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

# Postnatal care utilization, by intervention arm
ggplot() + 
  # Histogram for CONTROL arm; bin width of 1 visit
  geom_histogram(data = filter(pnc_data, PBL_treatment == 0), 
                 mapping = aes(x = PPN_Q302_pnc_no, y = after_stat(count / sum(count))),
                 fill = "#757575", binwidth = 1, boundary = 0, closed = "left", alpha = 0.7) + 
  # Histogram for TREATED arm; bin width of 1 visit
  geom_histogram(data = filter(pnc_data, PBL_treatment == 1), 
                 mapping = aes(x = PPN_Q302_pnc_no, y = after_stat(count / sum(count))),
                 fill = "#235597", color = "#235597", binwidth = 1, boundary = 0, closed = "left", alpha = 0.5) + 
  annotate("text", x = 0, y = 0.75, label = "Treatment", color = "#235597", size = 3.5, fontface = 2,
           hjust = 0) +
  annotate("text", x = 0, y = 0.675, label = "Control", color = "#757575", size = 3.5, fontface = 2,
           hjust = 0) + 
  theme_classic() + labs(x = "Number of PNC Visits",
                         y = "Frequency") + 
  geom_vline(xintercept = 2.0, col = "black", lty = 2) + 
  scale_x_continuous(breaks=seq(0,10,2),labels=seq(0,10,2)) + 
  theme(axis.text.x = element_text(colour = c('black', 'black','black', 'black', 'black', 'black')),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))

### 2. GENERATE FIGURE 5 (POSTPARTUM CARE CONTENT DOMAIN EFFECT SIZES)

# Postpartum care content domain component variables
varlist <- c("PPN_Q304_checkup_uncond", "PPN_Q305_physi_bb_uncond", "PPN_Q306_bb_imm_uncond",
             "PPN_Q307_fp_mom_uncond", "PPN_Q308_physi_mom_uncond", "PPN_Q309_cervical_uncond")
fig_care_content_df <- NA # Initialize data frame for figure
# Generate data frame storing each outcome's unadjusted and adjusted effect estimate
for(var in varlist){
  # Control arm mean
  ctrl_mean <- filter(pnc_data, PBL_treat == "Control")[, var] %>% 
    colMeans(na.rm = T) %>% round(3)
  # Unadjusted treatment effect: only includes covariates that account for  randomization procedure 
  # (e.g., recruitment facility’s baseline normal vaginal birth volume tertile)
  mod_unadj <- lm(pull(pnc_data[, var]) ~ pnc_data$PBL_treatment + 
                    factor(pnc_data$PBL_tertile_deliver_nor))
  # Cluster standard errors by recruitment facility
  coef_unadj_cl <- coeftest(mod_unadj, vcov = vcovCL, 
                            cluster = pnc_data$PBL_Q102_facilityid)
  ci_unadj_cl <- coefci(mod_unadj, vcov = vcovCL, # 95% CI
                        cluster = pnc_data$PBL_Q102_facilityid)
  # Adjusted treatment effect: adds baseline individual- and recruitment-facility-level covariates
  mod_adj <- lm(pull(pnc_data[, var]) ~ pnc_data$PBL_treatment + 
                  factor(pnc_data$PBL_tertile_deliver_nor) + 
                  factor(pnc_data$PBL_county) + 
                  factor(pnc_data$PBL_level_num) + pnc_data$PBL_gestation_age + 
                  pnc_data$PBL_age + pnc_data$PBL_first_pregnancy + 
                  pnc_data$PBL_edu_sec + pnc_data$PBL_adequate_anc)
  # Cluster standard errors by recruitment facility
  coef_adj_cl <- coeftest(mod_adj, vcov = vcovCL, 
                          cluster = pnc_data$PBL_Q102_facilityid)
  ci_adj_cl <- coefci(mod_adj, vcov = vcovCL, # 95% CI
                      cluster = pnc_data$PBL_Q102_facilityid)
  # Treatment effect plot
  trt_eff_df = data.frame(Variable = rep(var, 2),
                          Mean = rep(ctrl_mean, 2),
                          Model = c("Unadjusted", "Adjusted"),
                          Coef = c(coef_unadj_cl["pnc_data$PBL_treatment", 1], # Coefficient estimate
                                   coef_adj_cl["pnc_data$PBL_treatment", 1]),
                          Lower = c(ci_unadj_cl["pnc_data$PBL_treatment", 1], # 95% CI lower bound
                                    ci_adj_cl["pnc_data$PBL_treatment", 1]),
                          Upper = c(ci_unadj_cl["pnc_data$PBL_treatment", 2], # 95% CI upper bound
                                    ci_adj_cl["pnc_data$PBL_treatment", 2]))
  fig_care_content_df <- rbind(fig_care_content_df, trt_eff_df)
}
fig_care_content_df <- drop_na(fig_care_content_df)
# Relabel variables for manuscript figure; include control arm sample mean
varlabel_list <- c("Mother's health discussed\n(Control Mean: 0.63)",
                   "Provider conducted\n physical exam for newborn\n(Control Mean: 0.95)",
                   "Provider administered\n immunization to newborn\n(Control Mean: 0.98)",
                   "Provider discussed\n family planning with mother\n(Control Mean: 0.53)",
                   "Provider conducted\n physical exam for mother\n(Control Mean: 0.54)",
                   "Provider offered mother\n cervical cancer screening\n(Control Mean: 0.11)")
fig_care_content_df$VarLabel <- rep(varlabel_list, each = 2)
# Re-order variables for manuscript figure
fig_care_content_df$VarLabel <- factor(fig_care_content_df$VarLabel, 
                                       levels = c(varlabel_list[3], varlabel_list[2], varlabel_list[6],
                                                  varlabel_list[4], varlabel_list[5], varlabel_list[1]))
# Plot all effect sizes (unadjusted in gray; adjusted in black): point estimates with corresponding 95% CI
ggplot(data = fig_care_content_df) + 
  geom_pointrange(aes(x = VarLabel, y = Coef, ymin = Lower, ymax = Upper, color = Model), 
                 position = position_dodge(width = 0.6), size = 0.5) + 
  geom_hline(yintercept = 0, color = "#757575", lty = 2) + 
  scale_color_manual(values=c("#235597","lightskyblue3")) + labs(x = "", y = "Treatment Effect Estimate",
                                                      title = "Postpartum Care Content Index Component Measures") + 
  coord_flip() + 
  theme_classic() + theme(legend.position = "bottom", legend.title = element_blank(),
                          plot.title = element_text(size = 12),
                          axis.title.x = element_text(size = 11),
                          axis.text.y = element_text(size = 10),
                          axis.text.x = element_text(size = 10))
