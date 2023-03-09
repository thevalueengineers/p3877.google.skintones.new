library(readxl)
library(dplyr)
library(targets)
library(stringr)
library(purrr)
library(stats)

tar_load(se_data)

#names to lower
names(se_data) <- tolower(names(se_data))

skin_rep_own <- c("q14","q27","qid46","q66")
skin_rep_community <- c("q15","q28","q50","q65")
skin_rep_country <- c("q16","q29","q49","q64")
confident_picked_owntone <- c("q23","q36","q42","q57")
confident_picked_owntone <- c("q23","q36","q42","q57")
closest_skin_tone <- c("q9","q37","q203","q199")

se_data <-se_data %>%
  mutate(across(where(is.character), str_trim)) %>%
  #convert age to numeric
  mutate(age=as.numeric(q2)) %>%
  #recode gender
  mutate(gender1 = as.factor(ifelse(q3 == "Man","Male","Not Male"))) %>%
  #convert market to factor
  mutate(q83 = factor(q83))

  #agreement scales to numeric
skin_rep_own_dat <- se_data %>%
    select(all_of(skin_rep_own)) %>%
  map_dfc(~ case_when(
    .x == 'Strongly agree' ~ 2,
    .x =='Somewhat agree' ~ 1,
    .x =='Neither agree nor disagree' ~ 0,
    .x =='Somewhat disagree' ~ -1,
    .x =='Strongly disagree' ~ -2,
    TRUE ~ 0
  ), .cols = skin_rep_own) %>%
  set_names(paste0(skin_rep_own, "_n")) %>%
  bind_cols(se_data)

#analysis of My own skin tone is represented in this scale. (Primary DV)

#compare means across the markets - NEED TO ADD TOTAL ROW
skin_rep_own_dat %>%
  group_by(q83) %>%
  summarise(across(paste0(skin_rep_own, "_n"), ~ mean(., na.rm = TRUE)))

# Perform one-way ANOVA
skin_rep_own_anova <- aov(q14_n ~ q83, data = skin_rep_own_dat)
summary(skin_rep_own_anova)
# Perform Tukey test
tukey_results <- TukeyHSD(skin_rep_own_anova)
# View results
tukey_results

#NEXT STEPS
#compare mean of different scales at an overall level
#compare mean of different scales at a within market level
#compare mean of same scales across markets - automate this using code below as a base
#compare mean of same scales across skin tone selection (recoding needed for skin tone selection)
#compare mean of same scales across gender



# # Specify the variables to analyze
# skin_rep_own_vars <- paste0(skin_rep_own, "_n")
#
# # Perform ANOVA and Tukey test for each variable
# anova_results <- map(skin_rep_own_vars, function(var) {
#
#   # Perform ANOVA
#   anova_model <- aov(formula = as.formula(paste(var, "~ q83")), data = skin_rep_own_dat)
#   anova_summary <- summary(anova_model)
#
#   # Perform Tukey test
#   tukey_results <- TukeyHSD(anova_model)
#
#   # Return a list of results
#   list(anova_summary = anova_summary, tukey_results = tukey_results)
# })
#
# # View ANOVA results for the first variable
# anova_results[[1]]$anova_summary
#
# # View Tukey test results for the first variable
# anova_results[[]]$tukey_results
