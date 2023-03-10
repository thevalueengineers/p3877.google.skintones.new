library(readxl)
library(dplyr)
library(targets)
library(stringr)
library(purrr)
library(stats)
library(car)
library(tidyr)
library(corrplot)

tar_load(se_data)

#names to lower
names(se_data) <- tolower(names(se_data))

#create list of each of the questions for each group

dependent_vars <- list(

scale_familiar = c("q10","q25","q40","q55"),
skin_rep_own = c("q14","q27","qid46","q66"),
skin_rep_community = c("q15","q28","q50","q65"),
skin_rep_country = c("q16","q29","q49","q64"),
too_many_options = c("q17","q30","q48","q63"),
too_few_options = c("q19","q31","q47","q62"),
scale_rep_light = c("q20","q33","q46","q61"),
scale_rep_medium = c("q21","q32","q45","q60"),
scale_rep_dark = c("q22","q34","q44","q59"),
scale_rep_undertones = c("q18","q35","q43","q58"),
closest_skin_tone = c("q9","q37","q203","q199"), #this doesn't follow the same variable set up
easy_to_find_owntone = c("q24","q39","q53","q69"),
confident_picked_owntone = c("q23","q36","q42","q57")

)

#pivot data

pivot_func <- function(vars) {

  se_data %>%
    select(responseid,all_of(vars)) %>%
    pivot_longer(all_of(vars),names_to = "scale")

}

new_dat <- map_dfr(dependent_vars,pivot_func,.id = "dependent") %>%
  mutate(scale = case_when(
    scale %in% c("q10","q14","q15","q16","q17","q19","q20","q21","q22","q18","q9","q24","q23") ~ "fenty",
    scale %in% c("q25","q27","q28","q29","q30","q31","q33","q32","q34","q35","q37","q39","q36") ~ "fitzpatric",
    scale %in% c("q40","qid46","q50","q49","q48","q47","q46","q45","q44","q43","q203","q53","q42") ~ "monk swatch",
    scale %in% c("q55","q66","q65","q64","q63","q62","q61","q60","q59","q58","q199","q69","q57") ~ "monk orb",
    TRUE ~ scale
  )) %>%
  pivot_wider(names_from = dependent, values_from = value)

#merge in variables we need (e.g. covariates, market variable and demographics)

new_dat <- se_data %>%
  select(responseid,q2:q83,q7,q8,q71:q78) %>%
  left_join(new_dat, by = "responseid") %>%
  as_tibble() %>%
  relocate(scale:confident_picked_owntone, .before = q2)

#UP TO HERE - next steps: do recoding and then set up functions that do analysis

#-----------

#create list of all the groupings together for primary research question

primarylist <- c(skin_rep_own,skin_rep_community,skin_rep_country,confident_picked_owntone,
                 easy_to_find_owntone,too_many_options,too_few_options,
                 scale_rep_light,scale_rep_medium,scale_rep_dark,scale_rep_undertones)

skin_replist  <- c(skin_rep_own,skin_rep_community,skin_rep_country,scale_rep_light,scale_rep_medium,scale_rep_dark,scale_rep_undertones)

#convert subgroups to factors - ADD q7,q8

se_data <- se_data %>%
  mutate(across(where(is.character), str_trim)) %>%
  #convert age to numeric
  mutate(age=as.numeric(q2)) %>%
  #recode gender
  mutate(gender = as.factor(ifelse(q3 == "Man","Male","Not Male"))) %>%
  #convert market to factor
  mutate(market = factor(q83)) %>%
  #makeuponline
  mutate(makeup_online = factor(q72)) %>%
  #sun precautions
  mutate(sun_precautions = factor(q74))

covariateslist <- c(age,gender,makeup_online,sun_precautions)

  #agreement scales (including easy  to difficult) to numeric
se_data <- se_data %>%
    select(all_of(primarylist)) %>%
  map_dfc(~ case_when(
    .x %in% c('Strongly agree' , 'Very easy') ~ 2,
    .x %in% c('Somewhat agree' , 'Somewhat easy') ~ 1,
    .x %in% c('Neither agree nor disagree' , 'Neither easy nor difficult') ~ 0,
    .x %in% c('Somewhat disagree' , 'Somewhat difficult') ~ -1,
    .x %in% c('Strongly disagree' , 'Very difficult') ~ -2,
    TRUE ~ 0
  ), .cols = primarylist) %>%
  set_names(paste0(primarylist, "_n"))%>%
  bind_cols(se_data)

#check recoding as worked - looks good - 48 columns

select(se_data,ends_with("_n"))

#create top box variables

primarylist_n <-  paste0(primarylist, "_n")

se_data <- se_data %>%
  select(all_of(primarylist_n)) %>%
  map_dfc(~ case_when(
    .x == 2 ~1,
    TRUE ~ 0
  ), .cols = primarylist_n) %>%
  set_names(paste0(primarylist_n, "tb")) %>%
  bind_cols(se_data)

#check the top boxes are lining up with 2s
select(se_data,c(ends_with("_ntb")|ends_with("_n"))) %>%   select(order(readr::parse_number(names(.))))

#correlations

matrix <- se_data %>%
  select(ends_with("_n")) %>%
  cor()

corrplot(matrix)

#analysis of My own skin tone is represented in this scale. (Primary DV)

#1) compare mean of different scales at an overall level

se_data %>%
  summarise(across(paste0(primarylist, "_n"), ~ mean(., na.rm = TRUE)))

#compare mean of different scales at a within market level

#compare mean of same scales across markets - automate this using code below as a base

#compare means across the markets - NEED TO ADD TOTAL ROW
se_data %>%
  group_by(market) %>%
  summarise(across(paste0(skin_rep_own, "_n"), ~ mean(., na.rm = TRUE)))

# Perform one-way ANOVA
skin_rep_own_anova <- aov(q14_n ~ market, data = se_data)
summary(skin_rep_own_anova)
# Perform Tukey test
tukey_results <- TukeyHSD(skin_rep_own_anova)
# View results
tukey_results

#compare mean of same scales across skin tone selection (recoding needed for skin tone selection)
#compare mean of same scales across gender

# Specify the variables to analyze
skin_rep_own_vars <- paste0(skin_rep_own, "_n")

# Perform ANOVA and Tukey test for each variable
anova_results <- map(skin_rep_own_vars, function(var) {

  # Perform ANOVA
  anova_model <- aov(formula = as.formula(paste(var, "~ q83")), data = skin_rep_own_dat)
  anova_summary <- summary(anova_model)

  # Perform Tukey test
  tukey_results <- TukeyHSD(anova_model)

  # Return a list of results
  list(anova_summary = anova_summary, tukey_results = tukey_results)
})

# View ANOVA results for the first variable
anova_results[[1]]$anova_summary

# View Tukey test results for the first variable
anova_results[[]]$tukey_results
