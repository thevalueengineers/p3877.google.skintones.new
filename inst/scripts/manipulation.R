library(readxl)
library(dplyr)
library(targets)
library(stringr)
library(purrr)
library(stats)
library(car)
library(tidyr)
library(corrplot)
library(ggplot2)

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
closest_skin_tone = c("q9","q37","q203","q199"), #this doesn't follow the same variable set up. Need to discuss how to analyze this.
easy_to_find_owntone = c("q24","q39","q53","q69"),
confident_picked_owntone = c("q23","q36","q42","q57")

)

#pivot data

pivot_func <- function(vars) {

  se_data %>%
    select(responseid,all_of(vars)) %>%
    pivot_longer(all_of(vars),names_to = "scale")

}

new_data <- map_dfr(dependent_vars,pivot_func,.id = "dependent") %>%
  mutate(scale = case_when(
    scale %in% c("q10","q14","q15","q16","q17","q19","q20","q21","q22","q18","q9","q24","q23") ~ "fenty",
    scale %in% c("q25","q27","q28","q29","q30","q31","q33","q32","q34","q35","q37","q39","q36") ~ "fitzpatric",
    scale %in% c("q40","qid46","q50","q49","q48","q47","q46","q45","q44","q43","q203","q53","q42") ~ "monk swatch",
    scale %in% c("q55","q66","q65","q64","q63","q62","q61","q60","q59","q58","q199","q69","q57") ~ "monk orb",
    TRUE ~ scale
  )) %>%
  pivot_wider(names_from = dependent, values_from = value) %>%
  #merge in variables we need (e.g. covariates, market variable and demographics)
  merge(se_data, by = "responseid") %>%
  as_tibble() %>%
  #recoding and factors
  mutate((across(where(is.character), str_trim))) %>%
  mutate(age = as.numeric(q2),
         gender = as.factor(ifelse(q3 == "Man","Male","Not Male")),
         market = factor(q83),
         makeup_online = factor(q72, levels = c("Yes", "No")),
         sun_precautions = factor(q74, levels = c("Yes", "No"))) %>%
  mutate(ownskintone = as.factor(ifelse(q7 %in% c("Very light","Light"),"Light","Not light")),
         ownskintone = relevel(ownskintone, ref = "Light"),
         friendssimilar = as.factor(ifelse(q8 == "Most of them have a similar skin tone to mine","Similar to mine","Different to mine")),
         friendssimilar = relevel(friendssimilar, ref = "Similar to mine")
         ) %>%
  #recode agreement scales to numeric -2 to +2
  mutate(across(all_of(names(dependent_vars)[-11]),
                ~ case_when(
                  .x %in% c('Strongly agree' , 'Very easy') ~ 2,
                  .x %in% c('Somewhat agree' , 'Somewhat easy') ~ 1,
                  .x %in% c('Neither agree nor disagree' , 'Neither easy nor difficult') ~ 0,
                  .x %in% c('Somewhat disagree' , 'Somewhat difficult') ~ -1,
                  .x %in% c('Strongly disagree' , 'Very difficult') ~ -2,
                  TRUE ~ 0
                ), .names = "{.col}_n")) %>%
    #top box for agreement scales
  mutate(across(all_of(names(dependent_vars)[-11]),
                ~ case_when(
                  .x %in% c('Strongly agree' , 'Very easy') ~ 1,
                  TRUE ~ 0
                ), .names = "{.col}_ntb")) %>%
  #replace NA age with column average
  mutate(across(age, ~replace_na(., mean(., na.rm=TRUE))))


# useful check for making sure recoding has worked
# map(
#   names(dependent_vars)[-11],
#   ~select(check, all_of(c(.x, paste0(.x, "_ntb")))) %>%
#     table(useNA = "always")
# )

#checking effects on dependent variables below
# skin_rep_own
# skin_rep_community
# skin_rep_country
# confident_picked_owntone

independentlist <- list(age,gender,makeup_online,sun_precautions,ownskintone,friends_similar)
dependentlist <- list(skin_rep_own_n,skin_rep_community_n,skin_rep_country_n,confident_picked_owntone_n)

#then cut by: scale - then by: overall, across markets, gender

nested_dat <- bind_rows(
  new_data %>%
    nest(data = -market),
  new_data %>%
    nest(data = -c(market, scale)))


#check if there are significant differences among the three dependent variables based on the independent variables included in the model.

mfit <- manova(cbind(skin_rep_own_n,skin_rep_community_n,skin_rep_country_n)
               ~ age + gender + makeup_online + sun_precautions + ownskintone + friendssimilar, data = new_data)
summary(mfit)

#create a nested table with country and scale taken out to the upper level

#check what variables are more strongly associated with skin rep own
lmmodel <- lm(skin_rep_own_n ~ age + gender + makeup_online + sun_precautions + ownskintone + friendssimilar,data = new_data)
summary(lmmodel)

#check what variables are more strongly associated with skin rep own as a top box
glm <- glm(skin_rep_own_ntb ~ age + gender + makeup_online + sun_precautions + ownskintone + friendssimilar,data = new_data, family = binomial)
summary(glm)

p1 <-  new_data %>% group_by(scale) %>% summarise(m=weighted.mean(skin_rep_own_n),sd=sd(skin_rep_own_n),n=n()) %>% mutate(se=sd/sqrt(n)) %>%
  ggplot(aes(fill=scale,x=na.omit(scale),y=m,label=round(m,2))) +
  geom_col(position='dodge',width=0.5) +
  geom_label(position=position_dodge(0.5), vjust=3, show.legend=FALSE) +
  ylab('Represents own skin tone (+2:strong agree, -2:strong disagree)') +
  xlab('Skin tone scale - overall')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p1 + geom_errorbar(aes(ymin=m-se, ymax=m+se),width=.5,position=position_dodge(0.5))
