rm(list = ls())

library(tidyverse)
library(gtsummary)

load("C:/Users/fallouch/Box/PhD/CHERISH/CHERISH/deidRaw/output/cherish.RData")

cherish <- cherish %>% 
  select(q7.factor, q8.factor,
         q9.factor, q10.factor, q12.factor, q14.factor, 
         a_asking_questions_to_your.factor, b_traveling_to_medical_app.factor, c_understanding_medical_tr.factor,
         a_religious_spiritual_beli.factor, b_i_tend_to_avoid_things_h.factor) %>% 
  na.omit() %>% 
  mutate(q7.factor = case_when(q7.factor == "Yes" ~ 1,
                               q7.factor == "No" ~ 0),
         q8.factor = case_when(q8.factor == "Yes" ~ 1,
                               q8.factor == "No" ~ 0),
         q9.factor = case_when(q9.factor == "Yes" ~ 1,
                               q9.factor == "No" ~ 0),
         q10.factor = case_when(q10.factor == "Yes" ~ 1,
                                q10.factor == "No" ~ 0),
         q12.factor = case_when(q12.factor == "Yes" ~ 1,
                                q12.factor == "No" ~ 0),
         q14.factor = case_when(q14.factor == "Yes" ~ 1,
                                q14.factor == "No" ~ 0),
         a_asking_questions_to_your.factor_numeric = case_when(a_asking_questions_to_your.factor == "Not at All" ~ 0,
                                                               a_asking_questions_to_your.factor == "A Little Bit" ~ 1,
                                                               a_asking_questions_to_your.factor == "Somewhat" ~ 2,
                                                               a_asking_questions_to_your.factor == "Quite a Bit" ~ 3,
                                                               a_asking_questions_to_your.factor == "Extremely" ~ 4),
         b_traveling_to_medical_app.factor_numeric = case_when(b_traveling_to_medical_app.factor == "Not at All" ~ 0,
                                                               b_traveling_to_medical_app.factor == "A Little Bit" ~ 1,
                                                               b_traveling_to_medical_app.factor == "Somewhat" ~ 2,
                                                               b_traveling_to_medical_app.factor == "Quite a Bit" ~ 3,
                                                               b_traveling_to_medical_app.factor == "Extremely" ~ 4),  
         c_understanding_medical_tr.factor_numeric = case_when(c_understanding_medical_tr.factor == "Not at All" ~ 0,
                                                               c_understanding_medical_tr.factor == "A Little Bit" ~ 1,
                                                               c_understanding_medical_tr.factor == "Somewhat" ~ 2,
                                                               c_understanding_medical_tr.factor == "Quite a Bit" ~ 3,
                                                               c_understanding_medical_tr.factor == "Extremely" ~ 4), 
         a_religious_spiritual_beli.factor_numeric = case_when(a_religious_spiritual_beli.factor == "Strongly Disagree" ~ 0,
                                                               a_religious_spiritual_beli.factor == "Somewhat Disagree" ~ 1,
                                                               a_religious_spiritual_beli.factor == "Neutral" ~ 2,
                                                               a_religious_spiritual_beli.factor == "Somewhat Agree" ~ 3,
                                                               a_religious_spiritual_beli.factor == "Strongly Agree" ~ 4), 
         b_i_tend_to_avoid_things_h.factor_numeric = case_when(b_i_tend_to_avoid_things_h.factor == "Strongly Disagree" ~ 0,
                                                               b_i_tend_to_avoid_things_h.factor == "Somewhat Disagree" ~ 1,
                                                               b_i_tend_to_avoid_things_h.factor == "Neutral" ~ 2,
                                                               b_i_tend_to_avoid_things_h.factor == "Somewhat Agree" ~ 3,
                                                               b_i_tend_to_avoid_things_h.factor == "Strongly Agree" ~ 4), 
         good_health_behaviors_dich = ifelse((q7.factor == 1 |
                                               q8.factor == 0), 1, 0),
         good_health_behaviors_cont = case_when((q7.factor == 0 &
                                                   q8.factor == 1) ~ 0,
                                                ((q7.factor == 1 &
                                                  q8.factor == 1) |
                                                  (q7.factor == 0 &
                                                     q8.factor == 0)) ~ 1,
                                                (q7.factor == 1 &
                                                   q8.factor == 0) ~ 2),
         adverse_health_outcomes_dich = ifelse((q10.factor == 1 |
                                                 q12.factor == 1 | 
                                                 q14.factor == 1), 1, 0),
         adverse_health_outcomes_cont = q10.factor + q12.factor + q14.factor,
         pa_backwards = ifelse(q7.factor == 0, 1, 0),
         adverse_health_cont = pa_backwards + q8.factor + q9.factor + q10.factor + q12.factor + q14.factor,
         confidence_cont = a_asking_questions_to_your.factor_numeric + b_traveling_to_medical_app.factor_numeric + c_understanding_medical_tr.factor_numeric,
         confidence_dich = ifelse(confidence_cont >= 10, 1, 0),
         religion_cont = a_religious_spiritual_beli.factor_numeric + b_i_tend_to_avoid_things_h.factor_numeric,
         religion_dich = ifelse(religion_cont >= 6, 1, 0))

# Health behaviors (physical activity and smoking)
ggplot(aes(x = good_health_behaviors_cont), data = cherish) +
  geom_bar()

cor.test(cherish$religion_cont, cherish$good_health_behaviors_cont)
ggplot(aes(x = religion_cont, y = good_health_behaviors_cont), data = cherish) +
  geom_point() +
  geom_smooth()

table(cherish$good_health_behaviors_dich)
glm(good_health_behaviors_dich ~ religion_cont,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)

glm(good_health_behaviors_dich ~ religion_dich,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)

# Adverse health outcomes (hypertension, cholesterol, and diabetes)
ggplot(aes(x = adverse_health_outcomes_cont), data = cherish) +
  geom_bar()

cor.test(cherish$religion_cont, cherish$adverse_health_outcomes_cont)
ggplot(aes(x = religion_cont, y = adverse_health_outcomes_cont), data = cherish) +
  geom_point() +
  geom_smooth()

table(cherish$adverse_health_outcomes_dich)
glm(adverse_health_outcomes_dich ~ religion_cont,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)

glm(adverse_health_outcomes_dich ~ religion_dich,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)

# Adverse health (smoking, physical activity, weight, hypertension, cholesterol, and diabetes)
ggplot(aes(x = adverse_health_cont), data = cherish) +
  geom_bar()

cor.test(cherish$religion_cont, cherish$adverse_health_cont)
ggplot(aes(x = religion_cont, y = adverse_health_cont), data = cherish) +
  geom_point() +
  geom_smooth()

# Confidence
ggplot(aes(x = confidence_cont), data = cherish) +
  geom_bar()

cor.test(cherish$religion_cont, cherish$confidence_cont)
ggplot(aes(x = religion_cont, y = confidence_cont), data = cherish) +
  geom_point() +
  geom_smooth()

table(cherish$confidence_dich)
glm(confidence_dich ~ religion_cont,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)

glm(confidence_dich ~ religion_dich,
            data = cherish,
            family = binomial(link = "logit")) %>% 
  tbl_regression(exponentiate = TRUE)
