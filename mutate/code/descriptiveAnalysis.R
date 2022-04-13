rm(list = ls())

library(tidyverse)
library(gtsummary)

load("C:/Users/fallouch/Box/PhD/CHERISH/CHERISH/deidRaw/output/cherish.RData")

# OVERALL
# Neighborhood
cherish %>% 
  select(q1,
         a_there_are_walkable_sidew.factor, b_there_are_facilities_to.factor, c_my_neighborhood_has_free.factor, d_there_are_many_shops_sto.factor, e_a_large_selection_of_fre.factor,
         a_people_around_here_are_w.factor, b_people_in_my_neighborhoo.factor, c_people_in_my_neighborhoo.factor, d_people_in_my_neighborhoo.factor, e_i_feel_safe_walking_in_m.factor, f_violence_is_a_problem_in.factor) %>% 
  tbl_summary(digits = ~ c(1)) %>% 
  bold_labels() %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "mutate/output/merged/neighborhood.docx")

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
cherish <- cherish %>% 
  mutate(q5.factor = as.factor(case_when(q5.factor == "Yes" ~ 1,
                                          q5.factor == "No" ~ 0)),
         q6.factor = as.factor(case_when(q6.factor == "Yes" ~ 1,
                                          q6.factor == "No" ~ 0)),
         a.factor = as.factor(case_when(a.factor == "Yes" ~ 1,
                                         a.factor == "No" ~ 0)),
         q7.factor = as.factor(case_when(q7.factor == "Yes" ~ 1,
                                          q7.factor == "No" ~ 0)),
         q7a.factor = as.factor(case_when(q7a.factor == "Yes" ~ 1,
                                           q7a.factor == "No" ~ 0)),
         q8.factor = as.factor(case_when(q8.factor == "Yes" ~ 1,
                                          q8.factor == "No" ~ 0)),
         q8a.factor = as.factor(case_when(q8a.factor == "Yes" ~ 1,
                                           q8a.factor == "No" ~ 0)),
         q9.factor = as.factor(case_when(q9.factor == "Yes" ~ 1,
                                          q9.factor == "No" ~ 0)),
         q9a.factor = as.factor(case_when(q9a.factor == "Yes" ~ 1,
                                           q9a.factor == "No" ~ 0)),
         q10.factor = as.factor(case_when(q10.factor == "Yes" ~ 1,
                                           q10.factor == "No" ~ 0)),
         q11.factor = as.factor(case_when(q11.factor == "Yes" ~ 1,
                                           q11.factor == "No" ~ 0)),
         q12.factor = as.factor(case_when(q12.factor == "Yes" ~ 1,
                                           q12.factor == "No" ~ 0)),
         q13.factor = as.factor(case_when(q13.factor == "Yes" ~ 1,
                                           q13.factor == "No" ~ 0)),
         q14.factor = as.factor(case_when(q14.factor == "Yes" ~ 1,
                                           q14.factor == "No" ~ 0)),
         q15.factor = as.factor(case_when(q15.factor == "Yes" ~ 1,
                                           q15.factor == "No" ~ 0)),
         q17.factor = as.factor(case_when(q17.factor == "Yes" ~ 1,
                                           q17.factor == "No" ~ 0)))

library(labelled)

var_label(cherish) <- list(q1 = "How long have you lived in your current neighborhood? (years)",
                           a_there_are_walkable_sidew.factor = "There are walkable sidewalks on most of the streets in my neighborhood",
                           b_there_are_facilities_to.factor = "There are facilities to bicycle in or near my neighborhood, such as special lanes, separate paths or trails, shared use paths for cycles and pedestrians",
                           c_my_neighborhood_has_free.factor = "My neighborhood has free or low-cost recreation facilities, such as parks, walking trails, bike paths, recreation centers, playgrounds, public swimming pools, etc.",
                           d_there_are_many_shops_sto.factor = "There are many shops, stores, markets, or other places to buy things I need within easy walking distance",
                           e_a_large_selection_of_fre.factor = "A large selection of fresh fruits and vegetables is available in my neighborhood",
                           a_people_around_here_are_w.factor = "People around here are willing to help their neighbors",
                           b_people_in_my_neighborhoo.factor = "People in my neighborhood generally get along with each other",
                           c_people_in_my_neighborhoo.factor = "People in my neighborhood can be trusted",
                           d_people_in_my_neighborhoo.factor = "People in my neighborhood share the same values",
                           e_i_feel_safe_walking_in_m.factor = "I feel safe walking in my neighborhood, day or night",
                           f_violence_is_a_problem_in.factor = "Violence is a problem in my neighborhood",
                           q4.factor = "In the past 3 months, how often have you worried that your food would run out before you had money to buy more?",
                           q5.factor = "Are you easily able to get enough healthy food to eat? (i.e., fruits, vegetables etc.)",
                           q6.factor = "Do you usually eat fruits and vegetables every day?",
                           a.factor = "Are you willing to eat more fruits and vegetables?",
                           q7.factor = "Do you do any physical activity?",
                           q7a.factor = "If no, is this something you would be willing to do?",
                           q8.factor = "Do you currently smoke or uses any form of tobacco or nicotine products (i.e. chewing tobacco, vapes, etc.)?",
                           q8a.factor = "If yes are you willing to quit?",
                           q9.factor = "Do you weigh more than you would like to?",
                           q9a.factor = "If yes are you willing to lose weight?",
                           q10.factor	= "Has a doctor or health professional ever told you that you have hypertension or high blood pressure?",
                           q11.factor = "Do you currently take prescribed medication for hypertension or high blood pressure?",
                           q12.factor = "Has a doctor or health professional ever told you that your blood cholesterol level was high?",
                           q13.factor = "Do you currently take prescribed medication for high blood cholesterol?",
                           q14.factor = "Has a doctor or other health professional ever told you (except during pregnancy) that you have diabetes or high blood sugar?",
                           q15.factor = "Are you currently taking your prescribed medication for diabetes?",
                           q16.factor = "Do you take your medications regularly as prescribed?",
                           q17.factor = "(If yes or no to question 16) Are you confident that you can get your medications regularly?",
                           q17a.factor = "If no, why not?",
                           q17a___1.factor = "Have no need for the medication",
                           q17a___2.factor = "Too expensive or lack of insurance",
                           q17a___3.factor = "Pick up is not convenient (location or hours)",
                           q17a___4.factor = "Tend to put it off",
                           q17a___5.factor = "Other",
                           a_asking_questions_to_your.factor = "How confident are you in your ability to ask questions to your health care provider",
                           b_traveling_to_medical_app.factor = "How confident are you in your ability to travel to medical appointments",
                           c_understanding_medical_tr.factor = "How confident are you in your ability to understand medical treatment plans",
                           q19.factor = "Are you currently covered by any of the following types of health insurance or health coverage plans? If you have more than one kind of health insurance, tell me all plans that you have.",
                           q19___1.factor = "Private health insurance",
                           q19___2.factor = "Medicare",
                           q19___3.factor = "Medicaid",
                           q19___4.factor = "Other government insurance",
                           q19___5.factor = "No coverage of any type",
                           q20.factor = "When was the last time you saw a doctor or other health professional for a routine physical, general check-up or treatment for conditions like high blood pressure, cholesterol, or diabetes?",
                           q21.factor = "If you have not seen a doctor or health professional in over a year why? (Check all that apply)",
                           q21___1.factor = "No need for a health care provider",
                           q21___2.factor = "Mistrust or dislike health care providers",
                           q21___3.factor = "Do not know where to go",
                           q21___4.factor = "Previous health care provider is not available or moved",
                           q21___5.factor = "Too expensive or lack of insurance",
                           q21___6.factor = "Care not convenient (location or hours)",
                           q21___7.factor = "Tend to put it off",
                           q21___8.factor = "Concerns about COVID-19",
                           q21___9.factor = "Other",
                           q22.factor = "Where do you go for a general check-up or treatment for conditions such as, high blood pressure, cholesterol, or diabetes?",
                           q22___1.factor = "A doctor's office or health center",
                           q22___2.factor = "Walk-in clinic, urgent care center, or retail clinic in a pharmacy or grocery store",
                           q22___3.factor = "Emergency room",
                           q22___4.factor = "A VA Medical Center or VA outpatient clinic",
                           q22___5.factor = "Some other place",
                           q22___6.factor = "Do not go anywhere",
                           a_sometimes_doctors_care_m.factor = "Sometimes, health care providers care more about what is convenient for (him/her) than about your medical needs.",
                           b_doctor_are_extremely_tho.factor = "Health care providers are extremely thorough and careful",
                           c_you_completely_trust_doc.factor = "You completely trust health care providers decisions about which medical treatments are best",
                           d_a_doctor_would_never_mis.factor = "A health care providers would never mislead you about anything",
                           e_all_in_all_you_have_comp.factor = "All in all, you have complete trust in health care providers",
                           a_religious_spiritual_beli.factor = "Religious/spiritual beliefs have great in􀀄uence on my health",
                           b_i_tend_to_avoid_things_h.factor = "I tend to avoid things harmful to my body because of my religious/spiritual beliefs",
                           a_medical_researchers_have.factor = "Medical Researchers have no selfish reasons for doing research studies",
                           b_it_is_safe_to_be_in_a_me.factor = "It is safe to be in a medical research study",
                           c_there_are_some_things_ab.factor = "There are some things about medical research that I do not trust at all",
                           d_medical_researchers_do_n.factor = "Medical researchers do not tell people everything they really need to know about being in a research study",
                           a_i_think_being_connected.factor = "I think being connected to a primary care provider for routine care is possible for me",
                           b_i_think_receiving_health.factor = "I think receiving health coaching is possible for me",
                           c_i_think_participating_in.factor = "I think participating in exercises classes is possible for me",
                           d_i_think_participating_in.factor = "I think participating in nutrition education classes is possible for me",
                           e_i_think_measuring_my_blo.factor = "I think measuring my blood pressure, glucose, and physical activity at home and recording my values are possible for me",
                           q28 = "How old are you?",
                           q29.factor = "What was your sex assigned at birth?",
                           q30.factor = "Are you of Spanish or Hispanic origin or ancestry?",
                           q31.factor = "What racial group best represents your background?",
                           q32.factor = "What best describes your current marital status?",
                           q33.factor = "What is the highest grade or level of school you have completed or the highest degree you have received?",
                           q34.factor = "We would like to know about what you do-are you working now, looking for work, retired, keeping house, a student, or what?")

cherish %>% 
  select(q4.factor, q5.factor, q6.factor, a.factor, q7.factor, q7a.factor, q8.factor, q8a.factor, q9.factor, q9a.factor, q10.factor, q11.factor, q12.factor, q13.factor, q14.factor, q15.factor,
         q16.factor, q17.factor, q17a___1.factor, q17a___2.factor, q17a___3.factor, q17a___4.factor, q17a___5.factor,
         a_asking_questions_to_your.factor, b_traveling_to_medical_app.factor, c_understanding_medical_tr.factor,
         q19___1.factor, q19___2.factor, q19___3.factor, q19___4.factor, q19___5.factor,
         q20.factor, q21___1.factor, q21___2.factor, q21___3.factor, q21___4.factor, q21___5.factor, q21___6.factor, q21___7.factor, q21___8.factor, q21___9.factor,
         q22___1.factor, q22___2.factor, q22___3.factor, q22___4.factor, q22___5.factor, q22___6.factor) %>% 
  tbl_summary(digits = ~ c(1)) %>% 
  bold_labels() %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "mutate/output/merged/food and health.docx")

summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care
cherish %>% 
  select(a_sometimes_doctors_care_m.factor, b_doctor_are_extremely_tho.factor, c_you_completely_trust_doc.factor, d_a_doctor_would_never_mis.factor, e_all_in_all_you_have_comp.factor,
         a_religious_spiritual_beli.factor, b_i_tend_to_avoid_things_h.factor,
         a_medical_researchers_have.factor, b_it_is_safe_to_be_in_a_me.factor, c_there_are_some_things_ab.factor, d_medical_researchers_do_n.factor,
         increase_physical_activity, reduce_sugar_intake, reduce_salt_intake, eat_more_fruits_and_vegata, reduce_processed_foods, lose_weight, quit_smoking) %>% 
  tbl_summary(digits = ~ c(1)) %>% 
  bold_labels() %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "mutate/output/merged/trust and mistrust in health care.docx")

summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
cherish %>% 
  select(a_i_think_being_connected.factor, b_i_think_receiving_health.factor, c_i_think_participating_in.factor, d_i_think_participating_in.factor, e_i_think_measuring_my_blo.factor) %>% 
  tbl_summary(digits = ~ c(1)) %>% 
  bold_labels() %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "mutate/output/merged/CHERISH program.docx")

summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
cherish %>% 
  select(q28, q29.factor, q30.factor, q31.factor, q32.factor, q33.factor, q34.factor) %>% 
  tbl_summary(digits = ~ c(1)) %>% 
  bold_labels() %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = "mutate/output/merged/demographics.docx")

summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)



# BY SEX
# FEMALE
# Neighborhood
cherish_main <- cherish

cherish <- cherish %>% 
  filter(q29.factor == "Female")

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care 
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)

# MALE
# Neighborhood
rm(cherish)

cherish <- cherish_main %>% 
  filter(q29.factor == "Male")

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care 
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)


# BY AGE
# <= 65
# Neighborhood
rm(cherish)

cherish <- cherish_main %>% 
  filter(q28 <= 65)

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care 
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)

# > 65
# Neighborhood
rm(cherish)

cherish <- cherish_main %>% 
  filter(q28 > 65)

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)



# BY HYPERTENSION STATUS
# YES HTN
# Neighborhood
rm(cherish)

cherish <- cherish_main %>% 
  filter(q10.factor == "Yes")

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care 
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)

# NO HTN
# Neighborhood
rm(cherish)

cherish <- cherish_main %>% 
  filter(q10.factor == "No")

summary(cherish$q1)

summary(cherish$a_there_are_walkable_sidew.factor)
summary(cherish$b_there_are_facilities_to.factor)
summary(cherish$c_my_neighborhood_has_free.factor)
summary(cherish$d_there_are_many_shops_sto.factor)
summary(cherish$e_a_large_selection_of_fre.factor)

summary(cherish$a_people_around_here_are_w.factor)
summary(cherish$b_people_in_my_neighborhoo.factor)
summary(cherish$c_people_in_my_neighborhoo.factor)
summary(cherish$d_people_in_my_neighborhoo.factor)
summary(cherish$e_i_feel_safe_walking_in_m.factor)
summary(cherish$f_violence_is_a_problem_in.factor)

# Food and health
summary(cherish$q4.factor)
summary(cherish$q5.factor)

summary(cherish$q6.factor)
summary(cherish$a.factor)

summary(cherish$q7.factor)
summary(cherish$q7a.factor)

summary(cherish$q8.factor)
summary(cherish$q8a.factor)

summary(cherish$q9.factor)
summary(cherish$q9a.factor)

summary(cherish$q10.factor)
summary(cherish$q11.factor)

summary(cherish$q12.factor)
summary(cherish$q13.factor)

summary(cherish$q14.factor)
summary(cherish$q15.factor)

summary(cherish$q16.factor)
summary(cherish$q17.factor)
summary(cherish$q17a___1.factor)
summary(cherish$q17a___2.factor)
summary(cherish$q17a___3.factor)
summary(cherish$q17a___4.factor)
summary(cherish$q17a___5.factor)

summary(cherish$a_asking_questions_to_your.factor)
summary(cherish$b_traveling_to_medical_app.factor)
summary(cherish$c_understanding_medical_tr.factor)

summary(cherish$q19___1.factor)
summary(cherish$q19___2.factor)
summary(cherish$q19___3.factor)
summary(cherish$q19___4.factor)
summary(cherish$q19___5.factor)

summary(cherish$q20.factor)
summary(cherish$q21___1.factor)
summary(cherish$q21___2.factor)
summary(cherish$q21___3.factor)
summary(cherish$q21___4.factor)
summary(cherish$q21___5.factor)
summary(cherish$q21___6.factor)
summary(cherish$q21___7.factor)
summary(cherish$q21___8.factor)
summary(cherish$q21___9.factor)

summary(cherish$q22___1.factor)
summary(cherish$q22___2.factor)
summary(cherish$q22___3.factor)
summary(cherish$q22___4.factor)
summary(cherish$q22___5.factor)
summary(cherish$q22___6.factor)

# Trust and mistrust in health care
summary(cherish$a_sometimes_doctors_care_m.factor)
summary(cherish$b_doctor_are_extremely_tho.factor)
summary(cherish$c_you_completely_trust_doc.factor)
summary(cherish$d_a_doctor_would_never_mis.factor)
summary(cherish$e_all_in_all_you_have_comp.factor)

summary(cherish$a_religious_spiritual_beli.factor)
summary(cherish$b_i_tend_to_avoid_things_h.factor)

summary(cherish$a_medical_researchers_have.factor)
summary(cherish$b_it_is_safe_to_be_in_a_me.factor)
summary(cherish$c_there_are_some_things_ab.factor)
summary(cherish$d_medical_researchers_do_n.factor)

table(cherish$increase_physical_activity, useNA = "always")
table(cherish$reduce_sugar_intake, useNA = "always")
table(cherish$reduce_salt_intake, useNA = "always")
table(cherish$eat_more_fruits_and_vegata, useNA = "always")
table(cherish$reduce_processed_foods, useNA = "always")
table(cherish$lose_weight, useNA = "always")
table(cherish$quit_smoking, useNA = "always")

# CHERISH program
summary(cherish$a_i_think_being_connected.factor)
summary(cherish$b_i_think_receiving_health.factor)
summary(cherish$c_i_think_participating_in.factor)
summary(cherish$d_i_think_participating_in.factor)
summary(cherish$e_i_think_measuring_my_blo.factor)

# Demographics
summary(cherish$q28)
summary(cherish$q29.factor)
summary(cherish$q30.factor)
summary(cherish$q31.factor)
summary(cherish$q32.factor)
summary(cherish$q33.factor)
summary(cherish$q34.factor)