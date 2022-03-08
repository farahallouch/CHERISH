rm(list = ls())

library(tidyverse)

load("C:/Users/fallouch/Box/PhD/CHERISH/CHERISH/deidRaw/output/cherish.RData")

# cherish %>% 
#   select(-record_id) %>% 
#   gtsummary::tbl_summary(by = q10.factor,
#                          digits = ~ c(2))

# OVERALL
# Neighborhood
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