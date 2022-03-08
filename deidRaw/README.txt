Add these lines at the end of R script to get R dataset in input


library(tidyr)
library(tidyverse)

data <- data %>% 
  filter(record_id != "TEST_sb1") %>% 
  separate(record_id, 
           sep = "--",
           into = c("record_id", "dde")) %>% 
  filter(is.na(dde)) %>% 
  select(-dde)

cherish <- data
save(cherish, file = "C:/Users/fallouch/Box/PhD/CHERISH/CHERISH/deidRaw/input/cherish.RData")


****FOR DDE 1 ONLY****

library(tidyr)
library(tidyverse)

data <- data %>% 
  filter(record_id != "TEST_sb1") %>% 
  separate(record_id, 
           sep = "--",
           into = c("record_id", "dde")) %>% 
  filter(dde == 1) %>% 
  select(-dde)

cherish <- data
save(cherish, file = "C:/Users/fallouch/Box/PhD/CHERISH/CHERISH/deidRaw/input/cherish.RData")