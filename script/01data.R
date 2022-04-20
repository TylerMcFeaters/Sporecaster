library(lubridate)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
###Data


dta <- read.csv(here("dta/risk_date_dta.csv"))

##Convert char to date for dates
dta$date <- dmy(dta$date)

cols.to.long <- colnames(dta)[!grepl("date",colnames(dta))]

dta <- 
  dta %>% 
  mutate_at(.vars = cols.to.long, as.numeric) 


dta.l <- 
  dta %>% 
  pivot_longer(cols = all_of(cols.to.long),
               values_to = "risk",
               names_to = "id")


paste0("PA", 1:23, collapse = "|") 

dta.l <-
  dta.l %>%
  mutate(ind = ifelse(id %in% paste0("PA", 1:23), 1, 0))

dta.l[dta.l$ind == 1, "date"] <- 
  dta.l[dta.l$ind == 1, "date"] - 365  

dta.l$ind <- NULL

dta.l <-
  dta.l %>% 
  mutate(yr =year(date))

dta.l <- 
  dta.l %>% 
  arrange(id) %>% 
  # mutate(id = factor("id", levels = c()))
  group_by(id, yr) %>% 
  summarise(
    maxr = max(risk, na.rm= TRUE)
  )

dta.add <- 
  read.csv(here("dta/dta.csv")) %>% 
  rename(id = field) %>% 
  select(-c(risk, year))

dta <- 
  left_join(dta.l, dta.add, dta.add2, by = "id")

#save(dta, file = "dta/full_data.RData")
write.csv(dta, here("dta/full_dta.csv"))

beepr::beep()