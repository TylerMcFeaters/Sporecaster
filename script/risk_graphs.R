library(lubridate)
library(magrittr)
library(ggplot2)
library(dplyr)
library(tidyr)
###Data

success_dta <- read.csv(here("dta/success_dta.csv"))
risk_dta <- read.csv(here("dta/risk_dta.csv"))

dta <- read.csv(here("dta/risk_date_dta.csv"))


##Convert char to date for dates
 dta$date <- dmy(dta$date)
 

 
cols.to.long <- colnames(dta)[!grepl("date",colnames(dta))]
 
dta %>% 
  pivot_longer(cols = all_of(cols.to.long))



# Basic line plot with points
ggplot(data=dta, aes(x=date, y=NY1 )) +
  geom_line()+
  geom_point()+
  ylim(0,100)
  #scale_x_date(scale_x_date(limits = c()), date_labels = "%b/%d")
  






# Change the line type
ggplot(data=df, aes(x=date, y=PA1, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()
# Change the color
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(color="red")+
  geom_point()


ggplot(dta, aes(x = date, y = PA1)) +            # Draw ggplot2 plot
  geom_line() + 
  scale_x_date(date = "%Y-%m")
