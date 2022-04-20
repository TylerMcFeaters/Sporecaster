
################################################################################
# Visualizations
################################################################################

##Data for risk graphs
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

################################################################################
##Basic line plot with points-2020

p1 <- 
  dta.l %>% 
  filter(yr == 2020) %>%  
  ggplot( aes(x=date, y=risk, color = id )) +
  geom_line()+
  # geom_point()+
  ylim(0,100)  +
  theme(legend.position = "none")
#scale_x_date(scale_x_date(limits = c()), date_labels = "%b/%d")

# Basic line plot with point-2021
p2 <-
  dta.l %>% 
  filter(yr == 2021) %>%  
  ggplot( aes(x=date, y=risk, group = id), color = "lightgray") +
  geom_line()+
  # geom_point()+
  geom_hline( aes(yintercept  =50), color = "lightgray", linetype= "dashed")+
  ylim(0,100) +
  theme(legend.position = "none")
#scale_x_date(scale_x_date(limits = c()), date_labels = "%b/%d")

p1

p2

egg::ggarrange(plots = list(p1,p2))

################################################################################
# Map data

library(usmap)
library(ggplot2)
library(data.table)
library(dplyr)
library(stringr)
library(RColorBrewer)


dta <- read.csv(here("dta/full_dta.csv"))

sites <- data.frame(longitude = c(dta$long), latitude = c(dta$lat),
                    Field = (dta$id), Year = (dta$yr), Row_spacing = (dta$row_spacing),
                    Risk = (dta$maxr), Disease_Incidence = (dta$di))

toString(sites$Field)

dta_t <- usmap_transform(sites, c("longitude", "latitude"))

################################################################################
# 2020 Sporecaster Risk Map

dta_2020 <- dta_t %>% 
  filter(str_detect(Year, "2020"))

dta_2020[,10] <- log(dta_2020[,6])

risk_2020 <- plot_usmap(include = c("PA"), regions = "counties", fill = "#FFFFCC", alpha = 0.25) + 
  labs(title = "Sporecaster Risk",
       subtitle = "2020") +
  geom_point(data = dta_2020, 
             aes(x = x, y = y, color = Risk), 
             size = dta_2020$Risk/10,
             alpha = 0.8)+
  scale_color_gradient(low= "yellow", high= "red")



risk_2020 +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_text_repel(data = dta_2020,
                           aes(x = x, y = y, label = Risk),
                           size = dta_2020$V10, alpha = 0.8,
                           # label.r = unit(0.5, "lines"), label.size = 0.5,
                           segment.color = "black", segment.size = 1,
                           seed = 1002,
                           max.overlaps = 30) +
  theme(legend.position = "right")

ggsave("out/risk_2020.png")

################################################################################
# 2021 Sporecaster Risk Map

dta_2021 <- dta_t %>% 
  filter(str_detect(Year, "2021"))

dta_2021[,10] <- log(dta_2020[,6])

risk_2021 <- plot_usmap(include = c("PA", "NY"), regions = "counties", fill = "#FFFFCC", alpha = 0.25) + 
  labs(title = "Sporecaster Risk",
       subtitle = "2021") +
  geom_point(data = dta_2021, 
             aes(x = x, y = y, color = Risk), 
             size = dta_2021$Risk/10,
             alpha = 0.8)+
  scale_color_gradient(low= "yellow", high= "red")



risk_2021 +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_text_repel(data = dta_2021,
                           aes(x = x, y = y, label = Risk),
                           size = dta_2021$V10, alpha = 0.8,
                           # label.r = unit(0.5, "lines"), label.size = 0.5,
                           segment.color = "black", segment.size = 1,
                           seed = 1002,
                           max.overlaps = 30) +
  theme(legend.position = "right")

ggsave("out/risk_2021.png")

################################################################################
# 2020 Sporecaster DI Map

dta_2020 <- dta_t %>% 
  filter(str_detect(Year, "2020"))

dta_2020[,10] <- log(dta_2020[,6])


di_2020 <- plot_usmap(include = c("PA"), regions = "counties", fill = "#FFFFCC", alpha = 0.25) + 
  labs(title = "Sporecaster Disease Incidence",
       subtitle = "2020") +
  geom_point(data = dta_2020, 
             aes(x = x, y = y, color = Disease_Incidence), 
             size = dta_2020$Disease_Incidence,
             alpha = 0.8)+
  scale_color_gradient(low= "yellow", high= "red")



di_2020 +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_text_repel(data = dta_2020,
                           aes(x = x, y = y, label = Disease_Incidence),
                           size = dta_2020$V10, alpha = 0.8,
                           # label.r = unit(0.5, "lines"), label.size = 0.5,
                           segment.color = "black", segment.size = 1,
                           seed = 1002,
                           max.overlaps = 30) +
  theme(legend.position = "right")

ggsave("out/di_2020.png")

################################################################################
# 2021 Sporecaster DI Map

dta_2021 <- dta_t %>% 
  filter(str_detect(Year, "2021"))

dta_2021[,10] <- log(dta_2020[,6])


di_2021 <- plot_usmap(include = c("PA", "NY"), regions = "counties", fill = "#FFFFCC", alpha = 0.25) + 
  labs(title = "Sporecaster Disease Incidence",
       subtitle = "2021") +
  geom_point(data = dta_2021, 
             aes(x = x, y = y, color = Disease_Incidence), 
             size = dta_2021$Disease_Incidence/5,
             alpha = 0.8)+
  scale_color_gradient(low= "yellow", high= "red")



di_2021 +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_text_repel(data = dta_2021,
                           aes(x = x, y = y, label = Disease_Incidence),
                           size = dta_2021$V10, alpha = 0.8,
                           # label.r = unit(0.5, "lines"), label.size = 0.5,
                           segment.color = "black", segment.size = 1,
                           seed = 1002,
                           max.overlaps = 30) +
  theme(legend.position = "right")

ggsave("out/di_2021.png")

################################################################################
# PA map

dta_pa <- dta_t %>% 
  filter(str_detect(Field, "PA"))

pa <- plot_usmap(include = c("PA"), regions = "counties", fill = "#FFFF99", alpha = 0.25) + 
  labs(title = "PA Counties",
       subtitle = "Sporecaster Risk 2020") +
  geom_point(data = dta_pa, 
             aes(x = x, y = y), 
             color = "red",
             size = 3,
             alpha = 0.5) +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_text_repel(data = dta_pa,
                            aes(x = x, y = y, label = Field),
                            size = 5, alpha = 0.8,
                            # label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002,
                            max.overlaps = 30) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  theme(legend.position = "right")

pa

################################################################################
#NY map

dta_t %>%
  filter(str_detect(Field, "NY"))

ny <- plot_usmap(include = c("NY"), regions = "counties", fill = "#FFFF99", alpha = 0.25) + 
  labs(title = "NY Counties",
       subtitle = "Sporecaster Risk 2020") +
  geom_point(data = dta_t, 
             aes(x = x, y = y), 
             color = "red",
             size = 3,
             alpha = 0.5) +
  theme(panel.background = element_rect(color = "black", fill = "lightblue")) +
  ggrepel::geom_label_repel(data = dta_t,
                            aes(x = x, y = y, label = Field),
                            size = 2.5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002,
                            max.overlaps = 30) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  theme(legend.position = "right")

ny

################################################################################
# Anaysis 
dta[dta$di == 0, "di"] <- .00001
dta$di <- dta$di /100 

dta$di <- ifelse(dta$di >.1, 1,0)
dta$di <- ifelse(dta$di >.1, 1,0)


model_glm = 
  glm(di ~ maxr, data = dta, family = "binomial")

summary(model_glm)

head(predict(model_glm, type = "response"))
model_glm_pred = 
  ifelse(predict(model_glm, type = "link") > 0, "Yes", "No")

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}


calc_class_err(actual = dta$di, predicted = model_glm_pred)



train_tab = 
  table(predicted = model_glm_pred,
                  actual = dta$di)
library(caret)
train_con_mat = 
  confusionMatrix(train_tab, positive = "Yes")
c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])


get_logistic_error = 
  function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  preds = ifelse(probs > cut, pos, neg)
  calc_class_err(actual = data[, res], predicted = preds)
}
get_logistic_error(
  model_glm, data = dta, 
  res = "di", pos = "Yes", neg = "No", cut = 0.5)


plot(di ~ maxr, data = dta, 
     col = "darkorange", pch = "|", ylim = c(-0.2, 1),
     main = "Using Logistic Regression for Classification")
abline(h = 0, lty = 3)
abline(h = 1, lty = 3)
abline(h = 0.5, lty = 2)
curve(predict(model_glm, data.frame(maxr = x), type = "response"), 
      add = TRUE, lwd = 3, col = "dodgerblue")
abline(v = -coef(model_glm)[1] / coef(model_glm)[2], lwd = 2)





