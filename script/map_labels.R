
library(usmap)
library(ggplot2)


sites <- data.frame(longitude = c(dta$long), latitude = c(dta$lat),
                    field = (dta$field), year = (dta$year), Row_spacing = (dta$row_spacing),
                    Risk = (dta$risk), Disease_Incidence = (dta$di))
class(sites)

sites

plot_usmap(include = c("PA", "NY"), regions = "counties") + 
  labs(title = "PA and NY Counties",
       subtitle = "Sporecaster Risk 2020") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))
  

        


# Lat/Long of data 
dta
sites
toString(sites$field)

dta_t <- usmap_transform(sites, c("longitude", "latitude"))

plot_usmap(include = c("PA", "NY"), regions = "counties") + 
  geom_point(data = dta_t, 
             aes(x = x, y = y), 
             color = "red",
             size = 3,
             alpha = 0.5)


base_map <- plot_usmap(include = c("PA", "NY"), regions = "counties", fill = "#FFFF99", alpha = 0.25) +
  ggrepel::geom_label_repel(data = dta_t,
                            aes(x = x, y = y, label = field),
                            size = 2.5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002,
                            max.overlaps = 30) 

base_map +
  geom_point(data = dta_t,
             aes(x = x, y = y) ,
             color = "purple", alpha = 0.3) +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(title = "Sporecaster Field Locations",
       subtitle = "2020",
       size = "Sporecaster Risk") +
  theme(legend.position = "right")























