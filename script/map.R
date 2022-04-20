

list.of.packages <-
  c(
    "cowplot", "googleway", "ggplot2", "ggrepel", 
    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "usmap", "here", "magrittr"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)


library("ggplot2")
theme_set(theme_bw())
library("sf")


#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}



here::here()

conflict_prefer("here", "here")

rm(packages_load, list.of.packages, new.packages)

######################################################################
###Method 1
#########################################################################
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

###Basic Map
ggplot(data = world) +
  geom_sf()

###Add Labels
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

###Add color
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

###Color by attribute
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

###Set the projection
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

###
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-80.15, -74.12), ylim = c(39, 42), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-80.15, -74.12), ylim = c(39, 42), expand = FALSE)





###############################################
###Method 2
###############################################



library(usmap)


plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))



map <- plot_usmap(include = c("PA", "NY"))
  #labs(title = "Sporecaster Validation",
       #subtitle = "Validation locations")

dta <- read.csv(here("dta/dta.csv"))


#plot_usmap(data = statepop, values = "field", color = "red") + 
   # theme(legend.position = "right")
 #theme(legend.position = "right")

ggplot2(map)
