

list.of.packages <-
  c(
    "cowplot", "googleway", "ggplot2", "ggrepel", 
    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "usmap", "here", "dplyr"
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


beepr::beep()
