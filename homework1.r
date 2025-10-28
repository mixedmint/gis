library(sf)
library(tidyverse)
library(here)

shape <- st_read("data/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")
shape_simple <- st_simplify(shape, dTolerance = 1000)

employed_2018 <- read_csv("data/2018_paid_employee_field.csv")

Datatypelist <- shape_simple %>% 
  summarise_all(class)
Datatypelist

summary(shape_simple)

shape_simple %>%
  st_geometry()%>%
  plot()

shape2 <- shape_simple%>%
  merge(.,
        employed_2018,
        by.x="TA2018_V_1", 
        by.y="Area_Description")


library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
my_map<-shape2 %>%
  qtm(.,fill = "Paid employee")

my_map