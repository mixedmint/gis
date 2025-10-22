# read UK geopackage
# check the layers of gpkg
```{r}
library(sf)
library(here)
library(janitor)
library(tidyverse)
library(terra)
library(ggplot2)
```

#read layer0
```{r}
UK <- st_read(here("data","gadm41_GBR.gpkg"),
                     layer="ADM_ADM_0")
# or
UK <- st_read("data/gadm41_GBR.gpkg")   # we can use tab to select path in code chunk

st_layers(here("data","gadm41_GBR.gpkg"))
```

#read world_cities
```{r}
world_cities <- st_read("data/World_Cities/World_Cities.shp")
```
# read ssp1 and ssp5 tiffs
```{r}
ssp1 <- terra::rast("data/wc2.1_2.5m_tmax_ACCESS-CM2_ssp126_2081-2100.tif")

ssp1_mean <- mean(ssp1)

ssp5 <- terra::rast("data/wc2.1_2.5m_tmax_ACCESS-CM2_ssp585_2081-2100.tif")
print(ssp1)
print(ssp5)
```

# filter uk cities
```{r}
ukcities <- world_cities %>%
  janitor::clean_names() %>%
  dplyr::filter(cntry_name=="United Kingdom")
```

# crop and mask rasters
```{r}
ssp1_diff <- ssp1 %>%
  terra::crop(.,UK)  

exact_uk <- ssp1_diff %>%
  terra::mask(.,UK)

ssp5_diff <- ssp5 %>%
  terra::crop(.,UK)

exact_uk5 <- ssp5_diff %>%
  terra::mask(.,UK)
```

# subtract rasters
```{r}
diff_climate_modle <- exact_uk5 - exact_uk
```

# rename the extract data from points
```{r}
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(diff_climate_modle) <- month

uk_cities_diff <- diff_climate_modle %>%
  terra::extract(.,ukcities)
 View(uk_cities_diff)
```

# join the city names to uk_cities_diff
```{r}
uk_cities_join_id <- ukcities %>%
  dplyr::mutate(join_id = 1:n())  #add columns id 1-15 to the ukcities table

#now join
uk_cities_diff2 <- uk_cities_join_id %>%
  left_join(.,
            uk_cities_diff,
            by = c("join_id"="ID"))
```

# drop the geometry and plot
```{r}
# extract just the months (jan - december) columns
city_climate_diff <- uk_cities_diff2 %>%
  dplyr::select(c(,16:27))%>%
  sf::st_drop_geometry(.)%>%
  dplyr::as_tibble()

#pivot the months longer
tidy_city_diff <- city_climate_diff %>%
  tidyr::pivot_longer(everything(), 
               names_to="Months", 
               values_to="temp_diff")

# change the months to a factor column type (levels / ordered categories) this will be to facet
facet_plot <- tidy_city_diff %>%
  dplyr::mutate(Months = factor(Months, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

# Plot faceted histogram
plot<-ggplot(facet_plot, 
             # plot the temp data
             aes(x=temp_diff, na.rm=TRUE))+
  #plot with histogram
  geom_histogram(color="black", binwidth = .1)+
  labs(title="Ggplot2 faceted difference in climate scenarios of max temp", 
       x="Temperature",
       y="Frequency")+
  # use the levels to facet
  facet_grid(Months ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

plot
```



