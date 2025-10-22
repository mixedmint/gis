library(dplyr)
library(here)
library(sf)
library(tidyverse)
library(janitor)
library(reader)
#read csv 并将NULL变为na
original_csv <- read_csv("data/Report_Card_Assessment_Data_2018-19_School_Year_20251010.csv",
                        locale = locale(encoding = "latin1"),
                                        na = "NULL")
#检查路径
here::here()
getwd()

#clean and select columns
cleaned_csv <- original_csv %>%
  clean_names() %>%
  select(county, organization_level, test_subject,
         count_met_standard, count_of_students_expected_to_test,
         grade_level)

#fliter data: TestSubject-Science; GradeLeve-All Grades; 
flitered_csv <- cleaned_csv %>%
  filter(
    county != "Multiple",
    organization_level == "School",
    test_subject == "Science",
    grade_level == "All Grades"
    )

#data types
Datatypelist <- flitered_csv %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

county_summary<- flitered_csv %>%
  group_by(county) %>%
  summarise(
    TotalCountMetStandard = sum(count_met_standard, na.rm = TRUE),
TotalStudentsExpectedToTest = sum(count_of_students_expected_to_test, na.rm = TRUE)
) #如果变量名称里有空格，要用反引号`` e.g. 'count met standard'

#calculating average percent of every county and Washington, descending county average
average_percent<- county_summary %>%
  mutate(county_average = TotalCountMetStandard/TotalStudentsExpectedToTest) %>%
  mutate(washington_average = sum(TotalCountMetStandard)/sum(TotalStudentsExpectedToTest)) %>%
  arrange(desc(county_average)) %>%
#transform to percentage: average*100
  mutate(county_average = county_average*100) %>%
  mutate(washington_average = washington_average*100)

#compare county_average and Washington_average
library(stringr)
average_percent <- average_percent %>%
  mutate(difference = county_average-washington_average) %>%
  #round 1
  mutate(across(where(is.numeric), round, 0)) %>%
  mutate(percentage_compare = case_when(difference > 0 ~ "above",
                                       difference == 0 ~ "equal",
                                       difference < 0 ~ "below"))

#Plotting
library(tmap)
library(tmaptools)
#read boundary map
getwd()
Shoreline <- st_read(here::here("data","Washington_Counties_with_Natural_Shoreline___washsh_area.geojson"))
qtm(Shoreline)

#Jointing data
Shoreline_jointed <- Shoreline %>%
  merge(.,
        average_percent,
        by.x = "COUNTYLABEL",
        by.y = "county",
        all.x = TRUE)

#tmap plot
tmap_mode("plot")
#only shows above/below
qtm(Shoreline_jointed, fill = "percentage_compare")
#show the accurate difference percantage
qtm(Shoreline_jointed, fill = "difference")
