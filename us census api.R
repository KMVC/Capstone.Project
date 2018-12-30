#API script
#load relevant libraries
#install.packages("tidycensus")
library(tidycensus)
library(tidyverse)

#my census key
#my_key <- census_api_key("7e17eb37151215db6ed133db6d107bb6143a9e37", install = TRUE)

#load variables 
v15 <-  load_variables(2015, "acs5", cache = TRUE)
View(v15)

#AGE
#median age by zip code
median_age <- get_acs(geography = "zcta", variables =  "B01002_001")

#RACE by zip code
#estimate of total population of 2 or more races
two_race <- get_acs(geography = "zcta", variables =  "C02003_010")

#estimate of total population of race who are solely white
white <- get_acs(geography = "zcta", variables =  "C02003_003")

#estimate of total population of race who are solely african-american
black <- get_acs(geography = "zcta", variables =  "C02003_004")

#estimate of total population by race who are hispanic
hispanic <- get_acs(geography = "zcta", variables =  "B03002_012")

#estimate of total population by race who are asian
asian <- get_acs(geography = "zcta", variables =  "C02003_006")

#NATIVE OR FOREIGN-Born, children and parents
#estimate of # of children 6-18 years old, living with 2 parents, child is native born
native_child_2_parents <- get_acs(geography = "zcta", variables =  "B05009_022")

#estimate of # of children from 6-18 years old, living with 1 parent, child is native born
native_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_032")

#estimate of # of children from 6-18 years old, living with 2 parents, child is foreign born
foreign_child_2_parents <-  get_acs(geography = "zcta", variables =  "B05009_023")

#estimate of # of children from 6-18 years old, living with 1 parent, child is foreign born
foreign_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_033")

#estimate, children 6-17, living with 2 native born parents
two_native_parents <- get_acs(geography = "zcta", variables =  "B05009_024")

#estimate, children 6-17, living with 1 native born parent
one_native_parent <- get_acs(geography = "zcta", variables =  "B05009_034")

#estimate, children 6-17, living with 2 foreign born parents
two_foreign_born_parents <- get_acs(geography = "zcta", variables =  "B05009_025")

#estimate, child 6-17, living with 1 foreign-born parent
one_foreign_born_parent <- get_acs(geography = "zcta", variables =  "B05009_035")

#ESTIMATE OF CHILDREN LIVING WITH 1 OR 2 PARENTS
#estimate, child from 6-18, living with 1 parent
child_only_parent <- get_acs(geography = "zcta", variables =  "B05009_031")

#estimate, child 6-18, living with 2 parents
child_two_parents <- get_acs(geography = "zcta", variables =  "B05009_003")

#LANGUAGE

#estimate of population that speak only English
speak_only_english <- get_acs(geography = "zcta", variables =  "B06007_002")

#estimate of population that speak Spanish whose English is rated as less than "very well"
speak_spanish_not_great_english <- get_acs(geography = "zcta", variables =  "B16005_007")

#EMPLOYMENT
#estimate of  total unemployed
total_unemployed <- get_acs(geography = "zcta", variables =  "B27011_015")

#estimate of total employed
total_employed <- get_acs(geography = "zcta", variables =  "B27011_003")

#POVERTY and FOOD STAMPS
#estimate of total who received food stamps in past 12 months and are below poverty line
below_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_003")

#estimate of total who received food stamps, in past 12 months, and above or at poverty line
above_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_004")

#estimate of total who did not receive food stamps, past 12 months, and are below poverty line
below_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_006")

#estimate, total, did not receive food stamps, past 12 months above or at the poverty line
above_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_007")

#HOUSING: RENTING OR OWNING?
#estimate, total, renters 
renting <- get_acs(geography = "zcta", variables =  "B25012_010")

#estimate, total, home owners 
owning <- get_acs(geography = "zcta", variables =  "B25012_002")

#AGGREGATE EARNINGS
#estimate, aggregate earnings, 2016
earnings <-  get_acs(geography = "zcta", variables =  "B20003_001")

