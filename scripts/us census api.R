#API script
#load relevant libraries
install.packages("tidycensus")
library(tidycensus)

#my_key <- census_api_key("7e17eb37151215db6ed133db6d107bb6143a9e37", install = TRUE)

#load variables
v15 <-  load_variables(2015, "acs5", cache = TRUE)
v15

#median age by zip code
median_age <- get_acs(geography = "zcta", variables =  "B01002_001")

#estimate of total population of 2 or more races
estimate_pop_two_race <- get_acs(geography = "zcta", variables =  "C02003_010")

#estimate of total population of race, white
estimate_pop_white <- get_acs(geography = "zcta", variables =  "C02003_003")

#estimate of total population of race, african-american
estimate_pop_black <- get_acs(geography = "zcta", variables =  "C02003_004")

#estimate of total population by race, hispanic
estimate_pop_hispanic <- get_acs(geography = "zcta", variables =  "B03002_012")

#estimate of total population by race, asian
estimate_pop_asian <- get_acs(geography = "zcta", variables =  "C02003_006")

#estimate, child from 6-18 years old, living with 2 parents and is native born
estimate_pop_native_child_2_parents <- get_acs(geography = "zcta", variables =  "B05009_022")

#estimate, child from 6-18, living with 1 parent and is native born
estimate_pop_native_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_032")

#estimate, child from 6-18, living with 1 parent
estimate_pop_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_031")

#estimate, child 6-18, living with 2 parents
estimate_pop_child_2_parent <- get_acs(geography = "zcta", variables =  "B05009_003")

#estimate, speak only English
estimate_speak_only_english <- get_acs(geography = "zcta", variables =  "B06007_002")

#estimate, speak Spanish, English less than "very well"
estimate_speak_spanish_not_great_english <- get_acs(geography = "zcta", variables =  "B16005_007")

#estimate, childm 6-17, living with 2 foreign born parents
estimate_child_2_foreign_born_parents <- get_acs(geography = "zcta", variables =  "B05009_025")

#estimate, child 6-17, living with 1 foreign-born parent
estimate_child_1_foreign_born_parent <- get_acs(geography = "zcta", variables =  "B05009_035")

#estimate, total unemployed
estimate_total_unemployed <- get_acs(geography = "zcta", variables =  "B27011_015")

#estimate, total employed
estimate_total_employed <- get_acs(geography = "zcta", variables =  "B27011_003")

#estimate, total, received food stamps in past 12 months, below poverty line
estimate_below_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_003")

#estimate, total, received food stamps, in past 12 months, above or at poverty line
estimate_above_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_004")

#estimate, total, did not receive food stamps, past 12 months, below poverty line
estimate_below_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_006")

#estimate, total, did not receive food stamps, past 12 months above or at the poverty line
estimate_above_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_007")

#estimate, total, renters w
estimate_renting_children <- get_acs(geography = "zcta", variables =  "B25012_010")

#estimate, total, home owners 
estimate_owning_children <- get_acs(geography = "zcta", variables =  "B25012_002")

#estimate, aggregate earnings, 2016
earnings <-  get_acs(geography = "zcta", variables =  "B20003_001")

