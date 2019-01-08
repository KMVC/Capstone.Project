#load relevant libraries
library(tidyverse)
#install.packages("tidycensus")
library(tidycensus)

#read datasets into R, use fileEncoding to get rid of junk symbol that appears at start of School ID
schools2016 <-  read.csv("Chicagoschools2016.csv", na = c("", "NA"), fileEncoding="UTF-8-BOM")
schools2015 <-  read.csv("Chicagoschools2015.csv", na = c("", "NA"),fileEncoding="UTF-8-BOM" )
schools2014 <-  read.csv("Chicagoschools2014.csv", na = c("", "NA"), fileEncoding="UTF-8-BOM")
allzips <- read.csv("allzips.csv")


#eliminate all schools from schools2015 schools2016 that are not elementary schools (high schools and middle schools)
schools2015 <-  filter(schools2015, Primary_Category == "ES")
schools2016 <-  filter(schools2016, Primary_Category == "ES")

#eliminate un-needed columns from schools2016
schools2016 <-  select(schools2016, School_ID, 
                       NWEA_Reading_Attainment_Grade_3_Pct, NWEA_Math_Attainment_Grade_3_Pct)

#put 2016 labels on columns
colnames(schools2016)[2:3] <- paste("2016", colnames(schools2016)[2:3], sep = "_")


#eliminate un-needed columns from schools2015
schools2015 <-  select(schools2015, 
                       School_ID, 
                       Zip,
                       NWEA_Reading_Attainment_Grade_3_Pct, 
                       NWEA_Math_Attainment_Grade_3_Pct)


#put label on some columns marking them as 2015
colnames(schools2015)[3:4] <- paste("2015", colnames(schools2015)[3:4], sep = "_")


#bring the two school tables together
schools201516 <- inner_join(schools2015, schools2016, by = "School_ID") 


#bring together the table with the dataset on income and graduation percentages
schools201516 <- left_join(schools201516, allzips, by = c( "Zip" = "Zips"))

#finding and pulling demographic data from the American Community Survey 2012-2015 (ACS)
#all data pulled by zip code, estimate are of total population

#creating my census key
#my_key <- census_api_key("7e17eb37151215db6ed133db6d107bb6143a9e37", install = TRUE)

v15 <-  load_variables(2015, "acs5", cache = TRUE)

#AGE
#median age by zip code
median_age <- get_acs(geography = "zcta", variables =  "B01002_001")

#eliminate un-needed columns from median_age
median_age <-  select(median_age, GEOID, estimate, moe)

#filter results with MOE higher than 20%
median_age <- median_age %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent < 0.2)

#add labels to median_age
colnames(median_age)[2:3] <- paste("median age", colnames(median_age)[2:3], sep = "_")

#turning data in GEOID into integer class
median_age$GEOID <- as.integer(median_age$GEOID)

#bring together the 20152016 table with the dataset on median age
schools201516 <- inner_join(schools201516, median_age, by = c( "Zip" = "GEOID")) 

#TOTAL POPULATION
#total pop by zip code
total_pop <- get_acs(geography = "zcta", variables =  "B00001_001")

#eliminate un-needed columns from total pop
total_pop <-  select(total_pop, GEOID, estimate)

#add labels to total pop
colnames(total_pop)[2] <- paste("total pop", colnames(total_pop)[2], sep = "_")

#turning data in GEOID into integer class
total_pop$GEOID <- as.integer(total_pop$GEOID)

#bring together the 20152016 table with the dataset on total pop
schools201516 <- inner_join(schools201516, total_pop, by = c( "Zip" = "GEOID")) 

#GENDER by zip code
#female by zip code
female <-  get_acs(geography = "zcta", variables =  "B05003_013")

#eliminate un-needed columns from female
female <-  select(female, GEOID, estimate, moe)

#filter results with MOE higher than 20%
female <- female %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent < 0.2)

#add labels to female
colnames(female)[2:3] <- paste("female", colnames(female)[2:3], sep = "_")

#turning data in GEOID into integer class
female$GEOID <- as.integer(female$GEOID)

#bring together the 20152016 table with the dataset on female pop
schools201516 <- inner_join(schools201516, female, by = c( "Zip" = "GEOID")) 

#male by zip code
male <-  get_acs(geography = "zcta", variables =  "B05003_002")

#eliminate un-needed columns from male
male <-  select(male, GEOID, estimate, moe)

#filter results with MOE higher than 20%
male <- male %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent < 0.2)

#add labels to male
colnames(male)[2:3] <- paste("male", colnames(male)[2:3], sep = "_")

#turning data in GEOID into integer class
male$GEOID <- as.integer(male$GEOID)

#bring together the 20152016 table with the dataset on male pop
schools201516 <- inner_join(schools201516, male, by = c( "Zip" = "GEOID")) 


#RACE by zip code
#estimate of total population of 2 or more races
#two_race_or_more <- get_acs(geography = "zcta", variables =  "C02003_010")

#eliminate un-needed columns from two_race_or_more
#two_race_or_more <-  select(two_race_or_more, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#two_race_or_more <- two_race_or_more %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#only 4 observations remain after filter, have to remove this variable

#estimate of total population of race who are solely white
#white <- get_acs(geography = "zcta", variables =  "C02003_003")

#eliminate un-needed columns from white
#white <-  select(white, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#white <- white %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent <0.2)

#add labels to white
#colnames(white)[2:3] <- paste("white population", colnames(white)[2:3], sep = "_")

#turning data in GEOID into integer class
#white$GEOID <- as.integer(white$GEOID)

#bring together the 20152016 table with the dataset on white
#schools201516 <- inner_join(schools201516, white, by = c( "Zip" = "GEOID")) 

# this join removes too many observations, have to remove this variable

#estimate of total population of race who are solely african-american
#black <- get_acs(geography = "zcta", variables =  "C02003_004")

#eliminate un-needed columns from black
#black <-  select(black, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#black <- black %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent <0.2)

#add labels to black
#colnames(black)[2:3] <- paste("black population", colnames(black)[2:3], sep = "_")

#turning data in GEOID into integer class
#black$GEOID <- as.integer(black$GEOID)

#bring together the 20152016 table with the dataset on black
#schools201516 <- inner_join(schools201516, black, by = c( "Zip" = "GEOID")) 

#this join removes too many observations, have to remove this variable

#estimate of total population by race who are hispanic
#hispanic <- get_acs(geography = "zcta", variables =  "B03002_012")

#eliminate un-needed columns from hispanic
#hispanic <-  select(hispanic, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#hispanic <- hispanic %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent <0.2)

#add labels to hispanic
#colnames(hispanic)[2:3] <- paste("hispanic population", colnames(hispanic)[2:3], sep = "_")

#turning data in GEOID into integer class
#hispanic$GEOID <- as.integer(hispanic$GEOID)

#bring together the 20152016 table with the dataset on hispanic
#schools201516 <- inner_join(schools201516, hispanic, by = c( "Zip" = "GEOID")) 

#this join removes too many observations, have to remove this variable

#estimate of total population by race who are asian
#asian <- get_acs(geography = "zcta", variables =  "C02003_006")

#eliminate un-needed columns from asian
#asian <-  select(asian, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#asian <- asian %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent <0.2)

#add labels to asian
#colnames(asian)[2:3] <- paste("asian population", colnames(asian)[2:3], sep = "_")

#turning data in GEOID into integer class
#asian$GEOID <- as.integer(asian$GEOID)

#bring together the 20152016 table with the dataset on asian
#schools201516 <- inner_join(schools201516, asian, by = c( "Zip" = "GEOID")) 

#this join removes too many observations, have to remove this variable

#NATIVE OR FOREIGN-Born, children and parents
#estimate of # of children 6-18 years old, living with 2 parents, child is native born
#native_child_2_parents <- get_acs(geography = "zcta", variables =  "B05009_022")

#eliminate un-needed columns from native child 2 parents
#native_child_2_parents <-  select(native_child_2_parents, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#native_child_2_parents <- native_child_2_parents %>% mutate(MOE_percent = moe/estimate) %>% filter(MOE_percent <0.2)

#add labels to native child 2 parents
#colnames(native_child_2_parents)[2:3] <- paste("native child, 6-18, living with 2 parents population",
                                               #colnames(native_child_2_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
#native_child_2_parents$GEOID <- as.integer(native_child_2_parents$GEOID)

#bring together the 20152016 table with the dataset on native child 2 parents
#schools201516 <- inner_join(schools201516, native_child_2_parents, by = c( "Zip" = "GEOID")) 

#this join removes too many observations, have to remove this variable

#estimate of # of children from 6-18 years old, living with 1 parent, child is native born
#native_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_032")

#eliminate un-needed columns from native child 1 parent
#native_child_1_parent <-  select(native_child_1_parent, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#native_child_1_parent <- native_child_1_parent %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to native child 1 parent
#colnames(native_child_1_parent)[2:3] <- paste("native child, 6-18, living with 1 parent population",
                                              #colnames(native_child_1_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
#native_child_1_parent$GEOID <- as.integer(native_child_1_parent$GEOID)

#bring together the 20152016 table with the dataset on native child 1 parent
#schools201516 <- inner_join(schools201516, native_child_1_parent, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate of # of children from 6-18 years old, living with 2 parents, child is foreign born
#foreign_child_2_parents <-  get_acs(geography = "zcta", variables =  "B05009_023")

#eliminate un-needed columns from foreign child 2 parents
#foreign_child_2_parents <-  select(foreign_child_2_parents, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#foreign_child_2_parents <- foreign_child_2_parents %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#too few observations left after the filter, have to eliminate this variable

#estimate of # of children from 6-18 years old, living with 1 parent, child is foreign born
#foreign_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_015")

#eliminate un-needed columns from foreign child 1 parent
#foreign_child_1_parent <-  select(foreign_child_1_parent, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#foreign_child_1_parent <- foreign_child_1_parent %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#0 observations after filter, have to remove this variable

#estimate, children 6-17, living with 2 native born parents
#two_native_parents <- get_acs(geography = "zcta", variables =  "B05009_024")

#eliminate un-needed columns from  child 2 native parent
#two_native_parents <-  select(two_native_parents, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#two_native_parents <- two_native_parents %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to child 2 native parents
#colnames(two_native_parents)[2:3] <- paste("child, 6-18, living with 2 native parents population",
                                           #colnames(two_native_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
#two_native_parents$GEOID <- as.integer(two_native_parents$GEOID)

#bring together the 20152016 table with the dataset on child, 2 native parents
#schools201516 <- inner_join(schools201516, two_native_parents, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate, children 6-17, living with 1 native born parent
#one_native_parent <- get_acs(geography = "zcta", variables =  "B05009_034")

#eliminate un-needed columns from  child 1 native parent
#one_native_parent <-  select(one_native_parent, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#one_native_parent <- one_native_parent %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to child 1 native parent
#colnames(one_native_parent)[2:3] <- paste("child, 6-18, living with 1 native parent population",
                                          #colnames(one_native_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
#one_native_parent$GEOID <- as.integer(one_native_parent$GEOID)

#bring together the 20152016 table with the dataset on child, 1 native parent
#schools201516 <- inner_join(schools201516, one_native_parent, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate, children 6-17, living with 2 foreign born parents
#two_foreign_born_parents <- get_acs(geography = "zcta", variables =  "B05009_025")

#eliminate un-needed columns from  child 2 foreign born paren
#two_foreign_born_parents<-  select(two_foreign_born_parents, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#two_foreign_born_parents <- two_foreign_born_parents %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to child 2 foreign born parent
#colnames(two_foreign_born_parents)[2:3] <- paste("child, 6-18, living with 2 foreign-born parents population",
                                                 #colnames(two_foreign_born_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
#two_foreign_born_parents$GEOID <- as.integer(two_foreign_born_parents$GEOID)

#bring together the 20152016 table with the dataset on child, 2 foreign born parents
#schools201516 <- inner_join(schools201516, two_foreign_born_parents, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable from the data

#estimate, child 6-17, living with 1 foreign-born parent
#one_foreign_born_parent <- get_acs(geography = "zcta", variables =  "B05009_035")

#eliminate un-needed columns from  child 1 foreign born parent
#one_foreign_born_parent<-  select(one_foreign_born_parent, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#one_foreign_born_parent <- one_foreign_born_parent %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#only 184 observations left after filter, cannot use this variable

#ESTIMATE OF CHILDREN LIVING WITH 1 OR 2 PARENTS
#estimate, child from 6-18, living with 1 parent
#child_only_parent <- get_acs(geography = "zcta", variables =  "B05009_031")

#eliminate un-needed columns from  child 1 parent
#child_only_parent<-  select(child_only_parent, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#child_only_parent <- child_only_parent %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to child 1  parent
#colnames(child_only_parent)[2:3] <- paste("child, 6-18, living with 1 parent population",
                                          #colnames(child_only_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
#child_only_parent$GEOID <- as.integer(child_only_parent$GEOID)

#bring together the 20152016 table with the dataset on child, 1 parent
#schools201516 <- inner_join(schools201516, child_only_parent, by = c( "Zip" = "GEOID"))

#estimate, child 6-18, living with 2 parents
#child_two_parents <- get_acs(geography = "zcta", variables =  "B05009_021")

#eliminate un-needed columns from  child 2 parents
#child_two_parents<-  select(child_two_parents, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#child_two_parents <- child_two_parents %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels to child 2  parents
#colnames(child_two_parents)[2:3] <- paste("child, 6-18, living with 2 parents population",
                                          #colnames(child_two_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
#child_two_parents$GEOID <- as.integer(child_two_parents$GEOID)

#bring together the 20152016 table with the dataset on child,2 parents
#schools201516 <- inner_join(schools201516, child_two_parents, by = c( "Zip" = "GEOID"))

#this join removes too many observations, cannot use this variable

#LANGUAGE
#estimate of population that speak only English
speak_only_english <- get_acs(geography = "zcta", variables =  "B06007_002")

#eliminate un-needed columns from only English
speak_only_english<-  select(speak_only_english, GEOID, estimate, moe)

#filter results with MOE higher than 20%
speak_only_english <- speak_only_english %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels to only English
colnames(speak_only_english)[2:3] <- paste("population only speaks English",
                                           colnames(speak_only_english)[2:3], sep = "_")
#turning data in GEOID into integer class
speak_only_english$GEOID <- as.integer(speak_only_english$GEOID)

#bring together the 20152016 table with the dataset on only English
schools201516 <- inner_join(schools201516, speak_only_english, by = c( "Zip" = "GEOID"))

#estimate of population that speak Spanish whose English is rated as "not well"
#speak_spanish_not_great_english <- get_acs(geography = "zcta", variables =  "B16005_007")

#eliminate un-needed columns from speak Spanish, not great Englsih
#speak_spanish_not_great_english<-  select(speak_spanish_not_great_english, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#speak_spanish_not_great_english <- speak_spanish_not_great_english %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#only 142 observations left after filter, cannot use this variable

#EMPLOYMENT
#estimate of  total unemployed, 18-64 years old
#total_unemployed <- get_acs(geography = "zcta", variables =  "B27011_015")

#eliminate un-needed columns from unemployed
#total_unemployed<-  select(total_unemployed, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#total_unemployed <- total_unemployed %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels unemployed
#colnames(total_unemployed)[2:3] <- paste("unemployed, 18-64, population",
                                         #colnames(total_unemployed)[2:3], sep = "_")

#turning data in GEOID into integer class
#total_unemployed$GEOID <- as.integer(total_unemployed$GEOID)

#bring together the 20152016 table with the dataset on unemployed
#schools201516 <- inner_join(schools201516, total_unemployed, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate of total employed
total_employed <- get_acs(geography = "zcta", variables =  "B27011_003")

#eliminate un-needed columns from employed
total_employed<-  select(total_employed, GEOID, estimate, moe)

#filter results with MOE higher than 20%
total_employed <- total_employed %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels employed
colnames(total_employed)[2:3] <- paste("total employed population",
                                       colnames(total_employed)[2:3], sep = "_")
#turning data in GEOID into integer class
total_employed$GEOID <- as.integer(total_employed$GEOID)

#bring together the 20152016 table with the dataset on employed
schools201516 <- inner_join(schools201516, total_employed, by = c( "Zip" = "GEOID"))

#POVERTY and FOOD STAMPS
#estimate of total who received food stamps in past 12 months and are below poverty line
#below_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_003")

#eliminate un-needed columns from poor got food stamps
#below_poverty_received_stamps<-  select(below_poverty_received_stamps, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#below_poverty_received_stamps <- below_poverty_received_stamps %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels poor got food stamps
#colnames(below_poverty_received_stamps)[2:3] <- paste("poor and received food stamps",
                                                      #colnames(below_poverty_received_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
#below_poverty_received_stamps$GEOID <- as.integer(below_poverty_received_stamps$GEOID)

#bring together the 20152016 table with the dataset on poor who got food stams
#schools201516 <- inner_join(schools201516, below_poverty_received_stamps, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate of total who received food stamps, in past 12 months, and above or at poverty line
#above_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_004")

#eliminate un-needed columns from above poverty level and got food stamps
#above_poverty_received_stamps<-  select(above_poverty_received_stamps, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#above_poverty_received_stamps <- above_poverty_received_stamps %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels above poverty level and got food stamps
#colnames(above_poverty_received_stamps)[2:3] <- paste("less poor and received food stamps",
                                                      #colnames(above_poverty_received_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
#above_poverty_received_stamps$GEOID <- as.integer(above_poverty_received_stamps$GEOID)

#bring together the 20152016 table with the dataset on above poverty level who got food stams
#schools201516 <- inner_join(schools201516, above_poverty_received_stamps, by = c( "Zip" = "GEOID"))

#this join removes too many observations, have to remove this variable

#estimate of total who did not receive food stamps, past 12 months, and are below poverty line
#below_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_006")

#eliminate un-needed columns from below poverty level and no food stamps
#below_poverty_no_stamps<-  select(below_poverty_no_stamps, GEOID, estimate, moe)

#filter results with MOE higher than 20%
#below_poverty_no_stamps <- below_poverty_no_stamps %>% mutate(MOE_percent = moe/estimate) %>% 
  #filter(MOE_percent <0.2)

#add labels below poverty level and no food stamps
#colnames(below_poverty_no_stamps)[2:3] <- paste("poor, no food stamps",
                                                #colnames(below_poverty_no_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
#below_poverty_no_stamps$GEOID <- as.integer(below_poverty_no_stamps$GEOID)

#bring together the 20152016 table with the dataset on below poverty level, no food stamps
#schools201516 <- inner_join(schools201516, below_poverty_no_stamps, by = c( "Zip" = "GEOID"))

#this join removes too many observations from the data, have to remove this variable

#estimate, total, did not receive food stamps, past 12 months above or at the poverty line
above_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_007")

#eliminate un-needed columns from above poverty level and no food stamps
above_poverty_no_stamps<-  select(above_poverty_no_stamps, GEOID, estimate, moe)

#filter results with MOE higher than 20%
above_poverty_no_stamps <- above_poverty_no_stamps %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels above poverty level and no food stamps
colnames(above_poverty_no_stamps)[2:3] <- paste("less poor, no food stamps",
                                                colnames(above_poverty_no_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
above_poverty_no_stamps$GEOID <- as.integer(above_poverty_no_stamps$GEOID)

#bring together the 20152016 table with the dataset on above poverty level, no food stamps
schools201516 <- inner_join(schools201516, above_poverty_no_stamps, by = c( "Zip" = "GEOID"))

#HOUSING: RENTING OR OWNING?
#estimate, total, rental units
renting <- get_acs(geography = "zcta", variables =  "B25012_010")

#eliminate un-needed columns from renting
renting <-  select(renting, GEOID, estimate, moe)

#filter results with MOE higher than 20%
renting <- renting %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels renting
colnames(renting)[2:3] <- paste("rental units",
                                colnames(renting)[2:3], sep = "_")
#turning data in GEOID into integer class
renting$GEOID <- as.integer(renting$GEOID)

#bring together the 20152016 table with the dataset on renting
schools201516 <- inner_join(schools201516, renting, by = c( "Zip" = "GEOID"))

#estimate, total, owned housing units 
owning <- get_acs(geography = "zcta", variables =  "B25012_002")

#eliminate un-needed columns from owning
owning <-  select(owning, GEOID, estimate, moe)

#filter results with MOE higher than 20%
owning <- owning %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels owning
colnames(owning)[2:3] <- paste("owned units",
                               colnames(owning)[2:3], sep = "_")

#turning data in GEOID into integer class
owning$GEOID <- as.integer(owning$GEOID)

#bring together the 20152016 table with the dataset on renting
schools201516 <- inner_join(schools201516, owning, by = c( "Zip" = "GEOID"))

#AGGREGATE EARNINGS
#estimate, aggregate earnings, 2016
earnings <-  get_acs(geography = "zcta", variables =  "B20003_001")

#eliminate un-needed columns from earnings
earnings <-  select(earnings, GEOID, estimate, moe)

#filter results with MOE higher than 20%
earnings <- earnings %>% mutate(MOE_percent = moe/estimate) %>% 
  filter(MOE_percent <0.2)

#add labels earnings
colnames(earnings)[2:3] <- paste("aggregate earnings",
                                 colnames(earnings)[2:3], sep = "_")

#turning data in GEOID into integer class
earnings$GEOID <- as.integer(earnings$GEOID)

#bring together the 20152016 table with the dataset on earnings
schools201516 <- inner_join(schools201516, earnings, by = c( "Zip" = "GEOID"))

#taking zip codes out of the data set
schools201516 <-  select(schools201516, -Zip)


#remove un-needed columns from schools2014
schools2014 <-  select(schools2014, Name.of.School, Supportive.Environment,
                       Ambitious.Instruction, Effective.Leaders, Collaborative.Teachers, Safe, Involved.Family, NWEA.Reading.Attainment.Percentile.Grade.3, 
                       NWEA.Math.Attainment.Percentile.Grade.3, Location,
                       School.ID)

#put 2014 labels on columns
colnames(schools2014)[8:9] <- paste("2014", colnames(schools2014)[8:9], sep = "_")

#join the datasets together
schools14_16 <- inner_join(schools2014, schools201516, by = c("School.ID" = "School_ID"))

#separating the Location column into Longtitude and Latitude
schools14_16 <-  separate(schools14_16, Location, c("Longitude", "Latitude"), sep ="," )

#remove brackets from Longitude and Latitude column
schools14_16$Longitude <- sub("[(]", "", schools14_16$Longitude)
schools14_16$Latitude <- sub("[)]", "", schools14_16$Latitude)


#fixing the column order
col_order <- c("Name.of.School", 
               "Supportive.Environment", 
               "Ambitious.Instruction", 
               "Effective.Leaders", 
               "Collaborative.Teachers", 
               "Safe", 
               "Involved.Family", 
               "MedianIncome.2012.2016", 
               "X..of.Pop.graduated.from.HS.2012.2016",
               "median age_estimate", 
               "total pop_estimate",
               "female_estimate",
               "male_estimate",
               "population only speaks English_estimate", 
               "less poor, no food stamps_estimate", 
               "total employed population_estimate",  
               "rental units_estimate",  
               "owned units_estimate", 
               "aggregate earnings_estimate",
               "2014_NWEA.Reading.Attainment.Percentile.Grade.3", 
               "2015_NWEA_Reading_Attainment_Grade_3_Pct", 
               "2016_NWEA_Reading_Attainment_Grade_3_Pct", 
               "2014_NWEA.Math.Attainment.Percentile.Grade.3", 
               "2015_NWEA_Math_Attainment_Grade_3_Pct", 
               "2016_NWEA_Math_Attainment_Grade_3_Pct",
               "Longitude", 
               "Latitude")

schools14_16 <- schools14_16[, col_order]

#correlations for Reading percentiles
#removing na values
schools1416b <- na.omit(schools14_16)

#creating matrix for correlation, demographic info are rows, percentiles for 2016 are columns
x <- schools1416b[8:21]
y <- schools1416b[22]

#correlating demographic data and reading 2016 variable
cor(x, y)

#median income, pop that graduated from high school, aggregated earnings,
#2014 reading and 2015 reading percentiles were most correlated with 2016 reading percentiles
# (only income and 2014, 2015 are over 0.5)

#correlations for Math scores
#creating matrix for correlation
w <- schools1416b[8:19]
k <- schools1416b[23:24]
z <-  schools1416b[25]

#correlating demographic info with 2016 math percentiles
cor(w,z)
#correlating 2014 and 2015 math percentiles with 2016 math percentiles
cor(k,z)

#median income, those who did not receive food stamps and are above the poverty line,
#aggregate earnings, 2014 and 2015 math percentiles were most strongly correlated with 2016 percentiles
#only income, 2014 and 2015 percentiles are over 0.5, in terms of correlation



#can remove these variables, which have a correlation with both outcome variables that is less than
#0.3, from dataset:
#median age
#total pop
#female
#male
#population that only speaks english
#total employed
#rental units
#owned units

schools14_16 <-  schools14_16[,(-(10:14))]
schools14_16 <- schools14_16[,(-(11:13))]


#fixing the column names
colnames(schools14_16)[colnames(schools14_16)=="Supportive.Environment"] <- "Environment Rating"

colnames(schools14_16)[colnames(schools14_16)=="Ambitious.Instruction"] <- "Instruction Rating"

colnames(schools14_16)[colnames(schools14_16)=="Effective.Leaders"] <- "Leadership Rating"

colnames(schools14_16)[colnames(schools14_16)=="Collaborative.Teachers"] <- "Collaboration Rating"

colnames(schools14_16)[colnames(schools14_16)=="Safe"] <- "Safety Rating"

colnames(schools14_16)[colnames(schools14_16)=="Involved.Family"] <- "Family Rating"

colnames(schools14_16)[colnames(schools14_16)=="MedianIncome.2012.2016"] <- "Median Income"

colnames(schools14_16)[colnames(schools14_16)=="X..of.Pop.graduated.from.HS.2012.2016"] <- "H.S.Graduation %"

colnames(schools14_16)[colnames(schools14_16)=="2014_NWEA.Reading.Attainment.Percentile.Grade.3"] <- "2014 NWEA Reading Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="2015_NWEA_Reading_Attainment_Grade_3_Pct"] <- "2015 NWEA Reading Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="2016_NWEA_Reading_Attainment_Grade_3_Pct"] <- "2016 NWEA Reading Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="2014_NWEA.Math.Attainment.Percentile.Grade.3"] <- "2014 NWEA Math Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="2015_NWEA_Math_Attainment_Grade_3_Pct"] <- "2015 NWEA Math Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="2016_NWEA_Math_Attainment_Grade_3_Pct"] <- "2016 NWEA Math Gr.3 Pct"

colnames(schools14_16)[colnames(schools14_16)=="less poor, no food stamps_estimate"] <- "Estimate of Pop. At or Above Poverty Line who Did Not Receive Food Stamps"

colnames(schools14_16)[colnames(schools14_16)=="aggregate earnings_estimate"] <- "Estimate of Aggregate Earnings"

#save the data
write.csv(schools14_16, file = "schools14_16.csv")


