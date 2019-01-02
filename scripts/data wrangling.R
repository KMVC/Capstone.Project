#load relevant libraries
library(tidyverse)
#install.packages("tidycensus")
library(tidycensus)

#read datasets into R
schools2016 <-  read.csv("Chicagoschools2016.csv", na = c("", "NA"))
schools2015 <-  read.csv("Chicagoschools2015.csv", na = c("", "NA"))
schools2014 <-  read.csv("Chicagoschools2014.csv", na = c("", "NA"))
allzips <- read.csv("allzips.csv")


#eliminate all schools from schools2015 schools2016 that are not elementary schools (high schools and middle schools)
schools2015 <-  filter(schools2015, Primary_Category == "ES")
schools2016 <-  filter(schools2016, Primary_Category == "ES")

#eliminate un-needed columns from schools2016
schools2016 <-  select(schools2016, ï..School_ID, 
                       NWEA_Reading_Attainment_Grade_3_Pct, NWEA_Math_Attainment_Grade_3_Pct)

#put 2016 labels on columns
colnames(schools2016)[2:3] <- paste("2016", colnames(schools2016)[2:3], sep = "_")


#eliminate un-needed columns from schools2015
schools2015 <-  select(schools2015, 
                       ï..School_ID, 
                       Zip,
                       NWEA_Reading_Attainment_Grade_3_Pct, 
                       NWEA_Math_Attainment_Grade_3_Pct)


#put label on some columns marking them as 2015
colnames(schools2015)[3:4] <- paste("2015", colnames(schools2015)[3:4], sep = "_")


#bring the two school tables together
schools201516 <- inner_join(schools2015, schools2016, by = "ï..School_ID") 


#bring together the table with the dataset on income and graduation percentages
schools201516 <- full_join(schools201516, allzips, by = c( "Zip" = "Zips"))

#finding and pulling demographic data from the American Community Survey 2012-2015 (ACS)
#all data pulled by zip code, estimate are of total population

#my_key <- census_api_key("7e17eb37151215db6ed133db6d107bb6143a9e37", install = TRUE)

#creating my census key
#my_key <- census_api_key("7e17eb37151215db6ed133db6d107bb6143a9e37", install = TRUE)

v15 <-  load_variables(2015, "acs5", cache = TRUE)

#AGE
#median age by zip code
median_age <- get_acs(geography = "zcta", variables =  "B01002_001")

#eliminate un-needed columns from median_age
median_age <-  select(median_age, GEOID, estimate, moe)

#add labels to median_age
colnames(median_age)[2:3] <- paste("median age", colnames(median_age)[2:3], sep = "_")

#turning data in GEOID into integer class
median_age$GEOID <- as.integer(median_age$GEOID)


#bring together the 20152016 table with the dataset on median age
schools201516 <- inner_join(schools201516, median_age, by = c( "Zip" = "GEOID")) 

#RACE by zip code
#estimate of total population of 2 or more races
two_race_or_more <- get_acs(geography = "zcta", variables =  "C02003_010")

#eliminate un-needed columns from two_race_or_more
two_race_or_more <-  select(two_race_or_more, GEOID, estimate, moe)

#add labels to 2 races or more
colnames(two_race_or_more)[2:3] <- paste("multiracial population", colnames(two_race_or_more)[2:3], sep = "_")

#turning data in GEOID into integer class
two_race_or_more$GEOID <- as.integer(two_race_or_more$GEOID)


#bring together the 20152016 table with the dataset on 2 or more races
schools201516 <- inner_join(schools201516, two_race_or_more, by = c( "Zip" = "GEOID")) 

#estimate of total population of race who are solely white
white <- get_acs(geography = "zcta", variables =  "C02003_003")

#eliminate un-needed columns from white
white <-  select(white, GEOID, estimate, moe)

#add labels to white
colnames(white)[2:3] <- paste("white population", colnames(white)[2:3], sep = "_")

#turning data in GEOID into integer class
white$GEOID <- as.integer(white$GEOID)

#bring together the 20152016 table with the dataset on white
schools201516 <- inner_join(schools201516, white, by = c( "Zip" = "GEOID")) 

#estimate of total population of race who are solely african-american
black <- get_acs(geography = "zcta", variables =  "C02003_004")

#eliminate un-needed columns from black
black <-  select(black, GEOID, estimate, moe)

#add labels to black
colnames(black)[2:3] <- paste("black population", colnames(black)[2:3], sep = "_")

#turning data in GEOID into integer class
black$GEOID <- as.integer(black$GEOID)

#bring together the 20152016 table with the dataset on black
schools201516 <- inner_join(schools201516, black, by = c( "Zip" = "GEOID")) 

#estimate of total population by race who are hispanic
hispanic <- get_acs(geography = "zcta", variables =  "B03002_012")

#eliminate un-needed columns from hispanic
hispanic <-  select(hispanic, GEOID, estimate, moe)

#add labels to hispanic
colnames(hispanic)[2:3] <- paste("hispanic population", colnames(hispanic)[2:3], sep = "_")

#turning data in GEOID into integer class
hispanic$GEOID <- as.integer(hispanic$GEOID)

#bring together the 20152016 table with the dataset on hispanic
schools201516 <- inner_join(schools201516, hispanic, by = c( "Zip" = "GEOID")) 

#estimate of total population by race who are asian
asian <- get_acs(geography = "zcta", variables =  "C02003_006")

#eliminate un-needed columns from asian
asian <-  select(asian, GEOID, estimate, moe)

#add labels to asian
colnames(asian)[2:3] <- paste("asian population", colnames(asian)[2:3], sep = "_")

#turning data in GEOID into integer class
asian$GEOID <- as.integer(asian$GEOID)

#bring together the 20152016 table with the dataset on asian
schools201516 <- inner_join(schools201516, asian, by = c( "Zip" = "GEOID")) 

#NATIVE OR FOREIGN-Born, children and parents
#estimate of # of children 6-18 years old, living with 2 parents, child is native born
native_child_2_parents <- get_acs(geography = "zcta", variables =  "B05009_022")

#eliminate un-needed columns from native child 2 parents
native_child_2_parents <-  select(native_child_2_parents, GEOID, estimate, moe)

#add labels to native child 2 parents
colnames(native_child_2_parents)[2:3] <- paste("native child, 6-18, living with 2 parents population",
                                               colnames(native_child_2_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
native_child_2_parents$GEOID <- as.integer(native_child_2_parents$GEOID)

#bring together the 20152016 table with the dataset on native child 2 parents
schools201516 <- inner_join(schools201516, native_child_2_parents, by = c( "Zip" = "GEOID")) 

#estimate of # of children from 6-18 years old, living with 1 parent, child is native born
native_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_032")

#eliminate un-needed columns from native child 1 parent
native_child_1_parent <-  select(native_child_1_parent, GEOID, estimate, moe)

#add labels to native child 1 parent
colnames(native_child_1_parent)[2:3] <- paste("native child, 6-18, living with 1 parent population",
                                               colnames(native_child_1_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
native_child_1_parent$GEOID <- as.integer(native_child_1_parent$GEOID)

#bring together the 20152016 table with the dataset on native child 1 parent
schools201516 <- inner_join(schools201516, native_child_1_parent, by = c( "Zip" = "GEOID"))

#estimate of # of children from 6-18 years old, living with 2 parents, child is foreign born
foreign_child_2_parents <-  get_acs(geography = "zcta", variables =  "B05009_023")

#eliminate un-needed columns from foreign child 2 parents
foreign_child_2_parents <-  select(foreign_child_2_parents, GEOID, estimate, moe)

#add labels to foreign child 2 parents
colnames(foreign_child_2_parents)[2:3] <- paste("foreign-born child, 6-18, living with 2 parents population",
                                              colnames(foreign_child_2_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
foreign_child_2_parents$GEOID <- as.integer(foreign_child_2_parents$GEOID)

#bring together the 20152016 table with the dataset on foreign born child, 2 parents
schools201516 <- inner_join(schools201516, foreign_child_2_parents, by = c( "Zip" = "GEOID"))

#estimate of # of children from 6-18 years old, living with 1 parent, child is foreign born
foreign_child_1_parent <- get_acs(geography = "zcta", variables =  "B05009_015")

#eliminate un-needed columns from foreign child 1 parent
foreign_child_1_parent <-  select(foreign_child_1_parent, GEOID, estimate, moe)

#add labels to foreign child 1 parent
colnames(foreign_child_1_parent)[2:3] <- paste("foreign-born child, 6-18, living with 1 parent population",
                                                colnames(foreign_child_1_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
foreign_child_1_parent$GEOID <- as.integer(foreign_child_1_parent$GEOID)

#bring together the 20152016 table with the dataset on foreign born child, 1 parent
schools201516 <- inner_join(schools201516, foreign_child_1_parent, by = c( "Zip" = "GEOID"))

#estimate, children 6-17, living with 2 native born parents
two_native_parents <- get_acs(geography = "zcta", variables =  "B05009_024")

#eliminate un-needed columns from  child 2 native parent
two_native_parents <-  select(two_native_parents, GEOID, estimate, moe)

#add labels to child 2 native parents
colnames(two_native_parents)[2:3] <- paste("child, 6-18, living with 2 native parents population",
                                               colnames(two_native_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
two_native_parents$GEOID <- as.integer(two_native_parents$GEOID)

#bring together the 20152016 table with the dataset on child, 2 native parents
schools201516 <- inner_join(schools201516, two_native_parents, by = c( "Zip" = "GEOID"))

#estimate, children 6-17, living with 1 native born parent
one_native_parent <- get_acs(geography = "zcta", variables =  "B05009_034")

#eliminate un-needed columns from  child 1 native parent
one_native_parent <-  select(one_native_parent, GEOID, estimate, moe)

#add labels to child 1 native parent
colnames(one_native_parent)[2:3] <- paste("child, 6-18, living with 1 native parent population",
                                           colnames(one_native_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
one_native_parent$GEOID <- as.integer(one_native_parent$GEOID)

#bring together the 20152016 table with the dataset on child, 1 native parent
schools201516 <- inner_join(schools201516, one_native_parent, by = c( "Zip" = "GEOID"))

#estimate, children 6-17, living with 2 foreign born parents
two_foreign_born_parents <- get_acs(geography = "zcta", variables =  "B05009_025")

#eliminate un-needed columns from  child 2 foreign born paren
two_foreign_born_parents<-  select(two_foreign_born_parents, GEOID, estimate, moe)

#add labels to child 2 foreign born parent
colnames(two_foreign_born_parents)[2:3] <- paste("child, 6-18, living with 2 foreign-born parents population",
                                          colnames(two_foreign_born_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
two_foreign_born_parents$GEOID <- as.integer(two_foreign_born_parents$GEOID)

#bring together the 20152016 table with the dataset on child, 2 foreign born parents
schools201516 <- inner_join(schools201516, two_foreign_born_parents, by = c( "Zip" = "GEOID"))

#estimate, child 6-17, living with 1 foreign-born parent
one_foreign_born_parent <- get_acs(geography = "zcta", variables =  "B05009_035")

#eliminate un-needed columns from  child 1 foreign born parent
one_foreign_born_parent<-  select(one_foreign_born_parent, GEOID, estimate, moe)

#add labels to child 1 foreign born parent
colnames(one_foreign_born_parent)[2:3] <- paste("child, 6-18, living with 1 foreign-born parent population",
                                                 colnames(one_foreign_born_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
one_foreign_born_parent$GEOID <- as.integer(one_foreign_born_parent$GEOID)

#bring together the 20152016 table with the dataset on child, 1 foreign born parent
schools201516 <- inner_join(schools201516, one_foreign_born_parent, by = c( "Zip" = "GEOID"))

#ESTIMATE OF CHILDREN LIVING WITH 1 OR 2 PARENTS
#estimate, child from 6-18, living with 1 parent
child_only_parent <- get_acs(geography = "zcta", variables =  "B05009_031")

#eliminate un-needed columns from  child 1 parent
child_only_parent<-  select(child_only_parent, GEOID, estimate, moe)

#add labels to child 1  parent
colnames(child_only_parent)[2:3] <- paste("child, 6-18, living with 1 parent population",
                                                colnames(child_only_parent)[2:3], sep = "_")
#turning data in GEOID into integer class
child_only_parent$GEOID <- as.integer(child_only_parent$GEOID)

#bring together the 20152016 table with the dataset on child, 1 parent
schools201516 <- inner_join(schools201516, child_only_parent, by = c( "Zip" = "GEOID"))

#estimate, child 6-18, living with 2 parents
child_two_parents <- get_acs(geography = "zcta", variables =  "B05009_021")

#eliminate un-needed columns from  child 2 parents
child_two_parents<-  select(child_two_parents, GEOID, estimate, moe)

#add labels to child 2  parents
colnames(child_two_parents)[2:3] <- paste("child, 6-18, living with 2 parents population",
                                          colnames(child_two_parents)[2:3], sep = "_")
#turning data in GEOID into integer class
child_two_parents$GEOID <- as.integer(child_two_parents$GEOID)

#bring together the 20152016 table with the dataset on child,2 parents
schools201516 <- inner_join(schools201516, child_two_parents, by = c( "Zip" = "GEOID"))

#LANGUAGE
#estimate of population that speak only English
speak_only_english <- get_acs(geography = "zcta", variables =  "B06007_002")

#eliminate un-needed columns from only English
speak_only_english<-  select(speak_only_english, GEOID, estimate, moe)

#add labels to only English
colnames(speak_only_english)[2:3] <- paste("population only speaks English",
                                          colnames(speak_only_english)[2:3], sep = "_")
#turning data in GEOID into integer class
speak_only_english$GEOID <- as.integer(speak_only_english$GEOID)

#bring together the 20152016 table with the dataset on only English
schools201516 <- inner_join(schools201516, speak_only_english, by = c( "Zip" = "GEOID"))

#estimate of population that speak Spanish whose English is rated as "not well"
speak_spanish_not_great_english <- get_acs(geography = "zcta", variables =  "B16005_007")

#eliminate un-needed columns from speak Spanish, not great Englsih
speak_spanish_not_great_english<-  select(speak_spanish_not_great_english, GEOID, estimate, moe)

#add labels to speak Spanish, not great English
colnames(speak_spanish_not_great_english)[2:3] <- paste("population speaks Spanish, low English skills",
                                           colnames(speak_spanish_not_great_english)[2:3], sep = "_")
#turning data in GEOID into integer class
speak_spanish_not_great_english$GEOID <- as.integer(speak_spanish_not_great_english$GEOID)

#bring together the 20152016 table with the dataset on speak Spanish, not great English
schools201516 <- inner_join(schools201516, speak_spanish_not_great_english, by = c( "Zip" = "GEOID"))

#EMPLOYMENT
#estimate of  total unemployed, 18-64 years old
total_unemployed <- get_acs(geography = "zcta", variables =  "B27011_015")

#eliminate un-needed columns from unemployed
total_unemployed<-  select(total_unemployed, GEOID, estimate, moe)

#add labels unemployed
colnames(total_unemployed)[2:3] <- paste("unemployed, 18-64, population",
                                                        colnames(total_unemployed)[2:3], sep = "_")
#turning data in GEOID into integer class
total_unemployed$GEOID <- as.integer(total_unemployed$GEOID)

#bring together the 20152016 table with the dataset on unemployed
schools201516 <- inner_join(schools201516, total_unemployed, by = c( "Zip" = "GEOID"))

#estimate of total employed
total_employed <- get_acs(geography = "zcta", variables =  "B27011_003")

#eliminate un-needed columns from employed
total_employed<-  select(total_employed, GEOID, estimate, moe)

#add labels employed
colnames(total_employed)[2:3] <- paste("total employed population",
                                         colnames(total_employed)[2:3], sep = "_")
#turning data in GEOID into integer class
total_employed$GEOID <- as.integer(total_employed$GEOID)

#bring together the 20152016 table with the dataset on employed
schools201516 <- inner_join(schools201516, total_employed, by = c( "Zip" = "GEOID"))

#POVERTY and FOOD STAMPS
#estimate of total who received food stamps in past 12 months and are below poverty line
below_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_003")

#eliminate un-needed columns from poor got food stamps
below_poverty_received_stamps<-  select(below_poverty_received_stamps, GEOID, estimate, moe)

#add labels poor got food stamps
colnames(below_poverty_received_stamps)[2:3] <- paste("poor and received food stamps",
                                       colnames(below_poverty_received_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
below_poverty_received_stamps$GEOID <- as.integer(below_poverty_received_stamps$GEOID)

#bring together the 20152016 table with the dataset on poor who got food stams
schools201516 <- inner_join(schools201516, below_poverty_received_stamps, by = c( "Zip" = "GEOID"))

#estimate of total who received food stamps, in past 12 months, and above or at poverty line
above_poverty_received_stamps <- get_acs(geography = "zcta", variables =  "B22003_004")

#eliminate un-needed columns from above poverty level and got food stamps
above_poverty_received_stamps<-  select(above_poverty_received_stamps, GEOID, estimate, moe)

#add labels above poverty level and got food stamps
colnames(above_poverty_received_stamps)[2:3] <- paste("less poor and received food stamps",
                                                      colnames(above_poverty_received_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
above_poverty_received_stamps$GEOID <- as.integer(above_poverty_received_stamps$GEOID)

#bring together the 20152016 table with the dataset on above poverty level who got food stams
schools201516 <- inner_join(schools201516, above_poverty_received_stamps, by = c( "Zip" = "GEOID"))

#estimate of total who did not receive food stamps, past 12 months, and are below poverty line
below_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_006")

#eliminate un-needed columns from below poverty level and no food stamps
below_poverty_no_stamps<-  select(below_poverty_no_stamps, GEOID, estimate, moe)

#add labels below poverty level and no food stamps
colnames(below_poverty_no_stamps)[2:3] <- paste("poor, no food stamps",
                                                      colnames(below_poverty_no_stamps)[2:3], sep = "_")
#turning data in GEOID into integer class
below_poverty_no_stamps$GEOID <- as.integer(below_poverty_no_stamps$GEOID)

#bring together the 20152016 table with the dataset on below poverty level, no food stamps
schools201516 <- inner_join(schools201516, below_poverty_no_stamps, by = c( "Zip" = "GEOID"))

#estimate, total, did not receive food stamps, past 12 months above or at the poverty line
above_poverty_no_stamps <- get_acs(geography = "zcta", variables =  "B22003_007")

#eliminate un-needed columns from above poverty level and no food stamps
above_poverty_no_stamps<-  select(above_poverty_no_stamps, GEOID, estimate, moe)

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
                       ï..School.ID)

#put 2014 labels on columns
colnames(schools2014)[8:9] <- paste("2014", colnames(schools2014)[8:9], sep = "_")

#join the datasets together
schools14_16 <- inner_join(schools2014, schools201516, by = c("ï..School.ID" = "ï..School_ID"))

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
               "multiracial population_estimate", 
               "white population_estimate", 
               "black population_estimate", 
               "hispanic population_estimate", 
               "asian population_estimate", 
               "native child, 6-18, living with 2 parents population_estimate", 
               "native child, 6-18, living with 1 parent population_estimate", 
               "foreign-born child, 6-18, living with 2 parents population_estimate", 
               "foreign-born child, 6-18, living with 1 parent population_estimate",  
               "child, 6-18, living with 2 native parents population_estimate", 
               "child, 6-18, living with 1 native parent population_estimate", 
               "child, 6-18, living with 2 foreign-born parents population_estimate", 
               "child, 6-18, living with 1 foreign-born parent population_estimate", 
               "child, 6-18, living with 1 parent population_estimate", 
               "child, 6-18, living with 2 parents population_estimate", 
               "population only speaks English_estimate", 
               "population speaks Spanish, low English skills_estimate",  
               "poor and received food stamps_estimate", 
               "poor, no food stamps_estimate", 
               "less poor and received food stamps_estimate", 
               "less poor, no food stamps_estimate", 
               "unemployed, 18-64, population_estimate",
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

colnames(schools14_16)[colnames(schools14_16)=="median age_estimate"] <- "Median Age Estimate"

colnames(schools14_16)[colnames(schools14_16)=="multiracial population_estimate"] <- "Multiracial Pop. Estimate"

colnames(schools14_16)[colnames(schools14_16)=="white population_estimate"] <- "White Pop. Estimate"

colnames(schools14_16)[colnames(schools14_16)=="black population_estimate"] <- "Black Pop. Estimate"

colnames(schools14_16)[colnames(schools14_16)=="hispanic population_estimate"] <- "Hispanic Pop. Estimate"

colnames(schools14_16)[colnames(schools14_16)=="asian population_estimate"] <- "Asian Pop. Estimate"

colnames(schools14_16)[colnames(schools14_16)=="native child, 6-18, living with 2 parents population_estimate"] <- "Estimate of Native-born Children 6-18, Living with 2 Parents"

colnames(schools14_16)[colnames(schools14_16)=="native child, 6-18, living with 1 parent population_estimate"] <- "Estimate of Native-born Children, 6-18, Living with 1 Parent"

colnames(schools14_16)[colnames(schools14_16)=="foreign-born child, 6-18, living with 2 parents population_estimate"] <- "Estimate of Foreign-born Children, 6-18, Living with 2 Parents"

colnames(schools14_16)[colnames(schools14_16)=="foreign-born child, 6-18, living with 1 parent population_estimate"] <- "Estimate of Foreign-born Children, 6-18, Living with 1 Parent"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 2 native parents population_estimate"] <- "Estimate of Children, 6-18, Living with 2 Native-born Parents"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 1 native parent population_estimate"] <- "Estimate of Children, 6-18, Living with 1 Native-born Parent"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 2 foreign-born parents population_estimate"] <- "Estimate of Children, 6-18, Living with 2 Foreign-Born Parents"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 1 foreign-born parent population_estimate"] <- "Estimate of Children, 6-18, Living with 1 Foreign-Born Parent"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 1 parent population_estimate"] <- "Estimate of Children, 6-18, Living with 1 Parent"

colnames(schools14_16)[colnames(schools14_16)=="child, 6-18, living with 2 parents population_estimate"] <- "Estimate of Children, 6-18, Living with 2 Parents"

colnames(schools14_16)[colnames(schools14_16)=="population only speaks English_estimate"] <- "Estimate of Pop. Speaking only English"

colnames(schools14_16)[colnames(schools14_16)=="population speaks Spanish, low English skills_estimate"] <- "Estimate of Spanish-speaking Pop., Less than Fluent in English"

colnames(schools14_16)[colnames(schools14_16)=="poor and received food stamps_estimate"] <- "Estimate of Poor Who Received Food Stamps"

colnames(schools14_16)[colnames(schools14_16)=="poor, no food stamps_estimate"] <- "Estimate of Poor Who Did Not Receive Food Stamps"

colnames(schools14_16)[colnames(schools14_16)=="less poor and received food stamps_estimate"] <- "Estimate of Pop. At or Above Poverty Line who Received Food Stamps"

colnames(schools14_16)[colnames(schools14_16)=="less poor, no food stamps_estimate"] <- "Estimate of Pop. At or Above Poverty Line who Did Not Receive Food Stamps"

colnames(schools14_16)[colnames(schools14_16)=="unemployed, 18-64, population_estimate"] <- "Estimate of Pop. 18-64, Unemployed"

colnames(schools14_16)[colnames(schools14_16)=="total employed population_estimate"] <- "Estimate of total Pop. Employed"

colnames(schools14_16)[colnames(schools14_16)=="rental units_estimate"] <- "Estimate of Rental Units"

colnames(schools14_16)[colnames(schools14_16)=="owned units_estimate"] <- "Estimate of Owned Units"

colnames(schools14_16)[colnames(schools14_16)=="aggregate earnings_estimate"] <- "Estimate of Aggregate Earnings"

#save the data
write.csv(schools14_16, file = "schools14_16.csv")

