#load relevant libraries
library(tidyverse)
#install.packages("tidycensus")
library(tidycensus)

#read datasets into R
schools2016 <-  read.csv("Chicagoschools2016.csv", na = c("", "NA"))
schools2015 <-  read.csv("Chicagoschools2015.csv", na = c("", "NA"))
schools2014 <-  read.csv("Chicagoschools2014.csv", na = c("", "NA"))
allzips <- read.csv("allzips.csv")


#eliminate all schools from schools2016 that are not elementary schools (high schools and middle schools)
schools2015 <-  filter(schools2015, Primary_Category == "ES")
schools2016 <-  filter(schools2016, Primary_Category == "ES")

#eliminate un-needed columns from schools2016
schools2016 <-  select(schools2016, ï..School_ID, 
                       NWEA_Reading_Attainment_Grade_3_Pct, NWEA_Math_Attainment_Grade_3_Pct)

#put 2016 labels on columns
colnames(schools2016)[2:3] <- paste("2016", colnames(schools2016)[2:3], sep = "_")


#eliminate un-needed columns from schools2015
schools2015 <-  select(schools2015, ï..School_ID, Zip,
                       NWEA_Reading_Attainment_Grade_3_Pct, NWEA_Math_Attainment_Grade_3_Pct)


#put label on some columns marking them as 2015
colnames(schools2015)[3:4] <- paste("2015", colnames(schools2015)[3:4], sep = "_")


#bring the two school tables together
schools201516 <- inner_join(schools2015, schools2016, by = "ï..School_ID") 


#bring together the table with the dataset on income and graduation percentages
schools201516 <- full_join(schools201516, allzips, by = c( "Zip" = "Zips"))

#finding and pulling demographic data from the American Community Survey 2012-2015
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
col_order <- c("Name.of.School", "Supportive.Environment", "Ambitious.Instruction", 
               "Effective.Leaders", "Collaborative.Teachers", "Safe", "Involved.Family", "MedianIncome.2012.2016", 
               "X..of.Pop.graduated.from.HS.2012.2016",
               "2014_NWEA.Reading.Attainment.Percentile.Grade.3", 
               "2015_NWEA_Reading_Attainment_Grade_3_Pct", "2016_NWEA_Reading_Attainment_Grade_3_Pct", 
               "2014_NWEA.Math.Attainment.Percentile.Grade.3", "2015_NWEA_Math_Attainment_Grade_3_Pct", 
               "2016_NWEA_Math_Attainment_Grade_3_Pct","Longitude", "Latitude")

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


#save the data
write.csv(schools14_16, file = "schools14_16.csv")

