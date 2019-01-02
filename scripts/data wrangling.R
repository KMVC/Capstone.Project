#load relevant libraries
library(tidyverse)

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

