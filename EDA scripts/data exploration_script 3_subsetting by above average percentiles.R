#subsetting by above average and below percentiles

schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)


#looking at schools that were above national average for math
schools14_16aamath <-  filter(schools14_16, X2016.NWEA.Math.Gr.3.Pct >= 51)
#204 schools were in the 51st percentile or higher in the data set

summary(schools14_16aamath)


#The 5essentials and above average schools in math

#plotting environment rating
ggplot(schools14_16aamath, aes( x = Environment.Rating )) + geom_bar(fill = "blue")+ 
  ggtitle("Above average in math and Environment Ratings")

#for Environment, neutral is the most frequent score for schools above average in math


#plotting Family rating
ggplot(schools14_16aamath, aes( x = Family.Rating )) + geom_bar(fill = "purple") +
  ggtitle("Above average in math and Family Ratings")
#for Family, Strong is the most frequent score for schools above average in math

#plotting Instruction rating
ggplot(schools14_16aamath, aes( x = Instruction.Rating )) + geom_bar(fill = "pink") + 
  ggtitle("Above average in math and Instruction Ratings")

#strong is most frequent, followed closely by neutral

#plotting Leadership Rating
ggplot(schools14_16aamath, aes( x = Leadership.Rating )) + geom_bar(fill = "white")+ 
  ggtitle("Above average in math and Leadership Ratings")

#Neutral is most frequent, followed by strong

#plotting Collaboration Rating
ggplot(schools14_16aamath, aes( x = Collaboration.Rating )) + geom_bar(fill = "red")+ 
  ggtitle("Above average in math and Collaboration Ratings")
#Neutral most frequent, followed by Strong

#plotting Safety Rating
ggplot(schools14_16aamath, aes( x = Safety.Rating )) + geom_bar(fill = "green")+
  ggtitle("Above average in math and Safety Rating")

#strong is most frequent, followed by neutral

#which of the five essentials is most frequently scored as strong or very strong for schools 
#who are above average in math?
schools14_16aamathE <- schools14_16aamath %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 1, 
                                          Environment.Rating == "STRONG" ~ 1, 
                                          Environment.Rating == "NEUTRAL" ~ 0, 
                                          Environment.Rating == "WEAK" ~ 0,
                                          Environment.Rating == "VERY WEAK" ~ 0, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aamathE <- schools14_16aamathE %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 1, 
                                          Instruction.Rating == "STRONG" ~ 1, 
                                          Instruction.Rating == "NEUTRAL" ~ 0, 
                                          Instruction.Rating == "WEAK" ~ 0,
                                          Instruction.Rating == "VERY WEAK" ~ 0, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16aamathE <- schools14_16aamathE %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 1, 
                                            Collaboration.Rating == "STRONG" ~ 1, 
                                            Collaboration.Rating == "NEUTRAL" ~ 0, 
                                            Collaboration.Rating == "WEAK" ~ 0,
                                            Collaboration.Rating == "VERY WEAK" ~ 0, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aamathE <- schools14_16aamathE %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 1, 
                                     Family.Rating == "STRONG" ~ 1, 
                                     Family.Rating == "NEUTRAL" ~ 0, 
                                     Family.Rating == "WEAK" ~ 0,
                                     Family.Rating == "VERY WEAK" ~ 0, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aamathE <- schools14_16aamathE %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 1,
                                         Leadership.Rating == "STRONG" ~ 1, 
                                         Leadership.Rating == "NEUTRAL" ~ 0, 
                                         Leadership.Rating == "WEAK" ~ 0,
                                         Leadership.Rating == "VERY WEAK" ~ 0, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

#sum up the new columns
colsums14_16aamathE <-  colSums(schools14_16aamathE[,21:25])

print(colsums14_16aamathE)

#turn the column sums into a talbe
colsums14_16aamathET <- as.table(colsums14_16aamathE)

#turn into a dataframe
colsums14_16aamathEdf <-  as.data.frame(colsums14_16aamathET)

#plot five essentials strength frequency
 ggplot(colsums14_16aamathEdf, aes(x = Var1, y = Freq)) + geom_point(col = "hotpink") + 
   ggtitle("Which of the 5 essentials is most commonly cited as strong/very strong for above average math?")

#family is most frequently strong/very strong, followed by Instruction
 
#income and above average in math
ggplot(schools14_16aamath, aes( x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct )) + geom_point () +
  geom_smooth() + ggtitle("Median Income and Math Percentiles for Schools above average in math")

#above poverty line and above average in math
ggplot(schools14_16aamath, aes( x = Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps, y = X2016.NWEA.Math.Gr.3.Pct )) + geom_point () +
  geom_smooth() + ggtitle("Did not Receive Food Stamps and Math Percentiles for Schools above average in math")

#map of above average schools in math
legendaamath <- tags$h5("Schools scoring over the 50th Percentile in math")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16aamath$Longitude, lat = schools14_16aamath$Latitude) %>%
  addControl(legendaamath, position = "bottomright")
             

 
#######################################################################################################
#looking at schools that were above national average for reading
schools14_16aareading <-  filter(schools14_16, X2016.NWEA.Reading.Gr.3.Pct >= 51)

#summary(schools1416aareading)
summary(schools14_16aareading)
#209 schools fit within this category

#plotting environment rating
ggplot(schools14_16aareading, aes( x = Environment.Rating )) + geom_bar(fill = "blue")+
  ggtitle("Environment rating and above average in reading")
#neutral is by far most frequent

#plotting Family rating
ggplot(schools14_16aareading, aes( x = Family.Rating )) + geom_bar(fill = "red") +
  ggtitle("Family rating and above average in reading")
#strong is most frequent followed by neutral

#plotting Instruction rating
ggplot(schools14_16aareading, aes( x = Instruction.Rating )) + geom_bar(fill = "pink") +
  ggtitle("Instruction rating and above average in reading")
#strong is most frequent

#plotting Leadership Rating
ggplot(schools14_16aareading, aes( x = Leadership.Rating )) + geom_bar(fill = "white") +
  ggtitle("Leadership rating and above average in reading")
#neutral is most frequent

#plotting Collaboration Rating
ggplot(schools14_16aareading, aes( x = Collaboration.Rating )) + geom_bar(fill = "green") +
  ggtitle("Collaboration rating and above average in reading")
#strong is most frequent

#plotting Safety Rating
ggplot(schools14_16aareading, aes( x = Safety.Rating )) + geom_bar(fill = "cyan") +
  ggtitle("Safety rating and above average in reading")

#neutral is most frequent, closely followed by strong

#which of the five essentials is most frequently associated with schools who are above average in Reading?
schools14_16aareadingE <- schools14_16aareading %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 1, 
                                          Environment.Rating == "STRONG" ~ 1, 
                                          Environment.Rating == "NEUTRAL" ~ 0, 
                                          Environment.Rating == "WEAK" ~ 0,
                                          Environment.Rating == "VERY WEAK" ~ 0, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aareadingE <- schools14_16aareadingE %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 1, 
                                          Instruction.Rating == "STRONG" ~ 1, 
                                          Instruction.Rating == "NEUTRAL" ~ 0, 
                                          Instruction.Rating == "WEAK" ~ 0,
                                          Instruction.Rating == "VERY WEAK" ~ 0, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16aareadingE <- schools14_16aareadingE %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 1,
                                            Collaboration.Rating == "STRONG" ~ 1, 
                                            Collaboration.Rating == "NEUTRAL" ~ 0, 
                                            Collaboration.Rating == "WEAK" ~ 0,
                                            Collaboration.Rating == "VERY WEAK" ~ 0, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aareadingE <- schools14_16aareadingE %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 1, 
                                     Family.Rating == "STRONG" ~ 1, 
                                     Family.Rating == "NEUTRAL" ~ 0, 
                                     Family.Rating == "WEAK" ~ 0,
                                     Family.Rating == "VERY WEAK" ~ 0, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16aareadingE <- schools14_16aareadingE %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 1, 
                                         Leadership.Rating == "STRONG" ~ 1, 
                                         Leadership.Rating == "NEUTRAL" ~ 0, 
                                         Leadership.Rating == "WEAK" ~ 0,
                                         Leadership.Rating == "VERY WEAK" ~ 0, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

#sum up the new columns
colsums14_16aareadingE <-  colSums(schools14_16aareadingE[,21:25])

print(colsums14_16aareadingE)

#turn the column sums into a talbe
colsums14_16aareadingEtable <- as.table(colsums14_16aareadingE)
#turn into a dataframe
colsums14_16aareadingEdf <-  as.data.frame(colsums14_16aareadingEtable)

#plot five essentials strength frequency
ggplot(colsums14_16aareadingEdf, aes(x = Var1, y = Freq)) + geom_point(col = "red") + 
   ggtitle("Which of the 5 essentials is most commonly cited as strong/very strong for schools ranked higher 
           than average for reading?")

#family is most frequent, followed by instruction

#income and above average in reading
ggplot(schools14_16aareading, aes( x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct )) + geom_point () +
  geom_smooth() + ggtitle("Median Income and Reading Percentiles for Schools above average in math")

#aggregate earnings and above average in reading
ggplot(schools14_16aareading, aes( x = Estimate.of.Aggregate.Earnings, y = X2016.NWEA.Reading.Gr.3.Pct )) +
  geom_point () + geom_smooth() + ggtitle("Aggregate Earnings and Reading Percentiles for Schools 
                                          above average in reading")

#create a map
legendaareading <- tags$h5("Schools scoring over the 50th Percentile in reading")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16aareading$Longitude, lat = schools14_16aareading$Latitude) %>% 
  addControl(legendaareading, position = "bottomright")


######################################################################################################

#looking at schools that are ranked in 75th percentile or higher in Reading
schools14_16_75 <-  filter(schools14_16, X2016.NWEA.Reading.Gr.3.Pct >= 75)

#create a map
legend75reading <-  tags$h5("Schools scoring in the 75th percentile or above in reading")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_75$Longitude, lat = schools14_16_75$Latitude) %>%
  addControl(legend75reading, position = "bottomright")

#seem to be more on northern side of the city

#looking at schools that are ranked in 75th percentile or higher in math
schools14_16_m75 <-  filter(schools14_16, X2016.NWEA.Math.Gr.3.Pct >= 75)

#create a map
legendm75 <-  tags$h5("Schools scoring in the 75th percentile or above in math")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_m75$Longitude, lat = schools14_16_m75$Latitude) %>% 
  addControl(legendm75, position = "bottomright")

#seems to be higher concentration on the north side

######################################################################################################
#looking at schoools that are ranked 25 and lower in Reading
schools14_16_r25 <-  filter(schools14_16, X2016.NWEA.Reading.Gr.3.Pct <= 25)


#create a map of schools scoring 25 or less in reading
legendr25 <- tags$h5("Schools scoring in the 25th percentile or lower in reading")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_r25$Longitude, lat = schools14_16_r25$Latitude)%>% 
  addControl(legendr25, position = "bottomright")

#seems to be a higher concentration on the southern side of the city

#income and 25 and below  in reading
ggplot(schools14_16_r25, aes( x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct )) + geom_point () +
  geom_smooth() + ggtitle("Median Income and Reading Percentiles for Schools below average in reading")

#incomes  seem to mostly fall within 20,000 - 40,000 range

#aggregate earnings and above average in reading
ggplot(schools14_16_r25, aes( x = Estimate.of.Aggregate.Earnings, y = X2016.NWEA.Reading.Gr.3.Pct )) + 
  geom_point () + geom_smooth() + ggtitle("Aggregate Earnings and Reading Percentiles for Schools
                                          below average in reading")

#not receiving food stamps and reading
ggplot(schools14_16_r25, aes( x = Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps, y = X2016.NWEA.Reading.Gr.3.Pct )) + 
  geom_point () + geom_smooth() + ggtitle("Did not Receive Food Stamps and Reading Percentiles for Schools
                                          below average in reading")
#not a very high level of correlation

###############################################################################################
#looking at schoools that are ranked 25 and lower in Math
schools14_16_m25 <-  filter(schools14_16, X2016.NWEA.Math.Gr.3.Pct <= 25)


#create a map of schools scoring 25 or less in reading
legendm25 <- tags$h5("Schools scoring in the 25th percentile or lower in math")

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_m25$Longitude, lat = schools14_16_m25$Latitude)%>% 
  addControl(legendm25, position = "bottomright")

#seems to be a # on the southern side of the city

#income and 25 and below  in math
ggplot(schools14_16_m25, aes( x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct )) + geom_point () +
  geom_smooth() + ggtitle("Median Income and Math Percentiles for Schools below average in math")
#incomes  seem to mostly fall within 20,000 - 40,000 range

#aggregate earnings and below average in math
ggplot(schools14_16_m25, aes( x = Estimate.of.Aggregate.Earnings, y = X2016.NWEA.Math.Gr.3.Pct )) + 
  geom_point () + geom_smooth() + ggtitle("Aggregate Earnings and Math Percentiles for Schools
                                          below average in math")

#Not receiving food stamps and math
ggplot(schools14_16_m25, aes( x = Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps, y = X2016.NWEA.Math.Gr.3.Pct )) + 
  geom_point () + geom_smooth() + ggtitle("Did not Receive Food Stamps and Math Percentiles for Schools
                                          below average in math")

