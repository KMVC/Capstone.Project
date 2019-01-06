#subsetting the data by the 5 essentials



#looking at schools that have the same score on all 5 essentials

schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(leaflet)

########################################################################################################

##Very Strong Schools
#how many schools are very strong in every 5 essential category
schools14_16_vs <-  filter(schools14_16, Family.Rating == "VERY STRONG"
                         & Environment.Rating == "VERY STRONG"
                         & Instruction.Rating == "VERY STRONG"
                         & Leadership.Rating == "VERY STRONG"
                         & Collaboration.Rating == "VERY STRONG")

summary(schools14_16_vs)

#only 3 schools

#Find differences between individual school's math percentiles, 2014 and 2016
schools14_16_vs$mathdifference <-  schools14_16_vs$X2016.NWEA.Math.Gr.3.Pct - schools14_16_vs$X2014.NWEA.Math.Gr.3.Pct
print(schools14_16_vs$mathdifference)

#one school had a major improvement (45 percentiles), other 2 schools dropped (-15, -16)

#plot math percentile differences between 2014 to 2016
ggplot(schools14_16_vs, aes(x = Name.of.School, y = mathdifference)) + geom_point() + 
  ggtitle("Difference in Math Percentiles for Very Strong Schools 2014 to 2016")

#Find differences between individual schools' reading percentiles, 2014 and 2016
schools14_16_vs$readdifference <-  schools14_16_vs$X2016.NWEA.Reading.Gr.3.Pct - schools14_16_vs$X2014.NWEA.Reading.Gr.3.Pct
print(schools14_16_vs$readdifference)

#one school had huge improvement (85 percentiles), other two also improved more modestly (9 percentiles, 23 percentiles)

#plot reading percentile differences between 2014 to 2016
ggplot(schools14_16_vs, aes(x = Name.of.School, y = readdifference)) + 
  geom_point() + ggtitle("Difference in Reading Percentiles for Very Strong Schools, from 2014 to 2016 ")

#plot of reading percentiles and safety
ggplot(schools14_16_vs, aes(x = Name.of.School, y = X2016.NWEA.Reading.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Very Strong Schools Reading Percentile and Safety")

#plot of math percentiles and safety 
ggplot(schools14_16_vs, aes(x = Name.of.School, y = X2016.NWEA.Math.Gr.3.Pct, col = Safety.Rating)) + 
  geom_point() + ggtitle("Very Stong Schools Math Percentile and Safety")

#income and very strong schools, math percentiles
ggplot(schools14_16_vs, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + ggtitle("Very Stong Schools Math Percentile and Median Income")

#income, very strong schools and reading percentiles
ggplot(schools14_16_vs, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct)) + 
  geom_point() + ggtitle("Very Stong Schools Reading Percentile and Median Income")


#create a map of very strong schools to showcase location
pal <- colorFactor(
  palette = 'Set1',
  domain = schools14_16_vs$Name.of.School)

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_vs$Longitude, lat = schools14_16_vs$Latitude,
             label = schools14_16_vs$Name.of.School)%>% addLegend("bottomright", pal = pal, 
                                                                values = schools14_16_vs$Name.of.School,
                                                                title = "Very Strong Schools", 
                                                                opacity = 1)

#######################################################################################################

#schools that are either very strong or strong in all categories
schools14_16_allstrong <-  filter(schools14_16, Family.Rating == "VERY STRONG"
                             & Environment.Rating == "VERY STRONG"
                             & Instruction.Rating == "VERY STRONG"
                             & Leadership.Rating == "VERY STRONG" 
                             & Collaboration.Rating == "VERY STRONG" |
                               Family.Rating == "STRONG"
                             & Environment.Rating == "STRONG"
                             & Instruction.Rating == "STRONG"
                             & Leadership.Rating == "STRONG" 
                             & Collaboration.Rating == "STRONG")

summary(schools14_16_allstrong)
#10 schools fit within these parameters

#Find differences between individual schools' math percentiles, 2014 and 2016
schools14_16_allstrong$mathdifference <-  schools14_16_allstrong$X2016.NWEA.Math.Gr.3.Pct - schools14_16_allstrong$X2014.NWEA.Math.Gr.3.Pct
print(schools14_16_allstrong$mathdifference)

#plot math percentile differences
ggplot(schools14_16_allstrong, aes(x = Name.of.School, y = mathdifference)) + geom_point() +
  ggtitle("Difference in Math Percentiles 2014-2016, Very Strong/Strong Schools")+
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#can see that 5 schools got worse, 1 school stayed roughly the same and four schools improved

#Find differences between individual schools reading percentiles in 2016 and 2014
schools14_16_allstrong$readdifference <-  schools14_16_allstrong$X2016.NWEA.Reading.Gr.3.Pct - schools14_16_allstrong$X2014.NWEA.Reading.Gr.3.Pct
print(schools14_16_allstrong$readdifference)

#plot reading percentile differences
ggplot(schools14_16_allstrong, aes(x = Name.of.School, y = readdifference)) + geom_point() + 
  ggtitle("Difference in Reading Percentiles 2014-2016, Very Strong/Strong Schools") + 
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#3 schools got a lower percentile in 2016 than they did in 2014, while 7 improved their percentile

#plot of math scores with safety rating
ggplot(schools14_16_allstrong, aes(x = Name.of.School, y = X2016.NWEA.Math.Gr.3.Pct, color = Safety.Rating )) + 
  geom_point() + ggtitle("Math Percentiles and Safety, Very strong/Strong Schools") + 
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#plot of reading sores with safety rating
ggplot(schools14_16_allstrong, aes(x = Name.of.School, y = X2016.NWEA.Reading.Gr.3.Pct, color = Safety.Rating )) + 
  geom_point() + ggtitle("Reading Percentiles and safety, Very strong/Strong Schools")+
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#income and all strong schools, reading percentiles
ggplot(schools14_16_allstrong, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct)) + 
  geom_point() + ggtitle("Income and Reading Percentiles, Very strong/Strong Schools")

#income and all strong schools, math percentiles
ggplot(schools14_16_allstrong, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + ggtitle("Income and Math Percentiles, Very strong/Strong Schools")


#create a map of strong/very schools to showcase location 
pal <- colorFactor(
  palette = 'Set1',
  domain = schools14_16_allstrong$Name.of.School)

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_allstrong$Longitude, lat = schools14_16_allstrong$Latitude,
             label = schools14_16_allstrong$Name.of.School)%>% addLegend("bottomright", pal = pal, 
                                                                         values = schools14_16_allstrong$Name.of.School,
                                                                          title = "Very Strong/Strong Schools", 
                                                                          opacity = 1)

########################################################################################################
#schools that are strong in all categories
schools14_16_strong <-  filter(schools14_16, Family.Rating == "STRONG"
                                & Environment.Rating == "STRONG"
                                & Instruction.Rating == "STRONG"
                                & Leadership.Rating == "STRONG" 
                                & Collaboration.Rating == "STRONG")

summary(schools14_16_strong)
#7 schools fit within these parameters

#Find differences between individual school's math percentiles, 2014 and 2016
schools14_16_strong$mathdifference <-  schools14_16_strong$X2016.NWEA.Math.Gr.3.Pct - schools14_16_strong$X2014.NWEA.Math.Gr.3.Pct
print(schools14_16_strong$mathdifference)

#Find differences between individual school's reading percentiles, 2014 and 2016
schools14_16_strong$readdifference <-  schools14_16_strong$X2016.NWEA.Reading.Gr.3.Pct - schools14_16_strong$X2014.NWEA.Reading.Gr.3.Pct
print(schools14_16_strong$readdifference)

#plot math diff between 2014 and 2016 
ggplot(schools14_16_strong, aes(x = Name.of.School , y = mathdifference))  + 
  geom_point() + ggtitle("Difference in Math Percentiles 2014-2016, Strong Schools") +
  theme(axis.text.x = element_text(angle = 90, hjust=0))
  
  #can see that 4 got a lower percentile in 2016 than in 2014, while 3 improved

#plot reading diff between 2014 and 2016 
ggplot(schools14_16_strong, aes(x = Name.of.School , y = readdifference))  + 
  geom_point() + ggtitle("Difference in Reading Percentiles 2014-2016, Strong Schools") +
  theme(axis.text.x = element_text(angle = 90, hjust=0))
  
#can see that 3 schools got lower percentiles in 2016 than in 2014, while 4 improved

#plot of math percentiles with income, and safety
ggplot(schools14_16_strong, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Math Percentiles, Income and Safety, Strong Schools") 

#plot of reading scores and safety
ggplot(schools14_16_strong, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Reading Percentiles, Income and Safety, Strong Schools")

#create a map of  strong schools to showcase location 
pal <- colorFactor(
  palette = 'Set1',
  domain = schools14_16_strong$Name.of.School)

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_strong$Longitude, lat = schools14_16_strong$Latitude,
             label = schools14_16_strong$Name.of.School)%>% addLegend("bottomright", pal = pal, values = schools14_16_strong$Name.of.School,
                                                                       title = "Strong Schools", opacity = 1)


##########################################################################################################

#schools that are Neutral in all categories
schools14_16_neutral <-  filter(schools14_16, Family.Rating == "NEUTRAL"
                              & Environment.Rating == "NEUTRAL"
                              & Instruction.Rating == "NEUTRAL"
                              & Leadership.Rating == "NEUTRAL" 
                              & Collaboration.Rating == "NEUTRAL")

summary(schools14_16_neutral)
#14 schools fit within these parameters

#Find differences between individual schools' math percentiles, 2014 and 2016 
schools14_16_neutral$mathdifference <-  schools14_16_neutral$X2016.NWEA.Math.Gr.3.Pct - schools14_16_neutral$X2014.NWEA.Math.Gr.3.Pct
print(schools14_16_neutral$mathdifference)

#Find differences between individual schools' reading percentiles, 2014 and 2016 
schools14_16_neutral$readdifference <-  schools14_16_neutral$X2016.NWEA.Reading.Gr.3.Pct - schools14_16_neutral$X2014.NWEA.Reading.Gr.3.Pct
print(schools14_16_neutral$readdifference)

#math diff between 2014 and 2016 
ggplot(schools14_16_neutral, aes(x = Name.of.School , y = mathdifference))  + 
  geom_point() + ggtitle("Difference in Math Percentiles, 2014-2016, Neutral Schools")+
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#4 schools got lower percentiles in 2016 than 2014, while 10 improve

#reading diff between 2014 and 2016  
ggplot(schools14_16_neutral, aes(x = Name.of.School , y = readdifference))  + geom_point() + 
  ggtitle("Difference in Reading Percentiles, 2014-2016, Neutral Schools")+
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#3 schools got lower percentiless in 2016 than 2014, 11 improved

#plot of income, math percentiles with safety
ggplot(schools14_16_neutral, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Math Percentiles, Income and Safety, Neutral Schools")

#incomes in this category go lower than in previous subsetted categories(very strong, all strong, and strong)
  
#plot of reading scores, income with safety
ggplot(schools14_16_neutral, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Reading Percentiles, Income and Safety, Neutral Schools")

#create a map of neutral schools to showcase location
pal <- colorFactor(
  palette = 'Set1',
  domain = schools14_16_neutral$Name.of.School)

leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_neutral$Longitude, lat = schools14_16_neutral$Latitude,
             label = schools14_16_neutral$Name.of.School)%>% addLegend("bottomright", pal = pal, values = schools14_16_neutral$Name.of.School,
                                                                     title = "Neutral Schools", opacity = 1)

#########################################################################################################

#schools that are weak in all categories
schools14_16_weak <-  filter(schools14_16, Family.Rating == "WEAK"
                           & Environment.Rating == "WEAK"
                           & Instruction.Rating == "WEAK"
                           & Leadership.Rating == "WEAK" 
                           & Collaboration.Rating == "WEAK")
summary(schools14_16_weak)
#5 schools in this category

#Find differences between individual schools' percentiles,  math, 2014 and 2016
schools14_16_weak$mathdifference <-  schools14_16_weak$X2016.NWEA.Math.Gr.3.Pct - schools14_16_weak$X2014.NWEA.Math.Gr.3.Pct
print(schools14_16_weak$mathdifference)

#Find differences between individual schools' percentiles, reading, 2014 and 2016
schools14_16_weak$readdifference <-  schools14_16_weak$X2016.NWEA.Reading.Gr.3.Pct - schools14_16_weak$X2014.NWEA.Reading.Gr.3.Pct
print(schools14_16_weak$readdifference)

#math diff between 2014 and 2016
ggplot(schools14_16_weak, aes(x = Name.of.School, y = mathdifference))  + 
  geom_point() + ggtitle("Difference in Math Percentile, 2014-2016, Weak Schools")+
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#can see that two schools got lower rankings, while 3 improved

#reading diff between 2014 and 2016 
ggplot(schools14_16_weak, aes(x = Name.of.School , y = readdifference))  + 
  geom_point() + ggtitle("Difference in Reading Percentiles, 2014-2016, Weak Schools") +
  theme(axis.text.x = element_text(angle = 90, hjust=0))

#2 schools got lowered percentile ranks while 3 improved on their 2014 ranking


#plot of math percentiles, income with safety

ggplot(schools14_16_weak, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Math Percentile, Income and Safety, Weak Schools")

#lower income brackets for this category, highest in this category is around 38,000, and no 
#stregth in safety

#plot of reading percentiles, income with safety
ggplot(schools14_16_weak, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct, col = Safety.Rating )) + 
  geom_point() + ggtitle("Reading Percentile, Income and Safety, Weak Schools")


#create a map of weak schools to showcase location
pal <- colorFactor(
  palette = 'Set1',
  domain = schools14_16_weak$Name.of.School)


leaflet() %>% addTiles() %>% 
  addMarkers(lng = schools14_16_weak$Longitude, lat = schools14_16_weak$Latitude,
             label = schools14_16_weak$Name.of.School) %>%  addLegend("bottomright", pal = pal, values = schools14_16_weak$Name.of.School,
                                                                    title = "Weak Schools", opacity = 1)

#schools located mostly in the south part of the city

########################################################################################################

#schools that are very weak in all categories
schools14_16_vweak <-  filter(schools14_16, Family.Rating == "VERY WEAK"
                            & Environment.Rating == "VERY WEAK"
                            & Instruction.Rating == "VERY WEAK"
                            & Leadership.Rating == "VERY WEAK" 
                            & Collaboration.Rating == "VERY WEAK")

#0 schools scored Very Weak in all five essentials