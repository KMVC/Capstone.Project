#some EDA on entire data set

#load the data and relevant libraries
schools14_16 <- read.csv("schools14_16.csv", na = c("", "NA"))
library(tidyverse)
library(corrplot)


#take a look at summary of the dataset to get means of high school graduation/median income/math and reading percentile means
summary(schools14_16)    

################################################################################################
#Plots and Analysis of 2016 Reading Percentiles (drawing on correlations done in data wrangling)

#plotting % of pop graduated from high school and reading percentiles
ggplot(schools14_16, aes(x = H.S.Graduation.., y = X2016.NWEA.Reading.Gr.3.Pct)) + 
  geom_point()+ geom_smooth() + ggtitle("Reading Percentile and Neighbhourhood Rates
                                        of High School Graduation")

#income and grade 3 reading
ggplot(schools14_16, aes(x = Median.Income, y = X2016.NWEA.Reading.Gr.3.Pct)) + 
  geom_point() +geom_smooth() + ggtitle("Reading Percentile and Income by Neighbourhood")

#Aggregated earnings and grade 3 reading
ggplot(schools14_16, aes(x = Estimate.of.Aggregate.Earnings,
                         y = X2016.NWEA.Reading.Gr.3.Pct)) + 
                        geom_point()+ geom_smooth() + ggtitle("Reading Percentile and Estimate of Aggregated Earnings 
                                                                    in Zip Code")

#those who did not receive food stamps and grade 3 reading
ggplot(schools14_16, aes(x = Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps, y = X2016.NWEA.Reading.Gr.3.Pct)) + 
  geom_point() +geom_smooth() + ggtitle("Reading Percentile and # of Those Who Did Not Receive
                                        Food Stamps in School Zip Code")



#The 5Essentials and Grade 3 Reading(2016)
#ambitious instruction and grade 3 reading
ggplot(schools14_16, aes(x = Instruction.Rating, y = X2016.NWEA.Reading.Gr.3.Pct))+ geom_point() +
  ggtitle("Reading Percentiles and Ambitious Instruction")

#supportive environment and grade 3 reading
ggplot(schools14_16, aes(x = Environment.Rating, y = X2016.NWEA.Reading.Gr.3.Pct))+ geom_point() + 
  ggtitle("Reading Percentiles and Supportive Environment")

#collaborative teachers and grade 3 reading
ggplot(schools14_16, aes(x = Collaboration.Rating, y = X2016.NWEA.Reading.Gr.3.Pct)) + geom_point() +
  ggtitle("Reading Percentiles and Collaborative Teachers")

#involved families and grade 3 reading
ggplot(schools14_16, aes(x = Family.Rating, y = X2016.NWEA.Reading.Gr.3.Pct))+ geom_point() +
  ggtitle("Reading Percentiles and Involved Families")

#effective leaders and grade 3 reading
ggplot(schools14_16, aes(x = Leadership.Rating, y = X2016.NWEA.Reading.Gr.3.Pct))+ geom_point() + 
  ggtitle("Reading Percentiles and Effective Leaders")

#not a lot can be gleaned from the above graphs; visually weak/very weak Family Rating and
#Ambitious Instruction seem to have some (weak) correlation with ranking within lower reading percentiles



#Comparing percentiles from 2014/2015 for reading
#2014 reading and 2016 grade 3 reading
ggplot(schools14_16, aes(x = X2014.NWEA.Reading.Gr.3.Pct , y = X2016.NWEA.Reading.Gr.3.Pct))+ 
  geom_point() + geom_smooth() + ggtitle("2014 Reading Percentiles and 2016 Reading percentiles")

#2015 reading and 2016 grade 3 reading
ggplot(schools14_16, aes(x = X2015.NWEA.Reading.Gr.3.Pct, y = X2016.NWEA.Reading.Gr.3.Pct))+
  geom_point() + geom_smooth() + ggtitle("2015 Reading Percentile and 2016 Reading Percentiles")

#the above graphs seem to show the strongest correlation - the percentiles achieved in 2014/2015 reading 
#seem to highly correlate with the percentiles achieved in 2016



###############################################################################################
#Plots and Analysis of 2016 Math Percentiles (drawing on correlations done in data wrangling)

#education by area and math
ggplot(schools14_16, aes(x = H.S.Graduation.., y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + geom_smooth() + 
  ggtitle("High School Graduation Rates by Neighbourhood and Math Percentiles")

#income and grade 3 math
ggplot(schools14_16, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct )) + 
  geom_point() + geom_smooth() + ggtitle("Median Income and Math Percentiles")

#Aggregated earnings and grade 3 math
ggplot(schools14_16, aes(x = Estimate.of.Aggregate.Earnings,
                         y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point()+ geom_smooth() + ggtitle("Math Percentile and Estimate of Aggregated Earnings 
                                        in Zip Code")

#those who did not receive food stamps and grade 3 math
ggplot(schools14_16, aes(x = Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps, 
                         y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() +geom_smooth() + ggtitle("Math Percentile and # of Those Who Did Not Receive
                                        Food Stamps in School Zip Code")



#5Essentials and Grade 3 Math (2016)

#ambitious instruction and third grade math
ggplot(schools14_16, aes(x = Instruction.Rating, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + ggtitle("Math Percentile and Ambitious Instruction")

#supportive environment and third grade math
ggplot(schools14_16, aes(x = Environment.Rating, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() +ggtitle("Math Percentile and Supportive Environment")

#involved families and third grade math
ggplot(schools14_16, aes(x = Family.Rating, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + ggtitle("Math Percentile and Involved Families")

#collaborative teachers and third grade math
ggplot(schools14_16, aes(x = Collaboration.Rating, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() +ggtitle("Math Percentile and Collaborative Teachers")

#effective leaders and third grade math
ggplot(schools14_16, aes(x = Leadership.Rating, y = X2016.NWEA.Math.Gr.3.Pct))+ geom_point() +
  ggtitle("Math Percentile and Effective Leaders")

#as with the 5essential/reading percentile charts, the charts above do not provide much 
#information. The Family Rating once again, seems to have some effect/correlation with the very weak schools' 
#lower percentile rankings



##Comparing percentiles from 2014/2015 for Math
#2014 math and 2016 grade 3 
ggplot(schools14_16, aes(x = X2014.NWEA.Math.Gr.3.Pct, y = X2016.NWEA.Math.Gr.3.Pct))+ 
  geom_point() + geom_smooth() + ggtitle("2014 Math Percentile and 2016 Math Percentile")

#2015 math and 2016 grade math 
ggplot(schools14_16, aes(x = X2015.NWEA.Math.Gr.3.Pct, y = X2016.NWEA.Math.Gr.3.Pct)) + 
  geom_point() + geom_smooth() + ggtitle("2015 Math Percentile and 2016 Math Percentile")

#as with reading, 2014/2015 percentile rankings in math seem to be, of all available variables, the most
#highly correlated with 2016 math percentiles

###################################################################################################
#using the information from the above charts, trying to put more variables 
#together to see possible correlations (using highest correlated variables)

#income, involved families and reading percentiles
ggplot(schools14_16, aes(x = Median.Income, y =  X2016.NWEA.Reading.Gr.3.Pct, col = Family.Rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("Median Income, Involved Families and Reading Percentiles")

#can see from this graph (above) that very strong/strong family category also contain highest incomes in data

#2014 percentiles, Family Rating and 2016 Reading percentiles
ggplot(schools14_16, aes(x = X2014.NWEA.Reading.Gr.3.Pct, y =  X2016.NWEA.Reading.Gr.3.Pct, col = Family.Rating)) + 
  geom_point() + geom_smooth() + 
  ggtitle("2014 Reading percentiles, Involved Families and 2016 Reading Percentiles")

#the schools with very weak family rating (in graph above) seem to show lack of improvement in percentile from 2014 to
#2016


#income, involved families and math scores
ggplot(schools14_16, aes(x = Median.Income, y = X2016.NWEA.Math.Gr.3.Pct, col = Family.Rating)) + 
  geom_point() + geom_smooth() + ggtitle("Median Income, Involved Families and Math Percentiles")

#as with the reading/income/family graph, this graph seems to show that the strong/very strong
#schools (for Involved Families) include the highest income areas in the data set

#2014 percentiles, Involved Families and 2016 Math Percentiles
ggplot(schools14_16, aes(x = X2014.NWEA.Math.Gr.3.Pct, y = X2016.NWEA.Math.Gr.3.Pct, col = Family.Rating)) + 
  geom_point() + geom_smooth() +
  ggtitle("2014 Math Percentiles, Involved Families and 2016 Math Percentiles")

#unlike with the 2014 reading/2016 reading/family chart, this graph seems to indicate a wider range 
#of percentile outcomes for schools rated very weak


########################################################################################################

#for the schools in the data set, which five essential is most often rated strong or very strong?
#which is least frequently given this ranking?
#create columns that give each categorical variable a binary score (0/1)
schools14_16a <- schools14_16 %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 1, 
                                          Environment.Rating == "STRONG" ~ 1, 
                                          Environment.Rating == "NEUTRAL" ~ 0, 
                                          Environment.Rating == "WEAK" ~ 0,
                                          Environment.Rating == "VERY WEAK" ~ 0, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16a <- schools14_16a %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 1, 
                                          Instruction.Rating == "STRONG" ~ 1, 
                                          Instruction.Rating == "NEUTRAL" ~ 0, 
                                          Instruction.Rating == "WEAK" ~ 0,
                                          Instruction.Rating == "VERY WEAK" ~ 0, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16a <- schools14_16a %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 1, 
                                            Collaboration.Rating == "STRONG" ~ 1, 
                                            Collaboration.Rating == "NEUTRAL" ~ 0, 
                                            Collaboration.Rating == "WEAK" ~ 0,
                                            Collaboration.Rating == "VERY WEAK" ~ 0, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16a <- schools14_16a %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 1, 
                                     Family.Rating == "STRONG" ~ 1, 
                                     Family.Rating == "NEUTRAL" ~ 0, 
                                     Family.Rating == "WEAK" ~ 0,
                                     Family.Rating == "VERY WEAK" ~ 0, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16a <- schools14_16a %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 1,
                                         Leadership.Rating == "STRONG" ~ 1, 
                                         Leadership.Rating == "NEUTRAL" ~ 0,
                                         Leadership.Rating == "WEAK" ~ 0,
                                         Leadership.Rating == "VERY WEAK" ~ 0, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

#sum up the values in the new columns
colsums14_16a <-  colSums(schools14_16a[,21:25])

#turn the column sums into a table
colsums14_16atable <- as.table(colsums14_16a)

#turn into a dataframe
colsums14_16adf <-  as.data.frame(colsums14_16atable)

#fixing column names
colnames(colsums14_16adf)[colnames(colsums14_16adf)=="Var1"] <- "5 Essentials"
colnames(colsums14_16adf)[colnames(colsums14_16adf)=="Freq"] <- "Frequency"

#plot five essentials strength frequency
ggplot(colsums14_16adf, aes(x = `5 Essentials`, y = Frequency)) + 
  geom_point(size = 2, color ="firebrick") +
  ggtitle("Frequency of a 5 Essential Rated Strong or Very Strong")

##can see from the graph above that the # of times a 5 essential is rated strong/very strong
#in the data is:
#Instruction Scores: 203
#Family Scores: 184
#Environment Scores: 162
#Collaboration Scores: 157
#Leadership Scores: 146

#for the schools in the data set, which five essential is most often rated weak or very weak?
#which is least frequently given these rankings?
#create columns that give each categorical variable a binary score (0/1)
schools14_16b <- schools14_16 %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 0, 
                                          Environment.Rating == "STRONG" ~ 0, 
                                          Environment.Rating == "NEUTRAL" ~ 0,
                                          Environment.Rating == "WEAK" ~ 1,
                                          Environment.Rating == "VERY WEAK" ~ 1, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16b <- schools14_16b %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 0, 
                                          Instruction.Rating == "STRONG" ~ 0, 
                                          Instruction.Rating == "NEUTRAL" ~ 0, 
                                          Instruction.Rating == "WEAK" ~ 1,
                                          Instruction.Rating == "VERY WEAK" ~ 1, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16b <- schools14_16b %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 0, 
                                            Collaboration.Rating == "STRONG" ~ 0, 
                                            Collaboration.Rating == "NEUTRAL" ~ 0, 
                                            Collaboration.Rating == "WEAK" ~ 1,
                                            Collaboration.Rating == "VERY WEAK" ~ 1, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16b <- schools14_16b %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 0, 
                                     Family.Rating == "STRONG" ~ 0, 
                                     Family.Rating == "NEUTRAL" ~ 0, 
                                     Family.Rating == "WEAK" ~ 1,
                                     Family.Rating == "VERY WEAK" ~ 1, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16b <- schools14_16b %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 0, 
                                         Leadership.Rating == "STRONG" ~ 0, 
                                         Leadership.Rating == "NEUTRAL" ~ 0, 
                                         Leadership.Rating == "WEAK" ~ 1,
                                         Leadership.Rating == "VERY WEAK" ~ 1, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

#sum up the values in the new columns
colsums14_16b <-  colSums(schools14_16b[,21:25])

#turn the column sums into a table
colsums14_16btable <- as.table(colsums14_16b)

#turn into a dataframe
colsums14_16bdf <-  as.data.frame(colsums14_16btable)

#fixing column names
colnames(colsums14_16bdf)[colnames(colsums14_16bdf)=="Var1"] <- "5 Essentials"
colnames(colsums14_16bdf)[colnames(colsums14_16bdf)=="Freq"] <- "Frequency"

#plot five essentials strength frequency
ggplot(colsums14_16bdf, aes(x = `5 Essentials`, y = Frequency)) + 
  geom_point(size = 2, color ="black") +
  ggtitle("Frequency of a 5 Essential Rated Very Weak or Weak")

#How frequently is a 5 essential rated weak/very weak in the data set?
#Collaborative Rating: 139
#Leadership Rating: 131
#Family Rating: 104
#Environment Rating: 69
#Instruction Rating: 58

###########################################################################################################################################
#creating a correlation matrix, and plots


#converting categorical variables to numerical
schools14_16cor <- schools14_16 %>% 
  mutate("environment scores" = case_when(Environment.Rating == "VERY STRONG" ~ 5, 
                                          Environment.Rating == "STRONG" ~ 4, 
                                          Environment.Rating == "NEUTRAL" ~ 3,
                                          Environment.Rating == "WEAK" ~ 2,
                                          Environment.Rating == "VERY WEAK" ~ 1, 
                                          Environment.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16cor <- schools14_16cor %>% 
  mutate("instruction scores" = case_when(Instruction.Rating == "VERY STRONG" ~ 5, 
                                          Instruction.Rating == "STRONG" ~ 4, 
                                          Instruction.Rating == "NEUTRAL" ~ 3, 
                                          Instruction.Rating == "WEAK" ~ 2,
                                          Instruction.Rating == "VERY WEAK" ~ 1, 
                                          Instruction.Rating == "NOT ENOUGH DATA" ~ 0))


schools14_16cor <- schools14_16cor %>% 
  mutate("collaboration scores" = case_when(Collaboration.Rating == "VERY STRONG" ~ 5, 
                                            Collaboration.Rating == "STRONG" ~ 4, 
                                            Collaboration.Rating == "NEUTRAL" ~ 3, 
                                            Collaboration.Rating == "WEAK" ~ 2,
                                            Collaboration.Rating == "VERY WEAK" ~ 1, 
                                            Collaboration.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16cor <- schools14_16cor %>% 
  mutate("family scores" = case_when(Family.Rating == "VERY STRONG" ~ 5, 
                                     Family.Rating == "STRONG" ~ 4, 
                                     Family.Rating == "NEUTRAL" ~ 3, 
                                     Family.Rating == "WEAK" ~ 2,
                                     Family.Rating == "VERY WEAK" ~ 1, 
                                     Family.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16cor <- schools14_16cor %>% 
  mutate("leadership scores" = case_when(Leadership.Rating == "VERY STRONG" ~ 5, 
                                         Leadership.Rating == "STRONG" ~ 4, 
                                         Leadership.Rating == "NEUTRAL" ~ 3, 
                                         Leadership.Rating == "WEAK" ~ 2,
                                         Leadership.Rating == "VERY WEAK" ~ 1, 
                                         Leadership.Rating == "NOT ENOUGH DATA" ~ 0))

schools14_16cor <- schools14_16cor %>% 
  mutate("safety scores" = case_when(Safety.Rating == "VERY STRONG" ~ 5, 
                                     Safety.Rating == "STRONG" ~ 4, 
                                     Safety.Rating == "NEUTRAL" ~ 3, 
                                     Safety.Rating == "WEAK" ~ 2,
                                     Safety.Rating == "VERY WEAK" ~ 1, 
                                     Safety.Rating == "NOT ENOUGH DATA" ~ 0))


#remove latitude, longitude, name of school and categorical values
schools14_16cor <-  schools14_16cor[,(-(1:8))]
schools14_16cor <-  schools14_16cor[,(-(11:12))]


#removing na values
schools14_16cor <- na.omit(schools14_16cor)

#alter labels so they are not too long
colnames(schools14_16cor)[colnames(schools14_16cor)=="environment scores"] <- "envi"

colnames(schools14_16cor)[colnames(schools14_16cor)=="instruction scores"] <- "inst"

colnames(schools14_16cor)[colnames(schools14_16cor)=="leadership scores"] <- "lead"

colnames(schools14_16cor)[colnames(schools14_16cor)=="collaboration scores"] <- "collab"

colnames(schools14_16cor)[colnames(schools14_16cor)=="safety scores"] <- "safe"

colnames(schools14_16cor)[colnames(schools14_16cor)=="family scores"] <- "fam"

colnames(schools14_16cor)[colnames(schools14_16cor)=="Median.Income"] <- "income"

colnames(schools14_16cor)[colnames(schools14_16cor)=="H.S.Graduation.."] <- "grad"

colnames(schools14_16cor)[colnames(schools14_16cor)=="X2014.NWEA.Reading.Gr.3.Pct"] <- "2014 reading"

colnames(schools14_16cor)[colnames(schools14_16cor)=="X2015.NWEA.Reading.Gr.3.Pct"] <- "2015 reading "

colnames(schools14_16cor)[colnames(schools14_16cor)=="X2016.NWEA.Reading.Gr.3.Pct"] <- "2016 reading"

colnames(schools14_16cor)[colnames(schools14_16cor)== "X2014.NWEA.Math.Gr.3.Pct"] <- "2014 math"

colnames(schools14_16cor)[colnames(schools14_16cor)=="X2015.NWEA.Math.Gr.3.Pct"] <- "2015 math"

colnames(schools14_16cor)[colnames(schools14_16cor)=="X2016.NWEA.Math.Gr.3.Pct"] <- "2016 Math"

colnames(schools14_16cor)[colnames(schools14_16cor)=="Estimate.of.Pop..At.or.Above.Poverty.Line.who.Did.Not.Receive.Food.Stamps"] <- "no stamps"

colnames(schools14_16cor)[colnames(schools14_16cor)=="Estimate.of.Aggregate.Earnings"] <- "earn"



#creating correlation matrix
corschools <- cor(schools14_16cor)

#create correlation chart with colour
corrplot(corschools)

#create correlation chart with numbers
corrplot(corschools, method = "number")

#can see that:
#for outcome variables:
#reading and 2016: 
#most correlated are: 
#income, 2014 and 2015 math and reading scores, and to a lesser extent, safety and then family
#income and earn are highly correlated
# earn and no stamps are highly correlated
#envi and instr are correlated, as is envi and safe
#collab is correlated with fam, collab is also correlated with leadership
#fam is corrleated with leadership
#safety is correlated with instruction

