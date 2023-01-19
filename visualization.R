library(tidyverse)
library(dplyr)
library(scales)
getwd()
setwd("D:/")
setwd('DATA')

#setting currency
euro <- dollar_format(prefix = "\u20ac", big.mark = ",")

#importing cleaned population data frame
Towns = read_csv("cleanedData/PopulationCleaned.csv", show_col_types = FALSE) %>% 
  select(ShortPostCode, town, district, county)

#importing cleaned house price data frame
HousePrices=read_csv("cleanedData/cleanedHouseprices.csv", show_col_types = FALSE)

#Joining house price and population data frame
HousePricesclean <- HousePrices %>% 
  left_join(Towns, by ="ShortPostCode")  %>% 
  na.omit()


#BARGRAPH average house price by district (2021)
HousePricesclean %>%
  filter(date == 2021) %>%
  group_by(district,county) %>%
  summarise(AveragePrice = mean(price)) %>%
  ggplot(aes(x = district, y = AveragePrice, fill = county)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(breaks = seq(0, 5000000, 30000),
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)),
            vjust = -0.25) +
  labs(title = "2021 Average house prices with respect to district") +
  coord_flip()


#BOXPLOT Average house prices by district (2019-2022)
HousePricesclean %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = price, fill=county)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2022 house prices by district")


#LINEGRAPH Average house prices by year (2019-2022) combined
HousePricesclean %>% 
  group_by(date) %>% 
  summarise(AveragePrice = mean(price)) %>% 
  
  ggplot(aes(x = date, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2022) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2022 Average house prices by year")



#importing cleaned broadband data frame
BroadbandSpeedsclean=read_csv("cleanedData/CleanBroadband.csv")


#Joining broadband and population data frame
broadband=Towns %>% 
  left_join(BroadbandSpeedsclean,by="ShortPostCode") %>% 
  na.omit()


#Bar chart of LANCASHIRE broadband Speed
ggplot(broadband,aes(y=town)) +
  labs(x="Speeds (Mbits/s)",y="Towns",title="LANCASHIRE Broadband Speeds")+
  geom_bar(data=filter(broadband,county=="LANCASHIRE"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadband,county=="LANCASHIRE"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))


#Bar chart of LEICESTERSHIRE broadband Speed
ggplot(broadband,aes(y=town)) +
  labs(x="Speeds (Mbits/s)",y="Towns",title="LEICESTERSHIRE Broadband Speeds")+
  geom_bar(data=filter(broadband,county=="LEICESTERSHIRE"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadband,county=="LEICESTERSHIRE"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))



#BOXPLOT Average download speed
broadband %>% 
  group_by(district) %>% 
  ggplot(aes(x = district, y = `AverageDownload`, fill=county)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  geom_boxplot() +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()


Town = read_csv("cleanedData/PopulationCleaned.csv", show_col_types = FALSE)


#importing cleaned crime data frame
crime_data = read_csv("cleanedData/CleanedCrime.csv") %>% 
  select(ShortPostCode,CrimeType,CrimeCount)

#Joining crime and population data frame
crimeData = crime_data %>% 
  left_join(Town, by="ShortPostCode") %>% 
  na.omit()


#Boxplot for 2019-2021 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=district, y=CrimeCount, fill=CrimeType)) + 
  geom_boxplot() +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()


#Pie chart for 2021 Robbery by District
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery") %>%
  group_by(town) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(CrimeCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(town, sumCount, perc, labels) %>% 
  select(town, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2021 Robbery by District")


# line graph for Drugs Offence
Drugoffenserate <- crimeData %>%
  filter(CrimeType=="Drugs") %>%
  group_by(town) %>%
  mutate(sumCount = sum(CrimeCount)) %>%
  ungroup() %>%
  mutate(perc =sumCount / sum(CrimeCount)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  distinct(town, sumCount, perc, labels,date,county) %>%
  select(town, sumCount, perc, labels,date,county)

Drugoffenserate %>%
  group_by(date,county) %>%
  summarise(Averagerate = mean(perc*100)) %>%
  ggplot(aes(x = date, y = Averagerate, color = county))+
  geom_line(size = 1.5,color = "lightgrey") +
  geom_text(aes(label = Averagerate),
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 12, by = 0.5)) +
  scale_x_continuous(breaks = 2019:2022)+
  geom_point(size = 2,
             color = "red")+
  labs(title = "Drug offence rate of both county")


#importing cleaned school data frame
schoolData = read_csv('cleanedData/cleanSchool.csv', show_col_types = FALSE)


#importing cleaned lancashire and leicestershire school data frame
lancashireSchool = read_csv("cleanedData/lancashireSchoolData.csv")
leicestershireSchoolData = read_csv('cleanedData/leicestershireSchoolData.csv')


#Linegraph Average Attainment8Score by year of lancashire county
lancashireSchool %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")




# Linegraph Average Attainment8Score by year of leicestershire county
leicestershireSchoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")



# Boxplot of year 2019-2021 where Attainment8Score is greater than 30 (Lancashire SCHOOL ONLY)
lancashireSchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 Average Attainment8Score of Lancashire Schools")



# Boxplot of year 2019-2021 where Attainment8Score is greater than 30 (Leicestershire SCHOOL ONLY)
leicestershireSchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 Average Attainment8Score of Leicestershire Schools")


