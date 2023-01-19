
library(tidyverse)

getwd()
setwd("D:/")
setwd('DATA')

#importing data of house prices to r
pp_2019 <- read_csv("obtainedData/Houseprice/pp-2019.csv")
pp_2020 <- read_csv("obtainedData/Houseprice/pp-2020.csv")
pp_2021 <- read_csv("obtainedData/Houseprice/pp-2021.csv")
pp_2022 <- read_csv("obtainedData/Houseprice/pp-2022.csv")


#changing column name
colnames(pp_2019)=c("id","price","date","PAON","SAON","Y/N","FL","HouseNum","flat","street","locality","town","district","county","type1","type2")
colnames(pp_2020)=c("id","price","date","PAON","SAON","Y/N","FL","HouseNum","flat","street","locality","town","district","county","type1","type2")
colnames(pp_2021)=c("id","price","date","PAON","SAON","Y/N","FL","HouseNum","flat","street","locality","town","district","county","type1","type2")
colnames(pp_2022)=c("id","price","date","PAON","SAON","Y/N","FL","HouseNum","flat","street","locality","town","district","county","type1","type2")


#merging all data frame
uncleanedhouseprice = pp_2019 %>%
  add_row(pp_2020) %>%
  add_row(pp_2021) %>% 
  add_row(pp_2022)


#saving merged data frame
write.csv(uncleanedhouseprice,"obtainedData/Houseprice/uncleanedhouseprice.csv", row.names = FALSE)


#cleaning data
cleanedData =select(uncleanedhouseprice, c("id","price","date","PAON","town","district","county","type1"))

filteredData=filter(cleanedData, county =="LANCASHIRE" | county== "LEICESTERSHIRE")

housePrice = filteredData %>%  
  mutate(ShortPostCode = substr(PAON, start = 1, stop = 4)) %>% 
  mutate(date = str_trim(substring(date, 1,4))) %>% 
  select(id, PAON, ShortPostCode, price, date,type1)
View(housePrice)


#Store the final data frame of house price.
write.csv(housePrice,"cleanedData/CleanedHousePrices.csv", row.names = FALSE)


#Cleaning Data for Population
uncleanedhouseprices = read_csv("obtainedData/Houseprice/uncleanedhouseprice.csv")

Population = read_csv("obtainedData/Population/Population2011.csv")

#generating other population based on 2011 population
Populationcleaned = Population %>%  
  mutate(ShortPostCode = str_trim(substring(Postcode, 1,4))) %>%
  group_by(ShortPostCode) %>%
  summarise_at(vars(Population),list(Population2011 = sum)) %>%
  mutate(Population2012= as.integer(1.00695353132322269 * Population2011)) %>%
  mutate(Population2013= as.integer(1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= as.integer(1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= as.integer(1.00792367505802859 * Population2014)) %>%
  mutate(Population2015= as.integer(1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= as.integer(1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= as.integer(1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= as.integer(1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= as.integer(1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= as.integer(1.00561255390388033 * Population2019)) %>%
  mutate(Population2021= as.integer(1.00561255390388033 * Population2020)) %>%
  select(ShortPostCode,Population2019,Population2020,Population2021)


#saving merged population data frame
write.csv(Populationcleaned,"cleanedData/population.csv", row.names = FALSE)

FilteredCounty = filteredData %>% 
  mutate(ShortPostCode = str_trim(substring(PAON, 1,4))) %>%
  mutate(date = str_trim(substring(date, 1,4))) %>% 
  left_join(Populationcleaned,by="ShortPostCode") %>% 
  select(PAON, ShortPostCode, date, town,district, county, Population2019,Population2020,Population2021) %>% 
  group_by(ShortPostCode) %>%
  filter(row_number()==1) %>%
  arrange(county) %>% 
  na.omit()

colnames(FilteredCounty) = c("PostCode", "ShortPostCode", "date", "town","district", "county", "Population2019","Population2020","Population2021")

#saving cleaned population data frame
write.csv(FilteredCounty,"cleanedData/PopulationCleaned.csv", row.names = FALSE)


#importing broadband speed data frame to r
Broadband = read_csv("obtainedData/Broadband/BroadbandSpeeds.csv", show_col_types = FALSE)

Towns = read_csv("cleanedData/PopulationCleaned.csv",show_col_types = FALSE)

#replacing NA data with 0
Broadband = replace(Broadband,is.na(Broadband), 0)


#cleaning broadband speed data frame
cleanBroadbandSpeeds = Broadband %>%
  mutate(ShortPostCode = str_trim(substring(postcode_space, 1,4))) %>% 
  group_by(ShortPostCode) %>% 
  summarise_at(vars("Average download speed (Mbit/s)","Maximum download speed (Mbit/s)","Average upload speed (Mbit/s)",
                    "Maximum upload speed (Mbit/s)"),list(name = mean)) %>% 
  left_join(Towns,by="ShortPostCode") %>% 
  filter(county=="LANCASHIRE"|county=="LEICESTERSHIRE") %>% 
  arrange(county) %>% 
  select(-county,town,district,county,Population2019,Population2020,Population2021) %>% 
  rename("AverageDownload"="Average download speed (Mbit/s)_name","MaxDownload"="Maximum download speed (Mbit/s)_name",
         "AverageUpload"="Average upload speed (Mbit/s)_name","MaxUpload"="Maximum upload speed (Mbit/s)_name") %>% 
  na.omit()

#selecting certain columns
cleanBroadband = cleanBroadbandSpeeds %>%
  select(ShortPostCode,AverageDownload, MaxDownload,AverageUpload,MaxUpload)

#saving cleaned broadband data frame
write.csv(cleanBroadband, "cleanedData/cleanBroadband.csv")


#importing school data frame to r
lancashireSchool19 = read_csv("obtainedData/SchoolData/lancashire2019.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2019)
lancashireSchool20 = read_csv("obtainedData/SchoolData/lancashire2020.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2020)
lancashireSchool21 = read_csv("obtainedData/SchoolData/lancashire2021.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2021)

leicestershireSchool19 = read_csv("obtainedData/SchoolData/leicestershire2019.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2019)
leicestershireSchool20 = read_csv("obtainedData/SchoolData/leicestershire2020.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2020)
leicestershireSchool21 = read_csv("obtainedData/SchoolData/leicestershire2021.csv", show_col_types = FALSE)%>% 
  mutate(Year = 2021)

#selecting required column from data frame
lancashireSchool19 = select(lancashireSchool19,Year, PCODE, SCHNAME, ATT8SCR)
lancashireSchool20 = select(lancashireSchool20,Year, PCODE, SCHNAME, ATT8SCR)
lancashireSchool21 = select(lancashireSchool21,Year, PCODE, SCHNAME, ATT8SCR)

leicestershireSchool19 = select(leicestershireSchool19,Year, PCODE, SCHNAME, ATT8SCR)
leicestershireSchool20 = select(leicestershireSchool20,Year, PCODE, SCHNAME, ATT8SCR)
leicestershireSchool21 = select(leicestershireSchool21,Year, PCODE, SCHNAME, ATT8SCR)


#cleaning school data frame
schoolData = lancashireSchool19 %>% 
  add_row(lancashireSchool20) %>% 
  add_row(lancashireSchool21) %>% 
  add_row(leicestershireSchool19) %>% 
  add_row(leicestershireSchool20) %>% 
  add_row(leicestershireSchool21) %>%
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>% 
  mutate(ID = row_number()) %>% 
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>%
  na.omit()
colnames(schoolData) = c("ID", "Year", "PostCode", "ShortPostCode", "SchoolName", "Attainment8Score")


#saving cleaned school data frame
write.csv(schoolData, "cleanedData/cleanSchool.csv")



#importing  Lancashire crime data to r 
cd2019_12_lan = read_csv("obtainedData/Crime Rate/2019-12/2019-12-lancashire-street.csv",show_col_types = FALSE)
cd2020_01_lan = read_csv("obtainedData/Crime Rate/2020-01/2020-01-lancashire-street.csv",show_col_types = FALSE)
cd2020_02_lan = read_csv("obtainedData/Crime Rate/2020-02/2020-02-lancashire-street.csv",show_col_types = FALSE)
cd2020_03_lan = read_csv("obtainedData/Crime Rate/2020-03/2020-03-lancashire-street.csv",show_col_types = FALSE)
cd2020_04_lan = read_csv("obtainedData/Crime Rate/2020-04/2020-04-lancashire-street.csv",show_col_types = FALSE)
cd2020_05_lan = read_csv("obtainedData/Crime Rate/2020-05/2020-05-lancashire-street.csv",show_col_types = FALSE)
cd2020_06_lan = read_csv("obtainedData/Crime Rate/2020-06/2020-06-lancashire-street.csv",show_col_types = FALSE)
cd2020_07_lan = read_csv("obtainedData/Crime Rate/2020-07/2020-07-lancashire-street.csv",show_col_types = FALSE)
cd2020_08_lan = read_csv("obtainedData/Crime Rate/2020-08/2020-08-lancashire-street.csv",show_col_types = FALSE)
cd2020_09_lan = read_csv("obtainedData/Crime Rate/2020-09/2020-09-lancashire-street.csv",show_col_types = FALSE)
cd2020_10_lan = read_csv("obtainedData/Crime Rate/2020-10/2020-10-lancashire-street.csv",show_col_types = FALSE)
cd2020_11_lan = read_csv("obtainedData/Crime Rate/2020-11/2020-11-lancashire-street.csv",show_col_types = FALSE)
cd2020_12_lan = read_csv("obtainedData/Crime Rate/2020-12/2020-12-lancashire-street.csv",show_col_types = FALSE)
cd2020_12_lan = read_csv("obtainedData/Crime Rate/2020-12/2020-12-lancashire-street.csv",show_col_types = FALSE)

cd2021_01_lan = read_csv("obtainedData/Crime Rate/2021-01/2021-01-lancashire-street.csv",show_col_types = FALSE)
cd2021_02_lan = read_csv("obtainedData/Crime Rate/2021-02/2021-02-lancashire-street.csv",show_col_types = FALSE)
cd2021_03_lan = read_csv("obtainedData/Crime Rate/2021-03/2021-03-lancashire-street.csv",show_col_types = FALSE)
cd2021_04_lan = read_csv("obtainedData/Crime Rate/2021-04/2021-04-lancashire-street.csv",show_col_types = FALSE)
cd2021_05_lan = read_csv("obtainedData/Crime Rate/2021-05/2021-05-lancashire-street.csv",show_col_types = FALSE)
cd2021_06_lan = read_csv("obtainedData/Crime Rate/2021-06/2021-06-lancashire-street.csv",show_col_types = FALSE)
cd2021_07_lan = read_csv("obtainedData/Crime Rate/2021-07/2021-07-lancashire-street.csv",show_col_types = FALSE)
cd2021_08_lan = read_csv("obtainedData/Crime Rate/2021-08/2021-08-lancashire-street.csv",show_col_types = FALSE)
cd2021_09_lan = read_csv("obtainedData/Crime Rate/2021-09/2021-09-lancashire-street.csv",show_col_types = FALSE)
cd2021_10_lan = read_csv("obtainedData/Crime Rate/2021-10/2021-10-lancashire-street.csv",show_col_types = FALSE)
cd2021_11_lan = read_csv("obtainedData/Crime Rate/2021-11/2021-11-lancashire-street.csv",show_col_types = FALSE)
cd2021_12_lan = read_csv("obtainedData/Crime Rate/2021-12/2021-12-lancashire-street.csv",show_col_types = FALSE)

cd2022_01_lan = read_csv("obtainedData/Crime Rate/2022-01/2022-01-lancashire-street.csv",show_col_types = FALSE)
cd2022_02_lan = read_csv("obtainedData/Crime Rate/2022-02/2022-02-lancashire-street.csv",show_col_types = FALSE)
cd2022_03_lan = read_csv("obtainedData/Crime Rate/2022-03/2022-03-lancashire-street.csv",show_col_types = FALSE)
cd2022_04_lan = read_csv("obtainedData/Crime Rate/2022-04/2022-04-lancashire-street.csv",show_col_types = FALSE)
cd2022_05_lan = read_csv("obtainedData/Crime Rate/2022-05/2022-05-lancashire-street.csv",show_col_types = FALSE)
cd2022_06_lan = read_csv("obtainedData/Crime Rate/2022-06/2022-06-lancashire-street.csv",show_col_types = FALSE)
cd2022_07_lan = read_csv("obtainedData/Crime Rate/2022-07/2022-07-lancashire-street.csv",show_col_types = FALSE)
cd2022_08_lan = read_csv("obtainedData/Crime Rate/2022-08/2022-08-lancashire-street.csv",show_col_types = FALSE)
cd2022_09_lan = read_csv("obtainedData/Crime Rate/2022-09/2022-09-lancashire-street.csv",show_col_types = FALSE)
cd2022_10_lan = read_csv("obtainedData/Crime Rate/2022-10/2022-10-lancashire-street.csv",show_col_types = FALSE)
cd2022_11_lan = read_csv("obtainedData/Crime Rate/2022-11/2022-11-lancashire-street.csv",show_col_types = FALSE)


#importing  Leicestershire crime data to r
cd2019_12_lei = read_csv("obtainedData/Crime Rate/2019-12/2019-12-leicestershire-street.csv",show_col_types = FALSE)
cd2020_01_lei = read_csv("obtainedData/Crime Rate/2020-01/2020-01-leicestershire-street.csv",show_col_types = FALSE)
cd2020_02_lei = read_csv("obtainedData/Crime Rate/2020-02/2020-02-leicestershire-street.csv",show_col_types = FALSE)
cd2020_03_lei = read_csv("obtainedData/Crime Rate/2020-03/2020-03-leicestershire-street.csv",show_col_types = FALSE)
cd2020_04_lei = read_csv("obtainedData/Crime Rate/2020-04/2020-04-leicestershire-street.csv",show_col_types = FALSE)
cd2020_05_lei = read_csv("obtainedData/Crime Rate/2020-05/2020-05-leicestershire-street.csv",show_col_types = FALSE)
cd2020_06_lei = read_csv("obtainedData/Crime Rate/2020-06/2020-06-leicestershire-street.csv",show_col_types = FALSE)
cd2020_07_lei = read_csv("obtainedData/Crime Rate/2020-07/2020-07-leicestershire-street.csv",show_col_types = FALSE)
cd2020_08_lei = read_csv("obtainedData/Crime Rate/2020-08/2020-08-leicestershire-street.csv",show_col_types = FALSE)
cd2020_09_lei = read_csv("obtainedData/Crime Rate/2020-09/2020-09-leicestershire-street.csv",show_col_types = FALSE)
cd2020_10_lei = read_csv("obtainedData/Crime Rate/2020-10/2020-10-leicestershire-street.csv",show_col_types = FALSE)
cd2020_11_lei = read_csv("obtainedData/Crime Rate/2020-11/2020-11-leicestershire-street.csv",show_col_types = FALSE)
cd2020_12_lei = read_csv("obtainedData/Crime Rate/2020-12/2020-12-leicestershire-street.csv",show_col_types = FALSE)

cd2021_01_lei = read_csv("obtainedData/Crime Rate/2021-01/2021-01-leicestershire-street.csv",show_col_types = FALSE)
cd2021_02_lei = read_csv("obtainedData/Crime Rate/2021-02/2021-02-leicestershire-street.csv",show_col_types = FALSE)
cd2021_03_lei = read_csv("obtainedData/Crime Rate/2021-03/2021-03-leicestershire-street.csv",show_col_types = FALSE)
cd2021_04_lei = read_csv("obtainedData/Crime Rate/2021-04/2021-04-leicestershire-street.csv",show_col_types = FALSE)
cd2021_05_lei = read_csv("obtainedData/Crime Rate/2021-05/2021-05-leicestershire-street.csv",show_col_types = FALSE)
cd2021_06_lei = read_csv("obtainedData/Crime Rate/2021-06/2021-06-leicestershire-street.csv",show_col_types = FALSE)
cd2021_07_lei = read_csv("obtainedData/Crime Rate/2021-07/2021-07-leicestershire-street.csv",show_col_types = FALSE)
cd2021_08_lei = read_csv("obtainedData/Crime Rate/2021-08/2021-08-leicestershire-street.csv",show_col_types = FALSE)
cd2021_09_lei = read_csv("obtainedData/Crime Rate/2021-09/2021-09-leicestershire-street.csv",show_col_types = FALSE)
cd2021_10_lei = read_csv("obtainedData/Crime Rate/2021-10/2021-10-leicestershire-street.csv",show_col_types = FALSE)
cd2021_11_lei = read_csv("obtainedData/Crime Rate/2021-11/2021-11-leicestershire-street.csv",show_col_types = FALSE)
cd2021_12_lei = read_csv("obtainedData/Crime Rate/2021-12/2021-12-leicestershire-street.csv",show_col_types = FALSE)

cd2022_01_lei = read_csv("obtainedData/Crime Rate/2022-01/2022-01-leicestershire-street.csv",show_col_types = FALSE)
cd2022_02_lei = read_csv("obtainedData/Crime Rate/2022-02/2022-02-leicestershire-street.csv",show_col_types = FALSE)
cd2022_03_lei = read_csv("obtainedData/Crime Rate/2022-03/2022-03-leicestershire-street.csv",show_col_types = FALSE)
cd2022_04_lei = read_csv("obtainedData/Crime Rate/2022-04/2022-04-leicestershire-street.csv",show_col_types = FALSE)
cd2022_05_lei = read_csv("obtainedData/Crime Rate/2022-05/2022-05-leicestershire-street.csv",show_col_types = FALSE)
cd2022_06_lei = read_csv("obtainedData/Crime Rate/2022-06/2022-06-leicestershire-street.csv",show_col_types = FALSE)
cd2022_07_lei = read_csv("obtainedData/Crime Rate/2022-07/2022-07-leicestershire-street.csv",show_col_types = FALSE)
cd2022_08_lei = read_csv("obtainedData/Crime Rate/2022-08/2022-08-leicestershire-street.csv",show_col_types = FALSE)
cd2022_09_lei = read_csv("obtainedData/Crime Rate/2022-09/2022-09-leicestershire-street.csv",show_col_types = FALSE)
cd2022_10_lei = read_csv("obtainedData/Crime Rate/2022-10/2022-10-leicestershire-street.csv",show_col_types = FALSE)
cd2022_11_lei = read_csv("obtainedData/Crime Rate/2022-11/2022-11-leicestershire-street.csv",show_col_types = FALSE)

#Merging lancashire and leicestershire data frame
crime_data = add_row(cd2019_12_lan) %>% 
  add_row(cd2020_01_lan) %>% add_row(cd2020_02_lan) %>% add_row(cd2020_03_lan) %>% add_row(cd2020_04_lan) %>% add_row(cd2020_05_lan) %>% add_row(cd2020_06_lan) %>% 
  add_row(cd2020_07_lan) %>% add_row(cd2020_08_lan) %>% add_row(cd2020_09_lan) %>% add_row(cd2020_10_lan) %>% add_row(cd2020_11_lan) %>% add_row(cd2020_12_lan) %>% 
  add_row(cd2021_01_lan) %>% add_row(cd2021_02_lan) %>% add_row(cd2021_03_lan) %>% add_row(cd2021_04_lan) %>% add_row(cd2021_05_lan) %>% add_row(cd2021_06_lan) %>% 
  add_row(cd2021_07_lan) %>% add_row(cd2021_08_lan) %>% add_row(cd2021_09_lan) %>% add_row(cd2021_10_lan) %>% add_row(cd2021_11_lan) %>% add_row(cd2021_12_lan) %>% 
  add_row(cd2022_01_lan) %>% add_row(cd2022_02_lan) %>% add_row(cd2022_03_lan) %>% add_row(cd2022_04_lan) %>% add_row(cd2022_05_lan) %>% add_row(cd2022_06_lan) %>% 
  add_row(cd2022_07_lan) %>% add_row(cd2022_08_lan) %>% add_row(cd2022_09_lan) %>% add_row(cd2022_10_lan) %>% add_row(cd2022_11_lan) %>% 
  add_row(cd2019_12_lei) %>% 
  add_row(cd2020_01_lei) %>% add_row(cd2020_02_lei) %>% add_row(cd2020_03_lei) %>% add_row(cd2020_04_lei) %>% add_row(cd2020_05_lei) %>% add_row(cd2020_06_lei) %>% 
  add_row(cd2020_07_lei) %>% add_row(cd2020_08_lei) %>% add_row(cd2020_09_lei) %>% add_row(cd2020_10_lei) %>% add_row(cd2020_11_lei) %>% add_row(cd2020_12_lei) %>% 
  add_row(cd2021_01_lei) %>% add_row(cd2021_02_lei) %>% add_row(cd2021_03_lei) %>% add_row(cd2021_04_lei) %>% add_row(cd2021_05_lei) %>% add_row(cd2021_06_lei) %>% 
  add_row(cd2021_07_lei) %>% add_row(cd2021_08_lei) %>% add_row(cd2021_09_lei) %>% add_row(cd2021_10_lei) %>% add_row(cd2021_11_lei) %>% add_row(cd2021_12_lei) %>% 
  add_row(cd2022_01_lei) %>% add_row(cd2022_02_lei) %>% add_row(cd2022_03_lei) %>% add_row(cd2022_04_lei) %>% add_row(cd2022_05_lei) %>% add_row(cd2022_06_lei) %>% 
  add_row(cd2022_07_lei) %>% add_row(cd2022_08_lei) %>% add_row(cd2022_09_lei) %>% add_row(cd2022_10_lei) %>% add_row(cd2022_11_lei)


#saving merged data frame
write.csv(crime_data, "obtainedData/Crime Rate/MergedCrimeData1.csv") 


#Cleaning crime data
crimedata = read_csv('obtainedData/Crime Rate/MergedCrimeData1.csv') %>% 
  select(Month, `LSOA code`, `Crime type`)

colnames(crimedata) = c("Year", "lsoa11cd", "CrimeType")

LsoaToPostcode = read_csv('obtainedData/Population/Postcode to LSOA 2.csv')

crimedataCleaned = crimedata %>%  
  left_join(LsoaToPostcode,by="lsoa11cd") %>% 
  mutate(shortPostCode = substr(pcds,start = 1,stop = 4)) %>% 
  mutate(Year = substr(Year, start=1,stop = 4)) %>% 
  group_by(shortPostCode,CrimeType,Year)%>% 
  select(shortPostCode, Year, CrimeType, ) %>% 
  na.omit() %>% 
  tally()

crimedataCleaned = cbind(ID = 1:nrow(crimedataCleaned), crimedataCleaned)  
colnames(crimedataCleaned)= c("ID","ShortPostCode","CrimeType","Year" , "CrimeCount")

#saving cleaned crime data frame
write.csv(crimedataCleaned, "cleanedData/CleanedCrime.csv") 





