library(tidyverse)


setwd('D:/')
setwd('DATA')



#---------------house rank------

#importing cleaned population data frame
Towns = read_csv("cleanedData/PopulationCleaned.csv", show_col_types = FALSE) %>% 
  select(ShortPostCode, town, district, county)

#importing cleaned house price data frame
House_price = read_csv("cleanedData/CleanedHousePrices.csv",show_col_types = FALSE) %>% 
  na.omit()

#house price rank
Houseprice= House_price %>%
  left_join(Towns,by="ShortPostCode") %>% 
  na.omit()

housePrices=Houseprice  %>% 
  group_by(town) %>% 
  summarise(price=mean(price)) %>% 
  arrange(price) %>% 
  mutate(HouseScore=10-(price/120000)) %>% 
  select(town, HouseScore)

housePrices




#importing cleaned broadband speed data frame
speed_downloads = read_csv("cleanedData/cleanBroadband.csv")


#download speed rank
Speed_Download = speed_downloads %>%
  left_join(Towns,by="ShortPostCode") %>% 
  na.omit()

colnames(Speed_Download)=c("ID","ShortPostCode","AverageDownload","MaxDownload","Averageupload","Maxupload","town","district","county")

download_speed=Speed_Download%>% 
  group_by(town) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/48.4)) %>% 
  select(town,DownloadScore)

download_speed




#crime score rank
#importing cleaned crime data frame
crime_score=read_csv("cleanedData/CleanedCrime.csv")

#crime score rank
crime_rank = crime_score %>%
  left_join(Towns,by="ShortPostCode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(town) %>% 
  summarise(crime_score=mean(CrimeCount)) %>% 
  arrange(crime_score) %>% 
  mutate(crime_score=10-(crime_score/1200)) %>% 
  select(town,crime_score)

crime_rank



#importing cleaned school data frame
school_score=read_csv("cleanedData/cleanSchool.csv")


#school score
school_rank = school_score %>%
  left_join(Towns,by="ShortPostCode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(town) %>% 
  summarise(Att8score=mean(Attainment8Score)) %>% 
  arrange(Att8score) %>% 
  mutate(Att8score=10-(Att8score/1800)) %>% 
  select(town,Att8score)

school_rank


#joining all rank score on the basis of town
rank = housePrices %>% 
  left_join(download_speed,by="town") %>% 
  left_join(school_rank,by="town") %>% 
  left_join(crime_rank,by="town")
  

#replacing NA from data frame to 0
data_without_na <- rank %>%                      
  replace(is.na(.), 0) 

#generating overall rank from data frame
overall_rank = data_without_na %>% 
  mutate(overall_score = rowSums(.[2:5])) 

#ordering 
ordered_rank <- overall_rank[order(-overall_rank$overall_score),]

#getting top three data
topThree=head(ordered_rank,3)

write_csv(topThree,"cleanedData/ranking.csv")
