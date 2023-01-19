library(tidyverse)


setwd('D:/')
setwd('DATA')


#liner regression

#importing cleaned population data frame
Towns = read_csv("cleanedData/PopulationCleaned.csv", show_col_types = FALSE) %>% 
  select(ShortPostCode, town, district, county)

#importing cleaned house price data frame
prices = read_csv("cleanedData/CleanedHousePrices.csv",show_col_types = FALSE) %>% 
  na.omit()


#importing cleaned broadband data frame
speeds = read_csv("cleanedData/cleanBroadband.csv",show_col_types = FALSE) %>% 
  na.omit()  

#importing cleaned crime data frame
crime=read_csv("cleanedData/CleanedCrime.csv",show_col_types = FALSE)

#importing cleaned school data frame
schools=read_csv("cleanedData/cleanSchool.csv",show_col_types = FALSE) %>% 
  na.omit()



#----House prices and Download Speed----

HousePrices = prices %>%
  filter(date=="2020") %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(price=mean(price))

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(AverageDownload=mean(AverageDownload))

lm_res = HousePrices %>% 
  select(town,price) %>% 
  left_join(BroardbandSpeeds,by="town")

model = lm(data= lm_res, price~AverageDownload)

#linear model summary
summary(model)


color= c("LANCASHIRE" = "red", "LEICESTERSHIRE" = "blue")

#linear model summary
ggplot(lm_res,aes(x=AverageDownload,y=price)) +
  geom_point(data = filter(lm_res,county=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res,county=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Download Speed (Mbit/s)",y="Price (£)",title="House Prices vs Download Speed",color="county")





#----House price and drug offence----

HousePrices = prices %>%
  filter(date=="2020") %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(price=mean(price))

Drugs = crime %>%
  left_join(Towns,by="ShortPostCode") %>%
  group_by(town,county) %>%
  filter(CrimeType=="Drugs") %>% 
  tally() %>% 
  na.omit()

lm_res6 = HousePrices %>% left_join(Drugs ,by="town") %>%
  group_by(town,county.x,price) %>% 
  na.omit()

model1 = lm(data= lm_res6, price~n)

#linear model summary
summary(model1)

color= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "green")

#linear model visualization
ggplot(lm_res6,aes(x=price,y=n)) +
  geom_point(data = filter(lm_res6,county=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res6,county=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="town",y="Drug count",title="House Prices vs Drug",color="county")






#----average download and school----


school_lm = schools %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(score=mean(Attainment8Score))

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(AverageDownload=mean(AverageDownload))

lm_res4 = school_lm %>% 
  select(town,score) %>% 
  left_join(BroardbandSpeeds,by="town")

model4 = lm(data= lm_res4, score~AverageDownload)

#linear model summary
summary(model4)


model4 = lm(data= lm_res4, score~AverageDownload)
summary(model4)

colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

#linear model visualization
ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
  geom_point(data = filter(lm_res4,county=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res4,county=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="score vs AverageDownload",color="county")



#----House price and school----

HousePrices = prices %>%
  filter(date=="2020") %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(price=mean(price))


school_lm = schools %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(score=mean(Attainment8Score))

lm_res5 = HousePrices %>% 
  select(town,price) %>% 
  left_join(school_lm,by="town")

model5 = lm(data= lm_res5, price~score)

#linear model summary
summary(model5)


color= c("LANCASHIRE" = "red", "LEICESTERSHIRE" = "blue")

#linear model visualization
ggplot(lm_res5,aes(x=score,y=price)) +
  geom_point(data = filter(lm_res5,county=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res5,county=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="score",y="Price (£)",title="Attainment 8 Score vs House Prices ",color="county")




#----average download and drug----

BroardbandSpeeds = speeds %>%
  left_join(Towns,by="ShortPostCode") %>%  
  group_by(town,county) %>%
  summarise(AverageDownload=mean(AverageDownload))


Drugs = crime %>%
  left_join(Towns,by="ShortPostCode") %>%
  group_by(town,county) %>%
  filter(CrimeType=="Drugs") %>% 
  tally() %>% 
  na.omit()

lm_res3 = BroardbandSpeeds %>% 
  left_join(Drugs ,by="town") %>% 
  na.omit()

model3 = lm(data= lm_res3, AverageDownload~n)

#linear model summary
summary(model3)

colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

#linear model visualization
ggplot(lm_res3,aes(x=n,y=AverageDownload)) +
  geom_point(data = filter(lm_res3,county.x=="LANCASHIRE"),aes(color="LANCASHIRE"))+
  geom_point(data = filter(lm_res3,county.x=="LEICESTERSHIRE"), aes(color="LEICESTERSHIRE")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="count",y="Average Download",title="Drug vs AverageDownload",color="county")



