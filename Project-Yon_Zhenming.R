## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------------------------
library(rgeos)
library(rworldmap)
library(tidyverse)

# get world map
wmap <- getMap(resolution="high")

# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
head(df)
write.csv(df,'country_location.csv')


## ---------------------------------------------------------------------------------------------------------------------------------------

df %>%
  mutate(abs_lat= abs(y))->
  df


## ---------------------------------------------------------------------------------------------------------------------------------------
continent <- read_csv("countryContinent.csv")
location <- read_csv("country_location_to_use.csv")
suicide<- read_csv("Suicide.csv")
country_continent_join <-inner_join(location,continent, by="Country")
All_datasets_Suicide <- inner_join(suicide,country_continent_join, by="Country")

## ---------------------------------------------------------------------------------------------------------------------------------------

All_datasets_Suicide %>%
spread(key = "Sex", value = "SUM")->
  df_Regression

All_datasets_Suicide
df_Regression

## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Europe")%>%
  drop_na(`Both sexes`)->
  Y_Reg
y<- Y_Reg$`Both sexes`

x<-Y_Reg$Latitude
x1 <- Y_Reg$Longtitude

Regression_Europe_long <- lm(y~x1)

regression_Europe<- lm(y ~x)

ggplot(Y_Reg,aes(x=Latitude,y=`Both sexes`))+
  geom_point()+
  geom_smooth(method= lm, se=FALSE)+
  xlab("Latitude ")+
  ylab("Suicide rate per 100000 for both sexes")


## ---------------------------------------------------------------------------------------------------------------------------------------
ggplot(Y_Reg,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for Europe")+
  theme_bw()



## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression%>%
  group_by(Country)%>%
  drop_na(`Both sexes`)%>%
  mutate(abs_latit= abs(Latitude))->
  world_both_sexes

ggplot(world_both_sexes,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for the world")+
  theme_bw()

world_regression <- lm(world_both_sexes$`Both sexes`~world_both_sexes$abs_latit)

## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Asia")%>%
  drop_na(`Both sexes`)->
  Asia_both_sexes

ggplot(Asia_both_sexes,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for Asia")+
  theme_bw()



## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Americas")%>%
  drop_na(`Both sexes`)->
  Americas_both_sexes

ggplot(Americas_both_sexes,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for Americas")+
  theme_bw()


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Africa")%>%
  drop_na(`Both sexes`)->
  Africa_both_sexes

ggplot(Africa_both_sexes,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for Africa")+
  theme_bw()


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Oceania")%>%
  drop_na(`Both sexes`)->
  Oceania_both_sexes

ggplot(Oceania_both_sexes,aes(x=Longtitude,y=Latitude, color=`Both sexes`))+
  geom_point()+
  xlab("Longitude")+
  ggtitle("Suicide rate map for Oceania")+
  theme_bw()



## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Europe")%>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  Europe_Female

df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Europe")%>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  Europe_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  World_Female

df_Regression %>%
  group_by(Country) %>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  World_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Asia")%>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  Asia_Female

df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Asia")%>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  Asia_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Africa")%>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  Africa_Female

df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Africa")%>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  Africa_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Americas")%>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  Americas_Female

df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Americas")%>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  Americas_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Oceania")%>%
  drop_na(Female)%>%
  ungroup()%>%
  summarize(Female_sum=sum(Female))->
  Oceania_Female

df_Regression %>%
  group_by(Country) %>%
  filter(continent=="Oceania")%>%
  drop_na(Male)%>%
  ungroup()%>%
  summarize(Male_sum=sum(Male))->
  Oceania_Male


## ---------------------------------------------------------------------------------------------------------------------------------------
df_Regression %>%
  group_by(Country) %>%
  drop_na(Female)%>%
  select(Country, Female, Latitude, Longtitude, continent, sub_region)->
  World_Female_join

df_Regression %>%
  group_by(Country) %>%
  drop_na(Male)%>%
  select(Country, Male, Latitude, Longtitude, continent, sub_region)->
  World_Male_join

World_join<- inner_join(World_Male_join,World_Female_join,by='Country')

World_join %>%
  mutate(Ratio_Male_to_Female= Male/Female)->
  World_join

ggplot(World_join,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
  geom_point()+
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("World map of the ratio between men and women")




## ---------------------------------------------------------------------------------------------------------------------------------------
World_join%>%
  group_by(Country)%>%
  filter(continent.y=="Europe")->
  world_join_Europe

  ggplot(world_join_Europe,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
    geom_point()+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Europe VS the ratio between men and women")


## ---------------------------------------------------------------------------------------------------------------------------------------
World_join%>%
  group_by(Country)%>%
  filter(continent.y=="Asia")->
  world_join_Asia

  ggplot(world_join_Asia,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
    geom_point()+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Asia VS the ratio between men and women")

## ---------------------------------------------------------------------------------------------------------------------------------------
World_join%>%
  group_by(Country)%>%
  filter(continent.y=="Americas")->
  world_join_Americas

  ggplot(world_join_Americas,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
    geom_point()+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Americas VS the ratio between men and women")

## ---------------------------------------------------------------------------------------------------------------------------------------
World_join%>%
  group_by(Country)%>%
  filter(continent.y=="Oceania")->
  world_join_Oceania

  ggplot(world_join_Oceania,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
    geom_point()+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Oceania VS the ratio between men and women")

## ---------------------------------------------------------------------------------------------------------------------------------------
World_join%>%
  group_by(Country)%>%
  filter(continent.y=="Africa")->
  world_join_Africa

  ggplot(world_join_Africa,aes(x=Longtitude.x,y=Latitude.x, color=Ratio_Male_to_Female))+
    geom_point()+
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("Africa VS the ratio between men and women")

