library(tidyverse)
library(maptools)
library(ggplot2)
library(ggmap)
# install necessary packages
library(dplyr)
library(maptools)
library(ggplot2)
library(ggmap)  # only for superimposing with Google maps
library(doBy)
# to make fortify possible without errors

library(gpclib)
gpclibPermit()

France <- read.csv("France1.csv",sep = ";" )
Germany <- read.csv("Germany.csv",sep = ";" )
Hungary <- read.csv("Hungary.csv",sep = ";" )
Italy1 <- read.csv("italy1992.csv",sep = ";" )
Italy2 <- read.csv("italy1994.csv",sep = ";" )
Italy3a <- read.csv("italy1996magg.csv",sep = ";" )
Italy4a <- read.csv("italy2001mag.csv",sep = ";" )
Italy5 <- read.csv("italy2006.csv",sep = ";" )
Italy6 <- read.csv("italy2008.csv",sep = ";" )
Italy7 <- read.csv("italy2013.csv",sep = ";" )
Uk1 <- read.csv("uk1992.csv",sep = ";" )
Uk2 <- read.csv("uk1997.csv",sep = ";" )
Uk3 <- read.csv("uk2001.csv",sep = ";" )
Uk4 <- read.csv("uk2005.csv",sep = ";" )
Uk5 <- read.csv("uk2010.csv",sep = ";" )
austria <- read.csv("austria.csv", sep = ";")

#Let's tidy up the data

#France

View(France1)

France1 <- France %>% filter(France$X != "Year") %>%
   select(-c(`Electorate`,`Votes`,`Valid.votes`,`Invalid.votes`)) %>%
   gather(-`X`,key = "party", value = "Result" ) 

France1$Result[France1$Result == "-"] <- NA


France1 %>% group_by(party) %>% summarize(mean(Result))

partyF <- read.csv("France_parties.csv",sep = ";")

partyF <- partyF %>% filter(X != "") %>% rename( "party" = X, "weight" = X.1)
partyF

France_07 <- inner_join(France1, partyF) %>% filter(Result != "<NA>" ) %>%
  rename( "Year" = X, "Votes" = Result, "Parties" = party) %>% mutate( country = "FR")



#legislative elections 2012 and 2017
#source: https://www.interieur.gouv.fr/Elections/Les-resultats/Legislatives/elecresult__LG2012/(path)/LG2012//FE.html

France1217 <- read.csv("France2012-2017.csv",sep = ";")

France1217 <- France1217 %>% select(Parties,Votes,weight,Year,country) 

France_07[1] <- as.numeric(as.character(France_07$Year))


France_el <- rbind(France_07,France1217)   

France_el <- France_el %>% filter(Votes != is.na(TRUE)) %>%
            rename("Party" = Parties, "Weights"=weight, "Country" = country)
  
View(France_el)
  

#Germany


Germany1 <- Germany %>% filter(Germany$Year != "NA") %>%
  select(-c(`Elect..Tot.`,`Votes`,`Valid.votes`,`Invalid.votes`)) %>%
  gather(-`Year`,key = "party", value = "Result" ) 

Germany1$Result[Germany1$Result == "-"] <- NA

#Germany_towrite <- Germany1 %>% group_by(party) %>% summarize(mean(Result))

#write.csv(Germany_towrite, "Germany_parties.csv")

Germany_parties <- read.csv("Germany_parties_toread.csv",sep=";")

Germany_parties <- Germany_parties %>% select(-Column1)

Germany09 <- inner_join(Germany1,Germany_parties)

Germany09 <- Germany09 %>% mutate(Country = "DE") %>%
  rename("Votes" = Result, "Weights" = weights, "Party" = party)

Germany1317 <- read.csv("Germany1317.csv", sep = ";")

Germany1317 <- Germany1317 %>% rename("Weights" = Weigths)


Germany_el <- rbind(Germany09,Germany1317)



View(Germany09)
#Hungary

View(Hungary1)

Hungary1 <- Hungary %>% filter(Hungary$X != "Year") %>%
  select(-c(`Elect..Total`,`Votes`,`Valid.votes`,`Invalid.votes`)) %>%
  gather(-`X`,key = "party", value = "Result" ) 

#Hungary_tosave <- Hungary1 %>% group_by(party) %>% summarize(mean(Result))

#write.csv(Hungary_tosave, "Hungary_parties.csv")


Hungary_parties <- read.csv("Hungary_parties_toread.csv", sep= ";")

Hungary_parties <- Hungary_parties %>% select(-Column1)

Hungary10 <- inner_join(Hungary1, Hungary_parties)

Hungary10 <- Hungary10 %>% filter(Result != "-") %>%
        rename("Year" = X, "Party" = party, "Votes" = Result, "Weights" = mean.Result.)  %>%
        mutate(Country = "HU")


Hungary1418 <- read.csv("Hungary_1418.csv", sep=";")

Hungary10$Year <- as.numeric(as.character(Hungary10$Year))

Hungary_el <- rbind(Hungary10, Hungary1418)
View(Hungary_el)


#Italy

View(italy6)

italy1 <- Italy1 %>% select(`Party.List`,`Votes..`) %>%
          mutate(Year = "1992")

italy2 <- Italy2 %>% select(`Party.List`,`Votes..`) %>%
  mutate(Year = "1994")

italy3a <- Italy3a %>% select(`Party.List`,`Votes..`) %>%
  mutate(Year = "1996")


italy4a <- Italy4a %>% select(`Party.List`,`Votes..`) %>%
  mutate(Year = "2001")


italy5 <- Italy5 <- Italy5 %>% select(`Party.List`,`Votes..`) %>%
  mutate(Year = "2006")

italy6 <- Italy6 %>% 
gather(-Party, key = "Party.List", value="Votes..")   %>% 
select(-Party) %>%
  mutate(Year = "2008")


italy7 <- Italy7 %>% 
  gather(-Party, key = "Party.List", value="Votes..")   %>% 
  select(-Party) %>%
  mutate(Year = "2013")

Italy <- rbind(italy1,italy2,italy3a,italy4a,italy5)


Italy$Votes..[Italy$Votes.. == "-"] <- NA

Italy$Votes.. <- as.numeric(as.numeric(as.character(Italy$Votes..)))

italy6 <- as.tibble(italy6)

italy6$Votes.. <- as.numeric(italy6$Votes..)

italy7 <- as.tibble(italy7)

italy7$Votes.. <- as.numeric(italy7$Votes..)

Italy <- rbind(Italy,italy6,italy7)

#we take into consideration the parties with more then 3% as outcome

Italy <- Italy %>% filter( Votes.. > 3.00)

#Ita_tosave <- Italy %>% group_by(Party.List) %>% summarize(mean(Votes..))

#write.csv(Ita_tosave, "Italy_parties.csv")

partyI <- read.csv("Italy_parties_toread.csv", sep = ";")

partyI <- partyI %>% select(-Column1)

View(Italy)
View(partyI)
Italy13 <- inner_join(Italy, partyI) 

Italy13 <- Italy13 %>% filter(Party.List != "Valid.votes") %>%
  filter(Party.List != "Invalid.votes") %>% 
  filter(Party.List != "Valid votes")  %>% 
  filter(Party.List != "Invalid votes")  %>% 
  filter(Party.List != "Votes")  %>% 
  filter(Party.List != "Electorate") %>%
  rename("Party" = Party.List, "Votes" = Votes.., "Weights" = weights) %>%
  mutate(Country = "IT")

Italy18 <- read.csv("Italy2018.csv", sep = ";")

Italy_el <- rbind(Italy13,Italy18)

View(Italy_el)

#UK

uk1 <- Uk1 %>% select(-`Total.Turnout`) %>%
  filter(Con != "NA") %>%
  gather(-Party, key= "party", value ="Result") %>%
  select(-Party) %>% 
  mutate( Year = "1992")

uk2 <- Uk2 %>% select(-`Total.Turnout`,-`Electorate`)%>%
  gather(-Party, key= "party", value ="Result")%>%
  select(-Party) %>% 
  mutate( Year = "1997")

uk3 <- Uk3 %>% select(-`Total.Turnout`) %>%
  gather(-Party, key= "party", value ="Result") %>%
  select(-Party) %>% 
  mutate( Year = "2001")

uk4 <- Uk4 %>% select(-`Total.Turnout`) %>%
  gather(-Party, key= "party", value ="Result") %>%
  select(-Party) %>% 
  mutate( Year = "2005")

uk5 <- Uk5 %>% select(-`Total.Turnout`) %>%
  gather(-Party, key= "party", value ="Result") %>%
  select(-Party) %>% 
  mutate( Year = "2010")

UK <- rbind(uk1,uk2,uk3,uk4,uk5)

UK <- UK %>% filter(UK$party != "Electorate")




#Uk_tosave <- UK %>% group_by(party) %>% summarize(mean(Result))

#write.csv(Uk_tosave, "Uk_parties.csv")

Uk_parties <- read.csv("Uk_parties_toread.csv", sep = ";")

Uk_parties <- Uk_parties %>% select(-Column1)

UK10 <- inner_join(UK,Uk_parties)


UK1517 <- read.csv("UK1517.csv", sep = ";")

UK1517 <- UK1517 %>% select(-X) %>% mutate(Votes = Votes*100)

UK10 <- UK10 %>% rename("Party"=party, "Votes" = Result, "Weights" = weights) %>%
              mutate(Country = "UK")

UK10$Year <- as.numeric(as.character(UK10$Year))



UK_el <- rbind(UK10,UK1517)


#austria

View(Austria)

Austria <- austria %>% select(-`Electorate`,-`Votes`,-`Valid.votes`,-`Invalid.votes`) %>%
  gather(-`Year`,key="party", value = "Result")



#Austria_tosave <- Austria %>% group_by(party) %>% summarize(mean(Result))

#write.csv(Austria_tosave, "Austria_parties.csv")


Austria_parties <- read.csv("Austria_parties_toread.csv", sep = ";")

Austria_parties <- Austria_parties %>% select(-Column1)

Austria08 <- inner_join(Austria, Austria_parties)

Austria08 <- Austria08 %>% filter(Result != "-")


Austria1317 <- read.csv("Austria1317.csv", sep=";")

Austria08 <- Austria08 %>% rename("Party" = party, "Votes"=Result, "Weights"=weights) %>%
                          mutate(Country = "AT")

Austria_el <- rbind(Austria08,Austria1317)


Elections <- rbind(Austria_el,Italy_el,Germany_el,UK_el,Hungary_el,France_el)

Elections$Votes <- as.numeric(Elections$Votes)


Elections <- Elections %>% filter(Weights != is.na(TRUE))  %>%
                           filter(Votes != is.na(TRUE)) %>%
                           filter(Votes != 0.00)


Elections <- mutate(Elections, Index = Weights* Votes / 100) 

Elections_mean <- Elections %>% select(Country,Year,Index) %>% 
                        group_by(Country) %>% 
                        summarise(mean(Index))


codes <- read.csv("iso_code_europe_partial.csv", sep= ";")

codes <- codes %>% rename("Populist index" = Populist.Index )
# requires downloading and unzipping NUTS_2013_01M_SH.zip from
# http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units
eurMap <- readShapeSpatial("NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp")

# only main adm level
eurMap <- subset(eurMap, nchar(as.character(NUTS_ID)) == 2)

# EL for Grece, UK for United Kingdom
eurMapDf <- fortify(eurMap, region='NUTS_ID')

# we assume two columns: CountryCode and NGA_Coverage


Elections_mean <- Elections_mean %>% rename("CountryCode" = Country, "Populist index" = `mean(Index)`)

#write.csv(Elections_mean, "Overview_Elections.csv")

eurData <- Elections_mean
#eurData <- read.csv("Overview_Elections.csv")
#eurData <- eurData %>% rename("Populist index" = Populist.index ) %>% select(-X)
#eurData <- rbind(eurData,codes)
eurData <- eurData %>% mutate(`Populist index`= `Populist index` * 10)
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  


txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)

# inverse order (to have visible borders)

p <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.2f", `Populist index`)), data=txtVal, col="gray50",cex=3) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()
p

Elections %>% group_by(Year,Country) %>% summarize(mean(Index))

Elections$Year <- as.numeric(Elections$Year)

Elections00 <- Elections %>%filter(Year >= 1990)%>%filter(Year <2000) %>%
              group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`)
              
Elections10 <- Elections %>%filter(Year >= 2000)%>%filter(Year <2010) %>%
  group_by(Country) %>% summarize(mean(Index))  %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`)

Elections15 <- Elections %>%filter(Year >= 2010)%>%filter(Year <2015) %>%
  group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`)

Elections20 <-  Elections %>%filter(Year >= 2015)%>%filter(Year <=2018)%>%
  group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`)

#ELECTIONS from 1990 till 2000

eurData <- Elections00

eurData <- eurData %>% mutate(`Populist index`= `Populist index` * 10)
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  


txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)

# inverse order (to have visible borders)

p1 <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.2f", `Populist index`)), data=txtVal, col="gray50",cex=3) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()

p1
#ELECTIONS from 2000 to 2010

eurData <- Elections10

eurData <- eurData %>% mutate(`Populist index`= `Populist index` * 10)
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  


txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)

# inverse order (to have visible borders)

p2 <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.2f", `Populist index`)), data=txtVal, col="gray50",cex=3) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()

p2
#ELECTIONS from 2010 to 2015

eurData <- Elections15

eurData <- eurData %>% mutate(`Populist index`= `Populist index` * 10)
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  


txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)

# inverse order (to have visible borders)

p3 <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.f", `Populist index`)), data=txtVal, col="gray50",cex=3) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()

p3
#ELECTIONS from 2015 to 2018

eurData <- Elections20

eurData <- eurData %>% mutate(`Populist index`= `Populist index` * 10)
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  


txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)

# inverse order (to have visible borders)
p4 <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.2f", `Populist index`)), data=txtVal, col="gray50",cex=3) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()
p4

#LET'S SEE IF THERE'S BEEN AN INCREASING IN THE VOTES OF RIGHT WING POPULISMS
#IN THE MAJOR EUROPEAN POPULIST PARTIES  

populisms <- Elections %>% filter(Weights >= 1.7) %>%
            group_by(Country, Year)

#Freedom Party of Austria

austria_party <- c("Freedom Party of Austria","FP.","Freedom Party of AustriaÊ(FP…)")
#does not recognize Freedom Party of AustriaÊ(FP…)

FP <- populisms %>% filter(Party %in% austria_party) 

FP1 <- populisms %>% filter(Country == "AT") %>% filter(Year == 2017)

FP <- rbind(FP,FP1)

#Northern League in Italy

league <- c("LEGA LOMBARDA - Northern (Lombardy) League (Lega Nord per l'Indipendenza della Padania)",
            "LN - North League (Lega Nord)","LN - Northern League (Lega Nord)","LN","League")
League <- populisms %>% filter(Party %in% league) 

View(populisms)

#FN in France

FN <- populisms %>% filter(Party == "FN")

#FIDESZ in Hungary

hungary_party <- c("FIDESZ","Fidesz Ð Hungarian Civic Union(Fidesz)", "FideszÊÐÊChristian Democratic People's Party")
FIDESZ <- populisms %>% filter(Party %in% hungary_party)

#UKIP

uk_party <- c("UKIP","UK Independence Party","UK Independence")
UKIP <- populisms %>% filter(Party %in% uk_party)


ggplot() + 
  geom_line(data = FP, aes(x =Year , y = Votes,color = "FP Austria") ,size =1) +
  geom_line(data = League, aes(x =Year , y = Votes,color ="League Italy"),size =1) +
  geom_line(data = FN, aes(x =Year , y = Votes, color = "FN France"),size =1) +
  geom_line(data = FIDESZ, aes(x =Year , y = Votes, color = "FIDESZ Hungary"),size =1) +
  geom_line(data = UKIP, aes(x =Year , y = Votes, color = "UKIP"),size =1) +
  labs(color="Legend text")+
  geom_vline(xintercept = 2015.5,linetype = 2)+
  geom_text(aes(x=2015.5, y=20,label="Brexit"), colour="dark red", angle=90, vjust = 1.2)
  

