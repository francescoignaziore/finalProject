#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(dplyr)
library(wordcloud)
library(tidytext)
library(knitr)
library(tidyverse)
library(maptools)
library(ggmap)
# install necessary packages
library(scales)
library(dplyr)
library(doBy)
# to make fortify possible without errors
library(gpclib)
gpclibPermit()
library(kableExtra)


Elections1 <- read.csv("Elections.csv", sep = ",")
Elections1 <- Elections1 %>% select(-X)

Elections00 <- Elections1 %>%filter(Year >= 1990)%>%filter(Year < 2000) %>%
  group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`) %>%
  mutate(Year_batch = 1)

Elections10 <- Elections1 %>%filter(Year >= 2000)%>%filter(Year < 2010) %>%
  group_by(Country) %>% summarize(mean(Index))  %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`) %>%
  mutate(Year_batch = 2)

Elections15 <- Elections1 %>%filter(Year >= 2010)%>%filter(Year < 2015) %>%
  group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`) %>%
  mutate(Year_batch = 3)

Elections20 <-  Elections1 %>%filter(Year >= 2015)%>%filter(Year <= 2018)%>%
  group_by(Country) %>% summarize(mean(Index)) %>%
  rename("CountryCode" = Country, "Populist index"= `mean(Index)`) %>%
  mutate(Year_batch = 4)


Elections_tot <- rbind(Elections00,Elections10,Elections15,Elections20)


#############################################################################################
#function to place text in a more centred position
middle = function (x) {
  (max(x) + min(x)) / 2
}

eurMap <- readShapeSpatial("NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp")

# only main adm level
eurMap <- subset(eurMap, nchar(as.character(NUTS_ID)) == 2)

# EL for Grece, UK for United Kingdom
eurMapDf <- fortify(eurMap, region='NUTS_ID')
eurData <- Elections_tot

# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly

eurMapDataDf1 <- eurMapDataDf %>% filter(Year_batch == 1)
eurMapDataDf1 <- eurMapDataDf1[order(eurMapDataDf1$order),]
# limit data to main Europe
eurMapDataDf1 <- subset(eurMapDataDf1, long > -15 & long < 32 & lat > 34 & lat < 75)
txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf1, FUN=mean, keep.names=T)

p1 <- ggplot(data=eurMapDataDf1) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.4f", `Populist index`)), data=txtVal, col="black",cex=3,fontface =2) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()+ theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank()) 
################################################################################################

eurMapDataDf2 <- eurMapDataDf %>% filter(Year_batch == 2)
eurMapDataDf2 <- eurMapDataDf2[order(eurMapDataDf2$order),]
eurMapDataDf2 <- subset(eurMapDataDf2, long > -15 & long < 32 & lat > 34 & lat < 75)

txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf2, FUN=middle, keep.names=T)

p2 <-   ggplot(data=eurMapDataDf2) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.4f", `Populist index`)), data=txtVal, col="black",cex=3,fontface =2) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()+ theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank()) 


#####################################################################################
eurMapDataDf3 <- eurMapDataDf %>% filter(Year_batch == 3)
eurMapDataDf3 <- eurMapDataDf3[order(eurMapDataDf3$order),]
eurMapDataDf3 <- subset(eurMapDataDf3, long > -15 & long < 32 & lat > 34 & lat < 75)

txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf3, FUN=middle, keep.names=T)


p3 <-   ggplot(data=eurMapDataDf3) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.4f", `Populist index`)), data=txtVal, col="black",cex=3,fontface =2) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()+ theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank()) 
########################################################################

eurMapDataDf4 <- eurMapDataDf %>% filter(Year_batch == 4)
eurMapDataDf4 <- eurMapDataDf4[order(eurMapDataDf4$order),]
eurMapDataDf4 <- subset(eurMapDataDf4, long > -15 & long < 32 & lat > 34 & lat < 75)

txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf4, FUN=middle, keep.names=T)

p4 <-  ggplot(data=eurMapDataDf4) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%.4f", `Populist index`)), data=txtVal, col="black",cex=3,fontface =2) +
  scale_fill_gradient2(low = "navy", mid = "white", high = "red4") +
  coord_equal()+ theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank()) 


##################################################################

populisms <- Elections1 %>% filter(Weights >= 1.7) %>%
  group_by(Country, Year)

#Freedom Party of Austria

austria_party <- c("Freedom Party of Austria","FP.","Freedom Party of AustriaÊ(FP…)")
#does not recognize Freedom Party of AustriaÊ(FP…)

FP <- populisms %>% filter(Party %in% austria_party) 

FP1 <- populisms %>% filter(Country == "AT") %>% filter(Year == 2017)

FP <- rbind(FP,FP1)

FP <- FP %>% mutate(Party = "FP")

#Northern League in Italy

league <- c("LEGA LOMBARDA - Northern (Lombardy) League (Lega Nord per l'Indipendenza della Padania)",
            "LN - North League (Lega Nord)","LN - Northern League (Lega Nord)","LN","League")
League <- populisms %>% filter(Party %in% league) %>% mutate(Party = "League")


#FN in France

FN <- populisms %>% filter(Party == "FN") %>% mutate(Party = "FN")

#FIDESZ in Hungary

hungary_party <- c("FIDESZ","Fidesz Ð Hungarian Civic Union(Fidesz)", "FideszÊÐÊChristian Democratic People's Party")
FIDESZ <- populisms %>% filter(Party %in% hungary_party)  %>% mutate(Party = "FIDESZ")

#UKIP

uk_party <- c("UKIP","UK Independence Party","UK Independence")
UKIP <- populisms %>% filter(Party %in% uk_party)  %>% mutate(Party = "UKIP")

partiess <- rbind(League,FN,FIDESZ,UKIP,FP)

################################################################################

#Facebook Analysis

Salvini <- read.csv2("Salvini_feed.csv")

Salvini <- Salvini %>% select(-X)


S <- Salvini %>% separate(created_time, into= c("date", "hour"), sep = "T")
S1 <- S %>% separate(hour, into = c("time","plus"), sep = "[+]")


S1$date <- as.Date(S1$date)

elec_date <- c("2018/03/04")
elec_date <-as.Date(elec_date)

S1 <- as.tibble(S1)



S2 <- S1 %>% mutate(from_el = abs(difftime(date, elec_date, units = "days")))

############ regression model lin-lin

S3 <- S2 %>% filter(from_el < 100) %>%group_by(from_el)
S3 <- as.data.frame(S3)
S3$from_el <- as.numeric(S3$from_el)
####Log-lin model

S4 <- S2 %>% filter(from_el < 100) %>%group_by(from_el) %>% mutate(likes_count = log(likes_count))
S4 <- as.data.frame(S4)
S4from_el <- as.numeric(S4$from_el)
###ANALYSIS TAKING THE MAXIMUM LIKED POST FOR EACH DAY


S5 <- S2 %>% filter(from_el < 100) %>%group_by(from_el) %>% summarize(max(likes_count)) %>%
  rename("likes_count" = `max(likes_count)`)
S5 <- as.data.frame(S5)
S5$from_el <- as.numeric(S5$from_el)


#################################### immigrations number


immigrants_n <- read.csv("immigrants_perception_numbers.csv", sep = ";")
immigrants_n <- immigrants_n %>% select(Code,X..of.valid) %>% rename("Value"= X..of.valid)

immigrants_p <- immigrants_n %>% mutate(Value = Value * 10)

#we create observations in order to get a histogram
f <- function(a,b){
  vector1 = 0
  for(i in 1:length(a)){
    
    vector = rep(a[i], b[i])
    
    
    vector1 = c(vector,vector1)
    
    
  }
  
  return(vector1)
  
}



########### SURVEY ON EUROPEAN UNION

eu_survey <- read.csv("europe_survey.csv", sep = ";")

eu_survey <- as.data.frame(t(eu_survey))
countries <- rownames(eu_survey)
eu_survey <- cbind(countries, eu_survey)
eu_survey <- as.tibble(eu_survey)
rownames(eu_survey) <- NULL

colnames <- c("Country","Things are going in the right direction",
              "Things are going in the wrong direction",
              "Neither the one nor the other","delete","Not specified")

colnames(eu_survey) <- colnames
eu_survey <- eu_survey %>% filter(Country != "Column1") %>% select(-delete)


eu_survey1 <- eu_survey %>% gather(`Things are going in the right direction`,
                                   `Things are going in the wrong direction`,
                                   `Neither the one nor the other`,`Not specified`, key = "status", value = "percentage" )

eu_survey1$percentage <- as.numeric(eu_survey1$percentage)


#translation dataframe

dtranslation <- data.frame("Italian"= c("profughi","primagliitaliani","andiamoagovernare")
                 , "English" =c("immigrants","italiansfirst","letsgorule"),
                 "italian"= c("4marzovotolega","stopinvasione","facciamosquadra")
                 , "english" =c("I'mvotingLegaonMarch 4th","stopinvasion","let'ssticktogether"))



server <- function(input, output, session) {
  
  
  
  
  output$distPlot <- renderPlot({
    
    
    if(input$dist == "p1"){
      
      print(p1)
      
    }
    if(input$dist == "p2"){
      
      print(p2)
      
    }
    
    if(input$dist == "p3"){
      
      print(p3)
      
    }
    
    
    if(input$dist == "p4"){
      print(p4)
      
      
    }
    
    
    
    
  })
  
  
  output$parties <- renderPlot({
    
    ggplot(data = partiess[partiess$Party %in% input$party,], aes(x = Year, y = Votes, group = Party, color = Party)) +
      geom_line(size = 2) + labs(color = "Legend text") + 
      geom_vline(xintercept = 2016.5,linetype = 2,alpha = 0)
    #geom_text(aes(x=2016.5, y=20,label="Brexit"), colour="dark red", angle=90, vjust = 1.2)
    
  })
  
  
  val <-  eventReactive(input$act, {
    # reactive function to define the configulation
    
    
    
    dates <- as.Date(input$DatesMerge)
    
    
    if(dates > elec_date){
      ggplot(data = S1[S1$date > dates,], aes(x = date,y =likes_count)) + 
        geom_line(colour ="lightblue2", size = 3,alpha = 0.7)+
        geom_point(colour = "dodgerblue4")+ ylab("Likes") +xlab("Date")
      
      
    }
    
    else{
      ggplot(data = S1[S1$date > dates,], aes(x = date,y =likes_count)) + 
        geom_line(colour ="lightblue2", size = 3,alpha = 0.7)+
        geom_point(colour = "dodgerblue4")+ 
        geom_vline(aes(xintercept=as.numeric(elec_date)
        ), color="red", size = 1.5, alpha = 0.2)+
        geom_text( aes(x=elec_date, y=100000, label="Elections day"), colour="dark red", angle=90, vjust = 1.2
        ) + ylab("Likes") +xlab("Date")
      
    }
    
    
    
  })  
  
  
  val2 <- eventReactive(input$act, {
    
    
    
    
    
    S1$date <- as.Date(S1$date)
    
    
    S1 <- as.tibble(S1)
    
    S1sel <- S1 %>% filter( date > input$DatesMerge)
    
    S1sel$message <- as.character(S1sel$message)
    text <- S1sel$message
    line<-c(1:length(text))
    
    
    stop_words_it <- read.csv2("stop_words_italian.csv")
    
    
    colnames(stop_words_it) <- "word"
    
    stop_words_it$word <- as.character(stop_words_it$word)
    
    stop_words_it <- as.tibble(stop_words_it)
    text_df <- cbind(line,text)
    
    text_df <- as.tibble(text_df)
    
    
    text_df <- text_df %>%
      unnest_tokens(word, text)
    
    text_df <- text_df[nchar(text_df$word)>3, ]
    
    text_df <- text_df %>% anti_join(stop_words_it)
    
    text_df_1 <- gsub(" ?0\\w+ ?", "", text_df$word)
    
    text_df_1 <-  as.tibble(text_df_1)
    
    
    text_df_1 <- text_df_1 %>%  filter(value != "") %>% 
      filter(value != "perché") %>%
      filter(value != "sarà") 
    
    count <- count(text_df_1, value, sort = TRUE)
    
    
    wordcloud(words = count$value, freq = count$n, min.freq = input$frequence,
              max.words=200, random.order=FALSE,use.r.layout=FALSE,
              colors=c("firebrick","firebrick3","firebrick4","darkred"))
    
    
    
  })
  
  


  output$likes <- renderPlot({
    
    
    
    val()
    
    
    
    
    
    
  })
  
  
  
  
  output$regression <- renderPlot({
    
    if(input$reg == "S3" ){
      
      
      reg_data = S3
    }
    
    if(input$reg == "S4" ){
      
      
      reg_data = S4
    }
    
    if(input$reg == "S5" ){
      
      
      reg_data = S5
    }
    
    
    ggplot(data = reg_data, aes(x = from_el, y= likes_count )) + geom_point(colour = "dodgerblue4") + 
      geom_line(stat="smooth",method = "lm",
                size = 2,
                color = "red3",
                alpha = 0.6)+
      xlab("Days from election") +
      ylab("Likes")   
  })
  
  
  output$word <- renderPlot({
    
    val2()
    
    
    
    
  })
  
  output$immigration <- renderPlot({
    
    if(input$immigr == "immigrants_analysis_1.csv" ){
      
      init = "Take jobs aways \n 0"
      final = "Create new jobs \n 10"
      title = "Survey on the relationship between immigrants and jobs"
    }
    
    if(input$immigr == "immigrants_analysis_2.csv" ){
      
      init = "Take out more \n 0"
      final = "Put in more \n 10"
      title = "Survey on whether immigrants take out more than putting in"
      
    }
    
    
    if(input$immigr == "immigrants_analysis_3.csv" ){
      
      init = "Make it worse \n 0"
      final = "Make it better \n 10"
      title = "Survey on whether immigrants make crime problems worse or better"
      
    }
    
    
    
    
    immigrants <- read.csv(input$immigr, sep = ";")
    immigrants <- immigrants %>% filter(Code < 11) %>% mutate(Values = Code)
    immigrants$Code <- as.factor(immigrants$Code)
    
    ggplot(immigrants, aes(x = Code, y=`X..of.valid`,fill = Values ))+
      geom_bar(stat = "identity") + xlab("Values of the survey") +
      ylab("Votes") + scale_fill_gradient2(low="red4",mid = "lightsteelblue2", high="navy", 
                                           midpoint=5, guide = FALSE) +
      ggtitle(title) +
      scale_x_discrete(labels=c("0" = init, "1" = "",
                                "2" = "", "3" = "","4" = "","5" = "5","6" = "",
                                "7" = "","8" = "","9" = "","10" = final))
    
    
    
    
    
    
  })
  
  
  output$immigr_n <- renderPlot ({
    
  
    
    immigrants_p <- f(immigrants_p$Code, immigrants_p$Value)
    
    
    immigrants_p <- immigrants_p[immigrants_p < input$focus]
    
    immigrants_p <- as.data.frame(immigrants_p)
    
    ggplot(immigrants_p, aes(x = immigrants_p)) + geom_histogram(bins = input$imm_bins,fill = "orangered4") +
      xlab("Number of expected immigrants per 100 people") + ylab("Count of people that partecipated in the survey \n (Out of 1000)")
    
    
    
    
    
  })
  
  output$mytable = DT::renderDataTable({
    dtranslation
  })
  
  
  output$real <- renderPlot({
    
    
    imm <- data.frame( Type = c("Non-citizens", "citizens"),
                       Value = c(4.2,93.8))
    
    
    ggplot(imm, aes(x = 1, y = Value, fill = Type))+ geom_bar(stat = "identity") +
      coord_polar(theta = "y") + theme(axis.line=element_blank(),
                                       axis.text.y=element_blank(),
                                       axis.title.x=element_blank(),
                                       axis.title.y=element_blank()) +
      scale_fill_manual(values=c("deepskyblue4","orangered4"))
    
  })
  
  
  output$topics <- renderPlot({
    
    eu_survey_gen <- eu_survey1 %>% filter(Country == input$Country)
    eu_survey_gen$status <- factor(eu_survey_gen$status, levels = eu_survey_gen$status[order(eu_survey_gen$percentage)])
    
    
    ggplot(eu_survey_gen, aes(x = status, y = percentage, fill = status))+
      geom_bar(stat = "identity") + scale_fill_manual(values=c("ivory4","lightsteelblue1","lightskyblue1","indianred")) +
      coord_flip() + guides(fill = FALSE) + 
      scale_x_discrete(labels=c(                         
        "Neither one \n nor the other","Not specified",
        "Things are going \n in the right direction",
        "Things are going \n in the wrong direction")) + 
      theme(axis.title.y=element_blank()) + 
      ylab("Votes")
    
    
    
    
    
    
    
  })
  
  
  
  
}

