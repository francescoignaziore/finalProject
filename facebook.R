library(devtools)
library(tidyverse)
install_github("pablobarbera/Rfacebook/Rfacebook", force = TRUE)

library(ggplot2)
library(scales)
library(plotly)
library(tidytext)

library(wordcloud)
install.packages("Rfacebook") # from CRAN

library(Rfacebook)
install_github("pablobarbera/Rfacebook/Rfacebook") # from GitHub


# 
# token <- 'EAACEdEose0cBAOFuvN21V5LaiatJpT0MIu5U2aoZATku4kFM5XX3yIBephqoDN7f1AXP4qtYvxxQynCvzpbqjCjzTdFpHk9aNKRLLnJsA8YIiIzc6sMKoZA5tTN07nBj6TWjjTRqau0PfeJREia1iX7CCb4RsDqAVaOrHHqkFBkV93r3HL0ObuuJSqDL8ZD'
# page <- getPage("salviniofficial", token, n = 1600)
# View(page)

#write.csv2(page, "Salvini_feed.csv")

Salvini <- read.csv2("Salvini_feed.csv")

Salvini <- Salvini %>% select(-X)


S <- Salvini %>% separate(created_time, into= c("date", "hour"), sep = "T")
S1 <- S %>% separate(hour, into = c("time","plus"), sep = "[+]")


S1$date <- as.Date(S1$date)


#df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)

#df <- do.call(rbind, df.list)


dates <- c("2018/03/07")
dates <-as.Date(dates)



#  ggplot() + 
#   geom_line(data = S1, aes(x = date,y =likes_count), colour ="lightblue2", size = 3,alpha = 0.7)+
#   geom_point(data = S1, aes(x = date, y = likes_count),colour = "dodgerblue4") + ylab("Likes") +
#    xlab("Date") +  geom_vline(data = S1, aes(xintercept=as.numeric(dates)
# ), color="red", size = 1.5, alpha = 0.2)+
#    geom_text(data=S1, aes(x=dates, y=150000, label="Elections day"), colour="dark red", angle=90, vjust = 1.2,
#               inherit.aes=FALSE)
 
 ggplot(data = S1, aes(x = date,y =likes_count)) + 
   geom_line(colour ="lightblue2", size = 3,alpha = 0.7)+
   geom_point(colour = "dodgerblue4")+ 
   geom_vline(aes(xintercept=as.numeric(dates)
   ), color="red", size = 1.5, alpha = 0.2)+
   geom_text( aes(x=dates, y=150000, label="Elections day"), colour="dark red", angle=90, vjust = 1.2
             ) + ylab("Likes") +xlab("Date")

 
############ regression model lin-lin
election_day <- dates
S2 <- S1 %>% mutate(from_el = abs(difftime(date, election_day, units = "days")))


S3 <- S2 %>% filter(from_el < 100) %>%group_by(from_el)
S3 <- as.data.frame(S3)
S3$from_el <- as.numeric(S3$from_el)


ggplot(data = S3, aes(x = from_el, y= likes_count )) + geom_point(colour = "dodgerblue4") + 
  geom_line(stat="smooth",method = "lm",
            size = 2,
            color = "red3",
            alpha = 0.6)+
  xlab("Days from election") +
  ylab("Likes")

S4 <- S2 %>% filter(from_el < 100) %>%group_by(from_el) %>% mutate(likes_count = log(likes_count))
S4 <- as.data.frame(S4)
S4from_el <- as.numeric(S4$from_el)

####Log-lin model
ggplot(data = S4, aes(x = from_el, y= likes_count )) + geom_point(colour = "dodgerblue4") + 
  geom_line(stat="smooth",method = "lm",
            size = 2,
            color = "red3",
            alpha = 0.6)+
  xlab("Days from election") +
  ylab("Likes")

###ANALYSIS TAKING THE MAXIMUM LIKED POST FOR EACH DAY

S2 <- S2 %>% filter(from_el < 100) %>%group_by(from_el) %>% summarize(max(likes_count))
S2 <- as.data.frame(S2)
S2$from_el <- as.numeric(S2$from_el)
ggplot(data = S2, aes(x = from_el, y= `max(likes_count)` )) + geom_point(colour = "dodgerblue4") + 
  geom_line(stat="smooth",method = "lm",
            size = 2,
            color = "red3",
            alpha = 0.6)+
  xlab("Days from election") +
  ylab("Likes")


linearMod <- lm(from_el ~ `max(likes_count)`, data=S2)

summary(linearMod)
## convert Facebook date format to R date format

summary(linearMod)$r.squared

pf(summary(linearMod)$fstatistic[1], summary(linearMod)$fstatistic[2],
   summary(linearMod)$fstatistic[3], lower.tail = FALSE)
#  
# Salvini <- Salvini %>% select(-X)
# format.facebook.date <- function(datestring) {
#   date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
# }
# # Salvinimonth <- format(Salvini$datetime, "%Y-%m")
# 
# Salvini$datetime <- format.facebook.date(Salvini$created_time)

Salvini <- read.csv2("Salvini_feed.csv")
 

## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(Salvini[[paste0(metric, "_count")]], list(month = Salvini$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# create data frame with average metric counts per month

df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# visualize evolution in metric

ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric), size = 2) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", 
                                                                                  breaks = c(10, 100, 1000, 10000, 50000)) + theme_bw() + theme(axis.title.x = element_blank())



#WORDCLOUD

Salvini <- read.csv2("Salvini_feed.csv")

Salvini <- Salvini %>% select(-X)

S <- Salvini %>% separate(created_time, into= c("date", "hour"), sep = "T")
S1 <- S %>% separate(hour, into = c("time","plus"), sep = "[+]")
S1$date <- as.Date(S1$date)




dates <- c("2018/03/07")
dates <-as.Date(dates)


S1 <- as.tibble(S1)

S1$message <- as.character(S1$message)
text <- S1$message
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


wordcloud(words = count$value, freq = count$n, min.freq = 20,
          max.words=200, random.order=FALSE,use.r.layout=FALSE,
          colors=c("firebrick","firebrick3","firebrick4","darkred"))

###### immigrant analysis

  immigrants <- read.csv("immigrants_analysis_1.csv", sep = ";")
  immigrants <- immigrants %>% filter(Code < 11) %>% mutate(Values = Code)
  immigrants$Code <- as.factor(immigrants$Code)
  
  ggplot(immigrants, aes(x = Code, y=`X..of.valid`,fill = Values ))+
    geom_bar(stat = "identity") + xlab("Values of the survey") +
    ylab("Votes") + scale_fill_gradient2(low="red4",mid = "lightsteelblue2", high="navy", 
                                        midpoint=5) +
    ggtitle("Survey on the relationship between immigrants and jobs") +
    scale_x_discrete(labels=c("0" = "Take jobs aways \n 0", "1" = "",
                              "2" = "", "3" = "","4" = "","5" = "5","6" = "",
                              "7" = "","8" = "","9" = "","10" = "Create new jobs \n 10"))
  
  
  
  immigrants2 <- read.csv("immigrants_analysis_2.csv", sep = ";")
  immigrants2 <- immigrants2 %>% filter(Code < 11) %>% mutate(Values = Code)
  immigrants2$Code <- as.factor(immigrants2$Code)
  
  ggplot(immigrants2, aes(x = Code, y=`X..of.valid`,fill = Values ))+
    geom_bar(stat = "identity") + xlab("Values of the survey") +
    ylab("Votes") + scale_fill_gradient2(low="red4",mid = "lightsteelblue2", high="navy", 
                                         midpoint=5) +
    ggtitle("Survey on whether immigrants take out more than putting in") +
    scale_x_discrete(labels=c("0" = "Take out more \n 0", "1" = "",
                              "2" = "", "3" = "","4" = "","5" = "5","6" = "",
                              "7" = "","8" = "","9" = "","10" = "Put in more \n 10"))
  
    
  
  immigrants3 <- read.csv("immigrants_analysis_3.csv", sep = ";")
  immigrants3 <- immigrants3 %>% mutate(Values = Code)
  immigrants3$Code <- as.factor(immigrants1$Code)
  
  ggplot(immigrants3, aes(x = Code, y=`X..of.valid`,fill = Values ))+
    geom_bar(stat = "identity") + xlab("Values of the survey") +
    ylab("Votes") + scale_fill_gradient2(low="red4",mid = "lightsteelblue2", high="navy", 
                                         midpoint=5) +
    ggtitle("Survey on whether immigrants make crime problems worse or better") +
    scale_x_discrete(labels=c("0" = "Make it worse \n 0", "1" = "",
                              "2" = "", "3" = "","4" = "","5" = "5","6" = "",
                              "7" = "","8" = "","9" = "","10" = "Make it better \n 10"))

  
  
  immigrants_n <- read.csv("immigrants_perception_numbers.csv", sep = ";")
  immigrants_n <- immigrants_n %>% select(Code,X..of.valid) %>% rename("Value"= X..of.valid)
  
  immigrants_p <- immigrants_n %>% mutate(Value = Value * 10)
  
  
  immigrants_p$Code

  f <- function(a,b){
    vector1 = 0
    for(i in 1:length(a)){
      
      vector = rep(a[i], b[i])
      
      
      vector1 = c(vector,vector1)
      
      
    }
    
    return(vector1)
    
    
    
    
  }
  
  
  immigrants_p <- f(immigrants_p$Code, immigrants_p$Value)
  
  immigrants_p <- as.data.frame(immigrants_p)
  ggplot(immigrants_p, aes(x = immigrants_p)) + geom_histogram(bins = 10,fill = "orangered4") +
  xlab("Number of expected immigrants per 100 people") + ylab("Count of people that partecipated in the survey \n (Out of 1000)")
  
  
#http://ec.europa.eu/eurostat/statistics-explained/index.php/Migration_and_migrant_population_statistics#Migrant_population
#Actual migrant population is 4.2%
  
imm <- data.frame( Type = c("Non-citizens", "citizens"),
                   Value = c(4.2,93.8))
  

ggplot(imm, aes(x = 1, y = Value, fill = Type))+ geom_bar(stat = "identity") +
  coord_polar(theta = "y") + theme(axis.line=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.title.x=element_blank(),
                                             axis.title.y=element_blank()) +
  scale_fill_manual(values=c("lightskyblue","orangered4"))


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
eu_survey_gen <- eu_survey1 %>% filter(Country == "UE28")
eu_survey_gen$status <- factor(eu_survey_gen$status, levels = eu_survey_gen$status[order(eu_survey_gen$percentage)])


ggplot(eu_survey_gen, aes(x = status, y = percentage, fill = status))+
 geom_bar(stat = "identity") + scale_fill_manual(values=c("ivory4","lightsteelblue1","lightskyblue1","indianred")) +
coord_flip() + guides(fill = FALSE) + 
  scale_x_discrete(labels=c(                         
                             "Neither one \n nor the other","Not specified",
                              "Things are going \n in the right direction",
                              "Things are going \n in the wrong direction")) + 
  theme(axis.title.y=element_blank()) + 
  ylab("Votes") + ggtitle("Survey on feelings towards the European Union")





