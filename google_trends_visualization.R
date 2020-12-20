library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(knitr)

search_1 <- read.csv('google_trends_data/search_terms.csv')
search_2 <- read.csv('google_trends_data/search_terms2.csv')
search_3 <- read.csv('google_trends_data/search_terms3.csv')
search_4 <- read.csv('google_trends_data/search_terms4.csv')
search_5 <- read.csv('google_trends_data/search_terms5.csv')
search_6 <- read.csv('google_trends_data/search_terms6.csv')

df <- merge(merge(merge(merge(merge(search_1, search_2, by = 'Week'), search_3, by = 'Week'), search_4, by = 'Week'), search_5, by = 'Week'), search_6, by = 'Week')

changelessthan1 <- function(x){
  if(is.factor(x)) return(factor(x, levels=c(levels(x), "0.5")))
  return(x)
}

df <- as.data.frame(lapply(df, changelessthan1))

df[df == '<1'] <- '0.5'
df[df == '0'] <- '0.1'
df['Week'] <- as.Date(df[['Week']], "%m/%d/%Y")

df <- df %>% mutate_if(is.factor,as.character)
df <- df %>% mutate_if(is.character, as.numeric)
colnames(df) <- sub("...United.States.", "", colnames(df))

df <- df[ , -which(names(df) %in% c("Asymptomatic","Yemen", "How.to.cut.mens.hair.at.home", "Stimulus.checks", "Dalgona.coffee", "Zoom", "WAP", "Naya.Rivera", "election.results", "Among.Us"))]

df <- melt(df, id = "Week")

#df[,-1] <- sweep(df[,-1],2,colSums(df[,-1]),`/`)
df$variable <- factor(df$variable, levels = c(  "Vaccine",    "Joe.Biden",    "Shooting.of.Breonna.Taylor", "Ruth.Bader.Ginsburg", "Chadwick.Boseman",  "Beirut",  "NBA", "Black.Lives.Matter", "George.Floyd", "Where.is.my.stimulus.money", "Tiger.King","Stock.market", "Zoom.Video.Communications", "coronavirus", "Unemployment","Parasite", "Kobe.Bryant", "Bushfires.in.Australia","TikTok" ), order = TRUE)
df$value <- as.numeric(df$value)
df[is.na(df)] <- 0 

ggplot(df, aes(x = Week, y = variable, height = value, fill = variable)) +
  geom_density_ridges(show.legend = FALSE, stat = "identity", scale = 5) +
  scale_fill_manual(values = rep(c("#DB4437", "#4285F4", "#F4B400", "#0F9D58"), 6)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),plot.title = element_text(hjust = 0.5, face = "bold", size=25), plot.subtitle = element_text(hjust = 0.5), axis.text=element_text(size=18, vjust = 0, hjust = 0), axis.ticks = element_blank(), axis.title=element_blank()) +
  scale_y_discrete(labels = rev(c("TikTok", "Bushfires in Australia", "Kobe Bryant" ,"Parasite", "Unemployment", "Coronavirus", "Zoom Video Communications", "Stock market","Tiger King", "Where is my stimulus money", "George Floyd", "Black Lives Matter", "NBA", "Beruit", "Chadwick Boseman", "Ruth Bader Ginsburg", "Shooting of Breonna Taylor", "Joe Biden", "Vaccine")))+
  labs(title = "2020: What Mattered Most and When", subtitle = "A Ridgeline Plot of Popular Google Searches in the United States", caption = "based on data from Google Trends")
