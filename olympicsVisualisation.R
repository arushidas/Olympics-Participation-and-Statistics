#loading required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)

#loading the dataset
sum <- read.csv("datasets/summer.csv")
View(sum)

win <- read.csv("datasets/winter.csv")
View(win)

dict <- read.csv("datasets/dictionary.csv")
View(dict)

###########################################################

names(sum)[6] <- 'Code'
View(sum)
s <- left_join(sum, dict, by = c('Code'='Code'))
View(s)

names(win)[6] <- 'Code'
View(win)
w <- left_join(win, dict, by = c('Code'='Code'))
View(w)

m <- group_by(s, Country) 
m <- summarise(m, length(Country))
names(m)[2] <- 'NoOfMedals'
m <- m %>% mutate(Country=recode(str_trim(Country),
                    "United States"="USA","United Kingdom"="UK"))

#world heat map plotting
world_map <- map_data("world")
map <- left_join(world_map,m, by = c("region"="Country"))
ggplotly(ggplot(map,aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=NoOfMedals),colour="white", size=0.3)+
  scale_fill_viridis_c(na.value="#cccccc",direction=-1)+
  theme_void()+
  labs(fill="Total Medals in \nSummer Olympics",
       title="World map coloured by Total Medals in Summer Olympics",
       caption="Data source: Kaggle"))


#Viz-2
######################################################################
#Gender participation percentage : Summer
ps <- group_by(s,Year, Gender)
ps <- summarise(ps, length(Gender))
ps <- na.omit(ps)
names(ps)[3] <- 'Freq'

#male particpation in summer olympics
smCity <- c("Seoul 1988", "Atlanta 1996", "Athens 2004", "London 2012")
smPer <- c(65.40,58.20,55.00,52.56)
sMen <- data.frame(smCity,smPer)

#female particpation in summer olympics
swCity <- c("Seoul 1988", "Atlanta 1996", "Athens 2004", "London 2012")
swPer <- c(34.60,41.80,45.00,47.43)
sWomen <- data.frame(swCity,swPer)

#Gender participation percentage : Winter
pw <- group_by(w,Year, Gender)
pw <- summarise(pw, length(Gender))
pw <- na.omit(pw)
names(pw)[3] <- 'Freq'

#male particpation in winter olympics
wmCity <- c("Calgary 1988", "Nagano 1998", "Turin 2006", "Sochi 2014")
wmPer <- c(76.13,57.71,56.30,55.55)
wMen <- data.frame(wmCity,wmPer)

#female particpation in winter olympics
wwCity <- c("Calgary 1988", "Nagano 1998", "Turin 2006", "Sochi 2014")
wwPer <- c(23.86,42.28,43.69,44.45)
wWomen <- data.frame(wwCity,wwPer)

sMen$neu <- c(1,2,3,4)

#men - summer
ggplot(sMen, aes(x=reorder(smCity,-neu), y=smPer,fill=smCity)) + 
  ggtitle("Men's Participation in Summer Olympics") +
  geom_text(data = sMen, hjust = 1.05, size = 3.25,
            aes(x = smCity, y = 0, 
                label = paste(smCity," - ", smPer, "%", sep=""))) +
  geom_bar(width = 0.75, stat="identity") + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") + 
  ylim(c(0,100)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank()) 


#women - summer
ggplot(sWomen, aes(x = swCity, y = swPer,fill = swCity)) + 
  ggtitle("Women's Participation in Summer Olympics") +
  geom_text(data = sWomen, hjust = 1.05, size = 3.25,
            aes(x = swCity, y = 0, 
                label = paste(swCity," - ", swPer, "%", sep=""))) +
  geom_bar(width = 0.75, stat="identity") + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


#men - winter
ggplot(wMen, aes(x = wmCity, y = wmPer,fill = wmCity)) + 
  ggtitle("Men's Participation in Winter Olympics") +
  geom_text(data = wMen, hjust = 1.05, size = 3.25,
            aes(x = wmCity, y = 0, 
                label = paste(wmCity," - ", wmPer, "%", sep=""))) +
  geom_bar(width = 0.75, stat="identity") + 
  coord_polar(theta = "y",start=0,direction = 1) +
  xlab("") + ylab("") +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


ggplot(wMen,aes(x=fct_reorder(wmCity,wmPer, .desc = FALSE),wmPer)) + 
  geom_bar(stat="identity",fill =rainbow(4),alpha=0.5) + 
  coord_polar("y", start=0,direction = 1)+
  ylim(0,25) + xlab("Different manufacterers") +ylab("# of different cereals")




#women - winter
ggplot(sWomen, aes(x = wwCity, y = wwPer,fill = wwCity)) + 
  ggtitle("Women's Participation in Winter Olympics") +
  geom_text(data = wWomen, hjust = 1.05, size = 3.25,
            aes(x = wwCity, y = 0, 
                label = paste(wwCity," - ", wwPer, "%", sep=""))) +
  geom_bar(width = 0.75, stat="identity") + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


#Viz-3
#######################################################################

Diff <- c(0,91,183,144,40,30,-3,18,47,69,9,2,24,11,11,8,14,6,70,80,14,18,-7,17,3,37,14)
hostC <- unique(s$City)
app <- c("Sydney","Athens","Beijing","London")
hostC <- append(hostC,"Los Angeles", after = 19)
hostC <- append(hostC,app, after=length(hostC))
year <- unique(s$Year)
hfa <- data.frame(year,hostC,Diff)
hfa <- hfa[-1, ]

class(Diff)
ggplotly(ggplot(hfa,mapping=aes(year,Diff)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#32a6a8") +
        ggtitle("Difference between No. of Medals won in Away and Home Game")+
        theme(plot.title=element_text(color="#281c63", size=12, face="bold"))+
        scale_x_continuous(breaks=seq(from=1900,to=2020,by=8))+
        scale_y_continuous(breaks=seq(from=-20,to=200,by=20))+
        xlab("Years")+
        ylab("Difference between Away and Home Game"))


#Viz-4
####################################################################
sp <- count(s,Sport)
names(sp)[2] <- 'sFreq'
sp <- sp %>% mutate(perc=round(sFreq*100/sum(sFreq),1))
sp <- mutate(sp, pGroup = ifelse(perc<3.5, sum(perc<3.5), perc), 
                  Sport = ifelse(perc<3.5,"Others <3.5%",Sport))
sp <- select(sp,Sport,pGroup)
sp <- unique(sp)
sp <- sp %>% arrange(desc(Sport)) %>% mutate(y_pos = cumsum(pGroup)-0.5*pGroup)
ggplot(sp,aes(x=2,pGroup, fill=Sport))+ 
  geom_bar(stat="identity",color="white", alpha =.5) + 
  ggtitle("Popularity of Various Olympic Sports") +
  theme(plot.title=element_text(color="#281c63", size=14, face="bold"))+
  coord_polar(theta = "y", start=0)+
  geom_text(aes(y=y_pos,label=paste0(pGroup,"%")), color="black",size=2.7)+
  scale_fill_manual(values = rainbow(33)) +
  theme_void()+
  theme(plot.title=element_text(color="#281c63", size=14, face="bold"))+
  xlim(0.6, 2.5)
####################################################################

aqua <- filter(s,Sport=="Aquatics")
aqua <- count(aqua,Discipline)
names(aqua)[2] <- 'aqFreq'
aqua <- mutate(aqua,perc=round(aqFreq*100/sum(aqFreq),1))
aqua <- aqua %>% arrange(desc(Discipline)) %>% mutate(y_pos=cumsum(perc)-0.5*perc)
ggplot(aqua,aes(x=2,perc, fill=Discipline))+ 
  geom_bar(stat="identity",color="white", alpha =.5) + 
  ggtitle("Popularity of Aquatic Sports") +
  coord_polar(theta="y",start=0)+
  geom_text(aes(y=y_pos,label=paste0(perc,"%")), color="black",size=2.7)+
  theme_void()+
  scale_fill_manual(values = rainbow(23)) +
  theme(plot.title=element_text(color="#281c63", size=14, face="bold"))+
  xlim(0.6, 2.5)
####################################################################




