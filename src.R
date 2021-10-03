## Packages Installation

package.install("dplyr")
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
library(plotpy)
installed.packages()
install.packages("plotly")
library(plotly)
install.packages("maps")
library(maps)
install.packages("GGally")
library(ggcorrplot)
library(ggcorrplot)
install.packages("corrplot")
library(ggcorrplot)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("infer")
library(infer)

### File  Read
fifa_21 <- read.csv("player_21.csv")
fifa_21 <- read.csv("D:\College\R_project\player_21.csv")
fifa_21 <- read.csv("players_21.csv")
fifa_21 <- read.csv("players_21.csv", na.strings = c("", "NA"))
fifa_21[,2]
head(fifa_21[,2])
fifa_21 <- fifa_21[,-2]
fifa_21 <- fifa_21[, c(-22, -23)]
fifa_21 <- fifa_21[,-23]
fifa_21 <- fifa_21[,-25]
fifa_21 <- fifa_21[,-41]

levels(factor(fifa_21$club_name))
length(factor(fifa_21$club_name)))
length(factor(fifa_21$club_name))
fifa_21 %>% group_by(club_name== "Barcelona")
levels(factor(fifa_21$overall))
levels(factor(fifa_21$potential))
head(strsplit(fifa_21$player_positions, split = ','))
levels(factor(strsplit(fifa_21$player_positions, split = ',')))
levels(factor(strsplit(fifa_21$player_positions, split = ",")))
levels(factor(strsplit(fifa_21$player_positions, split = ",")))
head(fifa_21, c(41:45))
head(fifa_21[, c(41:45)])
fifa_21[, c(41:45)] %>% colnames() %>% strsplit(.,split = '_')
## Erasing some data
fifa_21 <- fifa_21 %>% select(-gk_diving, -gk_handling, -gk_kicking, -gk_reflexes, -gk_speed, -gk_positioning)
fifa_21 <- fifa_21 %>% select(-defending_marking)
fifa_21 <- fifa_21 %>% select(-sofifa_id)


# Nationality Wise Selection

France <- subset(fifa_21, fifa_21$nationality = "France")
France <- subset(fifa_21, fifa_21$nationality == "France")
India <- subset(fifa_21, fifa_21$nationality == "India")
Portugal <- subset(fifa_21, fifa_21$nationality == "Portugal")
Brazil <- subset(fifa_21, fifa_21$nationality == "Brazil")
Spain <- subset(fifa_21, fifa_21$nationality == "Spain")
Germany <- subset(fifa_21, fifa_21$nationality == "Germany")
Argentina <- subset(fifa_21, fifa_21$nationality == "Argentina")
Barcelona <- subset(fifa_21, fifa_21$club_name == "Barcelona")
Juventus <- subset(fifa_21, fifa_21$club_name == "Juventus")
view(Juventus)
View(Juventus)

La_liga <- subset(fifa_21, fifa_21$league_name == "Spain Primera Division")
ligue <- subset(fifa_21, fifa_21$league_name == "French Ligue 1")
EPL <- subset(fifa_21, fifa_21$league_name == "English Premier League")

df <- subset(fifa_21, fifa_21$league_name == "Spain Primera Division"|fifa_21$league_name == "Italian Serie A"
            |fifa_21$league_name=="German 1. Bundesliga"|fifa_21$league_name == "French Ligue 1"
            |fifa_21$league_name=="English Premier League"|fifa_21$league_name=="Holland Eredivisie")

# Distribution of Average age player on basis of league_name

summ <- df %>%
    group_by(league_name) %>%
    summarise(Age = mean(age))
View(summ)


options(repr.plot.width = 12, repr.plot.height = 8)

# Plotting Graph on Avverage Age Players in Leagues

ggplot()+
    geom_histogram(df, mapping = aes(age, fill = league_name))+
    geom_vline(summ, mapping = aes(xintercept = Age), color = "red", size = 1.5)+
    geom_text(summ, mapping = aes(x = Age + 3, y = 65, label = round(Age, digits = 2)))+
    facet_wrap(league_name~.)+
    theme_minimal()+
    theme(legend.position = "bottom")+
    labs(y = "Frequency", title = "Distribution & The Average Age of The Players in each League", caption = "@EA Sports - FIFA 21")


options(repr.plot.width = 12, repr.plot.height = 8)

world_map <- map_data("world")

num_player <- world_map %>% 
    mutate(region = as.character(region)) %>% 
    left_join((fifa_21 %>% mutate(nationality = as.character(nationality),
    nationality = if_else(nationality %in% "England", "UK", nationality))%>%
    count(nationality, name = "No. of Players")%>%rename(region = nationality)%>%
    mutate(region = as.character(region))), by = "region")

# Plotting Graph of No. of Players

ggplot(num_player, aes(long, lat, group = group))+
    geom_polygon(aes(fill = factor('No. of Players')), color = "red", show.legend = F)+
    scale_fill_viridis_d(option = "D")+
    theme_void()+
    labs(fill = "No. of Players", title = "Players from Countries")


options(repr.plot.width = 12, repr.plot.height = 8)

# Players from Brazil

Brazil %>% 
    select(short_name, overall, potential) %>% 
    arrange(-overall) %>% head(15) %>% 
    gather(variable, Exp, -short_name)%>% 
    ggplot(aes(short_name, Exp, fill = variable))+
    geom_col(position = "dodge")+geom_text(aes(label = Exp), position = position_dodge(width = 0.9), vjust = -0.5)+ 
    scale_fill_manual(values, c("#009c3b", "#ffdf00"))+theme_minimal()+
    theme(legend.position = "bottom"+labs(fill = NULL, x = NULL, title = "Brazil")+
    theme(axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5, hjust = 1)))


# Barcelona Players

Barcelona %>% select(short_name, overall, potential) %>%
    arrange(-overall) %>% head(15) %>% gather(variable, Exp, -short_name)%>%
    ggplot(aes(short_name, Exp, fill = variable))+geom_col(position = "dodge")+
    geom_text(aes(label = Exp), position = position_dodge(width = 0.9), vjust = -0.5)+
    scale_fill_manual(values = c("grey", "blue"))+theme_minimal()+theme(legend.position = "bottom")+
    labs(fill = NULL, x = NULL, title = "Barcelona")+
    theme(axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5, hjust = 1))


# Argentina Players

Argentina %>% select(short_name, overall, potential) %>%
    arrange(-overall) %>% head(15) %>% gather(variable, Exp, -short_name)%>%
    ggplot(aes(short_name, Exp, fill = variable))+geom_col(position = "dodge")+
    geom_text(aes(label = Exp), position = position_dodge(width = 0.9), vjust = -0.5)+
    scale_fill_manual(values = c("grey", "blue"))+theme_minimal()+theme(legend.position = "bottom")+
    labs(fill = NULL, x = NULL, title = "Argentina")+
    theme(axis.text.x = element_text(face = "bold", angle = 90, vjust = 0.5, hjust = 1))

# Messi & Ronald

Messi_Ronald <- rbind(fifa_21[1,], fifa_21[2, ])

player<-  Messi_Ronald %>% mutate(Name= paste0(short_name,",", club_name))%>% select(Name,pace:mentality_composure)%>%
    gather(Skill,Exp,pace:'mentality_composure',-Name)

options(repr.plot.width = 12, repr.plot.height = 8)


ggplot(player, aes(Skill, Exp, fill = Name))+geom_col(position = "fill")+
    coord_flip()+scale_fill_manual(values = c("red", "green"))+
    theme_minimal()+geom_hline(yintercept = 0.5, color = "orange", size = 0.5, linetype = 2)+
    theme(legend.position = "top", axix.text.y = element_text(face = "bold"), axix.text.x = element_blank())+labs(title = "Ronaldo vs Messi")

View(Messi_Ronald)

# Foreign Players

La_liga_Native <- La_liga %>% 
    mutate(Nationality = as.character(nationality), Nationality = if_else(nationality %in% "Spain", "Native", "Foreginer"))

ggplot(La_liga_Native)+
    geom_bar(aes(x = Nationality, fill = Nationality)show.legend = F)+
    facet_wrap(club_name~.)+labs(title = "La Liga Native and Foreigner Player")



Bundesliga <- subset(fifa_21,fifa_21$league_name=="German 1. Bundesliga")
germany_native <- Bundesliga %>% 
    mutate(Nationality = as.character(nationality), Nationality = if_else(nationality %in% "Germany", "Native", "Foreginer"))

ggplot(germany_native)+
    geom_bar(aes(x = Nationality, fill = Nationality), show.legend = F)+
    facet_wrap(club_name~.)+labs(title = "Germany Native and Foreigner Player")

#Distribution of Players in the whole Fifa Dataset

fifa_21$player_positionsdat <- sub("\\,.*","",fifa_21$player_positions)

options(repr.plot.width = 15,repr.plot.height = 8)

fifa_21 %>% 
    drop_na(player_positionsdat)%>%ggplot()+
    geom_bar(aes(x = player_positionsdat, fill = player_positionsdat), show.legend = F)+
    labs(title = "Player position Distribution in the World")

# Distribution in Some Leagues

fifa_21$player_positionsdat <- sub("\\,.*","",fifa_21$player_positions)


options(repr.plot.width = 15,repr.plot.height = 8)

df$player_positionsb <- sub("\\,.*","",df$player_positions)

options(repr.plot.width = 15,repr.plot.height = 8)

df%>% drop_na(player_positionsb)%>%
    ggplot()+geom_bar(aes(y = reorder(player_positionsb, player_positionsb, function(x) tapply(x, x, length)), fill = player_positionsb), show.legend = F)+
    labs(title = "League Wise player distribution")+xlab("Count")+ylab("Positions")


#Positional players
#attacking = ST,CF
#midfielder= CAM,CDM,CM,LM,RM
#winger= LW, RW
#full back= LB,LWB,RB,RWB
#defender= CB
#GoalKeeper= GK

pow_check <- function(x){
        if_else(x %in% c("CAM", "CDM", "CM", "LM", "RM"), "Midfielder", 
        if_else(x %in% c("LB", "LWB", "RWB", "RB"), "Full Back", 
        if_else(x %in% c("LW", "RW"), "Winger", if_else(x %in% "CB", "Defender", 
        if_else(x %in% c("ST", "CF")"Forward", "Goal Keeper")))))
}


fifa_pos <- fifa_21 %>% mutate(POs = as.character(player_positionsdat), Pos = Pos_check(Pos))


dfpos <- df%>%
         mutate(Pos = as.character(player_positionsb), 
         Pos = pow_check(Pos))

subset(subset(fpos, Pos == "Forward"), overall >= 85)[,1]

subset(subset(fifa_pos, Pos == "Forward"), overall >= 85)[,1]

# Top 20 Forwards in the World

subset(fifa_pos, Pos == "Forward")%>% 
    arrange(desc(overall))%>%head(20)%>% ggplot(aes(x = overall, y = reorder(short_name, overall)))+
    geom_col(aes(fill = short_name), show.legend = F)+
    labs(x = "overall", y = "Name", title = "Top 20 Forwards in the World")

# Top 20 Winger in the World

subset(fifa_pos, Pos == "Winger")%>% 
    arrange(desc(overall))%>%head(20)%>% ggplot(aes(x = overall, y = reorder(short_name, overall)))+
    geom_col(aes(fill = short_name), show.legend = F)+
    labs(x = "overall", y = "Name", title = "Top 20 Winger in the World")

# Top 20 Defenders in the World

subset(fifa_pos,Pos=="Defender") %>% arrange(desc(overall))%>%head(20)%>%
ggplot(aes(x=overall,y=reorder(short_name,overall)))+
    geom_col(aes(fill=short_name),show.legend = F)+
    labs(x="Overall",y="Name",title = "Top 20 Defenders in the World")

# Top 20 Goal Keepers in the World

subset(fifa_pos,Pos=="Goal Keeper") %>% arrange(desc(overall))%>%head(20)%>%
ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
labs(x="Overall",y="Name",title = "Top 20 Goal Keepers in the World")


# Top 20 powerful clubs with their postion class

powerful_club <- fifa_pos%>%group_by(club_name)%>%summarise(mean = mean(overall))%>%arrange(-mean)%>%head(20)

fifa_pos %>% 
    group_by(club_name, Pos) %>% summarise(mean = mean(overall))%>%ungroup()%>%
    filter(club_name %in% powerful_club$club_name)%>%ggplot(aes(reorder(club_name, mean), mean, fill = Pos))+
    geom_col(position = "fill")+geom_text(aes(label = round(mean, digits = 2)), position = position_fill(0.5), size = 3.5)+
    coord_flip()+theme_minimal()+theme(legend.position = "top", axis.text.y = element_text(face = "bold"), axis.title.x = element_blank())+
    labs(x = "", y = "", title = "Top 20 powerful clubs with their postion class")


subset(fifa_21, team_jersey_no == 10)%>% arrange(desc((overall+potential)/2))%>%select(short_name, club_name, player_positionsdat)%>%head(20)
numcol <- cbind(fifa_21$age, fifa_21$height_cm, fifa_21$weight_kg, fifa_21$overall, fifa_21$value_eur, fifa_21$wage_eur, fifa_21$release_clause_eur)
colnames(numcol) <- c("Age", "height", "Weight", "Ovr", "Valuation", "Wage", "Release_Clause")
numcol <- as.data.frame(numcol)
cor(numcol)
ggcorrplot(cor(numcol)))

ggplot(df,aes(overall,wage_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()

ggplot(df,aes(age,wage_eur))+
  geom_hex(bins=60)+
  facet_wrap(league_name~.,scales = "free")+
  scale_fill_viridis_c()+
  geom_smooth(method = "loess")+
  theme_minimal()


subset(dfpos, player_positionsdat == "GK")%>%arrange(-value_eur)%>%head(1)%>%select(short_name)
subset(dfpos, player_positionsb == "GK")%>%arrange(-value_eur)%>%head(1)%>%select(short_name)
subset(dfpos, player_positionsb == "CB")%>%arrange(-value_eur)%>%head(2)%>%select(short_name)
subset(dfpos,player_positionsb=="RW") %>% arrange(-value_eur) %>% head(1) %>% select(short_name)

s2 <- data.frame(x=c(0, 0, 16.5, 100, 100,83.5),
                xend=c(16.5,16.5, 16.5, 83.5,83.5,83.5),
                y=rep(c(13.68, 61.32, 13.68),2),
                yend=rep(c(13.68,61.32,61.32),2))

sp <- data.frame(x=c(0,16.5,16.5,25,25,50,50,75,75,87.5),
                y=c(37.5,13.68,61.32,0,75,18.75,56.25,0,75,37.5),
                name=c("J. Oblak"," V. van Dijk","A. Laporte"
                        "T. Alexander-Arnold","A. Robertson",
                        "T. Kroos","F. de Jong",
                        "M. Salah","Neymar Jr",
                        "K. Mbappé"))
View(sp)

# Most Expensive team possible in Fifa 21

pgraph<- ggplot(dfpos)+
    xlim(0,100)+ylim(0,75)+
    geom_vline(xintercept = c(0,50,100), color="white") +
    geom_segment(data = d2,aes(x=x, xend=xend, y=y,yend=yend), color="white") +
    geom_point(aes(x=50,y=75/2), size=2, color="white") +
    geom_point(data=pp,aes(x=x,y=y), size=7, color="orange")+
    geom_text(data=pp,aes(x=x,y=y,label = name),size=5)+
    theme(panel.background = element_rect(fill = "darkgreen"),
    panel.grid = element_line(colour = "darkgreen"))+
    labs(title = "Most Expensive team possible in Fifa 21",subtitle = "With most recent player valuation")+
    xlab("")+ylab("")
