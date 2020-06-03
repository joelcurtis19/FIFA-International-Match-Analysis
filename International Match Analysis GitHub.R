### Load Section ###

#load packages
library(tidyverse)
library(lubridate)
library(scales)

#read in paths
int_read_path <- "results.csv" #all international matches path

#reading in all the data
int_result <- read_csv(int_read_path) #reading in all international matches

#normalize column names
names(int_result) <- make.names(colnames(int_result))


### data wrangling section ###

#clean international match data
int_result <- int_result %>%
  mutate(year = year(date), #get the year from date col
         abs_goal_diff = abs(home_score - away_score),#absolute goal difference
         total_goals = home_score + away_score) #total goals in a given match

#create df with each match listed twice
#enables individual team calculations
int_dbl_1 <- int_result %>%
  mutate(team_1 = home_team, team_2 = away_team,#create team_1 and team_2 cols
         t1_score = home_score, t2_score = away_score)%>% #assign scores to t1 and t2
  select(date, team_1, team_2, t1_score, t2_score, tournament, #select only necessary cols
         city, country, neutral, year, abs_goal_diff, total_goals)

#does the same as int_dbl_1 but switches around team 1 and team 2
int_dbl_2 <- int_result %>%
  mutate(team_2 = home_team, team_1 = away_team, t2_score = home_score, t1_score = away_score)%>%
  select(date, team_1, team_2, t1_score, t2_score, tournament, city, country, neutral, year, abs_goal_diff, total_goals)

#bind rows of int_dbl_1 and int_dbl_2
#this creates a df with each match listed twice
#each listing of the match has a different team listed as "team_1"
int_dbl <- bind_rows(int_dbl_1, int_dbl_2)
int_dbl <- int_dbl%>%
  mutate(year_t1 = paste(year, team_1, sep = "_"),#create unique id for the team and year
         team_1_gd = t1_score - t2_score, #goal difference for team 1
         result = ifelse(t1_score > t2_score, "W", #result is "W" if t1 wins
                         ifelse(t1_score < t2_score, "L", "D")))#result is "L" if t1 loses
#result is "D" if the match results in a draw

#creates a df calculating the number of teams with matches in a given year
#only team 1 will be considered
n_teams <- int_dbl%>%
  mutate(year_team = year_t1, team = team_1)%>% #t1 becomes the only team
  select(year_team, year, team)%>% #selection of cols
  distinct() #removes duplicated year and team combo's

#create df with yearly totals of goals and goal difference
int_total_yearly <- int_dbl %>%
  group_by(year, team_1)%>% #group by year and t1
  summarise(sum_gf = sum(t1_score), sum_ga = sum(t2_score), 
            mean_gf = mean(t1_score), mean_ga = mean(t2_score),
            median_gf = median(t1_score), median_ga = median(t2_score), 
            max_gf = max(t1_score), max_ga = max(t2_score),
            sd_gf = sd(t1_score), sd_ga = sd(t2_score), 
            sum_gd = sum(team_1_gd), mean_gd = mean(team_1_gd),
            median_gd = median(team_1_gd))#calculations for yearly totals


### PLOTTING SECTION ###

# Mean Goals per International Game plot
int_goal_plot <- int_result%>%
  group_by(year)%>%
  summarise(avg_goals = mean(total_goals),avg_diff=mean(abs_goal_diff))%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = avg_goals),color="red")+
  geom_smooth(mapping = aes(x = year, y = avg_goals),color="red",method="lm", se = FALSE)+ 
  geom_point(mapping = aes(x = year, y = avg_diff),color="blue")+ 
  geom_smooth(mapping = aes(x = year, y = avg_diff),color="blue",method="lm", se = FALSE)+
  ylab("Goals")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  ggtitle("Mean Goals per International Game")

int_goal_plot #display plot

# Number of International Games per Year plot
int_ngame_plot <- int_result%>%
  group_by(year)%>%
  tally()%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = n),color="red")+
  geom_smooth(mapping = aes(x = year, y = n),color="red", method = "loess", se = FALSE)+
  ylab("Games")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  scale_y_continuous(breaks = pretty_breaks(n = 5))+
  ggtitle("Number of International Games per Year")

int_ngame_plot #display plot

# Number of International Teams with at Least 1 Game Played plot
int_nteams_plot <- n_teams%>%
  group_by(year)%>%
  tally()%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = n),color="red")+
  geom_smooth(mapping = aes(x = year, y = n),color="red", method = "loess", se = FALSE)+
  ylab("Teams")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  scale_y_continuous(breaks = pretty_breaks(n = 5))+
  ggtitle("Number of International Teams with at Least 1 Game Played")

int_nteams_plot #display plot


# Mean Goal Differential by Team by Year plot
int_meangd_plot <- int_total_yearly%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = mean_gd),color="red")+
  ylab("Mean Goal Differential")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  scale_y_continuous(breaks = pretty_breaks(n = 5))+
  ggtitle("Mean Goal Differential by Team by Year")

int_meangd_plot #display plot


# Total Goal Differential by Team by Year plot
int_gd_plot <- int_total_yearly%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = sum_gd),color="red")+
  ylab("Goal Differential")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  scale_y_continuous(breaks = pretty_breaks(n = 5))+
  ggtitle("Total Goal Differential by Team by Year")

int_gd_plot #display plot


# Median Goal Differential by Team by Year plot
int_mediangd_plot <- int_total_yearly%>%
  ggplot()+
  geom_point(mapping = aes(x = year, y = median_gd),color="red")+
  ylab("Median Goal Differential")+
  scale_x_continuous(breaks = pretty_breaks(n = 15))+
  scale_y_continuous(breaks = pretty_breaks(n = 5))+
  ggtitle("Median Goal Differential by Team by Year")

int_mediangd_plot #display plot

