---
# title: "Indian Premier League T20 Analysis"
# subtitle: "Data Science Professional Certificate Program from HarvardX - CYO"
# author: "Riaz Mohamed Vellamparambil"
# date: 02/21/2021
---

# Install all needed libraries 
  
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra" , repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if (!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")

# Load required libraries
library(tidyverse)
library(caret)
library(kableExtra)
library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(plotly)
library(ggthemes)
library(formattable)

ball_by_ball <- read.csv("https://raw.githubusercontent.com/riazvm/capstone-ipl/main/data/IPL-Ball-by-Ball-2008-2020.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
matches <- read.csv("https://raw.githubusercontent.com/riazvm/capstone-ipl/main/data/IPL-Matches-2008-2020.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# inspect column nammes in the datasets (Can use colnames or names)
colnames(ball_by_ball)
names(matches)

# Lets check how the data looks in the two data sets
head(matches)
head(ball_by_ball)

# Let us check the data set for any anamolies , eg. duplicates, spellings etc. For this we will use the Factor 
levels(as.factor(ball_by_ball$batting_team))
levels(as.factor(ball_by_ball$bowling_team))

# ball_by_ball data set

ball_by_ball$batting_team[ball_by_ball$batting_team == "Delhi Daredevils"] <- "Delhi Capitals"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Kings XI Punjab"] <- "Punjab Kings"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Gujarat Lions"] <- "Gujarat Titans"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Gujarat Lions"] <- "Gujarat Titans"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
ball_by_ball$batting_team[ball_by_ball$batting_team == "Pune Warriors"] <- "Lucknow Super Giants"

ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Delhi Daredevils"] <- "Delhi Capitals"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Kings XI Punjab"] <- "Punjab Kings"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Gujarat Lions"] <- "Gujarat Titans"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Gujarat Lions"] <- "Gujarat Titans"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
ball_by_ball$bowling_team[ball_by_ball$bowling_team == "Pune Warriors"] <- "Lucknow Super Giants"

# Check if we hav all the correct teams
levels(as.factor(ball_by_ball$batting_team))
levels(as.factor(ball_by_ball$bowling_team))

# Lets look at the matches dataset for the same anomalies
levels(as.factor(matches$venue))

# The only Anamoly in this set is "M Chinnaswamy Stadium" & "M.Chinnaswamy Stadium"   

matches$venue[matches$venue == "M Chinnaswamy Stadium"] <- "M. A. Chidambaram Stadium"
matches$venue[matches$venue == "M. Chinnaswamy Stadium"] <- "M. A. Chidambaram Stadium"
matches$venue[matches$venue == "MA Chidambaram Stadium, Chepauk"] <- "M. A. Chidambaram Stadium"

levels(as.factor(matches$venue))
# Contiue other columns
levels(as.factor(matches$team1))


matches$team1[matches$team1 == "Delhi Daredevils"] <- "Delhi Capitals"
matches$team1[matches$team1 == "Kings XI Punjab"] <- "Punjab Kings"
matches$team1[matches$team1 == "Gujarat Lions"] <- "Gujarat Titans"
matches$team1[matches$team1 == "Gujarat Lions"] <- "Gujarat Titans"
matches$team1[matches$team1 == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
matches$team1[matches$team1 == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
matches$team1[matches$team1 == "Pune Warriors"] <- "Lucknow Super Giants"

levels(as.factor(matches$team2))

matches$team2[matches$team2 == "Delhi Daredevils"] <- "Delhi Capitals"
matches$team2[matches$team2 == "Kings XI Punjab"] <- "Punjab Kings"
matches$team2[matches$team2 == "Gujarat Lions"] <- "Gujarat Titans"
matches$team2[matches$team2 == "Gujarat Lions"] <- "Gujarat Titans"
matches$team2[matches$team2 == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
matches$team2[matches$team2 == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
matches$team2[matches$team2 == "Pune Warriors"] <- "Lucknow Super Giants"

levels(as.factor(matches$toss_winner))

matches$toss_winner[matches$toss_winner == "Delhi Daredevils"] <- "Delhi Capitals"
matches$toss_winner[matches$toss_winner == "Kings XI Punjab"] <- "Punjab Kings"
matches$toss_winner[matches$toss_winner == "Gujarat Lions"] <- "Gujarat Titans"
matches$toss_winner[matches$toss_winner == "Gujarat Lions"] <- "Gujarat Titans"
matches$toss_winner[matches$toss_winner == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
matches$toss_winner[matches$toss_winner == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
matches$toss_winner[matches$toss_winner == "Pune Warriors"] <- "Lucknow Super Giants"

levels(as.factor(matches$winner))

matches$winner[matches$winner == "Delhi Daredevils"] <- "Delhi Capitals"
matches$winner[matches$winner == "Kings XI Punjab"] <- "Punjab Kings"
matches$winner[matches$winner == "Gujarat Lions"] <- "Gujarat Titans"
matches$winner[matches$winner == "Gujarat Lions"] <- "Gujarat Titans"
matches$winner[matches$winner == "Rising Pune Supergiant"] <- "Lucknow Super Giants"
matches$winner[matches$winner == "Rising Pune Supergiants"] <- "Lucknow Super Giants"
matches$winner[matches$winner == "Pune Warriors"] <- "Lucknow Super Giants"

levels(as.factor(matches$winner))

# Let us Check some unique sets

# Total players
n_distinct(ball_by_ball$player)

# Matches set 
matches %>% summarize(
  venue=n_distinct(matches$venue),
  teams=n_distinct(c(unique(matches$team1),unique(matches$team2))),
  matches_played=(tot_mat_played = n()),
  noresults=nrow(matches[is.na(matches$winner),])
)


# Total players, extras are the number of times that 
extras_data <- ball_by_ball %>% filter(ball_by_ball$extras_type %in% c("wides","noballs"))
nrow(extras_data)

# Create a Player role

delivery <- ball_by_ball %>%
  gather(playerRole, player, batsman:bowler) %>%
  mutate(playerRole=as.factor(playerRole))

delivery
# Visual Representation: Let us look at how the runs are distributed as percentage

delivery %>% filter(playerRole == "batsman") %>%
  group_by(runs = total_runs) %>%
  summarize(count = n()) %>%
  mutate(distribution = percent(count / sum(count)))  %>% 
  ggplot(aes(x = runs , y = distribution*100))+
  ggtitle("Runs Percentage distribution")+
  xlab("Runs")+
  ylab("Distribution (%)")+
  geom_bar(stat = "identity",  fill = "orange", color = "black")+
  theme(plot.title = element_text(hjust = 1.0))

# Visual Representation: Let us see number of times a batsman had scored a different run 

delivery %>% 
  filter(playerRole == "batsman") %>%
  group_by(run = batsman_runs) %>%
  summarize(count = n()) %>%
  mutate(percent = percent(count / sum(count))) %>% 
  ggplot(aes(x = run , y = count))+
  ggtitle("Run Type Vs Count")+
  xlab("Runs")+
  ylab("Count")+
  geom_bar(stat = "identity",  fill = "orange", color = "black")+
  theme(plot.title = element_text(hjust = 1.0))

# Major run types scored on each ball and correlation between them
runs_and_batsman <- delivery %>%
  group_by(id, inning) %>%
  mutate(ball_no = 1:n()) %>% 
  ungroup() %>% 
  filter(playerRole == "batsman") %>%
  filter(batsman_runs != "" & batsman_runs != "7" & ball_no %in% 1: 124) %>%
  group_by(ball_no, batsman_runs) %>% 
  summarize(count=n())

runs_and_batsman

# Visualization on how runs stack up over the innings

runs_and_batsman %>%
  ggplot(aes(ball_no,count, col=factor(ball_no))) +
  geom_col() +
  facet_wrap( ~ batsman_runs) +
  scale_y_log10()+
  theme(axis.text.x = element_text(
    angle = 90,
    size = 10,
    hjust = 1
  ),
  legend.position = "none")

# Correlation
cor(runs_and_batsman$batsman_runs, as.numeric(runs_and_batsman$ball_no))

# Run trends during the innings
delivery %>%
  group_by(id, inning) %>%
  mutate(ball_no = 1:n()) %>%
  ungroup() %>%
  filter (inning %in% 1:2 & playerRole == "batsman" & ball_no %in% 1:124) %>%
  group_by(ball_no=as.numeric(ball_no), runs = as.factor(batsman_runs)) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  arrange(ball_no) %>%
  top_n(5) %>%
  ggplot(aes(ball_no,count,col=runs, shape = runs)) +
  geom_point(size = 2)

# top 25 players by player_of_match
matches %>%
  group_by(mvp=player_of_match) %>%
  summarize(player_of_match= n()) %>%
  arrange(desc(player_of_match)) %>%
  head(n = 25)


# Top 25 basmen with boundaries (boundary is a 4 or a 6)
delivery %>%
  filter(playerRole == "batsman" & batsman_runs == 6 | batsman_runs == 4) %>%
  group_by(player, batsman_runs) %>%
  summarize(n = n()) %>%
  spread(batsman_runs, n) %>%
  rename(sixes = `6`,fours = `4`) %>%
  select(sixes, fours) %>%
  arrange(desc(sixes)) %>%
  head(25)

# Top 25 basmen 25 batsmen with the maximum strike rate (Runs scored vs deliveries faced)
delivery %>%
  filter(playerRole == "batsman") %>%
  group_by(player) %>%
  summarize(runs_scored=sum(batsman_runs), balls_faced=n(), strike_rate=sum(batsman_runs)/n()) %>%
  arrange(desc(strike_rate)) %>%
  head(25)

# Let us check the the distict set of dimissal types
levels(as.factor(delivery$dismissal_kind))

# Top 25 bowlers  with maximum number of wickets
delivery %>%
  filter(playerRole == "bowler" & 
           dismissal_kind != "" & dismissal_kind != "retired hurt" & dismissal_kind != "run out" & dismissal_kind != "run out"
  ) %>%
  group_by(player) %>%
  summarise(total_wickets=n())%>%
  arrange(desc(total_wickets)) %>%
  head(25)

# Top 25 bowlers  with maximum number of runs conceded
delivery %>%
  filter(playerRole == "bowler")  %>%
  group_by(player) %>%
  summarise(runsconceded=sum(batsman_runs), deliveries_bowled=n())%>%
  arrange(desc(runsconceded)) %>%
  head(25)

# Top 25 bowlers  with best economy rate
delivery %>%
  filter(playerRole == "bowler") %>%
  group_by(player) %>%
  summarize(runs_conceded=sum(batsman_runs), deliveries_bowled=n(), economy_rate=sum(batsman_runs)/n()) %>%
  arrange(economy_rate) %>%
  head(25)


# Top 25 bowlers  with best strike rate

t_wickets <- delivery %>%
  filter(playerRole == "bowler" & dismissal_kind != "" & dismissal_kind != "retired hurt" & dismissal_kind != "run out" & dismissal_kind != "run out") %>%
  group_by(player) %>%
  summarize(total_wickets = n())

top_max_deliveries <- delivery %>%
  filter(playerRole == "bowler") %>%
  group_by(player) %>%
  summarize(total_delivery = n()) %>%
  arrange(desc(total_delivery)) 


top_max_deliveries %>%
  full_join(t_wickets, by = "player") %>%
  mutate(strike_rate = total_delivery/ total_wickets) %>%
  arrange(strike_rate) %>%
  head(25)

# The total runs in the IPL
total_ipl_runs <- delivery %>%
  filter(playerRole == "batsman") %>%
  summarize(total_runs_ipl = sum(total_runs)) 
total_ipl_runs

# The total runs by the top 25 batsman in the IPL
total_runs_by_top_25 <-  delivery %>%
  filter(playerRole == "batsman") %>%
  group_by(player) %>%
  summarize(tot_runs_top_batsmman = sum(batsman_runs))%>%
  arrange(desc(tot_runs_top_batsmman)) %>%
  head(n = 25)

sum(total_runs_by_top_25$tot_runs_top_batsmman)

# What is the percentage that they contributed

percent(sum(total_runs_by_top_25$tot_runs_top_batsmman)/total_ipl_runs$total_runs_ipl)

# The total wickets in the IPL
total_ipl_wickets <- delivery %>%
  filter(playerRole == "bowler" & dismissal_kind != "") %>%
  summarize(overall_wickets = n())

total_ipl_wickets

# The wickets runs by the top 25 bowlers in the IPL
total_wickets_by_top_25 <- delivery %>%
  filter(playerRole == "bowler" & 
           dismissal_kind != "" & dismissal_kind != "retired hurt" & dismissal_kind != "run out" & dismissal_kind != "run out"
  ) %>%
  group_by(player) %>%
  summarise(total_wickets=n())%>%
  arrange(desc(total_wickets)) %>%
  head(25)

total_wickets_by_top_25


# What is the percentage that they contributed

percent(sum(total_wickets_by_top_25$total_wickets)/total_ipl_wickets)

# Visual Total runs scored by each team

delivery %>% filter(playerRole == "batsman") %>%
  group_by(team = batting_team) %>%
  summarize(teamruns = sum(total_runs)) %>%
  ggplot(aes(x = teamruns , y = team))+
  ggtitle("Total Runs Per Team")+
  xlab("Runs")+
  ylab("Team")+
  geom_bar(stat = "identity",  fill = "red", color = "black")+
  theme(plot.title = element_text(hjust = 1.0))


# Match Winners

matches %>% 
  group_by(team = winner) %>%
  filter(winner != "") %>%
  summarize(count = n()) %>%
  ggplot(aes(x = count , y = team))+
  ggtitle("Total Wins Per Team")+
  xlab("Number of Wins")+
  ylab("Team")+
  geom_bar(stat = "identity",  fill = "blue", color = "black")+
  theme(plot.title = element_text(hjust = 1.0))

# Which team won how many tosses
toss_winners <- matches %>%
  group_by(toss_winner) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename(team = toss_winner, number_of_tosses_won = count)
toss_winners

# Matches played by teams
team1_played <- matches %>%
  group_by(team1) %>%
  summarize(count1 = n()) %>%
  arrange(team1) %>%
  rename(team = team1)

team2_played <- matches %>%
  group_by(team2) %>%
  summarize(count2 = n()) %>%
  arrange(team2) %>%
  rename(team = team2)

matches_team <- team1_played %>%
  full_join(team2_played, by = "team") 

matches_team
# Number of wins by eacxh team
winners <- matches %>%
  group_by(winner) %>%
  filter(winner != "") %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename(team = winner, matches_won = count)
winners

# Number of losses by each team

team1_lost <- matches %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team1)) %>%
  group_by(team1) %>%
  summarize(count1 = n()) %>%
  arrange(team1) %>%
  rename(team = team1)
team2_lost <- matches %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team2)) %>%
  group_by(team2) %>%
  summarize(count2 = n()) %>%
  arrange(team2) %>%
  rename(team = team2)
losers <- team1_lost %>%
  full_join(team2_lost, by = "team") %>%
  mutate(matches_lost = count1 + count2) %>%
  select(-count1,-count2) %>%
  arrange(desc(matches_lost))
losers

# Team matches , wins and losses
teams <- matches_team %>%
  right_join(toss_winners, by = "team")%>%
  right_join(winners, by = "team") %>%
  right_join(losers, by = "team") %>%
  mutate(matches_played = matches_won + matches_lost) 

teams
# Visualization of matches playes vs won
teams %>%
  ggplot(aes(matches_played , matches_won, fill = team)) +
  geom_point(shape=21, size=5)

# Correlation between mamtches played to matches won
cor(teams$matches_played, teams$matches_won)


teams %>%
  ggplot(aes(number_of_tosses_won, matches_won, fill = team)) +
  geom_point(shape=22, size = 5)
cor(teams$matches_won, teams$number_of_tosses_won)

# Toss wins and toss decisions

toss_winners <- matches %>%
  group_by(toss_winner) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename(team = toss_winner, number_of_tosses_won = count)


toss_wins_results <- matches %>%
  filter(winner != "") %>%
  mutate(match_result = ifelse(toss_winner==winner, "win", "loss")) %>%
  group_by(toss_winner, toss_decision, match_result) %>%
  summarize(match_result_count = n()) 

toss_wins_results

# Visualization : Effect of toss wins vs decision to bat or field
toss_wins_results %>% 
  ggplot(aes(toss_winner, match_result_count, col = toss_decision, size = match_result)) +
  geom_point()  +
  ggtitle("Toss Decision Vs Winner")+
  xlab("Toss_Winner")+
  ylab("Matches")+
  theme(axis.text.x = element_text(
    angle = 50,
    size = 6,
    hjust = 1
  ),
  legend.position = "right")


# Venue effect on team wins
matches %>%
  filter(winner != "") %>%
  group_by(venue, winner) %>%
  summarize(count = n()) %>%
  ggplot(aes(venue, count, group = winner)) +
  geom_point() +
  ggtitle("Venu Vs Wins")+
  xlab("Venue")+
  ylab("Win Count")+
  geom_line(aes(col = winner), size = 1) +
  theme(axis.text.x = element_text(
    angle = 50,
    size = 6,
    hjust = 1
  ),
  legend.position = "right", legend.title.align=0)

# batsmen stats
batsmen_avgs <- delivery %>%
  filter(playerRole == "batsman") %>%
  group_by(player) %>%
  summarize(total_deliveries = n(), tot_runs = sum(batsman_runs)) %>%
  summarize (
    avg_deliveries = mean(total_deliveries),
    avg_runs = mean(tot_runs),
    median_deliveries = median(total_deliveries),
    median_runs = median(tot_runs)
  )
as_tibble(batsmen_avgs)


# bowler stats
bowler_avgs <- delivery %>%
  filter(playerRole == "bowler") %>%
  group_by(player) %>%
  summarize(total_deliveries = n(), tot_runs = sum(batsman_runs)) %>%
  summarize (
    avg_deliveries = mean(total_deliveries),
    avg_runs = mean(tot_runs),
    median_deliveries = median(total_deliveries),
    median_runs = median(tot_runs)
  )
as_tibble(bowler_avgs)


strike_rates_regularized <- delivery %>%
  filter(playerRole == "batsman") %>%
  group_by(player) %>%
  summarize(batsman_reg_str_rate = (sum(batsman_runs) + batsmen_avgs$median_runs) / (n() +
                                                                                       batsmen_avgs$median_deliveries)) %>%
  arrange(desc(batsman_reg_str_rate)) 

strike_rates_regularized %>%
  head(25)


economy_rates_regularized <- delivery %>%
  filter(playerRole == "bowler") %>%
  group_by(player) %>%
  summarize(bowler_reg_eco_rate = (sum(batsman_runs) + bowler_avgs$median_runs) / 
              (n() + bowler_avgs$median_deliveries)) %>%
  arrange(bowler_reg_eco_rate)
economy_rates_regularized %>%
  head(25)


# Top playes as per strike and economy (we have already seen this in a different format earlier)
top_players <- strike_rates_regularized %>%
  full_join(economy_rates_regularized, by = "player") %>%
  mutate(batsman_reg_str_rate = replace_na(
    batsman_reg_str_rate,
    batsmen_avgs$median_runs / batsmen_avgs$avg_deliveries
  )) %>%
  mutate(bowler_reg_eco_rate = replace_na(bowler_reg_eco_rate, bowler_avgs$avg_runs /
                                            bowler_avgs$median_deliveries)) %>%
  mutate(value_of_player = 100 * (batsman_reg_str_rate + 1 / bowler_reg_eco_rate)) 
top_players %>%
  arrange(desc(value_of_player)) %>%
  select(player, value_of_player) %>%
  mutate(rank = row_number()) %>%
  head(100) %>% 
  knitr::kable()

# Top 25 batsman and bowlers
top_batsmen <- strike_rates_regularized %>%
  head(25)
top_batsmen
top_bowlers <- economy_rates_regularized %>%
  head(25)
top_bowlers


# batsmen strike rate against top_bowlers bowlers
# we will use the original set

top_25_bat_strikerate<- ball_by_ball %>%
  filter(bowler %in% top_bowlers$player) %>%
  group_by(player = batsman) %>%
  summarize(strike_rate = (sum(batsman_runs) + batsmen_avgs$median_runs) / (n() +batsmen_avgs$avg_deliveries)) %>%
  arrange(desc(strike_rate))
top_25_bat_strikerate %>%
  mutate(rank = row_number())%>%
  head(25) 


# bowler ecorate rate against top_bowlers bowlers
# we will use the original set

top_25_bow_ecorate <- ball_by_ball %>%
  filter(batsman %in% top_batsmen$player) %>%
  group_by(player = bowler) %>%
  summarize(eco_rate = (sum(batsman_runs) + bowler_avgs$avg_runs) / (n() +bowler_avgs$median_deliveries)) %>%
  arrange(eco_rate)
top_25_bow_ecorate %>%
  mutate(rank = row_number())%>%
  head(25) 


  

# Top class players based on strike rates & economy rates against each other
top_class_players <- top_25_bat_strikerate %>%
  full_join(top_25_bow_ecorate, by = "player") %>%
  mutate(strike_rate = replace_na(strike_rate,
    batsmen_avgs$median_runs / batsmen_avgs$avg_deliveries
  )) %>%
  mutate(eco_rate = replace_na(eco_rate, bowler_avgs$avg_runs /
                                            bowler_avgs$median_deliveries)) %>%
  mutate(value_of_player = 100 * (strike_rate + 1 / eco_rate)) 

# top class player summary

top_class_players %>%
  arrange(desc(value_of_player)) %>%
  select(player, value_of_player, strike_rate,eco_rate) %>%
  mutate(rank = row_number()) %>%
  head(100) %>% 
  knitr::kable()


# Top pool players based on strike rate and economy for bowlers and their performance with each other
# we will again join the two sets and replace the strike rate and economy rate as previously done
world_top_players <- top_players %>%
  select(-value_of_player) %>%
  full_join(top_class_players, by = "player") %>%
  mutate(strike_rate = replace_na(strike_rate,
                                  batsmen_avgs$median_runs / batsmen_avgs$avg_deliveries
  )) %>%
  mutate(eco_rate = replace_na(eco_rate, bowler_avgs$avg_runs /
                                 bowler_avgs$median_deliveries)) %>%
  mutate(player_value = 100 * ((batsman_reg_str_rate + strike_rate) + 1 /
                                 (bowler_reg_eco_rate + eco_rate)))  %>%
  select(player, value_of_player) %>%
  arrange(desc(value_of_player)) %>%
  mutate(rank = row_number())
world_top_players %>%
  head(200)


# Model 2: Predict the winner of a Match

# Create a new dataset. We are going to be taking matches as our base data set to work out of
# Let us remove  Gujarat Titans and Kochi Tuskers Kerala because they  no more take part in the IPL
best_teams <- teams %>% arrange(desc(matches_played)) %>%
  head(8)
best_teams
match_predictor_ds <- matches %>%
  select(team1,team2, winner, venue, toss_winner, toss_decision) %>%
  filter(winner != ""  & team1 %in% best_teams$team & team2 %in% best_teams$team)
any(is.na(match_predictor_ds))
summary(match_predictor_ds)
match_predictor_ds

n_distinct(match_predictor_ds$team1)

levels(as.factor(match_predictor_ds$team1))

match_predictor_ds$team1[match_predictor_ds$team1 == "Mumbai Indians"] <- "Mumbai"
match_predictor_ds$team1[match_predictor_ds$team1 == "Delhi Capitals"] <- "DC"
match_predictor_ds$team1[match_predictor_ds$team1 == "Kolkata Knight Riders"] <- "KKR"
match_predictor_ds$team1[match_predictor_ds$team1 == "Royal Challengers Bangalore"] <- "RCB"
match_predictor_ds$team1[match_predictor_ds$team1 == "Punjab Kings"] <- "Punjab"
match_predictor_ds$team1[match_predictor_ds$team1 == "Chennai Super Kings"] <- "CSK"
match_predictor_ds$team1[match_predictor_ds$team1 == "Sunrisers Hyderabad"] <- "Hyd"
match_predictor_ds$team1[match_predictor_ds$team1 == "Rajasthan Royals"] <- "Rajasthan"


levels(as.factor(match_predictor_ds$team2))
match_predictor_ds$team2[match_predictor_ds$team2 == "Mumbai Indians"] <- "Mumbai"
match_predictor_ds$team2[match_predictor_ds$team2 == "Delhi Capitals"] <- "DC"
match_predictor_ds$team2[match_predictor_ds$team2 == "Kolkata Knight Riders"] <- "KKR"
match_predictor_ds$team2[match_predictor_ds$team2 == "Royal Challengers Bangalore"] <- "RCB"
match_predictor_ds$team2[match_predictor_ds$team2 == "Punjab Kings"] <- "Punjab"
match_predictor_ds$team2[match_predictor_ds$team2 == "Chennai Super Kings"] <- "CSK"
match_predictor_ds$team2[match_predictor_ds$team2 == "Sunrisers Hyderabad"] <- "Hyd"
match_predictor_ds$team2[match_predictor_ds$team2 == "Rajasthan Royals"] <- "Rajasthan"

levels(as.factor(match_predictor_ds$winner))

match_predictor_ds$winner[match_predictor_ds$winner == "Mumbai Indians"] <- "Mumbai"
match_predictor_ds$winner[match_predictor_ds$winner == "Delhi Capitals"] <- "DC"
match_predictor_ds$winner[match_predictor_ds$winner == "Kolkata Knight Riders"] <- "KKR"
match_predictor_ds$winner[match_predictor_ds$winner == "Royal Challengers Bangalore"] <- "RCB"
match_predictor_ds$winner[match_predictor_ds$winner == "Punjab Kings"] <- "Punjab"
match_predictor_ds$winner[match_predictor_ds$winner == "Chennai Super Kings"] <- "CSK"
match_predictor_ds$winner[match_predictor_ds$winner == "Sunrisers Hyderabad"] <- "Hyd"
match_predictor_ds$winner[match_predictor_ds$winner == "Rajasthan Royals"] <- "Rajasthan"


levels(as.factor(match_predictor_ds$toss_winner))
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Mumbai Indians"] <- "Mumbai"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Delhi Capitals"] <- "DC"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Kolkata Knight Riders"] <- "KKR"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Royal Challengers Bangalore"] <- "RCB"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Punjab Kings"] <- "Punjab"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Chennai Super Kings"] <- "CSK"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Sunrisers Hyderabad"] <- "Hyd"
match_predictor_ds$toss_winner[match_predictor_ds$toss_winner == "Rajasthan Royals"] <- "Rajasthan"

match_predictor_ds$team1
match_predictor_ds$team2
# Create a Train Set and a Test Set

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
options(digits=3)

test_index <- createDataPartition(y = match_predictor_ds$winner, times = 1, p = 0.1, list = FALSE)

# create a test & train set
train <- match_predictor_ds[-test_index,]
temp <- match_predictor_ds[test_index,]

# Matching team1,team2,venue,toss_decision and toss_winner in both train and test sets
test <- temp %>%
  semi_join(train, by = "team1") %>%
  semi_join(train, by = "team2") %>%
  semi_join(train, by = "venue") %>%
  semi_join(train, by = "toss_decision") %>%
  semi_join(train, by = "toss_winner") 
# Add rows removed from temp set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed) 
rm(removed, temp, test_index)

#Let us explore the train data set

train %>% as_tibble()

test %>% as_tibble()


#  Let us model 
# F score : In statistical analysis of binary classification, the F-score or F-measure is a measure of a test's 
# accuracy. It is calculated from the precision and recall of the test, where the precision is the number of 
# true positive results divided by the number of all positive results, including those not identified correctly, 
# and the recall is the number of true positive results divided by the number of all samples that should have 
# been identified as positive. Precision is also known as positive predictive value, and recall is also known 
# as sensitivity in diagnostic binary classification.

# We are not worried abut negative outcomes and hence going with an F score.
# F1 score: https://towardsdatascience.com/the-f1-score-bec2bbc38aa6 , we will use F1 score.

# Naive 

# Since all the variables in the  dataset are variables that deermine he winner we will calulcae the F1 score 
# for all classes
naive <- train(winner ~ ., method = "naive_bayes", data = train)
predicion_test_naive <- predict(naive, test)
f1_score_nb <- confusionMatrix(predicion_test_naive, as.factor(test$winner))$byClass[,"F1"]
f1_score_nb <- as.data.frame(t(f1_score_nb)) %>% mutate(averageF1Score = rowMeans(.))
# Table
# Make column names more readable
colnames(f1_score_nb) = gsub("Class: ", "", colnames(f1_score_nb))
# F1 table for different models
f1_model_table <- data.frame(Model = "Naive Bayes") %>% bind_cols(f1_score_nb)
f1_model_table %>% knitr::kable()


# Logistic Regression

logistic_regression <- train(winner ~ ., method = "lda", data = train)
predicion_test_lr <- predict(logistic_regression, test)
f1_score_lr <- confusionMatrix(predicion_test_lr, as.factor(test$winner))$byClass[,"F1"]
f1_score_lr <- as.data.frame(t(f1_score_lr)) %>% mutate(averageF1Score = rowMeans(.))
f1_score_lr

# Table
colnames(f1_score_lr) = gsub("Class: ", "", colnames(f1_score_lr))

f1_model_table <- bind_rows(f1_model_table,
                      data.frame(Model = "LDA") %>% bind_cols(f1_score_lr))
f1_model_table %>% knitr::kable()

# Random Forest (multiple trees)

trainctrl <- trainControl(method="cv")
random_forest <- train(winner ~ ., method = "rf", data = train,  trControl=trainctrl)
predicion_test_rf <- predict(random_forest, test)
f1_score_rf <- confusionMatrix(predicion_test_rf,  as.factor(test$winner))$byClass[,"F1"]
f1_score_rf <- as.data.frame(t(f1_score_rf)) %>% mutate(averageF1Score = rowMeans(.))
f1_score_rf

colnames(f1_score_rf) = gsub("Class: ", "", colnames(f1_score_rf))
f1_model_table <- bind_rows(f1_model_table,
                            data.frame(Model = "RF") %>% bind_cols(f1_score_rf))
f1_model_table %>% knitr::kable()


# KNN (Nearest neighbour)

knn <- train(winner ~ ., method = "knn", data = train,)
predicion_test_knn <- predict(knn, test)
f1_score_knn <- confusionMatrix(predicion_test_knn, as.factor(test$winner))$byClass[,"F1"]
f1_score_knn <- as.data.frame(t(f1_score_knn)) %>% mutate(averageF1Score = rowMeans(.))
f1_score_knn

# Table
colnames(f1_score_knn) = gsub("Class: ", "", colnames(f1_score_knn))
f1_model_table <- bind_rows(f1_model_table,
                      data.frame(Model = "KNN") %>% bind_cols(f1_score_knn))
f1_model_table %>% knitr::kable()

