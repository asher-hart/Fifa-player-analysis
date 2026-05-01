library(readr)
data <- read_csv("fifa_players.csv")

library(ggplot2)

elite_players <- data$overall >= 85
percent_elite <- sum(elite_players, na.rm = TRUE) / length(data$overall) * 100
percent_elite

#About 0.5% of players have an overall rating of 85 or higher. Since ratings
#above 85 are considered elite in FIFA, this shows that only a small percentage
#of players reach top-tier status.

avg_wage <- mean(data$wage_eur, na.rm = TRUE)
avg_wage

low_players <- data$overall < 83
high_players <- data$overall >= 83

avg_wage_low <- mean(data$wage_eur[low_players], na.rm = TRUE)
avg_wage_high <- mean(data$wage_eur[high_players], na.rm = TRUE)

avg_wage_low
avg_wage_high
avg_wage_high / avg_wage_low

#The average wage for players with an overall rating below 83 is about 7,758
#euros, while the average wage for players rated 83 or higher is about 129,783
#euros. Higher-rated players make about 17x what lower-rated players make on
#average. This shows that higher-rated players earn significantly more than
#lower-rated players.

young_talent <- data$age <= 21 & data$potential >= 83
num_young_talent <- sum(young_talent, na.rm = TRUE)
num_young_talent

#There are 264 of the total 19,178 players in this data set that are 21 or
#younger with a potential of at least 83. These players are considered elite
#prospects and likely future stars.

growth <- data$potential - data$overall
avg_growth <- mean(growth, na.rm = TRUE)
avg_growth

#The average growth is 5.3 overall. This shows how much players can improve. A
#higher value means many players still have significant development potential.

ggplot(data, aes(x= overall)) +
  geom_histogram() +
  labs(title="Distribution of Players Overall",
       x="Overall Ratings",
       y="Number of Players")

#This histogram shows how overall ratings are distributed. Most players are
#clustered in the mid-range (around 65–70),while fewer players reach
#very high ratings.

ggplot(data, aes(x = overall, y = wage_eur)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title="Overall vs Wage with Regression Line",
       x="Overall Rating",
       y="Wage")

#This plot shows the relationship between age and performance. players usually 
#reach their "prime" by their late 20s and then gradually decline after.

ggplot(data, aes(y= potential)) +
  geom_boxplot() +
  labs(title="Distribution of Player Potential",
       y="Potential Rating")

#This box plot shows how players’ potential ratings are distributed.The median
#represents a typical potential, and the whiskers show the range of possible
#development. Outliers indicate exceptional players who could reach high
#levels or players who wont be good enough to get to the median.

ggplot(data, aes(x= age)) +
  geom_bar() +
  labs(title="Number of Player by Age",
       x="Age",
       y="Number of Players")

#This bar chart counts how many players are at each age. Most players are 
#in their 20s, as usual with all professional sports.

cor(data$overall, data$wage_eur, use = "complete.obs")
cor(data$overall, data$wage_eur, use = "complete.obs", method = "spearman")


ggplot(data, aes(x = overall, y = age)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title="Overall vs Age",
       x="Overall Rating",
       y="Age")