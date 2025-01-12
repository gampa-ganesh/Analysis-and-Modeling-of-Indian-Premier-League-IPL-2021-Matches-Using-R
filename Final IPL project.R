# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
ipl_data <- read_csv("D:/581/ipl final/new ipl/indian premium leuage/ALL_2021_IPL_MATCHES_BALL_BY_BALL.csv")

# View the first few rows of the dataset
head(ipl_data)

# Get a summary of the dataset
summary(ipl_data)

# Inspect the structure of the dataset
str(ipl_data)

# Remove a column named 'Dates'
ipl_data <- ipl_data %>%
  select(-dates)

#To check if the the coloumn is deleted or not
colnames(ipl_data)

library(dplyr)
library(tidyr)

# Split the 'Match_Played' column into 'teams' and 'match_date'
ipl_data <- ipl_data %>%
  separate(col = match_name, 
           into = c("match_played", "match_date"), 
           sep = "\\s+(?=[0-9]{4}-[0-9]{2}-[0-9]{2}$)", 
           remove = TRUE, 
           convert = FALSE, 
           extra = "merge", 
           fill = "right")
# View the updated data frame to confirm changes
glimpse(ipl_data)

library(lubridate)

# Convert 'match_date' to date format
ipl_data <- ipl_data %>%
  mutate(match_date = ymd(match_date))

# Check the converted 'match_date'
glimpse(ipl_data)

# Function to capitalize the first letter of each word and after each underscore
capitalize <- function(name) {
  # Split the name at underscores, capitalize the first letter of each part, then rejoin
  parts <- strsplit(name, "_", fixed = TRUE)[[1]]
  parts <- sapply(parts, function(x) {
    # Capitalize the first letter and lower the rest
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
  })
  name <- paste(parts, collapse = "_")
  return(name)
}

# Apply the function to column names
colnames(ipl_data) <- sapply(colnames(ipl_data), capitalize)

# Check the converted 'match_date'
glimpse(ipl_data)

colnames(ipl_data)

# Standardize the match identifier by sorting team names alphabetically
unique_matches <- ipl_data %>%
  mutate(
    team1 = pmin(Batting_Team, Bowling_Team),  # Picks the alphabetically first team
    team2 = pmax(Batting_Team, Bowling_Team)   # Picks the alphabetically second team
  ) %>%
  distinct(Match_Date, team1, team2) %>%
  mutate(match_id = paste(Match_Date, team1, team2, sep = "_"))

# Count the total number of unique matches
total_matches <- nrow(unique_matches)

# Print the total number of matches
print(paste("Total unique matches counted:", total_matches))

# Optionally, list unique matches
print(unique_matches)


library(dplyr)

# Calculate total runs conceded, balls bowled, wickets taken, and economy rate for each bowler across all matches
bowler_stats <- ipl_data %>%
  group_by(Bowler) %>%
  summarise(
    total_runs_conceded = sum(Total_Runs),
    balls_bowled = n(),
    wickets_taken = sum(Elimination_Kind != "not out" & !is.na(Elimination_Kind), na.rm = TRUE), # Adjust wicket condition accordingly
    .groups = 'drop'
  ) %>%
  mutate(
    overs_bowled = balls_bowled / 6,
    economy_rate = ifelse(overs_bowled > 0, total_runs_conceded / overs_bowled, NA)
  )

# View the overall bowler statistics
print(bowler_stats)
library(ggplot2)
library(dplyr)

# Assuming bowler_stats is already calculated as provided in the previous code
# Here we visualize the data:

# Top 30 bowlers by wickets taken
top_bowlers_wickets <- bowler_stats %>%
  arrange(desc(wickets_taken)) %>%
  top_n(30, wickets_taken)

# Bar Chart: Top 10 Bowlers by Wickets Taken
ggplot(top_bowlers_wickets, aes(x = reorder(Bowler, wickets_taken), y = wickets_taken, fill = Bowler)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Bowlers by Wickets Taken",
       x = "Bowler",
       y = "Total Wickets Taken") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Adjust text angle for better readability

library(dplyr)

# Calculate total runs, number of 4s, number of 6s, and strike rate for each batsman
batsman_stats <- ipl_data %>%
  group_by(Batsman) %>%
  summarise(
    total_runs = sum(Batsman_Run),
    fours = sum(Batsman_Run == 4, na.rm = TRUE),
    sixes = sum(Batsman_Run == 6, na.rm = TRUE),
    balls_faced = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    strike_rate = (total_runs / balls_faced) * 100
  )

# View the batsman statistics
print(batsman_stats)

library(ggplot2)

# Scatter Plot: Strike Rate vs. Total Runs with number of 6s as size
ggplot(batsman_stats, aes(x = total_runs, y = strike_rate, size = sixes, color = Batsman)) +
  geom_point(alpha = 0.7) +
  labs(title = "Strike Rate vs. Total Runs with Number of 6s",
       x = "Total Runs",
       y = "Strike Rate",
       size = "Number of 6s") +
  theme_minimal() +
  theme(legend.position = "none") # Hide legend to focus on the data points

# Execute the plot command to view the graph

library(ggplot2)
library(dplyr)

# Filter for the top 25 batsmen by total runs
top_25_runs <- batsman_stats %>%
  arrange(desc(total_runs)) %>%
  slice_head(n = 25)

# Lollipop Chart: Top 25 Batsmen by Total Runs
ggplot(top_25_runs, aes(x = reorder(Batsman, total_runs), y = total_runs)) +
  geom_segment(aes(xend = Batsman, yend = 0), color = "grey") +  # Draw the lines
  geom_point(aes(color = Batsman), size = 5) +  # Draw points at the ends of the lines
  labs(title = "Top 25 Batsmen by Total Runs",
       x = "Batsman",
       y = "Total Runs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "none")  # Hide the legend for clarity




# Create standardized match identifier and extract the winner for the last ball recorded for each match
ipl_data_winner <- ipl_data %>%
  mutate(
    team1 = pmin(Batting_Team, Bowling_Team),  # Picks the alphabetically first team
    team2 = pmax(Batting_Team, Bowling_Team)   # Picks the alphabetically second team
  ) %>%
  group_by(Match_Date, team1, team2) %>%
  slice(n()) %>%
  ungroup() %>%
  distinct(Match_Date, team1, team2, Winner)  # Ensuring one entry per match with winner

# Count the number of wins for each team
team_wins <- ipl_data_winner %>%
  count(Winner, name = "wins")

# Print the number of wins for each team
print(team_wins)


# Determine the loser for each match
ipl_data_winner <- ipl_data_winner %>%
  mutate(Loser = if_else(Winner == team1, team2, team1))

# Count losses
team_losses <- ipl_data_winner %>%
  count(Loser, name = "losses")

# Merge wins and losses
total_wins_loss <- full_join(team_wins, team_losses, by = c("Winner" = "Loser")) %>%
  replace_na(list(wins = 0, losses = 0)) %>%
  rename(team = Winner)

# Print the results
print(total_wins_loss)


# Convert to long format for easier plotting
team_performance <- total_wins_loss %>%
  pivot_longer(cols = c(wins, losses), names_to = "Outcome", values_to = "Count")

# Plotting the data
ggplot(team_performance, aes(x = team, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", position = position_dodge()) +  # position_dodge() to place bars side by side
  labs(title = "Team Performance: Wins vs Losses",
       x = "Team",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text angle for better legibility if necessary

# Print the plot
print(team_performance)

# Load necessary libraries
library(dplyr)
library(tidyr)
# Load necessary libraries
library(dplyr)
library(tidyr)

# Create a dataframe from your data
team_performance <- data.frame(
  team = c("Chennai Super Kings", "Chennai Super Kings", "Delhi Capitals", "Delhi Capitals", 
           "Kolkata Knight Riders", "Kolkata Knight Riders", "Mumbai Indians", "Mumbai Indians", 
           "Punjab Kings", "Punjab Kings", "Rajasthan Royals", "Rajasthan Royals",
           "Royal Challengers Bangalore", "Royal Challengers Bangalore", 
           "Sunrisers Hyderabad", "Sunrisers Hyderabad"),
  Outcome = c("wins", "losses", "wins", "losses", "wins", "losses", "wins", "losses", 
              "wins", "losses", "wins", "losses", "wins", "losses", "wins", "losses"),
  Count = as.numeric(c(11, 5, 10, 6, 9, 8, 7, 7, 6, 8, 5, 9, 9, 6, 3, 11))
)

# Transform data to a wider format and calculate win rate
team_stats <- team_performance %>%
  spread(key = Outcome, value = Count) %>%
  mutate(win_rate = wins / (wins + losses))

# Plot win rates for all teams
ggplot(team_stats, aes(x = reorder(team, -win_rate), y = win_rate, fill = team)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Win Rates of IPL Teams", x = "Team", y = "Win Rate") +
  coord_flip() +  # Flips the x and y axes for better visualization
  theme_minimal()

# Build the logistic regression model
model <- glm(cbind(wins, losses) ~ win_rate, family = binomial, data = team_stats)

# Extract win rates for Chennai Super Kings and Mumbai Indians
csk_win_rate <- team_stats$win_rate[team_stats$team == "Chennai Super Kings"]
mi_win_rate <- team_stats$win_rate[team_stats$team == "Mumbai Indians"]

# Prepare data for prediction
prediction_data <- data.frame(win_rate = c(csk_win_rate, mi_win_rate))

# Predict the outcome
probabilities <- predict(model, newdata = prediction_data, type = "response")
prob_csk_wins <- probabilities[1]  # Probability that Chennai Super Kings wins
prob_mi_wins <- probabilities[2]  # Probability that Mumbai Indians wins

# Output the predictions
cat("Probability that Chennai Super Kings wins:", prob_csk_wins, "\n")
cat("Probability that Mumbai Indians wins:", prob_mi_wins, "\n")

# Data frame for plotting predictions
prediction_plot_data <- data.frame(
  Team = c("Chennai Super Kings", "Mumbai Indians"),
  Probability = probabilities
)

# Plot predicted probabilities
ggplot(prediction_plot_data, aes(x = Team, y = Probability, fill = Team)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Predicted Winning Probability", x = "Team", y = "Probability") +
  theme_minimal()

# Transform data to a wider format and calculate win rate
team_stats <- team_performance %>%
  spread(key = Outcome, value = Count) %>%
  mutate(win_rate = wins / (wins + losses))

# Build the logistic regression model
model <- glm(cbind(wins, losses) ~ win_rate, family = binomial, data = team_stats)

# Create all possible matchups
matchups <- expand.grid(team1 = team_stats$team, team2 = team_stats$team, stringsAsFactors = FALSE)
matchups <- matchups[matchups$team1 != matchups$team2,]  # Remove matchups of a team against itself

# Add win rates for each team in the matchups
matchups <- matchups %>%
  left_join(team_stats, by = c("team1" = "team")) %>%
  rename(win_rate1 = win_rate) %>%
  left_join(team_stats, by = c("team2" = "team")) %>%
  rename(win_rate2 = win_rate)

# Prepare a new dataset for prediction that includes the win_rate for each team in the format expected by the model
# The model expects a single 'win_rate' column, so we need to do this separately for each team
prediction_data <- with(matchups, data.frame(win_rate = win_rate1))
prediction_data2 <- with(matchups, data.frame(win_rate = win_rate2))

# Predict probabilities for each team in the matchups
matchups$prob_team1_wins <- predict(model, newdata = prediction_data, type = "response")
matchups$prob_team2_wins <- predict(model, newdata = prediction_data2, type = "response")

# Plot all matchups and their predicted probabilities
ggplot(matchups, aes(x = team1, y = team2, fill = prob_team1_wins)) +
  geom_tile(color = "white") +  # Use tiles to create a heatmap
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, 
                       name = "Winning\nProbability", labels = scales::percent_format()) +
  labs(title = "Predicted Winning Probabilities for IPL Teams",
       x = "Team 1",
       y = "Team 2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate team names for clarity

# Load necessary libraries
library(ggplot2)

# Plot paired bar charts for predicted probabilities
ggplot(matchups, aes(x = team1)) +
  geom_bar(aes(y = prob_team1_wins, fill = team1), stat = "identity", position = position_dodge(width = 0.9)) +
  geom_bar(aes(y = prob_team2_wins, fill = team2), stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Comparative Winning Probabilities for IPL Teams",
       x = "Matchup",
       y = "Probability") +
  facet_wrap(~ team1, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")  # Uses a distinct color palette

# Plot probabilities in a facet grid
ggplot(matchups, aes(x = team2, y = prob_team1_wins, fill = prob_team1_wins)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5,
                       name = "Winning\nProbability", labels = scales::percent_format()) +
  labs(title = "Winning Probability of Team 1 vs Team 2",
       x = "Opponent (Team 2)",
       y = "Probability of Team 1 Winning") +
  facet_wrap(~ team1, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
library(dplyr)

# Select top players for Sunrisers Hyderabad
top_batsmen <- batsman_stats %>% arrange(desc(total_runs)) %>% head(6)
top_bowlers <- bowler_stats %>% arrange(desc(wickets_taken)) %>% head(6)

# Remove these top players from the pool
remaining_batsmen <- setdiff(batsman_stats, top_batsmen)
remaining_bowlers <- setdiff(bowler_stats, top_bowlers)

# Create a function to randomly assign players to teams
assign_players_to_teams <- function(players, teams, column) {
  num_players <- nrow(players)
  if (num_players > 0) {
    sample_teams <- sample(teams, num_players, replace = TRUE)
    return(data.frame(Player = players[[column]], Team = sample_teams))
  } else {
    return(data.frame(Player = character(0), Team = character(0)))
  }
}

# Define the teams
teams <- c("Chennai Super Kings", "Delhi Capitals", "Kolkata Knight Riders", "Mumbai Indians", "Punjab Kings", "Rajasthan Royals", "Royal Challengers Bangalore", "Sunrisers Hyderabad")

# Assign remaining players to teams
batsmen_assignments <- assign_players_to_teams(remaining_batsmen, teams, "Batsman")
bowlers_assignments <- assign_players_to_teams(remaining_bowlers, teams, "Bowler")

# Combine top players into Sunrisers Hyderabad
sunrisers_batsmen <- data.frame(Player = top_batsmen$Batsman, Team = rep("Sunrisers Hyderabad", nrow(top_batsmen)))
sunrisers_bowlers <- data.frame(Player = top_bowlers$Bowler, Team = rep("Sunrisers Hyderabad", nrow(top_bowlers)))

# Combine all player assignments
final_batsmen_assignments <- rbind(sunrisers_batsmen, batsmen_assignments)
final_bowlers_assignments <- rbind(sunrisers_bowlers, bowlers_assignments)

# Simple model to predict winning percentage (you can expand on this based on your statistical model)
# This part is highly simplified and just a placeholder
winning_percentage <- function(team_name) {
  total_runs_scored <- sum(batsman_stats$total_runs[batsman_stats$Batsman %in% final_batsmen_assignments$Player[final_batsmen_assignments$Team == team_name]])
  total_wickets_taken <- sum(bowler_stats$wickets_taken[bowler_stats$Bowler %in% final_bowlers_assignments$Player[final_bowlers_assignments$Team == team_name]])
  return(list(runs = total_runs_scored, wickets = total_wickets_taken))
}

# Calculate for Sunrisers Hyderabad as an example
sunrisers_performance <- winning_percentage("Sunrisers Hyderabad")
print(sunrisers_performance)




library(dplyr)

# Create aggregated statistics for each team
team_stats <- data.frame(
  Team = unique(final_batsmen_assignments$Team),
  Total_Runs = sapply(unique(final_batsmen_assignments$Team), function(t) {
    sum(batsman_stats$total_runs[batsman_stats$Batsman %in% final_batsmen_assignments$Player[final_batsmen_assignments$Team == t]])
  }),
  Total_Wickets = sapply(unique(final_bowlers_assignments$Team), function(t) {
    sum(bowler_stats$wickets_taken[bowler_stats$Bowler %in% final_bowlers_assignments$Player[final_bowlers_assignments$Team == t]])
  })
)

# Ensure team_stats is correctly populated
print(team_stats)

# Generate all possible matches excluding self-matches
matches <- expand.grid(Home = team_stats$Team, Away = team_stats$Team)
matches <- matches[matches$Home != matches$Away,]

# Predict outcomes based on a simple comparative logic
predict_match_outcome <- function(home, away) {
  home_stats <- team_stats[team_stats$Team == home,]
  away_stats <- team_stats[team_stats$Team == away,]
  home_score <- home_stats$Total_Runs / away_stats$Total_Wickets
  away_score <- away_stats$Total_Runs / home_stats$Total_Wickets
  
  if (home_score > away_score) home else away
}

# Simulate the outcomes
matches$Winner <- mapply(predict_match_outcome, matches$Home, matches$Away)

# Check results
print(head(matches))

library(ggplot2)

# Count wins per team
win_counts <- table(matches$Winner)

# Create a bar plot of win counts
ggplot(data = as.data.frame(win_counts), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Predicted IPL Season Wins", x = "Team", y = "Total Wins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



