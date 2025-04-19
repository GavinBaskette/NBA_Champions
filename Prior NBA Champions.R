
Title: "Prior NBA Champions"
Author: "Gavin Baskette"
Date: Sys.Date()


# ---- R Libraries ----

library(dplyr)
library(car)
library(lmtest)


# ---- DATA LOADING AND INITIAL PROCESSING ----

Prior_NBA_champions <- read.csv("C:/Users/gavin/OneDrive/Desktop/Sport Models/NBA Champions Clean.csv")
head(Prior_NBA_champions)
tail(Prior_NBA_champions)


# ---- DATA CLEANING: REMOVE NON-NUMERIC COLUMNS & HANDLE MISSING DATA ----

non_numeric_columns <- c("Team","Top.10.in.both.","Top.5.in.either.","HC.Playoff.Win..",
                         "X5.Year.HC.Rule", "X40.20.Rule.","All.Star.guard.",
                         "Highest.DPOY.Finisher","Title.Winning.HC.")

numeric_data <- Prior_NBA_champions[, !(names(Prior_NBA_champions) %in% 
                                              non_numeric_columns)]

numeric_data <- numeric_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

clean_numeric_data <- numeric_data[complete.cases(numeric_data), ]

clean_numeric_data <- clean_numeric_data[, -which(names(clean_numeric_data) == "Season")]


# ---- INITIAL LINEAR MODEL TO CHECK MULTICOLLINEARITY ----

clean_numeric_data$dummy_y <- rnorm(nrow(clean_numeric_data))
lm_model <- lm(dummy_y ~ ., data = clean_numeric_data)

print(names(clean_numeric_data))

predictors <- clean_numeric_data[, !names(clean_numeric_data) %in% "dummy_y"]
target <- clean_numeric_data$dummy_y


# ---- IDENTIFYING AND REMOVING HIGHLY CORRELATED VARIABLES ----

cor_matrix <- cor(predictors)
high_corr <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

variables_to_drop <- c("HC.Playoff.Wins", "HC.Playoff.Games", "Difference.between.Win...Loss.streak", 
                       "Longest.win.streak")  # Use high_corr results
predictors_reduced <- predictors[, !(names(predictors) %in% variables_to_drop)]


# ---- UPDATED MODEL AFTER DROPPING HIGHLY CORRELATED VARIABLES ----

final_data <- cbind(predictors_reduced, dummy_y = target)

lm_model_final <- lm(dummy_y ~ ., data = final_data)


# ---- CHECKING FOR ALIASED VARIABLES ----

alias_results_final <- alias(lm_model_final)
print(alias_results_final)

alias(lm_model_final)$Complete

lm_model_final

final_data_filtered <- final_data

na_vars <- names(coef(lm_model_final))[is.na(coef(lm_model_final))]

final_data_filtered <- final_data_filtered[, !names(final_data_filtered) %in% na_vars]

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)

summary(lm_model_final)

dim(final_data_filtered)

cor_matrix <- cor(final_data_filtered)
high_corr <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

vars_to_remove <- unique(rownames(high_corr))

final_data_filtered <- final_data_filtered[, !(names(final_data_filtered) %in% vars_to_remove)]

dim(final_data_filtered)

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)
summary(lm_model_final)


# ---- FINAL MODEL SELECTION & VIF ANALYSIS ----

vif_values_final <- vif(lm_model_final)
print(vif_values_final)


# ---- ADJUSTING WEIGHTS BASED ON VIF ----

# Original weights (z-scores from dataset)
weights <- c(6.234317264, 3.715634528, 7.452988813, 1.966765208, 
             2.377977123, 2.042510748, 1.961647434, 1.25726369, 
             1.146393571, 1.395108997, 1.817885003, 1.343150624, 
             4.843046965, 0.940820324, 3.032079954, 2.660121631, 
             3.708295536, 2.385158406, 3.133333333, 0.405447046,
             0.911175483, 1.767566801)

# Actual VIF values from earlier result
vif_values <- c(132.456952, 15191.389497, 97.111808, 155.583703, 
                197.755032, 26.526604, 9.646426, 21.178221, 
                102.460611, 33.155752, 83.790181, 35.912888, 
                8.931464, 9.393736, 10229.659281, 15851.254591,
                5201.753287, 3955.814614, 27.372030, 30.411146, 
                34.528878, 25.755725)

# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(log(vif_values))

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("SRS.Rank", "Wins", "Seeding", "Wins.vs...500..teams",
               "Win...vs...500..teams", "Offensive.Rating.Rank",
               "Defensive.Rating.Rank", "Points.For.Rank", "Points.Against.Rank", 
               "X..of.all.stars","X..of.all.nba.players", "X..of.all.defense.players", 
               "Pre.Season.Odds.Rank", "Playoff.Wins.Prior.Year", "Home.win..",
               "Away.win..", "Win...vs..same.conference", "Win...vs..other.conference",
               "Longest.losing.streak", "Wins.in.close.games", 
               "Win...in.games.decided.by.5.or.less..or.OT..close.games.",
               "Average.starting.5.age"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)


# ---- FINAL SELECTION OF REMAINING VARIABLES & RUNNING VIF ANALYSIS ----

head(Prior_NBA_champions)
head(predictors)

remaining_vars <- c("HC.Playoff.Games", "HC.Playoff.Wins", "Wins.Prior.Year", 
                    "Seed.Prior.Year", "Longest.win.streak", 
                    "Difference.between.Win...Loss.streak", 
                    "Average.Starting.5.Playoff.Experience", 
                    "X3.Point...Rank", "Defensive.3.Point...Rank", 
                    "eFG..Rank", "Defensive.eFG..Rank", 
                    "Opponent.FTA.Per.Game.Rank", 
                    "Amount.of.FTA.Per.Game.Compared.to.Opponent", 
                    "Top.guard.Offensive.Rating", "Highest.MVP.Finisher",
                    "Turnover.Differential")

predictors_with_target <- cbind(predictors, dummy_y = target)

final_data_remaining <- predictors_with_target[, c(remaining_vars, "dummy_y")]

lm_remaining <- lm(dummy_y ~ ., data = final_data_remaining)

vif_values_remaining <- vif(lm_remaining)

print(vif_values_remaining)


# ---- ADJUSTING WEIGHTS BASED ON VIF FOR REMAINING VARIABLES ----

# Original weights (z-scores from dataset)
weights <- c(0.974821878, 0.941831592, 1.44014859, 1.379669752, 
             1.009831192, 1.680762809, 2.080290053, 0.734338554, 
             0.854018373, 2.272600342, 2.204540769, 0.54311234, 
             0.519364421, 0.218627633, 3.915002774, 0.296955706)

# Actual VIF values from earlier result
vif_values <- c(193.845366, 205.734316, 11.795591, 11.347767, 
                60.619104, 57.819782, 3.926082, 6.541595, 
                1.799615, 5.401954, 3.703328, 4.469929, 
                6.022427, 1.816428, 4.891130, 2.946629)


# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(vif_values)

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("HC.Playoff.Games", "HC.Playoff.Wins", 
               "Wins.Prior.Year", "Seed.Prior.Year", 
               "Longest.win.streak", 
               "Difference.between.Win...Loss.streak", 
               "Average.Starting.5.Playoff.Experience", 
               "X3.Point...Rank", "Defensive.3.Point...Rank", 
               "eFG..Rank", "Defensive.eFG..Rank", 
               "Opponent.FTA.Per.Game.Rank", 
               "Amount.of.FTA.Per.Game.Compared.to.Opponent", 
               "Top.guard.Offensive.Rating", "Highest.MVP.Finisher",
               "Turnover.Differential"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)
