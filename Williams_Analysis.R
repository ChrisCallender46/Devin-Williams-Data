library(tidyverse)

advanced_stats <- Williams_Advanced_Stats
batted_ball <- Williams_Batted_Ball_Data
fangraphs <- Williams_Fangraphs_Stats
more_batted_ball <- Williams_More_Batted_Ball_Data
pitch_type <- Williams_Pitch_Type_and_Velo
pitch_values <- Williams_Pitch_Values_Data
plate_discipline <- Williams_Plate_Discipline_Data
standard <- Williams_Standard_Stats
statcast <- Williams_Statcast_Data


# Merge batted ball
batted_ball_merge <- merge(more_batted_ball, batted_ball, by = c("Season", "Team", "Level"))
batted_ball <- batted_ball_merge

# Merge stats
stats <- merge(standard, fangraphs, by = c("Season", "Team", "Level", "W", "L", "ERA", "G", "GS", "SV", "IP"))
stats <- stats |>
  relocate(Age, .after = Level)

advanced_stats_clean <- advanced_stats |>
  select(!c("K/9", "BB/9", "HR/9", "BABIP", "LOB%", "FIP"))
  
stats_merge <- merge(stats, advanced_stats_clean, by = c("Season", "Team", "Level")) # "K/9", BB/9", "HR/9", "BABIP", "LOB%", "FIP"))
stats <- stats_merge

#Merge pitches
pitches <- pitch_values |>
  select(Season:wFA, wCH, `wFA/C`, `wCH/C`)

pitches_type <- pitch_type |>
  select(Season:vFA, `CH%`, vCH)

pitch_data <- merge(pitches_type, pitches, by = c("Season", "Team", "Level"))


# 5 final dataframes
View(batted_ball)
cor.test(batted_ball$EV, batted_ball$xFIP) # EV and xFIP highly correlated and EV is statistically significant
cor.test(batted_ball$LA, batted_ball$xFIP) # Not stat sig

# Turn barrel % into decimal
barrel_decimal <- as.numeric(sub("%", "", batted_ball$`Barrel%`, fixed=TRUE))/100
cor.test(barrel_decimal, batted_ball$xFIP) # Not stat sig

# Turn hard % into decimal
hard_decimal <- as.numeric(sub("%", "", batted_ball$`Hard%`, fixed=TRUE))/100
cor.test(hard_decimal, batted_ball$xFIP) # Not stat sig

# Turn gb% into decimal
gb_decimal <- as.numeric(sub("%", "", batted_ball$`GB%`, fixed=TRUE))/100
cor.test(gb_decimal, batted_ball$xFIP) # Very stong negative correlation and highly stat sig
# High Ground ball rate leads to lower xFIP and low ground ball rate leads to higher xFIP

# Increase in EV (exit velo), LA (launch angle), barrel %, GB %, Hard %, xFIP
# Williams usually has success by avoiding barrels, inducing ground balls, and avoiding hard hits
# However, he has not done that this year
# That is causing him to not be successful 

# Make time series graphs to show this (x = season, y = stat)

ggplot(batted_ball, aes(x = Season, y = EV)) +
  geom_smooth() +
  ylim(83, 89)

ggplot(batted_ball, aes(x = Season, y = gb_decimal)) +
  geom_smooth() +
  ylim(.35, .62)

View(stats)

ggplot(stats, aes(x = Season, y = ERA)) +
  geom_smooth() +
  ylim(0, 12)

ggplot(stats, aes(x = Season, y = xERA)) +
  geom_smooth() +
  ylim(1, 7)

ggplot(stats, aes(x = Season, y = FIP)) +
  geom_smooth() +
  ylim(2, 5)

ggplot(stats, aes(x = Season, y = xFIP)) +
  geom_smooth() +
  ylim(1, 6.5)

ggplot(stats, aes(x = Season, y = `K/9`)) +
  geom_smooth() +
  ylim(9, 18)

# Turn K% into decimal
k_decimal <- as.numeric(sub("%", "", stats$`K%`, fixed = TRUE))/100

ggplot(stats, aes(x = Season, y = k_decimal)) +
  geom_smooth() +
  ylim(.15, .55)

ggplot(stats, aes(x = Season, y = `BB/9`)) +
  geom_smooth() +
  ylim(3, 8)

ggplot(stats, aes(x = Season, y = BABIP)) +
  geom_smooth() +
  ylim(.19, .43)

# ERA, xERA, FIP, xFIP, K/9, BB/9, BABIP, AVG, WHIP
# Williams is not striking out as many batters as usual
# He also usually excels at getting ground balls
# This year his strikeouts are down, getting less ground balls, and more balls put in play are hits

View(pitch_data)
# His fastball is much less effective this year, due to a loss of velocity
# His changeup is also less effective this year
# Those are the only 2 pitches he has thrown this year
# In previous years he will sometimes mix in a cutter, slider, or sinker

View(plate_discipline)
# Getting fewer chases on balls out of zone
# More contact
# Fewer swings and misses

View(statcast)
# EV, LA, Barrel%, and HardHit% all increase
# ERA and xERA by far the worst of his career








