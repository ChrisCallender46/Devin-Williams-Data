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


# 5 Dataframes
View(batted_ball)
View(stats)
View(pitch_data)
View(plate_discipline)
View(statcast)


# Exploratory Data Analysis (EDA)

# p <= .05  <- statistically significant
# r >= .75 <- High correlation

cor.test(batted_ball$EV, batted_ball$xFIP) # SS, HPC

# Turn barrel % into decimal
barrel_decimal <- as.numeric(sub("%", "", batted_ball$`Barrel%`, fixed=TRUE))/100

# Turn hard % into decimal
hard_decimal <- as.numeric(sub("%", "", batted_ball$`Hard%`, fixed=TRUE))/100

# Turn gb% into decimal
gb_decimal <- as.numeric(sub("%", "", batted_ball$`GB%`, fixed=TRUE))/100
cor.test(gb_decimal, batted_ball$xFIP) # SS, HNC
cor.test(gb_decimal, stats$xERA) #SS, HNC
cor.test(gb_decimal, stats$FIP) #SS, HNC
# High Ground ball rate leads to lower FIP, xFIP, and xERA and low ground ball rate leads to higher FIP, xFIP, and xERA

# Time Series Graphs
ggplot(batted_ball, aes(x = Season, y = EV)) +
  geom_smooth() +
  ylim(83, 89)

ggplot(batted_ball, aes(x = Season, y = gb_decimal)) +
  geom_smooth() +
  ylim(.35, .62)

ggplot(stats, aes(x = Season, y = ERA)) +
  geom_smooth() +
  ylim(0, 12)

cor.test(stats$ERA, batted_ball$xFIP) #SS, HPC
cor.test(stats$xFIP, stats$WAR) # SS, HNC
cor.test(stats$WHIP, stats$WAR) # SS, HNC

cor.test(batted_ball$EV, stats$ERA) #SS, HPC
cor.test(batted_ball$EV, stats$xERA) #SS, HPC
cor.test(batted_ball$EV, stats$FIP) #SS, HPC
cor.test(batted_ball$EV, stats$xFIP) #SS, HPC

# Hard Hit balls usually lead to giving up more runs

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

cor.test(k_decimal, stats$ERA) #SS, HNC
cor.test(k_decimal, stats$xERA) #SS, HNC
cor.test(k_decimal, stats$FIP) #SS, HNC
cor.test(k_decimal, stats$xFIP) #SS, HNC


View(pitch_data)

ggplot(pitch_data, aes(x = Season, y = vFA)) +
  geom_smooth() +
  ylim(93, 97)

ggplot(pitch_data, aes(x = Season, y = wFA)) +
  geom_smooth() +
  ylim(-3.5, 8.5)

ggplot(pitch_data, aes(x = Season, y = wCH)) +
  geom_smooth() +
  ylim(-1, 15.5)

View(plate_discipline)

# Turn O-Swing % to decimal
o_swing_decimal <- as.numeric(sub("%", "", plate_discipline$`O-Swing%`, fixed = TRUE))/100

ggplot(plate_discipline, aes(x = Season, y = o_swing_decimal)) +
  geom_smooth() +
  ylim(.24, .36)
# Getting fewer chases on balls out of zone

cor.test(o_swing_decimal, stats$xERA) #SS, HNC
cor.test(o_swing_decimal, stats$FIP) #SS, HNC
cor.test(o_swing_decimal, stats$xFIP) #SS, HNC
cor.test(o_swing_decimal, stats$WAR) #SS, HPC

# Turn contact % to decimal
contact_decimal <- as.numeric(sub("%", "", plate_discipline$`Contact%`, fixed = TRUE))/100

ggplot(plate_discipline, aes(x = Season, y = contact_decimal)) +
  geom_smooth() +
  ylim(.5, .8)
# More contact

cor.test(contact_decimal, stats$WAR) #SS, HNC
cor.test(contact_decimal, stats$ERA) #SS, HPC
cor.test(contact_decimal, stats$FIP) #SS, HPC

# Turn SwStr (Swing and miss) % to decimal
SwStr_decimal <- as.numeric(sub("%", "", plate_discipline$`SwStr%`, fixed = TRUE))/100

ggplot(plate_discipline, aes(x = Season, y = SwStr_decimal)) +
  geom_smooth() +
  ylim(.085, .225)
# Fewer swings and misses

cor.test(SwStr_decimal, stats$WAR) #SS, HPC
cor.test(SwStr_decimal, stats$ERA) #SS, HNC
cor.test(SwStr_decimal, stats$xERA) #SS, HNC
cor.test(SwStr_decimal, stats$FIP) #SS, HNC
cor.test(SwStr_decimal, stats$xFIP) #SS, HNC

# Chasing out of zone leads to more strikeouts
cor.test(o_swing_decimal, k_decimal) #SS, HPC

# Chasing out of zone leads to more weak hit balls
cor.test(o_swing_decimal, batted_ball$EV) #SS, HNC

# Swing and miss leads to strikeouts
cor.test(SwStr_decimal, k_decimal) #SS, HPC



