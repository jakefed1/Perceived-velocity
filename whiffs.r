library(tidyverse)
library(modelr)
library(pscl)

swings_july20 <- read_csv(file = "swings_july20.csv")
swings_august20 <- read_csv(file = "swings_august20.csv")
swings_september20 <- read_csv(file = "swings_september20.csv")
swings_october20 <- read_csv(file = "swings_october20.csv")
swings_april21 <- read_csv(file = "swings_april21.csv")
swings_may21 <- read_csv(file = "swings_may21.csv")
swings_june21 <- read_csv(file = "swings_june21.csv")
swings_july21 <- read_csv(file = "swings_july21.csv")

swings <- merge(swings_july20, swings_august20, all = TRUE)
swings <- merge(swings, swings_september20, all = TRUE)
swings <- merge(swings, swings_october20, all = TRUE)
swings <- merge(swings, swings_april21, all = TRUE)
swings <- merge(swings, swings_may21, all = TRUE)
swings <- merge(swings, swings_june21, all = TRUE)
swings <- merge(swings, swings_july21, all = TRUE)

swings <-
  swings %>% 
  mutate(whiff = case_when(
    description %in% c('swinging_strike', 'swinging_strike_blocked', 'foul_tip') ~ 1,
    !description %in% c('swinging_strike', 'swinging_strike_blocked', 'foul_tip') ~ 0
  ))

swings <-
  swings %>% 
  filter(!is.na(release_extension)) %>% 
  filter(!is.na(release_spin_rate))

swings <-
  swings %>% 
  mutate(PV_diff = effective_speed - release_speed)

swings <-
  swings %>% 
  filter(!is.na(PV_diff))

swings <-
  swings %>% 
  filter(release_speed > 80)

swings_85 <-
  swings %>% 
  filter(release_speed > 85)



#This study is to determine the effect of release extension on whiff generation. Obviously higher velocity
#Correlates to more whiffs, but I'd like to see if perceived velocity, which factors in release extension
#And essentially represents the velocity that the hitter sees, is even more important.
#So a pitcher who throws 97 but gets a ton of extension, like Carter Capps, Tyler Glasnow,
#Or Chad Bradford (I know he threw like 81 but it played up to like 85), feel like 100 to a hitter.
#I figure that 97 that plays up to 100 will generate more whiffs than 97 that feels like 97.

#I'm going to look at the data on both a pitch by pitch basis and on a player overall basis.
#For the pitch by pitch basis, I'm going to run logistic regressions between velocity and whiff
#And PV and whiff. Then I will look at the McFadden's R-squared value (a pseudo R-squared value since
#no true R-squared value exists for logistic regression) to see how much of the variance in whiff can be
#Explained by velocity/PV. I will also try a binned approach where I place the pitches into bins
#Of 1 mph each and examine the trend that way. I could also potentially create a secondary bin for PV within
#Each velocity bin so I can explore whether extension matters more at different velocity levels. This
#Would also allow me to really compare apples to apples, as the goal isn't to see if 95 playing up to 97
#Performs like 97, rather it is to see if 97 playing up to 100 performs differently than normal 97.

#For the player overall basis, I will simply plot average velocity and average PV vs whiff %. I could also
#Try binning it and analyzing from that perspective. I don't think this will be as insightful
#As the pitch by pitch comparisons though.

#I am only looking at fastballs because velocity is more of a factor relative to other things on fastballs.
#On breaking balls, movement and location are a lot more impactful on whiff probability, and they would
#Be too powerful confounding factors.

#I also filtered out pitches under 85 mph because a lot of them are misclassified or they will mess up the
#Data. Sorry Tyler Rogers and Ryan Yarbrough.

#Logistic regression models
logit_velo<- glm(whiff~release_speed, family = binomial, data = swings_85)
logit_velo
logit_PV <- glm(whiff~effective_speed, family = binomial, data = swings_85)
logit_PV

swings2 <-
  swings_85 %>% 
  add_predictions(model = logit_velo, type = 'response', var = 'phat_velo') %>% 
  add_predictions(model = logit_PV, type = 'response', var = 'phat_PV')

#Small dataset is just for testing my code because it takes a while to run things on the 183K row dataset
swings_small <-
  swings2 %>% 
  sample_n(5000, replace = FALSE)

smallplot2 <-
  ggplot(data = swings_small, aes(x = release_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 1, se = FALSE) +
  xlim(c(80,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = ",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
smallplot2


plot1 <-
  ggplot(data = swings2, aes(x = release_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.001495",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
plot1
plot1 + scale_y_continuous(breaks=seq(0.15,0.275,0.025))


plot2 <-
  ggplot(data = swings2, aes(x = effective_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Perceived Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Perceived Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.00161",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
plot2
plot2 + scale_y_continuous(breaks=seq(0.15,0.275,0.025))

#Just have to make sure this is the same line as the previous graph because then it means the glms are the same
#And I need to have the actual GLM to find the McFadden's R-squared
plot2test <-
  ggplot(data = swings2)+
  geom_line(mapping = aes(x = effective_speed, y = phat_PV), col = 'red', size = 0.75)+
  #geom_point(size = 0.5, alpha = 0.01) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Perceived Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Perceived Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = ",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
plot2test
plot2test + scale_y_continuous(breaks=seq(0.15,0.275,0.025))

#Perfect it is the same
#Well I guess I could've graphed it like this lol
#I'm not even going to run this test for plot1

pR2(logit_velo)
pR2(logit_PV)

summarise(swings2,
          velo = mean( (whiff - phat_velo)^2),
          PV = mean( (whiff - phat_PV)^2))

#The R-squared for PV is higher and the Brier Score is lower, W


#Let's work on WRAE.
#So the idea is WRAE which stands for whiff rate above expected. I'm going to take the whiff rate at each mph,
#And set that as a pitch's expected whiff rate. Then, for each velocity value, I'll take the PV-velo,
#And calculate the whiff rate at that difference for each difference at each velocity value. Then I will
#Subtract the expected whiff rate from that whiff rate, and that's the WRAE. Then I will take a weighted
#Average of each WRAE at every velocity level for the same PV-velo difference. That's the final thing I'll graph.
#That will show the importance of Perceived Velocity.

#Here I'm running tests to make sure that it isn't showing duplicates
counttest1 <- count(swingsbinWRAE, release_speed)
counttest2 <- count(swingsbinWRAE, PV_diff)
counttest3 <- count(swings2, PV_diff)
#I didn't notice until later that multiple rows were showing up for the same PV_diff values
#And different pitches are in each of them
#There should be 1 row with each PV_diff
#So like there should be 1 row with 18 pitches with PV_diff -2.6
#Instead there are 2, one with 12 pitches and one with 6
#This is why the correlations I initially saw were so bad and why the graph made no sense
#I have to figure out why it is doing that and how to fix it
#The glitch clearly goes all the way back as far as swings2 and probably further
#Luckily it doesn't matter for anything before this because I didn't use PV_diff

write_csv(swings2,"swingsforchecking.csv")
swings2 <- read_csv(file = 'swingsforchecking.csv')

#I pulled the table out of R and into Excel to see if I could diagnose the problem.
#Turned out the PV_diff's weren't always exact numbers to the tenth place, and sometimes there was
#A 17th decimal point messed up or something.
#To fix, I just rounded the values.
#So now I just put it back into R to get back to work

#First let's rerun the test to make sure its fixed
counttest4 <- count(swings2, PV_diff)
#Perfect, it is fixed

swings2 <-
  swings2 %>% 
  select(player_name, pitch_type, game_year, release_speed, effective_speed, release_extension, description, release_spin_rate, estimated_woba_using_speedangle, woba_value, woba_denom, whiff, PV_diff, phat_velo, phat_PV)

swingsWRAE <-
  swings2 %>% 
  select(PV_diff, release_speed, effective_speed, phat_velo, whiff) %>% 
  group_by(release_speed, PV_diff) %>% 
  mutate(whiff_rate_each = mean(whiff)) %>% 
  ungroup() %>% 
  mutate(WRAE = whiff_rate_each - phat_velo)

swingsWRAE_0 <-
  swingsWRAE %>% 
  group_by(release_speed) %>% 
  add_count(PV_diff, name = 'PV_diff_at_each_velo_count') %>% 
  ungroup() %>% 
  add_count(PV_diff, name = 'PV_diff_count') %>% 
  mutate(percentage_of_each_PV_diff_at_each_velo = PV_diff_at_each_velo_count / PV_diff_count) %>% 
  select(PV_diff, release_speed, effective_speed, phat_velo, whiff_rate_each, WRAE, PV_diff_at_each_velo_count, PV_diff_count, percentage_of_each_PV_diff_at_each_velo) %>% 
  distinct() %>% 
  mutate(WRAE_final_0 = percentage_of_each_PV_diff_at_each_velo * WRAE) %>% 
  group_by(PV_diff) %>% 
  mutate(WRAE_final = sum(WRAE_final_0)) %>% 
  ungroup()
swingsWRAE_0

#Have to do this so I don't graph duplicate points on top of each other messing up the regression line
swingsWRAE_0_graph <-
  swingsWRAE_0 %>% 
  select(PV_diff, WRAE_final) %>% 
  distinct()

WRAE_reg_1 <- lm(data = swingsWRAE_0_graph, WRAE_final ~ PV_diff)
WRAE_reg_1
WRAE_reg_plot_1<-
  ggplot(data=swingsWRAE_0_graph)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = -0.0220042, slope = -0.0001037, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 1 swing)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = -0.01",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
WRAE_reg_plot_1
summary(WRAE_reg_1)

#Ok that's frustrating
#But I feel like that doesn't make sense
#A lot of the values on the ends are messing this up. The values in the middle have a small upslope,
#Which makes more sense.
#Maybe the PV_diffs on the edges don't have enough observations

swingsWRAE_0_graph_0 <-
  swingsWRAE_0 %>% 
  select(PV_diff, WRAE_final, PV_diff_count) %>% 
  filter(PV_diff_count > 50) %>% 
  distinct()

WRAE_reg_1a <- lm(data = swingsWRAE_0_graph_0, WRAE_final ~ PV_diff)
WRAE_reg_1a
WRAE_reg_plot_1a<-
  ggplot(data=swingsWRAE_0_graph_0)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = 0.0009535, slope = 0.0057476, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 50 swings)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = 0.2192",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
WRAE_reg_plot_1a
summary(WRAE_reg_1a)

swingsWRAE_0_graph_00 <-
  swingsWRAE_0 %>% 
  select(PV_diff, WRAE_final, PV_diff_count) %>% 
  filter(PV_diff_count > 250) %>% 
  distinct()

WRAE_reg_1b <- lm(data = swingsWRAE_0_graph_00, WRAE_final ~ PV_diff)
WRAE_reg_1b
WRAE_reg_plot_1b<-
  ggplot(data=swingsWRAE_0_graph_00)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = -0.0007807, slope = 0.0079349, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 250 swings)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = 0.4356",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
WRAE_reg_plot_1b
summary(WRAE_reg_1b)

swingsWRAE_0_graph_000 <-
  swingsWRAE_0 %>% 
  select(PV_diff, WRAE_final, PV_diff_count) %>% 
  filter(PV_diff_count > 350) %>% 
  distinct()

WRAE_reg_1c <- lm(data = swingsWRAE_0_graph_000, WRAE_final ~ PV_diff)
WRAE_reg_1c
WRAE_reg_plot_1c<-
  ggplot(data=swingsWRAE_0_graph_000)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = -0.001147, slope = 0.007038, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 350 swings)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = 0.5196",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
WRAE_reg_plot_1c
summary(WRAE_reg_1c)


#So clearly, the correlation quickly improves with increasing the minimums.
#While results vary for pitches with PV_diffs large in magnitude, in the middle sections, there is a clear
#Direct relationship between PV_diff and WRAE. A WRAE of 1.25% might not sound like a lot, but think of it as
#Generating 21.25% whiffs on your fastball vs 20%. That can propel a player more than 20 spots on the leaderboard
#In some places. Still, the difference is not as high as I expected.

#Something that I know can be confounding the data is pitch type. I know I've already selected only fastballs,
#But sinkers are known to generate less whiffs than four-seamers. Maybe if I run the logistic regression
#Model on only four-seamers and re-calculate the WRAEs for only four-seamers I'll generate better results.


swingsFF <-
  swings_85 %>% 
  filter(pitch_type == 'FF')

logit_veloFF <- glm(whiff~release_speed, family = binomial, data = swingsFF)
logit_veloFF
logit_PVFF <- glm(whiff~effective_speed, family = binomial, data = swingsFF)
logit_PVFF

swingsFF2 <-
  swingsFF %>% 
  add_predictions(model = logit_veloFF, type = 'response', var = 'phat_velo') %>% 
  add_predictions(model = logit_PVFF, type = 'response', var = 'phat_PV')

FFplot1 <-
  ggplot(data = swingsFF2, aes(x = release_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.0049",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FFplot1
FFplot1 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))


FFplot2 <-
  ggplot(data = swingsFF2, aes(x = effective_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Perceived Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Perceived Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.0043",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FFplot2
FFplot2 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))

pR2(logit_veloFF)
pR2(logit_PVFF)

summarise(swingsFF2,
          velo = mean( (whiff - phat_velo)^2),
          PV = mean( (whiff - phat_PV)^2))

#What the heck that's insane, PV has a higher Brier Score and a lower McFadden's R-Squared for four-seamers
#Wait so if PV is better for all fastballs but not four-seamers is it better for sinkers or cutters?

swingsFC <-
  swings_85 %>% 
  filter(pitch_type == 'FC')

logit_veloFC <- glm(whiff~release_speed, family = binomial, data = swingsFC)
logit_veloFC
logit_PVFC <- glm(whiff~effective_speed, family = binomial, data = swingsFC)
logit_PVFC

swingsFC2 <-
  swingsFC %>% 
  add_predictions(model = logit_veloFC, type = 'response', var = 'phat_velo') %>% 
  add_predictions(model = logit_PVFC, type = 'response', var = 'phat_PV')

FCplot1 <-
  ggplot(data = swingsFC2, aes(x = release_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.00014",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FCplot1
FCplot1 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))


FCplot2 <-
  ggplot(data = swingsFC2, aes(x = effective_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Perceived Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Perceived Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.00022",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FCplot2
FCplot2 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))

pR2(logit_veloFC)
pR2(logit_PVFC)

summarise(swingsFC2,
          velo = mean( (whiff - phat_velo)^2),
          PV = mean( (whiff - phat_PV)^2))

#Yes, PV does have a higher R-squared and lower Brier Score for cutters than four-seamers, but the relationships
#Are much weaker in the first place

swingsSI <-
  swings_85 %>% 
  filter(pitch_type == 'SI')

logit_veloSI <- glm(whiff~release_speed, family = binomial, data = swingsSI)
logit_veloSI
logit_PVSI <- glm(whiff~effective_speed, family = binomial, data = swingsSI)
logit_PVSI

swingsSI2 <-
  swingsSI %>% 
  add_predictions(model = logit_veloSI, type = 'response', var = 'phat_velo') %>% 
  add_predictions(model = logit_PVSI, type = 'response', var = 'phat_PV')

SIplot1 <-
  ggplot(data = swingsSI2, aes(x = release_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.0018",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
SIplot1
SIplot1 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))


SIplot2 <-
  ggplot(data = swingsSI2, aes(x = effective_speed, y = whiff)) +
  #geom_point(size = 0.5, alpha = 0.01) +
  geom_smooth(method = glm, method.args = list(family="binomial"), size = 0.75) +
  #xlim(c(85,104)) +
  #ylim(c(0,1)) +
  labs(
    x = "Perceived Velocity",
    y = "Whiff probability",
    title = "Predicting Whiff Probability from Perceived Velocity Using Logistic Regression",
    subtitle = "McFadden's R-Squared = 0.0021",
    caption = "Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
SIplot2
SIplot2 + scale_y_continuous(breaks=seq(0.15,0.35,0.05))

pR2(logit_veloSI)
pR2(logit_PVSI)

summarise(swingsSI2,
          velo = mean( (whiff - phat_velo)^2),
          PV = mean( (whiff - phat_PV)^2))

#The same is true for sinkers as cutters except the sinker relationships are much stronger than the cutter relationships
#Except the Brier Scores are really low for sinkers and I don't know why

#Anyway let's get back to running the WRAE for fastballs
write_csv(swingsFF2, 'swingsFFchecking.csv')
swingsFF2 <- read_csv(file = 'swingsFFchecking.csv')

swingsFF2 <- select(swingsFF2, player_name, pitch_type, game_year, release_speed, effective_speed, release_extension, description, release_spin_rate, estimated_woba_using_speedangle, woba_value, woba_denom, whiff, PV_diff, phat_velo, phat_PV)

FFswingsWRAE <-
  swingsFF2 %>% 
  select(PV_diff, release_speed, effective_speed, phat_velo, whiff) %>% 
  group_by(release_speed, PV_diff) %>% 
  mutate(whiff_rate_each = mean(whiff)) %>% 
  ungroup() %>% 
  mutate(WRAE = whiff_rate_each - phat_velo)

FFswingsWRAE_0 <-
  FFswingsWRAE %>% 
  group_by(release_speed) %>% 
  add_count(PV_diff, name = 'PV_diff_at_each_velo_count') %>% 
  ungroup() %>% 
  add_count(PV_diff, name = 'PV_diff_count') %>% 
  mutate(percentage_of_each_PV_diff_at_each_velo = PV_diff_at_each_velo_count / PV_diff_count) %>% 
  select(PV_diff, release_speed, effective_speed, phat_velo, whiff_rate_each, WRAE, PV_diff_at_each_velo_count, PV_diff_count, percentage_of_each_PV_diff_at_each_velo) %>% 
  distinct() %>% 
  mutate(WRAE_final_0 = percentage_of_each_PV_diff_at_each_velo * WRAE) %>% 
  group_by(PV_diff) %>% 
  mutate(WRAE_final = sum(WRAE_final_0)) %>% 
  ungroup()
FFswingsWRAE_0

#Have to do this so I don't graph duplicate points on top of each other messing up the regression line
FFswingsWRAE_0_graph <-
  FFswingsWRAE_0 %>% 
  select(PV_diff, WRAE_final) %>% 
  distinct()

FFWRAE_reg_1 <- lm(data = FFswingsWRAE_0_graph, WRAE_final ~ PV_diff)
FFWRAE_reg_1
FFWRAE_reg_plot_1<-
  ggplot(data=FFswingsWRAE_0_graph)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = -0.028962, slope = 0.007031, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 1 swing)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = 0.01",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FFWRAE_reg_plot_1
summary(FFWRAE_reg_1)

#Ok time to do the minimums again
FFswingsWRAE_0_graph_0 <-
  FFswingsWRAE_0 %>% 
  select(PV_diff, WRAE_final, PV_diff_count) %>% 
  filter(PV_diff_count > 2500) %>% 
  distinct()

FFWRAE_reg_1a <- lm(data = FFswingsWRAE_0_graph_0, WRAE_final ~ PV_diff)
FFWRAE_reg_1a
FFWRAE_reg_plot_1a<-
  ggplot(data=FFswingsWRAE_0_graph_0)+
  geom_point(mapping = aes(x = PV_diff , y = WRAE_final), col = "red") +
  geom_abline(intercept = 0.001189, slope = -0.003332, col = "blue") +
  labs(
    x="Perceived Velocity Difference (min 500 swings)",
    y="WRAE",
    title = "WRAE at each PV difference",
    subtitle = "R-squared = 0.2192",
    caption="Data from 7/23/2020-7/24/2021, from Baseball Savant") +
  theme_bw()
FFWRAE_reg_plot_1a
summary(FFWRAE_reg_1a)

#Ok increasing the minimums actually makes this worse. That's really weird


#Since having a large PV_diff doesn't create as high of a WRAE as I expected, (and not really at all for four-seamers),
#Let's take a look at the pitchers with the highest and lowest WRAEs. Maybe we can find a confounding factor
#By analyzing the pitchers and seeing what characteristics they share.
#To do this, we will take the expected whiff rate based on velocity, and subtract that from either 1 (whiff) or
#0 (no whiff) for each pitch. Then instead of taking a weighted average based on the PV_diff, we'll take
#A weighted average based on the player/number of pitches.
#This might be getting outside of the original goal of the project, but if I can find something else besides
#Velocity that helps pitchers get whiffs (spin rate helps but not as much as expected), that would be cool

swings2_player <-
  swings2 %>% 
  mutate(WRAE = whiff - phat_velo) %>% 
  group_by(player_name) %>% 
  mutate(WRAE_average = mean(WRAE)) %>% 
  mutate(expected_whiff_rate = mean(phat_velo)) %>% 
  mutate(velo_avg = mean(release_speed)) %>% 
  mutate(spin_avg = mean(release_spin_rate)) %>% 
  mutate(PV_diff_avg = mean(PV_diff)) %>% 
  add_count(player_name, name = 'swing_count') %>%
  ungroup() %>% 
  select(player_name, swing_count, velo_avg, spin_avg, PV_diff_avg, expected_whiff_rate, WRAE_average) %>% 
  distinct() %>% 
  filter(swing_count > 100) %>% 
  arrange(desc(WRAE_average))

player_whiffs <-
  swings2 %>% 
  filter(whiff == 1) %>% 
  group_by(player_name) %>% 
  add_count(player_name, name = 'whiffs') %>%
  select(player_name, whiffs) %>% 
  ungroup()

swings2_player <-
  swings2_player %>% 
  inner_join(player_whiffs, by = 'player_name') %>% 
  distinct() %>% 
  mutate(whiff_rate = whiffs/swing_count)
  
summarize(swings2_player, correlation = cor(expected_whiff_rate, WRAE_average))

#Ok that wasn't actually that hard lmaoooo I really overcomplicated that
#I could've just run a regression between velocity and whiff rate and then taken the residuals LMAO

#I'm going to write up my findings so far basically about how PV stuff isn't as important as I thought

#Have to do this for article
densityplot<- density(swings2$PV_diff)
plot(densityplot, main="KDE Plot of Difference between Perceived Velocity and Actual Pitch Velocity")
polygon(densityplot, col="grey", border="blue")