######################
##### HW 11 ######
## Sophia Mummert ####

## libraries ##
library(readxl)
library(dplyr)
library(ggplot2)

# set seed
set.seed(123)

# simulated IDs
n <- 100

# create a two-level categorical variable
season = factor(rep(c("summer", "winter"), each = n/2))

# continuous covariate x
time_spent_foraging = runif(n, min = 15, max = 80)

# parameters
beta_0 = 6  # intercept for Summer
beta_1 = -.05 # slope Summer
beta_2 = 3 # intercept dif for Winter
beta_3 = -.10 # slope dif for Winter

# varience
error = rlnorm(n, meanlog = 0, sdlog = 1)

# generate y depending on group
time_spent_resting = beta_0 +
  beta_1 * time_spent_foraging +
  beta_2 * (season == "winter") +
  beta_3 * time_spent_foraging * (season == "winter") +
  error

# dataframe
df <- data.frame(time_spent_resting, time_spent_foraging, season)
head(df)

ggplot(df, aes(x = time_spent_foraging, y = time_spent_resting, color = season)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  labs(title = "Simulation",
       x = "Mean time spent foraging hours per week",
       y = "Mean time spent resting hours per week")

write.csv(df, "baboon_sim_data.csv", row.names = FALSE)

######################## OBJ 1 #################################
# The chacma baboons at NVBP faces ecological pressure in the winter season
# in nutrition available in their Afrotemperate habitat. With this seasonal
# flux in nutrient availability, they face changes to activity budgets, seeing
# a decrease in activities such as resting. This is due to an
# increased amount of time spent on foraging, as their preferred foodds are not
# available. 

# Does the chacma baboon's face a seasonal difference in time spent resting
# in response to the time spent foraging?

#Two-level factor (Season): Summer vs. Winter 
#Continuous predictor: Mean time spent foraging in hours
#Response Variable: Mean time spent resting in hours 

######################## OBJ 2 ##### (from Maddie) ############
#Scenario: Primate habitats are often fragmented due to anthropogenic pressures
#such as deforestation and land-use change.Researchers are intereste in 
#understanding how habitat type (primary vs. secondary forest) and temperature 
#influence the daily travel distance of primates, and whether the effect of
#temperature on travel distance differs between habitat types due to differing 
#food distributions and tree cover.

#Question: How does temperature and habitat affect travel distance, and do monkeys 
#in Primary and Secondary forests respond differently to changes in mean daily 
#temperature in terms of their daily travel distance?

#Variables:
#   Two-level factor: habitat ("Primary" and "Secondary")
#   Continuous predictor: meanDailyTemp_C (mean daily temperature in Celsius)
#   Response variable: meanDailyDistance_km (mean daily travel distance in kilometers)


# pull in Maddie's sim data
monkey_travel_df = read.csv("monkey_travel_data.csv")

# Check with model 1
model1 = lm(meanDailyDistance_km ~ meanDailyTemp_C * Habitat, data = monkey_travel_df)
anova(model1)
#can drop meanDailyTemp_C:habitat bc not significant

# fit model 2
model2 = lm(meanDailyDistance_km ~ meanDailyTemp_C + Habitat, data = monkey_travel_df)
anova(model2)

#meanDailyDistance_km: p < .05 significant
#Habitat: p < .05 significant
#meanDailyTemp_C:Habitat: p > .05 not significant

summary(model2)
###estimates:###
#Beta_0 = 5.9537, Beta_1 = -0.14, Beta_2 .43574, Beta_3 = -0.01 (?)

# These results show that mean daily temp had significant effects on daily travel 
# distance, such that when mean temp increased, daily distance in km decreased. 
# Additionally, living in a primary or secondary forest showed a significant
# difference in mean daily travel distances, such that in secondary forests
# primates are traveling more per day. 
# However, since mean daily temp x habitat differences was not significant, 
# that means that primary and secondary forests do not respond differently to
# temperature changes. This would mean the graph of these are showing parallel 
# slopes. 

# running a plot of Maddie's to see it
ggplot(monkey_travel_df, aes(x = meanDailyTemp_C, y = meanDailyDistance_km, color = Habitat)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() 

# I was correct about Maddie's ecological question. My parameters were 
# beta_0 = 4, beta_1 = -0.12 beta_2 = 2, beta_3 = -0.05, which were different
# compared to my estimates. I think I am confused on the beta_3 metric, because
# I didn't know where to pull that from on the model2 ANOVA test. I just went based
# off of assuming that the difference in slope would be slight.
