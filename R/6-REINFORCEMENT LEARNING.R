
# REINFORCEMENT LEARNING.

# The aim is to give to each user which log into the web site an ad. 
# Actually only one ad is shown to the user due to the narrowness of budget.
# We want to seek the best ad for us since showing the ad to a user isn't free.
# In this situation, we haven't any data and consequently, each time the user log,
# we propose an ad and we save the reward. It's a real time process 
# which is performed in reality. Each result depends on the previous result,
# indeed this procedure is also known as online procedure. 
#The dataset assumes we know the result for each user, so we don't perform 
# the previous strategy.

dataset = read.csv('Dataset/Ads_CTR_Optimisation.csv')


# IMPLEMENTING THE RANDOM SELECTION: there isn't a relation between
# the choices.
N = 10000
d = 10
ads_selected = integer(0)
total_reward = 0

for (n in 1:N){
  ad = sample(1:10, 1)
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  total_reward = total_reward + reward
}

hist(ads_selected,
     col = 'blue')


# IMPLEMENTING THE UCB.

ads_selected = integer()               # the best ad for each user.
numbers_of_selections = integer(d) # number of times ads is selected.
sums_of_rewards = integer(d)       
total_reward = 0

# loop over users.
for (n in 1:N){
  ad = 0
  max_upper_bound = 0
# loop over ads.
  for (i in 1:d){
    if (numbers_of_selections[i] > 0){
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n + 1) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else { 
    upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = c(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}


hist(ads_selected,
     col = 'blue')

# the total reward is much higher than the previous one.
# from one point, only one ad is selected: the exploited part begin. 




# Thompson Sampling

# Importing the dataset
dataset = read.csv('Ads_CTR_Optimisation.csv')

# Implementing Thompson Sampling
N = 10000
d = 10
ads_selected = integer(0)
numbers_of_rewards_1 = integer(d)
numbers_of_rewards_0 = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n = 1,
                        shape1 = numbers_of_rewards_1[i] + 1,
                        shape2 = numbers_of_rewards_0[i] + 1)
    if (random_beta > max_random) {
      max_random = random_beta
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  if (reward == 1) {
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')





















