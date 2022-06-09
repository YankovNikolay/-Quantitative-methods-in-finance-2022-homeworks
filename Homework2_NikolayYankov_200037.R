#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.

Bet = 1
Money = 100
while (Money > 0){
  cointoss = sample (c("win", "loss"), 1, prob = c(0.486,0.514))
  if (cointoss =='win'){
    Money = Money + Bet
    Bet = 1
  } else {
    Money = Money - Bet
    Bet = min(Money, Bet*2)
  }
  print(Money)
}

#####Problem 1#####


#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html
#Do all the exercises:
# 5.2.4 Exercises 
# 5.3.1 Exercises 
# 5.4.1 Exercises 
# 5.5.2 Exercises 
#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html

####Problem 5.2.4####

#Problem 5.2.4.1
delay2 = filter(flights, arr_delay >= 120 )
destin = filter(flights, dest == "IAH" | dest == "HOU" )
carri_er = filter(flights, carrier %in% c("AA", "DL", "UA"))
summer = filter(flights, month >= 7, month <= 9)
late_notlate = filter(flights, arr_delay > 120, dep_delay <= 0)
made_up = filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
midnight = filter(flights, dep_time <= 600 | dep_time == 2400)

#Problem 5.2.4.2 
summer2_0 = filter(flights, between(month, 7, 9))

#Problem 5.2.4.3
misInfor = filter(flights, is.na(dep_time))

#Problem 5.2.4.4 
NA ^ 0 # every number powered on 0 is 1 

#Problem 5.3.1
#1 
arrangeNA = arrange(flights, desc(is.na(dep_time)), dep_time)
#2 
topDelayed = arrange(flights, desc(dep_delay))
#3 
fastest = arrange(flights, desc(distance / air_time))
#4
farthest = arrange(flights, desc(distance))

#Problem 5.4.1
#1
select(flights, year, month, day)
select(flights, "year", "month", "day")
select(flights, 1, 2, 3)
#2
select(flights, year, month, year, year) #it does not include them 
#3 
choice = c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(choice))

#Problem 5.5.2
#1
Times_change = mutate(flights,
                       dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                       sched_dep_time_mins = (sched_dep_time %/% 100 * 60 +
                                                sched_dep_time %% 100) %% 1440)
#4 
rankme = tibble(
  x = c(10, 5, 1, 5, 5)
)

#####Problem 2#####