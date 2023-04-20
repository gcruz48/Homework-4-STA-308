## STA 308 Homework #4: Birthday Problem
## Author: Gio Cruz

library(tidyverse)

## Function to generate the random birthday for each individual
birthdayGenerate <- function(){
  sample(1:365, size = 1)
}

## Function to check for shared birthdays and return the number of duplicates
birthdayCheck <- function(num_people=23){
  bday_dates <- c()
  for(i in 1:num_people){
    bday_dates <- c(bday_dates,
                      birthdayGenerate())
  }
  dupes <- sum(duplicated(bday_dates))
  dupes
}
birthdayCheck()

# Simulation for 10 people
avg_bday_dupes10 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes10 <- c(avg_bday_dupes10,
                      birthdayCheck(num_people=10))
}
summary(avg_bday_dupes10)
bar10 <- table(avg_bday_dupes10)
barplot(bar10, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is right skewed with the majority of the iterations
## resulting in 0 shared birthdays.

# Simulation for 20 people
avg_bday_dupes20 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes20 <- c(avg_bday_dupes20,
                        birthdayCheck(num_people=20))
}
summary(avg_bday_dupes20)
bar20 <- table(avg_bday_dupes20)
barplot(bar20, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is again mostly 0 shared birthdays, but with more instances
## of 1 or more shared birthdays than with number of people at 10.

# Simulation for 30 people
avg_bday_dupes30 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes30 <- c(avg_bday_dupes30,
                        birthdayCheck(num_people=30))
}
summary(avg_bday_dupes30)
bar30 <- table(avg_bday_dupes30)
barplot(bar30, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is skewed right, with a center around 1 shared birthday

# Simulation for 40 people
avg_bday_dupes40 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes40 <- c(avg_bday_dupes40,
                        birthdayCheck(num_people=40))
}
summary(avg_bday_dupes40)
bar40 <- table(avg_bday_dupes40)
barplot(bar40, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is right skewed with a center around 2 shared birthdays.
## Less skewed and more of a bell shaped curve compared to the previous simulations.

# Simulation for 50 people
avg_bday_dupes50 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes50 <- c(avg_bday_dupes50,
                        birthdayCheck(num_people=50))
}
summary(avg_bday_dupes50)
bar50 <- table(avg_bday_dupes50)
barplot(bar50, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## This distribution is slightly skewed right, with even more of a bell shaped
## curve than the previous simulations, with a center around 3 shared birthdays.

# Simulation for 60 people
avg_bday_dupes60 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes60 <- c(avg_bday_dupes60,
                        birthdayCheck(num_people=60))
}
summary(avg_bday_dupes60)
bar60 <- table(avg_bday_dupes60)
barplot(bar60, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is getting increasingly more normally distributed as the
## number of people increases. This distribution is once again slightly right
## skewed but getting close to being normally distributed, with a center 
## measure somewhere around 4 or 5.

# Simulation for 70 people
avg_bday_dupes70 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes70 <- c(avg_bday_dupes70,
                        birthdayCheck(num_people=70))
}
summary(avg_bday_dupes70)
bar70 <- table(avg_bday_dupes70)
barplot(bar70, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is normally distributed around the center measure of 6. 

# Simulation for 80 people
avg_bday_dupes80 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes80 <- c(avg_bday_dupes80,
                        birthdayCheck(num_people=80))
}
summary(avg_bday_dupes80)
bar80 <- table(avg_bday_dupes80)
barplot(bar80, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is again a bell shaped, normal distribution around the 
## center measure of 8 shared birthdays.

# Simulation for 90 people
avg_bday_dupes90 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes90 <- c(avg_bday_dupes90,
                        birthdayCheck(num_people=90))
}
summary(avg_bday_dupes90)
bar90 <- table(avg_bday_dupes90)
barplot(bar90, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## The distribution is normally distributed around the center measure of 10.

# Simulation for 100 people
avg_bday_dupes100 <- c()
set.seed(314159)
for(i in 1:100000){
  avg_bday_dupes100 <- c(avg_bday_dupes100,
                        birthdayCheck(num_people=100))
}
summary(avg_bday_dupes100)
bar100 <- table(avg_bday_dupes100)
barplot(bar100, main="Distribution of Duplicate Birthdays",
        xlab="Number of Shared Birthdays")
## For 100 people, the distribution is a normal or bell-curve distribution
## around the median of 12. There is at least 1 shared birthday in every
## iteration of the simulation. This embodies the Central Limit Theorem as
## n grows the sample mean behaves as a normal random variable.