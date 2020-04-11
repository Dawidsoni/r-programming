library("tidyr")

# Task 1
paired_dancers <- read.csv2(url("http://www.math.uni.wroc.pl/~dyba/materials/R/tancerze.csv"))
male_dancers <- paired_dancers[, -2]
colnames(male_dancers) <- c("pair_number", "name")
male_dancers["gender"] <- "M"
female_dancers <- paired_dancers[, -3]
colnames(female_dancers) <- c("pair_number", "name")
female_dancers["gender"] <- "F"
merged_dancers <- rbind(male_dancers, female_dancers)
named_dancers <- separate(merged_dancers, "name", into=c("surname", "firstname"), sep=", ")

# Task 2
named_dancers["height"] <- round(rnorm(200, 5.5, 0.5), 2)
named_dancers["age"] <- sample(20:30, 200, replace=TRUE)

# Task 3
ordered_dancers <- named_dancers[order(named_dancers$surname), ]

# Task 4
ordered_dancers["height"] <- ordered_dancers["height"] * 30.48

# Task 5
ordered_dancers["gender"] <- factor(ordered_dancers[, "gender"], labels=c("Female", "Male"))

# Task 6
print(mean(ordered_dancers[ordered_dancers["gender"] == "Male", "height"]))
print(mean(ordered_dancers[ordered_dancers["gender"] == "Female", "height"]))
print(mean(ordered_dancers[ordered_dancers["height"] >= 170, "age"]))
print(mean(ordered_dancers[ordered_dancers["height"] < 170, "age"]))

# Task 7
rm(named_dancers, merged_dancers, paired_dancers)
rm(male_dancers, female_dancers, ordered_dancers)