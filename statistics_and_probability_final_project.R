library(ggplot2)

data <- read.csv(file.choose(), header = T)

########## Data cleaning & preparation

# We need just caffeine drinking population
table(data$prefered_drink)
caffeine_drinkers_sample <- subset(data, drinks_caffeine != "No")
table(caffeine_drinkers_sample$prefered_drink)
# More detailed analysis revealed, that many people likely hurried to complete
# all the surveys during the class, and pressed the buttons not carefully.
# Since we have 3 people who pressed "No" for "Do you drink caffeine drinks?",
# however stated the amount of cups drunk and favorite drink.

# At this point I decided not to drop "drinks_caffeine" rows with "No" response,
# but filter and drop people who didn't state their preferred drink.

caffeine_drinkers_sample <- subset(data, prefered_drink != "" & cups_daily != "")
nrow(caffeine_drinkers_sample)

# Getting rid of the useless column.
caffeine_drinkers_sample$drinks_caffeine <- NULL
caffeine_drinkers_sample$Timestamp <- NULL

# The sample size is 39. Great.

# Now - since I am interested to test the hypothesis about
# not just the amount of coffee cups, but also its
# equivalents - I have to convert other caffeine containing drinks
# into equivalent number of coffee cups.

# Among the stated preferred drinks students reported such labels as:
table(caffeine_drinkers_sample$prefered_drink)
# - Coffee,
# - Energy drink,
# - Black/green tea,
# - Matcha,
# - Coke/Pepsi/Dr.Pepper,
# - Coffee, Oolong/Green/Black tea (1 person drinks all of this drinks, lets assume with equal frequency, if all were stated as equally preffered).


# 1 Grande late/cappuccino/americano contain 2 espresso shots = 150 mg of caffeine

# Most common energy drinks sold in the Groove are RockStar (= 160 mg) and Redbull (= 140 mg). So on average (= 150 mg), what is equal to coffee.
# Following this logic - conversion multiplier for the energy drink is 1.

# Black / Green / Oloong (1 teabag), Matcha drinks from Starbucks or Coke/Pepsi/Dr.Pepper all on average contain 60 mg of caffeine,
# what is 2.5 times less then in 1 cup of coffee.
# Following this logic - conversion multiplier for tea is (2.5/1=) 0.4.

# A sample with the label reported "Coffee, Oolong/Green/Black tea" can be classified as average between the 2 separate groups both of which they selected.
# Following this logic - conversion multiplier for the person drinking Coffee, Oolong/Green/Black tea is ((1+0.4)/2 =) 0.7

conversion_map <- c(
  "Coffee" = 1.0,
  "Coffee, Oolong/Green/Black tea" = 0.7,
  "Black/green tea" = 0.4,
  "Coke/Pepsi/Dr pepper" = 0.4,
  "Energy drink" = 1.0,
  "Matcha" = 0.4,
  "matcha AND black/green tea" = 0.4
)

# So, transforming all other drinks into coffee cup equivalents:

for (drink in names(conversion_map)) {
  idx <- caffeine_drinkers_sample$prefered_drink == drink
  caffeine_drinkers_sample$coffee_cups_equiv[idx] <- caffeine_drinkers_sample$cups_daily[idx] * conversion_map[drink]
  caffeine_drinkers_sample$prefered_drink[idx] <- "Coffee"  # Standardize label
}

# Great, now we have all drinks converted to coffee cups equivalents
table(caffeine_drinkers_sample$prefered_drink)

# Getting rid of the unnecessary columns.
caffeine_drinkers_sample$cups_daily <- NULL
caffeine_drinkers_sample$prefered_drink <- NULL

# After transforming the data for caffeine drinkers - let's add back the population who were filtered away because of not drinking coffee.
nrow(data) - nrow(caffeine_drinkers_sample)
# 11 people drink zero cups of coffee, and don't drink any other coffee

not_caffeine_drinkers_sample <- subset(data,
                                          prefered_drink == "" | is.na(prefered_drink) |
                                          cups_daily == "" | is.na(cups_daily)
)
nrow(not_caffeine_drinkers_sample)
not_caffeine_drinkers_sample$drinks_caffeine <- NULL
not_caffeine_drinkers_sample$Timestamp <- NULL
not_caffeine_drinkers_sample$cups_daily <- NULL
not_caffeine_drinkers_sample$prefered_drink <- NULL
not_caffeine_drinkers_sample$coffee_cups_equiv <- 0.0

sample <- rbind(caffeine_drinkers_sample, not_caffeine_drinkers_sample)
nrow(sample)

# Not a single row was lost, but since I want to test the claim
# just about the students with 15 units and more, I will filter my sample once again.

sample <- sample[ sample$units_per_semester >= 15 & !is.na(sample$units_per_semester), ]

# Finally, data is prepared for the following hypothesis test
nrow(sample)
# The final sample size is 34, what works for 1-Population mean hypothesis test.

########## Histogram

coffee_cups_basic_histogram <- ggplot(sample, aes(sample$coffee_cups_equiv))
coffee_cups_basic_histogram + geom_histogram(col = "black", fill = "grey", breaks =
                                               seq(min(sample$coffee_cups_equiv), max(sample$coffee_cups_equiv), by = 0.5), lwd = 1.5) +
  labs(x = "Coffee cups drinked on the average day", y = "Frequency", title = "Basic Histogram for the Daily Coffee Cups") +
  theme(plot.title = element_text(hjust = 0.5))

########## QQ-Plot

qqnorm(sample$coffee_cups_equiv, main = "QQ Plot for the Coffee Cups distribution")

########## Mean, the standard deviation, the 5-number summary, and the sample size.

summary(sample$coffee_cups_equiv)
sd(sample$coffee_cups_equiv)
length(sample$coffee_cups_equiv)

# 1-Population Mean test

t.test(sample$coffee_cups_equiv, mu = 2, alternative = "less")









