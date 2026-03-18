

# I found there are approximately 300,000 car crashes resulting in property damage in California per year.
# So I will generete a 300000 car crashes data with infos


#Below, I generate data of 5 million cars in California. I try to make it realistic by randomization with real percentages.

set.seed(123)   

n <- 5000000

# 1) Car Models 
car_models <- c(
  "Ford F-Series",
  "Chevrolet Silverado",
  "Toyota RAV4",
  "Tesla Model Y",
  "Honda CR-V",
  "Ram Pickups",
  "GMC Sierra",
  "Toyota Camry",
  "Nissan Rogue",
  "Honda Civic",
  "Toyota Corolla",
  "Jeep Grand Cherokee",
  "Chevrolet Equinox",
  "Hyundai Tucson",
  "Chevrolet Trax",
  "Ford Explorer",
  "Toyota Tacoma",
  "Subaru Crosstrek",
  "Subaru Forester",
  "Toyota Highlander",
  "Honda Accord",
  "Kia Sportage",
  "Subaru Outback",
  "Toyota Tundra",
  "Nissan Sentra"
)

# 2024 sales
car_sales <- c(
  732139,  # Ford F-Series
  542517,  # Chevrolet Silverado
  475193,  # Toyota RAV4
  405900,  # Tesla Model Y
  402791,  # Honda CR-V
  373120,  # Ram Pickups
  340946,  # GMC Sierra
  309876,  # Toyota Camry
  245724,  # Nissan Rogue
  242005,  # Honda Civic
  232908,  # Toyota Corolla
  216148,  # Jeep Grand Cherokee
  207730,  # Chevrolet Equinox
  206126,  # Hyundai Tucson
  200689,  # Chevrolet Trax
  194094,  # Ford Explorer
  192813,  # Toyota Tacoma
  181811,  # Subaru Crosstrek
  175521,  # Subaru Forester
  169543,  # Toyota Highlander
  161917,  # Honda Accord
  161814,  # Kia Sportage
  159528,  # Subaru Outback
  159528,  # Toyota Tundra
  152659   # Nissan Sentra
)

# Shares
car_probs <- car_sales / sum(car_sales)

# Randomization
car_model <- sample(car_models, n, replace = TRUE, prob = car_probs)

# 2) Driver Gender
driver_gender <- sample(c("Male", "Female"),
                        n, replace = TRUE, prob = c(0.51, 0.49)) #checked real life percentage

# 3) Driver Age Group
driver_age_group <- sample(c("18-24", "25-44", "45-64", "65+"), n, replace = TRUE, prob = c(0.12, 0.40, 0.35, 0.13))

# 4) Model Year Group (Age year) I assume new models are more than the old
model_year <- sample(2000:2024, n, replace = TRUE,
                     prob = seq(1,25)/sum(seq(1,25)))

# 5) County (10bigggest counties in California + Other)
counties <- c(
  "Los Angeles",
  "San Diego",
  "Orange",
  "Riverside",
  "San Bernardino",
  "Santa Clara",
  "Alameda",
  "Sacramento",
  "Contra Costa",
  "Fresno",
  "Other"
)

county_probs <- c(
  0.25, # Los Angeles
  0.10, # San Diego
  0.09, # Orange
  0.08, # Riverside
  0.08, # San Bernardino
  0.06, # Santa Clara
  0.05, # Alameda
  0.05, # Sacramento
  0.04, # Contra Costa
  0.03, # Fresno
  0.17  # Other
)

county <- sample(counties, n, replace = TRUE, prob = county_probs)

driver_drinkalcohol <- sample(c(TRUE,FALSE), n, replace = TRUE, prob = c(0.04,0.96))

previous_accidents <- sample(0:3, n, replace = TRUE, prob = c(0.85,0.11,0.03,0.01))

car_value <- round(35000 * (0.98^(2024 - model_year)) + rnorm(n, 0, 2000))

california_cars <- data.frame(
  car_model,
  model_year,
  driver_age_group,
  driver_gender,
  county,
  driver_drinkalcohol,
  previous_accidents 
)

head(california_cars, 10)

# I found there are approximately 300,000 car crashes resulting in property damage in California per year.
set.seed(123)
t=300000

# Sample car models
crash_car_sums<-  c(
  892139,  # Ford F-Series
  692517,  # Chevrolet Silverado
  305193,  # Toyota RAV4
  301900,  # Tesla Model Y
  372791,  # Honda CR-V
  533120,  # Ram Pickups
  540946,  # GMC Sierra
  210876,  # Toyota Camry
  245724,  # Nissan Rogue
  342005,  # Honda Civic
  312908,  # Toyota Corolla
  406148,  # Jeep Grand Cherokee
  207730,  # Chevrolet Equinox
  103126,  # Hyundai Tucson
  200689,  # Chevrolet Trax
  194094,  # Ford Explorer
  192813,  # Toyota Tacoma
  141811,  # Subaru Crosstrek
  105521,  # Subaru Forester
  119543,  # Toyota Highlander
  101917,  # Honda Accord
  191814,  # Kia Sportage
  99528,  # Subaru Outback
  199528,  # Toyota Tundra
  241659   # Nissan Sentra
)
crash_car_prob<-crash_car_sums/sum(crash_car_sums)
crash_car_model <- sample(car_models, t, replace = TRUE, prob = crash_car_prob)
crash_model_year <- sample(2000:2024, t, replace = TRUE,
                           prob = seq(1,25)/sum(seq(1,25)))#I assume new cars are more than the old cars again


crash_driver_age <- sample(18:80, t, replace = TRUE, prob = dnorm(18:80, mean=28, sd=20))

crash_driver_gender <- sample(c("Male","Female"), t, replace = TRUE, prob = c(0.55,0.45))

crash_county <- sample(counties, t, replace = TRUE, prob = county_probs)

crash_driver_alcohol <- sample(c(TRUE,FALSE), t, replace = TRUE, prob = c(0.15,0.85))

crash_previous_accidents <- sample(0:3, t, replace = TRUE, prob = c(0.79,0.14,0.05,0.02))

crash_car_value <- round(35000 * (0.98^(2024 - crash_model_year)) + rnorm(t, 0, 2000))

# Random proportion of car value (damage ratio)
damage_ratio <- rbeta(t, shape1 = 2, shape2 = 8)  

# Property damage cost based on car value * ratio
property_damage_cost <- round(crash_car_value * damage_ratio)

# Minimum 200 USD
property_damage_cost[property_damage_cost < 200] <- 200
property_damage_cost
# Combine all variables into one dataframe
crash_data <- data.frame(
  crash_car_model,
  crash_model_year,
  crash_driver_age,
  crash_driver_gender,
  crash_county,
  crash_driver_alcohol,
  crash_previous_accidents,
  crash_car_value,
  property_damage_cost = property_damage_cost
)

# Preview first 10 rows
head(crash_data, 10)

# Quick summary check
summary(crash_data)


crash_model_percent <- prop.table(table(crash_data$crash_car_model))
crash_model_percent
crash_model_df=as.data.frame(crash_model_percent)
crash_model_df
colnames(crash_model_df) <- c("car_model", "crash_share")

cal_model_percent<-prop.table(table(california_cars$car_model))
cal_model_df<-as.data.frame(cal_model_percent)
colnames(cal_model_df)<-c('car_model', 'model_share')
cal_model_df

# Merge crash and fleet model share data by car_model
model_risk_df <- merge(cal_model_df, crash_model_df, by = "car_model")
model_risk_df$model_risk_index<- model_risk_df$crash_share/model_risk_df$model_share
model_risk_df
# Sort by risk index descending
model_risk_df <- model_risk_df[order(-model_risk_df$model_risk_index), ]
model_risk_df



# Compute risk index for model_year
# Calculate relative risk index for model_year
cal_table <- table(california_cars$model_year)
crash_table <- table(crash_data$crash_model_year)

cal_share <- prop.table(cal_table)
crash_share <- prop.table(crash_table)

cal_df <- as.data.frame(cal_share)
crash_df <- as.data.frame(crash_share)

colnames(cal_df) <- c("model_year", "cal_share")
colnames(crash_df) <- c("model_year", "crash_share")

year_risk_df <- merge(cal_df, crash_df, by = "model_year")
year_risk_df$year_risk_index <- year_risk_df$crash_share / year_risk_df$cal_share
year_risk_df <- year_risk_df[order(-year_risk_df$year_risk_index), ]
year_risk_df


# Create age groups in crash_data (same logic as cal)
crash_data$driver_age_group <- cut(
  crash_data$crash_driver_age,
  breaks = c(17, 24, 44, 64, Inf),
  labels = c("18-24", "25-44", "45-64", "65+"),
  right = TRUE
)

# Compute share tables
cal_table <- table(california_cars$driver_age_group)
crash_table <- table(crash_data$driver_age_group)

cal_share <- prop.table(cal_table)
crash_share <- prop.table(crash_table)

cal_df <- as.data.frame(cal_share)
crash_df <- as.data.frame(crash_share)

colnames(cal_df) <- c("driver_age_group", "cal_share")
colnames(crash_df) <- c("driver_age_group", "crash_share")

# Merge and calculate risk index
age_risk_df <- merge(cal_df, crash_df, by = "driver_age_group")
age_risk_df$age_risk_index <- age_risk_df$crash_share / age_risk_df$cal_share
age_risk_df <- age_risk_df[order(-age_risk_df$age_risk_index), ]
age_risk_df


# Compute alcohol share in cal and crash datasets
cal_table <- table(california_cars$driver_drinkalcohol)
crash_table <- table(crash_data$crash_driver_alcohol)

cal_share <- prop.table(cal_table)
crash_share <- prop.table(crash_table)

cal_df <- as.data.frame(cal_share)
crash_df <- as.data.frame(crash_share)

colnames(cal_df) <- c("driver_drinkalcohol", "cal_share")
colnames(crash_df) <- c("driver_drinkalcohol", "crash_share")

# Merge and calculate risk index
alcohol_risk_df <- merge(cal_df, crash_df, by = "driver_drinkalcohol")
alcohol_risk_df$alc_risk_index <- alcohol_risk_df$crash_share / alcohol_risk_df$cal_share
alcohol_risk_df <- alcohol_risk_df[order(-alcohol_risk_df$alc_risk_index), ]
alcohol_risk_df





# Compute previous accidents share in cal and crash datasets
cal_table <- table(california_cars$previous_accidents)
crash_table <- table(crash_data$crash_previous_accidents)

cal_share <- prop.table(cal_table)
crash_share <- prop.table(crash_table)

cal_df <- as.data.frame(cal_share)
crash_df <- as.data.frame(crash_share)

colnames(cal_df) <- c("previous_accidents", "cal_share")
colnames(crash_df) <- c("previous_accidents", "crash_share")

# Merge and calculate risk index
accident_history_risk_df <- merge(cal_df, crash_df, by = "previous_accidents")
accident_history_risk_df$acc_risk_index <- accident_history_risk_df$crash_share / accident_history_risk_df$cal_share
accident_history_risk_df <- accident_history_risk_df[order(-accident_history_risk_df$acc_risk_index), ]
accident_history_risk_df

crash_data



# Model Risk Index
california_cars <- merge(california_cars, model_risk_df[, c("car_model", "model_risk_index")],
                         by = "car_model", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "model_risk_index"


# Year Risk Index
california_cars <- merge(california_cars, year_risk_df[, c("model_year", "year_risk_index")],
                         by = "model_year", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "year_risk_index"

# Age Risk Index
california_cars <- merge(california_cars, age_risk_df[, c("driver_age_group", "age_risk_index")],
                         by = "driver_age_group", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "age_risk_index"

# Gender Risk Index
gender_risk_df <- data.frame(
  driver_gender = c("Male", "Female"),
  risk_index = c(1.1, 0.9)  # Dummy values for now
)
california_cars <- merge(california_cars, gender_risk_df,
                         by = "driver_gender", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "gender_risk_index"

# County Risk Index
county_risk_df <- data.frame(
  county = unique(california_cars$county),
  risk_index = runif(length(unique(california_cars$county)), 0.8, 1.2) # Dummy values
)
california_cars <- merge(california_cars, county_risk_df,
                         by = "county", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "county_risk_index"

# Alcohol Risk Index
california_cars <- merge(california_cars, alcohol_risk_df,
                         by = "driver_drinkalcohol", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "alc_risk_index"

# Accident History Risk Index
california_cars <- merge(california_cars, accident_history_risk_df,
                         by = "previous_accidents", all.x = TRUE)
colnames(california_cars)[ncol(california_cars)] <- "acc_risk_index"

california_cars


california_cars$total_risk_score <- with(california_cars,
                                         model_risk_index *
                                           year_risk_index *
                                           age_risk_index *
                                           gender_risk_index *
                                           county_risk_index *
                                           alc_risk_index *
                                           acc_risk_index
)
california_cars
summary(is.na(california_cars$total_risk_score)) #checked and there is no NA
hist(california_cars$total_risk_score, breaks = 50, main = "Distribution of Total Risk Score")

#Very important! Checking if other variables have a statistically significant effect on damage cost.
model_risk <- lm(property_damage_cost ~ crash_driver_age +
                   crash_model_year +
                   crash_driver_alcohol +
                   crash_previous_accidents +
                   crash_car_model +
                   crash_driver_gender +
                   crash_county,
                 data = crash_data)

summary(model_risk)
# Newer car models tend to cause higher property damage costs. #crash_model_year
# Less populated counties show significantly lower damage costs. #crash_countyOther
# San Diego drivers are associated with slightly lower average damages. #crash_countySanDiego


# Create new working copies of the original datasets
new_cal_data <- california_cars
new_crash_data <- crash_data


install.packages("car")
library(car)



# Standardize column names in crash dataset to match cal dataset
colnames(new_crash_data)[colnames(new_crash_data) == "crash_car_model"] <- "car_model"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_model_year"] <- "model_year"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_driver_age"] <- "driver_age"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_driver_gender"] <- "driver_gender"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_county"] <- "county"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_driver_alcohol"] <- "driver_drinkalcohol"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_previous_accidents"] <- "previous_accidents"
colnames(new_crash_data)[colnames(new_crash_data) == "crash_car_value"] <- "car_value"

# Make sure categorical variables are factors
new_crash_data$car_model <- as.factor(new_crash_data$car_model)
new_crash_data$driver_gender <- as.factor(new_crash_data$driver_gender)
new_crash_data$county <- as.factor(new_crash_data$county)
new_crash_data$driver_drinkalcohol <- as.factor(new_crash_data$driver_drinkalcohol)

# Refit model with factors
model_risk1 <- lm(property_damage_cost ~ driver_age + model_year + driver_drinkalcohol + 
                    previous_accidents + car_model + driver_gender + county,
                  data = new_crash_data)
summary(model_risk1)
vif(model_risk1) #checking if there is any multicollinearity
#all values are <5 and 10 so there is nothing to worry about our model


# Ask user input
cat("Enter customer information:\n")

# Ask user input
driver_age <- as.numeric(readline("Driver age (e.g., 35): "))
model_year <- as.numeric(readline("Car model year (e.g., 2020): "))
driver_drinkalcohol <- readline("Drinks alcohol? (TRUE/FALSE): ")
driver_drinkalcohol <- ifelse(driver_drinkalcohol == "TRUE", TRUE,
                              ifelse(driver_drinkalcohol == "FALSE", FALSE, NA))
previous_accidents <- as.integer(readline("Number of previous accidents (0-3): "))
car_model <- readline("Car model (copy from above list): ")
driver_gender <- readline("Gender (Male/Female): ")
county <- readline("County (copy from above list): ")

# Build data frame for prediction
new_customer <- data.frame(
  driver_age = driver_age,
  model_year = model_year,
  driver_drinkalcohol = factor(driver_drinkalcohol, levels = levels(new_crash_data$driver_drinkalcohol)),
  previous_accidents = previous_accidents,
  car_model = factor(car_model, levels = levels(new_crash_data$car_model)),
  driver_gender = factor(driver_gender, levels = levels(new_crash_data$driver_gender)),
  county = factor(county, levels = levels(new_crash_data$county))
)

new_customer




levels(new_crash_data$car_model)[1:10]
levels(new_crash_data$driver_gender)
levels(new_crash_data$county)
levels(new_crash_data$driver_drinkalcohol)
expected_damage <- predict(model_risk1, newdata = new_customer)
new_customer$expected_damage <- predict(model_risk1, newdata = new_customer)

cat("\n#️⃣ Expected damage cost for this customer: $", round(expected_damage, 2), "\n")


# Map age to age group
age_group <- cut(
  new_customer$driver_age,
  breaks = c(17, 24, 44, 64, Inf),
  labels = c("18-24", "25-44", "45-64", "65+"),
  right = TRUE
)

# Extract all relevant risk index values
model_risk_value   <- model_risk_df$model_risk_index[model_risk_df$car_model == new_customer$car_model]
year_risk_value    <- year_risk_df$year_risk_index[year_risk_df$model_year == new_customer$model_year]
age_risk_value     <- age_risk_df$age_risk_index[age_risk_df$driver_age_group == age_group]
gender_risk_value  <- gender_risk_df$risk_index[gender_risk_df$driver_gender == new_customer$driver_gender]
county_risk_value  <- county_risk_df$risk_index[county_risk_df$county == new_customer$county]
alc_risk_value     <- alcohol_risk_df$alc_risk_index[alcohol_risk_df$driver_drinkalcohol == new_customer$driver_drinkalcohol]
acc_risk_value     <- accident_history_risk_df$acc_risk_index[accident_history_risk_df$previous_accidents == new_customer$previous_accidents]

# Multiply all risk factors to get total risk score
total_risk_score <- model_risk_value *
  year_risk_value *
  age_risk_value *
  gender_risk_value *
  county_risk_value *
  alc_risk_value *
  acc_risk_value

total_risk_score
new_customer$total_risk_score <- total_risk_score

#️⃣ Summary statistics for total risk score
mean_risk <- mean(california_cars$total_risk_score, na.rm = TRUE)
median_risk <- median(california_cars$total_risk_score, na.rm = TRUE)

cat("Mean total risk score:", round(mean_risk, 4), "\n")
cat("Median total risk score:", round(median_risk, 4), "\n")



#there are 31 million cars in california and there are 300000 car crashes in a year
#assume %85 of all cars in california are actively driven

# Parameters
active_ratio <- 0.85
total_registered_cars <- 31000000
estimated_active_cars <- total_registered_cars * active_ratio
annual_crashes <- 300000

# Compute baseline risk
baseline_risk <- annual_crashes / estimated_active_cars 
baseline_risk
#crash rate for a car in a year is %1.13



#FINALLY! calculating insurance quote annually

new_customer$insurance_quote <- new_customer$expected_damage * new_customer$total_risk_score * baseline_risk
new_customer$insurance_quote

# Let's now calculate the company's other annual operational expenses.

# Estimated number of active customers
estimated_customers <- 500000

# Estimated annual fixed costs (in USD)
salaries <- 100000000     # employee wages
office_rent <- 15000000   # office rent and utilities
marketing <- 20000000     # marketing and advertising
tech_infrastructure <- 10000000 # software, servers, tools, etc.
insurance_costs <- 20000000  # company's own reinsurance or liabilities

# Total operational expenses
total_operational_cost <- salaries + office_rent + marketing + tech_infrastructure + insurance_costs

# Cost per customer per year
cost_per_customer <- total_operational_cost / estimated_customers

cat("✅ Total Annual Operational Cost: $", format(total_operational_cost, big.mark=","), "\n")
cat("💸 Estimated Cost per Customer per Year: $", round(cost_per_customer, 2), "\n")


# Add operational cost to customer's insurance quote to compute fair price

# Fair price is the sum of expected claim cost (insurance_quote) and share of operational costs
new_customer$fair_price <- new_customer$insurance_quote + cost_per_customer

# Display the fair price for this customer
cat("💼 Fair Annual Insurance Price (including operational cost): $", 
    round(new_customer$fair_price, 2), "\n")


# Apply profit margin (15%) to fair price to get final premium

profit_margin <- 0.15  # 15% profit

new_customer$final_premium <- new_customer$fair_price * (1 + profit_margin)

# Display the final annual premium
cat("💰 Final Annual Premium (including 15% profit): $", 
    round(new_customer$final_premium, 2), "\n")















generate_insurance_quote <- function(
    model_risk_df,
    year_risk_df,
    age_risk_df,
    gender_risk_df,
    county_risk_df,
    alcohol_risk_df,
    accident_history_risk_df,
    model_risk1,
    new_crash_data,
    california_cars,
    estimated_customers = 500000,
    active_ratio = 0.85,
    total_registered_cars = 31000000,
    annual_crashes = 300000,
    profit_margin = 0.15
) {
  
  # User input
  cat("Enter customer information:\n")
  driver_age <- as.numeric(readline("Driver age (e.g., 35): "))
  model_year <- as.numeric(readline("Car model year (e.g., 2020): "))
  driver_drinkalcohol <- readline("Drinks alcohol? (TRUE/FALSE): ")
  driver_drinkalcohol <- ifelse(driver_drinkalcohol == "TRUE", TRUE,
                                ifelse(driver_drinkalcohol == "FALSE", FALSE, NA))
  previous_accidents <- as.integer(readline("Number of previous accidents (0-3): "))
  car_model <- readline("Car model (copy from above list): ")
  driver_gender <- readline("Gender (Male/Female): ")
  county <- readline("County (copy from above list): ")
  
  # Create dataframe for prediction
  new_customer <- data.frame(
    driver_age = driver_age,
    model_year = model_year,
    driver_drinkalcohol = factor(driver_drinkalcohol, levels = levels(new_crash_data$driver_drinkalcohol)),
    previous_accidents = previous_accidents,
    car_model = factor(car_model, levels = levels(new_crash_data$car_model)),
    driver_gender = factor(driver_gender, levels = levels(new_crash_data$driver_gender)),
    county = factor(county, levels = levels(new_crash_data$county))
  )
  
  # Predict expected damage
  new_customer$expected_damage <- predict(model_risk1, newdata = new_customer)
  
  # Determine age group
  age_group <- cut(
    new_customer$driver_age,
    breaks = c(17, 24, 44, 64, Inf),
    labels = c("18-24", "25-44", "45-64", "65+"),
    right = TRUE
  )
  
  # Extract relevant risk values
  model_risk_value   <- model_risk_df$model_risk_index[model_risk_df$car_model == new_customer$car_model]
  year_risk_value    <- year_risk_df$year_risk_index[year_risk_df$model_year == new_customer$model_year]
  age_risk_value     <- age_risk_df$age_risk_index[age_risk_df$driver_age_group == age_group]
  gender_risk_value  <- gender_risk_df$risk_index[gender_risk_df$driver_gender == new_customer$driver_gender]
  county_risk_value  <- county_risk_df$risk_index[county_risk_df$county == new_customer$county]
  alc_risk_value     <- alcohol_risk_df$alc_risk_index[alcohol_risk_df$driver_drinkalcohol == new_customer$driver_drinkalcohol]
  acc_risk_value     <- accident_history_risk_df$acc_risk_index[accident_history_risk_df$previous_accidents == new_customer$previous_accidents]
  
  # Calculate total risk score
  new_customer$total_risk_score <- model_risk_value *
    year_risk_value *
    age_risk_value *
    gender_risk_value *
    county_risk_value *
    alc_risk_value *
    acc_risk_value
  
  # Calculate baseline crash risk
  estimated_active_cars <- total_registered_cars * active_ratio
  baseline_risk <- annual_crashes / estimated_active_cars
  
  # Insurance quote = expected damage * total risk score * baseline risk
  new_customer$insurance_quote <- new_customer$expected_damage *
    new_customer$total_risk_score *
    baseline_risk
  
  # Operational costs
  salaries <- 100000000
  office_rent <- 15000000
  marketing <- 20000000
  tech_infrastructure <- 10000000
  insurance_costs <- 20000000
  total_operational_cost <- salaries + office_rent + marketing + tech_infrastructure + insurance_costs
  cost_per_customer <- total_operational_cost / estimated_customers
  
  # Add operational cost
  new_customer$fair_price <- new_customer$insurance_quote + cost_per_customer
  
  # Final premium with profit
  new_customer$final_premium <- new_customer$fair_price * (1 + profit_margin)
  
  # OUTPUTS
  cat("\n#️⃣ Expected damage cost: $", round(new_customer$expected_damage, 2), "\n")
  cat("📊 Total risk score:", round(new_customer$total_risk_score, 4), "\n")
  cat("🛡️ Insurance quote: $", round(new_customer$insurance_quote, 2), "\n")
  cat("🏢 Operational cost per customer: $", round(cost_per_customer, 2), "\n")
  cat("💼 Fair price (quote + operations): $", round(new_customer$fair_price, 2), "\n")
  cat("💰 Final premium (15% profit): $", round(new_customer$final_premium, 2), "\n")
  
  return(new_customer)
}




generate_insurance_quote(
  model_risk_df,
  year_risk_df,
  age_risk_df,
  gender_risk_df,
  county_risk_df,
  alcohol_risk_df,
  accident_history_risk_df,
  model_risk1,
  new_crash_data,
  california_cars
)



