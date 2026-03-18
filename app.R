library(shiny)

ui <- fluidPage(
  titlePanel("Car Insurance Quote Estimator - California"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("driver_age", "Driver Age:", value = 30, min = 18, max = 100),
      numericInput("model_year", "Car Model Year:", value = 2020, min = 2000, max = 2024),
      selectInput("driver_drinkalcohol", "Drinks Alcohol:", choices = c("TRUE", "FALSE")),
      selectInput("previous_accidents", "Previous Accidents:", choices = 0:3),
      selectInput("car_model", "Car Model:", choices = levels(new_crash_data$car_model), selected = "Honda Civic"),
      selectInput("driver_gender", "Gender:", choices = c("Male", "Female")),
      selectInput("county", "County:", choices = levels(new_crash_data$county), selected = "Los Angeles"),
      actionButton("calculate", "Get Quote")
    ),
    
    mainPanel(
      verbatimTextOutput("quote_output")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$calculate, {
    output$quote_output <- renderPrint({
      
      new_customer <- data.frame(
        driver_age = as.numeric(input$driver_age),
        model_year = as.numeric(input$model_year),
        driver_drinkalcohol = factor(input$driver_drinkalcohol, levels = levels(new_crash_data$driver_drinkalcohol)),
        previous_accidents = as.integer(input$previous_accidents),
        car_model = factor(input$car_model, levels = levels(new_crash_data$car_model)),
        driver_gender = factor(input$driver_gender, levels = levels(new_crash_data$driver_gender)),
        county = factor(input$county, levels = levels(new_crash_data$county))
      )
      
      expected_damage <- predict(model_risk1, newdata = new_customer)
      new_customer$expected_damage <- expected_damage
      
      age_group <- cut(
        new_customer$driver_age,
        breaks = c(17, 24, 44, 64, Inf),
        labels = c("18-24", "25-44", "45-64", "65+"),
        right = TRUE
      )
      
      model_risk_value <- model_risk_df$model_risk_index[model_risk_df$car_model == as.character(new_customer$car_model)]
      year_risk_value <- year_risk_df$year_risk_index[year_risk_df$model_year == new_customer$model_year]
      age_risk_value <- age_risk_df$age_risk_index[age_risk_df$driver_age_group == as.character(age_group)]
      gender_risk_value <- gender_risk_df$risk_index[gender_risk_df$driver_gender == as.character(new_customer$driver_gender)]
      county_risk_value <- county_risk_df$risk_index[county_risk_df$county == as.character(new_customer$county)]
      alc_risk_value <- alcohol_risk_df$alc_risk_index[alcohol_risk_df$driver_drinkalcohol == as.logical(as.character(new_customer$driver_drinkalcohol))]
      acc_risk_value <- accident_history_risk_df$acc_risk_index[accident_history_risk_df$previous_accidents == new_customer$previous_accidents]
      
      new_customer$total_risk_score <- model_risk_value *
        year_risk_value *
        age_risk_value *
        gender_risk_value *
        county_risk_value *
        alc_risk_value *
        acc_risk_value
      
      active_ratio <- 0.85
      total_registered_cars <- 31000000
      annual_crashes <- 300000
      estimated_customers <- 500000
      
      baseline_risk <- annual_crashes / (total_registered_cars * active_ratio)
      new_customer$insurance_quote <- new_customer$expected_damage * new_customer$total_risk_score * baseline_risk
      
      cost_per_customer <- (100e6 + 15e6 + 20e6 + 10e6 + 20e6) / estimated_customers
      new_customer$fair_price <- new_customer$insurance_quote + cost_per_customer
      new_customer$final_premium <- new_customer$fair_price * 1.15
      
      cat("Expected damage cost: $", round(new_customer$expected_damage, 2), "\n")
      cat("Total risk score:", round(new_customer$total_risk_score, 4), "\n")
      cat("Insurance quote: $", round(new_customer$insurance_quote, 2), "\n")
      cat("Fair price: $", round(new_customer$fair_price, 2), "\n")
      cat("Final Premium (15% profit): $", round(new_customer$final_premium, 2), "\n")
    })
  })
}

shinyApp(ui = ui, server = server)