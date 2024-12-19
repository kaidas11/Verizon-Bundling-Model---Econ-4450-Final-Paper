### Feel free to change these inputs to fit any kind of customer (business, personal, etc.)!
# This model will tell you if a customer will join Verizon or switch away and Verizon's respective profits.

# Common inputs
input_params <- list(
  delta = 0.95,               # Discount factor
  P_V = 100,                  # Verizon price
  P_competitor = 90,          # Competitor price
  monthly_promo = 15,         # Monthly promo credit
  promo_duration = 6,         # Promo duration
  k_join = 150,               # Cost of joining Verizon
  k_leave = 300,              # Cost of leaving Verizon
  c = 70,                     # Marginal cost per customer
  N = 500000,                 # Number of customers
  WTP_V = 120,                # WTP for Verizon
  WTP_C = 110,                # WTP for competitor
  t_start = 3,                # Current period
  evaluation_period = 50     # Total evaluation period (set high for "infinite")
)

### Model for Joining Verizon
switching_costs_join <- function(inputs) {
  # Extract inputs from the list
  delta <- inputs$delta
  P_V <- inputs$P_V
  P_competitor <- inputs$P_competitor
  monthly_promo <- inputs$monthly_promo
  promo_duration <- inputs$promo_duration
  k_join <- inputs$k_join
  c <- inputs$c
  N <- inputs$N
  WTP_V <- inputs$WTP_V
  WTP_C <- inputs$WTP_C
  t_start <- inputs$t_start
  T <- inputs$evaluation_period
  
  # Promo period end
  x <- promo_duration
  
  # Value of joining Verizon
  Value_verizon_promo <- sum(delta^(t_start:x) * (WTP_V - P_V + monthly_promo))
  Value_verizon_regular <- sum(delta^((x + 1):T) * (WTP_V - P_V))
  Value_verizon <- Value_verizon_promo + Value_verizon_regular
  
  # Value of staying with competitor
  Value_competitor <- sum(delta^(t_start:T) * (WTP_C - P_competitor))
  
  # Decision rule for joining Verizon
  decision <- k_join <= (Value_competitor - Value_verizon)
  
  # Profit calculation
  Profit_promo <- sum(delta^(t_start:x) * (P_V - monthly_promo - c))
  Profit_regular <- sum(delta^((x + 1):T) * (P_V - c))
  Profit_total <- N * (Profit_promo + Profit_regular)
  
  # Return clean sentence
  if (decision) {
    return(paste("The consumer will join Verizon and Verizon makes a profit afterwards of", round(Profit_total, 2)))
  } else {
    return("The consumer will not join Verizon and Verizon makes zero profit.")
  }
}

# Run the model
switching_costs_join(input_params)

### Model for Leaving Verizon
switching_costs_leave <- function(inputs) {
  # Extract inputs from the list
  delta <- inputs$delta
  P_V <- inputs$P_V
  P_competitor <- inputs$P_competitor
  monthly_promo <- inputs$monthly_promo
  promo_duration <- inputs$promo_duration
  k_leave <- inputs$k_leave
  c <- inputs$c
  N <- inputs$N
  WTP_V <- inputs$WTP_V
  WTP_C <- inputs$WTP_C
  t_start <- inputs$t_start
  T <- inputs$evaluation_period
  
  # Promo period end
  x <- promo_duration
  
  # Value of staying with Verizon
  Value_verizon_promo <- sum(delta^(t_start:x) * (WTP_V - P_V + monthly_promo))
  Value_verizon_regular <- sum(delta^((x + 1):T) * (WTP_V - P_V))
  Value_verizon <- Value_verizon_promo + Value_verizon_regular
  
  # Value of switching to competitor
  Value_competitor <- sum(delta^(t_start:T) * (WTP_C - P_competitor))
  
  # Decision rule for leaving Verizon
  decision <- (Value_verizon - Value_competitor) <= k_leave
  
  # Profit calculation
  Profit_promo <- sum(delta^(t_start:x) * (P_V - monthly_promo - c))
  Profit_regular <- sum(delta^((x + 1):T) * (P_V - c))
  Profit_total <- N * (Profit_promo + Profit_regular)
  
  # Return clean sentence
  if (decision) {
    return("The consumer will leave Verizon and Verizon makes zero profit afterward.")
  } else {
    return(paste("The consumer will not leave Verizon and Verizon makes a profit afterwards of", round(Profit_total, 2)))
  }
}

# Run the model
switching_costs_leave(input_params)