# sample size
n <- 100

# customer segment breakdowns
# source: ASCI 2022-23, pg. 1/16 <https://theacsi.org/wp-content/uploads/2023/06/23may_Telecom-STUDY.pdf>
p_cordstacker <- 0.23
p_cordshaver <- 0.23
p_cordcutter <- 0.31
p_cordnever <- 0.23

n_segments <- c(p_cordstacker, p_cordshaver, p_cordcutter, p_cordnever) * n
segments <- c("Cord Stackers", "Cord Shavers", "Cord Cutters", "Cord Nevers")

# Verizon plan prices; source: <https://www.verizon.com/plans/unlimited/>
price_welcome <- 30
price_plus <- 45
price_ultimate <- 55

# bundle plan prices; source: <https://www.verizon.com/plans/unlimited/>
price_welcome_bundle <- 40
price_plus_bundle <- 55
price_ultimate_bundle <- 65

# bundle prices
price_appleone <- 19.95 # source: <https://www.apple.com/apple-one/>
price_walmartplus <- 12.95 # source: <https://www.walmart.com/help/article/walmart-membership/534c4edc29204a6bb15145a61146bf51>
price_netflixhbomax <- 6.99 + 9.99  # sources: <https://www.netflix.com/signup/planform>, <https://auth.max.com/product>

# WTPs for each customer segment
wtp_welcome <- c(45, 40, 35, 30)
wtp_plus <- c(65, 60, 55, 35)
wtp_ultimate <- c(80, 70, 60, 45)

# WTPs with bundles
wtp_welcome_bundle <- wtp_welcome + price_walmartplus
wtp_plus_bundle <- wtp_plus + price_netflixhbomax
wtp_ultimate_bundle <- wtp_ultimate + price_appleone

# consumer surplus for each plan
cs_welcome <- wtp_welcome - price_welcome
cs_plus <- wtp_plus - price_plus
cs_ultimate <- wtp_ultimate - price_ultimate

cs_welcome_bundle <- wtp_welcome_bundle - price_welcome_bundle
cs_plus_bundle <- wtp_plus_bundle - price_plus_bundle
cs_ultimate_bundle <- wtp_ultimate_bundle - price_ultimate_bundle

# uniform pricing options
uniform_prices <- sort(unique(wtp_welcome))  # Prices based on WTP levels
profits_uniform <- sapply(uniform_prices, function(price) {
  profit <- 0
  for (i in 1:4) {
    if (wtp_welcome[i] >= price) {  # Only count customers willing to buy
      profit <- profit + price * n_segments[i]
    }
  }
  return(profit)
})

# plans chosen under uniform pricing
plans_uniform_all <- sapply(uniform_prices, function(price) {
  sapply(wtp_welcome, function(wtp) ifelse(wtp >= price, 1, 0))
})
colnames(plans_uniform_all) <- paste0("$", uniform_prices)
rownames(plans_uniform_all) <- segments

# consumer surplus under uniform pricing
cs_uniform <- sapply(uniform_prices, function(price) {
  sapply(1:4, function(i) ifelse(wtp_welcome[i] >= price, wtp_welcome[i] - price, 0))
})
rownames(cs_uniform) <- segments
colnames(cs_uniform) <- paste0("$", uniform_prices)

# plan choice function
plan_choice <- function(cs_welcome, cs_plus, cs_ultimate) {
  surplus <- c(cs_welcome, cs_plus, cs_ultimate)
  if (max(surplus) >= 0) return(which.max(surplus))
  return(NA)  # no plan chosen
}

# map plan numbers to names
map_versioning <- c("Welcome", "Plus", "Ultimate")
map_bundling <- c("Welcome Bundle", "Plus Bundle", "Ultimate Bundle")

# plan choices for each segment under each scenario
plans_versioning <- sapply(1:4, function(i) plan_choice(cs_welcome[i], cs_plus[i], cs_ultimate[i]))
plans_versioning_named <- map_versioning[plans_versioning]

plans_bundling <- sapply(1:4, function(i) plan_choice(cs_welcome_bundle[i], cs_plus_bundle[i], cs_ultimate_bundle[i]))
plans_bundling_named <- map_bundling[plans_bundling]

# profit calculation (consumers choose plan with highest CS)
calculate_profit <- function(plans, prices) {
  profit <- 0
  for (i in 1:4) {
    if (!is.na(plans[i])) {
      profit <- profit + prices[plans[i]] * n_segments[i]
    }
  }
  return(profit)
}

# prices for plans
prices_versioning <- c(price_welcome, price_plus, price_ultimate)
prices_bundling <- c(price_welcome_bundle, price_plus_bundle, price_ultimate_bundle)

# profit calculations: versioning and bundling
profit_versioning <- calculate_profit(plans_versioning, prices_versioning)
profit_bundling <- calculate_profit(plans_bundling, prices_bundling)

# consumer surplus calculations:
calculate_cs <- function(plans, cs_welcome, cs_plus, cs_ultimate) {
  sapply(1:4, function(i) {
    if (plans[i] == 1) return(cs_welcome[i])
    if (plans[i] == 2) return(cs_plus[i])
    if (plans[i] == 3) return(cs_ultimate[i])
    return(0)
  })
}

cs_versioning <- calculate_cs(plans_versioning, cs_welcome, cs_plus, cs_ultimate)
names(cs_versioning) <- segments
cs_bundling <- calculate_cs(plans_bundling, cs_welcome_bundle, cs_plus_bundle, cs_ultimate_bundle)
names(cs_bundling) <- segments

# output results
results <- list(
  "Profit under Uniform Pricing (Lowest WTP to Highest)" = profits_uniform,
  "Profit under Versioning Only" = profit_versioning,
  "Profit under Versioning + Bundling" = profit_bundling,
  "Consumer Surplus under Uniform Pricing (per consumer)" = cs_uniform,
  "Consumer Surplus under Versioning (per consumer)" = cs_versioning,
  "Consumer Surplus under Bundling (per consumer)" = cs_bundling,
  "Plan Chosen under Uniform Pricing (1 if buys)" = plans_uniform_all,
  "Plan Chosen under Versioning" = plans_versioning_named,
  "Plan Chosen under Bundling" = plans_bundling_named)

print(results)
