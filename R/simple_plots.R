# simple graphs

library(tidyverse)

load("data/clean_names_bystate.Rda")

# counts by year by state for Gertrude ----

clara <- df %>%
  filter(name == "Clara")

ggplot(clara, aes(x = year, y = count, color = state, group = state)) +
  geom_line() +
  labs(title = "Name Counts Over Time by State",
       subtitle = "Name: Clara",
       x = "Year",
       y = "Count",
       color = "State") +
  theme_minimal()


# function ----

plot_name_trends <- function(df, name_input, sex_filter = NULL) {
  name_data <- df %>% filter(name == name_input)
  
  if (!is.null(sex_filter)) {
    name_data <- name_data %>% filter(sex == sex_filter)
  }
  
  ggplot(name_data, aes(x = year, y = count, color = state, group = state)) +
    geom_line() +
    labs(title = "Name Counts Over Time by State",
         subtitle = paste("Name:", name_input),
         x = "Year",
         y = "Count",
         color = "State") +
    theme_minimal()
}

# Usage:
plot_name_trends(df, "Gertrude", "F")

# rates function ----

plot_name_trends_rate <- function(df, name_input, sex_filter = NULL) {
  # Calculate total births per state/year for rate calculation
  df_with_rates <- df %>%
    group_by(state, year) %>%
    mutate(rate = count / sum(count)) %>%
    ungroup()
  
  # Filter for the name
  name_data <- df_with_rates %>% 
    filter(name == name_input)
  
  if (!is.null(sex_filter)) {
    name_data <- name_data %>% filter(sex == sex_filter)
  }
  
  # Plot with rates
  ggplot(name_data, aes(x = year, y = rate, color = state, group = state)) +
    geom_line() +
    labs(title = "Name Popularity Over Time by State",
         subtitle = paste("Name:", name_input),
         x = "Year",
         y = "Rate (Fraction of All Births)",
         color = "State") +
    theme_minimal()
}

plot_name_trends_rate(df, "Gertrude", "F")

plot_name_trends(df, "Donald", "M")
plot_name_trends_rate(df, "Donald", "M")


