# read in and concatenate national data by year

## RUN ONLY ONCE to generate tidy merged dataframes

library(tidyverse)

# examine one year df
data <- read_csv("data/raw_names_national/yob2022.TXT", col_names = c("name", "sex", "count"))


# write function to read in all years and store a df

years <- c(1880:2024)

mylist <- list()

for (i in years) {
  
  filename <- paste0("data/raw_names_national/yob", i, ".TXT")
  df <- read_csv(filename, col_names = c("name", "sex", "count"))
  df <- df %>%
    mutate(year = i)
  mylist[[i]] <- df
}


df <- do.call("rbind",mylist) #combine all vectors into a matrix

save(df, file = "data/clean_names_byyear.Rda")
