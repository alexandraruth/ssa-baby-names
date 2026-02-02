# read in state data and make tidy dataframe

## RUN ONLY ONCE to generate tidy merged dataframes

library(tidyverse)

# states ----

#example for one state
AK <- read_csv("data/raw_names_bystate/AK.TXT", col_names = c("state", "sex", "year", "name", "count"))

statenames <- state.abb

smallvec <- c("CA", "NE", "NC", "MI", "SC")

mylist <- list()

for (i in statenames) {
  
  filename <- paste0("data/raw_names_bystate/", i, ".TXT")
  df <- read_csv(filename, col_names = c("state", "sex", "year", "name", "count"))
  mylist[[i]] <- df
}


df <- do.call("rbind",mylist) #combine all vectors into a matrix

save(df, file = "data/clean_names_bystate.Rda")

# years ----


