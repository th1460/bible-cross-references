require(dplyr)
require(magrittr)
require(stringr)

# read raw data
dataset <- 
    read.delim(unz("data/cross-references.zip", 
                   "cross_references.txt"), 
               stringsAsFactors = FALSE)

dataset %<>% 
    as_tibble() %>%
    select(from_verse = 1, to_verse = 2)

# extract books
dataset %<>% 
    mutate(from_book = str_extract(from_verse, "[A-Za-z]*.(?=\\.)"),
           to_book = str_extract(to_verse, "[A-Za-z]*.(?=\\.)"))

# save
dataset %>% 
    readr::write_csv("data/dataset.csv")

