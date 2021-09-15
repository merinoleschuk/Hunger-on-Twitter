#Install needed packages
install.packages("rtweet")
install.packages("reactable")
install.packages("glue")
install.packages("stringr")
install.packages("httpuv")
install.packages("tidyverse")
install.packages("purrr")

#load libraries
library(rtweet)
library(dplyr)

#load data
tweet_data <- search_tweets("hunger OR hungry AND family OR kids OR children AND covid OR quarantine OR corona", n = 400, include_rts = FALSE)

#shows tweet data so that you can choose what variables you want 

names(tweet_data)

#tells R which variables to keep
tweets <- tweet_data %>% select(user_id, status_id, created_at, screen_name, name, text, favorite_count, retweet_count, urls_expanded_url, location)

# make a table so you can work with the data
tweet_table_data <- select(tweets, -user_id, -status_id)

library(reactable)
reactable::reactable(tweet_table_data)

#make a nicer table with the tweet data
reactable (tweet_table_data, filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 50, showPageSizeOptions = TRUE, 
           showSortable = TRUE, pageSizeOptions = c(50, 100, 200), defaultSortOrder = "desc",
           columns = list(created_at = colDef(defaultSortOrder = "asc"), screen_name = colDef(defaultSortOrder = "asc"), 
                          text = colDef(html = TRUE, minWidth = 190, resizable = TRUE) favorite_count = colDef(filterable = FALSE), retweet_count = colDef(filterable = FALSE), urls_expanded_url = colDef(html = TRUE)))

#? he said not to run this…
tweets <- tweet_data %>% select(user_id, status_id, created_at, screen_name, name, text, favorite_count, retweet_count, urls_expanded_url, location)

#glue the link to the twitter user to the actual tweet inside the table we made
tweets <- tweets %>% mutate( TweetUrl = glue::glue("https://twitter.com/{screen_name}/status/{status_id}"), TweetLink = glue::glue("<a href='{TweetUrl}'>>> </a>"), Tweet = paste(text, TweetLink) )

reactable(tweets[,"Tweet"], columns = list( Tweet = colDef(html = TRUE)) )

#making the table more workable again
tweet_table_data <- tweet_data %>% select(user_id, status_id, created_at, screen_name, name, text, favorite_count, retweet_count, urls_expanded_url, location) %>% mutate( Tweet = glue::glue("{text} <a href = 'https:/twitter.com/{screen_name}/status/{status_id}'>>> <a/>") )%>% select(DateTime = created_at, User = screen_name, name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url, location)

#taking all the html/urls and making sure it comes up as html/urls
make_url_html <- function(url) { if(length(url) < 2) { if(!is.na(url)) { as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") ) } else { "" } } else { paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ") } }

# final react table
reactable(tweet_table_data, filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE, showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 50, showPageSizeOptions = TRUE, pageSizeOptions = c(50, 75, 100, 200), columns = list( DateTime = colDef(defaultSortOrder = "asc"), User = colDef(defaultSortOrder = "asc"),
                                                                                                                                                                                                                                                                              Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE), Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)), RTs = colDef(filterable = FALSE, format = colFormat(separators = TRUE)), URLs = colDef(html = TRUE) ) )

#save & export to a csv file. removed the code that changes everything into a character because that was messing up the dates. Replaced it with code that just turns the url into dates
#tweet_table_data <- apply(tweet_table_data,2,as.character)
tweet_table_data$URLs <- vapply(tweet_table_data$URLs, paste, collapse = ", ", character(1L))
write.csv(tweet_table_data, "May 15_2021_FI.csv")
