##%######################################################%##
#                                                          #
####            Scraping Samsung Help Forums            ####
#                                                          #
##%######################################################%##

# The project involved text analysis of all help comments 
# left on the Samsung message boards re: one of their products
# The rss page only surfaces 15 results at a time, so it was 
# left to scrape. Multi-page topics (those with more than 10
# comments, used a different url path on page 2 and beyond)

# This worked, but I'm sure there is an easier way of doing it. 

library(xml2)
library(rvest)
library(tidyverse)
library(stringr)

# parts of the url to paste together 
part1 <- "https://us.community.samsung.com/t5/forums/searchpage/tab/message?q=bixby&page="
part2 <- "&sort_by=-topicPostDate&collapse_discussion=true"


##%######################################################%##
#                                                          #
####           Titles of Bixby Help Requests            ####
#                                                          #
##%######################################################%##

#   ____________________________________________________________________________
#   pull titles and views of each Bixby query                               ####

## notes: it looks like we run into a few pages where title and category 
## are the same and get dropped. we have to piecemeal the loop and add 
## everything into the dataframe after
## 
## let's find out what the pages are that have problems
## problem pages - 31,33,48,62,63,79,86,87,89,93,94,108,111,115,121

title_df <- NULL
# loop to pull all the titles and views
for (i in c(1:30,32,34:47,49:61,64:78,80:85,88,90:92,95:107,109,110,112:114,116:120,122:125)) {
  url <- paste0(part1, i, part2)
  sam <- read_html(url)
  
  titles <- sam %>%
    html_nodes(".lia-summary-view-statistic-views , .lia-message-subject") %>%
    html_text() %>%
    str_replace_all("[\r\n\t]" , "") 
  
  date <- sam %>%
    html_nodes(".lia-summary-view-statistic-post-date") %>%
    html_text() %>%
    str_replace_all("[\r\n\t]" , "")
    
  category <- sam %>%
    html_nodes(".search-board-link") %>%
    html_text()
  
  link <- sam %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    data.frame() %>% 
    rename_(url = names(.)[1]) %>% 
    filter(str_detect(url, "m-p")) %>% 
    mutate(partPath = sapply(str_split(url, "/"), "[[", 4),
           category = sapply(str_split(url, "/"), "[[", 3)) %>%
    distinct(partPath, category, .keep_all = TRUE) %>% 
    mutate(hash = str_extract(url, "#\\S+")) 
    
  # split into dataframe with two columns
  df <- data.frame(date = date,
                   title = titles[seq(1,(length(titles)-1),2)], 
                   category = category,
                   views = titles[seq(2,length(titles),2)],
                   url = link[,1],
                   hash = link[,4])
  
  title_df <- rbind(title_df, df)
  df <- NULL
}


##  ............................................................................
##  clean up the dataframe                                                  ####

# convert factor to character
title_df <- title_df %>% mutate_if(is.factor, as.character)
# strip out the leading 1 on the titles (using logical), 
# and split the number of views 
title_df <- title_df %>%
  mutate(date = str_trim(date, side = c("both")),
         postTime = ifelse(str_sub(date, -1) == "M", str_sub(date, -8), date),
         postDate = ifelse(str_sub(date, -1) == "M", str_sub(date, 1, -9), date),
         title = if_else(substring(title, 1, 1) == "1", str_sub(title, 2), title),
         views = as.numeric(gsub("\\V.*", "", views)))



##  ............................................................................
##  merge in url category codes                                             ####

# the url structure on the forum pages are different from what we 
# just scraped, so need to add in the forum category codes 
catNames <- title_df %>% distinct(category)

catCodes <- data.frame(category = catNames, 
                       catPath = c("BixbyQandA", "GalaxyNotePhones", 
                                   "GeneralDiscussion", "OtherMobile",
                                   "GalaxyS", "GS8QA", "NPD", "SMARTHOME",
                                   "Note8QA", "Bixby_Feedback", "Note8HowTo",
                                   "BixbyHT", "HT-Other", "Bixbyannounce",
                                   "ProductUpdates", "wearabletech", 
                                   "Gs8-HT", "tv", "QA", "Promotions"))

# merge into the df
title_df <- title_df %>% left_join(catCodes)


##  ............................................................................
##  clean the hashtags                                                      ####

# the forum urls don't have the #M, only the number
title_df <- title_df %>% 
  mutate(cleanHash = gsub(".*\\M", "", hash))



##  ............................................................................
##  function to pull the number of replies                                  ####

forum1 <- "https://us.community.samsung.com/"

replyCount <- NULL
for (i in 1:nrow(title_df)) {
  url <- paste0(forum1, title_df$url[i])
  sam <- read_html(url)
  
  replies <- sam %>%
    html_nodes(".lia-component-reply-count-conditional") %>%
    html_text() %>%
    str_replace_all("[\r\n\t]" , "") %>% 
    data.frame() %>%
    rename_(reply = names(.)[1]) %>%
    mutate(row = as.numeric(i), 
           replies = as.numeric(gsub("\\ .*", "", reply))) %>%
    select(row, replies)
  
  replyCount <- rbind(replyCount, replies)
  replies <- NULL
}


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### add replies to the data frame                                           ####
title_df <- title_df %>% 
  mutate(row = row_number()) %>%
  left_join(replyCount, by = c("row", "row"))

title_df$replies[is.na(title_df$replies)] <- 0

# now figure out how many pages need to be pulled given the number of replies
title_df <- title_df %>%
  mutate(totalMessage = replies + 1, 
         remainder = totalMessage %% 10,
         pages = floor(totalMessage/10),
         pages = ifelse(remainder > 0, pages + 1, pages)) 
         


##%######################################################%##
#                                                          #
####                Comments and Replies                ####
#                                                          #
##%######################################################%##


#   ____________________________________________________________________________
#   Now pull out the comments / replies                                     ####
forum1 <- "https://us.community.samsung.com/t5/forums/v3_1/forumtopicpage/board-id/"
forum2 <- "/thread-id/"
forum3 <- "/highlight/true/page/"

# manually have to do this because the scrap occasionally pulls in bad 
# data...
message_df <- NULL
for (i in 1096:nrow(title_df)) {
  for (j in 1:title_df$pages[i]) { 
    # print(i)
    # print(j)
    url <- paste0(forum1, title_df$catPath[i], forum2, 
                   title_df$cleanHash[i], forum3, j)
    
    sam <- read_html(url)
    
    users <- sam %>%
      html_nodes(".lia-component-user-name") %>%
      html_text() %>%
      str_replace_all("[\r\n\t]" , "") %>%
      data.frame() %>%
      rename_(userName = names(.)[1])
    
    rank <- sam %>%
      html_nodes(".lia-component-author-rank-name") %>%
      html_text() %>%
      str_replace_all("[\r\n\t]" , "") %>%
      data.frame() %>%
      rename_(userRank = names(.)[1])
    
    # this is pretty specific to something that's been edited....
    # hopefully this works across all pages even if not edited
    date <- sam %>%
      html_nodes(".lia-quilt-column-header-right .lia-quilt-column-alley-right") %>%
      html_text() %>%
      str_replace_all("[\r\n\t]" , "") %>%
      str_replace_all("OptionsMark as NewBookmarkSubscribeSubscribe to RSS FeedHighlightPrintEmail to a FriendReport Inappropriate Content", "") %>%
      str_trim( side = c("both")) %>%
      data.frame() %>% 
      rename_(date = names(.)[1]) %>%
      mutate(date = gsub("\\ - .*", "", date))

    topic <- sam %>%
      html_nodes(".lia-component-subject") %>%
      html_text() %>%
      str_replace_all("[\r\n\t]" , "") %>%
      str_trim( side = c("both")) %>%
      data.frame() %>% 
      rename_(topic = names(.)[1])
      # mutate(topic = gsub("\\[|\\]", "", topic))
    
    content <- sam %>%
      html_nodes(".lia-message-body-content") %>%
      html_text() %>%
      str_replace_all("[\r\n\t]" , "") %>%
      str_trim( side = c("both"))
    # split into dataframe with two columns
    df <- data.frame(userName = users,
                     userRank = rank,
                     date = date,
                     topic = topic,
                     content = content,
                     cleanHash = title_df$cleanHash[i])
    
    message_df <- rbind(message_df, df)
    df <- NULL
    
  }

}

##  ............................................................................
##  clean up the dataframe                                                  ####

# convert factor to character
message_df <- message_df %>% mutate_if(is.factor, as.character)

# drop all duplicates (will base off of the messages)
message_df2 <- message_df %>% distinct(content, .keep_all = TRUE)

























