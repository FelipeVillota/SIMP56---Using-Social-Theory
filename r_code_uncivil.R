# RE-EXAM ASSIGNMENT - PART 2A: APPLYING THEORY WITHIN RESEARCH
# Luis Felipe Villota Macías
# Ramez Ahadi


# RQ: 

# ¿how does the affordance of anonymity in Twitter foster incivility against the Muslim community?



# Loading packages

library(rtweet)
library(readxl)
library(dplyr)
library(tidyverse)	
library(psych)	
library(gridExtra)
library(readr)
library(skimr)
library(pscl)
library(knitr)
library(openxlsx)
library(peRspective)
library(stringr)
library(quanteda)



# Setting working directory 

getwd()
setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMP56 - Using Social Theory/Scripts/Michael")


# Setting token

my_token <- create_token(
        app = "harp_16",
        consumer_key= "XHzbYQOuqXgLYF6aKILaLeG0W",    # API Key and Secret
        consumer_secret ="wRjkVljBC9yDsv82gHyHDT5byI8yXZvcPdgUtpZXd2HgcISa1h", # API Secret
        access_token = "1493211970253869060-nyZRgl41Zjadx75l5EjpHP4G4XuVMk",
        access_secret = "fwHh15w6bIFziAlTs2ZkKAha61isu4sqLwVnezDUJys60",
        set_renv = TRUE
)


# Retrieving all tweets in a raw database. Our population = 4999 tweets (observations) and 90 variables


all_tweets <-  
        
        search_tweets(
                q = "islam",          # keyword
                n = 5000,             # raw population
                type = "recent",      # recent
                include_rts = FALSE,  # No retweets
                geocode = NULL,       # No geographical delimitations
                max_id = NULL,
                parse = TRUE,
                token = my_token ,
                retryonratelimit = FALSE,
                verbose = TRUE,
                lang = "en"           # Tweets in English
        )


# Acces date

date() # "Tue Apr 12 17:59:14 2022"

# Selected database
# we keep only the variables (columns) of interest in our study: 12 variables in total 


sel_data <- all_tweets %>% 
        select(user_id, screen_name, created_at, text, source, lang, is_retweet, 
               favorite_count, retweet_count, followers_count,
               friends_count, statuses_count)



# Checking for missing values in the selected database


any(is.na(sel_data)) # FALSE
sum(is.na(sel_data)) # 0
colSums(is.na(sel_data)) # No missing values in the 12 chosen variables



# Number of distinct USERS with all metadata in the selected database

unique_users <- sel_data %>% 
        distinct(user_id, .keep_all = T)

count(unique_users) # 3379 different users



# Number of distinct TWEETS with all metadata in the selected database

unique_tweets <- sel_data %>% 
        distinct(text, .keep_all = T)


count(unique_tweets) # 4956 different tweets, 
                     # we of course excluded rts from the beginning (in Twitter query) 
                     # but 43 observations were not counted as unique (they might harbor 
                     # duplicate texts that were not rts, possibly). 
                     
# For further confirmation for the non-existence of rts here: 


no_retweets <- sel_data %>% 
        filter(is_retweet == "FALSE")  

count(no_retweets) # 4999 observations are not rts

# ________

# Filtering by "followers_count" to look for potential anonymous users (those who have < 10 followers)
# For our study, we are going to look at the unique users (which we already saw by user_id 
# and were 3379 in total). Then we arrange them in descending order by the "favorite_count".

data_f <- unique_users %>% 
        filter(followers_count < 10 ) %>% 
        arrange(desc(favorite_count))     # This leaves us with 635 observations

data_f$text[1] # tweet with most favorite counts of users with less than 10 followers



# Sampling tweets = 200 items  

final_data <- sample_n(data_f,200)


# Saving into an excel file 

write.xlsx(final_data, file = "200_tweets.xlsx")



#### PERSPECTIVE API

library(peRspective)
library(dplyr)
library(readxl)
library(stringr)
library(quanteda)

prsp_models


data_p <- final_data

data_p_1 <- final_data %>% 
        select(user_id, text)

# Selecting IDENTITY_ATTACK as the score model

TW <- prsp_stream(
        .data= final_data,
        text = text,
        text_id = user_id,
        languages ="en",
        score_sentences=F, 
        score_model= "IDENTITY_ATTACK", 
        doNotStore = F, 
        key = "AIzaSyBsEkIVvC_yI3zol7KchRlwtcyVCr-5egY") # account of VILLOTAMACIAS@GMAIL.COM


IATTA <- TW %>%  
        rename(user_id=text_id)

IDATTACK <- inner_join(IATTA, data_p_1, by= "user_id") %>% 
        arrange(desc(IDENTITY_ATTACK))


IDATTACK$text[1] # tweet with the highest score on IDENTITY_ATTACK


#  Saving scores into to an excel file 


write.xlsx(IDATTACK, file = "persperctive_scores.xlsx")


