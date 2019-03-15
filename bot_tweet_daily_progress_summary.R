# run twice a day

library(rtweet)

# setup authentication
oauth <- readRDS("data/credentials.Rds")
create_token("advrsoltuions_bot",
             consumer_key = oauth$c_key,
             consumer_secret = oauth$c_secret,
             access_token = oauth$a_token,
             access_secret = oauth$a_secret)

# (check, if there are updates)

# create progresss image

# create some kind of text

# send tweet
post_tweet("test @henningsway", media = e_img)
