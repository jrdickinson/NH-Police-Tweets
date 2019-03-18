library(rtweet)
library(stringr)
consumer_key<- ''
consumer_secret<- ''
access_token<- ''
access_secret<- ''
twitteR::setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

city <- c("manchester", "nashua", "concord", "dover", "rochester", "keene", "portsmouth", "laconia", "lebanon", "somersworth", "hampton", "milford", "exeter", "salem", "nh_state")
user <- c("mht_nh_police", "nashuapolice", "ConcordPolice", "DoverNHPolice", "rochesterpolice", "keenenhpolice", "portsmouthnhpd", "LaconiaNHPolice", "lebpolice", "SomersworthPD", "HamptonNHPD", "milfordnhpolice", "ExeterNHPD", "SalemNHPolice", "NH_StatePolice")
labels <- c("#_of_tweets", "screen_name", "#retweets", "total_favorites_count", "#_of_followers", "creation_date", "top_tweet_1", "top_tweet_2", "top_tweet_3", "top_tweet_4", "top_tweet_5", "fav_top_tweet_1", "fav_top_tweet_2", "fav_top_tweet_3", "fav_top_tweet_4", "fav_top_tweet_5")
df <- data.frame(labels)

for (i in 1:length(city)){
  assign(city[i], rtweet::get_timeline(user[i], n=10000))
  assign(city[i]["text"], iconv(get(paste0(city[i]))$text, "UTF-8", "ASCII", sub = ""))
  write_as_csv(get(city[i])["text"], paste0(city[i], "_collection.csv"), prepend_ids = TRUE, na = "",fileEncoding = "ASCII")
  # write.csv(get(city[i]), paste0(city[i], "_information.csv"))
  assign(city[i], arrange(get(paste0(city[i])), desc(favorite_count)))
  assign(paste0(city[i], "_info"), c(nrow(get(city[i])), get(city[i])$screen_name[1], sum(get(city[i])$is_retweet), sum(get(city[i])$favorite_count), get(city[i])$followers_count[1], str_split(get(city[i])$account_created_at[1], " ")[[1]][1], get(city[i])$text[1], get(city[i])$text[2], get(city[i])$text[3], get(city[i])$text[4], get(city[i])$text[5], get(city[i])$favorite_count[1], get(city[i])$favorite_count[2], get(city[i])$favorite_count[3], get(city[i])$favorite_count[4], get(city[i])$favorite_count[5]))
  df[city[i]] <- get(paste0(city[i], "_info"))
  }
write.csv(df, "city_info.csv")




