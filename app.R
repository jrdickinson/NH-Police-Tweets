library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)

library(readr)
library(qdap)
library(tm)
library(tidytext)
library(dplyr)
library(RWeka)
library(wordcloud)
library(radarchart)
library(ggplot2)
library(syuzhet)
library(sentimentr)
library(lexicon)
library(data.table)
library(stringi)
library(text2vec)
library(BBmisc)
replace_words <- function(x){
  return(x %>%
           gsub("\\brd\\b", " road ", .) %>%
           gsub("\\bst\\b", " street ", .) %>%
           gsub("\\bln\\b", " lane ", .) %>%
           gsub("\\bblvd\\b", " boulevard ", .) %>%
           gsub("\\bpd\\b", " police department ", .) %>%
           gsub("\\bofc\\b", " officer ", .) %>%
           gsub("\\bave\\b", " avenue ", .) %>%
           gsub("\\bnh\\b", " new hampshire ", .)
  )
}
clean_text <- function(x, i){
  #input text column for x and name of new variables as i
  abbrev <- qdapDictionaries::abbreviations
  abbrev <- rbind(abbrev, c("hampton", ""), c("portsmouth", ""), c("manchester", ""), c("nashua", ""), c("concord", ""), c("dover", ""), c("rochester", ""), c("keene", ""), c("laconia", ""), c("lebanon", ""), c("somersworth", ""), c("milford", ""), c("exeter", ""), c("salem", ""))
  # change words to lowercase and remove newline calls and abbreviations
  x <- tolower(x)
  x <- replace_abbreviation(x, abbreviation = abbrev)
  x <- gsub("\n", " ", x)
  #remove urls, hashes, and tags
  x <- gsub("@\\w+ *", "", x)
  x <- gsub("http\\S+\\s*", "", x)
  x <- gsub("http\\w+ *", "", x)
  x <- gsub("#", "", x)
  #remove numbers and punctuation
  x <- gsub('\\S+[0-9]\\S+', '', x)
  x <- gsub('[0-9]', "", x)
  x <- removePunctuation(x)
  #remove acronyms and more abbreviations
  x <- replace_words(x)
  #remove stopwords
  stop_words <- c("rt", "twitter", "portsmouth", "police", "department", "amp", "hampton", "th", "new", "hampshire", "street", "road", "boulevard", "avenue", "dot", "com", "nh", "officer", "manchester", "nashua", "concord", "dover", "rochester", "keene", "laconia", "lebanon", "somersworth", "milford", "exeter", "salem", "state",stopwords("english"))
  x <- removeWords(x, stop_words)
  #remove leading and trailing whitespaces
  x <- gsub("^\\s+|\\s+$", "", x)
  #remove 1-2 character words
  x <- gsub('\\b\\w{1,2}\\b','',x)
  #remove empty strings and tokenize
  x <- x[x != "NULL"] 
  x <- removeWords(x, c("0", "character"))
  x <- x[x != ""]
  assign(i, x, envir = .GlobalEnv) 
  assign(paste0(i,"2"), word_tokenizer(x), envir = .GlobalEnv)
}
tweets_to_df <- function(x, i){
  #input text column for x and name of new variables as i
  #efficiently go from text column to df
  out <- stri_extract_all_words(stri_trans_tolower(SnowballC::wordStem(x, "english"))) #in old package versions it was named 'stri_extract_words'
  names(out) <- paste0("doc", 1:length(out))
  lev <- sort(unique(unlist(out)))
  dat <- do.call(cbind, lapply(out, function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
  }, lev = lev))
  rownames(dat) <- sort(lev)
  dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ] 
  dat2 <- slam::as.simple_triplet_matrix(dat)
  tdm <- tm::as.TermDocumentMatrix(dat2, weighting=weightTf)
  assign(paste0(i,"_df"), tidy(tdm), envir = .GlobalEnv)
}

#create clean dataframes and text columns
for (i in c("hampton", "portsmouth", "manchester", "nashua", "concord", "dover", "rochester", "keene", "laconia", "lebanon", "somersworth", "milford", "exeter", "salem", "nh_state")){
  assign(paste(i), read_csv(paste0(i,"_collection.csv")), envir = .GlobalEnv)
  clean_text(get(i)$text,i)
  tweets_to_df(get(paste0(i,"2")), i)
  #remove terms that equal "c" from dataframes
  assign(paste0(i, "_df"), subset(get(paste0(i, "_df")), !(term == "c")))
  assign(paste0(i, "_df2"), get(paste0(i, "_df")) %>% group_by(term) %>% summarise(count=sum(count)))
}
assign("info", read_csv("city_info.csv"))




ui <- fluidPage(
  list(
    tags$head(id="myhead",
      tags$style(
        ".title {margin: auto; width: '100%'; text-align: center}",
        ".title {background-color: #ff9933;}",
        ".title {color: #ffffff}"
      )
    )
  ),
  tags$div(class="title",
    titlePanel("NH Police Tweet Analysis")
  ),
  dashboardPage(
  dashboardHeader(title = "Menu"),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Insights", icon = icon("th"), tabName = "Insights"),
      menuItem("Comparison", icon = icon("th"), tabName = "Comparison")
    )
  ),
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
        fluidRow(
          box(
            title = "About Project", width = 12, solidHeader = TRUE, background = NULL, status = "info", collapsible = FALSE, collapsed = FALSE, 
            h3("This is a shiny app that compares the tweets of different New Hampshire police stations.  The goal is to analyze the positive/negative and emotional sentiment of the tweets to get a better understanding of the type of content that is put out in the tweets.  Another goal is to be able to help NH police stations be more aware of the content of their tweets and hopefully to better engage with their communities while being able to convey important messages."),
            h3(""),
            h3("If you are unsure of what a feature is doing, click on the question mark symbol for that feature for more information."),
            h3(""),
            h3("  The content on these pages is static and was recorded in March of 2019.  The code to this project can be found at:"),
            htmlOutput("link1")
          )
        )
      ),
      tabItem(tabName = "Insights",
        fluidRow(
          box(
            title = "Filter 1", width = 6, solidHeader = TRUE, status = "info", background = NULL, collapsible = FALSE, collapsed = FALSE,
            selectInput("city1", label="City", choices=c("concord", "dover", "exeter", "hampton", "keene", "laconia", "lebanon", "manchester", "milford", "nashua", "nh_state", "portsmouth", "rochester", "salem", "somersworth")) %>%
              helper(icon = "question",
                     colour = "orange",
                     type = "markdown",
                     content = "Help1")
          ),
          box(
            title = "Filter 2", width = 6, solidHeader = TRUE, status = "warning", background = NULL, collapsible = FALSE, collapsed = FALSE,
            selectInput("Grouping Method", label = "Grouping Method", choices = c(1,2,3, "tf-idf")) %>%
              helper(icon = "question",
                     colour = "teal",
                     type = "markdown",
                     content = "Help2") 
          )
        ),
        fluidRow(
          box(
            title = "Wordcloud", width = 6, solidHeader = TRUE, status = "success", background = NULL, collapsible = TRUE, collapsed = FALSE,
            plotOutput("Plot_1") %>%
              helper(icon = "question",
                     colour = "blue",
                     type = "markdown",
                     content = "Help3") 
          ),
          box(
            title = "Radar Plot", width = 6, solidHeader = TRUE, status = "primary", background = NULL, collapsible = TRUE, collapsed = FALSE,
            chartJSRadarOutput("Plot_2") %>%
              helper(icon = "question",
                     colour = "fuchsia",
                     type = "markdown",
                     content = "Help4") 
          )
        ),
        fluidRow(
          box(
            title = "Timeline", width = 12, solidHeader = TRUE, status = "danger", background = NULL, collapsible = TRUE, collapsed = FALSE,
            plotOutput("Plot_3") %>%
              helper(icon = "question",
                     colour = "lime",
                     type = "markdown",
                     content = "Help5") 
          )
        ),
        fluidRow(
          box(
            title = "Timeline", width = 12, solidHeader = TRUE, status = "info", background = NULL, collapsible = TRUE, collapsed = FALSE,
            plotOutput("Plot_4") %>%
              helper(icon = "question",
                     colour = "purple",
                     type = "markdown",
                     content = "Help6") 
          )
        )
      ),
      tabItem(tabName = "Comparison",
        fluidRow(
          box(
            title = "Filter 1", width = 6, solidHeader = TRUE, status = "primary", background = NULL, collapsible = FALSE, collapsed = FALSE,
            selectInput("city2", label = "City", choices = c("concord", "dover", "exeter", "hampton", "keene", "laconia", "lebanon", "manchester", "milford", "nashua", "nh_state", "portsmouth", "rochester", "salem", "somersworth")) %>%
              helper(icon = "question",
                     colour = "aqua",
                     type = "markdown",
                     content = "Help7") 
          ),
          box(
            title = "Filter 2", width = 6, solidHeader = TRUE, status = "warning", background = NULL, collapsible = FALSE, collapsed = FALSE,
            selectInput("city3", label = "City", choices = c("concord", "dover", "exeter", "hampton", "keene", "laconia", "lebanon", "manchester", "milford", "nashua", "nh_state", "portsmouth", "rochester", "salem", "somersworth"), selected = "dover") %>%
              helper(icon = "question",
                     colour = "teal",
                     type = "markdown",
                     content = "Help7") 
          )
        ),
        fluidRow(
          box(
            title = "Commonality Cloud", width = 6, solidHeader = TRUE, status = "success", background = NULL, collapsible = TRUE, collapsed = FALSE,
            plotOutput("Plot_5") %>%
              helper(icon = "question",
                     colour = "orange",
                     type = "markdown",
                     content = "Help8") 
          ),
          box(
            title = "Comparison Cloud", width = 6, solidHeader = TRUE, status = "info", background = NULL, collapsible = TRUE, collapsed = FALSE,
            plotOutput("Plot_6") %>%
              helper(icon = "question",
                     colour = "navy",
                     type = "markdown",
                     content = "Help9") 
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              solidHeader = TRUE,
              title = "Twiiter Stats",
              background = NULL,
              status = "danger",
              collapsible=TRUE,
              footer = fluidRow(
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head1"), 
                    text = textOutput("stat1"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head2"), 
                    text = textOutput("stat2"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head3"), 
                    text = textOutput("stat3"), 
                    right_border = FALSE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head4"), 
                    text = textOutput("stat4"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head5"), 
                    text = textOutput("stat5"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head6"), 
                    text = textOutput("stat6"), 
                    right_border = FALSE,
                    margin_bottom = TRUE
                  )
                )
              )
            ),
            box(
              solidHeader = TRUE,
              title = "Status summary",
              background = NULL,
              status = "danger",
              collapsible=TRUE,
              footer = fluidRow(
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head7"), 
                    text = textOutput("stat7"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head8"), 
                    text = textOutput("stat8"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head9"), 
                    text = textOutput("stat9"), 
                    right_border = FALSE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head10"), 
                    text = textOutput("stat10"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head11"), 
                    text = textOutput("stat11"), 
                    right_border = TRUE,
                    margin_bottom = TRUE
                  )
                ),
                column(
                  width = 4,
                  descriptionBlock(
                    header = textOutput("head12"), 
                    text = textOutput("stat12"), 
                    right_border = FALSE,
                    margin_bottom = TRUE
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Radar Plot", width = 6, solidHeader = TRUE, status = "success", background = NULL, collapsible = TRUE, collapsed = FALSE,
            chartJSRadarOutput("Plot_7") %>%
              helper(icon = "question",
                     colour = "light-blue",
                     type = "markdown",
                     content = "Help4")
          ),
          box(
            title = "Radar Plot", width = 6, solidHeader = TRUE, status = "info", background = NULL, collapsible = TRUE, collapsed = FALSE,
            chartJSRadarOutput("Plot_8") %>%
              helper(icon = "question",
                     colour = "fuchsia",
                     type = "markdown",
                     content = "Help4")
          )
        )
      )
    )
  )
)
)

server <- function(input, output, session) {
  update2 <- reactive({
    update2 <- c("concord", "dover", "exeter", "hampton", "keene", "laconia", "lebanon", "manchester", "milford", "nashua", "nh_state", "portsmouth", "rochester", "salem", "somersworth")
    update2 <- update2[update2 != input$city2]
    update2
  })
  observe({
    updateSelectInput(session, "city3", choices = update2())
  })
  filter0 <- reactive({
    test <- bind_tf_idf(get(paste0(input$city1, "_df")), term, document, count)
    df <- test %>%
      group_by(term) %>%
      summarise(freq = sum(tf_idf))
    df
  })
  filter1 <- reactive({
    temp <- paste0(input$city1, "2")
    it <- itoken(get(temp))
    vocab <- create_vocabulary(it, ngram = c(ngram_min = as.integer(input$`Grouping Method`), ngram_max = as.integer(input$`Grouping Method`)))
    vocab$term <- gsub("_", " ", vocab$term)
    vocab
  })
  output$Plot_1 <- renderPlot({
    if (input$`Grouping Method`==1){
      par(mar=rep(0,4))
      wordcloud(filter1()$term, filter1()$term_count, min.freq = 5, max.words = 100, colors=brewer.pal(8, "Paired"), scale=c(4,.5))
    }
    if (input$`Grouping Method`==2){
      par(mar=rep(0,4))
      wordcloud(filter1()$term, filter1()$term_count, min.freq = 4, max.words = 80, colors=brewer.pal(8, "Paired"), scale=c(3,.375))
    }
    if (input$`Grouping Method`==3){
      par(mar=rep(0,4))
      wordcloud(filter1()$term, filter1()$term_count, min.freq = 3, max.words = 60, colors=brewer.pal(8, "Paired"), scale=c(2,.25))
    }
    if (input$`Grouping Method`=="tf-idf"){
      par(mar = rep(0, 4))
      wordcloud(filter0()$term, filter0()$freq, min.freq=3, random.order = FALSE, max.words=100,colors=brewer.pal(8, "Paired"), scale=c(3,.375))
      par(mar = rep(0, 4))
    }
  })
  filter2 <- reactive({
    nrc_lex <- get_sentiments("nrc")
    nrc <- inner_join(get(paste0(input$city1, "_df2")), nrc_lex, by = c("term" = "word"))
    nrc_noposneg <- nrc[!(nrc$sentiment %in% c("positive","negative")),]
    emotion_summary <- nrc_noposneg %>%
      group_by(sentiment) %>%
      summarise(review_sentiment = sum(count)) %>%
      arrange(desc(review_sentiment))
    emotion_summary
  })
  output$Plot_2 <- renderChartJSRadar({
    chartJSRadar(filter2())
  })
  filter3 <- reactive({
    jr_lex <- lexicon::hash_sentiment_jockers_rinker
    jr_lex <- rbind(jr_lex, list("discuss", .4))
    jr_lex <- setkey(jr_lex, x)
    dataframe <- data.frame(text=get(input$city1))
    dataframe$text <- as.character(dataframe$text)
    eval <- left_just(data.frame(
      sentimentr_jockers_rinker = round(sentiment(dataframe$text, question.weight = 0)[["sentiment"]], 2),
      sentences = dataframe$text,
      stringsAsFactors = FALSE
    ), "sentences")
    out <- sentiment_by(eval$sentences, polarity_dt = jr_lex)
    extract <- extract_sentiment_terms(eval$sentences, polarity_dt= jr_lex)
    extract$ave_sentiment <- out$ave_sentiment
    extract$word_count <- out$word_count
    extract
  })
  output$Plot_3 <- renderPlot({
    ggplot(filter3(),aes(element_id,ave_sentiment))+geom_smooth(fill="green") + theme_bw() + geom_hline(yintercept = 0, color = "black")+xlab("Tweet")+ylab("Sentiment")+ggtitle(paste0(capitalizeStrings(input$city1), " Police Sentiment"))
  })
  output$Plot_4 <- renderPlot({
    ggplot(filter3(),aes(element_id,ave_sentiment))+geom_bar(stat = "identity", fill="green") + theme_bw() + geom_hline(yintercept = 0, color = "black")+xlab("Tweet")+ylab("Sentiment")+ggtitle(paste0(capitalizeStrings(input$city1), " Police Sentiment"))
  })
  filter4 <- reactive({
    full_df <- merge(get(paste0(input$city2, "_df2")), get(paste0(input$city3, "_df2")), by = "term", all = TRUE)
    full_df[is.na(full_df)] <- 0
    full_matrix<-as.matrix(full_df[,-1])
    rownames(full_matrix)<-full_df[,1]
    colnames(full_matrix) <- c(paste0(input$city2, " police"), paste0(input$city3, " police"))
    full_matrix
  })
  output$Plot_5 <- renderPlot({
    par(mar=rep(0,4))
    commonality.cloud(filter4(),colors=brewer.pal(8, "Dark2"),max.words = 100, random.order=FALSE)
    par(mar=rep(0,4))
  })
  filter5 <- reactive({
    full_df <- merge(get(paste0(input$city2, "_df2")), get(paste0(input$city3, "_df2")), by = "term", all = TRUE)
    full_df[is.na(full_df)] <- 0
    full_matrix<-as.matrix(full_df[,-1])
    rownames(full_matrix)<-full_df[,1]
    colnames(full_matrix) <- c(paste0(input$city2, " police"), paste0(input$city3, " police"))
    full_matrix
  })
  output$Plot_6 <- renderPlot({
    par(mar=rep(0,4))
    comparison.cloud(filter5(),colors=brewer.pal(8, "Dark2"),max.words = 100, random.order=FALSE, scale = c(4,.5))
    par(mar=rep(0,4))
  })
  filter6 <- reactive({
    nrc_lex <- get_sentiments("nrc")
    nrc <- inner_join(get(paste0(input$city2,"_df2")), nrc_lex, by = c("term" = "word"))
    nrc_noposneg <- nrc[!(nrc$sentiment %in% c("positive","negative")),]
    emotion_summary <- nrc_noposneg %>%
      group_by(sentiment) %>%
      summarise(review_sentiment = sum(count)) %>%
      arrange(desc(review_sentiment))
  })
  filter7 <- reactive({
    nrc_lex <- get_sentiments("nrc")
    nrc <- inner_join(get(paste0(input$city3,"_df2")), nrc_lex, by = c("term" = "word"))
    nrc_noposneg <- nrc[!(nrc$sentiment %in% c("positive","negative")),]
    emotion_summary <- nrc_noposneg %>%
      group_by(sentiment) %>%
      summarise(review_sentiment = sum(count)) %>%
      arrange(desc(review_sentiment))
  })
  output$Plot_7 <- renderChartJSRadar({
    chartJSRadar(filter6())
  })
  output$Plot_8 <- renderChartJSRadar({
    chartJSRadar(filter7())
  })
  output$value1_1 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][1], info[input$city2][[1]][1], color = "purple", icon = NULL
    )
  })
  output$value1_2 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][2], info[input$city2][[1]][2], color = "purple"
    )
  })
  output$value1_3 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][3], info[input$city2][[1]][3], color = "purple"
    )
  })
  output$value1_4 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][4], info[input$city2][[1]][4], color = "purple"
    )
  })
  output$value1_5 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][5], info[input$city2][[1]][5], color = "purple"
    )
  })
  output$value1_6 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][6], info[input$city2][[1]][6], color = "purple"
    )
  })
  output$value2_1 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][1], info[input$city2][[1]][1], color = "purple"
    )
  })
  output$value2_2 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][2], info[input$city2][[1]][2], color = "purple"
    )
  })
  output$value2_3 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][3], info[input$city2][[1]][3], color = "purple"
    )
  })
  output$value2_4 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][4], info[input$city2][[1]][4], color = "purple"
    )
  })
  output$value2_5 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][5], info[input$city2][[1]][5], color = "purple"
    )
  })
  output$value2_6 <- renderInfoBox({
    infoBox(
      info["labels"][[1]][6], info[input$city2][[1]][6], color = "purple"
    )
  })
  output$abc <- renderValueBox({
    valueBox(
      "hi", "test", color = "purple", width = 2
    )
  })
  output$head1 <- renderText({
    "Screen Name"
  })
  output$stat1 <- renderText({
    info[input$city2][[1]][2]
  })
  output$head2 <- renderText({
    "Number of Tweets"
  })
  output$stat2 <- renderText({
    info[input$city2][[1]][1]
  })
  output$head3 <- renderText({
    "Number of Retweets"
  })
  output$stat3 <- renderText({
    info[input$city2][[1]][3]
  })
  output$head4 <- renderText({
    "Favorites Count"
  })
  output$stat4 <- renderText({
    info[input$city2][[1]][4]
  })
  output$head5 <- renderText({
    "Number of Followers"
  })
  output$stat5 <- renderText({
    info[input$city2][[1]][5]
  })
  output$head6 <- renderText({
    "Creation Date"
  })
  output$stat6 <- renderText({
    info[input$city2][[1]][6]
  })
  output$head7 <- renderText({
    "Screen Name"
  })
  output$stat7 <- renderText({
    info[input$city3][[1]][2]
  })
  output$head8 <- renderText({
    "Number of Tweets"
  })
  output$stat8 <- renderText({
    info[input$city3][[1]][1]
  })
  output$head9 <- renderText({
    "Number of Retweets"
  })
  output$stat9 <- renderText({
    info[input$city3][[1]][3]
  })
  output$head10 <- renderText({
    "Favorites Count"
  })
  output$stat10 <- renderText({
    info[input$city3][[1]][4]
  })
  output$head11 <- renderText({
    "Number of Followers"
  })
  output$stat11 <- renderText({
    info[input$city3][[1]][5]
  })
  output$head12 <- renderText({
    "Creation Date"
  })
  output$stat12 <- renderText({
    info[input$city3][[1]][6]
  })
  observe_helpers(help_dir = "markdown")
  output$link1 <- renderUI({
    tagList("", a(h4("https://github.com/jrdickinson/NH-Police-Tweets"), href=paste("https://github.com/jrdickinson/NH-Police-Tweets")))
  })
}

shinyApp(ui, server)