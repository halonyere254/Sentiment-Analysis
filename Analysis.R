#Load required libraries
library(tidyverse)
library(Rcpp)
library(textdata)
library(tidytext)
library(plotly)
library(ggwordcloud)
library(tidyquant)
library(tm)
library(syuzhet)
library(wordcloud2)
library(qdapRegex)
library(highcharter)
library(gt)
library(htmltools)
library(viridis)
library(viridisLite)
library(RCurl)
library(base64enc)
library(readxl)
library(wordcloud2)
library(igraph)
library(ggraph)
library(devtools)
library(visNetwork)
library(pak)

#Load required data
df <- read_excel("biogasoutcomesmalawi.xlsx")

#convert date column into date
df <- df %>%
  mutate(interview_date = as.Date(interview_date))

# Tokenize the responses
df_tokens <- df %>% 
  unnest_tokens(word, interviewer) 
# Calculate sentiment using a lexicon (e.g., bing)
df_sentiment <- df_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(interviewer_category, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

tokenized_data <- df %>%
  select(interviewee) %>%  
  rowid_to_column() %>%
  unnest_tokens(word, interviewee)

# Count word frequencies
tokenized_data %>% count(word, sort = TRUE)

# Get Bing sentiment lexicon
get_sentiments(lexicon = "bing")

# Get AFINN sentiment lexicon
get_sentiments(lexicon = "afinn")

# Join sentiment data from Bing lexicon
sentiments_data <- tokenized_data %>%
  inner_join(get_sentiments("bing"))

custom_colors2 <- viridis::mako(n = 2)
chart_sentiment <-sentiments_data%>%count(sentiment)%>%
  hchart('pie', hcaes(x = sentiment, y = n, color = custom_colors2)) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_tooltip(pointFormat='<b>Proportion: </b> {point.percentage:,.0f}%')%>%
  hc_title(text = 'Sentiment Count',
           style = list(fontSize = '25px', fontWeight = 'bold'))
chart_sentiment

# Define Textprocessing function
Textprocessing <- function(x) {
  x <- gsub("http[[:alnum:]]*", '', x) ## Remove URLs
  x <- gsub("http\\S+\\s*", '', x) ## Remove URLs
  x <- gsub("\\*\\*.*?\\*\\*", "", x) #Removing **interviewer**
  x <- gsub("\\[.*?\\]", "", x) ## Remove text in square brackets
  x <- gsub('[[:cntrl:]]', '', x) ## Remove controls and special characters
  x <- gsub("\\d", '', x) ## Remove digits
  x <- gsub('[[:punct:]]', '', x) ## Remove punctuation
  x <- gsub("^[[:space:]]*", "", x) ## Remove leading whitespaces
  x <- gsub("[[:space:]]*$", "", x) ## Remove trailing whitespaces
  x <- gsub(' +', ' ', x) ## Remove extra whitespaces
  return(x) ## Ensure the processed text is returned
}

# Create the corpus
myCorpus <- Corpus(VectorSource(df$interviewee))

# Convert encoding to handle special characters
myCorpus <- tm_map(myCorpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))

# Apply the custom text processing function
myCorpus <- tm_map(myCorpus, content_transformer(Textprocessing))


#remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
#remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)


# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus,removeWords,stopwords(kind = "en"))
inspect(myCorpus[1:5])


# Create a Term-Document Matrix
tdm_myCorpus <- TermDocumentMatrix(myCorpus)

# Convert to matrix and inspect a subset
tdm_myCorpus <- as.matrix(tdm_myCorpus)
tdm_myCorpus[1:10, 1:20]

# Calculate word frequencies
mfw_myCorpus <- rowSums(tdm_myCorpus)

# Convert to a data frame and filter for visualization
mfw_bar_myCorpus <- as.data.frame(mfw_myCorpus)
mfw_bar_myCorpus <- rownames_to_column(mfw_bar_myCorpus)
names(mfw_bar_myCorpus) <- c('Word', 'Count')

# Sort by count, filter to top 40, and remove specific words
mfw_bar_myCorpus <- mfw_bar_myCorpus %>%
  arrange(desc(Count)) %>%       # Sort by descending count
  head(40) %>%                   # Select top 40 words
  filter(Word != "amp")          # Exclude "amp" if present

# Set custom colors
custom_colors40 <- viridis::mako(n = 40)

# Create the chart
chart_mpw_organic <- hchart(
  mfw_bar_myCorpus, 'column',
  hcaes(x = Word, y = Count, color = custom_colors40)
) %>%
  hc_tooltip(pointFormat = '<b>Count: </b> {point.y} <br>') %>%
  hc_title(
    text = 'Top 40 Most Popular Words',
    style = list(fontSize = '25px', fontWeight = 'bold')
  ) %>%
  hc_add_theme(hc_theme_google())

# Display the chart
chart_mpw_organic



# Define custom colors
custom_colors40_positive <- viridis::mako(n = 40)
custom_colors40_negative <- viridis::mako(n = 40)

# Top 40 Positive Words
positive_words <- sentiments_data %>%
  filter(sentiment == 'positive') %>%   # Filter positive sentiments
  group_by(word) %>%                    # Group by words
  count(sentiment) %>%                  # Count occurrences
  arrange(desc(n)) %>%                         # Sort by count
  head(40) %>%                                 # Limit to top 40 words
  hchart(
    type = "bar", 
    hcaes(x = word, y = n, color = custom_colors40_positive)
  ) %>%
  hc_tooltip(pointFormat = '<b>Count: </b> {point.y} <br>') %>%
  hc_title(
    text = 'Top 40 Positive Words',
    style = list(fontSize = '25px', fontWeight = 'bold')
  ) %>%
  hc_yAxis(title = list(text = "Count")) %>%
  hc_xAxis(title = list(text = "Word"))

# Display the chart
positive_words

# Top 40 Negative Words
negative_words <- sentiments_data %>%
  filter(sentiment == 'negative') %>%   # Filter negative sentiments
  group_by(word) %>%                    # Group by words
  count(sentiment) %>%                  # Count occurrences
  arrange(desc(n)) %>%                         # Sort by count
  head(40) %>%                                 # Limit to top 40 words
  hchart(
    type = "bar", 
    hcaes(x = word, y = n, color = custom_colors40_negative)
  ) %>%
  hc_tooltip(pointFormat = '<b>Count: </b> {point.y} <br>') %>%
  hc_title(
    text = 'Top 40 Negative Words',
    style = list(fontSize = '25px', fontWeight = 'bold')
  ) %>%
  hc_yAxis(title = list(text = "Count")) %>%
  hc_xAxis(title = list(text = "Word"))

# Display the chart
negative_words


# Generate NRC sentiment scores for the corpus
emotion <- get_nrc_sentiment(myCorpus$content)

# Summarize scores into a data frame
emotion <- data.frame(
  sentiment = names(emotion),
  score = colSums(emotion)
)

# Filter out positive and negative sentiments, focusing on emotions
emotion <- emotion %>%
  filter(sentiment != 'positive', sentiment != 'negative') %>%
  mutate(percentage = round(score / sum(score) * 100, 0)) %>%  # Calculate percentages
  arrange(desc(percentage))  # Sort by percentage

# Define custom colors
custom_colors8 <- viridis::mako(n = 8)

# Create the bar chart
chart_emotion_score <- hchart(
  emotion, 'column',
  hcaes(x = sentiment, y = percentage, color = custom_colors8)
) %>%
  hc_tooltip(pointFormat = '<b>Percentage: </b> {point.y}% <br>') %>%
  hc_title(
    text = 'Emotion Score of Interviewee',
    style = list(fontSize = '25px', fontWeight = 'bold')
  ) %>%
  hc_yAxis(title = list(text = "Percentage")) %>%
  hc_xAxis(title = list(text = "Emotion"))

# Display the chart
chart_emotion_score


sentiments_data%>%count(sentiment)
sentiment_by_row_id <- sentiments_data%>%
  select(-word)%>%
  count(rowid,sentiment)%>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0))%>%
  mutate(sentiment = positive - negative)%>%
  left_join(
    df%>%select(interview_id,interview_date,question_id,interviewee,interview_date)%>%rowid_to_column()
  )
sentiment_by_row_id

label_wrap <- label_wrap_gen(width = 60)
formatted <- sentiment_by_row_id%>%
  mutate(text_formatted = str_glue("Row ID:{rowid}
                                   Interviwed At: {interview_date}
                                   Interviwee ID: {interview_id}
                                   Question ID: {question_id}
                                   Response:
                                   {label_wrap(interviewee)}"))
polarity <- formatted%>%
  ggplot(aes(rowid,sentiment))+
  geom_line(color = "#2c3e50",alpha = 0.5)+
  geom_point(aes(interviewee = text_formatted), color = "#2c3e50")+
  geom_smooth(method = "loess", span = 0.25, se = FALSE,color = "blue")+
  geom_hline(aes(yintercept = mean(sentiment)),color = "blue")+
  geom_hline(aes(yintercept = median(sentiment) + 1.96+IQR(sentiment)),color = "red")+
  geom_hline(aes(yintercept = median(sentiment) - 1.96+IQR(sentiment)),color = "red")+
  theme_tq()+
  labs(title = "Sentiment Polarity Of Responses", x = "Interviwee ID",y = "sentiment")
polarity
polarity <- ggplotly(polarity, tooltip = "text")%>%
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )
polarity




# Create word cloud

# Sort by count, filter to top 40, and remove specific words
mfw_bar_myCorpus1 <- mfw_bar_myCorpus %>%
  arrange(desc(Count)) %>%       # Sort by descending count
  head(150) %>%                   # Select top 40 words
  filter(Word != "amp")          # 
word_cloud <- wordcloud2(data = mfw_bar_myCorpus1, size = 0.5, color = "random-light", backgroundColor = "white")

word_cloud




# Create Term-Document Matrix (TDM)
tdm <- TermDocumentMatrix(myCorpus)
tdm_matrix <- as.matrix(tdm)

# Calculate associations for a target word
target_word <- "biogas"  # Replace with your focus word
word_association <- findAssocs(tdm, terms = target_word, corlimit = 0.2)

# Convert associations to data frame
word_association_df <- word_association[[target_word]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df))

chart_word_association <- hchart(
  word_association_df, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association

# Other word association chart(Manure)
# Calculate associations for a target word
target_word1 <- "manure"  # Replace with your focus word
word_association1 <- findAssocs(tdm, terms = target_word1, corlimit = 0.2)

# Convert associations to data frame
word_association_df1 <- word_association1[[target_word1]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df1))

chart_word_association1 <- hchart(
  word_association_df1, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word1),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association1

# Other word association chart(digester)
# Calculate associations for a target word
target_word2 <- "digester"  # Replace with your focus word
word_association2 <- findAssocs(tdm, terms = target_word2, corlimit = 0.2)

# Convert associations to data frame
word_association_df2 <- word_association2[[target_word2]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df2))

chart_word_association2 <- hchart(
  word_association_df2, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word2),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association2

# Other word association chart(digester)
# Calculate associations for a target word
target_word3 <- "feedstock"  # Replace with your focus word
word_association3 <- findAssocs(tdm, terms = target_word3, corlimit = 0.2)

# Convert associations to data frame
word_association_df3 <- word_association3[[target_word3]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df3))

chart_word_association3 <- hchart(
  word_association_df3, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word3),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association3

# Other word association chart(fire wood)
# Calculate associations for a target word
target_word4 <- "firewood"  # Replace with your focus word
word_association4 <- findAssocs(tdm, terms = target_word4, corlimit = 0.2)

# Convert associations to data frame
word_association_df4 <- word_association4[[target_word4]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df4))

chart_word_association4 <- hchart(
  word_association_df4, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word4),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association4

# Other word association chart(gas)
# Calculate associations for a target word
target_word5 <- "gas"  # Replace with your focus word
word_association5 <- findAssocs(tdm, terms = target_word5, corlimit = 0.2)

# Convert associations to data frame
word_association_df5 <- word_association5[[target_word5]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df5))

chart_word_association5 <- hchart(
  word_association_df5, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word5),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association5

# Other word association chart(cooking)
# Calculate associations for a target word
target_word6 <- "cooking"  # Replace with your focus word
word_association6 <- findAssocs(tdm, terms = target_word6, corlimit = 0.2)

# Convert associations to data frame
word_association_df6 <- word_association6[[target_word6]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df6))

chart_word_association6 <- hchart(
  word_association_df6, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word6),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association6


# Other word association chart(water)
# Calculate associations for a target word
target_word7 <- "water"  # Replace with your focus word
word_association7 <- findAssocs(tdm, terms = target_word7, corlimit = 0.2)

# Convert associations to data frame
word_association_df7 <- word_association7[[target_word7]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df7))

chart_word_association7 <- hchart(
  word_association_df7, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word7),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association7

# Other word association chart(people)
# Calculate associations for a target word
target_word8 <- "people"  # Replace with your focus word
word_association8 <- findAssocs(tdm, terms = target_word8, corlimit = 0.2)

# Convert associations to data frame
word_association_df8 <- word_association8[[target_word8]] %>%
  enframe(name = "word", value = "correlation") %>%
  arrange(desc(correlation))

# Create bar chart for word association
custom_colors_association <- viridis::mako(n = nrow(word_association_df8))

chart_word_association8 <- hchart(
  word_association_df8, 'bar',
  hcaes(x = word, y = correlation, color = custom_colors_association)
) %>%
  hc_tooltip(pointFormat = '<b>Correlation: </b> {point.y:.2f}') %>%
  hc_title(
    text = paste("Word Associations with", target_word8),
    style = list(fontSize = "25px", fontWeight = "bold")
  ) %>%
  hc_yAxis(title = list(text = "Correlation")) %>%
  hc_xAxis(title = list(text = "Associated Words"))

# Display chart
chart_word_association8

#Sentiments year on year

# Ensure that each row in df gets a unique 'rowid' 

df <- df %>%
  rowid_to_column("rowid")  # This will create the 'rowid' column

# Tokenize the interviewee responses and perform sentiment analysis
# Tokenize the interviewee responses and perform sentiment analysis
df_tokens <- df %>%
  unnest_tokens(word, interviewee) %>%
  select(rowid, word)

# Get sentiment scores from the 'bing' lexicon (or any other lexicon you prefer)
df_sentiment <- df_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(rowid, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# Now, join sentiment data with the original df (that contains rowid)
df_sentiment <- df_sentiment %>%
  left_join(df %>% select(interview_date, rowid), by = "rowid")

# Extract year from interview_date for grouping
df_sentiment <- df_sentiment %>%
  mutate(interview_year = format(interview_date, "%Y"))

# Summarize sentiment scores by year (average sentiment score per year)
time_sentiment_trends <- df_sentiment %>%
  group_by(interview_year) %>%
  summarize(avg_sentiment_score = mean(sentiment_score)) %>%
  arrange(interview_year)

# Create custom color palette using Mako
custom_colors <- viridis::mako(n = 2)

# Create the sentiment trends chart using hchart
chart_sentiment_trends <- time_sentiment_trends %>%
  hchart(type = "column", hcaes(x = interview_year, y = avg_sentiment_score, color = custom_colors)) %>%
  hc_tooltip(pointFormat = '<b>Average Sentiment Score: </b> {point.y:.1f} <br>') %>%  # Rounded to 1 decimal
  hc_title(
    text = 'Average Sentiment Scores Over the Years',
    style = list(fontSize = '25px', fontWeight = 'bold')
  ) %>%
  hc_yAxis(title = list(text = "Average Sentiment Score")) %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_add_theme(hc_theme_google())

# Display the chart
chart_sentiment_trends
