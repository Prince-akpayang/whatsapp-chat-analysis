library(shiny)
library(shinydashboard)
library(rwhatsapp)
library(ggplot2)
library(lubridate)
library(tidytext)
library(stopwords)
library(tidyverse)
library(tidymodels)
library(syuzhet)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(tm)
library(quanteda)
library(wordcloud)
library(emojifont)
library(emoji)

# UI for the Shiny app
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "WhatsApp Chat Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Data Overview", tabName = "data", icon = icon("database")),
      menuItem("Message Count", tabName = "message_count", icon = icon("envelope")),
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile")),
      menuItem("Emoji Analysis", tabName = "emoji", icon = icon("grin")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("Summary", tabName = "summary", icon = icon("clipboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title = "Introduction", width = 12, 
                    "This application provides insights into WhatsApp chat history for multiple users.")
              )),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Upload WhatsApp Data", width = 12,
                    fileInput("file1", "Choose a WhatsApp Text File", accept = ".txt"),
                    "Upload the WhatsApp chat text file in the appropriate format.")
              )),
      tabItem(tabName = "message_count",
              fluidRow(
                box(title = "Message Count Analysis", width = 12, 
                    plotOutput("messageCountPlot"))
              )),
      tabItem(tabName = "sentiment",
              fluidRow(
                box(title = "Sentiment Analysis", width = 12, 
                    plotOutput("sentimentPlot"))
              )),
      tabItem(tabName = "emoji",
              fluidRow(
                box(title = "Emoji Analysis", width = 12, 
                    plotOutput("emojiPlot"))
              )),
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Word Cloud", width = 12,
                    selectInput("userSelection", "Select User:",
                                choices = NULL),  
                    plotOutput("wordCloudPlot"))
              )),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary", width = 12,
                    "This analysis provides a comprehensive view of chat activity, including message counts, sentiment, emoji usage, and word cloud generation.")
              ))
    )
  )
)

server <- function(input, output, session) {
  
  whatsapp_data <- reactive({
    req(input$file1)
    wat.df <- rwhatsapp::rwa_read(input$file1$datapath)
    
    clean_text_column <- function(column) {
      column <- gsub("[^[:alpha:][:space:]]", " ", column)
      column <- gsub("\\s+", " ", column)
      column <- trimws(column)
      return(column)
    }
    
    wat.df$author <- clean_text_column(wat.df$author)
    data.frame(wat.df) %>% filter(!is.na(author)) %>% select(-source)
  })
  
  observeEvent(whatsapp_data(), {
    updateSelectInput(session, "userSelection",
                      choices = c("All", unique(whatsapp_data()$author)),
                      selected = "All")
  })
  
  output$messageCountPlot <- renderPlot({
    req(whatsapp_data())
    message_counts <- whatsapp_data() %>% 
      group_by(author) %>%
      summarise(Message_Count = n()) %>%
      arrange(desc(Message_Count))
    
    ggplot(message_counts, aes(x = reorder(author, Message_Count), y = Message_Count)) +
      geom_col(aes(fill = author)) +
      coord_flip() + theme_bw() +
      ggtitle("Number of Messages by Each Author")
  })
  
  output$sentimentPlot <- renderPlot({
    req(whatsapp_data())
    lexicon <- get_sentiments("bing")
    sentiment_table <- whatsapp_data() %>%
      unnest_tokens(word, text) %>%
      inner_join(lexicon) %>%
      mutate(sentiment = as.factor(sentiment)) %>%
      group_by(author, sentiment) %>%
      count() %>%
      ungroup()
    
    ggplot(data = sentiment_table, aes(x = author, y = n, fill = sentiment)) +
      geom_col(position = "dodge") +
      facet_wrap(~sentiment) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
  
  output$emojiPlot <- renderPlot({
    req(whatsapp_data())
    
    emoji_counts <- table(unlist(whatsapp_data()$emoji))
    top_32_emojis <- head(sort(emoji_counts, decreasing = TRUE), 32)
    
    emoji_data <- data.frame(
      Emoji = names(top_32_emojis),
      Frequency = as.numeric(top_32_emojis)
    )
    
    ggplot(emoji_data, aes(x = reorder(Emoji, Frequency), y = Frequency)) +
      geom_col(aes(fill = Frequency)) +
      coord_flip() +
      xlab("Emoji") +
      ylab("Frequency") +
      ggtitle("Top 32 Most Frequently Used Emojis")+
    theme(text = element_text(family = "EmojiFont"))
  })
  
  output$wordCloudPlot <- renderPlot({
    req(whatsapp_data())
    
    data <- whatsapp_data()
    selected_user <- input$userSelection
    
    if (selected_user != "All") {
      data <- data %>% filter(author == selected_user)
    }
    
    data$text <- gsub("[[:punct:]]|[[:digit:]]|[[:cntrl:]]", "", data$text)
    data$text <- tolower(trimws(data$text))
    
    data <- data %>%
      filter(!str_detect(text, "media omitted|you deleted this message|this message was deleted"))
    
    word_freq <- str_split(data$text, "\\s+") %>%
      unlist() %>%
      table() %>%
      sort(decreasing = TRUE) %>%
      data.frame()
    
    colnames(word_freq) <- c("words", "Freq")
    
    wordcloud(words = word_freq$words, freq = word_freq$Freq, min.freq = 2,
              max.words = 500, random.order = FALSE, rot.per = 0.35,
              colors = brewer.pal(8, "Dark2"))
  })
}

# Run the Shiny app
shinyApp(ui, server)
