# server.R

library(shiny)
library(stringr)
library(data.table)
library(NLP)


onegram_data <- readRDS("data/1gram.rds")
twogram_data <- readRDS("data/2gram.rds")
threegram_data <- readRDS("data/3gram.rds")
badwords <- readRDS("data/badwords.rds")


input_txt <- function(text) {
  tmp <- unlist(str_split(text, " "))
  tmp <- tmp[tmp != ""]
  return(tmp)
}

input_wd <- function(text) {
  if (text != " ") { 
    words <- input_txt(tolower(text))
    num_words <- length(words)
    if (num_words > 0) {
      filter <- paste("^", words[num_words], sep = "")
      tmp_dt <- onegram_data[n0 %like% filter]
      pred_word <- dim(tmp_dt)[1]
      if (pred_word > 0) {
        tmp_dt <- tmp_dt[order(rank(-freq))]
        pred <- tmp_dt[1]$n0
        if (num_words > 2) {
          tmp_w <- paste(words[1])
          for (i in 2:(num_words - 1)) tmp_w <- paste(tmp_w, words[i])
          return(paste(tmp_w, pred))
        } else if (num_words > 1) {
          tmp_w <- paste(words[1])
          return(paste(tmp_w, pred))
        }
      }
    }
  }
  return(text)
}

best_pred <- function(text) {
  if (text != " ") { 
    input_words <- input_txt(text)
    len <- length(input_words)
    
    if (len > 1) {
      w1 <- input_words[len]
      w2 <- input_words[len - 1]
    } else if (len > 0) {
      w1 <- input_words[len]
      w2 <- "NA"
    } else return("the")

    len3 <- length(threegram_data[threegram_data[n2 == w2 & n1 == w1]]$freq)
    len2 <- length(twogram_data[twogram_data[n1 == w1]]$freq)
    matches <- matrix(nrow = len3 + len2, ncol = 2)
    matches[,1] <- ""
    matches[,2] <- 0
    
    if (len3 > 0) {
      for (i in 1:len3) {
        matches[i, 1] <- threegram_data[threegram_data[n2 == w2 & n1 == w1]]$n0[i]
        cnt2 <- length(twogram_data[twogram_data[n1 == w1 & n0 == matches[i, 1]]]$freq)
        cnt1 <- length(onegram_data[onegram_data[n0 == matches[i, 1]]]$freq)
        if (cnt2 > 0) freq2 <- twogram_data[twogram_data[n1 == w1 & 
                                                     n0 == matches[i, 1]]]$freq else freq2 <- 0
        if (cnt1 > 0) freq1 <- onegram_data[onegram_data[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- threegram_data[threegram_data[n2 == w2 & n1 == w1]]$freq[i] * 
          0.95 + freq2 * 0.05 + freq1 * 0.01     
      }
    }
    if (len2 > 0) {
      for (i in sum(len3, 1):sum(len3, len2)) {
        matches[i, 1] <- twogram_data[twogram_data[n1 == w1]]$n0[i - len3]
        cnt1 <- length(onegram_data[onegram_data[n0 == matches[i, 1]]]$freq)
        if (cnt1 > 0) freq1 <- onegram_data[onegram_data[n0 == matches[i, 1]]]$freq else freq1 <- 0
        matches[i, 2] <- twogram_data[twogram_data[n1 == w1]]$freq[i - len3] * 0.05 + freq1 * 0.01   
      }
    }
    match_len <- length(matches[which.max(matches[,2])])
    if (match_len > 0) return(matches[which.max(matches[,2])])
    return('the')
  }
  return(" ")
}

##############################
shinyServer(
  function(input, output) {
    output$text1 <- renderText({
      paste(input_wd(input$input_str))
    })
    output$text2 <- renderText({
      paste(best_pred(input$input_str))
    })
  }
)