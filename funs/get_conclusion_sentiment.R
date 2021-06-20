

get_conclusion_sentiment <- function(conclusion, verbose = TRUE) {
  
  flog.info("get_conclusion_sentiment", name = "funlog")  
  
  if (verbose) writeLines("Detecting emotionality of conclusions\n")
  
  
  # get sentiment dict:
  bing_sentiments <- get_sentiments("bing")
  
  # count pos and neg sentiment words:
  pos_neg_count <- 
    conclusion %>% 
    str_squish() %>% 
    tibble(text = .) %>% 
    unnest_tokens(word, text) %>% 
    inner_join(bing_sentiments) %>% 
    count(sentiment)
  
  
  emo_words_in_conclusion <- 
    conclusion %>% 
    str_squish() %>% 
    tibble(text = .) %>% 
    unnest_tokens(word, text) %>% 
    inner_join(bing_sentiments) %>% 
    summarise(emo_words = str_c(word, collapse = " - ")) %>% 
    pull(emo_words)
  
  output <- 
    tibble(
      pos_neg_ratio = pos_neg_count$n[2]/pos_neg_count$n[1],
      emo_count = sum(pos_neg_count$n),
      emo_words_in_conclusion = emo_words_in_conclusion
    )
  
  return(output)
  
}
