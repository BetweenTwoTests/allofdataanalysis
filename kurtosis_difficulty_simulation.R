library(tidyverse)
library(ggplot2)

make_test_score_df <- function(item_n, person_n, prob = 0.5) {
  if (length(prob) == 1) {
    df <- sapply(seq_len(item_n), function(i) { rbinom(person_n, size = 1, prob = prob) })
  } else {
    # if prob for all items not specified, recycle to lenfth of item_n
    prob <- rep_len(prob, item_n)
    res <- lapply(seq_along(prob), function(i) {
      rbinom(person_n, size = 1, prob[i])
    })
    df <- matrix(unlist(res), ncol = item_n, nrow = person_n, byrow = F)
  }
  
  colnames(df) <- paste0(rep("Item ", item_n), seq_len(item_n))
  
  df <- df %>% as_tibble() %>% 
    mutate(Person =  paste0(rep("Person ", person_n), seq_len(person_n))) %>% 
    pivot_longer(cols = 1:15, names_to = "Item") 
  
  df
}
make_test_score_plot <- function(df, item_n, person_n, subtitle = "") {
  df_Score_by_Person <- df %>% 
    group_by(Person) %>% 
    summarise(
      Score = sum(value)
    )
  as_tibble(
  df_freq <- as.data.frame.table(
    table(Score = factor(df_Score_by_Person$Score, levels = seq(0, item_n, by = 1))), 
    responseName='Frequency'))
  df_freq %>% 
    mutate(RelativeFrequency = Frequency/person_n,
         Score = as.numeric(as.character(Score))) %>% 
  ggplot(aes(x = Score, y = RelativeFrequency)) +
  geom_point() + geom_line() +
  scale_x_continuous(name = "Score", 
                     breaks = seq(0, item_n, by = 1),
                     minor_breaks = seq(0, item_n, by = 1), 
                     limits = c(0, item_n)) + 
  stat_function(fun = dnorm, 
                args = list(mean = item_n * 0.5, sd = item_n * 0.5 * (1-0.5)), # expected normal distribution at p = 0.5
                col = "red") + 
  ggtitle(label = paste0("Distribution of Test Scores for ", person_n, " test takers."),
          subtitle = subtitle)
}
  
df_p25 <- make_test_score_df(item_n = 15, person_n = 1000, prob = 0.25)
make_test_score_plot(df_p25, item_n = 15, person_n = 1000, subtitle = paste0("All items: p_i = ", 0.25, "(hard)"))

df_p50 <- make_test_score_df(item_n = 15, person_n = 1000, prob = 0.50)
make_test_score_plot(df_p50, item_n = 15, person_n = 1000, subtitle = paste0("All items: p_i = ", 0.50, "(medium)"))

df_p75 <- make_test_score_df(item_n = 15, person_n = 1000, prob = 0.75)
make_test_score_plot(df_p75, item_n = 15, person_n = 1000, subtitle = paste0("All items: p_i = ", 0.50, "(easy)"))

df_p02 <- make_test_score_df(item_n = 15, person_n = 1000, prob = 0.02)
make_test_score_plot(df_p02, item_n = 15, person_n = 1000, subtitle = paste0("All items: p_i = ", 0.02, "(very hard)"))

df_p98 <- make_test_score_df(item_n = 15, person_n = 1000, prob = 0.98)
make_test_score_plot(df_p98, item_n = 15, person_n = 1000, subtitle = paste0("All items: p_i = ", 0.98, "(very easy)"))

# Leptokurtic distribution of test scores bc items are too hard or too easy
df_p02_p98 <- make_test_score_df(item_n = 15, person_n = 1000, prob = c(0.02, 0.98))
make_test_score_plot(df_p02_p98, item_n = 15, person_n = 1000, 
                     subtitle = paste0("All items: p_i = 0.02 or p_i = 0.98 (very easy or very hard)"))

# To fix, add more moderate difficutly tests
df_p02_p50_p98 <- make_test_score_df(item_n = 15, person_n = 1000, prob = c(0.02 , 0.50, 0.50, 0.50, 0.98))
make_test_score_plot(df_p02_p50_p98, item_n = 15, person_n = 1000, 
                     subtitle = paste0("All items: p_i = {0.02 , 0.50, 0.50, 0.50, 0.98}"))
