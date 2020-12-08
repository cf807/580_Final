library(tidyverse)
library(tidytext)
library(scales)
library(igraph)
library(ggraph)
library(widyr)
library(psych)
library(plotly)
library(ggcorrplot)
library(reticulate)
library(cleanNLP)
library(packcircles)
library(patchwork)

recipe <- read.csv('recipes.csv')
recipe <- recipe[-1]



mod_recipe <- recipe %>% 
  mutate(ing = str_extract_all(Ingredients, "\\[.*?\\]")) %>% 
  mutate(Category = tolower(Category),
         Category = str_trim(str_replace_all(Category, "\\[.*?\\]", "")))

recipe_tokens <- mod_recipe %>%
  select(line = Category, ing, Tags, everything(), -Instructions, -Ingredients) %>% 
  unnest_tokens(word, ing, strip_numeric = TRUE) %>%
  mutate(word = str_replace_all(word, "'s$", ""))

tidy_tokens_ns <- recipe_tokens %>% 
  anti_join(stop_words, by = "word")

custom_stop_words <- bind_rows(data_frame(word = c('food'), 
                                          lexicon = c("custom")), 
                               stop_words)

tidy_tokens_ns <- tidy_tokens_ns %>% 
  anti_join(custom_stop_words, by = "word")

top_30_ing <- tidy_tokens_ns %>%
  count(word, sort = TRUE) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>%
  top_n(30, proportion) %>% # Keep top 30 proportion values 
  mutate(word = reorder(word, proportion)) %>% # Fixing levels
  ggplot(aes(word, percent(proportion))) + # labels the proportions
  geom_col(fill = "#19c0f4") +
  labs(x = NULL,
       y = "Word Frequency",
       title = "Most Frequent Ingredients") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

top_30_ing

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


main_r <- unique(factor(recipe$Category))

top_10_ing_freq_meal <- tidy_tokens_ns %>%
  count(line, word, sort = TRUE)%>% 
  group_by(line) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>%
  top_n(10, proportion)  %>% 
  ggplot(aes(reorder_within(word, n, line), percent(n), fill = line)) +
  geom_col() +
  scale_x_reordered() +
  labs(x = NULL,
       y = "Ingredient Frequency",
       title = "Frequent Ingredients") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(~ factor(str_to_title(line)), scales = 'free', ncol = 3) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=6),
        axis.text.y = element_text(size=4),
        strip.text = element_text(size=6))
top_10_ing_freq_meal


tidy_tokens_rec_tf_idf <- tidy_tokens_ns %>%
  count(line, word, sort = TRUE) %>%
  ungroup() %>% 
  bind_tf_idf(word, line, n)

top_10_tf_idf_recipe <- tidy_tokens_rec_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(line) %>% 
  top_n(10, tf_idf) %>%
  ungroup %>%
  ggplot(aes(reorder_within(word, tf_idf, line), tf_idf, fill = line)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  labs(x = NULL, 
       y = "tf-idf",
       title = "Ingredients by tf-Idf") +
  facet_wrap(~ factor(str_to_title(line)), scales = 'free', ncol = 3) +
  coord_flip() 
  

top_10_tf_idf_recipe

frequency_by_meal <- tidy_tokens_ns %>%
  count(line, word, sort = TRUE) %>% 
  group_by(line) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from='line', values_from='proportion')
cor_all_rec <- corr.test(frequency_by_meal[, -1], adjust = "none")
cor_plot_rec <- ggcorrplot(cor_all_rec[["r"]], # Just the correlation matrix
                       hc.order = TRUE, # Ordered using clustering
                       type = "lower",
                       method = "circle",
                       colors = c("#E46726", "white", "#6D9EC1"), # low, mid high
                       lab = TRUE,
                       lab_size = 2.5)

cor_plot_rec

dinner_appe_words <- frequency_by_meal %>% 
  select(word, dinners, `appetizers and snacks`) %>% 
  ggplot(aes(x = dinners, y = `appetizers and snacks`, color = abs(dinners - `appetizers and snacks`), label = word)) +
  geom_abline(color = "yellow", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  scale_x_log10(labels = percent_format(accuracy=0.01)) +
  scale_y_log10(labels = percent_format(accuracy=0.01)) +
  labs(x = "Dinners Ingredients",
       y = "Appetizers and Snacks Ingredients",
       title = "Ingredient Frequncy Comparison: Dinner vs Appetizers/Snacks") +
  theme(legend.position = "none")

dinner_appe_words

ggplotly(dinner_appe_words, tooltip = c("word"))

word_ratios_din_app <- tidy_tokens_ns %>%
  filter(line %in% c("appetizers and snacks", "dinners")) %>% 
  count(word, line) %>%
  filter(n >= 10) %>%
  spread(line, n, fill = 0) %>%
  mutate(across(where(is.numeric), ~(.+1)/sum(.+1))) %>% # Avoiding 0/0 issues
  # mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>% 
  mutate(log_ratio = log2(`appetizers and snacks` / dinners)) %>% # log base 2 scale
  arrange(desc(log_ratio))

knitr::kable(word_ratios_din_app %>% 
               arrange(abs(log_ratio)) %>% 
               head(10), "html") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

word_ratios_din_app %>%
  group_by(direction = ifelse(log_ratio < 0, 'dinners', "appetizers and snacks")) %>%
  top_n(15, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, log_ratio)) %>% # Re-ordering factor for plotting
  ggplot(aes(word, log_ratio, color = direction)) +
  geom_segment(aes(x = word, xend = word,
                   y = 0, yend = log_ratio),
               size = 1.1, alpha = 0.6) +
  geom_point(size = 2.5) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, 
       y = "Relative Occurrence",
       title = "Distinguishable Ingredients between Dinner and Appetizer/Snack Meals") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(-6, 6), 
                     labels = c("64x", "32x", "16x","8x", "4x", "2x", # This is due to log base 2
                                "Same", "2x", "4x", "8x", "16x", "32x", "64x")) +
  scale_color_manual(values = c("#daad62", "#9c311f"))












