library(tidytext)
library(tidyverse)
library(spacyr)
library(igraph)
library(ggraph)
library(widyr)
library(tm)
library(conflicted)
library(fs)
library(tokenizers)



# 
# stringi::stri_replace_all_fixed(
#   ., 
#   c("ä", "ö", "ü", "Ä", "Ö", "Ü"), 
#   c("ae", "oe", "ue", "Ae", "Oe", "Ue"), 
#   vectorize_all = FALSE
# ) %>% enc2utf8(.)

stopwords <- tibble(word = stopwords::stopwords(language = "de", 
                                                source = "stopwords-iso"))

# Lemma lookup table
# https://github.com/michmech/lemmatization-lists
lemma_lookup <- read_delim("lemmatization-de_noun.txt", delim = "\t") %>%
  set_names(c("lemma", "word"))
lemma_lookup$lemma <- lemma_lookup$lemma %>% str_to_lower()
lemma_lookup$word <- lemma_lookup$word %>% str_to_lower()

# Get all txt files
txt_files <- dir_ls("texts/", glob = "*.txt", recursive = TRUE)

# Function to process texts
process_text <- function(path) {
  sample_text <- read_file(path)[1]
  
  # Tokenize into sentences
  sentences <- tibble(
    text = tokenize_sentences(sample_text)[[1]]) %>%
    rownames_to_column(var = "line")
  
  # Tidy text
  tidy_text <- sentences %>%
    unnest_tokens(word, text) %>%
    anti_join(stopwords, by = "word") %>%
    left_join(lemma_lookup, by = "word") %>%
    mutate(
      lemma = coalesce(lemma, word)
    )
  
  # Build pairs
  pairs <- pairwise_count(tidy_text, 
                          item = lemma, 
                          feature = line,
                          sort = TRUE) %>%
    dplyr::filter(n > 1)
  
  
  # Build graph
  graph <- graph_from_data_frame(pairs) %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), 
                     edge_colour = "cyan4") +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.8, "lines")) +
      theme(axis.ticks = element_line(linetype = "blank"), 
           panel.grid.major = element_line(linetype = "blank"), 
           panel.grid.minor = element_line(linetype = "blank"), 
           axis.text = element_text(colour = NA), 
           panel.background = element_rect(fill = NA), 
           legend.position = "none", legend.direction = "horizontal") +labs(x = NULL, y = NULL)
  
  
  file_name <- substr(path, 7, nchar(path) - 4)
  
  ggsave(paste0("graphs/", file_name, ".png"), graph)
}


txt_files %>%
  walk(~ process_text(.))


