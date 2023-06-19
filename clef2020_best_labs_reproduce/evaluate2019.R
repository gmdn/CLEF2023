library(readr)
library(tibble)
library(dplyr)
library(ggplot2)

source("../official/load_data.R")

rels_per_topic <- qrels_abs_test %>%
  group_by(topic) %>%
  summarize(num_docs = n(), num_rels = sum(relevant)) %>%
  ungroup() %>%
  arrange(topic)

topics <- sort(unique(qrels_abs_test$topic))

# read all local runs
path_to_folder <- "./runs/local/"

experiments <- list.files(path_to_folder)

results <- tibble(topic = character(),
                  Q0 = integer(),
                  document = character(),
                  rank = integer(),
                  score = double(),
                  run = character(),
                  relevance = integer()
)

for (experiment in experiments) { 
  
  exp <- read_delim(file = paste0(path_to_folder, "/", experiment), 
                    delim = " ", 
                    col_names = c("topic", 
                                  "Q0", 
                                  "document", 
                                  "rank", 
                                  "score", 
                                  "run",
                                  "relevance"), 
                    col_types = cols(col_character(),
                                     col_integer(),
                                     col_character(),
                                     col_integer(),
                                     col_double(),
                                     col_character(),
                                     col_integer()))
  
  exp <- exp %>% 
    distinct(topic, document, .keep_all = TRUE) %>%
    group_by(topic) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  results <- results %>%
    bind_rows(exp)
  
  #break
  
}


# subset experiments

results_bm25 <- results %>% 
  filter(grepl(x = run, pattern = "bm25")) %>%
  group_by(run, topic) %>%
  summarize(num_shown = n(),
            rels_found = sum(relevance)) %>%
  ungroup() %>%
  inner_join(rels_per_topic) %>%
  mutate(type = "bm25")

bm25_at_k <- results %>% 
  filter(grepl(x = run, pattern = "bm25")) %>%
  inner_join(rels_per_topic) %>%
  filter(rank <= num_rels) %>%
  group_by(run, topic) %>%
  summarize(recall_at_k = sum(relevance) / num_rels[1]) %>%
  ungroup() %>%
  mutate(type = "bm25")

results_bm25 <- results_bm25 %>% 
  inner_join(bm25_at_k)




results_equal<- results %>% 
  filter(grepl(x = run, pattern = "official")) %>%
  group_by(run, topic) %>%
  summarize(num_shown = n(),
            rels_found = sum(relevance)) %>%
  ungroup() %>%
  inner_join(rels_per_topic) %>%
  mutate(type = "equal")


equal_at_k <- results %>% 
  filter(grepl(x = run, pattern = "official")) %>%
  inner_join(rels_per_topic) %>%
  filter(rank <= num_rels) %>%
  group_by(run, topic) %>%
  summarize(recall_at_k = sum(relevance) / num_rels[1]) %>%
  ungroup() %>%
  mutate(type = "equal")

results_equal <- results_equal %>%
  inner_join(equal_at_k)



results_distributed <- results %>% 
  filter(grepl(x = run, pattern = "distributed")) %>%
  group_by(run, topic) %>%
  summarize(num_shown = n(),
            rels_found = sum(relevance)) %>%
  ungroup() %>%
  inner_join(rels_per_topic) %>%
  mutate(type = "prop")


prop_at_k <- results %>% 
  filter(grepl(x = run, pattern = "distributed")) %>%
  inner_join(rels_per_topic) %>%
  filter(rank <= num_rels) %>%
  group_by(run, topic) %>%
  summarize(recall_at_k = sum(relevance) / num_rels[1]) %>%
  ungroup() %>%
  mutate(type = "prop")

results_prop <- results_distributed %>%
  inner_join(prop_at_k)




levels <- c("2019_baseline_bm25_t200",
            "2019_baseline_bm25_t400",
            "2019_baseline_bm25_t600",
            "2019_baseline_bm25_t800",
            "2019_baseline_bm25_t1000",
            "2019_baseline_bm25_t2000",
            "2019_baseline_bm25_t3000")

library(ggplot2)

results_bm25 %>%
  mutate(recall = rels_found / num_rels) %>%
  ggplot(aes(x = topic, y = recall)) +
  geom_point(aes(group = factor(run, levels), colour = factor(run, levels))) +
  guides(colour=guide_legend(title="run")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



results_bm25 %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(total_shown = sum(num_shown), 
            avg_recall = mean(recall)) %>%
  arrange(total_shown) %>%
  ggplot(aes(x = total_shown, y = avg_recall)) +
  geom_point() + 
  geom_line()


results_equal %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(total_shown = sum(num_shown), 
            avg_recall = mean(recall)) %>%
  arrange(total_shown) %>%
  ggplot(aes(x = total_shown, y = avg_recall)) +
  geom_point() + 
  geom_line()


results_distributed %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(total_shown = sum(num_shown), 
            avg_recall = mean(recall)) %>%
  arrange(total_shown) %>%
  ggplot(aes(x = total_shown, y = avg_recall)) +
  geom_point() + 
  geom_line()

