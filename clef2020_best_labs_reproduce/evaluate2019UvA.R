library(readr)
library(tibble)
library(dplyr)
library(ggplot2)
library(xtable)

source("../official/load_data.R")

rels_per_topic <- qrels_abs_test %>%
  group_by(topic) %>%
  summarize(num_docs = n(), num_rels = sum(relevant)) %>%
  ungroup() %>%
  arrange(topic)

topics <- sort(unique(qrels_abs_test$topic))

# read all local runs
path_to_folder <- "./runs/UoA/abs/"

experiments <- list.files(path_to_folder)

results <- tibble(topic = character(),
                  Q0 = integer(),
                  document = character(),
                  rank = integer(),
                  score = double(),
                  run = character()
)

for (experiment in experiments) { 
  
  exp <- read_delim(file = paste0(path_to_folder, "/", experiment), 
                    delim = "\t", 
                    col_names = c("topic", 
                                  "Q0", 
                                  "document", 
                                  "rank", 
                                  "score", 
                                  "run"), 
                    col_types = cols(col_character(),
                                     col_integer(),
                                     col_character(),
                                     col_integer(),
                                     col_double(),
                                     col_character()))
  
  exp <- exp %>% 
    distinct(topic, document, .keep_all = TRUE) %>%
    group_by(topic) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    mutate(run = substr(experiment, 1, 12))
  
  results <- results %>%
    bind_rows(exp)
  
  #break
  
}

after_r <- results %>% 
  filter(Q0 == 1) %>% 
  select(topic, rank, run) %>%
  rename(after_rank = rank)

results <- results %>% 
  inner_join(after_r) %>%
  filter(rank < after_rank) %>%
  inner_join(qrels_abs_test, by = c("topic" = "topic", "document" = "pid")) %>%
  rename(relevance = relevant) %>%
  select(topic, Q0, document, rank, score, run, relevance)

# subset experiments

results_hh <- results %>% 
  filter(grepl(x = run, pattern = "hh")) %>%
  group_by(run, topic) %>%
  summarize(num_shown = n(),
            rels_found = sum(relevance)) %>%
  ungroup() %>%
  inner_join(rels_per_topic) %>%
  mutate(type = "abs-hh-ratio")


hh_at_k <- results %>% 
  filter(grepl(x = run, pattern = "hh")) %>%
  inner_join(rels_per_topic) %>%
  filter(rank <= num_rels) %>%
  group_by(run, topic) %>%
  summarize(recall_at_k = sum(relevance) / num_rels[1]) %>%
  ungroup() %>%
  mutate(type = "abs-hh-ratio")

results_hh <- results_hh %>%
  inner_join(hh_at_k)


results_th <- results %>% 
  filter(grepl(x = run, pattern = "th")) %>%
  group_by(run, topic) %>%
  summarize(num_shown = n(),
            rels_found = sum(relevance)) %>%
  ungroup() %>%
  inner_join(rels_per_topic) %>%
  mutate(type = "abs-th-ratio")

th_at_k <- results %>% 
  filter(grepl(x = run, pattern = "th")) %>%
  inner_join(rels_per_topic) %>%
  filter(rank <= num_rels) %>%
  group_by(run, topic) %>%
  summarize(recall_at_k = sum(relevance) / num_rels[1]) %>%
  ungroup() %>%
  mutate(type = "abs-th-ratio")

results_th <- results_th %>%
  inner_join(th_at_k)





avg_hh <- results_hh %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(num_shown = sum(num_shown),
            avg_recall = mean(recall)) %>%
  mutate(type = "abs-hh-ratio") %>%
  ungroup()

avg_th <- results_th %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(num_shown = sum(num_shown),
            avg_recall = mean(recall)) %>%
  mutate(type = "abs-th-ratio") %>%
  ungroup()

  
avg_bm25 %>%
  bind_rows(avg_equal) %>%
  bind_rows(avg_prop) %>%
  bind_rows(avg_hh) %>%
  bind_rows(avg_th) %>%
  ggplot(aes(x = num_shown, y = avg_recall)) +
  geom_line(aes(group = type, color = type)) +
  geom_point(aes(group = type, color = type)) +
  labs(x = "documents shown (feedback)",
       y = "average recall")



results_all <- results_bm25 %>%
  bind_rows(results_equal) %>%
  bind_rows(results_prop) %>%
  bind_rows(results_hh) %>%
  bind_rows(results_th) %>%
  mutate(recall = rels_found / num_rels)
results_all


bm25 <- results_all %>%
  filter(run == "2019_baseline_bm25_t1000") %>%
  select(topic, recall_at_k, recall)

equal <- results_all %>%
  filter(run == "official_m10p100f0t1000p1m10") %>%
  select(topic, recall_at_k, recall)

prop <- results_all %>%
  filter(run == "2019_distributed_effort_m10p100f0t600p1m10s10000") %>%
  select(topic, recall_at_k, recall)

hh <- results_all %>%
  filter(run == "abs-hh-ratio") %>%
  select(topic, recall_at_k, recall)

rels_per_topic %>%
  #inner_join(bm25, by = "topic") %>%
  inner_join(equal, by = "topic") %>%
  #inner_join(prop, by = "topic") %>%
  inner_join(hh, by = "topic") %>%
  xtable()


results_selected <- results_all %>%
  filter(run == "2019_baseline_bm25_t1000" | 
           run == "official_m10p100f0t1000p1m10" |
           run == "official_m10p100f0t600p1m10" |
           run == "2019_distributed_effort_m10p100f0t600p1m10s10000" |
           run == "abs-hh-ratio" |
           run == "abs-th-ratio") 

library(xtable)

results_selected %>%
  group_by(run) %>%
  summarise(recall_at_k = mean(recall_at_k), 
            recall = mean(recall),
            num_shown = sum(num_shown)) %>%
  xtable::xtable()


raa <- results_all[1:10, 1:5]

raa %>% select(run, topic, num_shown) %>% spread(topic, num_shown)
  
  spread(key = topic, value = num_shown, rels_found)

