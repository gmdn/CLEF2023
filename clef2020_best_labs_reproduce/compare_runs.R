library(ggplot2)

tpc <- "CD000996"
tpc <- "CD001261"
tpc <- "CD008874"
tpc <- "CD010753"
tpc <- "CD011977"
tpc <- "CD011768"
tpc <- "CD012233"


num_rel <- qrels_abs_test %>% 
  filter(topic == tpc) %>% 
  summarize(num_rel = sum(relevant)) %>%
  pull()

# give run name
run_id <- paste0("2019_official_", id)
#run_id <- paste0("2019_official_", thresh)

run <- read.table(paste0("./runs/local/", run_id),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  colClasses = c("character",
                                 "numeric",
                                 "character",
                                 "numeric",
                                 "numeric",
                                 "character",
                                 "numeric"))

names(run) <- c("topic_id",
                "interaction",
                "pid",
                "rank",
                "score",
                "run_id",
                "relevance")


run <- run %>% 
  distinct(topic_id, pid, .keep_all = TRUE) %>%
  group_by(topic_id) %>%
  mutate(rank = row_number()) %>%
  ungroup()


run_original <- run %>% 
  filter(topic_id == tpc) %>%
  select(pid, rank, relevance) %>%
  mutate(rel_retr = cumsum(relevance), relevant = num_rel) %>%
  mutate(recall_at = rel_retr / relevant)
  

run %>% 
  filter(topic_id == tpc) %>%
  select(pid, rank, relevance) %>%
  mutate(rel_retr = cumsum(relevance), relevant = num_rel) %>%
  mutate(recall_at = rel_retr / relevant) %>%
  ggplot(aes(rank, recall_at)) + 
  geom_line()
  






run <- read.table(paste0("./runs/UoA/abs/abs-hh-ratio-ilps@uva"),
                  header = FALSE,
                  stringsAsFactors = FALSE,
                  colClasses = c("character",
                                 "numeric",
                                 "character",
                                 "numeric",
                                 "numeric",
                                 "character"))

names(run) <- c("topic_id",
                "interaction",
                "pid",
                "rank",
                "score",
                "run_id")


run <- run %>% 
  distinct(topic_id, pid, .keep_all = TRUE) %>%
  group_by(topic_id) %>%
  mutate(rank = row_number()) %>%
  ungroup()

results <- run %>% 
  inner_join(qrels_abs_test, by = c("topic_id" = "topic", "pid" = "pid")) %>%
  rename(relevance = relevant, Q0 = interaction) %>%
  select(topic_id, Q0, pid, rank, score, run_id, relevance)


num_rel <- qrels_abs_test %>% 
  filter(topic == tpc) %>% 
  summarize(num_rel = sum(relevant)) %>%
  pull()

run_uva_hh <- results %>% 
  filter(topic_id == tpc) %>%
  select(pid, rank, relevance) %>%
  mutate(rel_retr = cumsum(relevance), relevant = num_rel) %>%
  mutate(recall_at = rel_retr / relevant)




run_uva_hh %>% 
  #filter(topic_id == tpc) %>%
  select(pid, rank, relevance) %>%
  mutate(rel_retr = cumsum(relevance), relevant = num_rel) %>%
  mutate(recall_at = rel_retr / relevant) %>%
  ggplot(aes(rank, recall_at)) + 
  geom_line()


run_original <- run_original %>%
  mutate(run = "equal")
  
run_uva_hh <- run_uva_hh %>%
  mutate(run = "run_uva_hh")





r1_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")


r2_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")

r3_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")

r4_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")

r5_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")
r5_plot

r6_plot <- run_original %>%
  bind_rows(run_uva_hh) %>%
  filter(rank <= 400) %>%
  ggplot(aes(rank, recall_at)) +
  geom_line(aes(group = run, color = run)) +
  ggtitle(paste0("Topic: ", tpc)) + 
  labs(x = "documents shown (feedback)",
       y = "recall at")
r6_plot


grid.arrange(r1_plot, r6_plot)
