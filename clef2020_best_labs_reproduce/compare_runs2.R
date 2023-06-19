equal <- results_all %>%
  filter(run == "official_m10p100f0t1000p1m10") %>%
  select(topic, num_shown, recall_at_k, recall)

hh <- results_all %>%
  filter(run == "abs-hh-ratio") %>%
  select(topic, num_shown, recall_at_k, recall)

rels_per_topic %>%
  #inner_join(bm25, by = "topic") %>%
  inner_join(equal, by = "topic") %>%
  #inner_join(prop, by = "topic") %>%
  inner_join(hh, by = "topic") %>%
  #mutate(difference = num_shown.x - num_shown.y) %>%
  xtable()

super_table <- rels_per_topic %>%
  #inner_join(bm25, by = "topic") %>%
  inner_join(equal, by = "topic") %>%
  #inner_join(prop, by = "topic") %>%
  inner_join(hh, by = "topic") %>%
  mutate(difference = num_shown.x - num_shown.y)


super_table %>%
  summarize(sum(num_docs), sum(num_rels),
    sum(num_shown.x), sum(num_shown.y), #sum(difference), 
            mean(recall_at_k.x), mean(recall_at_k.y),
            mean(recall.x), mean(recall.y))

sum(super_table$recall_at_k.x > super_table$recall_at_k.y) 
sum(super_table$recall.x > super_table$recall.y) 
sum(super_table$num_shown.x > super_table$num_shown.y) 

t.test(super_table$recall_at_k.x, super_table$recall_at_k.y, paired = TRUE)

t.test(super_table$recall.x, super_table$recall.y, paired = TRUE)

wilcox.test(super_table$recall.x, super_table$recall.y, paired = TRUE, exact = FALSE)


plot(super_table$recall.x - super_table$recall.y, rep(0, length(super_table$recall.x)))


data_without <- super_table$recall.x - super_table$recall.y

data_without <- data_without[-which(data_without == -1)]

plot(data_without, rep(0, length(data_without)) )

wilcox.test(data_without, exact = F, alternative = "less")


data_with <- super_table$recall.x - super_table$recall.y

wilcox.test(data_with, exact = F, alternative = "greater")
