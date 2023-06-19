avg_bm25 <- results_bm25 %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(num_shown = sum(num_shown),
            avg_recall = mean(recall)) %>%
  mutate(type = "bm25") %>%
  ungroup()

avg_equal <- results_equal %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(num_shown = sum(num_shown),
            avg_recall = mean(recall)) %>%
  mutate(type = "equal") %>%
  ungroup()


avg_prop <- results_distributed %>%
  mutate(recall = rels_found / num_rels) %>%
  group_by(run) %>%
  summarize(num_shown = sum(num_shown),
            avg_recall = mean(recall)) %>%
  mutate(type = "prop") %>%
  ungroup()


avg_bm25 %>%
  bind_rows(avg_equal) %>%
  bind_rows(avg_prop) %>%
  ggplot(aes(x = num_shown, y = avg_recall)) +
  geom_line(aes(group = type, color = type)) +
  geom_point(aes(group = type, color = type)) +
  labs(x = "documents shown (feedback)",
       y = "average recall")



# avg_p10 <- results_orig_p10 %>%
#   mutate(recall = rels_found / num_rels) %>%
#   group_by(run) %>%
#   summarize(num_shown = sum(num_shown),
#             avg_recall = mean(recall)) %>%
#   mutate(type = "p10") %>%
#   ungroup()
# 
# 
# avg_p50 <- results_orig_p50 %>%
#   mutate(recall = rels_found / num_rels) %>%
#   group_by(run) %>%
#   summarize(num_shown = sum(num_shown),
#             avg_recall = mean(recall)) %>%
#   mutate(type = "p50") %>%
#   ungroup()
# avg_p50
# 
# avg_all <- avg_bm25 %>%
#   bind_rows(avg_dist) %>%
#   bind_rows(avg_p10) %>%
#   bind_rows(avg_p50)
# 
# avg_all %>%
#   ggplot(aes(x = num_shown, y = avg_recall)) +
#   geom_line(aes(group = type, color = type)) +
#   geom_point(aes(group = type, color = type)) +
#   labs(x = "documents shown (feedback)",
#        y = "average recall")
