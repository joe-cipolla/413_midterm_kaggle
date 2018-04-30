for_tsne <- df_train %>%
  select(-date) %>%
  map_df(~as.numeric(.x))
for_tsne

for_tsne_sampled <- for_tsne %>% sample_n(1e6) %>% distinct()
for_tsne_sampled %>% Rtsne(verbose=T, pca_scale = T) -> tsne_out
for_tsne_sampled %<>% bind_cols(tbl_df(tsne_out$Y))

xyplot(V1~V2, groups = month, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = year, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = week, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = weekend, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = shop_id, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = weekend, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = holiday, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = shop_center_lgl, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = sec_lgl, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = itemcat_lvl1, for_tsne_sampled, auto.key = T)
xyplot(V1~V2, groups = itemcat_lvl2, for_tsne_sampled, auto.key = T)
