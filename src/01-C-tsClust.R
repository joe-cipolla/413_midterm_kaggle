# Location Level 1
df_train %>%
  filter(loc_lvl1 %in% top_loc_lvl1) %>%
  group_by(loc_lvl1, date_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(date_block_num~loc_lvl1) %>%
  fill(Khimki) %>% select(-date_block_num) %>%
  as.matrix() %>% t()-> loc_matrix

diss(loc_matrix, 'COR') %>% hclust() %>% plot()
diss(loc_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'ward.D2', order = 'hclust', addrect = 2)
zoo::plot.zoo(t(loc_matrix),main = 'Location Level 1')


# itemcat_lvl1
df_train %>%
  filter(itemcat_lvl1 %in% top_items_categories) %>%
  group_by(itemcat_lvl1, date_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(date_block_num~itemcat_lvl1) %>% select(-date_block_num) %>%
  as.matrix() %>% t()-> itemcat_matrix

diss(itemcat_matrix, 'COR') %>% hclust() %>% plot()
diss(itemcat_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'centroid', order = 'hclust')
zoo::plot.zoo(t(itemcat_matrix),main = 'Item Cat Level 1')


# shop_id
df_train %>%
  filter(shop_id %in% top_shops) %>%
  group_by(shop_id, week) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week~shop_id) %>% select(-week) %>%
  fill('54') %>% fill(c('57','58'),.direction = 'up') %>% as.matrix() %>% t()-> shop_id_matrix

diss(shop_id_matrix, 'COR') %>% hclust() %>% plot()
diss(shop_id_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'centroid', order = 'hclust')
zoo::plot.zoo(t(shop_id_matrix),main = 'Shop ID')


# is there something unique about weeks?
df_train %>%
  group_by(week, shop_id) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week~shop_id) %>%
  map_df(~tidyr::replace_na(.x, replace = 0)) %>%
  select(-week) %>%
  as.matrix() -> week_matrix

diss(week_matrix, 'COR') %>% hclust() %>% plot()
diss(week_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'centroid', order = 'hclust')
zoo::plot.zoo(t(week_matrix),main = 'Shop ID')


# Random code to test..
df_train %>%
  group_by(itemcat_lvl2, date_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(date_block_num~itemcat_lvl2) %>% select(-date_block_num) %>%
  map_df(~tidyr::replace_na(.x, replace = 0)) %>%
  as.matrix() %>% t()-> random_matrix
diss(random_matrix, 'COR') %>% hclust() %>% plot()


df_train %>%
  count(date,shop_id,wt = item_cnt_day) %>%
  ggplot(aes(x=date,y=n))+
  geom_line(aes(color=shop_id))
