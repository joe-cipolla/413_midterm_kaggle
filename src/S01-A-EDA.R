df_train %>%
  group_by(shop_id) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(15) %>%
  pull(shop_id) -> top_shops
# shops %>% filter(shop_id %in% top_shops)
plot_topshop_ts(df_train, top_shops)

df_train %>%
  group_by(itemcat_lvl1) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(10) %>%
  pull(itemcat_lvl1) -> top_items_categories
# items %>% filter(item_id %in% top_items)
plot_topitemcategories_ts(df_train, top_items_categories)

df_train %>%
  group_by(loc_lvl1) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(10) %>%
  pull(loc_lvl1) -> top_loc_lvl1
# items %>% filter(item_id %in% top_items)
plot_top_loc_lvl1_ts(df_train, top_loc_lvl1)


random_items <- sample(df_master$item_id, 20)
df_master %>%
  filter(item_id %in% random_items) %>%
  group_by(item_id, date_block_num) %>%
  tally(wt = item_cnt_day) %>%
  ggplot(aes(x=date_block_num, y=n))+
  geom_line(aes(color=item_id))+
  geom_point(aes(color=item_id))+
  theme_bw()+
  labs(title = '50 random items')

df_train %>%
  group_by(item_id) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(15) %>%
  pull(item_id) -> top_items

