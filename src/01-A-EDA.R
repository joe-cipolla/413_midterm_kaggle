df_master %>%
  group_by(shop_id) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(10) %>%
  pull(shop_id) -> top_shops
shops %>% filter(shop_id %in% top_shops)
plot_topshop_ts(df_master, top_shops)

df_master %>%
  group_by(item_id) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(10) %>%
  pull(item_id) -> top_items
items %>% filter(item_id %in% top_items)
plot_topitem_ts(df_master, top_items)

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

df_master %>%
  group_by(itemcat_lvl1) %>%
  summarise(total_sales=sum(item_cnt_day)) %>%
  arrange(-total_sales) %>%
  head(10) %>%
  pull(itemcat_lvl1) -> top_itemcats
item.categories %>% filter(itemcat_lvl1 %in% top_itemcats)
plot_top_itemcats_ts(df_master, top_itemcats)
