# Shop ID 9 is odd

df_train %>% filter(shop_id == 9) %>%
  count(date,shop_id,wt = item_cnt_day) %>%
  ggplot(aes(x=date,y=n))+
  geom_line(aes(color=shop_id))