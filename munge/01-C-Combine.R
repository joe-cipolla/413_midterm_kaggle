item.categories <-
  item.categories %>%
    separate(item_category_name, sep = ' - ', into = c('itemcat_lvl1', 'itemcat_lvl2'), remove = F) %>%
  mutate(itemcat_lvl2=replace_na(itemcat_lvl2, replace = 'NoLvl2'))

shops <- shops %>%
  mutate(shop_name=str_replace_all(shop_name, pattern = '&quot;', replace = '')) %>%
  separate(shop_name,into = c('loc_lvl1','loc_lvl2'),sep = ' ',extra = 'merge')

cache('shops')
cache('item.categories')
cache('items')

df %>%
  left_join(shops, by = 'shop_id') %>%
  left_join(items, by = 'item_id') %>%
  left_join(item.categories, by = 'item_category_id') -> df_master

df_master <- df_master %>%
  mutate(shop_id = fct_infreq(shop_id),
         item_id = fct_infreq(item_id),
         loc_lvl1 = fct_infreq(loc_lvl1),
         itemcat_lvl1 = fct_infreq(itemcat_lvl1))

cache('df_master')
