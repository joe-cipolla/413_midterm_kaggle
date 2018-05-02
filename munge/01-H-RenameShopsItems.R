df_master <- df_master %>%
  mutate(
    shop_id = paste0('S',shop_id),
    item_id = paste0('I',item_id)
  )

cache('df_master')