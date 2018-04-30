# Item name and item category name not needed since the IDs exist for these cols
df_master <- df_master %>%
  select(-item_name, -item_category_name)

cache('df_master')
