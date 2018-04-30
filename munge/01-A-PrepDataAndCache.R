# Create dataframes

# item.categories$item_category_id <-
#   as.factor(item.categories$item_category_id)
#
# items$item_id <- as.factor(items$item_id)
# items$item_category_id <-
#   factor(items$item_category_id,
#          levels = levels(item.categories$item_category_id))
#
# shops$shop_id <- as.factor(shops$shop_id)
#
# df <- sales.train %>%
#   mutate(
#     date = as.Date(date, format = '%d.%m.%Y'),
#     shop_id = factor(shop_id, levels = levels(shops$shop_id)),
#     item_id = factor(item_id, levels = levels(items$item_id))
#  # year = year(date),
#  # month = month(date)
#   ) %>%
#   arrange(date)
#
# df
#
# cache('df')
# cache('item.categories')
# cache('items')
# cache('shops')