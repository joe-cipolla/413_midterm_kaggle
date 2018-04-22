# set.key('AIzaSyAS9BOBnYUBn0cMB1L8N__F9lwDCwQGaiA')

# item_cat_en <- map(.x = levels(item.categories$item_category_name),.f = ~translate(.x,source = 'ru',target = 'en')) %>% unlist()

# Run on AWS
# item_name_en <- map(.x = levels(items$item_name),.f = ~translate(.x,source = 'ru',target = 'en')) %>% unlist()

# shop_name_en <- map(.x = levels(shops$shop_name),.f = ~translate(.x,source = 'ru',target = 'en')) %>% unlist()

# cache('item_cat_en')
# cache('shop_name_en')
#
# shops$shop_name <- shop_name_en
# cache('shops')
#
# item.categories$item_category_name <- item_cat_en
# cache('item.categories')
#
# items$item_name <- item_name_en
# cache('items')
