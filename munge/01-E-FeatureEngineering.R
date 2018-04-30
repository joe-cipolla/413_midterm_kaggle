# Location Level 2 based feature variables
df_master <- df_master %>%
  mutate(shop_center_lgl = str_detect(loc_lvl2, '[Ss]hopping [Cc]enter'),
         sec_lgl = str_detect(loc_lvl2, 'SEC'),
         tc_lgl = str_detect(loc_lvl2, 'TC'),
         trc_lgl = str_detect(loc_lvl2, 'TRC'),
         trk_lgl = str_detect(loc_lvl2, 'TRK')
  )

# Date based features
df_master <- df_master %>%
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    weekend = isWeekend(date)
  )

## https://www.timeanddate.com/holidays/russia/2015
russian_holidays <- readr::read_csv('data/russian_holidays.csv',
                                    col_names = c('date','holiday_name','holiday_type'),
                                    skip = 1) %>% mutate(date = mdy(date))
df_master <- df_master %>%
  mutate(
    holiday = date %in% russian_holidays$date
  )
