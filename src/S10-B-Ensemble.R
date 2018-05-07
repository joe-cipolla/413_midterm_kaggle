c3 <- readr::read_csv('logs/c3_weekly_testout.csv')
c4 <- readr::read_csv('logs/c4_testout.csv')
c5 <- readr::read_csv('logs/c5_weekly_testout.csv')
c3
c4
c5

bind_cols(c3,c4,c5) %>%
  transmute(id, item_cnt_month = (item_cnt_month+item_cnt_month1+item_cnt_month2)/3) %>%
  write_csv('logs/ensemble_1.csv', col_names = T)
# score 1.24571