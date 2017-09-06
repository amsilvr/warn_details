harvey_events <- msg2 %>%
    filter(rec_time >= mdy("08-23-2017"), ) %>%
    filter(rec_time <= mdy("9/1/17")) %>%
    filter( grepl("TX|LA", areas) == TRUE)

harvey_events %>% group_by(type) %>% summarize(n())

select(harvey_events, rec_time, wea) %>% arrange(rec_time) %>% View()

