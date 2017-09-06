# Get all new WEA messages and move to
# "CMAS_Alerts_Processed" sheet
# Suggested monthly run

library(googlesheets)
require(tidyverse)
require(lubridate)

##

NewCMASImport <- function() { #copies new messages into main sheet
        raw <- gs_key("1GnchiRm2TXgQ1TpTGcsCIGggIjEIsd6TeuVyY_s4a3U") #CMAS Alerts
        full <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI") #CMAS_Alerts_Processed

        msg_last <- gs_read(full) %>%
            select(Rec_Time) %>%
            tail(1) %>%
            as.character() %>%
            mdy_hm()

        msg_new <- gs_read(raw) %>%
            mutate(Rec_Time = mdy_hm(Rec_Time)) %>%
            filter(Rec_Time > msg_last) %>%
            mutate(Rec_Time =
                paste0(
                month(Rec_Time,label = TRUE,abbr = FALSE), " ",
                day(Rec_Time), ", ",
                year(Rec_Time)," at ",
                stringr::str_pad(hour(Rec_Time),width = 2,side = "left", pad = "0"), ":",
                stringr::str_pad(minute(Rec_Time),width = 2,side = "left", pad = "0")
            ))

        gs_add_row(ss = full, ws = 1, input = msg_new, verbose = TRUE)
        }



