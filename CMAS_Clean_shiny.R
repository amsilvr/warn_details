## CMAS Clean
#browser()
library(googlesheets)
library(tidyverse)
library(lubridate)
library(sf)
library(htmltab)

load_msgs <- function() {
    #browser()
    if (!dir.exists("data")) {dir.create("data")}
    file_start <- 'msgfile_'
    day_file_name <- paste0('data/',file_start, today(),'.csv')

   if (file.exists(day_file_name)) { # get today's file from local disk
        msg <- read_csv(day_file_name)
   } else if (any(grepl(pattern = file_start, x = dir('data/')))) { # get previous file from local disk
        msg <- read_csv(file = paste0('data/',
                            dir('data/', pattern = file_start)))
   } else { #go back to google sheet
       # first update the sheet
       source('_Monthly_Message_Copy.R')
        NewCMASImport()
       # get all records

       #ss_new <- gs_key("1Xw4JefUCS4HHQ0KpvKhr-DjklqzhH3_CeA-zhoAuQfI", visibility = "private") #CMAS_Alerts_Processed
       msg <-  gs_read_csv(ss = ss_new
                       , col_names = c("rec_time", "cmac", "full_text")
                       , coltypes = "Tcc", skip = 1, trim_ws = TRUE) %>%
                mutate(rec_time = mdy_hms(gsub(" at ", " ", rec_time)
                                          , tz = "America/New_York"
                                          , truncated = 3)
                ) %>%
                separate(full_text,
                         c("blank", "gateway_id" ,"msg_id"
                           ,"special_handling", "message_type"
                           , "category", "response_type", "severity"
                           , "urgency", "certainty", "expire_time"
                           , "text_language", "alert_message","dummy")
                         , sep = "CMAC_[:word:]*: "
                         , fill = "right" ## drops the warning for rows with too many records
                         , remove = TRUE
                )


        ## creates a table for fields with "update" records

        updates <- filter(msg, nchar(special_handling) < 10) %>%
                select(rec_time, cmac, gateway_id, msg_id
                       , ref_id = special_handling
                       , special_handling = message_type
                       , message_type = category
                       , category = response_type
                       , response_type = severity
                       , severity = urgency
                       , urgency = certainty
                       , certainty = expire_time
                       , text_language = alert_message
                       , alert_message = dummy
                )

        msg <- filter(msg, nchar(special_handling) >= 10) %>%
                select(-blank, -dummy)

        ## puts all the records back into a single table and
        ## uses two different separators to split out the alert
        ## text from the plain English "area" field
        ## and finally removes the tcs boilerplate

        msg <- bind_rows(msg, updates) %>%
                mutate(expire_time = ymd_hms(expire_time)) %>%
                separate(alert_message, c("message_text","t2")
                         , sep = "Targeted Areas: "
                         , remove = TRUE) %>%
                separate(t2, c("areas"), sep = "[:punct:]{4}"
                         , extra = "drop", remove = TRUE) %>%
                mutate(threat_type = gsub("\\. .*","", cmac)
                        , msg_id = as.character(str_trim(msg_id))
                        , areas = str_trim(areas)) %>%
                dplyr::filter(!(gateway_id == "http://tcs.tsis.com\n") )

       msg <- msg[-grep(" test", msg$threat_type),]
       write_csv(msg, path = day_file_name, col_names = TRUE, append = FALSE)
       }
       return(msg)

    }
# State and Territory Lookup

map_states <- function() {
    state_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"

    if (!dir.exists("data")) {dir.create("data")}
    if (!file.exists("data/state_shape_file.zip")) {
        download.file(state_url
                      , destfile = "data/state_shape_file.zip")
        }
    t <- unzip("data/state_shape_file.zip", exdir = "data")
    # Read the file with sf
    state_sf <- st_read(t[grep("shp$",t)], stringsAsFactors = FALSE) %>%
        as_tibble() %>%
        select(STATEFP, STUSPS, NAME, geometry)

    file.remove(t)

    return(state_sf)
}


map_counties <- function() {
  # Download Shapefiles
    countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
    if (!dir.exists("data")) {dir.create("data")}
    if (!file.exists("data/county_shape_file.zip")) {
        download.file(countyshapes_url
                      , destfile = "data/county_shape_file.zip")
    }

    c_shp <- unzip(zipfile = 'data/county_shape_file.zip',
                   exdir = 'data')

    counties_sf <- read_sf(c_shp[grep("shp$", c_shp)]) %>%
        as.data.frame() %>% #to fix July 25 problem with the join.sf methods
        inner_join(lsad_lookup()) %>%
        select(STATEFP,
               COUNTYFP,
               GEOID,
               NAME,
               description,
               geometry) %>%
        left_join(state_sf %>% select(STATEFP, STUSPS)) %>%
        st_sf(sf_column_name = 'geometry') %>%
        st_transform('+proj=longlat +datum=WGS84')

    file.remove(c_shp)

    return(counties_sf)
    }


## Download local copy of FIPS lookup data and read into memory
load_fips <- function() {
    if(!exists('data/fips-lookup.csv')){
      counties_sf <- map_counties() %>%
        as.data.frame() %>%
        transmute(areaname = paste(STUSPS, NAME),
               GEOID = as.character(GEOID)) %>%
          as.tibble() %>%
          write_csv(path = "data/fips-lookup.csv")
     }
       read_csv('data/fips-lookup.csv')

  }
lsad_lookup <- function() {
# This looks up the location classification names
# from lsad.html
# and makes them readable for the data labels in the
# choropleth

  url <- "https://www.census.gov/geo/reference/lsad.html"
  lsad <- htmltab::htmltab(doc = url, which = "//th[text() = 'LSAD']/ancestor::table") %>%
    filter(grepl("06|04|12|05|03|00|15|25|13", LSAD) == TRUE) %>%
    transmute(LSAD,description = `LSAD Description`) %>%
    mutate(description =
                str_extract(pattern = "^[^(]*",string = description) %>%
                str_trim() %>%
                str_to_title()) %>%
    replace_na(list(LSAD = "", description = "")) %>%
    filter(!description == "Balance Of County Ec Place")

}

area_find <- function(area_list) {
        area_list <- str_replace_all(area_list
                     , pattern = "(([A-z]*) \\(([A-Z]{2})\\)), \\1"
                     , replacement = "\\2 city \\(\\3\\), \\2 \\(\\3\\)"
                     )

        m <- str_match_all(string = area_list
                             , pattern = "[A-z. ]{3,} ")

        n <- str_match_all(string = area_list
                           , pattern = "\\(?([A-Z]{2})\\)?")

        area_clean <- paste(n[[1]][,2]
                            , str_trim(m[[1]][,1], side = "both")) %>%

  # ## Clean TCS county names to match list from census county map
  # ##
        str_replace_all(pattern = "E\\.",replacement = "East") %>%
        str_replace_all(pattern = "W\\.",replacement = "West") %>%
        str_replace_all(pattern = "(IN La|IL La) (Porte|Salle)",replacement = "\\1\\2") %>%
        str_replace_all(pattern = "FL Dade", "FL Miami-Dade") %>%
        str_replace_all(pattern = "PR lsabela", "PR Isabela") %>%
        str_replace_all(pattern = "TX wall", "TX Wall") %>%
        str_replace_all(pattern = "TX hell", "TX Hall") %>%
        str_replace_all(pattern = "MT Lewis Clark", "MT Lewis and Clark") %>%
        str_replace_all(pattern = "SD Shannon", "SD Oglala Lakota") # Name Change effective 5/1/2015
       return(area_clean)

}

## Substitutes all counties in a state
## For areas that include only state names

full_state <- function(areas_states) {
  if (!exists("fips_lookup")) fips_lookup <- load_fips() %>%
  #fips_lookup %>%
        mutate(STUSPS = str_match(areaname, "[A-Z]{2}")) %>%
        distinct() %>%
        right_join(areas_states) %>%
        transmute(msg_id = as.character(msg_id)
                  ,GEOID = as.character(GEOID)) %>%
        return()
}

flatten_fips <- function(msg) {
  if (!exists("fips_lookup")) fips_lookup <- load_fips()
  areas <- transmute(msg
                     , msg_id = as.character(msg_id)
                     , areas)
  #separate out alerts with full state areas, convert directly to fips
  areas_states <- filter(areas, str_length(areas) == 2) %>%
      transmute(msg_id, STUSPS = areas) %>%
      full_state()
  #remove those alerts from the other areas
  areas <- filter(areas, str_length(areas) > 2)

  # create a matrix of areas for each message id that has individual counties
  areas <- tapply(areas$areas, area_find
                  , INDEX = areas$msg_id
                  , simplify = TRUE) %>%
      as.data.frame.array() %>%
      unlist(recursive = TRUE) %>%
      as_tibble(validate = TRUE) %>%
      rownames_to_column() %>%
      transmute(msg_id = as.character(str_extract(rowname, "[[:alnum:]]{8}"))
                                     , areaname = value) %>%
  # Join messages with FIPS codes by matching areanames

    left_join(fips_lookup) %>%
    transmute(msg_id, areaname, GEOID = as.character(GEOID))

  # Fix the 18 that don't seem to match for whatever reason
  areas <-  mutate(areas, GEOID =
             case_when(
                      grepl("MD Baltimore city"
                            ,areas$areaname, ignore.case = TRUE) ~ "24005",
                      grepl("SD Shannon",areas$areaname) ~ "46113",
                      grepl("TX Wall",areas$areaname) ~ "48473",
                      grepl("NV Carson",areas$areaname) ~ "32510",
                      grepl("PR Rio Grande",areas$areaname) ~ "72119",
                      grepl("PR Manati",areas$areaname) ~ "72091",
                      grepl("PR Juana Diaz",areas$areaname) ~ "72075",
                      grepl("PR Loiza",areas$areaname) ~ "72087",
                      grepl("LA La Salle",areas$areaname) ~ "22059",
                      grepl("SD Pennington city",areas$areaname) ~ "46103",
                      grepl("PR Las Marias",areas$areaname) ~ "72083",
                      grepl("CA Lake city",areas$areaname) ~ "06033",
                      grepl("PR Comerio",areas$areaname) ~ "72045",
                      grepl("KY Carter city",areas$areaname) ~ "21043",
                      grepl("VA Roanoke city",areas$areaname) ~ "51770",
                      grepl("MN McLeod city",areas$areaname) ~ "27085",
                      ## Account for accents in county names ##
                      grepl("NM Dona Ana", areas$areaname) ~ "35013", ## Account for ñ
                      grepl("PR Anasco", areas$areaname) ~ "72011", ## Account for ñ
                      grepl("PR Catano", areas$areaname) ~ "72033", ## Account for ñ
                      grepl("PR Penuelas", areas$areaname) ~ "72111", ## Account for ñ
                      grepl("PR Bayamon", areas$areaname) ~ "72021", ## Account for ó
                      grepl("PR Canovanas", areas$areaname) ~ "72029", ## Account for ó
                      grepl("PR Guanica", areas$areaname) ~ "72055", ## Account for á
                      grepl("PR Mayaguez", areas$areaname) ~ "72097", ## Account for ü
                      grepl("PR Rincon", areas$areaname) ~ "72117", ## Account for ó
                      grepl("PR San German", areas$areaname) ~ "72125", ## Account for á
                      grepl("PR San Sebastian", areas$areaname) ~ "72131", ## Account for á

                      TRUE ~ areas$GEOID
                )
      )    %>%
    select(-areaname) %>%
    rbind(areas_states) %>%
    return()
}

# Classify message type -
# Tornado, Flash Flood,
# AMBER, Hurricane, or Other

classify_message <- function(msg) {

  mutate(msg, type =
           case_when(
             grepl("Tornado", msg$message_text, ignore.case = TRUE) ~ "Tornado",
             grepl("Flash Flood", msg$message_text, ignore.case = TRUE) ~ "FlashFlood",
             grepl("Amber", msg$message_text, ignore.case = TRUE) ~ "AMBER",
             grepl("Kidnap", msg$message_text, ignore.case = TRUE) ~ "AMBER",
             grepl("child", msg$message_text, ignore.case = TRUE) ~ "AMBER",
             grepl("Hurricane", msg$message_text, ignore.case = TRUE) ~ "Hurricane",
             #grepl("Tsunami", msg$message_text, ignore.case = TRUE) ~ "Tsunami",
             TRUE ~ "Other")
  ) %>%
    transmute(msg_id = as.character(msg_id), rec_time
              , expire_time
              , response = response_type
              , urgency
              , wea = message_text
              , type = as.factor(type)
              , areas
    )
}

tally_alerts <- function(df = msg2
                         , fips_msg = fips_msg
                         , start = NULL
                         , end = NULL) {
    if(is.null(start)) start = min(df$rec_time)
    if(is.null(end)) end = max(df$rec_time)
    df %>%
        filter(rec_time >= start) %>%
        filter(rec_time <= end) %>%
        left_join(fips_msg) %>%
        select(msg_id, GEOID, type) %>%
        group_by(GEOID, type) %>%
        tally() %>%
        rename(WEATYPE = type, WEANUM = n) %>%
        spread(WEATYPE, WEANUM, fill = "0",drop = TRUE, convert = TRUE) %>%
        mutate(Total = AMBER + FlashFlood
               + Hurricane + Other + Tornado)}

######################
## Run Functions  ####
# If msg isn't in memory, check to see if we have already
# downloaded the data and cleaned it today. If so, get
# it from the csv created on today's date.
# If we haven't created the msg df today, then load
# from the google sheet.
# This is commented out for shinyapps.io versions

load_vars <- function() { ## Loads variables into global environment
    #browser()
    msg <- load_msgs()

    state_sf <<- map_states()
    counties_sf <<- map_counties()


    if(any(grepl(pattern = "msg-class", x = dir('data')))) {
         msg2 <<-  read_csv(
             file = paste0("data/",
                           dir("data/",
                               pattern = "msg-class")[1]))
     } else {msg2 <<- classify_message(msg) %>%
        arrange(desc(rec_time)) %>%
        distinct() %>%
        write_csv(
            path = paste0('data/msg-class_',
                          today(),
                          '.csv'),
            append = FALSE,col_names = TRUE)}

    if(any(grepl(pattern = "fips-msg", x = dir('data')))) {
        fips_msg <<- read_csv(
            file = paste0("data/",
                          dir("data/",
                              pattern = "fips-msg")[1]))
    } else {
        fips_msg <<- flatten_fips(msg2) %>%
        write_csv(path = paste0('data/fips-msg_',today(),'.csv'),append = FALSE,col_names = TRUE)
    }


    if((any(grepl(pattern = "alert-tally", x = dir('data'))))) {
      alert_tally <<- read_csv(
          file = paste0("data/",
                        dir("data/",
                            pattern = "alert-tally")[1]))
    } else {
        alert_tally <<- tally_alerts(msg2, fips_msg) %>%
        write_csv(path = paste0("data/alert-tally_",today(),".csv"),
                  append = FALSE,
                  col_names = TRUE
                )
    }
}


