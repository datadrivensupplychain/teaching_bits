datevec <- c('2021-1-3','12-1-2022')
mydate <- datevec %>% as.data.frame() %>% 
  dplyr::mutate(date = dplyr::case_when(
  is.na(lubridate::ymd(.))==FALSE ~ lubridate::ymd(.),
  is.na(lubridate::mdy(.))==FALSE ~ lubridate::mdy(.),
  TRUE ~ NULL))
#doesn't work yet!
