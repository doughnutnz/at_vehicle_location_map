match_icon <- function(location_bearing) {
  assert_that(is.number(location_bearing))
  assert_that(location_bearing <= 360)
  assert_that(location_bearing >= -1)
  case_when(
    location_bearing < 0      ~ c("Stopped Low("),
    location_bearing <= 22.5  ~ c("Up("), 
    location_bearing <= 67.5  ~ c("Up-Right("),
    location_bearing <= 112.5 ~ c("Right("),
    location_bearing <= 157.5 ~ c("Dn-Right("),
    location_bearing <= 202.5 ~ c("Down("),
    location_bearing <= 247.5 ~ c("Dn-Left("),
    location_bearing <= 292.5 ~ c("Left("),
    location_bearing <= 337.5 ~ c("Lf-Up("),
    location_bearing <= 360   ~ c("Up("),
    TRUE            ~ c("Stopped High(")
  ) %>%
    paste(as.character(location_bearing),")",sep="")
}

get_bus_locations(key)

thisStatus <- response %>% 
  jq(".status") %>% 
  unpack_jq()

if (thisStatus=="Error") cat("Error status from AT API call...\n")