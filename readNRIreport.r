library(dplyr)
library(readxl)
# tb2_12 <- read.table("clipboard", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
tb2_12 <- read_excel("NRIreports_2012.xls", trim_ws = TRUE, na = c("", "--"),
                     sheet = "Table 2", range = "A4:I718")
tb2_12 <- tb2_12 %>% mutate(table = "2", year  = 2012, version = "Final")
tb2_15 <- read_excel("NRIreports_2015.xls", trim_ws = TRUE, na = c("", "--"),
                     sheet = "Table 2 - Land Cover Use", range = "A4:I821")
tb2_15 <- tb2_15 %>% mutate(table = "2", year  = 2015, version = "Final")
tb3_12 <- read_excel("NRIreports_2012.xls", trim_ws = TRUE, na = c("", "--"),
                     sheet = "Table 15", range = "A4:G718")
tb3_12 <- tb3_12 %>% mutate(table = "3a", year  = 2012, version = "Final")
tb3_15 <- read_excel("NRIreports_2015.xls", trim_ws = TRUE, na = c("", "--"),
                     sheet = "Table 14 - Water Erosion", range = "A5:G822")
tb3_15 <- tb3_15 %>% mutate(table = "3a", year = 2015, version = "Final")

allexcels <- rbind(tb2_12, tb2_15) %>% tidyr::nest(-table, -year, -version) %>%
  rbind(rbind(tb3_12, tb3_15) %>% tidyr::nest(-table, -year, -version))

cleantbs <- allexcels %>% mutate(out = purrr::map(data, function(df) table.stretch(excel.clean(df))))
NRItb <- cleantbs %>% select(table, year, version, out) %>% tidyr::unnest()
write.csv(NRItb, "../shinyreview/NRItables_by_version_state_year.csv", row.names = FALSE, quote = FALSE)

excel.clean <- function(data){
  # browser()
  data <- data %>% filter(apply(!is.na(.[]), 1, any))
  out <- data %>% mutate(State_No = rep(1:51, each = 2*length(unique(na.omit(Year))))) %>% 
    mutate(Year = replace(Year, is.na(Year), Year[!is.na(Year)])) %>%
    group_by(State_No) %>% mutate(State = replace(State, is.na(State), State[!is.na(State)])) %>%
    ungroup %>%
    mutate(type = rep(c("level", "mar.err"), times = n()/2)) %>% 
    mutate_at(vars(-State, -Year, -type), funs(sapply(., nri.clean)))
  return(out)
}


nri.clean <- function(x){
  x <- gsub("±| |,", "", x)
  if(grepl("--", x) | !nzchar(x)) x <- NA
  return(as.numeric(x) %>% unname)
}

table.stretch <- function(data){
  # browser()
  out <- data %>%
    rename(row = Year, state = State) %>%
    tidyr::gather(column, value, -state, -row, -State_No, -type) %>%
    tidyr::spread(type, value) %>%
    mutate(se = mar.err/1.96,
           state = sapply(state, nri.state.abb) %>% unname) %>%
    select(row, column, level, se, state) 
  return(out)
}

nri.state.abb <- function(name){
  if(name %in% state.name){
    abb <- state.abb[state.name == name]
  }
  if(!name %in% state.name){
    abb <- ifelse(name == "Total", "US", "PR")
  }
  return(abb)
}
