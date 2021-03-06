library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

## diff table
rm(list = ls(all = T))
tb_str <- read.csv("data/clean/table_structures.csv", stringsAsFactors = FALSE)
nri_bu <- read.csv("data/clean/NRI_BU_CODE.csv", stringsAsFactors = FALSE)
## diff map
map <- readr::read_rds("data/clean/us_nri_mapdf.rds")
center <- read.csv("data/clean/cb_2017_us_state_20m_albers_centroid.csv", stringsAsFactors = FALSE)
# map.cty <- readr::read_rds("us_cty_mapdf.rds")
# center.cty <- readr::read_rds("us_cty_center.rds")
## method II
NRItb <- read.csv("data/clean/NRItables_by_version_state_year.csv", stringsAsFactors = F)
NRItb <- NRItb %>% mutate(setname = paste(version, year, sep = "_")) %>% select(-year, -version)
## color scale
breaks <- seq(0, 1, 0.1)
color.scale <- data.frame(breaks, color = leaflet::colorNumeric("Reds", c(0, 1))(breaks))
## plotly text font
f.text <- list(
  family = "Times New Roman",
  size = 14,
  color = "black")
f.tick <- list(
  size = 12
  # family = "Times New Roman"
)
f.axis <- list(
  size = 16
  # family = "Times New Roman"
)

## NRI to Diff table
NRI2Diff <- function(setname1, setname2, tbnum = "2a", data = NRItb){
  # browser()
  tb1 <- data %>% filter(setname == setname1, table == tbnum)
  tb2 <- data %>% filter(setname == setname2, table == tbnum)
  tb.diff <- inner_join(tb1 %>% select(-setname),
                        tb2 %>% select(-setname),
                        by = names(tb1)[!(names(tb1) %in% c("level", "se", "setname"))],
                        suffix = c(".1", ".2")) %>%
    mutate(
      absdiff.level = abs(level.1 - level.2),
      relabsdiff.level = replace(absdiff.level/level.1, absdiff.level == 0, 0),
      absdiff.se = abs(se.1 - se.2),
      relabsdiff.se = replace(absdiff.se/se.1, absdiff.se == 0, 0)
    ) %>%
    mutate_at(vars(matches("rel.")), funs(replace(., is.infinite(.), NA)))
  
  ## filter
  tb.diff <- tb.diff %>%
    mutate(absdiff.level.filter = absdiff.level, relabsdiff.level.filter = relabsdiff.level,
           absdiff.se.filter = absdiff.se, relabsdiff.se.filter = relabsdiff.se
    ) 
  filter <- tb_str %>% filter(table == tbnum) %>% select(cond_level_rel:cond_se) %>% unlist %>%
    sapply(function(x) strsplit(x, split = " ")) %>% unlist
  # browser()
  tb.diff <- tb.diff %>%
    mutate(out.level = !(get(filter[1])(relabsdiff.level, filter[2] %>% as.numeric) &
                            get(filter[3])(level.1,  filter[4] %>% as.numeric)),
           out.se = !(get(filter[5])(relabsdiff.se, filter[6] %>% as.numeric) &
                        get(filter[7])(se.1,  filter[8] %>% as.numeric))) %>%
    mutate_at(vars(absdiff.level.filter, relabsdiff.level.filter),
              funs(replace(., out.level, NA))) %>%
    mutate_at(vars(absdiff.se.filter, relabsdiff.se.filter),
              funs(replace(., out.se, NA))) %>%
    select(-contains("out"))
  
  if(length(unique(tb.diff$row))>1){
    if(grepl("2", tbnum)|tbnum == "4"){
      tb.diff <- tb.diff %>%
        mutate(row = factor(row, levels = rev(unique(row)))) %>%
        mutate_at(vars(column), 
                  funs(factor(., levels = nri_bu$Category, labels = nri_bu$Category)))
    }
    if(grepl("1", tbnum)){
      tb.diff <- tb.diff %>%
        # filter(table == "2a") %>%
        mutate_at(vars(row, column), 
                  funs(factor(., levels = nri_bu$Category, labels = nri_bu$Category))) %>%
        mutate(row = forcats::fct_relevel(row, rev(levels(row))))
    }
    if(grepl("3", tbnum)){
      # browser()
      tb.diff <- tb.diff %>%
        mutate(row = factor(row, levels = rev(unique(row)))) %>%
        mutate_at(vars(column), 
                  funs(factor(., levels = unique(column)[c(2,3,5,1,4)], labels = unique(column)[c(2,3,5,1,4)])))
    }
    if(grepl("4", tbnum) & tbnum != "4"){
      nri_bu2 <- nri_bu %>% 
        mutate(Category = gsub("Cult.", "Irr.", Category),
               Category = gsub("Noncult.", "Nonirr.", Category))
      tb.diff <- tb.diff %>%
        mutate_at(vars(row, column), 
                  funs(factor(., levels = nri_bu2$Category, labels = nri_bu2$Category))) %>%
        mutate(row = forcats::fct_relevel(row, rev(levels(row))))
    }
  }
  
  tb.diff %>% select(contains("level"), contains("se"), everything())
}
