## 1st panel and 3rd panel
diff_table <- function(source, tb.diff, tb_type, tb_color, yn_filter_table, setname1, setname2){
  # browser()
  tb_num <- unique(tb.diff$table)
  
  lab <- tb_str %>%
    filter(table == tb_num) %>%
    select(-table) %>% unlist
  
  tb_type <- tb_type
  tb_color <- paste0(tb_color, ".", tb_type)
  
  if(yn_filter_table){
    tb_color <- paste0(tb_color, ".filter")
  }
  
  digit <- tb_str %>% filter(table == tb_num) %>% select(digit) %>% unlist
  text.format <- sprintf("<b>%%.%df</b>", digit)
  hover.format <- sprintf("%%s: %%s\n%%s: %%s\nnew %%s (%%s): %%.%df\nold %%s (%%s): %%.%df\n%%s: %%.%df", 
                          digit, digit, digit)
  
  plot_ly(data = tb.diff, x = ~column, y = ~row, source = source) %>%
    add_markers(hoverinfo = "none", marker = list(opacity = 0), showlegend = FALSE) %>%
    layout(margin = list(l = 80, b = 120, t = 80),
           title = sprintf("<b>%s Difference (%s) Table %s</b>",
                           tb.diff$state[1], tb_type, tb_num),
           xaxis = list(title = sprintf("<b>%s</b>", lab["column"]),
                        tickfont = f.tick, titlefont = f.axis),
           yaxis = list(title = sprintf("<b>%s</b>", lab["row"]), 
                        # scaleanchor = "x", 
                        tickfont = f.tick, titlefont = f.axis)) -> p
  if(any(!is.na(tb.diff[,tb_color]))){
    p <- p %>% 
      add_heatmap(z = ~get(tb_color), hoverinfo = "none", colorscale = color.scale) %>%
      colorbar(title = "") %>%
      add_text(x = ~column, y = ~row, text = ~sprintf(text.format, get(tb_color)), textfont = f.text,
               data = tb.diff %>% filter(!is.na(get(tb_color))), name = "number",
               hoverinfo = "none", inherit = FALSE) %>%
      add_markers(marker = ~list(opacity = 0, size = 0, color = get(tb_color)), colors = "Red",
                  x = ~column, y = ~row, inherit = FALSE, showlegend = FALSE,
                  hoverinfo = "text",
                  text = ~sprintf(hover.format,
                                  lab["row"], row, lab["column"], column,
                                  tb_type, setname1, get(sprintf("%s.1", tb_type)),
                                  tb_type, setname2, get(sprintf("%s.2", tb_type)),
                                  tb_color, get(tb_color)))
  }
  p$elementId <- NULL
  p
}

## 2nd panel and 4th panel
diff_map <- function(source = "A", map.diff, is.county = FALSE, tb_type = "level", tb_color = "absdiff",
                     yn_filter_plot = FALSE, setname1 = "13Apr18_2015", setname2 = "17Jul15_2012", id_highlight = ""){
  # browser()
  tb_num <- unique(map.diff$table)
  
  if(nrow(map.diff) == 0) return()
  if(!is.county){
    map.df <- map %>% full_join(map.diff, by = c("id" = "state"))
    center.df <- center %>% full_join(map.diff, by = c("STUSPS" = "state")) %>%
      rename(label = STUSPS)
  }
  if(is.county){
    map.df <- map.diff %>% left_join(map.cty, by = c("county" = "id")) 
    center.df <- map.diff %>% left_join(center.cty, by = c("county" = "GEOID")) %>%
      rename(label = NAME)
  }
   map_color <- paste0(tb_color, ".", tb_type)
  if(yn_filter_plot){
    map_color <- paste0(map_color, ".filter")
  }
  
  # browser()
  lab <- tb_str %>%
    filter(table == tb_num) %>%
    select(-table) %>% unlist
  digit <- tb_str %>% filter(table == tb_num) %>% select(digit) %>% unlist
  hover.format <- sprintf("%s: %%s\nnew %%s (%%s): %%.%df\nold %%s (%%s): %%.%df\n%%s: %%.%df",
                          ifelse(is.county, "County", "State"), digit, digit, digit)
  state.name <- unique(map.diff$state)
  if(length(state.name) > 1) state.name <- "US"
  
  ggplot(map.df) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = get(map_color)),
                 color = "darkgrey") +
    scale_fill_gradient2(name = "", na.value = "white", high = "red") +
    geom_path(aes(x = long, y = lat, group = group),
                 color = "darkgoldenrod", size = rel(1),
                 data = map.df %>% filter(id == id_highlight)) +
    coord_equal() + ggthemes::theme_map() +
    theme(title = element_text(size = rel(1.75)),
          plot.title = element_text(hjust = 0.5)) -> gg
  # gg
  pgg <- ggplotly(gg, tooltip = NULL, source = source) 
  if(!is.county){
    pgg <- pgg %>%
      add_text(data = center.df, x = ~x, y = ~y, inherit = FALSE, showlegend = FALSE,
               text = ~sprintf("<b>%s</b>", label),
               hoverinfo = "none", textfont = f.text)
  }
  pgg <- pgg %>%
    add_trace(data = center.df, x = ~x, y = ~y, inherit = FALSE, showlegend = FALSE,
              type = "scatter", mode = "markers",
              marker = ~list(opacity = 0, size = 0, color = get(map_color)), colors = "Red",
              text = ~sprintf(hover.format,
                              label,
                              tb_type, setname1, get(sprintf("%s.1", tb_type)),
                              tb_type, setname2, get(sprintf("%s.2", tb_type)),
                              map_color, get(map_color)
              ),
              hoverinfo = "text") %>%
    layout(title = sprintf("<b>%s Difference (%s) Table %s </b><br>%s: %s, %s: %s",
                           state.name, tb_type, tb_num,
                           lab["row"], unique(map.diff$row),
                           lab["column"], unique(map.diff$column)),
           margin = list(t = 80)) -> pgg
  pgg$elementId <- NULL
  pgg
}