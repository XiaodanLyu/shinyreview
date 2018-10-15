source("help.r")
source("template.r")

# Define UI for application 
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NRI Data Review Visualization", titleWidth = 350),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    fluidRow(
      box(width = 12, status = "success", solidHeader = TRUE,
          title = "Info", collapsible = TRUE, collapsed = TRUE,
          p("You can collapse this info Box by clicking the '-' on the topright corner."),
          p("Please use the <Options> box to customize the difference tables to be displayed."),
          tags$ul(
            tags$li("The version is named by 'DateOfRun_YearOfData'. V1 (new values) is compared with V2 (old values)."), 
            tags$li("absolute relative difference = |new - old|/new, absolute difference = |new - old|"), 
            tags$li("The text in the bottom of the box describes the selected table.")
          ),
          p("Please use the <Difference Table> and <Difference Map> panels to interact with the plots.
            For all plots, you can:"),
          tags$ul(
            tags$li("get hover information by moving your cursor."), 
            tags$li("zoom in by drawing a rectangle, and reset by double clicking the plot.")
          ),
          p("Given the customized option,"),
          tags$ul(
            tags$li("1st panel (Difference Table - US) visualizes the difference table at national level;"),
            tags$li("2nd panel (Difference Map - US) visualizes the differences across state for the clicked cell (row, column) in the 1st panel;"),
            tags$li("3rd panel (Difference Table - State) visualizes the difference table at the clicked state polygon in the 2nd panel.")),
          p("You can hide the numbers on the heatmap by clicking the legend called 'number'.")
      )
    ),
    fluidRow(
      box(width = 12, status = "primary", title = "Options", solidHeader = TRUE,
          fluidRow(
            column(
              width = 3,
              selectInput("setname1", "Please select V1 with new values:",
                          choices = unique(NRItb$setname), selected = "Final_2015")),
            column(width = 3, uiOutput("secondset"))
          ),
          fluidRow(
            column(width = 3, uiOutput("tbnum")),
            column(
              width = 3,
              radioButtons("tb_type", "Table cell", inline = TRUE, choices = c("level", "se"))),
            column(
              width = 6,
              radioButtons("tb_color", "Color scale", inline = TRUE,
                           choices = c("absolute relative difference" = "relabsdiff",
                                       "absolute difference" = "absdiff")))
          ),
          tags$b(htmlOutput("tb_name"))
      ),
      fluidRow(
        column(width = 12,
               box(solidHeader = TRUE, status = "warning", title = "Difference Table - US",
                   collapsible = TRUE, collapsed = TRUE,
                   fluidRow(
                     column(
                       width = 6,
                       checkboxInput("yn_filter_table", "Apply filter", value = TRUE)),
                     column(
                       width = 6,
                       checkboxInput("yn_hover_table", "Enable hover", value = FALSE)
                     )
                   ),
                   withSpinner(plotlyOutput("plot_diff", height = 600))
               ),
               box(solidHeader = TRUE, status = "warning", title = "Difference Map - US",
                   checkboxInput("yn_filter_plot", "Apply filter", value = FALSE),
                   uiOutput("maporhelp_2"))
        )),
      fluidRow(
        column(width = 12,
               box(solidHeader = TRUE, status = "warning", title = "Difference Table - State",
                   fluidRow(
                     column(
                       width = 6,
                       checkboxInput("yn_filter_table_st", "Apply filter", value = TRUE)),
                     column(
                       width = 6,
                       checkboxInput("yn_hover_table_st", "Enable hover", value = FALSE)
                     )
                   ),
                   uiOutput("maporhelp_3")
               )
               # box(solidHeader = TRUE, status = "warning", title = "Difference Map - State",
               #     checkboxInput("yn_filter_plot_st", "Apply filter", value = FALSE),
               #     uiOutput("maporhelp_4"))
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  ## V2 choices ####
  ## V2 choices exclude selected V1
  output$secondset <- renderUI({
    selectInput("setname2", "Please select V2 with old values:",
                choices = unique(NRItb$setname)[!unique(NRItb$setname) %in% input$setname1])
  })
  
  ## table number choices ####
  ## less table choices for 2012
  output$tbnum <- renderUI({
    
    out12 <- tb_str %>% filter(grepl("2015", column)) %>% select(table) %>% unlist
    choices_tb <- unique(NRItb$table) %>% sort
    if(any(grepl("2012", c(input$setname1, input$setname2)))){
      choices_tb <- unique(NRItb$table)[!unique(NRItb$table) %in% out12] %>% sort
    }
    selectInput("tb_num", "Table number", choices = choices_tb)
    
  })
  
  ## table info ####
  output$tb_name <- renderUI({
    
    tb_info <- tb_str %>% filter(table == input$tb_num)
    cond <- tb_info %>% select(contains(input$tb_type)) %>% unlist
    filter_txt <- sprintf("diff %s of new val && new val %s", cond[1], cond[2])
    HTML(sprintf("Table %s (%s): %s <br/> Filter: %s", tb_info$table, input$tb_type, tb_info$name, filter_txt))
    
  })
  
  ## table label ####
  lab <- reactive({
    tb_str %>%
      filter(table == input$tb_num) %>%
      select(-table) %>% unlist
  })
  
  ## Panel 1 ####
  output$plot_diff <- renderPlotly({
    
    setname1 <- input$setname1
    setname2 <- input$setname2
    data_input <- NRItb %>% filter(state == "US", table == input$tb_num)
    tb.diff <- NRI2Diff(setname1, setname2, tbnum = input$tb_num, data = data_input)
    diff_table(source = "us_table", tb.diff, input$tb_type, input$tb_color, input$yn_filter_table, setname1, setname2)
    # browser()
  })
  
  ## Panel 3 plot ####
  output$plot_diff_st <- renderPlotly({
    
    setname1 <- input$setname1
    setname2 <- input$setname2
    tb.diff <- panel3.data()
    diff_table(source = "st_table", tb.diff, input$tb_type, input$tb_color, input$yn_filter_table_st, setname1, setname2)
    # browser()
  })
  
  ## input of Panel 2 ####
  panel2.data <- reactive({
    
    action <- ifelse(input$yn_hover_table, "plotly_hover", "plotly_click")
    event.data <- event_data(action, source = "us_table")
    if(is.null(event.data)){
      return(data.frame())
    }else{
      setname1 <- input$setname1
      setname2 <- input$setname2
      data_input <- NRItb %>% filter(row == event.data$y, column == event.data$x,
                                     table == input$tb_num, state != "US")
      tb.diff <- NRI2Diff(setname1, setname2, tbnum = input$tb_num, data = data_input)
      return(tb.diff)
    }
    
  })
  
  ## input of Panel 3 ####
  panel3.data <- reactive({
    
    event.data <- event_data("plotly_click", source = "us_map")
    if(is.null(event.data)){
      return(data.frame())
    }else{
      setname1 <- input$setname1
      setname2 <- input$setname2
      center %>% filter(abs(x - event.data$x) <= 1,
                        abs(y - event.data$y) <= 1) %>%
        select(STUSPS) %>% unlist %>% unname -> state.click
      data_input <- NRItb %>% filter(table == input$tb_num, state == state.click)
      tb.diff <- NRI2Diff(setname1, setname2, tbnum = input$tb_num, data = data_input)
      return(tb.diff)
    }
    
  })
  
  ## input of Panel 4 ####
  panel4.data <- reactive({
    
    action <- ifelse(input$yn_hover_table_st, "plotly_hover", "plotly_click")
    event.data <- event_data(action, source = "st_table")
    if(is.null(event.data)){
      return(data.frame())
    }else{
      setname1 <- input$setname1
      setname2 <- input$setname2
      state.name <- panel3.data()$state %>% unique %>% tolower
      NRIcty <- read.csv(sprintf("../data_clean/county/%s_cty.csv", state.name), stringsAsFactors = FALSE)
      NRIcty <- NRIcty %>% mutate(setname = paste(version, year, sep = "_")) %>% select(-year, -version)
      data_input <- NRIcty %>% filter(row == event.data$y, column == event.data$x, table == input$tb_num)
      tb.diff <- NRI2Diff(setname1, setname2, tbnum = input$tb_num, data = data_input)
      tb.diff <- tb.diff %>% mutate(county = as.character(county) %>% stringr::str_pad(width = 5, pad = "0"))
      return(tb.diff)
    }
    
  })
  
  ## Panel 2 plot ####
  output$map_diff <- renderPlotly({
    diff_map("us_map", panel2.data(), is.county = FALSE,
             input$tb_type, input$tb_color, input$yn_filter_plot,
             input$setname1, input$setname2)
    # browser()
  })
  
  ## Panel 4 plot ####
  output$map_diff_st <- renderPlotly({
    diff_map("st_map", panel4.data(), is.county = TRUE,
             input$tb_type, input$tb_color, input$yn_filter_plot_st,
             input$setname1, input$setname2)
  })
  
  ## Panel 2 UI ####
  output$maporhelp_2 <- renderUI({
    
    panel2.data <- panel2.data()
    if(nrow(panel2.data) == 0){
      helpText("Please click a cell in the 1st panel (Difference Table - US) to see the corresponding difference map.")
    }
    else{
      withSpinner(plotlyOutput("map_diff", height = 600))
    }
    
  })
  
  ## Panel 3 UI ####
  output$maporhelp_3 <- renderUI({
    
    panel3.data <- panel3.data()
    if(nrow(panel3.data) == 0){
      helpText("Please click a state polygon in the 2nd panel (Difference Table - State) to see the corresponding difference table.")
    }
    else{
      withSpinner(plotlyOutput("plot_diff_st", height = 600))
    }
    
  })
  
  ## Pnael 4 UI ####
  output$maporhelp_4 <- renderUI({
    
    panel4.data <- panel4.data()
    if(nrow(panel4.data) == 0){
      helpText("Please click a cell on the left heatmap to see the corresponding difference map.",
               br(),
               "Currently this panel is only available for comparison among versions
               17Jul15_2012, 13Apr18_2015, 23Aug18_2015, 24Aug18_2015 and 28Aug18_2015.")
    }
    else{
      withSpinner(plotlyOutput("map_diff_st", height = 600))
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

