source("helper.R")

database_present <- selectDbQuery("SHOW DATABASES") %>% filter(!Database %in% default_databases)
tables_data <- selectDbQuery(
  "SELECT TABLE_SCHEMA AS db_name, TABLE_NAME as table_name,
  COLUMN_NAME AS column_name, DATA_TYPE AS data_type
  FROM `INFORMATION_SCHEMA`.`COLUMNS`
  WHERE `TABLE_SCHEMA` IN (?) AND DATA_TYPE = 'int'",
  list(database_present$Database)
)

ui <- fluidPage(
  conditionalPanel(
    condition = "$('html').hasClass('shiny-busy')",
    tags$div(
      style = "position: fixed;top: 250px; left: 0px; width: 100%;
      padding: 5px 0px 5px 0px; text-align: center; font-weight: bold;
      font-size: 300%; color: #ffffff; background-color:'transparent'; z-index: 105;",
      tags$img(src = "loading_icon.svg", height = "200px", width = "200px")
    )
  ),
  fluidRow(
    style = "margin-top: 10px",
    column(
      3, style = "margin-left: 10px",
      selectizeInput("database_input", "Select Database", database_present$Database)
    ),
    column(
      3,
      selectizeInput("table_input", "Select Table", character(0))
    ),
    column(
      3,
      pickerInput(
        "column_input", "Select Column", character(0), multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE)
      )
    )
  ),
  fluidRow(column(6, offset = 3, DTOutput("table_output")))
)

server <- function(input, output, session) {
  observeEvent(input$database_input, {
    req(input$database_input)
    tables_data_filtered <- filter(tables_data, db_name == input$database_input)
    if (nrow(tables_data_filtered) != 0) {
      updateSelectizeInput(session, "table_input", choices = unique(tables_data_filtered$table_name))      
    } else {
      updateSelectizeInput(session, "table_input", choices = character(0))      
    }
  })
  observeEvent(input$table_input, {
    req(input$table_input)
    columns_data_raw <- filter(tables_data, db_name == input$database_input) %>%
      filter(table_name == input$table_input)
    if (nrow(columns_data_raw) != 0) {
      options_list <- unique(columns_data_raw$column_name)
      updatePickerInput(session, "column_input", choices = options_list, selected = options_list)      
    } else {
      updatePickerInput(session, "column_input", choices = character(0))      
    }
  })
  observeEvent(input$column_input, {
    req(input$column_input)
    table_data <- tibble()
    for (column_value in input$column_input) {
      current_data <- selectDbQuery(
        glue::glue(
          "SELECT MIN(`{ column_value }`) AS min,
        MAX(`{ column_value }`) AS max,
        AVG(`{ column_value }`) AS mean
        FROM { input$table_input }"
        ),
        dbName = input$database_input
      ) %>% mutate(column = column_value)
      table_data <- rbind(table_data, current_data)
    }
    output$table_output <- renderDT({
      datatable(
        table_data %>% select(c(4, 1:3)), rownames = FALSE,
        options = list(dom = 't')
      )
    })
  })
}

shinyApp(ui, server)







