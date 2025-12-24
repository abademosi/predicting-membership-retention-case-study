
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(scales)

# UI
ui <- fluidPage(
  titlePanel("Business Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("industry_ui"),
      uiOutput("naics2_ui"),
      uiOutput("county_ui"),
      uiOutput("size_ui"),
      
      # Accreditation filter from labeled column
      uiOutput("accred_ui"),
      
      uiOutput("rating_ui"),
      
      dateRangeInput(
        "joined_range", "Joined date range",
        start = NA, end = NA, startview = "year"
      ),
      
      tags$hr(),
      downloadButton("download_filtered", "Download filtered CSV")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs", type = "tabs",
        tabPanel(
          "Overview",
          br(),
          fluidRow(
            column(4, wellPanel(
              h4("Accredited"),
              textOutput("kpi_accredited")
            )),
            column(4, wellPanel(
              h4("Non-Accredited"),
              textOutput("kpi_nonaccredited")
            ))
          ),
          br(),
          h4("Top Industries (by business count)"),
          plotOutput("plot_industries", height = 320),
          br(),
          h4("Top Counties (by business count)"),
          plotOutput("plot_counties", height = 320),
          br(),
          h4("Joins Over Time"),
          plotOutput("plot_joins", height = 320)
        ),
        tabPanel(
          "Industry",
          br(),
          h4("Businesses by Industry"),
          plotOutput("plot_industry_full", height = 500)
        ),
        tabPanel(
          "Geography",
          br(),
          h4("Businesses by County"),
          plotOutput("plot_county_full", height = 500)
        ),
        tabPanel(
          "Companies",
          br(),
          DTOutput("table_companies")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data: prefer uploaded file, else df2 in global env
  raw_data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if (exists("df2", envir = .GlobalEnv)) {
      get("df2", envir = .GlobalEnv)
    } else {
      validate(need(FALSE, "Please upload a CSV or ensure df2 exists in the global environment."))
    }
  })
  
  # Prepare data: coerce types and build labeled Accreditation column
  prepared <- reactive({
    df <- raw_data()
    
    # Expected columns are optional, we warn if missing
    needed <- c(
      "Business.ID","Business","Accreditation","Accredited.","Size","Employees","Customers",
      "Revenue","Revenue.Range","City","State","Zip","County","Rating","Joined",
      "NAICS.Code","NAICS.Text","NAICS_2digit","NAICS Industry","AB."
    )
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) warning(paste("Missing columns:", paste(missing, collapse = ", ")))
    
    # Filter out anything before 2000
    if ("Joined" %in% names(df)) df <- df %>% dplyr::filter(Joined >= as.Date("2000-01-01") | is.na(Joined))
    
    if ("Employees" %in% names(df)) df$Employees <- suppressWarnings(as.numeric(df$Employees))
    if ("Revenue" %in% names(df)) df$Revenue <- suppressWarnings(as.numeric(df$Revenue))
    if ("Zip" %in% names(df)) df$Zip <- as.character(df$Zip)
    
    if ("Size" %in% names(df)) df$Size <- as.factor(df$Size)
    if ("Rating" %in% names(df)) df$Rating <- as.factor(df$Rating)
    if ("County" %in% names(df)) df$County <- as.factor(df$County)
    if ("NAICS_2digit" %in% names(df)) df$NAICS_2digit <- as.factor(df$NAICS_2digit)
    if ("NAICS Industry" %in% names(df)) df$`NAICS Industry` <- as.factor(df$`NAICS Industry`)
    
    # Accreditation source of truth:
    # Prefer df$Accredited if present. It should be logical TRUE/FALSE.
    # If not logical, coerce "True"/"False" safely to logical.
    if ("Accredited." %in% names(df)) {
      acc_chr <- trimws(tolower(as.character(df$Accredited.)))
      acc <- ifelse(is.na(acc_chr), NA, acc_chr == "true")
    } else {
      acc <- NA
    }
    
    # Labeled accreditation column used by the UI filter
    df$AccreditationStatus <- ifelse(
      is.na(acc), NA_character_,
      ifelse(acc, "Accredited", "Non-Accredited")
    )
    df$AccreditationStatus <- factor(df$AccreditationStatus,
                                     levels = c("Accredited","Non-Accredited"))
    
    df
  })
  
  observeEvent(prepared(), {
    df <- prepared()
    if (!"Joined" %in% names(df)) return(NULL)
    j <- df$Joined
    j <- j[!is.na(j)]
    if (length(j) == 0) return(NULL)
    
    updateDateRangeInput(
      session, "joined_range",
      start = min(j, na.rm = TRUE),
      end   = max(j, na.rm = TRUE)
    )
  })
  
  # Dynamic filter options
  output$industry_ui <- renderUI({
    df <- prepared()
    if (!"NAICS Industry" %in% names(df)) return(NULL)
    choices <- sort(unique(df$`NAICS Industry`))
    selectizeInput("industry", "Industry (NAICS)", choices = choices, multiple = TRUE)
  })
  
  output$naics2_ui <- renderUI({
    df <- prepared()
    if (!"NAICS_2digit" %in% names(df)) return(NULL)
    choices <- sort(unique(df$NAICS_2digit))
    selectizeInput("naics2", "NAICS 2-digit", choices = choices, multiple = TRUE)
  })
  
  output$county_ui <- renderUI({
    df <- prepared()
    if (!"County" %in% names(df)) return(NULL)
    choices <- sort(unique(df$County))
    selectizeInput("county", "County", choices = choices, multiple = TRUE)
  })
  
  output$size_ui <- renderUI({
    df <- prepared()
    if (!"Size" %in% names(df)) return(NULL)
    choices <- sort(unique(df$Size))
    selectizeInput("size", "Size", choices = choices, multiple = TRUE)
  })
  
  # Accreditation filter from labeled status
  output$accred_ui <- renderUI({
    df <- prepared()
    if (!"AccreditationStatus" %in% names(df)) return(NULL)
    choices <- levels(df$AccreditationStatus)
    checkboxGroupInput("accred_status", "Accreditation status",
                       choices = choices, selected = choices)
  })
  
  output$rating_ui <- renderUI({
    df <- prepared()
    if (!"Rating" %in% names(df)) return(NULL)
    choices <- sort(unique(df$Rating))
    selectizeInput("rating", "Rating", choices = choices, multiple = TRUE)
  })
  
  # Filtered data
  filtered <- reactive({
    df <- prepared()
    
    if (!is.null(input$joined_range[1]) && !is.na(input$joined_range[1]) && "Joined" %in% names(df)) {
      df <- df %>% filter(Joined >= input$joined_range[1])
    }
    if (!is.null(input$joined_range[2]) && !is.na(input$joined_range[2]) && "Joined" %in% names(df)) {
      df <- df %>% filter(Joined <= input$joined_range[2])
    }
    
    if (!is.null(input$industry) && length(input$industry) && "NAICS Industry" %in% names(df)) {
      df <- df %>% filter(`NAICS Industry` %in% input$industry)
    }
    if (!is.null(input$naics2) && length(input$naics2) && "NAICS_2digit" %in% names(df)) {
      df <- df %>% filter(NAICS_2digit %in% input$naics2)
    }
    if (!is.null(input$county) && length(input$county) && "County" %in% names(df)) {
      df <- df %>% filter(County %in% input$county)
    }
    if (!is.null(input$size) && length(input$size) && "Size" %in% names(df)) {
      df <- df %>% filter(Size %in% input$size)
    }
    if (!is.null(input$rating) && length(input$rating) && "Rating" %in% names(df)) {
      df <- df %>% filter(Rating %in% input$rating)
    }
    if (!is.null(input$accred_status) && length(input$accred_status) && "AccreditationStatus" %in% names(df)) {
      df <- df %>% filter(AccreditationStatus %in% input$accred_status)
    }
    
    df
  })
  
  # KPIs
  output$kpi_accredited <- renderText({
    df <- filtered()
    if (!"AccreditationStatus" %in% names(df)) return("—")
    format(sum(df$AccreditationStatus == "Accredited", na.rm = TRUE), big.mark = ",")
  })
  
  output$kpi_nonaccredited <- renderText({
    df <- filtered()
    if (!"AccreditationStatus" %in% names(df)) return("—")
    format(sum(df$AccreditationStatus == "Non-Accredited", na.rm = TRUE), big.mark = ",")
  })
  
  # Charts
  output$plot_industries <- renderPlot({
    df <- filtered()
    validate(need("NAICS Industry" %in% names(df), "NAICS Industry column missing"))
    df %>%
      count(`NAICS Industry`, name = "n", sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = n, y = reorder(`NAICS Industry`, n))) +
      geom_col() +
      labs(x = "Businesses", y = NULL)
  })
  
  output$plot_counties <- renderPlot({
    df <- filtered()
    validate(need("County" %in% names(df), "County column missing"))
    df %>%
      count(County, name = "n", sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = n, y = reorder(County, n))) +
      geom_col() +
      labs(x = "Businesses", y = NULL)
  })
  
  output$plot_joins <- renderPlot({
    df <- filtered()
    validate(need("Joined" %in% names(df), "Joined column missing or not parsed as dates"))
    df %>%
      filter(!is.na(Joined)) %>%
      mutate(month = floor_date(Joined, "month")) %>%
      count(month, name = "n") %>%
      ggplot(aes(x = month, y = n)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%m/%d/%Y") +
      labs(x = NULL, y = "New Businesses Joined")
  })
  
  output$plot_industry_full <- renderPlot({
    df <- filtered()
    validate(need("NAICS Industry" %in% names(df), "NAICS Industry column missing"))
    df %>%
      count(`NAICS Industry`, name = "n", sort = TRUE) %>%
      ggplot(aes(x = n, y = reorder(`NAICS Industry`, n))) +
      geom_col() +
      labs(x = "Businesses", y = NULL)
  })
  
  output$plot_county_full <- renderPlot({
    df <- filtered()
    validate(need("County" %in% names(df), "County column missing"))
    df %>%
      count(County, name = "n", sort = TRUE) %>%
      ggplot(aes(x = n, y = reorder(County, n))) +
      geom_col() +
      labs(x = "Businesses", y = NULL)
  })
  
  # Table
  output$table_companies <- renderDT({
    df <- filtered()
    cols <- intersect(c("Business.ID","Business","City","County","State","Zip","Region",
                        "Size","Employees","Revenue","Rating",
                        "Accredited","Accredited.","AccreditationStatus","Joined",
                        "NAICS.Code","NAICS.Text","NAICS_2digit","NAICS Industry",
                        "Email","Phone"),
                      names(df))
    datatable(df[, cols, drop = FALSE], options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # Download
  output$download_filtered <- downloadHandler(
    filename = function() paste0("businesses_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)

