# written by cleo tay
# last updated 2024-02-07

# import libraries ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)

source("orcidParser.R")

# Define UI for application ----
ui <- fluidPage(
  theme = shinytheme("darkly"),
  useShinyjs(),
  tags$head(
    tags$title("Publications")
  ),
  tags$style('
    a {
      color: #87CEFA;
    }
    a:hover {
      color: #87CEFA;
    }
    h1 {
      margin: 0
    }
    ol {
      padding-left: 1.5em;
    }
    .btn-group{
      margin-bottom: 3px;
    }
    '
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year_range",
        "Filter by year:", 
        min = min(df_articles$year),
        max = max(df_articles$year),
        value = c(min(df_articles$year), max(df_articles$year)),
        step = 1,
        round = TRUE,
        sep = ""),
      sliderTextInput(
        "authorship",
        "Filter by authorship order:",
        choices = c("First", "Second", "&#8805; Third"),
        selected = c("First", "&#8805; Third"),
        grid = TRUE),
      checkboxGroupButtons(
        "keyword_filter",
        "Filter by topic:",
        choices = keywords,
        selected = "",
        status = "primary",
        individual = TRUE,
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon")
        ),
        size = "xs")
    ),
    mainPanel(
      fixedRow(
        column(
          width = 8,
          tags$h1("Publications")
        ),
        column(
          width = 4,
          align = "right",
          actionButton(
            "show_all",
            "Showing all papers"
          )
          
        )
      ),
      br(),
      htmlOutput("filtered_citations")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
      ncleo_min <- recode_authorship(input$authorship[1])
      ncleo_max <- recode_authorship(input$authorship[2]) %>%
          ifelse(. == 5, max(df_articles$nauthors), .)
      
      filtered <- df_articles %>%
          mutate(apacitation = gsub("\\\\\\&", "&", apacitation)) %>%
          filter(year >= input$year_range[1] & year <= input$year_range[2],
                 ncleo >= ncleo_min & ncleo <= ncleo_max
                 )
      
      if (length(input$keyword_filter) != 0){
        filtered <- filtered %>%
          rowwise() %>%
          filter(all(input$keyword_filter %in% unlist(strsplit(keywords, ", ")))) %>%
          ungroup()
      }
      
      return(filtered$apacitation)
  })
  
  # output HTML list of citations
  output$filtered_citations <- renderUI({ 
      filtered <- filtered_data()
      
      if (length(filtered) == 0) {
          return("No matching publications")
      } else {
          # Create a list of HTML elements for each citation
          citation_list <- paste(filtered, collapse = "</li><br><li>") %>%
              paste("<ol><li>", ., "</li></ol>") %>%
              HTML()
          return(citation_list)
      }
  })
  
  # Observe the show all button click and reset inputs
  observeEvent(input$show_all, {
    updateSliderInput(session, "year_range", value = c(min(df_articles$year), max(df_articles$year)))
    updateSliderTextInput(session, "authorship", selected = c("First", "&#8805; Third"))
    updateCheckboxGroupButtons(session, "keyword_filter", selected = "")
  })
  
  # change button text depending on shown papers
  observe({
    filtered <- filtered_data()
    if(length(filtered) == n_entries){
      updateActionButton(session, "show_all", "Showing all papers")
      runjs('$("#show_all").prop("disabled", true); $("#show_all").css("background-color", "");')
    } else if (length(filtered) < n_entries){
      updateActionButton(session, "show_all", "Show all papers")
      runjs('$("#show_all").prop("disabled", false); $("#show_all").css("background-color", "#4F7298");')
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
