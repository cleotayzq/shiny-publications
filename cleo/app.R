# written by cleo tay
# last updated 2023-09-28

library(shiny)
# library(shinythemes)
library(shinyWidgets)
library(dplyr)

# import publication data
publications <- read.csv("list.csv")

recode_authorship <- function(input_string) {
    if (input_string == "First") {
        output_int <- 1
    } else if (input_string == "Second") {
        output_int <- 2
    } else if (input_string == "Third") {
        output_int <- 3
    } else if (input_string == "&#8805; Fourth") {
        output_int <- 4
    }
    return(output_int)
}

# coauthors <- publications$author %>%
#     strsplit(., " and ") %>%
#     unlist() %>%
#     unique() %>%
#     subset(., . != "Cleo Tay")
# coauthors <- c("Xiao Pan Ding", "Shu Juan Goh", "Ryan Y. Hong", "Yu Juan Chua", "Joey Kei Teng Cheng", "Liyang Sai",
#                "Kang Lee", "Sherann Ler Ying Teo") %>%
#     sort()

keywords <- publications$keywords %>%
    strsplit(., ", ") %>%
    unlist() %>%
    unique()

# Define UI for application
ui <- fluidPage(
    # theme = shinytheme("flatly"),
    titlePanel(
        "Publications",
        ),
    sidebarLayout(
        sidebarPanel(
            sliderInput("year_range", "Filter by year:", 
                        min = min(publications$year),
                        max = max(publications$year),
                        value = c(min(publications$year), max(publications$year)),
                        step = 1, round = TRUE, sep = ""),
            sliderTextInput("authorship", "Filter by authorship order:",
                            choices = c("First", "Second", "Third", "&#8805; Fourth"),
                            selected = c("First", "&#8805; Fourth"),
                            grid = TRUE),
            sliderInput("total_authors", "Filter by number of authors:",
                        min = min(publications$nauthors), max = max(publications$nauthors),
                        value = c(min(publications$nauthors), max(publications$nauthors)),
                        step = 1, round = TRUE, sep = ""),
            checkboxGroupButtons("keyword_filter", "Filter by topic(s):",
                                 choices = keywords, selected = keywords,
                                 direction = "horizontal", size = "xs"),
            actionButton("no_keywords", "Clear topics")
            # checkboxGroupButtons("coauthor_filter", "Filter by co-author(s):",
            #                    choices = coauthors, selected = coauthors,
            #                    direction = "horizontal", size = "xs"),
            # actionButton("no_coauthors", "Clear co-authors"),
        ),
        mainPanel(
            fixedRow(column(width = 12, offset = 1.5,
                            actionButton("show_all", "Show all papers"))
            ),
            br(),
            htmlOutput("filtered_citations")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    filtered_data <- reactive({
        
        nyay_min <- recode_authorship(input$authorship[1])
        nyay_max <- recode_authorship(input$authorship[2]) %>%
            ifelse(. == 4, max(publications$nauthors), .)
        
        filtered <- publications %>%
            mutate(apacitation = gsub("\\\\\\&", "&", apacitation)) %>%
            filter(year >= input$year_range[1] & year <= input$year_range[2],
                   ncleo >= nyay_min & ncleo <= nyay_max,
                   nauthors >= input$total_authors[1] & nauthors <= input$total_authors[2]) %>%
            rowwise() %>%
            filter(
                # any(input$coauthor_filter %in% unlist(strsplit(author, " and "))),
                any(input$keyword_filter %in% unlist(strsplit(keywords, ", ")))
                ) %>%
            ungroup()
                   
        return(filtered$apacitation)
    })

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
    
    # Observe the Reset Filters button click and reset inputs
    observeEvent(input$show_all, {
        updateSliderInput(session, "year_range", value = c(min(publications$year), max(publications$year)))
        updateSliderTextInput(session, "authorship", selected = c("First", "&#8805; Fourth"))
        updateSliderInput(session, "total_authors", value = c(min(publications$nauthors), max(publications$nauthors)))
        # updateCheckboxGroupButtons(session, "coauthor_filter", selected = coauthors)
        updateCheckboxGroupButtons(session, "keyword_filter", selected = keywords)
    })
    # observeEvent(input$no_coauthors, {
    #     updateCheckboxGroupButtons(session, "coauthor_filter", selected = "")
    # })
    observeEvent(input$no_keywords, {
        updateCheckboxGroupButtons(session, "keyword_filter", selected = "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
