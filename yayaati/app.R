# written by cleo tay
# last updated 2023-10-19

# libraries ----
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(stringr)

# ---------------------
# import data from ADS
# ---------------------

bibtex.lines <- readLines("export-bibtex.txt", warn = F)

n_entries <- sum(grepl("ARTICLE", bibtex.lines))
article.attributes <- c("author", "title", "journal", "keywords",
                        "year", "volume", "number", "pages",
                        "doi", "eprint", "adsurl")
articles.df <- data.frame(matrix(ncol = length(article.attributes), nrow = n_entries))
colnames(articles.df) <- article.attributes

n_entry <- 0
for (line in bibtex.lines) {
    if (grepl("^@ARTICLE", line)){
        n_entry <- n_entry + 1
    }
    if (grepl("=", line)) {
        parts <- strsplit(line, "=")
        type <- str_trim(parts[[1]][1])
        value <- str_trim(parts[[1]][2]) %>%
            gsub("\\,$|^\"\\{|\\}\"$|\\{|\\}", "", .)
        if (type %in% article.attributes){
            articles.df[n_entry, grep(type, colnames(articles.df))] <- value
        }
    }
}

# function for formatting authors' names
format_authors <- function(input_string) {
    coauthors <- unlist(strsplit(input_string, " and "))
    
    # Initialize lists to store formatted author names
    last_names <- vector("list", length = length(coauthors))
    first_initials <- vector("list", length = length(coauthors))
    i <- 0 
    
    # Split each author's name into first initials and last names
    for (coauthor in coauthors) {
        i <- i+1 # coauthor number within input string
        # print(paste("coauthor number", i))
        parts <- strsplit(coauthor, ", ")
        
        last_name <- parts[[1]][1]
        last_names[[i]] <- last_name
        
        first_names <- strsplit(parts[[1]][2], " ")
        # print(paste("first names", first_names))
        # note: the above 3 are created anew each iteration of coauthor & aren't dependent on i, i.e., coauthor number
        
        # see if the first letter of first name is a symbol or letter using regex
        j <- 0 
        for (first_name in first_names[[1]]) {
            j <- j+1 # first name number within coauthor list
            # print(paste("first name j", j, "for a given coauthor i", i))
            
            if (grepl("^[A-Z]", substr(first_name, 1, 1))){
                first_initials[[i]][j] <- substr(first_name, 1, 1) %>%
                    paste0(., ".")
                # print(paste(first_names[[1]][j], "first_name A-Z"))
            } else {
                first_initials[[i]][j] <- substr(first_name, 2, 2) %>%
                    paste0(., ".")
                # print(paste(first_names[[1]][j], "first_name not A-Z"))
            }
            # print(paste("first_initials", first_initials))
        }
    }
    
    # Combine first initials and last names for each author
    formatted_authors <- mapply(function(last, initials) {
        initials <- paste(initials, collapse = " ")
        return(paste(last, initials, sep = ", "))
    }, last_names, first_initials)
    
    if (length(coauthors) == 1) {
        formatted_string <- formatted_authors[1]
    } else if(length(coauthors) == 2) {
        formatted_string <- paste(formatted_authors, collapse = " & ")
    } else if (length(coauthors) <= 7) {
        last_author <- formatted_authors[length(formatted_authors)]
        preceding_authors <- formatted_authors[1:(length(formatted_authors) - 1)]
        formatted_string <- paste(paste(preceding_authors, collapse = ", "), "&", last_author, sep = " ")
    } else {
        # if APA style with meaningful senior author
        last_author <- formatted_authors[length(formatted_authors)]
        preceding_authors <- formatted_authors[1:6]
        formatted_string <- paste0(paste(preceding_authors, collapse = ", "), ", ... ", last_author)
        # if et al, which is NOT APA!
        # formatted_string <- paste(paste(formatted_authors[1:7], collapse = ", "), "et al.", sep = " ")
    }
    
    return(formatted_string)
}

# function for finding y's authorship order
wheres_yayaati <- function(author) {
    author_list <- unlist(strsplit(author, " and "))
    position <- which(author_list == "Chachan, Yayaati")
    return(position)
}

articles.df <- articles.df %>%
    mutate(author = author %>%
               gsub("\\\\'a", "á", .) %>%
               gsub("\\\\vc", "č", .) %>%
               gsub("\\\\'c", "ć", .) %>%
               gsub('\\\\\"o', "ö", .) %>%
               gsub("\\\\'o", "ó", .) %>%
               gsub("\\\\'i", "í", .) %>%
               gsub("\\\\~n", "ñ", .) %>%
               gsub("\\.\\~", ". ", .)
    ) %>%
    rowwise() %>%
    mutate(apaauthors = format_authors(author),
           nyayaati = wheres_yayaati(author)) %>%
    ungroup() %>%
    mutate(title = gsub("\"$", "", title),
           journal = gsub("^\\\\", "", journal),
           year = as.integer(year),
           apajournal = recode(journal,
                               apjl = "The Astrophysical Journal Letters",
                               aj = "The Astronomical Journal",
                               apj = "The Astrophysical Journal",
                               apjs = "The Astrophysical Journal Supplement",
                               mnras = "Monthly Notices of the Royal Astronomical Society",
                               icarus = "Icarus",
                               pasp = "Publications of the Astronomical Society of the Pacific"),
           arXiv = gsub("^(.*)$", "https://arxiv.org/abs/\\1", eprint),
           doiurl = gsub("^(.*)$", "http://doi.org/\\1", doi),
           nauthors = str_count(author, "and") + 1)

generate_apa_citation <- function(row) {
    authors <- row$apaauthors
    year <- row$year
    title <- row$title
    journal <- row$apajournal
    volume <- row$volume
    number <- row$number
    pages <- row$pages
    doi <- row$doiurl
    arXiv <- row$arXiv
    adsurl <- row$adsurl
    citation <- paste(authors, " (", year, "). ",
                      title, ". <i>",
                      journal, ", ", volume, "</i>",
                      ifelse(!is.na(number), paste0("(", number, "), "), ", "),
                      pages,
                      ". <a href=\"", doi, "\" target=\"_blank\">", doi, "</a> ",
                      " [<a href=\"", arXiv, "\" target=\"_blank\">", "arXiv", "</a>]",
                      " [<a href=\"", adsurl, "\" target=\"_blank\">", "ADS", "</a>]",
                      sep = "")
    return(citation)
}

publications <- articles.df %>%
    select(apaauthors, year, title,
           apajournal, volume, number, pages,
           doiurl, arXiv, adsurl,
           nyayaati, nauthors,
           author, keywords) %>%
    mutate(apacitation = generate_apa_citation(.) %>%
               gsub("Chachan, Y\\.", "<u><strong>Chachan, Y.</strong></u>", .))

n_pubs <- nrow(publications)

# ---------------------

# list of keywords to show for filter
keywords <- publications$keywords %>%
    strsplit(., ", ") %>%
    unlist() %>%
    subset(., !str_detect(., "\\d+")) %>%
    sort() %>%
    tolower() %>%
    unique()

# --------------------------
# Define UI for application
# --------------------------

ui <- fluidPage(
    tags$head(
    tags$title("My Shiny App")
    ),
    theme = shinytheme("darkly"),
    tags$style(HTML('
    a {
      color: #87CEFA;
    }
    a:hover {
    color: #87CEFA;
    }
    ')),
    br(),
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
            checkboxGroupButtons("keyword_filter", "Filter by keyword(s):",
                                 choices = keywords, selected = "",
                                 direction = "horizontal", size = "xs",
                                 individual = TRUE,
                                 status = "primary",
                                 checkIcon = list(yes = icon("moon", lib = "font-awesome"))
                                 ),
            div(fluidRow(column(width = 12, align = "right",
                actionButton("no_keywords", "Clear keywords")
                )))
            ),
        # main panel
        mainPanel(
            fluidRow(
                column(width = 8,
                       h1("Publications")),
                column(width = 4, align = "right",
                       actionButton("show_all", "Showing all papers")
                )),
            br(),
            # list of publications
            htmlOutput("filtered_citations")
        )
    )
)

# --------------------
# Define server logic
# --------------------

server <- function(input, output, session) {
    
    filtered_data <- reactive({
        
        # authorship
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
        nyay_min <- recode_authorship(input$authorship[1])
        nyay_max <- recode_authorship(input$authorship[2]) %>%
            ifelse(. == 4, max(publications$nyayaati), .)
        
        # list of publications from dataframe
        filtered <- publications %>%
            # slider filters
            filter(year >= input$year_range[1] & year <= input$year_range[2],
                   nyayaati >= nyay_min & nyayaati <= nyay_max)
        
        if (length(input$keyword_filter) > 0){
            filtered <- filtered %>%
                rowwise() %>%
                filter(all(input$keyword_filter %in% tolower(unlist(strsplit(keywords, ", "))))) %>%
                ungroup()
        }
                   
        return(filtered$apacitation)
    })
    
    # create a list of HTML elements for each citation
    output$filtered_citations <- renderUI({ 
        filtered <- filtered_data()
        
        if (length(filtered) == 0) {
            return("No matching publications")
        } else {
            citation_list <- paste(filtered, collapse = "</li><br><li>") %>%
                paste("<ol><li>", ., "</li></ol>") %>%
                HTML()
            return(citation_list)
        }
    })
    
    # clear keywords button
    observeEvent(input$no_keywords, {
        updateCheckboxGroupButtons(session, "keyword_filter", selected = "")
    })
    
    observe({
        filtered <- filtered_data()
        if(length(filtered) == n_pubs){
            updateActionButton(session, "show_all", "Showing all papers")
        } else if (length(filtered) < n_pubs){
            updateActionButton(session, "show_all", "Show all papers")
        }
    })
    
    # 'show all' button
    observeEvent(input$show_all, {
        updateActionButton(session, "show_all", "Showing all papers")
        updateSliderInput(session, "year_range", value = c(min(publications$year), max(publications$year)))
        updateSliderTextInput(session, "authorship", selected = c("First", "&#8805; Fourth"))
        updateCheckboxGroupButtons(session, "keyword_filter", selected = "")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
