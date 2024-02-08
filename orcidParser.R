library(stringr)
library(dplyr)

# import (edited) txt file from orcid
bibtex_lines <- readLines("orcidCleo.txt", warn = F)

# count number of articles
n_entries <- sum(grepl("^\\@article\\{", bibtex_lines))

# pull out relevant article attributes
article_cols <- c("author", "title", "journal", "year", "volume", "number", "pages", "url", "doi", "keywords")

# create empty data frame with relevant attributes as colnames
df_articles <- data.frame(matrix(ncol = length(article_cols), nrow = n_entries))
colnames(df_articles) <- article_cols

n_entry <- 0
for (line in bibtex_lines) {
  if (grepl("^\\@article\\{", line)){
    n_entry <- n_entry + 1
  }
  if (grepl("=", line)) {
    parts <- strsplit(line, "=")
    type <- str_trim(parts[[1]][1])
    value <- str_trim(parts[[1]][2]) %>%
      gsub("\\,$|^\"\\{|\\}\"$|\\{|\\}", "", .)
    if (type %in% article_cols){
      df_articles[n_entry, grep(type, colnames(df_articles))] <- value
    }
  }
}

# function to format author names
format_authors <- function(input_string) {
  coauthors <- unlist(strsplit(input_string, " and "))
  
  # initialize lists to store formatted author names
  last_names <- vector("list", length = length(coauthors))
  first_initials <- vector("list", length = length(coauthors))
  i <- 0 
  
  # Split each author's name into first initials and last names
  for (coauthor in coauthors) {
    i <- i+1 # coauthor number within input string
    # print(paste("coauthor number", i))
    parts <- strsplit(coauthor, "\\s+(?=\\S+$)", perl = TRUE)
    
    last_name <- parts[[1]][2]
    last_names[[i]] <- last_name
    
    first_names <- strsplit(parts[[1]][1], " ")
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
  } else if (length(coauthors) <= 10) {
    last_author <- formatted_authors[length(formatted_authors)]
    preceding_authors <- formatted_authors[1:(length(formatted_authors) - 1)]
    formatted_string <- paste(paste(preceding_authors, collapse = ", "), "&", last_author, sep = " ")
  } else {
    last_author <- formatted_authors[length(formatted_authors)]
    preceding_authors <- formatted_authors[1:9]
    formatted_string <- paste0(paste(preceding_authors, collapse = ", "), ", ... ", last_author)
  }
  
  return(formatted_string)
}


# function to find authorship order
wheres_cleo <- function(author) {
  author_list <- unlist(strsplit(author, " and "))
  position <- which(author_list == "Cleo Tay")
  return(position)
}

# function to generate APA style citation with bolded author
generate_apa_citation <- function(row) {
  authors <- row$apaauthors
  year <- row$year
  title <- row$title
  journal <- row$journal
  volume <- row$volume
  number <- row$number
  pages <- row$pages
  doi <- row$url
  citation <- paste(authors, " (", year, "). ",
                    title, ". <i>",
                    journal, ", ", volume, "</i>",
                    ifelse(!is.na(number), paste0("(", number, "), "), ", "),
                    pages,
                    ". <a href=\"", doi, "\" target=\"_blank\">", doi, "</a>",
                    sep = "")
  return(citation)
}

# edit data frame to include columns with apa citation and authorship order
df_articles <- df_articles %>%
  rowwise() %>%
  mutate(apaauthors = format_authors(author),
         ncleo = wheres_cleo(author)) %>%
  ungroup() %>%
  mutate(nauthors = str_count(author, "and") + 1,
         year = as.integer(year),
         apacitation = generate_apa_citation(.) %>%
           gsub("Tay, C\\.", "<u><strong>Tay, C.</strong></u>", .))

# function to recode authorship function for slider
recode_authorship <- function(input_string) {
  if (input_string == "First") {
    output_int <- 1
  } else if (input_string == "Second") {
    output_int <- 2
  } else if (input_string == "&#8805; Third") {
    output_int <- 3
  }
  return(output_int)
}

# list unique keywords across all articles
keywords <- df_articles$keywords %>%
  strsplit(., ", ") %>%
  unlist() %>%
  unique()