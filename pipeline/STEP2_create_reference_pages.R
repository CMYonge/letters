# STEP2_create_reference_pages_v2.R
# Creates reference pages for people, organizations, and bibliography

library(dplyr)
library(readr)
library(stringr)
library(purrr)

##### COpurrr##### CONFIGURATION #####

output_dir <- "C:/db/reference_pages_qmd_Apr26"
letter_dir <- "C:/db/all_letters_qmd_Apr26"

##### CLEAN TEXT FUNCTION #####

clean_text <- function(x) {
  if (is.na(x)) return(x)
  x <- str_replace_all(x, "<[^>]+>", "")
  x <- str_replace_all(x, '\\\\"', '"')
  x <- gsub("\\'", "'", x, fixed = TRUE)
  x <- str_replace_all(x, "â€˜", "'")
  x <- str_replace_all(x, "â€™", "'")
  x <- str_replace_all(x, "â€œ", '"')
  x <- str_replace_all(x, "â\u0080\u009d", '"')
  x <- str_replace_all(x, '"', "'")
  x <- trimws(x)
  return(x)
}

##### CREATE OUTPUT DIRECTORIES #####

dir.create(file.path(output_dir, "people"),        showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "organizations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "cmy_books"),     showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "other_books"),   showWarnings = FALSE, recursive = TRUE)

##### LOAD DATA #####

persons    <- read_csv("C:/db/persons.csv",                    show_col_types = FALSE)
others     <- read_csv("C:/db/wp_others.csv",                  show_col_types = FALSE)
cmy_bib    <- read_csv("C:/db/wp_cmybibliography.csv",         show_col_types = FALSE)
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv",
                    show_col_types = FALSE,
                    locale = locale(encoding = "UTF-8")) %>%
  filter(general_bookID != "general_bookID") %>%
  mutate(general_bookID = as.numeric(str_remove_all(general_bookID, ",")))
posts      <- read_csv("C:/db/wp_posts.csv",                   show_col_types = FALSE)

#### CMY bibliography: display_title and sort_title ####

move_trailing_article <- function(text) {
  m <- str_match(text, "^(.*),\\s+(The|A|An)$")
  if (!is.na(m[1, 1])) paste(m[1, 3], m[1, 2]) else text
}

strip_leading_article <- function(text) {
  str_replace(text, "^(The|A|An)\\s+", "")
}

make_titles <- function(raw_title) {
  if (str_detect(raw_title, "^<i>")) {
    italic_content <- str_match(raw_title, "<i>(.*?)</i>")[1, 2]
    after_italic   <- str_trim(str_replace(raw_title, "^<i>.*?</i>", ""))
    after_plain    <- str_trim(str_replace_all(after_italic, "<[^>]+>", ""))
    
    display_italic <- move_trailing_article(italic_content)
    display_title  <- if (nchar(after_plain) > 0) paste0(display_italic, after_plain)
    else display_italic
    sort_title     <- strip_leading_article(display_italic)
  } else {
    clean         <- clean_text(raw_title)
    display_title <- clean
    sort_title    <- strip_leading_article(clean)
  }
  list(display_title = display_title, sort_title = sort_title)
}

cmy_bib <- cmy_bib %>%
  mutate(
    processed     = map(title, make_titles),
    display_title = map_chr(processed, "display_title"),
    sort_title    = map_chr(processed, "sort_title")
  ) %>%
  select(-processed)

write_csv(cmy_bib, "C:/db/wp_cmybibliography_with_sort.csv")

##### LOAD LINKING TABLES #####

person_links <- read_csv("C:/db/wp_persons_posts.csv",            show_col_types = FALSE)
other_links  <- read_csv("C:/db/wp_others_posts.csv",             show_col_types = FALSE)
cmy_links    <- read_csv("C:/db/wp_cmybibliography_posts.csv",    show_col_types = FALSE)
gen_links    <- read_csv("C:/db/wp_generalbibliography_posts.csv",show_col_types = FALSE)

##### GET LETTER IDS #####

letter_files <- list.files(letter_dir, pattern = "\\.qmd$", full.names = TRUE)
letter_ids   <- as.numeric(str_extract(basename(letter_files), "\\d+"))

cat("Creating reference pages for", length(letter_ids), "letters\n\n")

##### CREATE PEOPLE PAGES #####

cat("Creating people pages...\n")

people_count <- 0
for (i in 1:nrow(persons)) {
  person_id <- persons$persons_id[i]
  
  name_parts <- c(
    persons$prefix[i],
    persons$first_name[i],
    persons$maiden_name[i],
    persons$surname[i],
    persons$suffix[i]
  )
  name_parts <- name_parts[!is.na(name_parts)]
  name <- paste(name_parts, collapse = " ")
  if (name == "") name <- paste("Person", person_id)
  name <- clean_text(name)
  
  bio_parts <- c()
  if (!is.na(persons$title[i])       && persons$title[i]       != "") bio_parts <- c(bio_parts, persons$title[i])
  if (!is.na(persons$dates[i])       && persons$dates[i]       != "") bio_parts <- c(bio_parts, persons$dates[i])
  if (!is.na(persons$description[i]) && persons$description[i] != "") bio_parts <- c(bio_parts, persons$description[i])
  if (!is.na(persons$biography[i])   && persons$biography[i]   != "") bio_parts <- c(bio_parts, persons$biography[i])
  bio <- paste(bio_parts, collapse = "\n\n")
  
  mentions <- person_links %>%
    filter(persons_id == person_id, post_id %in% letter_ids)
  
  content <- paste0(
    "---\n",
    "title: \"", name, "\"\n",
    "person_id: ", person_id, "\n",
    "---\n\n"
  )
  
  if (bio != "") {
    content <- paste0(content, "## Biographical Information\n\n", bio, "\n\n")
  }
  
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Person\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      slug_row  <- posts %>% filter(post_id == letter_id)
      slug      <- if (nrow(slug_row) > 0) slug_row$post_name[1] else paste0("letter-", letter_id)
      content   <- paste0(content, "- [Letter ", letter_id, "](../", letter_id, "-", slug, ".qmd)\n")
    }
  }
  
  output_file <- file.path(output_dir, "people", paste0("person_", person_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  people_count <- people_count + 1
}

cat("✓ Created", people_count, "people pages\n\n")

##### CREATE ORGANIZATION PAGES #####

cat("Creating organization pages...\n")

org_count <- 0
for (i in 1:nrow(others)) {
  other_id    <- others$others_id[i]
  name        <- clean_text(others$name[i])
  description <- ifelse(is.na(others$description[i]) || others$description[i] == "",
                        "", others$description[i])
  type        <- ifelse(is.na(others$type[i]) || others$type[i] == "",
                        "", others$type[i])
  
  mentions <- other_links %>%
    filter(others_id == other_id, post_id %in% letter_ids)
  
  content <- paste0(
    "---\n",
    "title: \"", name, "\"\n",
    "other_id: ", other_id, "\n",
    "---\n\n"
  )
  
  if (type        != "") content <- paste0(content, "**Type:** ", type, "\n\n")
  if (description != "") content <- paste0(content, "## Description\n\n", description, "\n\n")
  
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      slug_row  <- posts %>% filter(post_id == letter_id)
      slug      <- if (nrow(slug_row) > 0) slug_row$post_name[1] else paste0("letter-", letter_id)
      content   <- paste0(content, "- [Letter ", letter_id, "](../", letter_id, "-", slug, ".qmd)\n")
    }
  }
  
  output_file <- file.path(output_dir, "organizations", paste0("other_", other_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  org_count <- org_count + 1
}

cat("✓ Created", org_count, "organization pages\n\n")

##### CREATE CMY BOOK PAGES #####

cat("Creating CMY bibliography pages...\n")

cmy_count <- 0
for (i in 1:nrow(cmy_bib)) {
  book_id <- cmy_bib$cmy_bookID[i]
  title      <- cmy_bib$display_title[i]
  sort_title <- cmy_bib$sort_title[i]
  
  info_parts <- c()
  if (!is.na(cmy_bib$date[i])      && cmy_bib$date[i]      != "") info_parts <- c(info_parts, paste("**Date:**",      cmy_bib$date[i]))
  if (!is.na(cmy_bib$genre[i])     && cmy_bib$genre[i]     != "") info_parts <- c(info_parts, paste("**Genre:**",     cmy_bib$genre[i]))
  if (!is.na(cmy_bib$publisher[i]) && cmy_bib$publisher[i] != "") info_parts <- c(info_parts, paste("**Publisher:**", cmy_bib$publisher[i]))
  if (!is.na(cmy_bib$notes[i])     && cmy_bib$notes[i]     != "") info_parts <- c(info_parts, paste("**Notes:**",     cmy_bib$notes[i]))
  info <- paste(info_parts, collapse = "\n\n")
  
  mentions <- cmy_links %>%
    filter(cmy_bookID == book_id, post_ID %in% letter_ids)
  
  content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "book_id: ", book_id, "\n",
    "sort_title: \"", sort_title, "\"\n",
    "---\n\n"
  )
  
  if (info != "") content <- paste0(content, "## Publication Information\n\n", info, "\n\n")
  
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Work\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_ID[j]
      slug_row  <- posts %>% filter(post_id == letter_id)
      slug      <- if (nrow(slug_row) > 0) slug_row$post_name[1] else paste0("letter-", letter_id)
      content   <- paste0(content, "- [Letter ", letter_id, "](../", letter_id, "-", slug, ".qmd)\n")
    }
  }
  
  output_file <- file.path(output_dir, "cmy_books", paste0("cmybook_", book_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  cmy_count <- cmy_count + 1
}

cat("✓ Created", cmy_count, "CMY book pages\n\n")

##### CREATE GENERAL BIBLIOGRAPHY PAGES #####

cat("Creating general bibliography pages...\n")

gen_count <- 0
for (i in 1:nrow(gen_bib)) {
  book_id <- gen_bib$general_bookID[i]
  
  author_parts <- c(
    gen_bib$author_prefix[i],
    gen_bib$author_first_name[i],
    gen_bib$author_surname[i],
    gen_bib$author_suffix[i]
  )
  author_parts <- author_parts[!is.na(author_parts)]
  author       <- paste(author_parts, collapse = " ")
  
  title_parts <- c()
  if (author != "") title_parts <- c(title_parts, author)
  if (!is.na(gen_bib$title[i]) && gen_bib$title[i] != "") title_parts <- c(title_parts, gen_bib$title[i])
  
  title <- paste(title_parts, collapse = " - ")
  if (title == "") title <- paste("Book", book_id)
  title <- clean_text(title)
  
  info_parts <- c()
  if (!is.na(gen_bib$date[i])    && gen_bib$date[i]    != "") info_parts <- c(info_parts, paste("**Date:**",    gen_bib$date[i]))
  if (!is.na(gen_bib$imprint[i]) && gen_bib$imprint[i] != "") info_parts <- c(info_parts, paste("**Imprint:**", gen_bib$imprint[i]))
  if (!is.na(gen_bib$notes[i])   && gen_bib$notes[i]   != "") info_parts <- c(info_parts, paste("**Notes:**",   gen_bib$notes[i]))
  info <- paste(info_parts, collapse = "\n\n")
  
  mentions <- gen_links %>%
    filter(gen_bookID == book_id, post_id %in% letter_ids)
  
  content <- paste0(
    "---\n",
    "title: \"", title, "\"\n",
    "book_id: ", book_id, "\n",
    "---\n\n"
  )
  
  if (info != "") content <- paste0(content, "## Publication Information\n\n", info, "\n\n")
  
  if (nrow(mentions) > 0) {
    content <- paste0(content, "## Letters Mentioning This Work\n\n")
    for (j in 1:nrow(mentions)) {
      letter_id <- mentions$post_id[j]
      slug_row  <- posts %>% filter(post_id == letter_id)
      slug      <- if (nrow(slug_row) > 0) slug_row$post_name[1] else paste0("letter-", letter_id)
      content   <- paste0(content, "- [Letter ", letter_id, "](../", letter_id, "-", slug, ".qmd)\n")
    }
  }
  
  output_file <- file.path(output_dir, "other_books", paste0("otherbook_", book_id, ".qmd"))
  writeLines(content, output_file, useBytes = TRUE)
  gen_count <- gen_count + 1
}

cat("✓ Created", gen_count, "general bibliography pages\n\n")

##### SUMMARY #####

cat("✅ All reference pages created!\n\n")
cat("Summary:\n")
cat("  People:",        people_count, "\n")
cat("  Organizations:", org_count,    "\n")
cat("  CMY Books:",     cmy_count,    "\n")
cat("  Other Books:",   gen_count,    "\n")
cat("  Total:", people_count + org_count + cmy_count + gen_count, "\n\n")
cat("Output directory:", output_dir, "\n")
cat("\nNext: Run STEP 3 to create website structure\n")