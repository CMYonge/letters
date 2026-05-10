# STEP3_create_website_and_convert_links_CORRECTED.R
# Creates website structure, copies files, converts [[tags]] to links

library(dplyr)
library(readr)
library(stringr)

##### CONFIGURATION #####

letter_source <- "C:/db/all_letters_qmd_Apr26"
ref_source    <- "C:/db/reference_pages_qmd_Apr26"
website_dir   <- "C:/db/cmy_letters_website_Apr"

cat("STEP 3: Building website structure\n\n")

##### FUNCTION: PARSE DATE TO ISO FORMAT #####

parse_iso_date <- function(date_str) {
  if (is.na(date_str) || date_str == "") return("")
  
  year <- NA
  m <- str_match(date_str, "(\\d{4})")
  if (!is.na(m[1,2])) year <- m[1,2]
  
  if (is.na(year)) {
    m <- str_match(date_str, "\\[(\\d{4})\\]")
    if (!is.na(m[1,2])) year <- m[1,2]
  }
  
  if (is.na(year)) return("")
  
  months <- c(
    jan = "01", january = "01",
    feb = "02", febry = "02", february = "02",
    mar = "03", march = "03",
    apr = "04", april = "04",
    may = "05",
    jun = "06", june = "06",
    jul = "07", july = "07",
    aug = "08", august = "08",
    sep = "09", september = "09",
    oct = "10", october = "10",
    nov = "11", november = "11",
    dec = "12", december = "12"
  )
  
  month <- "01"
  date_lower <- tolower(date_str)
  for (mon_name in names(months)) {
    if (str_detect(date_lower, mon_name)) {
      month <- months[mon_name]
      break
    }
  }
  
  day <- "01"
  m <- str_match(date_str, "\\b(\\d{1,2})(?:st|nd|rd|th)?\\b")
  if (!is.na(m[1,2])) {
    d <- as.integer(m[1,2])
    if (d >= 1 && d <= 31) day <- sprintf("%02d", d)
  }
  
  return(paste0(year, "-", month, "-", day))
}

##### PART 1: CREATE WEBSITE STRUCTURE #####

cat("Part 1: Creating website folders...\n")

dir.create(website_dir,                                    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "letters"),              showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "people"),               showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "organizations"),        showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "cmy_books"),            showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "other_books"),          showWarnings = FALSE, recursive = TRUE)

cat("✓ Created folder structure\n\n")

##### PART 2: CREATE QUARTO CONFIGURATION #####

cat("Part 2: Creating Quarto config files...\n")

quarto_config <- '
project:
  type: website
  output-dir: _site

website:
  title: "Charlotte Mary Yonge Letters"
  navbar:
    left:
      - text: "Home"
        href: index.qmd
      - text: "Letters"
        href: letters/index.qmd
      - text: "People"
        href: people/index.qmd
      - text: "Organizations"
        href: organizations/index.qmd
      - text: "CMY Bibliography"
        href: cmy_books/index.qmd
      - text: "General Bibliography"
        href: other_books/index.qmd
  search: true

format:
  html:
    theme: cosmo
    css: styles.css
    toc: true
    toc-depth: 2
'

writeLines(quarto_config, file.path(website_dir, "_quarto.yml"))

index_content <- '---
title: "Charlotte Mary Yonge Letters Collection"
---

## About This Collection

This digital collection contains correspondence from Charlotte Mary Yonge, with detailed
cross-references to people, organizations, and publications mentioned in the letters.

## Browse

- **[Letters](letters/index.qmd)** - Browse all letters chronologically
- **[People](people/index.qmd)** - Index of correspondents and people mentioned
- **[Organizations](organizations/index.qmd)** - Churches, institutions, and groups
- **[CMY Bibliography](cmy_books/index.qmd)** - Works by Charlotte Mary Yonge
- **[General Bibliography](other_books/index.qmd)** - Other works referenced

## Search

Use the search box in the navigation bar to find letters, people, or topics.

---

*This collection was originally maintained as a WordPress database. It has been
converted to a static website for long-term preservation and accessibility.*
'

writeLines(index_content, file.path(website_dir, "index.qmd"))

# Placeholder index pages — will be overwritten by Parts 7-11
writeLines('---\ntitle: "Letters"\n---\n\nLetters are listed chronologically below.\n',
           file.path(website_dir, "letters", "index.qmd"))
writeLines('---\ntitle: "People"\n---\n\nBiographical information about correspondents and people mentioned in the letters.\n',
           file.path(website_dir, "people", "index.qmd"))
writeLines('---\ntitle: "Organizations"\n---\n\nChurches, institutions, and other entities referenced in the letters.\n',
           file.path(website_dir, "organizations", "index.qmd"))
writeLines('---\ntitle: "Works by Charlotte Mary Yonge"\n---\n\nBooks and publications by Charlotte Mary Yonge referenced in the correspondence.\n',
           file.path(website_dir, "cmy_books", "index.qmd"))
writeLines('---\ntitle: "General Bibliography"\n---\n\nBooks, articles, and publications by other authors mentioned in the letters.\n',
           file.path(website_dir, "other_books", "index.qmd"))

css_content <- '
/* Custom styles for CMY Letters website */

body {
  font-family: Georgia, serif;
  line-height: 1.6;
}

/* Hide Quarto auto-generated title metadata (Published date) */
.quarto-title-meta {
  display: none;
}

.letter-metadata {
  background-color: #f5f5f5;
  padding: 1em;
  margin-bottom: 1.5em;
  border-left: 4px solid #333;
}

.person-bio {
  font-style: italic;
  color: #555;
  margin-bottom: 1em;
}
'

writeLines(css_content, file.path(website_dir, "styles.css"))
cat("✓ Created config files\n\n")

##### PART 3: LOAD LOOKUP TABLES #####

cat("Part 3: Loading lookup tables...\n")

persons      <- read_csv("C:/db/persons.csv",                     show_col_types = FALSE)
others       <- read_csv("C:/db/wp_others.csv",                   show_col_types = FALSE)
cmy_bib      <- read_csv("C:/db/wp_cmybibliography.csv",          show_col_types = FALSE)
gen_bib      <- read_csv("C:/db/wp_generalbibliography.csv",      show_col_types = FALSE)
person_links <- read_csv("C:/db/wp_persons_posts.csv",            show_col_types = FALSE)
other_links  <- read_csv("C:/db/wp_others_posts.csv",             show_col_types = FALSE)
cmy_links    <- read_csv("C:/db/wp_cmybibliography_posts.csv",    show_col_types = FALSE)
gen_links    <- read_csv("C:/db/wp_generalbibliography_posts.csv",show_col_types = FALSE)

get_person_name <- function(id) {
  person <- persons %>% filter(persons_id == id)
  if (nrow(person) == 0) return(paste("Person", id))
  name_parts <- c(person$prefix[1], person$first_name[1],
                  person$maiden_name[1], person$surname[1], person$suffix[1])
  name_parts <- name_parts[!is.na(name_parts)]
  name <- paste(name_parts, collapse = " ")
  if (name == "") return(paste("Person", id))
  return(name)
}

get_other_name <- function(id) {
  other <- others %>% filter(others_id == id)
  if (nrow(other) == 0) return(paste("Other", id))
  return(other$name[1])
}

get_cmy_title <- function(id) {
  book <- cmy_bib %>% filter(cmy_bookID == id)
  if (nrow(book) == 0) return(paste("CMY Book", id))
  title <- book$title[1]
  title <- str_replace_all(title, "<[^>]+>", "")
  title <- trimws(title)
  return(title)
}

get_gen_title <- function(id) {
  book <- gen_bib %>% filter(general_bookID == id)
  if (nrow(book) == 0) return(paste("Book", id))
  title <- book$title[1]
  title <- str_replace_all(title, "<[^>]+>", "")
  title <- trimws(title)
  return(title)
}
cat("✓ Lookup tables loaded\n\n")

##### PART 4: FUNCTION TO CONVERT TAGS TO LINKS #####

convert_tags_and_fix_yaml <- function(filepath, file_location) {
  
  content      <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content_text <- paste(content, collapse = "\n")
  
  if (file_location == "letters") {
    person_path <- "../people/person_"
    org_path    <- "../organizations/other_"
    cmy_path    <- "../cmy_books/cmybook_"
    book_path   <- "../other_books/otherbook_"
  } else if (file_location == "people") {
    person_path <- "person_"
    org_path    <- "../organizations/other_"
    cmy_path    <- "../cmy_books/cmybook_"
    book_path   <- "../other_books/otherbook_"
  } else if (file_location == "organizations") {
    person_path <- "../people/person_"
    org_path    <- "other_"
    cmy_path    <- "../cmy_books/cmybook_"
    book_path   <- "../other_books/otherbook_"
  } else if (file_location == "cmy_books") {
    person_path <- "../people/person_"
    org_path    <- "../organizations/other_"
    cmy_path    <- "cmybook_"
    book_path   <- "../other_books/otherbook_"
  } else if (file_location == "other_books") {
    person_path <- "../people/person_"
    org_path    <- "../organizations/other_"
    cmy_path    <- "../cmy_books/cmybook_"
    book_path   <- "otherbook_"
  }
  
  # --- Person tags ---
  # Handle display text format first: [[person:123]Friend]
  person_display <- str_match_all(content_text, "\\[\\[person:(\\d+)\\]([^\\]]+)\\]")[[1]]
  if (nrow(person_display) > 0) {
    for (i in 1:nrow(person_display)) {
      tag          <- person_display[i, 1]
      id           <- as.numeric(person_display[i, 2])
      display_text <- gsub("\\'", "'", trimws(person_display[i, 3]), fixed = TRUE)
      link         <- paste0("[", display_text, "](", person_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  # Handle plain format: [[person:123]]
  person_matches <- str_match_all(content_text, "\\[\\[person:(\\d+)\\]\\]")[[1]]
  if (nrow(person_matches) > 0) {
    for (i in 1:nrow(person_matches)) {
      tag          <- person_matches[i, 1]
      id           <- as.numeric(person_matches[i, 2])
      name         <- get_person_name(id)
      link         <- paste0("[", name, "](", person_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # --- Other tags ---
  other_display <- str_match_all(content_text, "\\[\\[other:(\\d+)\\]([^\\]]+)\\]")[[1]]
  if (nrow(other_display) > 0) {
    for (i in 1:nrow(other_display)) {
      tag          <- other_display[i, 1]
      id           <- as.numeric(other_display[i, 2])
      display_text <- gsub("\\'", "'", trimws(other_display[i, 3]), fixed = TRUE)
      link         <- paste0("[", display_text, "](", org_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  other_matches <- str_match_all(content_text, "\\[\\[other:(\\d+)\\]\\]")[[1]]
  if (nrow(other_matches) > 0) {
    for (i in 1:nrow(other_matches)) {
      tag          <- other_matches[i, 1]
      id           <- as.numeric(other_matches[i, 2])
      name         <- get_other_name(id)
      link         <- paste0("[", name, "](", org_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # --- CMY book tags ---
  cmy_display <- str_match_all(content_text, "\\[\\[cmybook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  if (nrow(cmy_display) > 0) {
    for (i in 1:nrow(cmy_display)) {
      tag          <- cmy_display[i, 1]
      id           <- as.numeric(cmy_display[i, 2])
      display_text <- gsub("\\'", "'", trimws(cmy_display[i, 3]), fixed = TRUE)
      link         <- paste0("[", display_text, "](", cmy_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  cmy_matches <- str_match_all(content_text, "\\[\\[cmybook:(\\d+)\\]\\]")[[1]]
  if (nrow(cmy_matches) > 0) {
    for (i in 1:nrow(cmy_matches)) {
      tag          <- cmy_matches[i, 1]
      id           <- as.numeric(cmy_matches[i, 2])
      title        <- get_cmy_title(id)
      link         <- paste0("[", title, "](", cmy_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # --- Other book tags ---
  book_display <- str_match_all(content_text, "\\[\\[otherbook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  if (nrow(book_display) > 0) {
    for (i in 1:nrow(book_display)) {
      tag          <- book_display[i, 1]
      id           <- as.numeric(book_display[i, 2])
      display_text <- gsub("\\'", "'", trimws(book_display[i, 3]), fixed = TRUE)
      link         <- paste0("[", display_text, "](", book_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  book_matches <- str_match_all(content_text, "\\[\\[otherbook:(\\d+)\\]\\]")[[1]]
  if (nrow(book_matches) > 0) {
    for (i in 1:nrow(book_matches)) {
      tag          <- book_matches[i, 1]
      id           <- as.numeric(book_matches[i, 2])
      title        <- get_gen_title(id)
      link         <- paste0("[", title, "](", book_path, id, ".qmd)")
      content_text <- str_replace(content_text, fixed(tag), link)
    }
  }
  
  # Final safety pass — fix any remaining escaped apostrophes
  content_text <- gsub("\\'", "'", content_text, fixed = TRUE)
  
  return(content_text)
}

##### PART 5: COPY AND CONVERT LETTERS #####

cat("Part 5: Processing letters...\n")

letter_files      <- list.files(letter_source, pattern = "\\.qmd$", full.names = TRUE)
letters_processed <- 0

for (letter_file in letter_files) {
  converted_content <- convert_tags_and_fix_yaml(letter_file, "letters")
  output_file       <- file.path(website_dir, "letters", basename(letter_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  letters_processed <- letters_processed + 1
}

cat("✓ Processed", letters_processed, "letters\n\n")

# Copy decade introduction pages
dir.create(file.path(website_dir, "letters", "decades"), showWarnings = FALSE)
decade_files <- list.files(file.path(letter_source, "decades"),
                           pattern = "\\.qmd$", full.names = TRUE)
for (decade_file in decade_files) {
  converted_content <- convert_tags_and_fix_yaml(decade_file, "letters")
  output_file       <- file.path(website_dir, "letters", "decades", basename(decade_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
}
cat(sprintf("✓ Copied %d decade pages\n\n", length(decade_files)))

##### PART 6: COPY AND CONVERT REFERENCE PAGES #####

cat("Part 6: Processing reference pages...\n")

people_files  <- list.files(file.path(ref_source, "people"),        pattern = "\\.qmd$", full.names = TRUE)
people_processed <- 0
for (f in people_files) {
  writeLines(convert_tags_and_fix_yaml(f, "people"),
             file.path(website_dir, "people", basename(f)), useBytes = TRUE)
  people_processed <- people_processed + 1
}
cat("✓ Processed", people_processed, "people pages\n")

org_files     <- list.files(file.path(ref_source, "organizations"), pattern = "\\.qmd$", full.names = TRUE)
org_processed <- 0
for (f in org_files) {
  writeLines(convert_tags_and_fix_yaml(f, "organizations"),
             file.path(website_dir, "organizations", basename(f)), useBytes = TRUE)
  org_processed <- org_processed + 1
}
cat("✓ Processed", org_processed, "organization pages\n")

cmy_files     <- list.files(file.path(ref_source, "cmy_books"),     pattern = "\\.qmd$", full.names = TRUE)
cmy_processed <- 0
for (f in cmy_files) {
  writeLines(convert_tags_and_fix_yaml(f, "cmy_books"),
             file.path(website_dir, "cmy_books", basename(f)), useBytes = TRUE)
  cmy_processed <- cmy_processed + 1
}
cat("✓ Processed", cmy_processed, "CMY book pages\n")

book_files    <- list.files(file.path(ref_source, "other_books"),   pattern = "\\.qmd$", full.names = TRUE)
book_processed <- 0
for (f in book_files) {
  writeLines(convert_tags_and_fix_yaml(f, "other_books"),
             file.path(website_dir, "other_books", basename(f)), useBytes = TRUE)
  book_processed <- book_processed + 1
}
cat("✓ Processed", book_processed, "other book pages\n\n")

##### PART 7: CREATE LETTERS INDEX PAGE #####

cat("Part 7: Creating letters index page...\n")

letterinfo <- read_csv("C:/db/wp_letterinfo.csv",
                       show_col_types = FALSE,
                       col_types = cols(letter_date = col_character()))
posts      <- read_csv("C:/db/wp_posts.csv", show_col_types = FALSE)

# Filter to letters only (exclude decade intro pages)
letters_df <- posts %>%
  filter(post_type == "post") %>%
  left_join(letterinfo, by = c("post_id" = "post_ID")) %>%
  filter(!is.na(post_name)) %>%
  mutate(iso_date = sapply(letter_date, parse_iso_date)) %>%
  arrange(iso_date) %>%
  mutate(
    year   = as.integer(substr(iso_date, 1, 4)),
    decade = (floor(year / 10) * 10)
  )

n_letters <- nrow(letters_df)

index_content <- paste0(
  "---\n",
  "title: \"Letters Index\"\n",
  "---\n\n",
  "# Letters Index\n\n",
  "Browse all ", n_letters, " letters in chronological order. ",
  "Click any letter title to read the full text.\n\n",
  "## Search Tips\n\n",
  "Use your browser's search function (Ctrl/Cmd+F) to find:\n\n",
  "- Specific correspondents\n",
  "- Locations (e.g., Otterbourne, Winchester)\n",
  "- Topics or keywords from the letters\n\n",
  "---\n\n"
)

decades <- sort(unique(letters_df$decade[!is.na(letters_df$decade)]))

for (dec in decades) {
  decade_letters <- letters_df %>% filter(decade == dec) %>% arrange(iso_date)
  index_content  <- paste0(index_content, "## ", dec, "s\n\n")
  
  for (i in 1:nrow(decade_letters)) {
    row      <- decade_letters[i, ]
    filename <- paste0(row$post_id, "-", row$post_name, ".qmd")
    title    <- if (!is.na(row$post_title)        && row$post_title        != "") row$post_title        else paste("Letter", row$post_id)
    date     <- if (!is.na(row$letter_date)        && row$letter_date        != "") row$letter_date        else ""
    address  <- if (!is.na(row$letter_fromAddress) && row$letter_fromAddress != "") row$letter_fromAddress else ""
    
    index_content <- paste0(index_content, "**[", title, "](", filename, ")**  \n")
    if (date    != "") index_content <- paste0(index_content, "*", date, "*")
    if (address != "") index_content <- paste0(index_content, " • From: ", address)
    index_content <- paste0(index_content, "  \n\n")
  }
}

undated <- letters_df %>% filter(is.na(year))
if (nrow(undated) > 0) {
  index_content <- paste0(index_content, "## Undated\n\n")
  for (i in 1:nrow(undated)) {
    row      <- undated[i, ]
    filename <- paste0(row$post_id, "-", row$post_name, ".qmd")
    title    <- if (!is.na(row$post_title) && row$post_title != "") row$post_title else paste("Letter", row$post_id)
    index_content <- paste0(index_content, "**[", title, "](", filename, ")**  \n\n")
  }
}

writeLines(index_content, file.path(website_dir, "letters", "index.qmd"))
cat(sprintf("✓ Created letters index with %d letters across %d decades\n\n",
            n_letters, length(decades)))

##### PART 8: CREATE PEOPLE INDEX PAGE #####

cat("Part 8: Creating people index page...\n")

person_letter_counts <- person_links %>%
  filter(post_id %in% posts$post_id) %>%
  group_by(persons_id) %>%
  summarise(n_letters = n(), .groups = "drop")

persons_df <- persons %>%
  left_join(person_letter_counts, by = "persons_id") %>%
  mutate(
    n_letters = ifelse(is.na(n_letters), 0, n_letters),
    name = str_squish(paste(
      ifelse(is.na(prefix),      "", prefix),
      ifelse(is.na(first_name),  "", first_name),
      ifelse(is.na(maiden_name), "", maiden_name),
      ifelse(is.na(surname),     "", surname),
      ifelse(is.na(suffix),      "", suffix)
    )),
    name         = ifelse(name == "", paste("Person", persons_id), name),
    letter_group = toupper(substr(ifelse(is.na(surname) | surname == "", name, surname), 1, 1))
  ) %>%
  arrange(surname, first_name)

index_content <- paste0(
  "---\n",
  "title: \"People Index\"\n",
  "---\n\n",
  "# Index of People\n\n",
  "Biographical information about the ", nrow(persons_df), " correspondents and people ",
  "mentioned in the letters. Click any name to see their page and the letters in which they appear.\n\n",
  "---\n\n"
)

for (grp in sort(unique(persons_df$letter_group))) {
  grp_persons   <- persons_df %>% filter(letter_group == grp)
  index_content <- paste0(index_content, "## ", grp, "\n\n")
  
  for (i in 1:nrow(grp_persons)) {
    row         <- grp_persons[i, ]
    filename    <- paste0("person_", row$persons_id, ".qmd")
    n           <- row$n_letters
    letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
    
    index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
    if (!is.na(row$dates) && row$dates != "") index_content <- paste0(index_content, " (", row$dates, ")")
    if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
    index_content <- paste0(index_content, "  \n\n")
  }
}

writeLines(index_content, file.path(website_dir, "people", "index.qmd"))
cat(sprintf("✓ Created people index with %d people\n\n", nrow(persons_df)))

##### PART 9: CREATE ORGANIZATIONS INDEX PAGE #####

cat("Part 9: Creating organizations index page...\n")

org_letter_counts <- other_links %>%
  filter(post_id %in% posts$post_id) %>%
  group_by(others_id) %>%
  summarise(n_letters = n(), .groups = "drop")

others_df <- others %>%
  left_join(org_letter_counts, by = "others_id") %>%
  mutate(n_letters = ifelse(is.na(n_letters), 0, n_letters)) %>%
  arrange(name)

has_types <- !all(is.na(others_df$type) | others_df$type == "")

index_content <- paste0(
  "---\n",
  "title: \"Organizations Index\"\n",
  "---\n\n",
  "# Organizations, Places, and Topics\n\n",
  "This section covers the ", nrow(others_df), " churches, institutions, publications, ",
  "and other entities referenced in the letters.\n\n",
  "---\n\n"
)

if (has_types) {
  types <- sort(unique(others_df$type[!is.na(others_df$type) & others_df$type != ""]))
  for (typ in types) {
    type_orgs     <- others_df %>% filter(type == typ) %>% arrange(name)
    index_content <- paste0(index_content, "## ", typ, "\n\n")
    for (i in 1:nrow(type_orgs)) {
      row         <- type_orgs[i, ]
      filename    <- paste0("other_", row$others_id, ".qmd")
      n           <- row$n_letters
      letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
  untyped <- others_df %>% filter(is.na(type) | type == "") %>% arrange(name)
  if (nrow(untyped) > 0) {
    index_content <- paste0(index_content, "## Other\n\n")
    for (i in 1:nrow(untyped)) {
      row         <- untyped[i, ]
      filename    <- paste0("other_", row$others_id, ".qmd")
      n           <- row$n_letters
      letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
} else {
  for (i in 1:nrow(others_df)) {
    row         <- others_df[i, ]
    filename    <- paste0("other_", row$others_id, ".qmd")
    n           <- row$n_letters
    letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
    index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
    if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
    index_content <- paste0(index_content, "  \n\n")
  }
}

writeLines(index_content, file.path(website_dir, "organizations", "index.qmd"))
cat(sprintf("✓ Created organizations index with %d entries\n\n", nrow(others_df)))

##### PART 10: CREATE CMY BIBLIOGRAPHY INDEX PAGE #####

cat("Part 10: Creating CMY bibliography index page...\n")

cmy_letter_counts <- cmy_links %>%
  filter(post_ID %in% posts$post_id) %>%
  group_by(cmy_bookID) %>%
  summarise(n_letters = n(), .groups = "drop")

cmy_df <- cmy_bib %>%
  left_join(cmy_letter_counts, by = "cmy_bookID") %>%
  mutate(n_letters = ifelse(is.na(n_letters), 0, n_letters)) %>%
  arrange(date, title)

has_genres <- !all(is.na(cmy_df$genre) | cmy_df$genre == "")

index_content <- paste0(
  "---\n",
  "title: \"Works by Charlotte Mary Yonge\"\n",
  "---\n\n",
  "# Bibliography of CMY's Works\n\n",
  "The ", nrow(cmy_df), " works by Charlotte Mary Yonge referenced in the correspondence, ",
  "arranged chronologically.\n\n",
  "---\n\n"
)

if (has_genres) {
  genres <- sort(unique(cmy_df$genre[!is.na(cmy_df$genre) & cmy_df$genre != ""]))
  for (gen in genres) {
    gen_books     <- cmy_df %>% filter(genre == gen) %>% arrange(date, title)
    index_content <- paste0(index_content, "## ", gen, "\n\n")
    for (i in 1:nrow(gen_books)) {
      row         <- gen_books[i, ]
      filename    <- paste0("cmybook_", row$cmy_bookID, ".qmd")
      n           <- row$n_letters
      letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$title, "](", filename, ")**")
      if (!is.na(row$date) && row$date != "") index_content <- paste0(index_content, " (", row$date, ")")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
  ungenred <- cmy_df %>% filter(is.na(genre) | genre == "") %>% arrange(date, title)
  if (nrow(ungenred) > 0) {
    index_content <- paste0(index_content, "## Other\n\n")
    for (i in 1:nrow(ungenred)) {
      row         <- ungenred[i, ]
      filename    <- paste0("cmybook_", row$cmy_bookID, ".qmd")
      n           <- row$n_letters
      letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$title, "](", filename, ")**")
      if (!is.na(row$date) && row$date != "") index_content <- paste0(index_content, " (", row$date, ")")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
} else {
  for (i in 1:nrow(cmy_df)) {
    row         <- cmy_df[i, ]
    filename    <- paste0("cmybook_", row$cmy_bookID, ".qmd")
    n           <- row$n_letters
    letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
    index_content <- paste0(index_content, "**[", row$title, "](", filename, ")**")
    if (!is.na(row$date) && row$date != "") index_content <- paste0(index_content, " (", row$date, ")")
    if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
    index_content <- paste0(index_content, "  \n\n")
  }
}

writeLines(index_content, file.path(website_dir, "cmy_books", "index.qmd"))
cat(sprintf("✓ Created CMY bibliography index with %d works\n\n", nrow(cmy_df)))

##### PART 11: CREATE GENERAL BIBLIOGRAPHY INDEX PAGE #####

cat("Part 11: Creating general bibliography index page...\n")

gen_letter_counts <- gen_links %>%
  filter(post_id %in% posts$post_id) %>%
  group_by(gen_bookID) %>%
  summarise(n_letters = n(), .groups = "drop")

gen_df <- gen_bib %>%
  left_join(gen_letter_counts, by = c("general_bookID" = "gen_bookID")) %>%
  mutate(
    n_letters = ifelse(is.na(n_letters), 0, n_letters),
    author = str_squish(paste(
      ifelse(is.na(author_prefix),     "", author_prefix),
      ifelse(is.na(author_first_name), "", author_first_name),
      ifelse(is.na(author_surname),    "", author_surname),
      ifelse(is.na(author_suffix),     "", author_suffix)
    )),
    letter_group = toupper(substr(
      ifelse(is.na(author_surname) | author_surname == "", title, author_surname), 1, 1))
  ) %>%
  arrange(author_surname, author_first_name, title)

index_content <- paste0(
  "---\n",
  "title: \"General Bibliography\"\n",
  "---\n\n",
  "# Other Works Referenced\n\n",
  "The ", nrow(gen_df), " books, articles, and publications by other authors ",
  "mentioned in the letters, arranged alphabetically by author.\n\n",
  "---\n\n"
)

for (grp in sort(unique(gen_df$letter_group))) {
  grp_books     <- gen_df %>% filter(letter_group == grp)
  index_content <- paste0(index_content, "## ", grp, "\n\n")
  for (i in 1:nrow(grp_books)) {
    row         <- grp_books[i, ]
    filename    <- paste0("otherbook_", row$general_bookID, ".qmd")
    n           <- row$n_letters
    letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
    display     <- if (!is.na(row$title) && row$title != "") row$title else paste("Book", row$general_bookID)
    if (row$author != "") display <- paste0(row$author, ", *", display, "*")
    index_content <- paste0(index_content, "**[", display, "](", filename, ")**")
    if (!is.na(row$date) && row$date != "") index_content <- paste0(index_content, " (", row$date, ")")
    if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
    index_content <- paste0(index_content, "  \n\n")
  }
}

writeLines(index_content, file.path(website_dir, "other_books", "index.qmd"))
cat(sprintf("✓ Created general bibliography index with %d works\n\n", nrow(gen_df)))

##### SUMMARY #####

cat("✅ Website structure complete!\n\n")
cat("Summary:\n")
cat("  Letters:",       letters_processed, "\n")
cat("  People:",        people_processed,  "\n")
cat("  Organizations:", org_processed,     "\n")
cat("  CMY Books:",     cmy_processed,     "\n")
cat("  Other Books:",   book_processed,    "\n")
cat("  Total files:",   letters_processed + people_processed + org_processed +
      cmy_processed + book_processed, "\n\n")
cat("Website location:", website_dir, "\n\n")
cat("Next step: Render the website\n")
cat("1. Open terminal/command prompt\n")
cat("2. cd", website_dir, "\n")
cat("3. quarto render\n")
cat("4. Website will be built in _site/ folder\n")