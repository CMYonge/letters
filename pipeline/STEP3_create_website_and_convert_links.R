# STEP3_create_website_and_convert_links_May26.R
# Creates website structure, copies files, converts [[tags]] to links,
# and injects Zotero-compatible citation metadata into letter pages.
#
# Changes from Apr26:
#   - letterinfo + posts loaded in Part 3 so Part 5 can use them for citations
#   - make_citation_yaml() and inject_citation_yaml() functions added
#   - Part 5 letter loop injects citation: YAML block per letter
#   - _quarto.yml: added title-block-style: plain
#   - styles.css: .quarto-title-meta already hidden; added commented-out
#     .quarto-appendix rule (uncomment to hide "How to cite" section)
#   - site_base_url: update when domain goes live

library(dplyr)
library(readr)
library(stringr)

##### CONFIGURATION #####

data_dir    <- "C:/db"
derived_dir <- "C:/db"
text_dir <- "C:/db/letters/data"   # hand-written prose, version controlled
letter_source <- file.path(derived_dir, "all_letters_qmd_Apr26")
ref_source    <- file.path(derived_dir, "reference_pages_qmd_Apr26")
website_dir   <- file.path(derived_dir, "cmy_letters_website_Apr")


# Update this when charlottemyonge.org.uk goes live
site_base_url <- "https://cmyonge.github.io/letters"

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

##### FUNCTION: MAKE CITATION YAML BLOCK #####
# Builds a Quarto citation: block for a letter page.
# Quarto converts this to Highwire Press + Dublin Core meta tags,
# which Zotero and other reference managers can import directly.

make_citation_yaml <- function(post_id, post_title, iso_date, post_name) {
  
  safe_title <- gsub('"', "'", post_title, fixed = TRUE)
  safe_title <- trimws(safe_title)
  
  issued_line <- if (!is.na(iso_date) && nchar(iso_date) >= 4) {
    paste0('  issued: "', iso_date, '"')
  } else {
    NULL
  }
  
  url_line <- paste0(
    '  url: "', site_base_url, '/',
    post_id, '-', post_name, '"'
  )
  
  lines <- c(
    "citation:",
    "  type: personal_communication",
    "  author:",
    "    - family: Yonge",
    "      given: Charlotte Mary",
    "  editor:",
    "    - family: Mitchell",
    "      given: Charlotte",
    "    - family: Jordan",
    "      given: Ellen",
    "    - family: Schinske",
    "      given: Helen",
    paste0('  title: "', safe_title, '"'),
    '  container-title: "The Letters of Charlotte Mary Yonge"',
    issued_line,
    url_line
  )
  
  paste(lines[!sapply(lines, is.null)], collapse = "\n")
}

##### FUNCTION: CLEAN TITLE FOR SORTING #####
# Strips leading <i>/<em> tags and leading quote characters so titles
# sort correctly alphabetically (raw HTML/quote marks otherwise sort
# before letters, producing spurious "<" and "'" groups in indexes).

clean_sort_title <- function(title) {
  title <- ifelse(is.na(title), "", title)
  title <- str_remove(title, "^<i>|^<em>")
  title <- str_remove(title, "^['\"\u2018\u2019\u201c\u201d]+")
  str_squish(title)
}

##### FUNCTION: INJECT CITATION YAML INTO FRONT MATTER #####
# Inserts citation block inside existing YAML front matter,
# before the closing ---, leaving all other content unchanged.

inject_citation_yaml <- function(content_text, citation_yaml) {
  
  lines     <- strsplit(content_text, "\n", fixed = TRUE)[[1]]
  yaml_ends <- which(lines == "---")
  
  if (length(yaml_ends) < 2) {
    warning("Could not find YAML front matter — citation not injected")
    return(content_text)
  }
  
  close_idx      <- yaml_ends[2]
  before_close   <- lines[1:(close_idx - 1)]
  from_close     <- lines[close_idx:length(lines)]
  citation_lines <- strsplit(citation_yaml, "\n", fixed = TRUE)[[1]]
  
  paste(c(before_close, citation_lines, from_close), collapse = "\n")
}

##### PART 1: CREATE WEBSITE STRUCTURE #####

cat("Part 1: Creating website folders...\n")

dir.create(website_dir,                                    showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(website_dir, "decades"),              showWarnings = FALSE, recursive = TRUE)
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
        href: letters-index.qmd
      - text: "People"
        href: people/index.qmd
      - text: "Organizations"
        href: organizations/index.qmd
      - text: "Decades"
        menu:
          - text: "1834\u20131849"
            href: decades/3733-letters-1834-1849.qmd
          - text: "1850\u20131859"
            href: decades/3735-letters-1850-1859.qmd
          - text: "1860\u20131869"
            href: decades/3737-letters-1860-1869.qmd
          - text: "1870\u20131879"
            href: decades/3740-letters-1870-1879.qmd
          - text: "1880\u20131889"
            href: decades/3750-letters-1880-1889.qmd
          - text: "1890\u20131901"
            href: decades/3752-letters-1890-1901.qmd
      - text: "CMY Bibliography"
        href: cmy_books/index.qmd
      - text: "General Bibliography"
        href: other_books/index.qmd
  search: true

format:
  html:
    theme: cosmo
    css: styles.css
    toc: false
'

writeLines(quarto_config, file.path(website_dir, "_quarto.yml"))

#### Hand-written home page introduction

intro_path <- file.path(text_dir, "home_intro.md")
home_intro <- if (file.exists(intro_path)) {
  paste(readLines(intro_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  warning("home_intro.md not found - home page will have no introduction")
  ""
}

index_content <- paste0('---
title: "Charlotte Mary Yonge Letters Collection"
---

', home_intro, '

## Browse

- **[Letters](letters-index.qmd)** - Browse all letters chronologically
- **[People](people/index.qmd)** - Index of correspondents and people mentioned
- **[Organizations](organizations/index.qmd)** - Churches, institutions, and groups
- **[CMY Bibliography](cmy_books/index.qmd)** - Works by Charlotte Mary Yonge
- **[General Bibliography](other_books/index.qmd)** - Other works referenced

## Search

Use the search box in the navigation bar to find letters, people, or topics.
')

writeLines(index_content, file.path(website_dir, "index.qmd"))

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

/* Hide Quarto auto-generated title metadata (Published date etc.) */
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

/* Citation appendix: plain text, no box */
.quarto-appendix {
  background: none;
  border: none;
  padding: 0;
  margin-top: 2em;
}

/* Note about reference manager import — appears after citation text */
.quarto-appendix::after {
  content: "Citation metadata for this letter is available for automatic import into reference managers including Zotero, Mendeley, and EndNote.";
  display: block;
  font-style: italic;
  margin-top: 0.5em;
  font-size: 0.9em;
}

.quarto-appendix-bibtex,
.quarto-appendix-secondary-label {
  display: none;
}

'

writeLines(css_content, file.path(website_dir, "styles.css"))
cat("✓ Created config files\n\n")

##### PART 3: LOAD LOOKUP TABLES #####
# NB: letterinfo and posts loaded here (not just Part 7) so that
# Part 5 can build citation metadata for each letter.

cat("Part 3: Loading lookup tables...\n")

persons      <- read_csv(file.path(data_dir, "persons.csv"),                      show_col_types = FALSE)
others       <- read_csv(file.path(data_dir, "wp_others.csv"),                    show_col_types = FALSE)
cmy_bib      <- read_csv(file.path(derived_dir, "wp_cmybibliography_with_sort.csv"), show_col_types = FALSE)
gen_bib      <- read_csv(file.path(data_dir, "wp_generalbibliography.csv"),       show_col_types = FALSE)
person_links <- read_csv(file.path(data_dir, "wp_persons_posts.csv"),             show_col_types = FALSE)
other_links  <- read_csv(file.path(data_dir, "wp_others_posts.csv"),              show_col_types = FALSE)
cmy_links    <- read_csv(file.path(data_dir, "wp_cmybibliography_posts.csv"),     show_col_types = FALSE)
gen_links    <- read_csv(file.path(data_dir, "wp_generalbibliography_posts.csv"), show_col_types = FALSE)

# Loaded early for citation injection in Part 5
letterinfo   <- read_csv(file.path(data_dir, "wp_letterinfo.csv"),
                         show_col_types = FALSE,
                         col_types = cols(letter_date = col_character()))
posts        <- read_csv(file.path(data_dir, "wp_posts.csv"), show_col_types = FALSE)

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
  title <- str_replace_all(book$title[1], "<[^>]+>", "")
  return(trimws(title))
}

get_gen_title <- function(id) {
  book <- gen_bib %>% filter(general_bookID == id)
  if (nrow(book) == 0) return(paste("Book", id))
  title <- str_replace_all(book$title[1], "<[^>]+>", "")
  return(trimws(title))
}

cat("✓ Lookup tables loaded\n\n")

##### PART 4: FUNCTION TO CONVERT TAGS TO LINKS #####

convert_tags_and_fix_yaml <- function(filepath, file_location) {
  
  content      <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  content_text <- paste(content, collapse = "\n")
  
  if (file_location == "letters") {
    person_path <- "people/person_"
    org_path    <- "organizations/other_"
    cmy_path    <- "cmy_books/cmybook_"
    book_path   <- "other_books/otherbook_"
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
  
  content_text <- gsub("\\'", "'", content_text, fixed = TRUE)
  return(content_text)
}

##### PART 5: COPY AND CONVERT LETTERS #####
# For each letter: convert [[tags]] to links, then inject citation YAML.

cat("Part 5: Processing letters...\n")

letter_files      <- list.files(letter_source, pattern = "\\.qmd$", full.names = TRUE)
letters_processed <- 0
citation_injected <- 0

for (letter_file in letter_files) {
  
  converted_content <- convert_tags_and_fix_yaml(letter_file, "letters")
  
  # Inject citation metadata
  fname   <- basename(letter_file)
  post_id <- suppressWarnings(as.numeric(str_extract(fname, "^\\d+")))
  
  if (!is.na(post_id)) {
    post_row <- posts      %>% filter(post_id == !!post_id)
    info_row <- letterinfo %>% filter(post_ID == !!post_id)
    
    if (nrow(post_row) > 0) {
      post_title <- if (!is.na(post_row$post_title[1])) post_row$post_title[1] else paste("Letter", post_id)
      post_name  <- post_row$post_name[1]
      raw_date   <- if (nrow(info_row) > 0) info_row$letter_date[1] else NA_character_
      iso_date   <- parse_iso_date(raw_date)
      
      citation_yaml     <- make_citation_yaml(post_id, post_title, iso_date, post_name)
      converted_content <- inject_citation_yaml(converted_content, citation_yaml)
      citation_injected <- citation_injected + 1
    }
  }
  
  output_file <- file.path(website_dir, basename(letter_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
  letters_processed <- letters_processed + 1
}

cat("✓ Processed", letters_processed, "letters\n")
cat("  Citation metadata injected:", citation_injected, "\n\n")

# Copy and convert decade introduction pages
decade_files <- list.files(file.path(letter_source, "decades"),
                           pattern = "\\.qmd$", full.names = TRUE)
for (decade_file in decade_files) {
  converted_content <- convert_tags_and_fix_yaml(decade_file, "letters")
  output_file       <- file.path(website_dir, "decades", basename(decade_file))
  writeLines(converted_content, output_file, useBytes = TRUE)
}
cat(sprintf("✓ Copied %d decade pages\n\n", length(decade_files)))

##### PART 6: COPY AND CONVERT REFERENCE PAGES #####

cat("Part 6: Processing reference pages...\n")

people_files     <- list.files(file.path(ref_source, "people"),        pattern = "\\.qmd$", full.names = TRUE)
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

# NB: letterinfo and posts already loaded in Part 3

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

# Map floor-decade to intro page filename and display label.
# 1830 and 1840 both point to the 1834-1849 intro page.
decade_intro_map <- list(
  "1830" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1840" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1850" = list(file = "decades/3735-letters-1850-1859.qmd", label = "1850\u20131859"),
  "1860" = list(file = "decades/3737-letters-1860-1869.qmd", label = "1860\u20131869"),
  "1870" = list(file = "decades/3740-letters-1870-1879.qmd", label = "1870\u20131879"),
  "1880" = list(file = "decades/3750-letters-1880-1889.qmd", label = "1880\u20131889"),
  "1890" = list(file = "decades/3752-letters-1890-1901.qmd", label = "1890\u20131901")
)

for (dec in decades) {
  decade_letters <- letters_df %>% filter(decade == dec) %>% arrange(iso_date)
  index_content  <- paste0(index_content, "## ", dec, "s\n\n")
  
  # Link to decade intro page if one exists for this floor-decade
  intro <- decade_intro_map[[as.character(dec)]]
  if (!is.null(intro)) {
    index_content <- paste0(index_content,
                            "*[Introduction to letters ", intro$label, "](", intro$file, ")*\n\n")
  }
  
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

writeLines(index_content, file.path(website_dir, "letters-index.qmd"))
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
      row           <- type_orgs[i, ]
      filename      <- paste0("other_", row$others_id, ".qmd")
      n             <- row$n_letters
      letter_text   <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
  untyped <- others_df %>% filter(is.na(type) | type == "") %>% arrange(name)
  if (nrow(untyped) > 0) {
    index_content <- paste0(index_content, "## Other\n\n")
    for (i in 1:nrow(untyped)) {
      row           <- untyped[i, ]
      filename      <- paste0("other_", row$others_id, ".qmd")
      n             <- row$n_letters
      letter_text   <- if (n == 1) "1 letter" else paste(n, "letters")
      index_content <- paste0(index_content, "**[", row$name, "](", filename, ")**")
      if (n > 0) index_content <- paste0(index_content, " — ", letter_text)
      index_content <- paste0(index_content, "  \n\n")
    }
  }
} else {
  for (i in 1:nrow(others_df)) {
    row           <- others_df[i, ]
    filename      <- paste0("other_", row$others_id, ".qmd")
    n             <- row$n_letters
    letter_text   <- if (n == 1) "1 letter" else paste(n, "letters")
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

cmy_df <- cmy_df %>%
  mutate(category = str_squish(category),
         category = ifelse(is.na(category) | category == "", NA_character_, category),
         sort_year = as.integer(str_extract(date, "\\b(18|19)[0-9]{2}\\b")))

#### Display order - edit to taste

category_order <- c("Fiction",
                    "Non-fiction",
                    "Editorial work and prefaces",
                    "Literary criticism",
                    "Miscellaneous")

unlisted <- setdiff(na.omit(unique(cmy_df$category)), category_order)
if (length(unlisted) > 0) {
  warning("Categories not in category_order: ", paste(unlisted, collapse = ", "))
}
cats <- c(category_order[category_order %in% cmy_df$category], sort(unlisted))

index_content <- paste0(
  "---\n",
  "title: \"Works by Charlotte Mary Yonge\"\n",
  "---\n\n",
  "# Bibliography of CMY's Works\n\n",
  "The ", nrow(cmy_df), " works by Charlotte Mary Yonge referenced in the correspondence, ",
  "arranged by category and then chronologically.\n\n",
  "---\n\n"
)

add_entries <- function(txt, df) {
  for (i in seq_len(nrow(df))) {
    row         <- df[i, ]
    filename    <- paste0("cmybook_", row$cmy_bookID, ".qmd")
    n           <- row$n_letters
    letter_text <- if (n == 1) "1 letter" else paste(n, "letters")
    txt <- paste0(txt, "**[", row$display_title, "](", filename, ")**")
    if (!is.na(row$date) && row$date != "") txt <- paste0(txt, " (", row$date, ")")
    if (n > 0) txt <- paste0(txt, " — ", letter_text)
    txt <- paste0(txt, "  \n\n")
  }
  txt
}

for (ct in cats) {
  index_content <- paste0(index_content, "## ", ct, "\n\n")
  index_content <- add_entries(index_content,
                               cmy_df %>% filter(category == ct) %>% arrange(sort_year, date, sort_title))
}

uncategorised <- cmy_df %>% filter(is.na(category)) %>% arrange(sort_year, date, sort_title)
if (nrow(uncategorised) > 0) {
  index_content <- paste0(index_content, "## Other\n\n")
  index_content <- add_entries(index_content, uncategorised)
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
    sort_title = clean_sort_title(title),
    letter_group = toupper(substr(
      ifelse(is.na(author_surname) | author_surname == "", sort_title, author_surname), 1, 1))
  ) %>%
  arrange(author_surname, author_first_name, sort_title)


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
    row           <- grp_books[i, ]
    filename      <- paste0("otherbook_", row$general_bookID, ".qmd")
    n             <- row$n_letters
    letter_text   <- if (n == 1) "1 letter" else paste(n, "letters")
    display       <- if (!is.na(row$title) && row$title != "") row$title else paste("Book", row$general_bookID)
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
cat("  Letters:",       letters_processed, "(citations injected:", citation_injected, ")\n")
cat("  People:",        people_processed,  "\n")
cat("  Organizations:", org_processed,     "\n")
cat("  CMY Books:",     cmy_processed,     "\n")
cat("  Other Books:",   book_processed,    "\n")
cat("  Total files:",   letters_processed + people_processed + org_processed +
      cmy_processed + book_processed, "\n\n")
cat("Website location:", website_dir, "\n\n")
cat("Next steps:\n")
cat("1. Run process_decade_pages.R to add letter lists to decade pages\n")
cat("2. quarto render in", website_dir, "\n")
cat("3. Update site_base_url at top of this script when domain goes live\n")