# generate_decade_includes.R
# Generates _DECADE_letters.md include files for each decade page
# Run this whenever letters are added; output goes into decades/ folder

library(dplyr)
library(readr)

# ── Load data ────────────────────────────────────────────────────────────────

letterinfo <- read_csv("C:/db/wp_letterinfo.csv",
                       col_types = cols(letter_dbdate = col_character()))
posts <- read_csv("C:/db/wp_posts.csv")

out_dir <- "C:/db/cmy_letters_website_Apr/decades"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ── Date formatting: show only as much as is known ───────────────────────────

format_date <- function(d) {
  dplyr::case_when(
    is.na(d)                    ~ "",
    substr(d, 6, 10) == "00-00" ~ substr(d, 1, 4),        # year only
    substr(d, 9, 10) == "00"    ~ substr(d, 1, 7),        # year-month only
    TRUE                        ~ d                        # full date
  )
}

# ── Join and clean ───────────────────────────────────────────────────────────

letters <- letterinfo |>
  inner_join(
    posts |> filter(post_type == "post") |>
      select(post_id, post_name, post_title),
    by = c("post_ID" = "post_id")
  ) |>
  mutate(
    dbdate_clean = if_else(
      is.na(letter_dbdate) | letter_dbdate == "0000-00-00" | letter_dbdate == "",
      NA_character_,
      letter_dbdate
    ),
    year  = as.integer(substr(dbdate_clean, 1, 4)),
    decade = if_else(!is.na(year), paste0(floor(year / 10) * 10, "s"), NA_character_),
    display_date = format_date(dbdate_clean),
    link = sprintf("[%s](../letters/%s-%s.html)", post_title, post_ID, post_name)
  ) |>
  arrange(dbdate_clean)

# ── Helper: write one include file ───────────────────────────────────────────

write_include <- function(df, filename, date_col = TRUE) {
  if (nrow(df) == 0) return(invisible(NULL))
  
  if (date_col) {
    lines <- c(
      "| Date | Letter |",
      "|------|--------|",
      sprintf("| %s | %s |", df$display_date, df$link)
    )
  } else {
    lines <- c(
      "| Letter |",
      "|--------|",
      sprintf("| %s |", df$link)
    )
  }
  
  writeLines(lines, file.path(out_dir, filename))
  message("Written: ", filename, " (", nrow(df), " letters)")
}

# ── Write one file per decade ─────────────────────────────────────────────────

dated  <- letters |> filter(!is.na(decade))
decades <- sort(unique(dated$decade))

for (d in decades) {
  write_include(
    filter(dated, decade == d),
    paste0("_", d, "_letters.md")
  )
}

# ── Write undated include ─────────────────────────────────────────────────────

write_include(
  filter(letters, is.na(decade)),
  "_undated_letters.md",
  date_col = FALSE
)

message("Done. Decades found: ", paste(decades, collapse = ", "))