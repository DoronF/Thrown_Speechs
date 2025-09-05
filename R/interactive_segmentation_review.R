# run_interactive_review_color_FIXED.R
#
# An interactive tool to review and correct LLM-based speech segmentations.
# This version fixes the bug where the 'quit' command did not exit the session.

# --- 1. SETUP: Load Libraries and Data ---

# Check if crayon is installed, prompt if not
if (!require("crayon", quietly = TRUE)) {
  install.packages("crayon")
}

suppressPackageStartupMessages({
  library(crayon)
  library(readr)
  library(dplyr)
  library(stringr)
})

# Define file paths
detailed_segmentation_file <- "output/segmentation/llm_detailed_segmentation.csv"
issues_file <- "output/segmentation/llm_issues.csv"

# Check if the essential detailed segmentation file exists
if (!file.exists(detailed_segmentation_file)) {
  stop(
    red("FATAL ERROR:"),
    " The detailed segmentation file was not found at:\n  ",
    cyan(detailed_segmentation_file),
    "\nThis file is required for the tool to run. Please ensure you have run the ",
    "segmentation script first."
  )
}

# Load the data
cat(yellow("-> Loading data files...\n"))
issues_df <- read_csv(issues_file, show_col_types = FALSE)
full_data <- read_csv(detailed_segmentation_file, show_col_types = FALSE)

# Create a working copy of the data
corrected_data <- full_data

# --- 2. REGENERATION FUNCTION (Unchanged logic) ---
generate_outputs <- function(segmented_data) {
  cat(yellow("-> Regenerating output files...\n"))

  # ... (The logic inside this function is identical to the previous script)
  speech_summaries <- segmented_data %>%
    group_by(filename, date, year, parliament) %>%
    summarise(
      total_sentences = n(),
      opening_sentences = sum(section == "opening_ceremonial"),
      policy_sentences = sum(section == "policy_content"),
      closing_sentences = sum(section == "closing_ceremonial"),
      unclassified_sentences = sum(section == "unclassified"),
      .groups = "drop"
    ) %>%
    mutate(
      opening_pct = round(100 * opening_sentences / total_sentences, 1),
      policy_pct = round(100 * policy_sentences / total_sentences, 1),
      closing_pct = round(100 * closing_sentences / total_sentences, 1),
      unclassified_pct = round(
        100 * unclassified_sentences / total_sentences,
        1
      ),
      era = case_when(
        year <= 1920 ~ "Early Confederation",
        year <= 1960 ~ "Mid-20th Century",
        year <= 1990 ~ "Late 20th Century",
        TRUE ~ "Contemporary"
      )
    )
  clean_corpus <- segmented_data %>%
    group_by(filename, date, year, parliament) %>%
    summarise(
      opening_ceremonial = paste(
        sentence[section == "opening_ceremonial"],
        collapse = " "
      ),
      policy_content = paste(
        sentence[section == "policy_content"],
        collapse = " "
      ),
      closing_ceremonial = paste(
        sentence[section == "closing_ceremonial"],
        collapse = " "
      ),
      opening_sentence_count = sum(section == "opening_ceremonial"),
      policy_sentence_count = sum(section == "policy_content"),
      closing_sentence_count = sum(section == "closing_ceremonial"),
      unclassified_sentences = n() -
        (opening_sentence_count +
          policy_sentence_count +
          closing_sentence_count),
      .groups = "drop"
    )
  issues <- speech_summaries %>%
    mutate(
      issue = case_when(
        policy_pct < 40 ~ "Low policy content",
        opening_pct > 40 ~ "Excessive opening",
        closing_pct > 40 ~ "Excessive closing",
        policy_sentences < 10 ~ "Very few policy sentences",
        unclassified_pct > 5 ~ "High unclassified content",
        TRUE ~ "OK"
      )
    ) %>%
    filter(issue != "OK")

  cat(yellow("-> Writing new summary files to disk...\n"))
  write_csv(speech_summaries, "output/segmentation/final_speech_summaries.csv")
  write_csv(
    clean_corpus,
    "output/segmentation/final_clean_segmented_corpus.csv"
  )
  write_csv(issues, "output/segmentation/final_issues.csv")

  cat(
    green("-> Regeneration complete."),
    "Found",
    red(bold(nrow(issues))),
    "remaining issues.\n"
  )
}


# --- 3. HELPER FUNCTIONS FOR THE INTERACTIVE SESSION (with Color) ---
section_color <- list(
  opening_ceremonial = magenta,
  policy_content = green,
  closing_ceremonial = magenta,
  unclassified = red,
  default = white
)
display_speech <- function(fname, data_source) {
  speech_data <- data_source %>%
    filter(filename == fname) %>%
    arrange(sentence_id)
  cat(blue("\n--- Current Segmentation ---\n"))
  for (i in 1:nrow(speech_data)) {
    row <- speech_data[i, ]
    padded_id <- str_pad(row$sentence_id, 3, "left", " ")
    section_abbr <- toupper(substr(row$section, 1, 4))
    color_func <- section_color[[row$section]] %||% section_color$default
    cat(
      white(sprintf("[ %s | ", padded_id)),
      color_func(section_abbr),
      white(" ] "),
      row$sentence,
      "\n",
      sep = ""
    )
  }
  cat(blue("---------------------------\n"))
}
display_summary <- function(fname, issue_reason, data_source) {
  summary_df <- data_source %>%
    filter(filename == fname) %>%
    group_by(section) %>%
    summarise(count = n(), .groups = "drop")
  cat(blue("\n======================================================\n"))
  cat(bold("Reviewing:"), fname, "\n")
  cat(bold("Reason:"), yellow(issue_reason), "\n")
  cat(bold("Current Counts:\n"))
  for (i in 1:nrow(summary_df)) {
    row <- summary_df[i, ]
    color_func <- section_color[[row$section]] %||% section_color$default
    cat("  - ", color_func(sprintf("%-20s: %d\n", row$section, row$count)))
  }
  cat(blue("======================================================\n"))
}


# --- 4. MAIN INTERACTIVE LOOP (with Color) ---

cat(
  yellow("\nStarting interactive review session for"),
  bold(nrow(issues_df)),
  yellow("speeches.\n")
)

# *** FIX START ***
# Add a flag to control quitting the main loop
quit_session <- FALSE
# *** FIX END ***

for (i in 1:nrow(issues_df)) {
  current_filename <- issues_df$filename[i]
  current_issue <- issues_df$issue[i]

  interactive_mode <- TRUE
  while (interactive_mode) {
    display_summary(current_filename, current_issue, corrected_data)
    cat(
      "\n",
      cyan(bold("COMMANDS:")),
      cyan("[c]hange | [v]iew full text | [d]one | [s]kip | [q]uit\n")
    )
    command <- tolower(readline(prompt = yellow("Enter command > ")))

    if (command %in% c("c", "change")) {
      # (This block is unchanged)
      cat(yellow(
        "-> Enter new section (opening, policy, closing, unclassified): \n"
      ))
      new_section <- tolower(readline(prompt = "> "))
      if (new_section %in% c("opening", "policy", "closing", "unclassified")) {
        new_section_label <- paste0(
          new_section,
          ifelse(
            new_section %in% c("opening", "closing"),
            "_ceremonial",
            ifelse(new_section == "policy", "_content", "")
          )
        )
        cat(yellow("-> Enter sentence range (e.g., '15-22' or '30'): \n"))
        range_input <- readline(prompt = "> ")
        range_vals <- as.numeric(unlist(str_split(range_input, "-")))
        start_id <- range_vals[1]
        end_id <- ifelse(length(range_vals) > 1, range_vals[2], start_id)
        if (!is.na(start_id) && !is.na(end_id)) {
          corrected_data <- corrected_data %>%
            mutate(
              section = if_else(
                filename == current_filename &
                  sentence_id >= start_id &
                  sentence_id <= end_id,
                new_section_label,
                section
              )
            )
          cat(
            green("-> SUCCESS:"),
            "Sentences",
            bold(start_id),
            "to",
            bold(end_id),
            "changed to",
            bold(new_section),
            "\n"
          )
        } else {
          cat(red("-> ERROR:"), "Invalid range provided.\n")
        }
      } else {
        cat(red("-> ERROR:"), "Invalid section name.\n")
      }
    } else if (command %in% c("v", "view")) {
      display_speech(current_filename, corrected_data)
    } else if (command %in% c("d", "done")) {
      cat(
        green("-> Finished with"),
        bold(current_filename),
        green(". Moving to next speech.\n")
      )
      interactive_mode <- FALSE
    } else if (command %in% c("s", "skip")) {
      cat(
        yellow("-> Skipping"),
        bold(current_filename),
        yellow(". No changes for this file will be kept.\n")
      )
      corrected_data <- corrected_data %>%
        filter(filename != current_filename) %>%
        bind_rows(full_data %>% filter(filename == current_filename))
      interactive_mode <- FALSE
    } else if (command %in% c("q", "quit")) {
      cat(red("-> Quitting interactive session.\n"))
      # *** FIX START ***
      # Set the flags to exit both loops
      quit_session <- TRUE
      interactive_mode <- FALSE
      # *** FIX END ***
    } else {
      cat(red("-> Unknown command.\n"))
    }
  }

  # *** FIX START ***
  # After the inner while-loop, check if we need to quit the main for-loop
  if (quit_session) {
    break
  }
  # *** FIX END ***
}

# --- 5. SAVE CHANGES AND REGENERATE FILES (with Color) ---

# Only ask to save if the session wasn't quit prematurely
if (!quit_session) {
  cat(blue("\n--- Session Complete ---\n"))
  save_changes <- tolower(readline(
    prompt = yellow("Save all changes to disk? [y/n] > ")
  ))

  if (save_changes == "y") {
    backup_filename <- paste0(
      detailed_segmentation_file,
      ".bak.",
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
    cat(yellow("-> Backing up original data to:"), cyan(backup_filename), "\n")
    file.copy(detailed_segmentation_file, backup_filename)
    cat(yellow("-> Writing corrected segmentation data to disk...\n"))
    write_csv(corrected_data, detailed_segmentation_file)
    generate_outputs(corrected_data)
    cat(green(bold(
      "\nSUCCESS: All files have been updated with your corrections.\n"
    )))
  } else {
    cat(red("\nChanges discarded. No files were modified.\n"))
  }
} else {
  cat(yellow("\nSession was quit. No changes have been saved.\n"))
}
