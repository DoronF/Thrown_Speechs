library(tidytext)

#  Logging

log_message <- function(message, log_file) {
  log_dir <- "output/logs/"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  log_path <- file.path(paste0(log_dir, log_file))

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste0("[", timestamp, "] ", message)
  cat(msg, "\n")
  write_lines(msg, file = log_path, append = TRUE)
}

# custom list of words
custom_words <- tibble(
  word = c(
    "act",
    "also",
    "new",
    "year",
    "years",
    "work",
    "measures",
    "asked",
    "people",
    "country",
    "provinces",
    "national",
    "make",
    "good",
    "continue",
    "provide",
    "increase",
    "great",
    "ensure",
    "need",
    "made",
    "well",
    "must",
    "take",
    "canada",
    "canada's",
    "canadians",
    "canadian",
    "s",
    "government",
    "nation",
    "dominion",
    "bill",
    "policy",
    "minister"
  ),
  lexicon = "CUSTOM"
)

get_project_stopwords <- function() {
  # The standard stop word list from tidytext
  standard_stopwords <- tidytext::get_stopwords()

  bind_rows(standard_stopwords, custom_words)
}


generate_filename <- function(date, parliament) {
  filename <- paste0(date, "_P", parliament, ".txt")
  return(filename)
}

setup_speech_directories <- function(base_dir = "throne_speeches") {
  dir.create(base_dir, showWarnings = FALSE)
  return(base_dir)
}

clean_speech_text <- function(text) {
  # Fix words hyphenated across line breaks (e.g., "Parlia- ment")
  text <- str_replace_all(text, "([A-Za-z]+)-\\s+([A-Za-z]+)", "\\1\\2")

  #  Expand common ligatures from OCR
  text <- str_replace_all(text, "\uFB01", "fi") # ﬁ -> fi
  text <- str_replace_all(text, "\uFB02", "fl") # ﬂ -> fl
  text <- str_replace_all(text, "\u00E6", "ae") # æ -> ae
  text <- str_replace_all(text, "\u0153", "oe") # œ -> oe

  # Normalize whitespace
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_replace_all(text, "\\n+", " ")
  text <- str_replace_all(text, "\\t", " ")
  text <- str_replace_all(text, "\\r", " ")

  # Standardize quotes and dashes
  text <- str_replace_all(text, '[""\u2018\u2019`]', "'")
  text <- str_replace_all(text, '[\u2013\u2014]', "-")

  # Remove any remaining strange characters
  text <- str_replace_all(text, "[^\\w\\s.,;:!?'\"()\\-]", " ")

  # Final whitespace cleanup
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)

  return(text)
}

save_speech <- function(
  speech_text,
  date,
  parliament,
  base_dir = "output/throne_speeches_txt"
) {
  speech_text <- clean_speech_text(speech_text)

  filename <- generate_filename(date, parliament)
  filepath <- file.path(base_dir, filename)

  header <- paste0(
    "# THRONE SPEECH METADATA\n",
    "# Date: ",
    date,
    "\n",
    "# Parliament: ",
    parliament,
    "\n",
    "# File created: ",
    Sys.Date(),
    "\n",
    "# =====================================\n\n"
  )

  full_content <- paste0(header, speech_text)

  writeLines(full_content, filepath, useBytes = TRUE)

  cat("Saved:", filepath, "\n")
  cat("Characters:", nchar(speech_text), "\n")

  create_speech_index(base_dir)

  return(filepath)
}

create_speech_index <- function(base_dir = "output/throne_speeches_txt/") {
  speeches <- load_all_speeches(base_dir)

  index <- speeches %>%
    select(filename, date, parliament, text_length) %>%
    mutate(
      year = substr(date, 1, 4),
      decade = paste0(substr(year, 1, 3), "0s"),
      word_count = round(text_length / 5.1),
      file_creation_date = map_chr(filename, function(fname) {
        filepath <- file.path(base_dir, fname)
        content <- read_lines(filepath)
        creation_line <- content[grepl("File created:", content)]
        if (length(creation_line) > 0) {
          str_extract(creation_line, "\\d{4}-\\d{2}-\\d{2}")
        } else {
          as.character(Sys.Date())
        }
      })
    ) %>%
    arrange(date)

  index_path <- file.path(base_dir, "speech_index.csv")
  write_csv(index, index_path)

  cat("Created index with", nrow(index), "speeches\n")
  cat("Saved to:", index_path, "\n")

  return(index)
}
load_all_speeches <- function(
  base_dir = "output/throne_speeches_txt/",
  log_file
) {
  log_message("Scanning directory for .txt files", log_file)
  txt_files <- list.files(base_dir, pattern = "\\.txt$", full.names = TRUE)
  log_message(paste("Found", length(txt_files), "text files"), log_file)

  speeches <- map_dfr(txt_files, function(filepath) {
    content <- tryCatch(
      {
        read_lines(filepath)
      },
      error = function(e) {
        log_message(
          paste(
            "Error reading file",
            basename(filepath),
            ":",
            e$message
          ),
          log_file
        )
        return(NULL)
      }
    )
    if (is.null(content)) {
      return(NULL)
    }

    metadata_lines <- content[grepl("^#", content)]
    speech_lines <- content[!grepl("^#", content)]
    speech_text <- paste(speech_lines[speech_lines != ""], collapse = "\n")

    date_line <- metadata_lines[grepl("Date:", metadata_lines)]
    date <- if (length(date_line) > 0) {
      str_extract(date_line, "\\d{4}-\\d{2}-\\d{2}")
    } else {
      log_message(paste("No Date metadata in", basename(filepath)), log_file)
      NA
    }

    parliament_line <- metadata_lines[grepl("Parliament:", metadata_lines)]
    parliament <- if (length(parliament_line) > 0) {
      as.numeric(str_extract(parliament_line, "\\d+"))
    } else {
      log_message(
        paste("No Parliament metadata in", basename(filepath)),
        log_file
      )
      NA
    }

    if (
      is.na(date) ||
        !nzchar(date) ||
        !str_detect(date, "^\\d{4}-\\d{2}-\\d{2}$")
    ) {
      log_message(
        paste(
          "Invalid or missing date in",
          basename(filepath),
          "- skipping"
        ),
        log_file
      )
      return(NULL)
    }
    year <- as.numeric(str_sub(date, 1, 4))

    if (is.na(parliament)) {
      log_message(
        paste(
          "Invalid or missing parliament in",
          basename(filepath),
          "- skipping"
        ),
        log_file
      )
      return(NULL)
    }

    data.frame(
      filepath = filepath,
      filename = basename(filepath),
      date = date,
      year = year,
      parliament = parliament,
      speech_text = speech_text,
      text_length = nchar(speech_text),
      stringsAsFactors = FALSE
    )
  })

  if (nrow(speeches) == 0) {
    log_message("No valid speeches found - stopping", log_file)
    stop("No valid speeches found")
  }
  speeches <- speeches %>%
    filter(!is.na(date), !is.na(year), !is.na(parliament)) %>%
    arrange(date)

  log_message(paste("Loaded", nrow(speeches), "valid speeches"), log_file)
  write_csv(speeches, "output/segmentation/intermediate_speeches.csv")
  return(speeches)
}


process_speech_with_gemini <- function(
  numbered_text,
  filename,
  api_key,
  timeout = 60,
  log_file
) {
  log_message(paste("Processing speech:", filename), log_file)
  prompt <- paste0(
    "Segment this full throne speech into three sections:\n",
    "1. OPENING_CEREMONIAL: Formal protocol, addresses, historical acknowledgments, and introductory remarks (typically the first few paragraphs).\n",
    "2. POLICY_CONTENT: Specific government agendas, commitments, and policy details (the bulk, often starting with phrases like \"Our Government will...\" or \"My Government will...\").\n",
    "3. CLOSING_CEREMONIAL: Formal conclusion, blessings, or prorogation notes (typically the last paragraph).\n\n",
    "Provide the output as a JSON object with keys:\n",
    "- sections: An object with keys 'opening_ceremonial', 'policy_content', 'closing_ceremonial', each containing:\n",
    "  - start_sentence_id: Integer (1-based)\n",
    "  - end_sentence_id: Integer (1-based)\n",
    "- transition_markers: Array of strings (any identified transition phrases, e.g., \"Honourable Senators\", \"My Government will\", \"May Divine Providence\")\n\n",
    # Added a stricter instruction for the AI
    "Ensure the output is ONLY a single, valid JSON object with no extra text or formatting.\n\n",
    "Full speech text with numbered sentences:\n",
    numbered_text
  )

  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-latest:generateContent"

  tryCatch(
    {
      req <- request(url) %>%
        req_url_query(key = api_key) %>%
        req_timeout(timeout) %>%
        req_body_json(list(
          contents = list(
            list(
              parts = list(
                list(text = prompt)
              )
            )
          ),
          generationConfig = list(
            response_mime_type = "application/json"
          )
        ))

      resp <- req_perform(req)

      if (resp_status(resp) == 200) {
        body <- resp_body_json(resp)
        json_text <- body$candidates[[1]]$content$parts[[1]]$text

        # Improved error handling for JSON parsing
        json_output <- tryCatch(
          {
            fromJSON(json_text, simplifyVector = FALSE)
          },
          error = function(e) {
            log_message(
              paste(
                "JSON parsing error for",
                filename,
                ":",
                e$message
              ),
              log_file
            )
            # Important: return NULL so the retry logic can work
            return(NULL)
          }
        )

        # Only proceed if JSON parsing was successful
        if (!is.null(json_output)) {
          json_output$filename <- filename
          log_message(paste("Successfully processed", filename), log_file)
        }
        return(json_output)
      } else {
        error_body <- resp_body_string(resp)
        log_message(
          paste(
            "API error for",
            filename,
            ":",
            resp_status(resp),
            "Body:",
            error_body
          ),
          log_file
        )
        return(NULL)
      }
    },
    error = function(e) {
      log_message(
        paste("Failed to process", filename, ":", e$message),
        log_file
      )
      return(NULL)
    }
  )
}

# Analyze N-grams of a Specific Size
analyze_ceremony_ngrams <- function(
  n_size,
  ceremonial_corpus,
  clean_corpus,
  stopword_regex
) {
  ceremonial_ngrams_by_doc <- ceremonial_corpus %>%
    unnest_tokens(ngram, ceremonial_text, token = "ngrams", n = n_size) %>%
    filter(!str_detect(ngram, stopword_regex), !str_detect(ngram, "[0-9]")) %>%
    distinct(filename, ngram)
  policy_ngrams_by_doc <- clean_corpus %>%
    unnest_tokens(ngram, policy_content, token = "ngrams", n = n_size) %>%
    filter(!str_detect(ngram, stopword_regex), !str_detect(ngram, "[0-9]")) %>%
    distinct(filename, ngram)
  df_ceremonial <- ceremonial_ngrams_by_doc %>%
    count(ngram, name = "df_ceremonial")
  df_policy <- policy_ngrams_by_doc %>% count(ngram, name = "df_policy")
  total_docs_appeared <- bind_rows(
    ceremonial_ngrams_by_doc,
    policy_ngrams_by_doc
  ) %>%
    distinct(filename, ngram) %>%
    count(ngram, name = "total_docs")

  # Join and calculate metrics
  summary <- total_docs_appeared %>%
    left_join(df_ceremonial, by = "ngram") %>%
    left_join(df_policy, by = "ngram") %>%
    mutate(across(starts_with("df_"), ~ tidyr::replace_na(., 0))) %>%
    mutate(
      n_size = n_size,
      pervasiveness_pct = (total_docs / !!total_docs) * 100,
      ceremonial_proportion = df_ceremonial / total_docs
    )
  return(summary)
}

# Helper function to clean JSON response
clean_json_response <- function(json_text) {
  # Remove markdown code blocks if present
  json_text <- gsub("```json\\s*", "", json_text)
  json_text <- gsub("```\\s*$", "", json_text)

  # Remove any leading/trailing whitespace
  json_text <- trimws(json_text)

  # Sometimes there's extra text before or after - try to extract just the JSON array
  # Look for the first [ and last ]
  start_pos <- regexpr("\\[", json_text)[1]
  end_pos <- tail(gregexpr("\\]", json_text)[[1]], 1)

  if (start_pos > 0 && end_pos > start_pos) {
    json_text <- substr(json_text, start_pos, end_pos)
  }

  return(json_text)
}


interpret_topic_set_with_llm <- function(k_value, topics_df, api_key) {
  log_message(
    paste("Interpreting topic set for k =", k_value),
    "llm_interpretation_log.txt"
  )

  # Check if API key is available
  if (is.null(api_key) || api_key == "") {
    return(list(error = "API key is missing or empty"))
  }

  # Format the top terms for all topics into a single string
  topics_input_string <- topics_df %>%
    mutate(
      formatted_string = paste0("  - Topic ", topic, ": '", terms, "'")
    ) %>%
    pull(formatted_string) %>%
    paste(collapse = "\n")

  # Updated prompt to be more explicit about JSON format
  prompt <- paste0(
    "You are an expert political historian specializing in Canadian policy. ",
    "I have a set of ",
    k_value,
    " topics from a topic model of Canadian Throne Speeches. ",
    "Your task is to provide a concise interpretation for EACH topic based on its most probable words, considering the context of the other topics.\n\n",
    "Here is the full set of topics and their words:\n",
    topics_input_string,
    "\n\n",
    "Please provide your response as a valid JSON array (and ONLY the JSON array, no markdown formatting or extra text). ",
    "Each element should be an object with these three keys:\n",
    "1. 'topic_id': The integer topic number.\n",
    "2. 'label': A short, descriptive topic label of 3-5 words (e.g., 'National Defense & Foreign Affairs').\n",
    "3. 'focus': A single sentence describing the policy area this topic represents.\n\n",
    "Return only the JSON array, starting with [ and ending with ]:"
  )

  # Claude API endpoint and request structure
  url <- "https://api.anthropic.com/v1/messages"

  response <- tryCatch(
    {
      req <- request(url) %>%
        req_headers(
          "x-api-key" = api_key,
          "anthropic-version" = "2023-06-01",
          "content-type" = "application/json"
        ) %>%
        req_timeout(90) %>%
        req_body_json(list(
          model = "claude-sonnet-4-20250514", # Use the tested model
          max_tokens = 2000,
          messages = list(
            list(
              role = "user",
              content = prompt
            )
          )
        ))

      resp <- req_perform(req)

      # Check if response is successful
      if (resp_status(resp) != 200) {
        error_body <- tryCatch(
          {
            resp_body_json(resp)
          },
          error = function(e) {
            list(error = list(message = "Could not parse error response"))
          }
        )

        error_msg <- paste("HTTP", resp_status(resp))
        if (!is.null(error_body$error$message)) {
          error_msg <- paste(error_msg, "-", error_body$error$message)
        }

        return(list(error = error_msg))
      }

      resp_body_json(resp)
    },
    error = function(e) {
      list(error = e$message)
    }
  )

  Sys.sleep(2) # Pause between each major API call

  return(response)
}

calculate_coherence <- function(top_terms, dtm_tm, M = 10) {
  # Get the top M terms for each topic
  terms_to_check <- top_terms %>%
    group_by(topic) %>%
    slice_max(beta, n = M, with_ties = FALSE) %>%
    ungroup()

  # Convert the tm DTM to a standard sparse matrix for calculations
  dtm_sparse <- Matrix::sparseMatrix(
    i = dtm_tm$i,
    j = dtm_tm$j,
    x = dtm_tm$v,
    dims = c(dtm_tm$nrow, dtm_tm$ncol),
    dimnames = dtm_tm$dimnames
  )

  # Convert DTM to a binary matrix (presence/absence)
  dtm_binary <- dtm_sparse
  dtm_binary[dtm_binary > 0] <- 1

  # Create a term co-occurrence matrix
  term_cooc <- Matrix::t(dtm_binary) %*% dtm_binary

  # Get total number of documents
  N <- nrow(dtm_sparse)

  # Get document frequency for each term
  doc_freq <- Matrix::diag(term_cooc)
  names(doc_freq) <- colnames(term_cooc)

  # Calculate NPMI coherence for each topic
  coherence_scores <- terms_to_check %>%
    group_by(topic) %>%
    summarise(
      coherence = {
        current_terms <- term
        if (length(current_terms) < 2) {
          return(NA)
        }

        term_pairs <- combn(current_terms, 2, simplify = FALSE)
        pair_scores <- purrr::map_dbl(term_pairs, function(pair) {
          word1 <- pair[1]
          word2 <- pair[2]

          # Get frequencies
          cooc_count <- term_cooc[word1, word2]
          doc_freq_w1 <- doc_freq[word1]
          doc_freq_w2 <- doc_freq[word2]

          # Avoid division by zero
          if (cooc_count == 0 || doc_freq_w1 == 0 || doc_freq_w2 == 0) {
            return(-1) # Return minimum NPMI value
          }

          # Calculate PMI
          pmi <- log((cooc_count * N) / (doc_freq_w1 * doc_freq_w2))

          # Calculate NPMI (normalized PMI)
          # NPMI = PMI / -log(P(w1,w2))
          joint_prob <- cooc_count / N
          npmi <- pmi / (-log(joint_prob))

          # NPMI should be between -1 and 1
          # Clamp values to avoid numerical issues
          npmi <- pmax(-1, pmin(1, npmi))

          return(npmi)
        })

        # Remove any infinite or NaN values
        pair_scores <- pair_scores[is.finite(pair_scores)]

        if (length(pair_scores) == 0) {
          return(-1)
        }

        mean(pair_scores)
      },
      .groups = "drop"
    )

  return(coherence_scores$coherence)
}
