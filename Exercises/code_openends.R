# ─────────────────────────────────────────────────────────────────────────────
# code_openends.R
# LLM-based open-end coding using ellmer + local Ollama
#
# Prereqs:
#   install.packages("ellmer")
#   Ollama running locally: https://ollama.com
#   Model pulled:  ollama pull llama3.2
# ─────────────────────────────────────────────────────────────────────────────

library(ellmer)
library(dplyr)

# ── 1. Settings ───────────────────────────────────────────────────────────────
MODEL         <- "llama3.2"
OE_COL        <- "trump_pope_reaction"
RECYCLE_EVERY <- 30   # recreate chat every N rows to prevent context buildup

# ── 2. Load data ──────────────────────────────────────────────────────────────
df <- read.csv("https://raw.githubusercontent.com/apodkul/ppol6803_03/refs/heads/main/Data/survey_data_example.csv", stringsAsFactors = FALSE)

# ── 3. System prompt ──────────────────────────────────────────────────────────
SYSTEM_PROMPT <- 'You are a survey coder. Read the response and output ONLY
one label — nothing else, no punctuation, no explanation.

LABELS:
  pro-trump  = supports Trump or defends his comments about the Pope
  pro-leo    = supports Pope Leo or criticizes Trump for the comments
  nuanced    = genuine mixed / both-sides feelings
  irrelevant = does not know, no opinion, or non-answer (idk, n/a, etc.)

Examples:
Response: "Good for Trump, the pope should stay out of politics" -> pro-trump
Response: "I was horrified. You do not attack the Pope." -> pro-leo
Response: "Mixed — I get why Trump is annoyed but he went too far" -> nuanced
Response: "idk" -> irrelevant
Response: "Haven\'t been following it" -> irrelevant'

# ── 4. Robust output parser ───────────────────────────────────────────────────
parse_code <- function(raw) {
  if (is.null(raw) || is.na(raw)) return(NA_character_)
  s     <- tolower(trimws(raw))
  valid <- c("pro-trump", "pro-leo", "nuanced", "irrelevant")

  if (s %in% valid) return(s)

  s2 <- gsub("[ _]+", "-", s)
  s2 <- gsub("[^a-z\\-]", " ", s2)
  for (code in valid) {
    if (grepl(code, s2, fixed = TRUE)) return(code)
  }

  if (grepl("trump",                                     s)) return("pro-trump")
  if (grepl("leo|pope",                                  s)) return("pro-leo")
  if (grepl("nuanc|mix|both",                            s)) return("nuanced")
  if (grepl("idk|irrele|not sure|no opinion|don.t know", s)) return("irrelevant")

  return(NA_character_)
}

# ── 5. Chat management ────────────────────────────────────────────────────────
# Create the chat ONCE — Ollama keeps the model loaded between calls so there
# is no per-row reload cost. Recreate every RECYCLE_EVERY rows to flush the
# accumulated conversation history before it fills the context window.
make_chat <- function() {
  chat_ollama(model = MODEL, system_prompt = SYSTEM_PROMPT)
}

chat <- make_chat()

# ── 6. Single-row classifier with retry ──────────────────────────────────────
classify_response <- function(text, max_retries = 4) {
  msg  <- paste0('Response: "', gsub('"', "'", text), '"\nLabel:')
  wait <- 10

  for (attempt in seq_len(max_retries)) {
    raw <- tryCatch(
      trimws(chat$chat(msg)),
      error = function(e) {
        message("  ellmer error: ", conditionMessage(e))
        # Recreate chat on error so the model has a clean slate
        chat <<- make_chat()
        Sys.sleep(wait)
        NULL
      }
    )

    if (!is.null(raw)) {
      code <- parse_code(raw)
      if (!is.na(code)) return(code)
      # Got a response but couldn't parse it — log and return NA rather than retry
      message("  [unparseable] '", substr(raw, 1, 60), "'")
      return(NA_character_)
    }

    wait <- wait * 2
    cat(sprintf("\n  [retry %d/%d after error]", attempt, max_retries))
  }

  return(NA_character_)
}

# ── 7. Run over all rows ──────────────────────────────────────────────────────
cat("Model :", MODEL, "\n")
cat("Rows  :", nrow(df), "\n\n")

df$llm_code <- NA_character_

for (i in seq_len(nrow(df))) {

  # Recycle chat periodically to flush context
  if (i > 1 && (i - 1) %% RECYCLE_EVERY == 0) {
    chat <- make_chat()
    cat(sprintf("  [chat recycled at row %d]\n", i))
  }

  text <- df[[OE_COL]][i]
  cat(sprintf("[%03d/%d] %-55s → ", i, nrow(df), substr(text, 1, 55)))

  code           <- classify_response(text)
  df$llm_code[i] <- code
  cat(ifelse(is.na(code), "(failed)", code), "\n")
}

# ── 8. Results ────────────────────────────────────────────────────────────────
cat("\n── Code distribution ──────────────────────────────\n")
print(table(df$llm_code, useNA = "ifany"))

cat("\n── Failed rows ────────────────────────────────────\n")
cat(sum(is.na(df$llm_code)), "failed out of", nrow(df), "\n")

cat("\n── Party × code cross-tab ─────────────────────────\n")
print(table(df$party_id, df$llm_code, useNA = "ifany"))

# ── 9. Save ───────────────────────────────────────────────────────────────────
write.csv(df, "survey_data_coded.csv", row.names = FALSE)
cat("\nSaved → survey_data_coded.csv\n")
