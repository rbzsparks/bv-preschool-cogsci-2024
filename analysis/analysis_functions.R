# Lexical diversity - MTLD ####
make_krp_text <- function(data_df) {
  data_formatted <- data_df |>
    mutate(lttr = sapply(token, str_length),
           desc = NA,
           stop = NA,
           stem_ = NA,
           uID = utterance_no + 1) |>
    select(c(doc_id = file, 
             token = token, 
             tag = upos,
             lemma = lemma,
             lttr,
             wclass = upos,
             desc,
             stop,
             stem = stem_,
             sntc = uID))
  
  kRp_text(lang = "en", token = data_formatted)
}

get_krp_ld_vals <- function(krp_text_ld) {
  sapply(krp_text_ld, summary) |> t() |> 
    as.data.frame() |> rownames_to_column() |> 
    unnest(cols = c(index, value)) |>
    rename(file_name = rowname) |>
    mutate(index = as.factor(index),
           value = as.numeric(value))
}

get_ld_vals <- function(data_df, measure = c("MATTR", "MTLD")) {
  data_krp <- make_krp_text(data_df)
  data_ld <- lex.div(data_krp, measure = measure, char = "", quiet = TRUE)
  data_ld_vals <- get_krp_ld_vals(data_ld)
  return(list(data_krp, data_ld, data_ld_vals))
}

get_ld <- function(data_df, measure = c("MATTR", "MTLD")) {
  get_ld_vals(data_df, measure)[[3]] |>
    rename(metric = index)
}

# Lexical density - Lexicality ####
get_lexicality <- function(data_df) {
  data_df |> 
    mutate(pos_min = sub(":.*", "", .data$upos),
           is_lexical = pos_min %in% c("NOUN", "VERB", "ADJ", "ADV") & 
             !(upos %in% c("PROPN")) &
             !(lemma %in% c("be", "do", "get", "have", "need", "want"))) |>
    group_by(file) |>
    summarise(value = sum(is_lexical) / n()) |> 
    mutate(metric = "lexical_density")
}

# Lexical sophistication - Frequency and contextual diversity ####
# SUBTLEX_N_TOKENS = 51010983
# SUBTLEX_N_MOVIES = 8388
# subtlex_en <- read_xlsx(here("resources", "SUBTLEXusExcel2007.xlsx")) |> 
#   mutate(Word = tolower(Word),
#          language = "English") |> 
#   select(c(gloss = Word, freq = SUBTLWF, cd = SUBTLCD, language)) |> 
#   mutate(log_freqn = log10(freq + 1e6 / SUBTLEX_N_TOKENS),
#          log_cdn = log10(cd + 100 / SUBTLEX_N_MOVIES))
# 
# ESPAL_N_TOKENS = 462611693
# ESPAL_N_MOVIES = 40444
# subtlex_es <- read_csv(here("resources", "EsPal.csv")) |> 
#   mutate(Word = tolower(Word),
#          language = "Spanish") |> 
#   select(c(gloss = Word, cnt = SUBTLWF, doc_cnt = SUBTLCD, language)) |> 
#   mutate(freq = 1e6 * cnt / ESPAL_N_TOKENS,
#          cd = 100 * doc_cnt / ESPAL_N_MOVIES,
#          log_freqn = log10(freq + 1e6 / ESPAL_N_TOKENS),
#          log_cdn = log10(cd + 100 / ESPAL_N_MOVIES)) |> 
#   select(-cnt, -doc_cnt)
# 
# subtlex <- bind_rows(subtlex_en, subtlex_es)
# 
# get_sophistication <- function(data_df) {
#   data_df |> 
#     left_join(subtlex, by = c("gloss", "language")) |>
#     group_by(file_name) |>
#     summarise(freq = mean(log_freqn, na.rm = TRUE),
#               cd = mean(log_cdn, na.rm = TRUE)) |> 
#     pivot_longer(cols = c(freq, cd), names_to = "metric", values_to = "value")
# }

# Grammatical complexity - MLU-w and proportion complex utterances ####

get_mlu <- function(data_df, by) {
  # temporary---grabs from utterances
  data_df |> 
    mutate(n_tokens = text |> str_squish() |> str_count(" ") |> {\(n) n+1}()) |> 
    group_by(pick(all_of(c(by)))) |> 
    summarise(mlu = mean(n_tokens))
}

get_complexity <- function(data_df) {
  data_df |> 
    mutate(pos_min = sub(":.*", "", .data$upos),
           is_lexical_v = pos_min %in% c("VERB") & 
             !(lemma %in% c("be", "do", "get", "have", "need", "want"))) |>
    group_by(utterance_no, file) |>
    summarise(is_complex = sum(is_lexical_v) > 1) |>
    group_by(file) |>
    summarise(value = sum(is_complex) / n()) |> 
    mutate(metric = "prop_complex")
}

# Number of Utterances Normalized by Time

get_norm_utterance <- function(data_df) {
  data_df |>
    group_by(file) |>
    summarise(utt_norm = n()/s_length) |>
    distinct()
}

# Number of Tokens Normalized by Time

get_norm_token <- function(data_df) {
  data_df |>
    group_by(file) |>
    summarise(token_norm = n()/s_length) |>
    distinct()
}

# Number of Conversational Turns Normalized by Time

get_code_count_value <- function(data_df, code) {
  count = 0
  for (row in 2:nrow(data_df)) {
    if(data_df[code][row,] != data_df[code][row-1,]) {
      count = count + 1
    }
  }
  max_time = max(c(data_df$start_time, data_df$end_time), na.rm = TRUE)
  c_normed = (count/max_time)*60
  return(c_normed)
}

# Number of Context Changes Normalized by Time

get_code_count <- function(data_df, code) {
  count_table <- data_df |>
    group_by(file) |>
    summarise(count = get_code_count_value(cur_data(), code))
  col_name = paste(code,"count",sep="_")
  count_table %>% rename(col_name = count)
  return(count_table)
}

make_descriptives <- function(vals) {
  vals |> 
    filter(is.finite(value)) |> 
    group_by(metric, language, source) |> 
    summarise(median = median(value, na.rm = TRUE),
              mad = median(abs(value - median(value, na.rm = TRUE)), na.rm = TRUE),
              min = min(value),
              max = max(value))
}

# Keyness - ARF function adapted from Dawson et al. (2021)
get_ARF <- function(df, all_tokens) {
  tokens <- df$lemma
  ARF_values <- sapply(tokens |> unique(), function(tok) {
    positions <- which(tokens == tok)
    freq <- length(positions)
    chunk_len <- length(tokens) / freq
    dist <- c(positions[-1], length(tokens)) - positions
    max(sum(sapply(dist, function(x) min(x, chunk_len))) / chunk_len, 1)
  }) |> as_tibble(rownames = "lemma") |> rename("arf" = "value")
}

get_abs_freq <- function(df, all_tokens) {
  freq_values <- df |> count(lemma) |> rename("abs_freq" = "n")
}

get_norm_freq <- function(df, all_tokens) {
  arf <- get_ARF(df, all_tokens)
  abs_freq <- get_abs_freq(df, all_tokens)
  all_tokens |> left_join(arf) |> left_join(abs_freq) |>
    mutate(arf_norm = (arf / sum(abs_freq, na.rm=T)) * 1e6) |>
    mutate_all(~replace(., is.na(.), 0))
}