list.of.packages <- c("data.table", "ggplot2", "Hmisc", "tidyverse", "stringr","tidytext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/basic-social-protection/"
setwd(wd)

quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

remove_punct = function(string){
  str_replace_all(string, "[[:punct:]]", " ")
}

collapse_whitespace = function(string){
  str_replace_all(string, "\\s+", " ")
}

clean_text = function(string){
  return(
    trimws(collapse_whitespace(remove_punct(tolower(string))))
  )
}


crs = fread("large_input/crs.csv")

textual_cols = c(
  "project_title",
  "short_description",
  "long_description"
)

crs = crs %>%
  unite(text, all_of(textual_cols), sep=" ", na.rm=T, remove=T)

crs$text = clean_text(crs$text)

crs$social = "Not social protection"
crs$social[which(crs$purpose_code==16010)] = "Social protection"

crs = unique(crs[,c("social","text")])
gc()

social_words <- crs %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  count(social, word, sort = TRUE)

total_words <- social_words %>% 
  group_by(social) %>% 
  summarize(total = sum(n))

social_words <- left_join(social_words, total_words)

freq_by_rank <- social_words %>% 
  group_by(social) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

social_tf_idf <- social_words %>%
  bind_tf_idf(word, social, n)

View(subset(social_tf_idf, social=="Social protection"))
