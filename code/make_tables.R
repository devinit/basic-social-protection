list.of.packages <- c("data.table", "ggplot2", "Hmisc", "tidyverse", "stringr","tidytext")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/basic-social-protection/"
setwd(wd)

crs = fread("large_input/crs.csv")

# Tables 1 ####
# 
# ODA By recipient country, disbursed funding to social protection purpose code (16010) by year  
# 
t1a = subset(crs, purpose_code==16010)
t1a = t1a[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
  ),by=.(year, purpose_code, recipient_name)]
fwrite(t1a, "output/table1_a.csv")
# ODA By recipient country, disbursed funding to social protection purpose code (16010) by year, with adaptation flag as 2 (principal)  
# 
t1b = subset(crs, purpose_code==16010 & climate_adaptation==2)
t1b = t1b[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, purpose_code, recipient_name, climate_adaptation)]
fwrite(t1b, "output/table1_b.csv")
# ODA By recipient country, disbursed funding to social protection purpose code (16010) by year, with adaptation flag anything but 2 (blank, 0, 1) 
# 
t1c = subset(crs, purpose_code==16010 & climate_adaptation!=2)
t1c = t1c[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, purpose_code, recipient_name)]
t1c$climate_adaptation = "Blank, 0, or 1"
fwrite(t1c, "output/table1_c.csv")
# 
# Tables 2 ####
# 
# Total ODA (disbursed funding) by recipient country by year  
# 
t2a = crs[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, recipient_name)]
fwrite(t2a, "output/table2_a.csv")
# Total ODA by recipient country by year, without humanitarian/emergency sector codes (720, 730, 740)  
# 
t2b = subset(crs, !sector_code %in% c(720, 730, 740))
t2b = t2b[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, recipient_name)]
fwrite(t2b, "output/table2_b.csv")
# Total flows by recipient country by year, to humanitarian/emergency sector codes (720, 730, 740 – separately) 
# 
t2c = subset(crs, sector_code %in% c(720, 730, 740))
t2c = t2c[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, recipient_name, sector_code)]
fwrite(t2c, "output/table2_c.csv")
# 
# Table 3 ####
# 
# By recipient country, funding to social protection purpose code by year, disaggregated by “FlowName” type (e.g. ODA Grants, ODA Loans) 
# 
t3a = subset(crs, purpose_code==16010)
t3a = t3a[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, recipient_name, flow_name)]
fwrite(t3a, "output/table3_a.csv")
# 
# Table 4 ####
# 
# By donor, disbursed funding to social protection purpose code to each recipient country, by year 
# 
t4a = subset(crs, purpose_code==16010)
t4a = t4a[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, donor_name, recipient_name)]
fwrite(t4a, "output/table4_a.csv")
# 
# Table 5 ####
# 
# By recipient country, disbursed funding to social protection purpose code by year, by “channel name” 
# 
t5a = subset(crs, purpose_code==16010)
t5a = t5a[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, channel_name, recipient_name)]
fwrite(t5a, "output/table5_a.csv")
# 
# Table 6 ####
# 
# By recipient country, disbursed funding to General Budget Support sector code (150) by year 
# 
t6a = subset(crs, sector_code==510)
t6a = t6a[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, sector_code, recipient_name)]
fwrite(t6a, "output/table6_a.csv")
# By recipient country, disbursed funding to Government and Civil Society Development Sector code (151) by year 
# 
t6b = subset(crs, sector_code==151)
t6b = t6b[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, sector_code, recipient_name)]
fwrite(t6b, "output/table6_b.csv")
# By recipient country, disbursed funding to the following purpose codes by year: 
#  15110 – public sector policy and administrative management 
#  15111 – public financial management 
#  15114 – domestic revenue mobilisation 
t6c = subset(crs, purpose_code %in% c(15110, 15111, 15114))
t6c = t6c[,.(
  usd_disbursement=sum(usd_disbursement, na.rm=T),
  usd_disbursement_deflated=sum(usd_disbursement_deflated, na.rm=T)
),by=.(year, purpose_code, recipient_name)]
fwrite(t6c, "output/table6_c.csv")

# Keyword Search ####
# 
# To supplement social protection purpose code, are there any flows clearly for social protection that are allocated to other purpose codes? 
# KWS in “Short Description”, “Project Title” and “Long Description” columns for the following terms: 

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

textual_cols = c(
  "project_title",
  "short_description",
  "long_description"
)

non_protection = subset(crs, purpose_code!=16010)

non_protection = non_protection %>%
  unite(text, all_of(textual_cols), sep=" ", na.rm=T, remove=F)

non_protection$text = clean_text(non_protection$text)
keywords = read.csv("input/keywords.csv")
keywords = subset(keywords, is.na(addition))
keywords$query = quotemeta(clean_text(keywords$query))
keyword_regex = paste0(
  "\\b",
  paste(keywords$query, collapse="\\b|\\b"),
  "\\b"
)
non_protection$keyword_match = grepl(keyword_regex, non_protection$text, perl=T, ignore.case = T)
non_protection_matches = subset(non_protection, keyword_match)
non_protection_matches[,c("keyword_match", "text")] = NULL
fwrite(non_protection_matches, "large_output/keyword_matches.csv")
