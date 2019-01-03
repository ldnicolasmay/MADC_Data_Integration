# wip_translation_dictionary.R


# USEFUL VARS ----
`%>%` <- dplyr::`%>%`
source('~/Desktop/config.R')
rm(BOX_CLIENT_ID); rm(BOX_CLIENT_SECRET); rm(BOX_REDIRECT_URI)
rm(REDCAP_DATA_REQUESTS_TOKEN)

## switch to access API (TRUE) or not (FALSE)
get_api_data <- TRUE

# USEFUL HELPER FUNCTIONS ----
remove_NAs <- function(x) {
  if (is.vector(x)) return(x[!is.na(x)])
  else stop('x is not a vector')
}

# GET FIELDS ----

# _ UDS 2 ----
if (get_api_data) {
  export_fields_u2_json <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS2,
    content='exportFieldNames',
    format='json',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
export_fields_u2_df <- jsonlite::fromJSON(export_fields_u2_json) %>% 
  dplyr::na_if('')

# _ UDS 3 (old) ----
if (get_api_data) {
  export_fields_u31_json <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS31,
    content='exportFieldNames',
    format='json',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
export_fields_u31_df <- jsonlite::fromJSON(export_fields_u31_json) %>% 
  dplyr::na_if('')

# _ UDS 3 (new) ----
if (get_api_data) {
  export_fields_u32_json <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS3,
    content='exportFieldNames',
    format='json',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
export_fields_u32_df <- jsonlite::fromJSON(export_fields_u32_json) %>% 
  dplyr::na_if('')

# _ MiNDSet ----
if (get_api_data) {
  export_fields_ms_json <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_MINDSET,
    content='exportFieldNames',
    format='json',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
export_fields_ms_df <- jsonlite::fromJSON(export_fields_ms_json) %>% 
  dplyr::na_if('')

# _ Get work-in-progress translation dictionary ----
trans_dict <- readxl::read_excel('WIP__translation_dictionary.xlsx')

# _ Get work-in-progress UDS3 ivp-fvp-tvp matches ----
u32_ift_match <- readxl::read_excel('WIP__uds3_ift_matches.xlsx')

# _ UDS 2 ----
fields_u2_raw <- trans_dict$field_u2
fields_u2_raw <- remove_NAs(fields_u2_raw)
fields_u2 <- fields_u2_raw %>% paste(collapse = ',')

# _ UDS 3 (old) ----
fields_u31_raw <- trans_dict$field_u31
fields_u31_raw <- remove_NAs(fields_u31_raw)
fields_u31 <- fields_u31_raw %>% paste(collapse = ',')

# _ UDS 3 (new) ----
# _ _ IVP
fields_u32_raw_i <- trans_dict$field_u32
# _ _ FVP
fields_u32_raw_fu <- u32_ift_match %>% 
  dplyr::filter(field_ivp %in% fields_u32_raw_i) %>% 
  dplyr::filter(!is.na(field_fvp)) %>% 
  dplyr::pull(field_fvp)
# _ _ TVP
fields_u32_raw_tele <- u32_ift_match %>% 
  dplyr::filter(field_ivp %in% fields_u32_raw_i) %>% 
  dplyr::filter(!is.na(field_tvp)) %>%
  dplyr::pull(field_tvp)
# _ _ IVP + FVP + TVP
fields_u32_raw <- c(fields_u32_raw_i
                    , fields_u32_raw_fu
                    , fields_u32_raw_tele)
fields_u32_raw <- remove_NAs(fields_u32_raw) # remove NAs
fields_u32 <- fields_u32_raw %>% paste(collapse = ',') 


# GET DATA ----

# _ UDS 2 ----
if (get_api_data) {
  json_u2 <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS2,
    content='record',
    format='json',
    type='flat',
    fields=fields_u2,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
df_u2 <- jsonlite::fromJSON(json_u2) %>% dplyr::na_if('')

# _ UDS 3 (old) ----
if (get_api_data) {
  json_u31 <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS31,
    content='record',
    format='json',
    type='flat',
    fields=fields_u31,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
df_u31 <- jsonlite::fromJSON(json_u31) %>% dplyr::na_if('')

# _ UDS 3 (new) ----
if (get_api_data) {
  json_u32 <- RCurl::postForm(
    uri=REDCAP_API_URI,
    token=REDCAP_API_TOKEN_UDS3,
    content='record',
    format='json',
    type='flat',
    fields=fields_u32,
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json'#,
    # .opts = list(ssl.verifypeer = FALSE) # using linux
  )
}
df_u32 <- jsonlite::fromJSON(json_u32) %>% dplyr::na_if('')


# IVP/FVP/TVP TRANSFORM (UDS 2, UDS 3 old) ----

# _ UDS 2 ----
# _ _ UDS 2 IVP ----
df_u2_i <- df_u2 %>% 
  dplyr::filter(a1pkt_type == 'I') # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ UDS 2 FVP ----
u32_fvp_vars <- stringr::str_replace(fields_u32_raw_fu, 'fu_', '')
u2_fvp_vars <- trans_dict %>% 
  dplyr::filter(field_u32 %in% u32_fvp_vars) %>% 
  dplyr::pull(field_u2) %>% 
  remove_NAs(.)
df_u2_f <- df_u2 %>% 
  dplyr::filter(a1pkt_type == 'F') %>% 
  dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(u2_fvp_vars)),
                   .funs = dplyr::funs(sub('(*)', 'fu_', .))) # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ UDS 2 TVP ----
u32_tvp_vars <- stringr::str_replace(fields_u32_raw_tele, 'tele_', '')
u2_tvp_vars <- trans_dict %>% 
  dplyr::filter(field_u32 %in% u32_tvp_vars) %>% 
  dplyr::pull(field_u2) %>% 
  remove_NAs(.)
df_u2_t <- df_u2 %>% 
  dplyr::filter(a1pkt_type == 'T') %>% 
  dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(u2_tvp_vars)),
                   .funs = dplyr::funs(sub('(*)', 'tele_', .))) # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ Row bind UDS 2 I+F+T ----
df_u2_ift <- dplyr::bind_rows(df_u2_i, df_u2_f, df_u2_t)

# _ UDS 3 (old) ----
# _ _ UDS 3 (old) IVP ----
df_u31_i <- df_u31 %>% 
  dplyr::filter(a1pkt_type == 'I') # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ UDS 3 (old) FVP ----
u32_fvp_vars <- stringr::str_replace(fields_u32_raw_fu, 'fu_', '')
u31_fvp_vars <- trans_dict %>% 
  dplyr::filter(field_u32 %in% u32_fvp_vars) %>% 
  dplyr::pull(field_u31) %>% 
  remove_NAs(.)
df_u31_f <- df_u31 %>% 
  dplyr::filter(a1pkt_type == 'F') %>% 
  dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(u31_fvp_vars)),
                   .funs = dplyr::funs(sub('(*)', 'fu_', .))) # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ UDS 3 (old) TVP ----
u32_tvp_vars <- stringr::str_replace(fields_u32_raw_tele, 'tele_', '')
u31_tvp_vars <- trans_dict %>% 
  dplyr::filter(field_u32 %in% u32_tvp_vars) %>% 
  dplyr::pull(field_u31) %>% 
  remove_NAs(.)
df_u31_t <- df_u31 %>% 
  dplyr::filter(a1pkt_type == 'T') %>% 
  dplyr::rename_at(.vars = dplyr::vars(dplyr::one_of(u31_tvp_vars)),
                   .funs = dplyr::funs(sub('(*)', 'tele_', .))) # %>% 
  # dplyr::select(-a1pkt_type)
# _ _ Row bind UDS 3 (old) I+F+T ----
df_u31_ift <- dplyr::bind_rows(df_u31_i, df_u31_f, df_u31_t)


# STITCH TOGETHER DATA ----
# df_u2 + df_u31 + df_u32 based on translation dictionary

# _ Build standard names df ----
## Set up empty df
std_names_df <- data.frame(matrix(data = NA_character_,
                                  nrow = 0,
                                  ncol = 2),
                           stringsAsFactors = FALSE)
names(std_names_df) <- c('std_source', 'std_field')

## Loop through rows of `trans_dict`
for (i in seq_len(nrow(trans_dict))) { 
  
  ## Std field name is from UDS 3 (new), then UDS 3 (old), then UDS 2
  if (!is.na(trans_dict[i, 'field_u32'])) {
    std_names_df[i, ] <- list('field_u32', trans_dict[i, 'field_u32'])
  } else if (!is.na(trans_dict[i, 'field_u31'])) {
    std_names_df[i, ] <- list('field_u31', trans_dict[i, 'field_u31'])
  } else if (!is.na(trans_dict[i, 'field_u2'])) {
    std_names_df[i, ] <- list('field_u2', trans_dict[i, 'field_u2'])
  }
  
}
trans_dict_std <- cbind(trans_dict, std_names_df)

## Append `fu_*` fields ----
u32_ift_match_f <- u32_ift_match %>% 
  dplyr::filter(!is.na(field_fvp)) %>% 
  dplyr::filter(field_ivp %in% u32_fvp_vars) %>% 
  dplyr::pull(field_ivp)
for (i in seq_along(u32_ift_match_f)) {
  # cat(paste(i, u32_ift_match_f[i], '\n'))
  # cat(paste(i, nrow(trans_dict_std)+i, '\n'))
  curr_row <- nrow(trans_dict_std) + 1
  ## Add `fu_*` field from within trans_dict_std to end of trans_dict_std
  ## UDS 2
  trans_dict_std[curr_row, 'field_u2'] <-
    paste0('fu_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_f[i]), 'field_u2'
             ]])
  # UDS 3 (old)
  trans_dict_std[curr_row, 'field_u31'] <-
    paste0('fu_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_f[i]), 'field_u31'
             ]])
  # UDS 3 (new)
  trans_dict_std[curr_row, 'field_u32'] <-
    paste0('fu_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_f[i]), 'field_u32'
             ]])
  trans_dict_std[curr_row, 'form_u32'] <-
    stringr::str_replace(
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_f[i]), 'form_u32'
           ]],
           pattern = 'ivp_',
           replacement = 'fvp_')
  trans_dict_std[curr_row, 'Notes'] <- 
    '__ synthetic field in UDS 2, UDS 3 (old) __'
  trans_dict_std[curr_row, 'std_source'] <- 'field_u32'
  trans_dict_std[curr_row, 'std_field'] <- trans_dict_std[[curr_row, 'field_u32']]
}
## Append `tele_*` fields
u32_ift_match_t <- u32_ift_match %>% 
  dplyr::filter(!is.na(field_tvp)) %>% 
  dplyr::filter(field_ivp %in% u32_tvp_vars) %>% 
  dplyr::pull(field_ivp)
for (i in seq_along(u32_ift_match_t)) {
  # cat(paste(i, u32_ift_match_t[i], '\n'))
  # cat(paste(i, nrow(trans_dict_std)+i, '\n'))
  curr_row <- nrow(trans_dict_std) + 1
  ## Add `tele_*` field from within trans_dict_std to end of trans_dict_std
  ## UDS 2
  trans_dict_std[curr_row, 'field_u2'] <-
    paste0('tele_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_t[i]), 'field_u2'
             ]])
  # UDS 3 (old)
  trans_dict_std[curr_row, 'field_u31'] <-
    paste0('tele_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_t[i]), 'field_u31'
             ]])
  # UDS 3 (new)
  trans_dict_std[curr_row, 'field_u32'] <-
    paste0('tele_', 
           trans_dict_std[[
             which(trans_dict_std$field_u32 == u32_ift_match_t[i]), 'field_u32'
             ]])
  trans_dict_std[curr_row, 'form_u32'] <-
    stringr::str_replace(
      trans_dict_std[[
        which(trans_dict_std$field_u32 == u32_ift_match_t[i]), 'form_u32'
        ]],
      pattern = 'ivp_',
      replacement = 'tvp_')
  trans_dict_std[curr_row, 'Notes'] <- 
    '__ synthetic field in UDS 2, UDS 3 (old) __'
  trans_dict_std[curr_row, 'std_source'] <- 'field_u32'
  trans_dict_std[curr_row, 'std_field'] <- 
    trans_dict_std[[curr_row, 'field_u32']]
}
# _ Remove any instances of 'fu_NA' or 'tele_NA' -----
trans_dict_std <- trans_dict_std %>% dplyr::na_if('fu_NA')
trans_dict_std <- trans_dict_std %>% dplyr::na_if('tele_NA')

# _ Build stitched data df ----
## Stitched df is `df_u2_u31_u32`
df_u2_u31_u32 <- data.frame(matrix(data = NA_character_,
                                   nrow = sum(nrow(df_u2_ift), 
                                              nrow(df_u31_ift), 
                                              nrow(df_u32)),
                                   ncol = nrow(trans_dict_std)))
names(df_u2_u31_u32) <- trans_dict_std$std_field


## Loop through `trans_dict_std` rows to build stitched df column by column
## ... using df_u2_ift, df_u31_ift, df_u32
for (i in seq_len(nrow(trans_dict_std))) {
  cat(paste(i, '\n'))

  ## Build temp UDS 2 vector
  if (!is.na(trans_dict_std[i, 'field_u2'])) {
    temp_u2_vec <- df_u2_ift[[trans_dict_std[i, 'field_u2']]]
  } else {
    temp_u2_vec <- rep(NA_character_, times = nrow(df_u2_ift))
  }

  ## Build temp UDS 3 (old) vector
  if (!is.na(trans_dict_std[i, 'field_u31'])) {
    temp_u31_vec <- df_u31_ift[[trans_dict_std[i, 'field_u31']]]
  } else {
    temp_u31_vec <- rep(NA_character_, times = nrow(df_u31_ift))
  }

  ## Build temp UDS 3 (new) vector
  if (!is.na(trans_dict_std[i, 'field_u32'])) {
    temp_u32_vec <- df_u32[[trans_dict_std[i, 'field_u32']]]
  } else {
    temp_u32_vec <- rep(NA_character_, times = nrow(df_u32))
  }

  ## Concat `temp_*_vec` vectors
  temp_u2_u31_u32_vec <- c(temp_u2_vec, temp_u31_vec, temp_u32_vec)
  # print(length(temp_u2_u31_u32_vec))

  ## Lay concat'd `temp_*_vec` vectors into stitched df column
  df_u2_u31_u32[, i] <- temp_u2_u31_u32_vec
}

print(object.size(df_u2_u31_u32), units = 'auto')

# _ Guess column types
df_u2_u31_u32 <- readr::type_convert(df_u2_u31_u32)
print(object.size(df_u2_u31_u32), units = 'auto')

dplyr::glimpse(df_u2_ift)
dplyr::glimpse(df_u31_ift)
dplyr::glimpse(df_u32)
dplyr::glimpse(df_u2_u31_u32)


# WRITE TO CSV ----
readr::write_csv(df_u2_u31_u32, 'df_u2_u31_u32.csv', na = '')







