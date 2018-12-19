# helper_fxns.R


# Define handy function to get meta data ----
get_meta_data <- function(api_token, 
                          api_uri = REDCAP_API_TOKEN, 
                          api_call = FALSE) {
  # Get JSON string via API call
  if (api_call) {
    json_meta <- RCurl::postForm(
      uri=api_uri,
      token=api_token,
      content='metadata',
      format='json',
      returnFormat='json'
    )
  }
  # Convert JSON to data.frame
  df_meta <- jsonlite::fromJSON(json_meta) %>%
    dplyr::na_if('') %>% 
    dplyr::select(field_name, form_name)
  # Return the meta data data.frame
  return(df_meta)
}


# Define handy function to get data dictionary ----
get_data_dict <- function(api_token, 
                          api_uri = REDCAP_API_TOKEN, 
                          api_call = FALSE) {
  # Get JSON string via API call
  if (api_call) {
    json_dict <- RCurl::postForm(
      uri=api_uri,
      token=api_token,
      content='exportFieldNames',
      format='json',
      returnFormat='json'
    )
  }
  # Convert JSON to data.frame
  df_dict <- jsonlite::fromJSON(json_dict) %>%
    dplyr::na_if('')
  # Return the meta data data.frame
  return(df_dict)
}
