get_authentication <- function(service_account_key){
    get_service_token = function(str,scopes = c("https://www.googleapis.com/auth/bigquery", 
                                               "https://www.googleapis.com/auth/cloud-platform",
                                               "https://www.googleapis.com/auth/userinfo.email"),
                                ..., subject = NULL){
    info <- jsonlite::fromJSON(str)
    if (!identical(info[["type"]], "service_account")) {
      gargle:::gargle_debug(c("JSON does not appear to represent a service account", 
                              "Did you provide the JSON for an OAuth client instead of for a \\\n       service account?"))
      return()
    }
    token <- httr::oauth_service_token(endpoint = httr::oauth_endpoints("google"), 
                                       secrets = info, scope = scopes, sub = subject)
    if (is.null(token$credentials$access_token) || !nzchar(token$credentials$access_token)) {
      NULL
    }
    else {
      gargle:::gargle_debug("service account email: {.email {token_email(token)}}")
      token
    }
                                }
  
  ## pass in string via env_var...
  str <- Sys.getenv(service_account_key)
  bq_auth(token = get_service_token(str))

}

