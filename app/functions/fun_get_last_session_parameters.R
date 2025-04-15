fun_get_last_session_parameters <- function(user = NULL){
  
  query <- paste0(
    "select * from get_user_session(_user=>'", user, "')"
  )
  
  data <- dbGetQuery(con, query)
  
  if(is.na(data$get_user_session)){
    inputs <- list()
  }else{
    inputs <- data$get_user_session %>%
      rjson::fromJSON()
  }
  
  # output
  inputs
}
