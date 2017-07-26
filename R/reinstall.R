dime_reinstall <- function(){
  devtools::install_github("termatico/dime", auth_token = Sys.getenv("GITHUB_PAT"))
}