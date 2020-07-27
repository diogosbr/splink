#' @title download dos dados
#'
#' @name get_data
#'
#' @description faz download dos dados do splink com base na requisição
#'
#' @param list_data lista com os parâmetros a serem baixados
#'
#' @return list
#'
#' @importFrom httr VERB
#'
#' @export
get_data <- function(list_data){

  # link da API do speciesLink
  url <- "https://api.splink.org.br/records"

  # para testes de desenvolvimento
  list_data <- list(Scientificname = c("Manilkara maxima"),
             StateProvince = c("Bahia"),
             Synonyms = "flora2020")

  # baixando os dados do splink
  get_rec <-
    httr::VERB(
      verb = "POST", url = url,
      body = list_data,
      encode = "json",
      config = progress()
    )

  # convert response object to text
  get_text <- httr::content(get_rec, "text")

  # caracteres que podem causar erro na leitura
  bad_character <- intToUtf8(c(91, 62, 33, 180, 60, 35, 63, 38, 47, 92, 46, 93))

  # retirando os caracteres que podem causar erro na leitura
  get_text <- gsub(bad_character, "", get_text)

  # convertendo o texto em tabela
  res_table <- read.table(text = get_text, sep = '\t', header = TRUE, quote = "", stringsAsFactors = F)

  head(res_table)
  return(res_table)
}
