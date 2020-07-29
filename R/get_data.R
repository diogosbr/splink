#' @title download dos dados
#'
#' @name get_data
#'
#' @description faz download dos dados do splink com base na requisição
#'
#' @param list_data list. Object returned by `setup_list()`, or a named list with parameters to request download.
#'
#' @details https://api.splink.org.br/
#'
#' @return list
#'
#' @importFrom httr VERB progress
#' @importFrom utils read.table
#'
#' @examples
#' get_data(list_data = list(Scientificname = c("Manilkara maxima"),
#'                           MaxRecords = 5))
#' \dontrun{
#' get_data(list_data = list(ScientificName = c("Rauvolfia sellowii", "Cantinoa althaeifolia"),
#'                           StateProvince = c("São Paulo","Rio de Janeiro","Pernambuco"),
#'                           MaxRecords = 5,
#'                           Model = "Coords"))
#'                           }
#'
#' @export
get_data <- function(list_data){

  # link da API do speciesLink
  url <- "https://api.splink.org.br/records"

  # para testes de desenvolvimento
  # list_data <- list(Scientificname = c("Manilkara maxima"),
  #            StateProvince = c("Bahia"),
  #            Synonyms = "flora2020")

  # baixando os dados do splink
  get_rec <-
    httr::VERB(
      verb = "POST", url = url,
      body = list_data,
      encode = "json",
      config = httr::progress()
    )

  ################################
  # incluir salvar um metadado   #
  # pode ser a partir do get_rec #
  ################################

  # convert response object to text
  get_text <- httr::content(get_rec, "text")

  # check if exists downloaded data
  if(substring(get_rec,1,3) != 'seq'){
    stop("You search returned no data. Check your list.")}

  # caracteres que podem causar erro na leitura
  bad_character <- intToUtf8(c(91, 62, 33, 180, 60, 35, 63, 38, 47, 92, 46, 93))

  # retirando os caracteres que podem causar erro na leitura
  get_text <- gsub(bad_character, "", get_text)

  # convertendo o texto em tabela
  res_table <- read.table(text = get_text, sep = '\t', header = TRUE, quote = "", stringsAsFactors = F)

  return(res_table)
}
