#' @title Search for speciesLink registers
#'
#' @name get_data
#'
#' @description Search for speciesLink registers based on the list.
#'
#' @param list_data list. Object returned by `setup_list()`, or a named list with parameters to request download. To see the accepted parameters see type fields_data().
#'
#' @details This function is absed on CRIA speciesLink API v.0.1 beta, for more details see <https://api.splink.org.br/>.
#'
#' @return data.frame containing the records based on the request.
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

  # link to speciesLink API
  url <- "https://api.splink.org.br/records"

  # check list_data format
  if(!is.list(list_data)){stop("The list_data must be a list.")}

  # check if MaxRecords is > 0
  if(!is.null(list_data$MaxRecords)){
    if(list_data$MaxRecords <= 0) stop("MaxRecords values is not valid, inform a value > 0. ")
    }
  # download splink data
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
  get_text <- gsub(x = get_text, pattern = bad_character, replacement = "")

  # convert the text to table
  res_table <- read.table(text = get_text, sep = '\t', header = TRUE, quote = "", stringsAsFactors = F)

  return(res_table)
}
