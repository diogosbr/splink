#' @title Search for speciesLink registers
#'
#' @name get_data
#'
#' @description Search for speciesLink registers based on the list.
#'
#' @param list_data list. Object returned by `setup_list()`, or a named list with parameters to request download. To see the accepted parameters see type fields_data().
#'
#' @details This function is based on CRIA speciesLink API v.0.1 beta, for more details see <https://api.splink.org.br/>.
#'
#' @return data.frame containing the records based on the request.
#'
#' @importFrom httr VERB progress
#'
#' @examples
#' get_data(list_data = list(ScientificName = c("Manilkara maxima"),
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
  # check list_data names
  if(!all(tolower(names(list_data)) %in% tolower(fields_data()$field))){stop("One or more names of list_data are invalid.")}

  # check if MaxRecords is > 0
  if(!is.null(list_data$MaxRecords)){
    if(list_data$MaxRecords <= 0) stop("MaxRecords values is not valid, inform a value > 0. ")}

  list_data$Format <- "CSV"

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

  # get_rec$headers
  # get_rec$date

  ################################

  # convert response object to text
  res_table <- httr::content(get_rec, "parsed")

  # check if exists downloaded data
  if(class(res_table)[1] == "xml_document"){
    stop("You search returned no data. Check your list.",
         "If you are sure that your list is correct and the error still remains, please report it to the package developer.")}

  return(res_table)
}
