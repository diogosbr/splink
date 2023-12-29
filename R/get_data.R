#' @title Search for speciesLink registers
#'
#' @name get_data
#'
#' @description Search for speciesLink registers based on the list.
#'
#' @param list_data list. Object returned by `setup_list()`, or a named list with parameters to request download. To see the accepted parameters see type fields_data().
#' @param filename Output filename. If is NULL (default) only returns a tibble.
#'
#' @details This function is based on CRIA speciesLink API v.0.1 beta, for more details see <https://api.splink.org.br/>.
#'
#' @return data.frame containing the records based on the request.
#'
#' @importFrom httr VERB progress POST content
#' @importFrom pingr is_online is_up
#' @importFrom readr read_tsv
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
get_data <- function(list_data, filename = NULL){

  API <- "api.splink.org.br"

  if(!pingr::is_online()) {stop("você precisa estar online para usar esta função. Verifique sua conexão com a internet.")}
  if(!pingr::is_up(API)) {stop("CRIA speciesLink API v.0.1 beta are offline. Try again latter.")}

  url <- "https://api.splink.org.br/records"

  # link to speciesLink API

  # check list_data format
  if(!is.list(list_data)){stop("The list_data must be a list.")}
  # check list_data names
  if(!all(tolower(names(list_data)) %in% tolower(fields_data()$field))){stop("One or more names of list_data are invalid.")}

  # check if MaxRecords is > 0
  if(!is.null(list_data$MaxRecords)){
    if(list_data$MaxRecords <= 0) stop("MaxRecords values is not valid, inform a value > 0.")}

  #list_data$Format <- "CSV"

  # download splink data
  # get_rec <-
  #   httr::VERB(
  #     verb = "POST", url = url,
  #     body = list_data,
  #     encode = "json",
  #     config = httr::progress()
  #   )
  get_rec <-
    httr::POST(
      url = url,
      body = list_data,
      encode = "json",
      config = httr::progress()
    )

  # convert response object to text
  res_table <- httr::content(get_rec, "text")

  # check if exists downloaded data
  if(substring(res_table,1,3) != 'seq'){
    stop("You search returned no data. Check your list.",
         "If you are sure that your list is correct and the error still remains, please report it to the package developer.")}

  res_table <- readr::read_tsv(res_table)
  #res_table <-  read.table(text = res_table, sep = '\t', header = TRUE, quote = "", stringsAsFactors = F)

  if(!is.null(filename)) {utils::write.csv(res_table, file = paste0(filename), row.names = FALSE)}

  if(nrow(res_table) == 0) {warning("Your search returned no records.")}
  message("Downloaded at ", get_rec$headers$date)

  return(res_table)
}
