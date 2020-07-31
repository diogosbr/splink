#' @title Create a list object to request download of species register
#'
#' @name setup_list
#'
#' @description This function prepare a list to request download of species register with the `get_data` function.
#'
#' @param Barcode Barcode of specimen in a collection
#' @param BasisOfRecord .
#' @param CollectionCode Collection acronym to search. Ex: "FIOCRUZ-CEIOC", "UEC", "HUEFS".
#' @param CatalogNumber Register number of specimen in the collection.
#' @param Collector Collector name. Ex:	"Kock", "Almeida F".
#' @param CollectorNumber Collector number.
#' @param YearCollected Year of specimen was collected with four-digits.
#' @param IdentifiedBy Who identified the species of specimen. Ex: "Abreu MC".
#' @param YearIdentified Year of specimen was identified with four-digits.
#' @param Kingdom Kingdom classification name.
#' @param Phylum Phylum classification name.
#' @param Class Class classification name.
#' @param Order Order classification name.
#' @param Family Family classification name.
#' @param Genus Genus classification name.
#' @param ScientificName A character vector with scientific name, without authors. Accepts a vector of names. If you want include synonym taxa use `Synonyms` argument. See details to use 'embranco' or "naobranco" options.
#' @param TypeStatus .
#' @param Country Country of specimen register.
#' @param StateProvince State of specimen register.
#' @param County City of specimen register.
#' @param Locality Locality of specimen register. This field is very dificult to return with precision because it is a free form field.
#'
#' @param CoordinatesQuality Refere-se à qualidade das coordenadas. "Good" são registros que tiveram as coordenadas checadas, "Bad" não tiveram as coordenadas checadas e "any" (default) retorna coordenadas com qualquer qualidade.
#' @param Format JSON | XML | CSV | TAB ## MANTER?
#' @param Separator especifica qual o seprador de colunas "comma" (default) ou semicolon. Este argumento é valid only for Format = CSV. ## MANTER?
#' @param MaxRecords numeric. The maximum number of general records to be returned. Default is to return all records. See details for more information.
#' @param Model Escolhe o tamanho dos dados que se quer baixar (DwC | modelling)
#' @param Phonetic Premite uma busca fonética. Ou seja, permite que o sistema ignore algumas diferenças de grafia em nomes científicos. Por exemplo: I ou Y e letras duplas. Este argumento afeta apenas as buscas em `Phylum`, `Class`, `Order`, `Family` e `ScientificName`.
#'  Yes | No	  affects only taxonomic fields: class, phylum, order, family, genus, scientificname
#' @param RedList Yes | No	  no check
#' @param Scope Groups of organisms to search. The accepted values are "plants", "animals", "microrganisms" and "fossils". NULL to search in all groups (default).
#' @param Coordinates choose if will download the registers with coordinates. Default is all registers (with coordinates or not). See details for available opitions.
#' @param Images If NULL (default) will search all record, if "yes" search the records that contain images of the specimen. Images will not be downloaded. For more options see Details.
#' @param Synonyms Procura também por sinônimos definidos em alguns dicionários. Os valores aceitos são "sp2000", "flora2020", "MycoBank", "AlgaeBase", "DSMZ", "Moure" e/ou NULL (default) caso não queira realizar busca de sinônimos. Para mais infromações, veja a seção detalhes. ## IMPLEMENTAR
#' @param Typus Se TRUE, seleciona apenas registros que sejam tipos nomenclaturais. FALSE retorna apenas registros que não são tipos nomemclaturais. NULL é o default e retorna independente de ser tipo. ## Yes | No *no check*
#' @param ShowEmptyValues Yes | *No*
#' @param fieldsCase Lower | Upper | *Mixed*
#' @param Summary Yes | *No*
#'
#' @return list
#'
#' @details For more information about fields see <https://api.splink.org.br/> , <http://www.splink.org.br/> or <https://www.youtube.com/watch?v=I-4ftIWrKUA&feature=youtu.be>.
#'
#' Nos campos **buscadores** são aceitos valores 'embranco' ou 'naobranco'. Por exemplo: é possível buscar apenas os registros de um gênero que não tenha sido identificado ainda, para isso deve-se informar no campo `ScientificName` "Manilkara embranco" (para o gênero Manilkara). Em caso de querer apenas os registros de um gênero que tenha identificação deve-se informar "Manilkara naobranco". No caso de querer todos os registros de um gênero, basta colocar "Manilkara".
#' "embranco" returns only specimens without catalog number and "naobranco" resturns only specimens with catalog number.
#'
#' Coordinates have
#' Coordinates:  Yes | No | Original | Automatic | Blocked	  no check.
#'
#' The complete list of barcode e etc are available with `fields_data()` (not implemented yet) function.
#'
#' Attention, because the `MaxRecords` refers to the total number of records, in case of searching for more than one species it may be that records of only one species are returned. This depends on the number of records per species and the maximum number of records requested.
#'
#' To the Images argument, the options are "yes" for records with images, "no" for records without images, "live" for records with images of living material, "pollen" for records with images of pollen, "wood" for images of wood (Xylotheque) and NULL to search all records.
#'
#' @importFrom plyr compact
#'
#' @examples
#' setup_list(ScientificName = "Manilkara maxima")
#'
#' \dontrun{
#' setup_list(ScientificName = c("Rauvolfia sellowii", "Cantinoa althaeifolia"),
#'            StateProvince = c("São Paulo","Rio de Janeiro","Pernambuco"),
#'            MaxRecords = 5,
#'            Model = "Coords")
#'            }
#'
#' @export
setup_list <- function(Barcode = NULL,
                       BasisOfRecord = NULL,
                       CollectionCode = NULL,
                       CatalogNumber = NULL,
                       Collector = NULL,
                       CollectorNumber = NULL,
                       YearCollected = NULL,
                       IdentifiedBy = NULL,
                       YearIdentified = NULL,
                       Kingdom = NULL,
                       Phylum = NULL,
                       Class = NULL,
                       Order = NULL,
                       Family = NULL,
                       Genus = NULL,
                       ScientificName = NULL,
                       TypeStatus = NULL,
                       Country = NULL,
                       StateProvince = NULL,
                       County = NULL,
                       Locality = NULL,

                       # modifiers
                       CoordinatesQuality = "any",
                       Format = NULL,
                       Separator = NULL,
                       MaxRecords = NULL,
                       Model = NULL,
                       Phonetic = NULL,
                       RedList = NULL,
                       Scope = NULL,
                       Coordinates = NULL,
                       Images = NULL,
                       Synonyms = NULL,
                       Typus = NULL,
                       ShowEmptyValues = NULL,
                       fieldsCase = NULL,
                       Summary = NULL
){

  ## IMPLEMENTAR CHECKS ##
  # se foi informado algum dos campos obrigatórios, que são os *buscadores*
  #

  ##==============##
  ## *buscadores* ##
  ##==============##

  if(!is.null(Barcode)) Barcode = Barcode
  if(!is.null(BasisOfRecord)) BasisOfRecord = BasisOfRecord
  if(!is.null(CollectionCode)) CollectionCode = CollectionCode
  if(!is.null(CatalogNumber)) CatalogNumber = CatalogNumber
  if(!is.null(Collector)) Collector = Collector
  if(!is.null(CollectorNumber)) CollectorNumber = CollectorNumber
  if(!is.null(YearCollected)) YearCollected = YearCollected
  if(!is.null(IdentifiedBy)) IdentifiedBy = IdentifiedBy
  if(!is.null(Kingdom)) Kingdom = Kingdom
  if(!is.null(YearIdentified)) YearIdentified = YearIdentified
  if(!is.null(Kingdom)) Kingdom = Kingdom
  if(!is.null(Phylum)) Phylum = Phylum
  if(!is.null(Class)) Class = Class
  if(!is.null(Order)) Order = Order
  if(!is.null(Family)) Family = Family
  if(!is.null(Genus)) Genus = Genus
  if(!is.null(ScientificName)) ScientificName = ScientificName
  if(!is.null(TypeStatus)) TypeStatus = TypeStatus
  if(!is.null(Country)) Country = Country
  if(!is.null(StateProvince)) StateProvince = StateProvince
  if(!is.null(County)) County = County
  if(!is.null(Locality)) Locality = Locality

  ##===========##
  ## modifiers ##
  ##===========##

  # check if are valid values in CoordinatesQuality
  if(any(CoordinatesQuality %in% c("any", "Good", "Bad"))) {
    if(CoordinatesQuality == "any") {CoordinatesQuality <- NULL} else{
      CoordinatesQuality = CoordinatesQuality
    }
  } else{
    stop("Invalid value for CoordinatesQuality. The accepted values are 'Good, 'Bad or 'any'.")
  }

  if(!is.null(Format)) Format = Format
  if(!is.null(Separator)) Separator = Separator
  if(!is.null(MaxRecords)) MaxRecords = MaxRecords
  if(!is.null(Model)) Model = Model
  if(!is.null(Phonetic)) Phonetic = Phonetic
  if(!is.null(RedList)) RedList = RedList
  if(!is.null(Scope)) Scope = Scope
  if(!is.null(Coordinates)) Coordinates = Coordinates
  if(!is.null(Images)) Images = Images
  if(!is.null(Synonyms)) Synonyms = Synonyms
  if(!is.null(Typus)) Typus = Typus
  if(!is.null(RedList)) RedList = RedList
  if(!is.null(ShowEmptyValues)) ShowEmptyValues = ShowEmptyValues
  if(!is.null(fieldsCase)) fieldsCase = fieldsCase
  if(!is.null(Summary)) Summary = Summary


  # creating the list to request data in the API
  list_data <- list(
    Barcode = Barcode,
    BasisOfRecord = BasisOfRecord,
    CollectionCode = CollectionCode,
    CatalogNumber = CatalogNumber,
    Collector = Collector,
    CollectorNumber = CollectorNumber,
    YearCollected = YearCollected,
    IdentifiedBy = IdentifiedBy,
    Kingdom = Kingdom,
    YearIdentified = YearIdentified,
    Kingdom = Kingdom,
    Phylum = Phylum,
    Class = Class,
    Order = Order,
    Family = Family,
    Genus = Genus,
    ScientificName = ScientificName,
    TypeStatus = TypeStatus,
    Country = Country,
    StateProvince = StateProvince,
    County = County,
    Locality = Locality,

    CoordinatesQuality = CoordinatesQuality,
    Format = Format,
    Separator = Separator,
    MaxRecords = MaxRecords,
    Model = Model,
    Phonetic = Phonetic,
    RedList = RedList,
    Scope = Scope,
    Coordinates = Coordinates,
    Images = Images,
    Synonyms = Synonyms,
    Typus = Typus,
    RedList = RedList,
    ShowEmptyValues = ShowEmptyValues,
    fieldsCase = fieldsCase,
    Summary = Summary
  )

  # removing values not used
  list_data <- plyr::compact(list_data)

  return(list_data)
}

