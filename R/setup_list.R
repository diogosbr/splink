#' @title criar lista de espécies
#'
#' @name setup_list
#'
#' @description cria uma lista para busca
#'
#' @param ScientificName vetor com os nomes das espécies
#'
#' @return list
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
#'
#' @export
setup_list <- function(Barcode = NULL,
                     BasisOfRecord = NULL,
                     InstitutionCode = NULL,
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
                     CoordinatesQuality = NULL,
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

  if(!is.null(CoordinatesQuality)) CoordinatesQuality = CoordinatesQuality
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

  list_data <- plyr::compact(list_data)
  # list_data <- list(Scientificname = Scientificname,
  #                   StateProvince = StateProvince,
  #                   Synonyms = Synonyms,
  #                   CollectionCode = CollectionCode,
  #                   County = County)

  return(list_data)
}

