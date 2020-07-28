#' @title criar lista de espécies
#'
#' @name setup_list
#'
#' @description cria uma lista para busca
#'
#' @param Barcode .
#' @param BasisOfRecord .
#' @param InstitutionCode string. Accepts institution acronym. Ex: "UNICAMP", "Fiocruz".
#' @param CollectionCode string. collection acronym. Ex: "FIOCRUZ-CEIOC", "UEC", "HUEFS".
#' @param CatalogNumber numeric. 	Number of specimen in the collection. Also accepts 'embranco' or "naobranco". "embranco" returns only specimens without catalog number and "naobranco" resturns only specimens with catalog number.
#' @param Collector string. Collector name. Ex:	"Kock", "Almeida F".
#' @param CollectorNumber numeric.
#' @param YearCollected numeric. Year of specimen was collected with four-digits.
#' @param IdentifiedBy string. Who identified the species of specimen. Ex: "Abreu MC"
#' @param YearIdentified numeric. Year of specimen was identified with four-digits.
#' @param Kingdom .
#' @param Phylum .
#' @param Class .
#' @param Order .
#' @param Family .
#' @param Genus .
#' @param ScientificName .
#' @param TypeStatus .
#' @param Country .
#' @param StateProvince .
#' @param County .
#' @param Locality .
#'
#' @param CoordinatesQuality Good | Bad	 any coordinates
#' @param Format JSON | XML | CSV | TAB
#' @param Separator comma | semicolon	Valid only for Format = CSV
#' @param MaxRecords n > 0	 all records
#' @param Model DwC | modelling
#' @param Phonetic Yes | No	  affects only taxonomic fields: class, phylum, order, family, genus, scientificname
#' @param RedList Yes | No	  no check
#' @param Scope plants, animals, microrganisms,fossils	 all groups
#' @param Coordinates Yes | No | Original | Automatic | Blocked	  no check
#' @param Images Yes | No | Live | Polen | Wood	  no check
#' @param Synonyms sp2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure   no synonyms
#' @param Typus Yes | No 	  no check
#' @param ShowEmptyValues Yes | No
#' @param fieldsCase Lower | Upper | Mixed
#' @param Summary Yes | No
#'
#' @return list
#'
#' @details To more information about fields see <https://api.splink.org.br/> or <>
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

