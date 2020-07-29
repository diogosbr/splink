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
#' @param ScientificName vector. Accepts a vector of names.
#' @param TypeStatus .
#' @param Country .
#' @param StateProvince .
#' @param County .
#' @param Locality .
#'
#' @param CoordinatesQuality Refere-se à qualidade das coordenadas. "Good" são registros que tiveram as coordenadas checadas, "Bad" não tiveram as coordenadas checadas e "any" (default) retorna coordenadas com qualquer qualidade.
#' @param Format JSON | XML | CSV | TAB ## MANTER?
#' @param Separator especifica qual o seprador de colunas "comma" (default) ou semicolon. Este argumento é valid only for Format = CSV. ## MANTER?
#' @param MaxRecords numeric. Especificar o número máximo de registros devem ser retornados # POR ESPÉCIE? ##. O default é retornar todos os registros. # criar um check para n > 0.	 all records
#' @param Model DwC | modelling
#' @param Phonetic Premite uma busca fonética. Ou seja, permite que o sistema ignore algumas diferenças de grafia em nomes científicos. Por exemplo: I ou Y e letras duplas. Este argumento afeta apenas as buscas em `filo`, `classe`, `ordem`, `família` e `nome científico`.
#'  Yes | No	  affects only taxonomic fields: class, phylum, order, family, genus, scientificname
#' @param RedList Yes | No	  no check
#' @param Scope plants, animals, microrganisms,fossils	 all groups
#' @param Coordinates Yes | No | Original | Automatic | Blocked	  no check
#' @param Images Yes | No | Live | Polen | Wood	  no check
#' @param Synonyms Procura também por sinônimos definidos em alguns dicionários. Os valores aceitos são "sp2000", "flora2020", "MycoBank", "AlgaeBase", "DSMZ", "Moure" e/ou NULL (default) caso não queira realizar busca de sinônimos. Para mais infromações, veja a seção detalhes. ## IMPLEMENTAR
#' @param Typus Se TRUE, seleciona apenas registros que sejam tipos nomenclaturais. FALSE retorna apenas registros que não são tipos nomemclaturais. NULL é o default e retorna independente de ser tipo. ## Yes | No *no check*
#' @param ShowEmptyValues Yes | *No*
#' @param fieldsCase Lower | Upper | *Mixed*
#' @param Summary Yes | *No*
#'
#' @return list
#'
#' @details For more information about fields see <https://api.splink.org.br/> or <http://www.splink.org.br/>.
#' Nos campos **buscadores** são aceitos valores 'embranco' ou 'naobranco'.
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

