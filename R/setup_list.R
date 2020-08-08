#' @title Create a list object to request download of species register
#'
#' @name setup_list
#'
#' @description This function prepare a list to request download of species register with the `get_data` function.
#'
#' @param Barcode Barcode of specimen in a collection. Ex: NY00000001 or FPS00257.
#' @param BasisOfRecord Values accepted: 'PreservedSpecimen' , 'LivingSpecimen', 'FossilSpecimen', 'HumanObservation', 'MachineObservation' or 'MaterialSample'.
#' @param CollectionCode Collection acronym to search. Ex: "FIOCRUZ-CEIOC", "UEC", "HUEFS".
#' @param CatalogNumber Register number of specimen in the collection.
#' @param Collector Collector name. Ex:	"Kock", "Almeida F".
#' @param CollectorNumber Collector number.
#' @param YearCollected Year of specimen was collected with four-digits. Accepts a vector.
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
#' @param MaxRecords numeric. The maximum number of general records to be returned. Default is to return all records. See details for more information.
#' @param all.data Logical. Output all data or coordinates only. FALSE is default.
#' @param Phonetic Logical. If TRUE, allows the system to ignore some spelling differences in scientific names like 'Y' or 'I', double letters, etc. Affects only taxonomic fields `Phylum`, `Class`, `Order`, `Family` e `ScientificName`. FALSE is default.
#' @param RedList Searches for records with scientific names that appear in red list of MMA if 'yes' or it that not appear in red list of MMA if 'no'. To return any records the values is NULL (default).
#' @param Scope Groups of organisms to search. The accepted values are "plants", "animals", "microrganisms" and "fossils". NULL to search in all groups (default).
#' @param Coordinates Choose if will download the registers with coordinates. Default is all registers (with coordinates or not). See details for available options. ('Yes' , 'No' , 'Original' , 'Automatic' , 'Blocked')
#' @param Images If NULL (default) will search all record. If "yes" search the records that contain images of the specimen and 'no' will return only records without images. Images will not be downloaded. For more options see Details.
#' @param Synonyms Search also for synonyms defined in the following dictionaries: "sp2000", "flora2020", "MycoBank", "AlgaeBase", "DSMZ", "Moure" and/or NULL (default) if you don't want to search for synonyms.
#' @param Typus If 'yes' searches for type material only. If 'no', searches non type material only. Default is NULL and does no check type material.
#'
#' @return A list with parameters to use in `get_data()` function to download records.
#'
#' @details For more information about fields see <https://api.splink.org.br/> , <http://www.splink.org.br/> , <https://www.youtube.com/watch?v=I-4ftIWrKUA&feature=youtu.be> or <http://www.splink.org.br/showTips>.
#'
#'The search system is case insensitive, meaning that searching for Tabeuia aurea is the same as searching for tabebuia aurea or TABEBUIA AUREA.
#'
#'Accents are also not considered, meaning searching for São Paulo is the same as searching for Sao Paulo.
#'
#'Avoid using words such as de, do, em, the, etc. For example when searching for species that occur in Serra da Canastra, search for Serra Canastra or just Canastra within the locality field.
#'
#'In general, all words included in the form are considered when searching. Searching for Souza Silva in the collector’s field will retrieve all records that have the words Souza AND Silva simultaneously in that field.
#'
#'**Besides the connector |, users may search for a list of species – one in each line – with a maximum limit of 50 names.**
#'
#'Users can also search for blank fields, by typing the word null in the field. For example, to search for unidentified material of the family Asteraceae, users must type Asteraceae in the family field and null in the scientific name field. Also for Asteracea, if you want to see what genera of the Asteraceae family have unidentified material, you may type notnull null in the scientific name field. The words null and notnull can be used in other fields.
#'
#'The words 'null' and 'notnull' can also be used in the scientific name field. It is important to remember that the scientific name can be a genus (a single word), a binomial (genus + species) or a trinomial (genus + species + subspecies). The words 'null' and 'notnull' can replace any of these elements. For example: to search for records that have identification only up to the gender level, type 'notnull null' in the scientific name field.
#'
#' Nos campos **buscadores** são aceitos valores 'embranco' ou 'naobranco'. Por exemplo: é possível buscar apenas os registros de um gênero que não tenha sido identificado ainda, para isso deve-se informar no campo `ScientificName` "Manilkara embranco" (para o gênero Manilkara). Em caso de querer apenas os registros de um gênero que tenha identificação deve-se informar "Manilkara naobranco". No caso de querer todos os registros de um gênero, basta colocar "Manilkara".
#'
#' Coordinates:  Yes | No | Original | Automatic | Blocked	  no check.
#'
#' The complete list of searchable and modifiers fields are available with `fields_data()` (not implemented yet) function.
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
                       MaxRecords = NULL,
                       all.data = FALSE,
                       Phonetic = FALSE,
                       RedList = NULL,
                       Scope = NULL,
                       Coordinates = NULL,
                       Images = NULL,
                       Synonyms = NULL,
                       Typus = NULL
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

  if(!is.null(MaxRecords)) MaxRecords = MaxRecords
  if(all.data) {Model = "DwC"} else {Model = "coords"}
  if(Phonetic) {Phonetic = "yes"} else {Phonetic = "no"}
  if(!is.null(RedList)) RedList = RedList
  if(!is.null(Scope)) Scope = Scope
  if(!is.null(Coordinates)) Coordinates = Coordinates
  if(!is.null(Images)) Images = Images
  if(!is.null(Synonyms)) Synonyms = Synonyms
  if(!is.null(Typus)) Typus = Typus
  if(!is.null(RedList)) RedList = RedList


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

    MaxRecords = MaxRecords,
    Model = Model,
    Phonetic = Phonetic,
    RedList = RedList,
    Scope = Scope,
    Coordinates = Coordinates,
    Images = Images,
    Synonyms = Synonyms,
    Typus = Typus,
    RedList = RedList
  )

  # removing values not used
  list_data <- plyr::compact(list_data)

  return(list_data)
}

