#' @title Types of searchable fields
#'
#' @name fields_data
#'
#' @description searchable fields
#'
#' @details fields_data returns a matrix of the searchable fields that are supported.
#'
#' @return data.frame with fields of setup_list
#'
#' @export
fields_data <- function(){

  res_data_frame <-
    data.frame(
      field = c("Barcode",
                "BasisOfRecord",
                "InstitutionCode",
                "CollectionCode",
                "CatalogNumber",
                "Collector",
                "CollectorNumber",
                "YearCollected",
                "IdentifiedBy",
                "YearIdentified",
                "Kingdom",
                "Phylum",
                "Class",
                "Order",
                "Family",
                "Genus",
                "ScientificName",
                "TypeStatus",
                "Country",
                "StateProvince",
                "County",
                "Locality",
                # modifiers:
                "CoordinatesQuality",
                "Format",
                "Separator",
                "MaxRecords",
                "Model",
                "Phonetic",
                "RedList",
                "Scope",
                "Coordinates",
                "Images",
                "Synonyms",
                "Typus",
                "ShowEmptyValues",
                "fieldsCase",
                "Summary"),
      class = c(rep("character", 22),
                rep("numeric", 15)),
      type = c(rep("searchable", 22),
               rep("modifier", 15))
    )

  return(res_data_frame)

  #  searcheable fields
  #  @ = accepts ARRAY
  #  $ = scalar only
  #  * = implemented
  #
  #  @
  #  #
  #  #  @ *	Barcode		NY00000001 | FPS00257 | FCM00096 | UEC006758
  #  @ *	BasisOfRecord	PreservedSpecimen | LivingSpecimen | FossilSpecimen |
  #  #  HumanObservation | MachineObservation | MaterialSample	PreservedSpecimen
  #  @	InstitutionCode	sinstitution acronym	UNICAMP | Fiocruz
  #  @ *	CollectionCode	collection acronym	FIOCRUZ-CEIOC | UEC | HUEFS
  #  @ *	CatalogNumber	number	435643 | embranco | nãobranco
  #  @ *	Collector	collector name	Kock | Almeida F
  #  @ *	CollectorNumber	number	125 | 1897
  #  @ *	YearCollected	four-digits year	1987 | 1897 | 2016
  #  @ *	IdentifiedBy		Abreu MC
  #  @ *	YearIdentified	four-digits year	1887 | 1997 | 2015
  #  @ *	Kingdom		Plantae | Animalia | Fungi
  #  @ *	Phylum
  #  @ *	Class
  #  @ *	Order
  #  @ *	Family		Bromeliaceae | Apidae
  #  @ *	Genus		Tabebuia | Bacillus | Boa | Hoplias
  #  @ *	ScientificName		Bothrops neuwiedii matogrossensis | Leishmania braziliensis
  #  @ *	TypeStatus
  #  @ *	Country
  #  @ *	StateProvince
  #  @ *	County
  #  @ *	Locality
  #  @	Notes
  #
  #  modifiers:
  #  #  $ *	CoordinatesQuality	Good | Bad	 any coordinates
  #  $ *	Format			JSON | XML | CSV | TAB
  #  $ *	Separator		comma | semicolon	Valid only for Format = CSV
  #  $ *	MaxRecords		n > 0	 all records
  #  $ *	Model			DwC | modelling
  #  $ *	Phonetic		Yes | No	#  affects only taxonomic fields: class, phylum, order, family, genus, scientificname#
  #  $ *	RedList			Yes | No	#  no check#
  #  $ *	Scope			plants, animals, microrganisms,fossils	 all groups
  #  $ *	Coordinates		Yes | No | Original | Automatic | Blocked	#  no check#
  #  $ *	Images			Yes | No | Live | Polen | Wood	#  no check
  #  $	Synonyms		sp2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure#   no synonyms#
  #  $ *	Typus			Yes | No 	#  no check
  #  $ *	ShowEmptyValues		Yes | No
  #  $ *	fieldsCase		Lower | Upper | Mixed
  #  $ *	Summary			Yes | No
}
