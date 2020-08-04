#' @title Types of acceptable fields for splink searching
#'
#' @name fields_data
#'
#' @description Types of acceptable fields for splink searching
#'
#' @details fields_data returns a data.frame of the fields that are supported and the respective types. Searchable are free fields and the modifiers are close fields to modify one or more specific searchable field.
#'
#' @return data.frame with fields of setup_list
#'
#' @export
fields_data <- function(){

  res_data_frame <-
    data.frame(
      field = c("Barcode",
                "BasisOfRecord",
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
      class = c(rep("character", 21),
                rep("numeric", 15)),
      type = c(rep("searchable", 21),
               rep("modifier", 15))
    )

  return(res_data_frame)
}
