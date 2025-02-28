# these methods and documentation grabbed from https://github.com/OHDSI/Capr

setMethod("as.list", "Concept", function(x){
  nm <- methods::slotNames(methods::is(x))
  concept <- lapply(nm, methods::slot, object = x)
  # Convert NA_character to empty string
  concept <- lapply(concept, function(.) ifelse(is.character(.) && is.na(.), "", .))
  names(concept) <- toupper(nm)
  return(concept)
})

# as.list(x@Expression[[1]]@Concept)

setMethod("as.list", "ConceptSetItem", function(x){
  list('concept' = as.list(x@Concept),
       'isExcluded' = x@isExcluded,
       'includeDescendants' = x@includeDescendants,
       'includeMapped' = x@includeMapped)
})

# as.list(x@Expression[[1]])

setMethod("as.list", "ConceptSet", function(x){
  list('id' = x@id,
       'name' = x@Name,
       'expression' = list('items' = lapply(x@Expression, as.list)))
})

