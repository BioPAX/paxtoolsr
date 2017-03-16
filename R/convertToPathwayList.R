# # Convert to graphite Pathway Object 
# # 
# # @param lst list returned from 
# # 
# # @concept paxtoolsr
# # @export
# convertToPathwayList <- function(id="kegg", title="kegg", ident="DISPLAYNAME", 
#                                  species="hsapiens", lst) {
#     database <- unique(dbResults$edges$INTERACTION_DATA_SOURCE)
#     
#     edges <- data.frame(src=lst$edges$PARTICIPANT_A, 
#                         dest=lst$edges$PARTICIPANT_B, 
#                         direction=nrow(lst$edges), 
#                         type=lst$edges$INTERACTION_TYPE)
#     timestamp <- Sys.Date()
#     
#     pathway <- new("Pathway", 
#               id=id, 
#               title=title,
#               edges=edges,
#               database=database,
#               species=species,
#               identifier=ident,
#               timestamp=timestamp)
#     
#     return(pathway)
# }













