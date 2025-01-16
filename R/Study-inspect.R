#' Inspect a Study Object
#'
#' Provides a summary view of the study's metadata and its entity structure
#'
#' @param entity An Entity object to inspect.
#' @export
setMethod("inspect", "Study", function(object) {
  study <- object
  entities <- get_entities(study)
  
  
  cat(
    to_lines(
      c(
        heading("Study"),
        kable(
          tibble(
            Field = c(
              "Study name",
              "Number of entities"
            ),
            Value = c(
              study@name,
              length(entities)
            )
          )
        ),

        heading('Entities'),
        pretty_tree(get_root_entity(study)),
        
        heading('Entity summaries'),
        kable(
          entities %>% map(
            function(entity) {
              tibble(
                Entity = get_entity_name(entity),
                `Display name` = get_display_name(entity),
                `Display name plural` = get_display_name_plural(entity),
                `Row count` = entity %>% get_data() %>% nrow(),
                `Is valid` = entity %>% quiet() %>% validate()
              )   
            }
          ) %>% bind_rows()
        )  
      )
    )
  )
  
})
