# STF Metadata Fields Reference

This section describes the metadata fields required in STF YAML files. These metadata files provide essential structural information for interpreting and validating the study data.

## Study-Level Metadata (`study.yaml`)

The `study.yaml` file contains high-level metadata about the study and lists the entities included in the dataset.

### Fields

- **`name`**: *(string, required)*
  The name of the study. This should be a concise, human-readable identifier.

- **`entities`**: *(array of strings, required)*
  A list of entity names that are part of the study. Each name must match the corresponding `name` field in an entity YAML file.

### Example

```
name: My Awesome Study
entities:
  - household
  - participant
  - observation
```

---

## Entity-Level Metadata (`entity-<entity_name>.yaml`)

Each entity YAML file describes an entity in the study dataset, including its ID structure and variable definitions.

### Fields

- **`name`**: *(string, required)*
  The machine-readable name of the entity. This must match the entity's TSV filename.

- **`display_name`**: *(string, required)*
  A human-readable name for the entity.

- **`display_name_plural`**: *(string, required)*
  The plural form of `display_name`.

- **`id_columns`**: *(array, required)*
  Defines the ID columns for the entity, linking them to entity names and hierarchical levels.

  - **`id_column`**: *(string, required)*
    The name of the ID column in the TSV file.

  - **`entity_name`**: *(string, required)*
    The name of the entity this ID belongs to.

  - **`entity_level`**: *(integer, optional)*
    The relative level of the entity in the hierarchy.  
    - `0` (default) for the entity itself  
    - `-1` for the immediate parent  
    - `-2` for the grandparent, etc.

- **`variables`**: *(array, required)*
  Defines the variables (columns) in the entity’s TSV file.

  - **`variable`**: *(string, required)*
    The column name in the TSV file.

  - **`display_name`**: *(string, required)*
    A human-readable name for the variable.

  - **`data_type`**: *(string, required)*
    The data type of the variable. Allowed values:
    - `integer`
    - `decimal`
    - `string`
    - `date`
    - `boolean`

  - **`data_shape`**: *(string, required)*
    The variable’s statistical nature. Allowed values:
    - `continuous`
    - `categorical`
    - `ordinal`

  - **`provider_label`**: *(array of strings, optional)*
    Alternative label(s) from the data provider.

### Example

```
name: participant
display_name: Participant
display_name_plural: Participants

id_columns:
  - id_column: Household.Id
    entity_name: household
    entity_level: -1
  - id_column: Participant.Id
    entity_name: participant

variables:
  - variable: Name
    display_name: Full Name
    data_type: string
    data_shape: categorical

  - variable: Age
    display_name: Age
    data_type: integer
    data_shape: continuous

  - variable: Sex
    display_name: Sex
    data_type: string
    data_shape: categorical
    provider_label:
      - Gender
```

