# Study Transfer Format (STF) Documentation

The Study Transfer Format (STF) is designed for structured storage and
exchange of study data, supporting hierarchical entity
relationships. STF includes two formats: STF (full) and STF-Lite. The
STF format includes detailed metadata, while STF-Lite is a minimal
version without metadata.

## Studies, Entities, IDs, and Variables

A **study** consists of a dataset composed of linked
**entities**. Each entity is a table representing instances of a
real-world object or event, such as a `household` in a clinical
epidemiology study or a `biosample` in a genomics study. Entities are
arranged hierarchically, meaning that some entities serve as parents
to others:

- A `household` entity may have multiple associated `participant` records.
- A `biosample` entity may have multiple `sample` children, each of
  which might have one or more `assay` instances.

Each entity table must include a unique identifier (ID) column to
distinguish instances. If an entity is a child of another, it must
also include a parent ID column to establish the
relationship. Additional ancestor IDs (e.g., grandparent
relationships) can be included but are optional.

Beyond ID columns, entity tables contain **variables**, which describe
attributes of entity instances. Variables fall into different data
types:

- **Numeric**: e.g., `age`, `BMI`
- **Date**: e.g., `enrollment date`
- **Categorical**: e.g., `hair color`
- **Ordinal** *(full STF only)*: e.g., `study phase` (`preliminary`, `phase one`, `phase two`)
- **Longitude** *(full STF only)*: treated separately due to its circular nature (see also [this issue](https://github.com/VEuPathDB/study-wrangler/issues/31))

Note that **dates must be provided in ISO-8601 format** without a time component.

## STF Directory Structure

The STF directory should contain:

- Tab-delimited entity files (`entity-<entity_name>.tsv`)
- Optional entity metadata YAML files (`entity-<entity-name>.yaml`)
- Optional study-level metadata file (`study.yaml`)

All these are described in more detail below.

### Example STF directory structure:
```
my_study/
├── study.yaml
├── entity-household.tsv
├── entity-household.yaml
├── entity-participant.tsv
├── entity-participant.yaml
├── entity-observation.tsv
└── entity-observation.yaml
```

## STF (full format)

### Entity TSV files

STF entity files are tab-delimited tables where each row represents an
entity instance. The first column(s) define hierarchical relationships
through **parent ID columns**, linking child entities to their parent
records.

Each entity must have a unique identifier column, which is suffixed
with `\\ Descriptors` to indicate the start of data attributes. For
child entities, the first column is the parent ID, followed by the
child entity’s ID with the `\\ Descriptors` suffix. For deeper
hierarchies, you may provide all parent IDs or just the immediate
parent. See the example below.

For example:
- **Parent entity (`household`)**: The first column is `Household.Id \\ Descriptors`, as it has no parent.
- **Child entity (`participant`)**: The first column (`Household.Id`) references the parent, and the second column (`Participant.Id \\ Descriptors`) uniquely identifies each participant.
- **Grandchild entity (`observation`)**: Two options are available:
  - Provide *all* parent IDs in parent-to-child order: `Household.Id`, `Participant.Id`, `Part..Obs..Id \\ Descriptors`
  - Provide just the parent IDs: `Participant.Id`, `Part.Obs.Id \\ Descriptors`

#### Example `entity-household.tsv`:
| Household.Id \\\\ Descriptors | Number.of.animals | Owns.property | Enrollment.date | Construction.material |
|----------------------------|-------------------|---------------|----------------|----------------------|
| H001                       | 4                 | Yes           | 2021-01-09     | Concrete             |
| H002                       | 3                 | No            | 2021-02-28     | Timber               |
| H003                       | 3                 | Yes           | 2021-03-13     | Concrete             |


#### Example `entity-participant.tsv`:

| Household.Id | Participant.Id \\\\ Descriptors | Name    | Nickname | Sex    | Family.Role |
|-------------|--------------------------------|---------|----------|--------|-------------|
| H001        | H001-P1                        | Alice   | Di       | Female | Relative    |
| H001        | H001-P2                        | Bob     | Bobby    | Male   | Child       |
| H001        | H001-P3                        | Diana   | NA       | Female | Parent      |
| H002        | H002-P1                        | Charlie | Chuck    | Male   | Child       |
| H002        | H002-P2                        | Anna    | NA       | Female | Relative    |
| H003        | H003-P1                        | Helen   | Hells    | Female | Parent      |
| H003        | H003-P2                        | Eve     | NA       | Female | Child       |
| H003        | H003-P3                        | Michael | Micky    | Male   | Child       |


#### Example `entity-observation.tsv`:

(This is the all-parents version.)

| Household.Id | Participant.Id | Part..Obs..Id \\\\ Descriptors | Observation.date | Height..cm. | Weight..kg. | MUAC..cm. | Teeth.brushed.today |
|-------------|---------------|--------------------------------|------------------|-------------|-------------|-----------|--------------------|
| H001        | H001-P1       | H001-P1-Obs1                   | 2023-09-29       | 145         | 35          | 15.82     | 1                  |
| H001        | H001-P2       | H001-P2-Obs1                   | 2023-05-04       | 135         | 47          | NA        | 1                  |
| H001        | H001-P2       | H001-P2-Obs2                   | 2023-11-30       | 129         | 80          | 28.81     | 0                  |
| H001        | H001-P3       | H001-P3-Obs1                   | 2023-04-21       | 133         | 51          | NA        | 1                  |
| H002        | H002-P1       | H002-P1-Obs1                   | 2023-08-08       | 175         | 32          | 26.68     | NA                 |
| H002        | H002-P1       | H002-P1-Obs2                   | 2023-10-18       | 172         | 34          | 12.59     | 1                  |
| H002        | H002-P2       | H002-P2-Obs1                   | 2023-02-28       | 160         | 74          | 19.83     | 2                  |
| H003        | H003-P1       | H003-P1-Obs1                   | 2023-05-02       | 150         | 40          | 31.84     | 1                  |
| H003        | H003-P2       | H003-P2-Obs1                   | 2023-01-30       | 140         | 38          | 17.5      | 0                  |
| H003        | H003-P3       | H003-P3-Obs1                   | 2023-01-05       | 130         | 45          | NA        | NA                 |
| H003        | H003-P3       | H003-P3-Obs2                   | 2023-08-07       | 127         | 91          | NA        | 1                  |


### Entity Metadata YAML files
These files define entity properties as well as 
ID columns, variables, their types, and metadata.

Full reference documentation for all the metadata fields [will be provided soon](https://github.com/VEuPathDB/study-wrangler/issues/32).

#### Example `entity-household.yaml`:
```yaml
name: household
display_name: household
display_name_plural: households

id_columns:
  - id_column: Household.Id
    entity_name: household

variables:
  - variable: Number.of.animals
    display_name: Number of animals
    data_type: integer
    data_shape: continuous

  - variable: Owns.property
    display_name: Owns property
    data_type: string
    data_shape: categorical

  - variable: Enrollment.date
    display_name: Enrollment date
    data_type: date
    data_shape: continuous
```

### Study YAML file

The `study.yaml` file provides high-level study metadata and lists the
names of the entities in the study. The STF loader will only load the
files `entity-<entity_name>.{tsv,yaml}` as defined in this file. This
means you can keep work-in-progress or other versions of entity files
in the directory without them being loaded.

```yaml
name: My Awesome Study
entities:
  - household
  - participant
  - observation
```

## STF-Lite

STF-Lite is a minimal, metadata-free variant suitable for quick or simple data submissions.

- Filenames must follow the convention: `entity-<ENTITY_NAME>.tsv`
- No separate metadata YAML files are required.
- The first column headings must match entity names exactly.

### Example STF-Lite entity TSV file:

| household \\\\ Descriptors | Number.of.animals | Owns.property | Enrollment.date | Construction.material |
|----------------------------|-------------------|---------------|----------------|----------------------|
| H001                       | 4                 | Yes           | 2021-01-09     | Concrete             |
| H002                       | 3                 | No            | 2021-02-28     | Timber               |
| H003                       | 3                 | Yes           | 2021-03-13     | Concrete             |


## Comparison of Full STF vs. STF-Lite

| Feature                                  | Full STF ✅                                     | STF-Lite ⚠️                                |
|------------------------------------------|------------------------------------------------|-------------------------------------------|
| **Custom entity names & display names**  | ✅ Supported via metadata YAML                 | ❌ Not supported, entity names must match ID columns |
| **Data type enforcement**                | ✅ Enforced via metadata definitions           | ❌ No enforcement, at mercy of automatic type detection |
| **Ordinal variable support**             | ✅ Metadata allows explicit ordinal definitions | ❌ Not supported |
| **ID column flexibility**                | ✅ Custom ID column names allowed              | ❌ ID column headers must match entity names exactly |
| **Validation**                           | ✅ Ensures entity relationships and data integrity | ⚠️ Limited to basic TSV parsing only |


## TO DO

* more on STF Lite and row-wise parent-child checks
* metadata reference
