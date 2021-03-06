# rmdoc - R Markdown Software Documentation



Provides:

- Functionality to extract data model diagrams from databases
- RStudio AddIn for rendering data model diagrams from yaml
- R Markdown template

## Installation


```r
devtools::install_github("bergant/datamodelr")
devtools::install_github("bergant/rmdoc")
```

## Setup

Add connection strings for databases in your .Renviron file like this:

```
RMDOC_CON_STRING_xxxxx=Driver={SQL Server};Server=ssssss;Database=ddddd;Uid=***;Pwd=***
RMDOC_CON_STRING_yyyyy=Driver={SQL Server};Server=eeeeee;Database=aaaaa;Uid=***;Pwd=***
```

where xxxxx and yyyyyy are your "logical" database names.

## Usage

### Default Database Connection
Add parameters to your rmd file yaml header (which database is used by default):

```yaml
params: 
  database: xxxxx
```

Include the following knitr chunk before your content:


```r
if(!is.null(params$database)) {
  .rmdoc_db_meta <- get_db_meta(database = params$database)
}
```

### Draw Data Model Diagrams

Draw datamodel diagrams like this: 


```r
display_graph(
  get_graph_from_yaml("my_data_model.yml")
)
```

The yaml should be in this format:

```yaml
# reverse engineer existing tables:
- extract_to: some_diagram_segment
  tables:
  - Suppliers
  - Products

# draw new tables (not in the database):
- table: Order Line
  segment: "Transactions"
  columns:
    Order ID: {key: yes, ref: Order}
    Line number: {key: yes}
    Order item: {ref: Item}
    Quantity:
    Price:
  
```

You can preview the data model in RStudio: 
open your yaml file and select from menu Addins/Preview Data Model
 
### Display Data from Database


```r
some_data <- get_database_data(params$database, "select * from MY_CODE_TABLE")
knitr::kable(some_data)
```


## License
MIT
