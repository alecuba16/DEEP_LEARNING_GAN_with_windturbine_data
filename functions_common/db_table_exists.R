db_table_exists <- function(table_test, dbtype = "historical",db_config) {
    
    if(!exists("db_query")) source("functions_common/db_query.R")

    dbtype <- tolower(dbtype)
    
    qtest <- paste("SELECT 1 FROM ",table_test," LIMIT 1;")
    
    if(dbtype == "historical") {
        db_config$dbname <- "yourHistoricalBD"
        r <- try( db_query(query=qtest,db_config=db_config), silent = TRUE )
    }
    else if(dbtype == "realtime") {
        db_config$dbname <- "smartcast_DB"
        r <- try( db_query(query=qtest,db_config=db_config), silent = TRUE )
    }
    
    if(inherits(r, "try-error")) return(FALSE)
    
    return(TRUE)
}