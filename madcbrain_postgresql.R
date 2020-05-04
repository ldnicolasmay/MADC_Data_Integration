# madcbrain_postgresql.R

# Load Libraries

library(DBI)
# library(odbc)
# library(RPostgres)
library(dplyr)
library(stringi)


# Make a connection to the `madc_integ` database on the `madcbrain` server
con <- dbConnect(RPostgres::Postgres(), 
                 service  = "madcbrain pgsql madc_integ", # ~/.pg_service.conf
                 user     = rstudioapi::askForPassword("Username:"),
                 password = rstudioapi::askForPassword("Password:"))
dbListTables(con)

# Load the list of dfs that hold all the MADC Data Integration (MDI) data
date_str <- Sys.Date()
ls_dfs_u2_u3a_u3n <-
  readRDS(paste0("MDI Data/", date_str, "/ls_dfs_u2_u3a_u3n.Rds"))

# Drop views
dbExecute(con, "DROP VIEW IF EXISTS c2_neuropsych CASCADE;")
dbExecute(con, "DROP VIEW IF EXISTS d1_ift_madcdx_reqs CASCADE;")

for (df_name in stri_remove_empty_na(names(ls_dfs_u2_u3a_u3n))) {

  cat(paste0("Processing ", df_name, "...\n"))
  
  # If the df in `ls_dfs_u2_u3a_u3n` is of dimensions 0x0
  if (identical(dim(ls_dfs_u2_u3a_u3n[[df_name]]), c(0L, 0L))) {
    
    cat(paste0("  No data in `ls_dfs_u2_u3a_u3n[[\"", df_name, "\"]]`\n"))
    
    # Else the df has non-zero dimensions
  } else {
    
    if (dbExistsTable(con, df_name)) {
      cat(paste0("  Table `", df_name, 
                 "` in database `", DBI::dbGetInfo(con)$dbname,
                 "` already exists; dropping with cascade\n"))
      
      dbExecute(con, paste0("DROP TABLE ", df_name, " CASCADE;"))
    }
    
    # Create temporary df to write to `madc_integ` db, 
    # ensuring that the primary keys exist for every record
    df_tmp <- ls_dfs_u2_u3a_u3n[[df_name]] %>% 
      filter(!is.na(ptid)) %>% 
      filter(!is.na(form_date))
    
    # Write `df_tmp` to `madc_integ` db
    dbWriteTable(con, df_name, df_tmp,
                 overwrite = TRUE, append = FALSE)
    
    # Define primary key fields of written table
    DBI::dbExecute(
      con,
      paste0("ALTER TABLE ", df_name, " ADD PRIMARY KEY (ptid, form_date);")
    )
    
    DBI::dbExecute(
      con,
      paste0("GRANT SELECT ON ", df_name, " TO ldmay;")
    )
    DBI::dbExecute(
      con,
      paste0("GRANT SELECT ON ", df_name, " TO readerj;")
    )
  }
}

# Create views
dbExecute(con, readr::read_file("sql/c2_neuropsych.sql"))
dbExecute(con, readr::read_file("sql/d1_ift_madcdx_reqs.sql"))

dbDisconnect(con)
