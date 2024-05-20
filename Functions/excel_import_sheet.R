excel_import_sheet <-
function(path, sheet_list, col_types = NULL, skip = 0) {
  sheet <- sheet_list
   map(sheet,
      ~read_excel(path = path, 
      col_names = TRUE,
      col_types = col_types,
      sheet = .x, 
      skip = skip
      )
   )
}


excel_import_all_sheets <-
function(path, col_types = NULL, skip = 0) {
  sheet_list <- excel_sheets(path)
  excel_import_sheet(path, sheet_list, col_types, skip)
}