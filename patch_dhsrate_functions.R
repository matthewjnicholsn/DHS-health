patch_dhsrate_functions <- function(indicator_list) {
  for (indicator in indicator_list) {
    # get the function for each indicator
    fun <- get(indicator, envir = asNamespace("DHS.rates"))
    
    # Convert function body to text
    fun_lines <- deparse(body(fun))
    
    # Find the line starting with 'dstrat'
    dstrat_line <- grep("^\\s*dstrat", fun_lines)[1]
    
    # The line to insert
    insert_line <- "DeathEx <- subset(DeathEx, !is.na(v021) & !is.na(v022) & !is.na(rweight))"
    
    # Insert your line before the dstrat line
    if (!is.na(dstrat_line)) {
      fun_lines <- append(fun_lines, insert_line, after = dstrat_line - 1)
    } else {
      warning(paste("No dstrat line found in", indicator))
    }
    # reconstruct the function with the same arguments as the original
    args <- paste(names(formals(fun)), collapse = ", ")
    fun_def <- sprintf("function(%s) {\n%s\n}", args, paste(fun_lines, collapse = "\n"))
    new_fun <- eval(parse(text = fun_def))
    
    # assign the new function to the environment with the same name as the indicator 
    assign(indicator, new_fun, envir = .GlobalEnv)
  }
  invisible(NULL)
}