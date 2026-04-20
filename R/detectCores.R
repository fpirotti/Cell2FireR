#' detectCores
#'
#' @param all.tests  do all tests
#' @param logical do logical tests
#'
#' @returns number of cores
#' @export
#'
#' @examples 
#' #
detectCores <- function (all.tests = FALSE, logical = TRUE) 
{
  systems <- list(linux = "grep \"^processor\" /proc/cpuinfo 2>/dev/null | wc -l", 
                  darwin = if (logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null", 
                  solaris = if (logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l", 
                  freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", openbsd = "/sbin/sysctl -n hw.ncpuonline 2>/dev/null")
  nm <- names(systems)
  m <- pmatch(nm, R.version$os)
  m <- nm[!is.na(m)]
  if (length(m)) {
    cmd <- systems[[m]]
    if (!is.null(a <- tryCatch(suppressWarnings(system(cmd, 
                                                       TRUE)), error = function(e) NULL))) {
      a <- gsub("^ +", "", a[1])
      if (grepl("^[1-9]", a)) 
        return(as.integer(a))
    }
  }
  if (all.tests) {
    for (i in seq(systems)) for (cmd in systems[i]) {
      if (is.null(a <- tryCatch(suppressWarnings(system(cmd, 
                                                        TRUE)), error = function(e) NULL))) 
        next
      a <- gsub("^ +", "", a[1])
      if (grepl("^[1-9]", a)) 
        return(as.integer(a))
    }
  }
  NA_integer_
}