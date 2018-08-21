#' Get essential system information
#'
#' @return [\code{SystemInfo}]
#' @export
SystemInfo = function() {
  session.info = sessionInfo()
  sys.info = Sys.info()
  R.version.string = session.info$R.version$version.string
  R.version.nickname = session.info$R.version$nickname
  system.name = sys.info["sysname"]
  system.release = sys.info["release"]
  system.running = session.info$running
  if (rstudioapi::isAvailable()) {
    user.interface = paste0("RStudio (", rstudioapi::getVersion(), ")")
  } else user.interface = .Platform$GUI

  # session infos:
  working.directory = getwd()
  attached.packages = sort(sub("^package:", "", search()[grepl("^package:", search())]))

  return(structure(list(
    system.name = system.name,
    system.release = system.release,
    system.running = system.running,
    R.version.string = R.version.string,
    R.version.nickname = R.version.nickname,
    user.interface = user.interface,
    working.directory = working.directory,
    attached.packages = attached.packages
  ), class = "SystemInfo")
)
}

#' @export
print.SystemInfo = function(SI) {
  cat(                "------ General -----\n")
  cat(  crayon::green("   Operating system: "), SI$system.running, " (", SI$system.name, " ",
    SI$system.release, ")\n", sep = "")
  cat(   crayon::cyan("          R version:"), crayon::bold(gsub("^R version ", "", SI$R.version.string)),
    SI$R.version.nickname, "\n")
  cat( crayon::yellow("     User interface:"), SI$user.interface, "\n")
  cat(                "------ Session -----\n")
  cat(crayon::magenta("  Working directory:"), SI$working.directory, "(change it using",
    crayon::italic("setwd()"), ")\n")
  cat( crayon::silver("Packages (attached): "), paste0(SI$attached.packages, collapse = ", "), " (",
    length(SI$attached.packages), " packages attached)", "\n", sep = "")
  #cat(                "--------------------\n")
}