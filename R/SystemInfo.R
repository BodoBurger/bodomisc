#' Get essential system information
#'
#' @return [\code{SystemInfo}]
#' @export
SystemInfo = function() {
  session.info = utils::sessionInfo()
  sys.info = Sys.info()
  R.version.string = session.info$R.version$version.string
  R.version.nickname = session.info$R.version$nickname
  system.name = sys.info["sysname"]
  system.release = sys.info["release"]
  system.running = session.info$running
  if (rstudioapi::isAvailable()) {
    user.interface = paste0("RStudio ", rstudioapi::getVersion())
  } else user.interface = .Platform$GUI
  # if (exists("RStudio.Version")) { # method without RStudio API
  #   user.interface = paste0("RStudio ", as.character(RStudio.Version()$version))
  # } else user.interface = .Platform$GUI
  # session infos:
  working.directory = getwd()
  base.packages = session.info$basePkgs
  n.other.packages = length(session.info$otherPkgs)
  other.packages = character(n.other.packages)
  for (i in (1:n.other.packages)) {
    pkg = session.info$otherPkgs[[i]]
    other.packages[i] = paste(pkg$Package, pkg$Version)
  }
  return(structure(list(
    system.name = system.name,
    system.release = system.release,
    system.running = system.running,
    R.version.string = R.version.string,
    R.version.nickname = R.version.nickname,
    user.interface = user.interface,
    working.directory = working.directory,
    base.packages = base.packages,
    other.packages = other.packages
  ), class = "SystemInfo")
)
}

#' @export
print.SystemInfo = function(x, ...) {
  cat(                "------ General -----\n")
  cat(  crayon::green("   Operating system: "), x$system.running, " (", x$system.name, " ",
    x$system.release, ")\n", sep = "")
  cat(   crayon::blue("          R version:"), crayon::bold(gsub("^R version ", "", x$R.version.string)),
    x$R.version.nickname, "\n")
  cat( crayon::yellow("     User interface:"), x$user.interface, "\n")
  cat(                "------ Session -----\n")
  cat(crayon::magenta("  Working directory:"), x$working.directory, "(change it using",
    crayon::italic("setwd()"), ")\n")
  cat( crayon::silver("      Base packages: "), paste0(x$base.packages, collapse = ", "), "\n", sep = "")
  n.attached = length(x$other.packages)
  cat(   crayon::cyan("Packages (attached): "), paste0(x$other.packages, collapse = ", "), sep = "")
  cat(" (", n.attached, " packages attached)\n", sep = "")
  cat("\n")
}