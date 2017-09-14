#' @title create_template
#' @description This function builds a template directory  with all the necessary files, as well as a rmarkdown script which is ready to edit.
#' The result is an Elsevier ready document that conforms to most academic journal standards.
#' In the directory folder provided, the user will have the tex files, as well as all relevant figure folders created in R. These should not be renamed or moved.
#' Prior to submitting a paper, the user should zip the template_name.tex file, together with the figure files and Tex folder, and send it through to journal editors.
#' Feel free to edit any of the settings in the template_name.tex file to conform to particular preferences (e.g. setting margins, etc.)
#' @param directory A character string with the target folder directory This folder will be created if it does not already exist.
#' @param template_name The folder, as well as the rmarkdown template, will be given this name.
#' @param bib.location This can be a user specified location for a bib file. If omitted, the default bib file will be included. Note this command will create a renamed copy in the provided directory root.
#' @param launch_template TRUE by default, this will launch the rmarkdown after creating the directory. The Working directory will also be changed.
#' @return If launch_template is TRUE, the template Rmarkdown file will be opened in a new R session. This can then be used as a starting point for an academic research paper.
#' @examples create_template(directory = "C:/Temp", template_name = "Project", launch_template = TRUE)
#' @export

create_template <- function (directory, template_name, bib.location, launch_template = TRUE, ShowPDFatLaunch = FALSE)
{
  sink(tempfile())
  ifelse(!require(devtools, quietly = T), return(cat("Devtools library not found")),
         FALSE)
  tex.file.root <- system.file("Tex", package = "Texevier")
  rmd.file.root <- system.file("Template", package = "Texevier")
  data.file.root <- system.file("Data", package = "Texevier")
  code.file.root <- system.file("Code", package = "Texevier")
  if (missing(directory) | is.na(directory) == T) {
    directory <- "C:/Template"
  }
  if (missing(template_name)) {
    template_name <- "Template"
  }
  sink()
  if (file.exists(directory)) {
    answer <- readline(cat("--------------- \n PROMPT: \n \n Path: ",
                           file.path(directory), " provided already exists. \n Proceed to create template in this folder? Type: Y or N....\n"))
    while (!(answer %in% c("Y", "y", "N", "n"))) {
      answer <- readline(cat("--------------- \n PROMPT: \n---------------\n Invalid input, please type Y in order to create template folder in path: ",
                             file.path(directory), " OR type N to quit \n"))
    }
    if (answer %in% c("Y", "y")) {
      cat("Creating folder and executing Tex Platform creation")
      sink(tempfile())
      Sys.sleep(1)
      dir.create( file.path(directory, "template"), showWarnings = FALSE)
      directory <- file.path(directory, "template")
    }
    else {
      return(cat("!Template not created!"))
    }
  }
  else {
    sink(tempfile())
    mkdirs <- function(fp) {
      if (!file.exists(fp)) {
        mkdirs(dirname(fp))
        dir.create(fp)
      }
    }
    ifelse(!dir.exists(directory), mkdirs(directory), FALSE)
  }
  dir.create(file.path(directory, "Tex"), showWarnings = FALSE)
  dir.create(file.path(directory, "Code"), showWarnings = FALSE)
  dir.create(file.path(directory, "Data"), showWarnings = FALSE)
  if (!missing(bib.location) && !file.exists(bib.location)) {
    sink()
    stop(cat(paste0("\n bib file: \n", bib.location, " \n does not exist. Leave this parameter blank to create default .bibfile, or check bib.location provided.")))
  }
  if (missing(bib.location)) {
    file.copy(list.files(tex.file.root, full.names = TRUE),
              to = file.path(directory, "Tex"))
    file.copy(list.files(rmd.file.root, full.names = TRUE),
              to = directory)
  }
  else {
    bib_name <- unlist(strsplit(bib.location, "/"))[length(unlist(strsplit(bib.location,
                                                                           "/")))]
    file.copy(list.files(file.path(tex.file.root), full.names = TRUE)[!grepl("ref.bib",
                                                                             list.files(tex.file.root, full.names = TRUE))], to = file.path(directory,
                                                                                                                                            "Tex"))
    file.copy(list.files(bib.location, full.names = TRUE),
              to = file.path(directory, "Tex"))
    file.copy(list.files(rmd.file.root, full.names = TRUE),
              to = directory)
    file.rename(from = list.files(file.path(directory, "Tex"),
                                  full.names = TRUE)[grepl(bib_name, list.files(tex.file.root,
                                                                                full.names = TRUE))], to = paste0(file.path(directory,
                                                                                                                            "Tex"), "/refs.bib"))
  }
  file.copy(list.files(data.file.root, full.names = TRUE),
            to = file.path(directory, "Data"))
  file.copy(list.files(code.file.root, full.names = TRUE),
            to = file.path(directory, "Code"))
  if (!missing(template_name)) {
    template_name <- gsub(".Rmd", "", template_name)
    file.rename(from = list.files(directory, full.names = TRUE)[grepl(".Rmd",
                                                                      list.files(directory, full.names = TRUE))], to = paste0(file.path(directory,
                                                                                                                                        template_name), ".Rmd"))
  }
  if (launch_template) {
    library(rmarkdown)
    WD <- getwd()
    on.exit(setwd(WD))
    setwd(directory)
    on.exit(setwd(directory))
    rmarkdown::render(paste0(file.path(directory, template_name),
                             ".Rmd"), output_format = "pdf_document", envir = new.env())
    file.edit(paste0(file.path(directory, template_name),
                     ".Rmd"))

    if(ShowPDFatLaunch){
      shell.exec(paste0(file.path(directory, template_name),
                        ".PDF"))
      }
    sink()
    cat("\n\n  ------------------ README: \n\n  Your Template has successfully built the template PDF. \n\n  Close the illustrated PDF after building it (If ShowPDFatLaunch = TRUE). A PDF Cannot be built on an open PDF. \n\n  ------------------\n      \n Proceed to edit your template, and Press Cntrl + Shift + K to knit this into a pdf. A viewer will then appear showing the new pdf just built.\n      \n I suggest creating a .Rproj in your new directory before working further.\n      \n Visit http://rmarkdown.rstudio.com/ for tips on writing in R.\n      \n To customize the layout, change the code above between the first and second ``` in the template.")
  }
}
