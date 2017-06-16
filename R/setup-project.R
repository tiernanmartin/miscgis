#' Set up a project directory
#'
#' Creates the folders and appends .gitignore
#'
#' @name setup_project
#' @export

setup_project <- function(){

        # Append the `./gitignore` file

        system('echo ".httr-oauth" >> .gitignore')


        # Create directories

        inputs <- paste0("./1-data/", c("1-raw",
                                        "2-external",
                                        "3-interim",
                                        "4-ready"))

        lapply(inputs, dir.create, showWarnings = FALSE, recursive = TRUE)

        comms <- paste0("./2-communication/", c("1-rnotebooks", "1-rnotebooks/archive",
                                                "1-rnotebooks/tmp", "2-bookdown", "3-rmarkdown", "4-shinyapps",
                                                "others", "others/images", "others/msword", "others/pdf",
                                                "others/spatial"))

        lapply(comms, dir.create, showWarnings = FALSE, recursive = TRUE)

        dir.create(path = "./3-resources")

        dir.create(path = "./proj-setup-scripts")

        # Create .gitignore and .gitkeep files

        gitignore_cmd <- paste('echo "*\n!.gitignore" >',inputs,"/.gitignore",sep = "")

        input_gitkeep_cmd <- paste('touch ',inputs,"/.gitkeep",sep = "")

        comms_gitkeep_cmd <- paste('touch ',comms,"/.gitkeep",sep = "")

        lapply(gitignore_cmd,system)

        lapply(input_gitkeep_cmd,system)

        lapply(comms_gitkeep_cmd,system)

        system("touch ./3-resources/.gitkeep")

        return(NULL)
}


