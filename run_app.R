invisible(lapply(readLines('./Maps/dependencies.txt'), library, character.only = T))
rm(list = ls(all = T))

# invisible(lapply(paste0("./R/", list.files("./R")), source))
invisible(lapply(paste0("./Helpers/", list.files("./Helpers")), source))
invisible(lapply(c('./App/server.R', './App/ui.R'), source))

plan(multisession)
runApp(shinyApp(ui, server), launch.browser = T, port = 8815)
