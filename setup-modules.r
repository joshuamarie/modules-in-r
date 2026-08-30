if (!file.exists("_quarto.yml")) {
    stop("run this from the project root, next to _quarto.yml")
}

chapters = list.dirs(".", recursive = FALSE, full.names = FALSE)
chapters = chapters[grepl("^Chapter [0-9]+$", chapters)]

extract_r_chunks = function(file) {
    lines = readLines(file, warn = FALSE)
    chunk_start = grep("^```\\{r", lines)
    chunk_end = grep("^```\\s*$", lines)
    
    chunks = character(0)
    for (start in chunk_start) {
        end = chunk_end[chunk_end > start][1]
        if (!is.na(end)) {
            chunks = c(chunks, lines[(start + 1):(end - 1)])
        }
    }
    chunks
}

needs_module = function(chapter) {
    files = list.files(chapter, pattern = "\\.qmd$", full.names = TRUE)
    any(vapply(files, function(f) any(grepl("\\./module", extract_r_chunks(f))), logical(1)))
}

chapters = chapters[vapply(chapters, needs_module, logical(1))]

target = normalizePath("module")

link_module = function(chapter) {
    link = file.path(chapter, "module")
    
    if (dir.exists(link)) {
        message(link, " already exists, skipping")
        return(invisible())
    }
    
    if (.Platform$OS.type == "windows") {
        shell(sprintf('mklink /J "%s" "%s"', link, target))
    } else {
        file.symlink(target, link)
    }
    
    message("linked ", link, " -> ", target)
}

invisible(lapply(chapters, link_module))
