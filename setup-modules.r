link_module = function(chapter) {
    link = file.path(chapter, "module")
    
    if (dir.exists(link)) {
        message(link, " already exists, skipping")
        return(invisible())
    }
    
    ok = if (.Platform$OS.type == "windows") {
        shell(sprintf('mklink /J "%s" "%s"', link, target)) == 0
    } else {
        file.symlink(target, link)
    }
    
    if (!ok) {
        stop("failed to link ", link, " -> ", target)
    }
    
    message("linked ", link, " -> ", target)
}
