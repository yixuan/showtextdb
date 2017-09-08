#' Load Fonts for the 'showtext' Package
#' 
#' This function loads fonts that will be used by the \pkg{showtext} package.
#' 
#' @export
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' @examples \dontrun{
#' sysfonts::font_families()
#' 
#' load_showtext_fonts()
#' sysfonts::font_families()
#' }
load_showtext_fonts = function()
{
    load_default_font()
    load_user_fonts()
}



## Check if a family has been loaded
already_loaded = function(family)
{
    all(family %in% sysfonts::font_families())
}

## Load the default WenQuanYi Micro Hei font
load_default_font = function()
{
    ## Check if wqy-microhei has been loaded already
    if(already_loaded("wqy-microhei"))
        return(invisible(NULL))
    
    ## Extract font file
    font_file = system.file("fonts", "wqy-microhei.ttc.zip", package = "showtextdb")
    out_dir = tempdir()
    out_file = file.path(out_dir, "wqy-microhei.ttc")
    if(!file.exists(out_file))
        utils::unzip(font_file, exdir = out_dir, overwrite = FALSE)
    
    ## Add font to sysfonts
    sysfonts::font_add("wqy-microhei", out_file)
    
    invisible(NULL)
}

## Load fonts that have been installed by the user
load_user_fonts = function()
{
    ## The directory that contains the user fonts
    font_db = system.file("fonts", package = "showtextdb")
    
    ## Each folder under fonts_db is considered a user font with different font faces
    font_dirs = list.dirs(font_db, recursive = FALSE)
    if(!length(font_dirs))
        return(invisible(NULL))
    
    ## Family names without the full path
    family_names = basename(font_dirs)
    ## Scan all possible font faces
    faces = c("regular", "bold", "italic", "bolditalic", "symbol")
    ## Add fonts one by one
    for(i in seq_along(family_names))
    {
        if(!already_loaded(family_names[i]))
        {
            font_files = list.files(font_dirs[i])
            args = lapply(faces, function(face) {
                r = grep(sprintf("^%s\\.[[:alnum:]]+$", face), font_files, value = TRUE)
                if(!length(r)) NULL else file.path(font_dirs[i], r)
            })
            names(args) = faces
            args = c(family = family_names[i], args)
            do.call(sysfonts::font_add, args)
        }
    }
    
    invisible(NULL)
}
