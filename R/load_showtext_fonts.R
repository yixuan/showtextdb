#' Load Fonts for the 'showtext' Package
#' 
#' This function loads fonts that will be used by the \pkg{showtext} package.
#' 
#' @export
#' @author Yixuan Qiu <\url{https://statr.me/}>
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
    {
        ## Test for write permission
        if(file.access(out_dir, mode = 2) < 0)
        {
            msg = paste("the temporary directory ", out_dir,
                        " does not have write permission,\n",
                        "failed to load the 'WenQuanYi Micro Hei' font", sep = "")
            warning(msg)
            return(invisible(NULL))
        }
        utils::unzip(font_file, exdir = out_dir, overwrite = FALSE)
    }

    ## Add font to sysfonts
    sysfonts::font_add("wqy-microhei", out_file)

    invisible(NULL)
}

## Load fonts that have been installed by the user
load_user_fonts = function()
{
    ## The directory that contains the user fonts
    font_db = system.file("fonts", package = "showtextdb")

    ## Installed fonts
    family_names = font_installed()
    if(length(family_names) < 1)
        invisible(NULL)

    ## Scan all possible font faces
    faces = c("regular", "bold", "italic", "bolditalic", "symbol")
    ## Add fonts one by one
    font_dirs = file.path(font_db, family_names)
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
            ## Family names returned by font_installed() are guaranteed to contain
            ## at least the regular font face, but we still test it just to be safe
            if(is.null(args$regular))
            {
                msg = paste("the file for the \"regular\" font face is missing, ",
                            "font \"", family_names[i], "\" is not loaded",
                            sep = "")
                warning(msg)
            } else {
                do.call(sysfonts::font_add, args)
            }
        }
    }

    invisible(NULL)
}
