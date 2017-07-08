#' Installing Fonts To The 'showtextdb' Package
#' 
#' This function saves the specified font to the \file{fonts} directory of the
#' \pkg{showtextdb} package, so that it can be used by the \pkg{showtext}
#' package.
#' 
#' @export
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' @examples \dontrun{
#' install_fonts(source_han_serif())
#' }
install_fonts = function(font_desc, quiet = FALSE)
{
    font_desc = as.list(font_desc)
    name = font_desc$sysfonts_name
    ext  = font_desc$font_ext
    
    ## Create a directory with the font family name
    font_dir = file.path(system.file("fonts", package = "showtextdb"), name)
    if(!dir.exists(font_dir))
        dir.create(font_dir)

    ## font_desc must contain a URL for the regular font face
    if(!("regular_url" %in% names(font_desc)))
        stop("'font_desc' must contain a component named 'regular_url'")
    
    regular_file = file.path(font_dir, sprintf("regular.%s", ext))
    if(!file.exists(regular_file))
    {
        if(!quiet)
            message("downloading the 'regular' font face...")
        curl::curl_download(font_desc$regular_url, regular_file, quiet = quiet)
    }
    
    other_faces = c("bold", "italic", "bolditalic", "symbol")
    for(face in other_faces)
    {
        face_url = font_desc[[sprintf("%s_url", face)]]
        if(!is.null(face_url))
        {
            face_file = file.path(font_dir, sprintf("%s.%s", face, ext))
            if(!file.exists(face_file))
            {
                if(!quiet)
                    message(sprintf("downloading the '%s' font face...", face))
                curl::curl_download(face_url, face_file, quiet = quiet)
            }

        }
    }
    
    ## Load the newly installed font
    load_user_fonts()
    
    invisible(NULL)
}
