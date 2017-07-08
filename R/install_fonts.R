#' Install Fonts to the 'showtextdb' Package
#' 
#' This function saves the specified font to the \file{fonts} directory of the
#' \pkg{showtextdb} package, so that it can be used by the \pkg{showtext}
#' package.
#' 
#' @param font_desc A list that provides necessary information of the font
#'                  for installation. See the \strong{Details} section.
#' @param quiet Whether to show the progress of downloading and installation.
#' 
#' @details \code{font_desc} should contain at least the following components:
#' 
#' \describe{
#'   \item{\code{showtext_name}}{The family name of the font that will be used in
#'                               \pkg{showtext}.}
#'   \item{\code{font_ext}}{Extension name of the font files, e.g., \code{ttf} for
#'                          TrueType, and \code{otf} for OpenType.}
#'   \item{\code{regular_url}}{URL of the font file for "regular" font face.}
#' }
#' 
#' Optionally, \code{font_desc} can also contain \code{bold_url}, \code{italic_url},
#' \code{bolditalic_url}, and \code{symbol_url} that provide the URLs of the other
#' font faces.
#' 
#' See \code{\link{source_han_sans}()} and \code{\link{source_han_serif}()}
#' for an example of the \code{font_desc} parameter.
#' 
#' @export
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' @examples \dontrun{
#' install_fonts(source_han_serif())
#' }
install_fonts = function(font_desc, quiet = FALSE)
{
    font_desc = as.list(font_desc)
    name = font_desc$showtext_name
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
