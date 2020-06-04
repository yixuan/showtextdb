#' Install Fonts to the 'showtextdb' Package
#' 
#' @description 
#' \code{font_install()} saves the specified font to the \file{fonts} directory of
#' the \pkg{showtextdb} package, so that it can be used by the \pkg{showtext}
#' package. This function requires the \pkg{curl} package.
#' 
#' \code{font_installed()} lists fonts that have been installed to
#' \pkg{showtextdb}.
#' 
#' \strong{NOTE}: Since the fonts are installed locally to the package directory,
#' they will be removed every time the \pkg{showtextdb} package is upgraded or
#' re-installed.
#' 
#' @param font_desc A list that provides necessary information of the font
#'                  for installation. See the \strong{Details} section.
#' @param quiet Whether to show the progress of downloading and installation.
#' @param \dots Other parameters passed to \code{curl::curl_download()}.
#' 
#' @details \code{font_desc} is a list that should contain at least the following components:
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
#' @author Yixuan Qiu <\url{https://statr.me/}>
#' @examples \dontrun{
#' ## Install Source Han Serif Simplified Chinese
#' font_install(source_han_serif())
#' 
#' ## List available font families
#' sysfonts::font_families()
#' 
#' ## Use the font with the "showtext" package
#' if(require(showtext)) {
#'     wd = setwd(tempdir())
#'     showtext.auto()
#'     
#'     pdf("source-han-serif.pdf")
#'     par(family = "source-han-serif-cn")
#'     plot.new()
#'     box()
#'     text(0.5, 0.9, "\u601d\u6e90\u5b8b\u4f53", cex = 3, font = 2)
#'     text(0.5, 0.4, "\u843d\u5176\u5b9e\u8005\u601d\u5176\u6811", cex = 3)
#'     text(0.5, 0.2, "\u996e\u5176\u6d41\u8005\u6000\u5176\u6e90", cex = 3)
#'     dev.off()
#'     setwd(wd)
#' }
#' }
font_install = function(font_desc, quiet = FALSE, ...)
{
    font_desc = as.list(font_desc)
    name = font_desc$showtext_name
    ext  = font_desc$font_ext

    ## font_desc must contain a URL for the regular font face
    if(!("regular_url" %in% names(font_desc)))
        stop("'font_desc' must contain a component named 'regular_url'")

    ## The directory that contains the user fonts
    font_db = system.file("fonts", package = "showtextdb")
    ## Test for write permission
    if(file.access(font_db, mode = 2) < 0)
    {
        msg = paste("the %s directory does not have write permission,",
                    "unable to install fonts", sep = "\n")
        stop(sprintf(msg, font_db))
    }

    ## Create a directory with the font family name
    font_dir = file.path(system.file("fonts", package = "showtextdb"), name)
    if(!dir.exists(font_dir))
        dir.create(font_dir)
    
    regular_file = file.path(font_dir, sprintf("regular.%s", ext))
    if(!file.exists(regular_file))
    {
        if(!quiet)
            message("downloading the 'regular' font face...")
        curl::curl_download(font_desc$regular_url, regular_file, quiet = quiet, ...)
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
                curl::curl_download(face_url, face_file, quiet = quiet, ...)
            }
        }
    }

    ## Load the newly installed font
    load_user_fonts()

    invisible(NULL)
}

#' @rdname font_install
font_installed = function()
{
    ## The directory that contains the user fonts
    font_db = system.file("fonts", package = "showtextdb")

    ## Each folder under fonts_db is considered a user font with different font faces
    font_dirs = list.dirs(font_db, recursive = FALSE)
    if(!length(font_dirs))
        return(character(0))

    ## We require that a legitimate font directory must contain a "regular" font face file
    has_regular = sapply(font_dirs, function(dir) {
        regular_file = list.files(dir, pattern = "^regular\\.[[:alnum:]]+$")
        length(regular_file) > 0
    })

    basename(font_dirs[has_regular])
}
