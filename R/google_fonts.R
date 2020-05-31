#' Meta-information for the Google Fonts
#'
#' This function provides information of Google Fonts
#' that can be used in the \code{\link{font_install}()} function.
#' It will try to include all font faces available for a specified family name.
#'
#' @param name Name of the font that will be searched in Google Fonts.
#' @param \dots Other parameters passed to \code{curl::curl_fetch_memory()}.
#' @export
#' @author Yixuan Qiu <\url{https://statr.me/}>
#' @examples opensans = google_fonts("Open Sans")
#' print(opensans)
#'
#' \dontrun{
#' ## Install the Open Sans font to the showtexdb package
#' font_install(opensans)
#' }
google_fonts = function(name, ...)
{
    ## In the sysfonts package we get the URLs of font files using the JSON API.
    ## However, it will download the database of all available fonts, which is
    ## unnecessary. Instead, we use the "CSS API" that directly query one
    ## specific font.

    ## Escape spaces
    esc_name = gsub("[[:space:]]", "+", name)
    ## Request URL
    request_url = sprintf("https://fonts.googleapis.com/css?family=%s:r,b,i,bi", esc_name)
    ## Returned text
    ret = curl::curl_fetch_memory(request_url, ...)
    res = rawToChar(ret$content)

    ## If successful, the returned text begins with "@font-face", otherwise
    ## a piece of HTML will be given
    if(!grepl("^@font-face", res))
        stop(sprintf("the requested font family \"%s\" is not available", name))

    ## Parse CSS
    info = unlist(strsplit(res, "@font-face", fixed = TRUE))
    info = info[info != ""]
    ## Font style (normal/italic)
    style = gsub(".*font-style:[[:space:]]*([^:;]+);.*", "\\1", info)
    ## Font weight (400/700)
    weight = gsub(".*font-weight:[[:space:]]*([[:digit:]]+);.*", "\\1", info)
    ## Font face = style + weight
    face = paste(style, weight, sep = "")
    ## regular == normal400, bold == normal700
    ## italic == italic400, bolditalic == italic700
    face[face == "normal400"] = "regular"
    face[face == "normal700"] = "bold"
    face[face == "italic400"] = "italic"
    face[face == "italic700"] = "bolditalic"
    if(!("regular" %in% face))
        stop(sprintf("the \"regular\" font face is unavailable for font \"%s\"", name))
    ## Font URL
    url = gsub(".*url\\(([^()]+)\\).*", "\\1", info)
    url = as.list(url)
    names(url) = face
    url = url[intersect(c("regular", "bold", "italic", "bolditalic"), names(url))]
    names(url) = paste(names(url), "_url", sep = "")

    res = c(showtext_name = name,
            font_ext = gsub(".*\\.([[:alnum:]]+)$", "\\1", url$regular_url),
            url)
    res
}
