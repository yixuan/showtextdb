#' Meta-information for the Source Han Sans/Serif Fonts
#' 
#' These functions provide information of the Source Han Sans/Serif fonts
#' that can be used in the \code{\link{font_install}()} function.
#' Source Han Sans/Serif fonts provide complete support for the
#' CJK (\strong{C}hinese, \strong{J}apanese, and \strong{K}orean) characters.
#' 
#' @name source_han
#' 
#' @param lang Language of the font. "CN" for simplified Chinese, "TW" for
#'             traditional Chinese, "JP" for Japanese, and "KR" for Korean.
#' @export
#' @author Yixuan Qiu <\url{http://statr.me/}>
#' @examples \dontrun{
#' ## Install Source Han Sans font (by default Simplified Chinese)
#' ## to the showtexdb package
#' font_install(source_han_sans())
#' 
#' ## Source Han Serif Japanese
#' font_install(source_han_serif("JP"))
#' }
source_han_sans = function(lang = c("CN", "TW", "JP", "KR"))
{
    lang = match.arg(lang)
    list(
        showtext_name = sprintf("source-han-sans-%s", tolower(lang)),
        font_ext      = "otf",
        regular_url   = sprintf("https://github.com/adobe-fonts/source-han-sans/raw/release/SubsetOTF/%s/SourceHanSans%s-Regular.otf", lang, lang),
        bold_url      = sprintf("https://github.com/adobe-fonts/source-han-sans/raw/release/SubsetOTF/%s/SourceHanSans%s-Bold.otf", lang, lang)
    )
}

#' @rdname source_han
source_han_serif = function(lang = c("CN", "TW", "JP", "KR"))
{
    lang = match.arg(lang)
    list(
        showtext_name = sprintf("source-han-serif-%s", tolower(lang)),
        font_ext      = "otf",
        regular_url   = sprintf("https://github.com/adobe-fonts/source-han-serif/raw/release/SubsetOTF/%s/SourceHanSerif%s-Regular.otf", lang, lang),
        bold_url      = sprintf("https://github.com/adobe-fonts/source-han-serif/raw/release/SubsetOTF/%s/SourceHanSerif%s-Bold.otf", lang, lang)
    )
}
