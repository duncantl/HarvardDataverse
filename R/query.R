# https://www.nature.com/articles/s41597-022-01143-6

library(RCurl)
library(RJSONIO)


getDOIs =
function(url = "https://dataverse.harvard.edu/api/search",
          lang = "r",  isHarvested = FALSE, max = Inf, perPage = 1000L, con = getCurlHandle(.opts = .opts), .opts = list(...))
{
    
    query = sprintf("(fileContentType:type/x-%s-syntax AND isHarvested:%s", lang, tolower(as.logical(isHarvested)))
    getResults(url, query = query, start = 0, max = max, perPage = perPage)
}

getResults =
function(url, query, start = 0L, max = Inf, perPage = 1000L, verbose = TRUE)    
{
    ans = list()
browser()    
    while(start < max) {

        if(verbose)
            message("starting at ", start)

        js = getForm(url, q = query, start = start, perPage = perPage)
        v = fromJSON(js)

        dt = v$data
        ans = c(ans, dt$items)
        start = start + dt$count_in_response
browser()
        if(length(ans) >= dt$total_count)
            break
    }

    invisible(ans)
}



