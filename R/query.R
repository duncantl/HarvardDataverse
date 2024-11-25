# https://www.nature.com/articles/s41597-022-01143-6

library(RCurl)
library(RJSONIO)


# See https://guides.dataverse.org/en/latest/api/search.html
# for more parameters for the search query.

getDOIs =
function(lang = "r",  isHarvested = FALSE, max = Inf, perPage = 1000L, con = getCurlHandle(.opts = .opts), .opts = list(...),
         url = "https://dataverse.harvard.edu/api/search", wait = -1, start = 0L)
{
    getResults(url, query = mkQuery(lang, isHarvested), start = start, max = max, perPage = perPage)
}

mkQuery =
function(lang = "r", isHarvested = FALSE)
{
    sprintf("(fileContentType:type/x-%s-syntax AND isHarvested:%s", lang, tolower(as.logical(isHarvested)))
}

getNumMatches =
function(lang = "r", isHarvested = FALSE, url = "https://dataverse.harvard.edu/api/search")    
{
   ans = getPage(url, mkQuery(lang, isHarvested), 0, 10L)
   ans$data$total_count 
}


getResults =
function(url, query, start = 0L, max = Inf, perPage = 1000L, verbose = TRUE, wait = -1)    
{
    ans = list()

    while(start < max) {

        if(verbose)
            message("starting at ", start)

        v = getPage(url, query, start, perPage)

        dt = v$data
        ans = c(ans, dt$items)
        start = start + dt$count_in_response

        if(length(ans) >= dt$total_count)
            break

        if(wait > 0)
            Sys.sleep(wait)
    }

    invisible(ans)
}

getPage =
function(url, query, start, perPage)    
{
    js = getForm(url, q = query, start = start, per_page = perPage)
    v = fromJSON(js)
}



mkDF =
function(x)    
{
    scalars = c("name", "type", "url", "file_id", "published_at", "file_type", "file_content_type",
                "size_in_bytes", "md5", "dataset_name", "dataset_persistent_id",
                 "dataset_citation", "publicationStatuses", "releaseOrCreateDate")

    tmp = lapply(scalars, function(var) sapply(x, function(x) orNA(x[[var]])))
    names(tmp) = scalars
    # as.data.frame(tmp, check.names = FALSE)
    structure(tmp, class = "data.frame", row.names = seq_len(length(x)))
}


mkDF =
function(x)    
{
    scalars = c("name", "type", "url", "file_id", "published_at", "file_type", "file_content_type",
                "size_in_bytes", "md5", "dataset_name", "dataset_persistent_id",
                 "dataset_citation", "publicationStatuses", "releaseOrCreateDate")

    tmp = lapply(x, function(x) x[scalars])
    tmp2 = do.call(rbind, tmp)
    v = as.data.frame(tmp2)    
    v[] = lapply(v, function(x) { w = sapply(x, is.null); x[w] = NA })
    v[] = lapply(v, function(x) { w = sapply(x, length) > 1; x[w] = sapply(x[w], `[`, 1); unlist(x) })
    v
}

mkDF =
    # Faster version.
    # We delay coercing to a data.frame until after "fixing" the entries in each column.
    # This is about 3.5 times faster.
    # Note we don't have v[] = lapply(....), but simply v = lapply()
    # So less work to insert back into data.frame.
    # 
function(x)    
{
    scalars = c("name", "type", "url", "file_id", "published_at", "file_type", "file_content_type",
                "size_in_bytes", "md5", "dataset_name", "dataset_persistent_id",
                 "dataset_citation", "publicationStatuses", "releaseOrCreateDate")

    tmp = lapply(x, function(x) x[scalars])
    tmp2 = do.call(rbind, tmp)
    # Replace NULL values in each "column" with NA
    v = lapply(v, function(x) { w = sapply(x, is.null); x[w] = NA })
    # Now handle cases where we have more than one value in an entry.
    # Empirically, this is only publicationStatuses - so far.
    v = lapply(v, function(x) { w = sapply(x, length) > 1; x[w] = sapply(x[w], `[`, 1); unlist(x) })
    as.data.frame(tmp2)    
}


orNA =
function(x, val = NA)
{
    if(length(x))
        x
    else
        val
}
