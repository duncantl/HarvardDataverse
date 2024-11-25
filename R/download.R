getFiles =
function(urls, dir = options("HarvardDataverseDir", stop("need HarvardDataverseDir")),
         dois = basename(urls))
{
    out = file.path(dir, dois)
    mapply(download.file, urls, out)
    invisible(out)
}

getFiles2 =
function(urls, dir = options("HarvardDataverseDir", stop("need HarvardDataverseDir")),
         dois = basename(urls), curl = getCurlHandle())
{
    out = file.path(dir, dois)
    mapply(function(url, file) {
        content = getURLContent(url, curl = curl, binary = TRUE)
        writeBinary(content, out)
        }, urls, out)
    invisible(out)
}

writeBinary =
function (object, con, size = NA_integer_, endian = .Platform$endian, 
    useBytes = FALSE) 
{
    swap <- endian != .Platform$endian
    if (is.character(con)) {
        con <- file(con, "wb")
        on.exit(close(con))
    }
    .Internal(writeBin(object, con, size, swap, useBytes))
    con
}
