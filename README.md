Paste
=====

Usage:
------

    <command> | curl -F 'file=@-' -H 'Expect:' http://localhost:3000

Features:
---------

-   Very easy to use
-   Supports binary files
-   Keeps the Content-Type
-   Preserves filenames, sent as Content-Disposition
-   Optional expiration (self-destruct)

Details
-------

**File**: Send a POST request to this page with your data in the "file"
field name, the entire url uploaded is sent back or a HTTP 302 is
replied.

**Expiration**: One can specify the number of minutes after which the
file should be deleted. Specified by the "expire" field. Alternatively
you can specify the time unit: **s**(econds), **h**(ours), **d**(ays),
**w**(eeks), **m**(onths). Example: 120 (2 minutes), 2m (2 months).

**Content-Type**: It allows the browser or client to infer types of
files to correctly display them. cURL cannot infer the type of piped
input so you could explicitly give the content type (1st below) or pass
it the file directly (2nd below) so it can find the type itself (look at
man curl).

**Content-Disposition**: It allows to suggest a filename to the browser.
That means we can store filenames we received and provide them
afterward. cURL takes the filename of the path or you can provide it
separately (see below).

Examples
--------

    curl -F 'file=@/path/to/file' -H 'Expect:' http://localhost:3000
    <command> | curl -F 'file=@-;type=image/jpeg;filename=myphoto.jpg' -H 'Expect:' http://localhost:3000
    <command> | curl -F 'file=@-' -F 'expire=1d' -H 'Expect:' http://localhost:3000

**Expect**: If you are wondering about this Expect, this is a HTTP
header that our server does not support, so we say cURL not to send it
and it just works as expected :)

There is no guarantee of services of any kind. Note your IP address is
stored for security reasons.

Coded with [Yesod](http://www.yesodweb.com/) in Haskell by Axel Angel
for GNU Generation, 2014.
