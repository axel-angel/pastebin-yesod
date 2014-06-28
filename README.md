Paste
=====

Usage:
------

    <command> | curl -F 'file=@-' http://localhost:3000

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

**Expiration**: One can specify the number of seconds after which the
file should be deleted, use the "expire" field.

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

    curl -F 'file=@/path/to/file' http://localhost:3000
    <command> | curl -F 'file=@-;type=image/jpeg;filename=myphoto.jpg' http://localhost:3000
    <command> | curl -F 'file=@-' -F 'expire=600' http://localhost:3000

There is no guarantee of serivces of any kind. Note your IP address is
stored for security reasons.

Coded with [Yesod](http://www.yesodweb.com/) in Haskell by Axel Angel
for GNU Generation, 2014.
