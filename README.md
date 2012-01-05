Rebar Plugins for working with Webmachine
=========================================


Dispatch Rules
--------------
`rebar_webmachine_dispatch` generates a dispatch.conf file for you based on modules that implement the behaviour `webmachine_resource`.
Hiearchical names are interpreted to give suitable dispatch rules:

* `.` is the separater;

* `_` is considered as a `'*'`;

* `_Param` is considered as an atom `'Param'`

* `Path` is considered as the string `"Path"`


Resources
---------
`rebar_webmachine_rest` generates a webmachine resource for a rest api for each module implementing `rest_resource`.

the generated resource module is compatible with `rebar_webmachine_dispatch` and will accept urles of the form:

* "MODULE/new"
* "MODULE/ID"

the module needs to export some or all of the following callbacks:

* `get/1` (argument is the `<ID>` as a binary)

* `put/1` for PUT/POST requests (argument is the result from `from_type/{1,2}`).

* `last_modified/1` for last modified header (argument is result of `get/1`)

* for PUT/POST request to new: `from_TYPE/1` (argument is the request entity)

* for PUT/POST requests to resource: `from_TYPE/2` (arguments are the request entity and the result from `get/1`)

* for GET requests to anything: `to_TYPE/1` (argument is the result of `get/1`).

* for DELETE requests `delete/1` (argument is the result of `get/1`, returns `deleting` or `deleted` or `not_deleted`)

* `available/0` can report the status of the resources described by the module

`TYPE` is one of: `xml` `json` `pdf` `html` corresponding to the expected mime types.

etags are generated by `phash/1` on the result of `get/1`.





 

