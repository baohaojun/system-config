Import and export of vCards as defined in RFC 2425 and RFC 2426
to/from The Insidious Big Brother Database (BBDB) v3.

For user and developer documentation, refer to the bundled BBDB vCard
info manual.

Mapping of vCard types to BBDB types:

+-------------------------+-----------------------------------------+
| VCARD TYPE;PARAMETERS   | STORAGE IN BBDB                         |
|                         |                                         |
|-------------------------+-----------------------------------------|
| VERSION                 | -                                       |
| REV                     | -                                       |
|-------------------------+-----------------------------------------|
| N                       | First occurrence:                       |
|                         | Firstname                               |
|                         | Lastname                                |
|                         |                                         |
|                         | Rest:                                   |
|                         | AKAs (append)                           |
|-------------------------+-----------------------------------------|
| FN                      | AKAs (append)                           |
| NICKNAME                | AKAs (append)                           |
|-------------------------+-----------------------------------------|
| ORG                     | Organizations (append)                  |
|-------------------------+-----------------------------------------|
| ADR;TYPE=x,HOME,y       | Addresses<Home                          |
| ADR;TYPE=x;TYPE=HOME    | Addresses<Home                          |
| ADR;TYPE=x,WORK,y       | Addresses<Office                        |
| ADR;TYPE=x;TYPE=WORK    | Addresses<Office                        |
| ADR;TYPE=x,y,z          | Addresses<x,y,z                         |
| ADR;TYPE=x;TYPE=y       | Addresses<x,y                           |
| ADR                     | Addresses<Office                        |
|-------------------------+-----------------------------------------|
| TEL;TYPE=x,HOME,y       | Phones<Home (append)                    |
| TEL;TYPE=x;TYPE=HOME    | Phones<Home (append)                    |
| TEL;TYPE=x,WORK,y       | Phones<Office (append)                  |
| TEL;TYPE=x;TYPE=WORK    | Phones<Office (append)                  |
| TEL;TYPE=x,CELL,y       | Phones<Mobile (append)                  |
| TEL;TYPE=x;TYPE=CELL    | Phones<Mobile (append)                  |
| TEL;TYPE=x,y,z          | Phones<x,y,z (append)                   |
| TEL;TYPE=x;TYPE=y       | Phones<x,y (append)                     |
| TEL                     | Phones<Office (append)                  |
|-------------------------+-----------------------------------------|
| EMAIL;TYPE=x,y,z        | Net-Addresses (append)                  |
| URL                     | Xfields<url                             |
|-------------------------+-----------------------------------------|
| BDAY                    | Xfields<anniversary (append as birthday)|
| X-BBDB-ANNIVERSARY      | Xfields<anniversary (append)            |
|-------------------------+-----------------------------------------|
| PHOTO (inline base64)   | Xfields<image-filename                  |
|       (uri)             | Xfields<image-uri                       |
|-------------------------+-----------------------------------------|
| SOUND (inline base64)   | Xfields<sound-filename                  |
|       (uri)             | Xfields<sound-uri                       |
|-------------------------+-----------------------------------------|
| KEY   (inline base64)   | Xfields<gpg-key-filename                |
|       (uri)             | Xfields<gpg-key-uri                     |
|-------------------------+-----------------------------------------|
| NOTE                    | Xfields<notes (append)                  |
| CATEGORIES              | Xfields<mail-alias (append)             |
| SORT-STRING             | Xfields<sort-string                     |
| GEO                     | Xfields<geo                             |
| TZ                      | Xfields<tz                              |
| LABEL                   | Xfields<label                           |
| LOGO                    | Xfields<logo                            |
| TITLE                   | Xfields<title                           |
| ROLE                    | Xfields<role                            |
| AGENT                   | Xfields<agent                           |
| MAILER                  | Xfields<mailer                          |
| UID                     | Xfields<uid                             |
| PRODID                  | Xfields<prodid                          |
| CLASS                   | Xfields<class                           |
| X-BBDB-FOO              | Xfields<foo                             |
| X-FOO                   | Xfields<x-foo                           |
|-------------------------+-----------------------------------------|
| ANYJUNK;a=x;b=y         | Xfields<anyjunk;a=x;b=y                 |
+-------------------------+-----------------------------------------+
