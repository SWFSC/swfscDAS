# `das_dfr` class

The `das_dfr` class is a subclass of
[`data.frame`](https://rdrr.io/r/base/data.frame.html), created to
provide a concise and robust way to ensure that the input to
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md)
adheres to certain requirements. Specifically, objects of class
`das_dfr` are data frames with specific column names and classes, as
detailed in the 'Properties of `das_dfr`' section. Objects of class
`das_dfr` are created by
[`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md) or
[`as_das_dfr`](https://swfsc.github.io/swfscDAS/reference/as_das_dfr.md),
and are intended to be passed directly to
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md).

Subsetting or otherwise altering an object of class `das_dfr` will cause
the object to drop its `das_dfr` class attribute.
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md)
will then try to coerce the object to a `das_dfr` object. It is
**strongly** recommended to pass an object of class `das_dfr` to
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md)
before subsetting, e.g. for events from a certain date range.

## Properties of `das_dfr` objects

Objects of class `das_dfr` have a class attribute of
`c("das_dfr", "data.frame")`. In addition, they must have the following
column names and classes:

|               |                        |
|---------------|------------------------|
| *Column name* | *Column class*         |
| Event         | "character"            |
| EffortDot     | "logical"              |
| DateTime      | c("POSIXct", "POSIXt") |
| Lat           | "numeric"              |
| Lon           | "numeric"              |
| Data1         | "character"            |
| Data2         | "character"            |
| Data3         | "character"            |
| Data4         | "character"            |
| Data5         | "character"            |
| Data6         | "character"            |
| Data7         | "character"            |
| Data8         | "character"            |
| Data9         | "character"            |
| Data10        | "character"            |
| Data11        | "character"            |
| Data12        | "character"            |
| EventNum      | "integer"              |
| file_das      | "character"            |
| line_num      | "integer"              |

## See also

[`as_das_dfr`](https://swfsc.github.io/swfscDAS/reference/as_das_dfr.md)
