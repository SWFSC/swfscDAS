# `das_df` class

The `das_df` class is a subclass of
[`data.frame`](https://rdrr.io/r/base/data.frame.html), created to
provide a concise and robust way to ensure that the input to downstream
DAS processing functions, such as
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md),
adheres to certain requirements. Specifically, objects of class `das_df`
are data frames with specific column names and classes, as detailed in
the 'Properties of `das_df`' section. Objects of class `das_df` are
created by
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md)
or
[`as_das_df`](https://swfsc.github.io/swfscDAS/reference/as_das_df.md),
and are intended to be passed directly to DAS processing functions such
as
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md).

Subsetting, say for a specific date or cruise number, or otherwise
altering an object of class `das_df` will cause the object to drop its
`das_df` class attribute. If this object is then passed to a DAS
processing function such as
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md),
the function will try to coerce the object to a `das_df` object.

## Properties of `das_df` objects

All values in the Event column must not be `NA`.

Objects of class `das_df` have a class attribute of
`c("das_df", "data.frame")`. In addition, they must have the following
column names and classes:

|               |                        |
|---------------|------------------------|
| *Column name* | *Column class*         |
| Event         | "character"            |
| DateTime      | c("POSIXct", "POSIXt") |
| Lat           | "numeric"              |
| Lon           | "numeric"              |
| OnEffort      | "logical"              |
| Cruise        | "numeric"              |
| Mode          | "character"            |
| EffType       | "character"            |
| Course        | "numeric"              |
| SpdKt         | "numeric"              |
| Bft           | "numeric"              |
| SwellHght     | "numeric"              |
| WindSpdKt     | "numeric"              |
| RainFog       | "numeric"              |
| HorizSun      | "numeric"              |
| VertSun       | "numeric"              |
| Glare         | "logical"              |
| Vis           | "numeric"              |
| ObsL          | "character"            |
| Rec           | "character"            |
| ObsR          | "character"            |
| ObsInd        | "character"            |
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
| EffortDot     | "logical"              |
| EventNum      | "integer"              |
| file_das      | "character"            |
| line_num      | "integer"              |

## See also

[`as_das_df`](https://swfsc.github.io/swfscDAS/reference/as_das_df.md)
