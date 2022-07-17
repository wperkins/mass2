# MASS2 CGNS Utilities

This contains a number of utilities for processing MASS2 CGNS output.
Most of these were written for very specific purposes.

## Prerequisites

These utilites are written in C++ and require several libraries to
build:
- [Blitz](https://github.com/blitzpp/blitz) multi-dimensional array
  library 
- [Boost C++ Libraries](https://www.boost.org/)
- [CFD General Notation System](https://cgns.github.io/) 

## Commands

### mass2extract

`mass2extract` extracts a time series for a specific MASS2 cell from
one or more CGNS files.

```
Usage: mass2extract [options]
Available options:
  --help                produce this help message
  --verbose             produce some diagnostic messages
  --base arg (=1)       CGNS base node index
  --zone arg (=1)       Index of zone whose solutions should be used
  --no-skip             do not skip adjacent, indentically named solutions
  --field arg           Solution field(s) of interest
  --all                 Extract all fields from the specified solution
  --metric              Convert output values to SI units
  --i arg (=0)          The longitudinal cell index (1-based)
  --j arg (=0)          The lateral cell index (1-based)
  --solution-list arg   Read file/solution list from named file
  --cgns-input arg      input CGNS file(s)
  --output arg          Write statistics output to specified file
```

### mass2flux

This utility computes fluxes of transported quantities at a specific
location over time from one or more CGNS files.

```
Usage: mass2flux [options]
Available options:
  --help                produce this help message
  --verbose             produce some diagnostic messages
  --base arg (=1)       CGNS base node index
  --zone arg (=1)       Index of zone whose solutions should be used
  --field arg           Solution field of interest -- average is computed by 
                        default
  --i-index arg (=-1)   Compute laterally at the specified index
  --no-skip             do not skip (adjacent) indentically named solutions
  --cgns-input arg      input CGNS file
```

### mass2grass

`mass2grass` is used to import MASS2 results into GIS (GRASS,
e.g.). It extracts a list of points, with solution fields as
attributes, from a single MASS2 solution time into a delimited text
file.  

```
mass2grass: extract MASS2 solution in GRASS point format
Usage: mass2grass [options] [ file.cgns [field] [field] ... 
Available options:
  --help                produce this help message
  --verbose             produce some diagnostic messages
  --base arg (=1)       CGNS base node index
  --zone arg (=1)       Index of zone whose solutions should be used
  --solution arg (=1)   CGNS solution index to extract
  --field arg           Solution field(s) of interest
  --all                 Extract all fields from the specified solution
  --header              write a header with column names to output
  --xmin arg (=-1e+30)  Lower limit output x (east) coordinates
  --xmax arg (=1e+30)   Upper limit output x (east) coordinates
  --ymin arg (=-1e+30)  Lower limit output y (north) coordinates
  --ymax arg (=1e+30)   Upper limit output y (north) coordinates
  --separator arg (=|)  Field separator charactor
  --cgns-input arg      input MASS2 CGNS file
  --output arg          Write output to named file
```

### mass2insert

`mass2insert` is used to insert a solution field into a MASS2 CGNS
file.  

```
Usage: mass2insert [options]
Available options:
  --help                       produce this help message
  --verbose                    produce some diagnostic messages
  --base arg (=1)              CGNS base node index
  --zone arg (=1)              Index of zone whose solutions should be used 
                               (input file value is ignored)
  --solution-index arg (=1)    Index of flow solution in which to insert or 
                               overwrite the field
  --field arg (=InsertedValue) Name of solution field(s) to insert
  --scale arg (=1)             Multiply the input field by this number to scale
                               (e.g. unit conversion)
  --units arg                  Units of inserted field
  --description arg            Description of inserted field
  --cgns-input arg             MASS2 CGNS file to change
  --csv-input arg              input CSV file
```

### mass2scan

`mass2scan` scans one or more MASS2 CGNS files and prepares a list
that can be used as input to PT6 or with the `--solution-list` options
of other utilities.  It tries very hard to skip overlapping solution
times, if any. 

```
mass2scan: scan MASS2 CGNS files and produce a PT6 input file
Usage: mass2scan [options]
Available options:
  --help                produce this help message
  --verbose             produce some diagnostic messages
  --no-skip             do not try to skip time overlap -- put all time slices 
                        in output
  --no-time             do not put the elapsed time, s, in first output field
  --zero arg            date/time (YYYY-Mon-dd HH:MM:SS) at which time is 0.0
  --no-negative-time    Do not include times that are less than --zero
  --cgns-input arg      input MASS2 CGNS file
  --output arg          Write output to named file
```

The output looks like this

```
         0.0 /pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     1 # 08-17-2013 00:00:00
      3600.0 /pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     2 # 08-17-2013 01:00:00
      7200.0 /pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     3 # 08-17-2013 02:00:00
     10800.0 /pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     4 # 08-17-2013 03:00:00
     14400.0 /pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     5 # 08-17-2013 04:00:00
```
The first field is the elapsed time in seconds. This format is
produced with `--no-time`:

```
/pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     1 # 08-17-2013 00:00:00
/pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     2 # 08-17-2013 01:00:00
/pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     3 # 08-17-2013 02:00:00
/pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     4 # 08-17-2013 03:00:00
/pic/scratch/perk/mass2_4_sfa/2013a/plot000.cgns     5 # 08-17-2013 04:00:00

```
The latter can be used with other utilities that have the
`--solution-list` option

### mass2stats

`mass2stats` computes statistics for one or more solution fields over
time for each MASS2 cell (optionally limited by coordinate range).  

```
Usage: mass2stats [options]
Available options:
  --help                produce this help message
  --verbose             produce some diagnostic messages
  --base arg (=1)       CGNS base node index
  --zone arg (=1)       Index of zone whose solutions should be used
  --wet                 include cell wet count and percent wet fields
  --no-skip             do not skip (adjacent) indentically named solutions
  --field arg           Solution field(s) of interest
  --metric              Convert output values to SI units
  --xmin arg (=-1e+30)  Lower limit output x (east) coordinates
  --xmax arg (=1e+30)   Upper limit output x (east) coordinates
  --ymin arg (=-1e+30)  Lower limit output y (north) coordinates
  --ymax arg (=1e+30)   Upper limit output y (north) coordinates
  --solution-list arg   Read file/solution list from named file
  --cgns-input arg      input CGNS file(s)
  --output arg          Write statistics output to specified file
```

### mass2dewater

`mass2dewater` is used to compute changes in river area over time,
particularly as the water surface is falling.  It identifies
*stranding* and *entrapped* area.  *Stranding* is the area that
dewaters as the water surface falls. *Entrapped* is the area that
becomes isolated from the river, but remains wet, as the water surface
falls.  

```
Usage: mass2dewater [options]
Available options:
  --help                       produce this help message
  --verbose                    produce some diagnostic messages
  --base arg (=1)              CGNS base node index
  --zone arg (=1)              Index of zone whose solutions should be used
  --i-river arg (=-1)          Longitudinal index of a point known to be always
                               in the river
  --j-river arg (=-1)          Lateral index of a point known to be always in 
                               the river
  --begin arg                  Date/time (format: YYYY-Mon-dd HH:MM:SS) to 
                               begin extraction
  --end arg                    Date/time (format: YYYY-Mon-dd HH:MM:SS) to end 
                               extraction
  --auxiliary arg (=grid.cgns) Name of auxiliary CGNS file that matches 
                               --cgns-input
  --quadrant arg               Name of CSV file containing quadrant indexes for
                               each (i, j)
  --solution-list arg          Read file/solution list from named file
  --cgns-input arg             input CGNS file(s)
  --output arg                 Write statistics output to specified file
```


