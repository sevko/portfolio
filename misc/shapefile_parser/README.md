# Shapefile reader/writer
`shapefile.py` contains an incomplete implementation of an ESRI shapefile reader and writer (`.shx` and `.dbf` files
aren't accounted for), developed when I was trying to familiarize myself with the format. The module implements a
`Shapefile` class, with methods `read_from_file()` and `write_to_file()`, and supports the following shapes:

  * `NullShape`
  * `Point`
  * `MultiPoint`
  * `Polygon`

The module can be run as a command-line executable, with the following flags:

  * `--test` : run the module's unit tests.
  * `--read FILE_PATH` : read in the shapefile specified by `FILE_PATH`.
  * `--help` : print usage help information.

If imported into another Python module, `shapefile` can be used as follows:
```python
shapefile = Shapefile("in.shp")
shapefile.shapes.append(Point(13, 20))
shapefile.write_to_file("out.shp")
```
