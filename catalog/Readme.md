# Python Intake simplified data access

Load a catalog file and have access to preconfigured data sources in your code

[Intake](https://intake.readthedocs.io/en/latest/) base and Intake [xarray/rasterio](https://intake-xarray.readthedocs.io/en/latest/) plugin.

```python

import pandas as np
import numpy as np
import rasterio as rio
import xarray as xr

import intake


if __name__ == "__main__":

    cat = intake.open_catalog("test-catalog.yaml")
    print(list(cat))

    ds2 = cat.reola_flow
    print(ds2)
    ddf = ds2.to_dask()
    print(ddf.compute().head())

    ds3 = cat.eest_dem_z19
    print(ds3)
    raster = ds3.read()
    print(raster.meta)

```
