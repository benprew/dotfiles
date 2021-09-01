#!/usr/bin/env python3

import sys


def vaex_read(fn):
    import vaex

    df = vaex.open(fn)
    print(df.schema())
    print(df)


def pandas_read(fn):
    import pandas as pd

    df = pd.read_parquet(fn, engine="pyarrow")

    # print(df.columns)
    pd.set_option("display.max_columns", None)
    print(df.info(verbose=True))
    print(df.head(3))


def pyarrow_read(source):
    from pyarrow.parquet import ParquetFile
    import pprint

    # Source is either the filename or an Arrow file handle (which could be on HDFS)

    # TODO: figure out how to read from s3 directly
    args = {}
    if "s3://" in source:
        args["filesystem"] = "s3"

    m = ParquetFile(source, pre_buffer=True).metadata
    pp = pprint.PrettyPrinter(indent=4)
    pp.pprint(m)


fn = sys.argv[1]

# vaex_read(fn)
# pyarrow_read(fn)
pandas_read(fn)
