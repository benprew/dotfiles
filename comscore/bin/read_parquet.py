#!/usr/bin/env python3

import sys

import pandas as pd

fn = sys.argv[1]

df = pd.read_parquet(fn, engine="pyarrow")

print(df.columns)
