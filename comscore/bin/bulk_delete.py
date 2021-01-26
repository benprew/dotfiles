#!/usr/bin/env python3

import sys

import boto3

s3 = boto3.client("s3")
lines = []

with open(sys.argv[1], "r") as fh:
    lines = fh.readlines()

for i in range(0, len(lines), 1000):
    s = i
    e = min(i + 1000, len(lines))
    print(s, e)
    objs = list(map(lambda x: {"Key": x.rstrip()}, lines[s:e]))
    s3.delete_objects(
        Bucket="csps-csdmap-as-data",
        Delete={"Objects": objs},
    )
