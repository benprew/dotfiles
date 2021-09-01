#!/usr/bin/env python3

import sys

import boto3

s3 = boto3.client("s3")
lines = []
bucket = sys.argv[1]

with open(sys.argv[2], "r") as fh:
    lines = fh.readlines()

print(f"Deleting {len(lines)} objects")

for i in range(0, len(lines), 1000):
    s = i
    e = min(i + 1000, len(lines))
    print(s, e)
    objs = list(map(lambda x: {"Key": x.rstrip()}, lines[s:e]))
    resp = s3.delete_objects(
        Bucket=bucket,
        Delete={"Objects": objs},
    )

    if "Deleted" not in resp:
        raise Exception(f"Error deleting files {resp}")

    print(f"Deleted {len(resp['Deleted'])} items")
