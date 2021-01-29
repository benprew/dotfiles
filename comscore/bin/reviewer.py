#!/usr/bin/env python3

import os
import sys

as_dev = ["mfoster", "rchurch", "bprew", "cgoughnour", "mgenova"]


def reviewer(pr_owner, pr):
    reviewers = [x for x in as_dev if x != pr_owner]
    return reviewers[pr % len(reviewers)]


print(reviewer(os.getenv("USER"), int(sys.argv[1])))
