#!/usr/bin/env python

import os
import sys


def reviewer(pr_owner, pr):
    reviewers = list(
        filter(
            lambda x: x != pr_owner,
            ["mfoster", "rchurch", "bprew", "cgoughnour", "mgenova"],
        )
    )
    return reviewers[pr % len(reviewers)]


print(reviewer(os.getenv("USER"), int(sys.argv[1])))
