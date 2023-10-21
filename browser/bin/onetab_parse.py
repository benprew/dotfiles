#!/usr/bin/env python3

import sys
import re

with open(sys.argv[1], 'r') as fh:
    group = 'unknown'
    for l in fh:
        if 'tabGroupLabel' in l:
            m = re.search('>([^<]*)<', l)
            if not m:
                continue
            group = m[1] or "unknown"
            print(f"** {group}")
        if 'tabLink' in l:
            m = re.search('href="([^"]+)">([^<]+)<', l)
            if not m:
                continue
            url = m[1]
            title = m[2]
            print(f"- [[{url}][{title}]]")
            # print(group, url, title)
