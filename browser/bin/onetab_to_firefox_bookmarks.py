#!/usr/bin/env python3

from html.parser import HTMLParser
from datetime import datetime
import sys

# To Export Onetab URLs with groups
# Use Onetab's "share as webpage" then save the page as html


class OneTabParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_group_label = False
        self.in_link = False
        self.current_group = None
        self.groups = []
        self.link_href = ""
        self.link_text = ""

    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)
        if tag == "div" and attrs_dict.get("class") == "tabGroupLabel":
            self.in_group_label = True
        elif tag == "a" and attrs_dict.get("class") == "tabLink":
            self.in_link = True
            self.link_href = attrs_dict.get("href", "")

    def handle_endtag(self, tag):
        if tag == "div" and self.in_group_label:
            self.in_group_label = False
        elif tag == "a" and self.in_link:
            if self.current_group is not None:
                self.current_group["links"].append(
                    (self.link_text.strip(), self.link_href)
                )
            self.in_link = False
            self.link_text = ""
            self.link_href = ""

    def handle_data(self, data):
        if self.in_group_label:
            self.current_group = {"title": data.strip(), "links": []}
            self.groups.append(self.current_group)
        elif self.in_link:
            self.link_text += data


def export_to_firefox_bookmarks(groups, output_path):
    now_ts = str(int(datetime.now().timestamp()))
    lines = [
        "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
        '<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">',
        "<TITLE>Bookmarks</TITLE>",
        "<H1>Bookmarks</H1>",
        "<DL><p>",
    ]

    for group in groups:
        lines.append(
            f'  <DT><H3 ADD_DATE="{now_ts}" LAST_MODIFIED="{now_ts}">{group["title"]}</H3>'
        )
        lines.append("  <DL><p>")
        for text, href in group["links"]:
            lines.append(f'    <DT><A HREF="{href}" ADD_DATE="{now_ts}">{text}</A>')
        lines.append("  </DL><p>")

    lines.append("</DL><p>")

    with open(output_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))

    print(
        f"Created {output_path} with {sum(len(g['links']) for g in groups)} bookmarks."
    )


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(
            "Usage: python onetab_to_firefox_bookmarks_stdlib.py input.html output.html"
        )
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    with open(input_path, "r", encoding="utf-8") as f:
        html = f.read()

    parser = OneTabParser()
    parser.feed(html)
    export_to_firefox_bookmarks(parser.groups, output_path)
