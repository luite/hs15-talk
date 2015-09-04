#!/bin/sh
pandoc -c extra.css --filter filter.hs -H header.tmpl --smart --highlight-style zenburn -V theme:serif -s -f markdown -t revealjs hs15.markdown -o hs15.html
