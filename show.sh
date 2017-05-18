#!/bin/bash

for filename in output/*.pdf; do
	xdg-open $filename 2> /dev/null
done
