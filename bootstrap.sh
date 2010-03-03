#!/bin/sh

aclocal
automake --gnu --add-missing
autoheader
autoconf