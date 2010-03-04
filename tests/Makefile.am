## Process this file with automake to produce Makefile.in

TESTS = check_heman
check_PROGRAMS = check_heman
check_heman_SOURCES = check_heman.c $(top_builddir)/src/stats.c $(top_builddir)/src/stats.h $(top_builddir)/src/hashtable.c $(top_builddir)/src/hashtable.h
check_heman_CFLAGS = @CHECK_CFLAGS@ -g -Wall
check_heman_LDADD = @CHECK_LIBS@
