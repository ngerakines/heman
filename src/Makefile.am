## Process this file with automake to produce Makefile.in

bin_PROGRAMS = client heman

heman_SOURCES = heman.c heman.h heman_types.h hashtable.c hashtable.h  hashtable_private.h stats.c stats.h rules.c rules.h health.c health.h  hashtable_itr.h hashtable_itr.c commands.c commands.h globals.h
heman_CFLAGS = $(OPTIMIZATION) -Wall $(ARCH) $(PROF)

client_SOURCES = client.c
client_CFLAGS = $(OPTIMIZATION) -Wall $(ARCH) $(PROF)
