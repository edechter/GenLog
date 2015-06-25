
SHELL = /bin/sh
srcdir = ./src

OBJDIR := build
LIBDIR := lib

dir:
	mkdir -p $(OBJDIR)

$(OBJDIR)/digamma.o  : dir
	$(CC) $(CPPFLAGS) -fPIC  -c $(srcdir)/digamma.c -o $@

$(OBJDIR)/digamma.dylib : $(OBJDIR)/digamma.o dir
	$(CC) -shared -Wl $(LDFLAGS) -lswipl $< -o $@

$(OBJDIR)/multinom.o  :  dir
	$(CC) $(CPPFLAGS) -fPIC  -c $(srcdir)/multinom.c -o $@

$(OBJDIR)/multinom.dylib : $(OBJDIR)/multinom.o dir
	$(CC) -shared -Wl $(LDFLAGS) -lswipl $< -o $@

build   : dir $(OBJDIR)/digamma.dylib $(OBJDIR)/multinom.dylib

libdir  : 
	mkdir -p $(LIBDIR)

lib     : libdir build
	cp -v $(OBJDIR)/digamma.dylib $(LIBDIR)/ 
	cp -v $(OBJDIR)/multinom.dylib $(LIBDIR)/ 	
clean: 
	rm -v $(OBJDIR)/* 
	rmdir $(OBJDIR)
	rm -v $(LIBDIR)/*
	rmdir $(LIBDIR)

