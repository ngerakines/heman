# Makefile lifted (w/ minor changes) from Jacob Vorreuter's emongo project; thanks
.PHONY: templates
VERSION=0.0.1
PKGNAME=heman
LIBDIR=$(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
ROOTDIR=$(shell erl -eval 'io:format("~s~n", [code:root_dir()])' -s init stop -noshell)

all: rel templates
	
compile: app
	mkdir -p ebin/
	(cd src;$(MAKE))

app:
	sh ebin/$(PKGNAME).app.in $(VERSION)

test: compile templates
	prove t/*.t

cover: all
	COVER=1 prove t/*.t
	erl -detached -noshell -eval 'etap_report:create()' -s init stop

templates: 
	mkdir -p ./ebin/
	erl -noshell -eval "erltl:compile(\"./templates/heman_troot.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop
	erl -noshell -eval "erltl:compile(\"./templates/heman_tnamespace.et\", [{outdir, \"./ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.boot *.rel *.script ebin/*.beam ebin/*.app *.beam

rel: compile
	erl -pa ebin -noshell -run heman build_rel -s init stop
	
package: clean
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin include Makefile priv README.markdown src support t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/
	
install:
	@mkdir -p $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/{ebin,include}
	@mkdir -p $(prefix)/$(ROOTDIR)/bin
	for i in ebin/*.beam include/*.hrl ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/$(PKGNAME)-$(VERSION)/$$i ; done
	cp *.boot $(prefix)/$(ROOTDIR)/bin/

dev:
	erl -pa ./ebin -boot start_sasl -eval "[application:start(X) || X <- [sasl, mnesia, crypto, heman]]"
