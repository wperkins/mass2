# This way of generating doxygen docs with autotools was copied from
# the tvmet distribution

DOXY_DOC = 

HTML_MISC = 

LTEX_MISC = 

EXTRA_DIST = \
	$(DOXY_DOC) \
	$(HTML_MISC) \
	$(LTEX_MISC) \
	Doxyfile.in

DISTCLEANFILES = \
	Doxyfile doxygen-warning

# some depencies

Doxyfile: 	Doxyfile.in

#
# rules for documentation, if ordered
#
if CONFIG_DOC

#
# file created on the fly
#
DOXY_BUILD = \
	changelog.dox \
	todo.dox

DOXY_HTML_SRC = \
	$(DOXY_DOC) \
	$(DOXY_BUILD) \
	$(patsubst %.in, %, $(HTML_MISC))

DOXY_LTEX_SRC = \
	$(patsubst %.in, %, $(LTEX_MISC))

# file creating rules
changelog.dox: @top_srcdir@/ChangeLog
	@echo "/** \page changelog ChangeLog" > $@
	@echo "\verbatim" >> $@
	cat $< >> $@
	@echo "\endverbatim" >> $@
	@echo "*/" >> $@

todo.dox: @top_srcdir@/TODO
	@echo "/** \page big_todo TODO" > $@
	@echo "\verbatim" >> $@
	cat $< >> $@
	@echo "\endverbatim" >> $@
	@echo "*/" >> $@

#
# doxygen API
#
DOC_API_PDF = $(PACKAGE)-$(VERSION).pdf

doxygen: doxygen-api

# need's LaTeX style, since we use an own style for LaTeX
# which is required for generating formulas in HTML too.
# Doxygen LaTeX batchmode doesn't solves the problem right here.
doxygen-html-dir:
	@if test ! -d ./html; then mkdir ./html; fi

doxygen-api: doxygen-html-dir Doxyfile $(DOXY_HTML_SRC)
	@echo "Making HTML manual"
	@DOXYGEN@ $(DOXYGEN_OPTS)

#
# doxygen LaTeX API
#
noinst_DATA = $(DOC_API_PDF)

dvi-local:
ps-local:
pdf-local: $(DOC_API_PDF)

$(DOC_API_PDF): Doxyfile $(DOXY_LTEX_SRC)
	@if test -d ./latex; then \
		echo "Making PDF manual"; \
		if test -f $@; then \
			$(RM) $@; \
		fi; \
		$(MAKE) -C ./latex refman.pdf; \
		$(LN_S) ./latex/refman.pdf $@; \
	fi


#
# misc hooks
#
distclean-local:
	@rm -f $(DOXY_BUILD)
	@if test -f $(DOC_API_PDF); then \
		$(RM) -f $(DOC_API_PDF); \
	fi
	@if test -d html; then \
		$(RM) -rf html; \
	fi
	@if test -d latex; then \
		$(RM) -rf latex; \
	fi
	@if test -d rtf; then \
		$(RM) -rf rtf; \
	fi
	@if test -d man; then \
		$(RM) -rf man; \
	fi

doc-dist: doxygen-api
	echo "please wait while archiving the html docs."; \
	tar cf - -C ./html . | bzip2 --best -c > $(PACKAGE)-docs-$(VERSION).tar.bz2

# where to install all html documents
DOC_DIR = $(datadir)/doc/$(PACKAGE)-$(VERSION)

# Automake's "distcheck" is sensitive to having files left over
# after "make uninstall", so we have to clean up the install hook.
uninstall-local:
	@if test -d $(DOC_DIR); then \
		rm -rf $(DOC_DIR); \
	fi

# Install hooks
make-install-dirs:
	@if test '!' -d $(DOC_DIR); then \
		$(mkinstalldirs) $(DOC_DIR); \
	fi

install-data-hook: make-install-dirs
	@echo Installing documentations into $(DOC_DIR)
	@echo "install html"
	@$(INSTALL_DATA) @top_builddir@/doc/html/* $(DOC_DIR)

else
doxygen:
doc-dist:
install-data-hook:
endif	# CONFIG_DOC


