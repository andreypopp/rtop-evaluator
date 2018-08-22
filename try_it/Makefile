all: toplevel.js

ifeq ($(shell ocamlc -safe-string 2> /dev/null ; echo $$?),0)
SAFESTRING=-safe-string
else
SAFESTRING=-package bytes
endif


JSFILES= +toplevel.js +dynlink.js +nat.js

PACKAGES= \
	sketch_evaluator \
	dynlink

ifeq ($(WITH_ASYNC_JS),YES)
PACKAGES += async_js core_kernel async_kernel base
JSFILES += +base/runtime.js +core_kernel/runtime.js +bin_prot/runtime.js +ppx_expect/collector/runtime.js
endif

ifeq ($(WITH_PPX),YES)
PACKAGES += js_of_ocaml-ppx.as-lib
endif

ifeq ($(WITH_GRAPHICS),YES)
JSFILES += +graphics.js
PACKAGES += graphics js_of_ocaml-lwt.graphics
endif

#-jsopt "--pretty"
MKTOP=jsoo_mktop -verbose $(SAFESTRING) -g \
	${addprefix -top-syntax , ${SYNTAXES}} \
	-jsopt "--disable shortvar --pretty" \
	${addprefix -export-package , ${PACKAGES}} \
	# ${addprefix -export-unit $(TYXML_DIR)/, html_types.cmi html_sigs.cmi xml_wrap.cmi } \

TOPLEVEL_NAME=toplevel
$(TOPLEVEL_NAME).js:
	$(MKTOP) \
	$(BER) \
	${addprefix -jsopt , ${JSFILES}} \
	-o $(TOPLEVEL_NAME).byte

clear::
	rm *.js *.byte
