OCAMLC=ocamlfind ocamlc
JSOO_MKTOP=jsoo_mktop

EXERCISES=bdd1.ml graphviz.ml

TOPLEVEL_LIBS=colorize.cmo ace.cmo ocp_indent.cmo promise.cmo \
    	      graphviz.cmo dragdiv.cmo svgpanzoom.cmo gv.cmo \
	      bdd.cmo

toplevel.js: $(TOPLEVEL_LIBS) toplevel.ml $(addprefix ../exercises/, $(EXERCISES))
	$(JSOO_MKTOP) -safe-string -g \
	    -jsopt "--disable shortvar --pretty" \
	    -jsopt +toplevel.js \
	    -jsopt +dynlink.js \
	    -jsopt -I -jsopt ../ -jsopt --file -jsopt exercises \
	    -export-package lwt \
	    -export-package js_of_ocaml \
	    -export-package js_of_ocaml-toplevel \
	    -export-unit gv.cmi \
	    -export-unit bdd.cmi \
	    -export-unit ace_types.cmi \
	    -package higlo \
	    -package ocp-indent.lib \
	    -package js_of_ocaml-lwt \
	    -package js_of_ocaml-tyxml \
	    -package js_of_ocaml-ppx \
	    -o $(@:.js=.byte) \
	    $(TOPLEVEL_LIBS) toplevel.ml

dist: basic.css \
      index.html \
      about.html \
      toplevel.js \
      exported-unit.cmis.js \
      js_of_ocaml-toplevel.cmis.js \
      js_of_ocaml.cmis.js \
      lwt.cmis.js \
      stdlib.cmis.js \
      ace_bindings.js \
      svg-pan-zoom.min.js \
      full.render.js \
      viz.js
	mkdir -p ./dist/ace-min
	cp $^ ./dist
	cp -r ace-builds-1.4.1/src-min/* ./dist/ace-min

bdd.cmo: bdd.ml bdd.cmi
	$(OCAMLC) -c $<

bdd.cmi: bdd.mli
	$(OCAMLC) $<

gv.cmo: gv.ml promise.cmi graphviz.cmi dragdiv.cmi svgpanzoom.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -c $<

colorize.cmi: colorize.mli
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-tyxml \
		  $<

colorize.cmo: colorize.ml colorize.cmi
	$(OCAMLC) -rectypes \
		  -package js_of_ocaml \
		  -package js_of_ocaml-tyxml \
		  -package js_of_ocaml-ppx \
		  -package higlo \
		  -c $<

svgpanzoom.byte: svgpanzoom.cmo svgpanzoom.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -linkpkg $< -o $@

svgpanzoom.cmi: svgpanzoom.mli
	$(OCAMLC) -package js_of_ocaml $<

svgpanzoom.cmo: svgpanzoom.ml svgpanzoom.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -linkpkg \
		  -c -o $@ $<

ace.cmo: ace.ml ace.cmi ace_types.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -c $<

ace.cmi: ace.mli ace_types.cmi
	$(OCAMLC) -package js_of_ocaml $<

ace_types.cmi: ace_types.mli
	$(OCAMLC) -package js_of_ocaml $<

ocp_indent.cmo: ocp_indent.ml
	$(OCAMLC) -package ocp-indent.lib -c $<

dragdiv.cmi: dragdiv.mli
	$(OCAMLC) -package js_of_ocaml $<

dragdiv.cmo: dragdiv.ml dragdiv.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -linkpkg \
		  -c -o $@ $<

graphviz.cmi: graphviz.mli promise.cmi
	$(OCAMLC) -package js_of_ocaml $<

graphviz.cmo: graphviz.ml graphviz.cmi promise.cmi
	$(OCAMLC) -package js_of_ocaml \
		  -package js_of_ocaml-ppx \
		  -c $<

promise.cmi: promise.mli
	$(OCAMLC) -package js_of_ocaml $<

promise.cmo: promise.ml promise.cmi
	$(OCAMLC) -c -package js_of_ocaml -package js_of_ocaml-ppx $<

clean:
	-@rm -rf stdlib.cmis.js exported-unit.cmis.js
	-@rm -rf svgpanzoom.cmi svgpanzoom.cmo
	-@rm -rf toplevel.byte toplevel.js toplevel.cmi toplevel.cmo
	-@rm -rf js_of_ocaml-lwt.cmis.js js_of_ocaml-toplevel.cmis.js
	-@rm -rf js_of_ocaml-tyxml.cmis.js js_of_ocaml.cmis.js
	-@rm -rf lwt.cmis.js
	-@rm -rf ace.cmi ace.cmo
	-@rm -rf gv.cmi gv.cmo
	-@rm -rf colorize.cmi colorize.cmo
	-@rm -rf ace_types.cmi ace_types.cmo
	-@rm -rf ocp_indent.cmi ocp_indent.cmo
	-@rm -rf dragdiv.cmi dragdiv.cmo
	-@rm -rf promise.cmi promise.cmo
	-@rm -rf graphviz.cmi graphviz.cmo

