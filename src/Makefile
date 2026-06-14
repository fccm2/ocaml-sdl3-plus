RMDIR=rmdir
all: sdl3.cmxa sdl3.cma

sdl3.cmi: sdl3.mli
	ocamlc -c $<

sdl3.cmx: sdl3.ml sdl3.cmi
	ocamlopt -c $<

sdl3_stubs.o: sdl3_stubs.c
	ocamlopt -c $<

sdl3.cmxa: sdl3.cmx sdl3_stubs.o
	ocamlmklib -o sdl3 -oc sdl3_stub sdl3.cmx sdl3_stubs.o -L/usr/local/lib -cclib -lSDL3

sdl3.cma: sdl3.cmo sdl3_stubs.o
	ocamlmklib -o sdl3 -oc sdl3_stub sdl3.cmo sdl3_stubs.o -L/usr/local/lib -linkall -lSDL3

sdl3.cmo: sdl3.ml sdl3.cmi
	ocamlc -c $<

clean:
	$(RM) dllsdl3_stub.so
	$(RM) libsdl3_stub.a
	$(RM) sdl3.a
	$(RM) sdl3.cma
	$(RM) sdl3.cmi
	$(RM) sdl3.cmo
	$(RM) sdl3.cmx
	$(RM) sdl3.cmxa
	$(RM) sdl3.o
	$(RM) sdl3_stubs.o
	$(RM) doc/Sdl3.APP.html
	$(RM) doc/Sdl3.Events.html
	$(RM) doc/Sdl3.MakeApp.html
	$(RM) doc/Sdl3.Render.html
	$(RM) doc/Sdl3.Scancode.html
	$(RM) doc/Sdl3.Surface.html
	$(RM) doc/Sdl3.Texture.html
	$(RM) doc/Sdl3.Timer.html
	$(RM) doc/Sdl3.html
	$(RM) doc/index.html
	$(RM) doc/index_attributes.html
	$(RM) doc/index_class_types.html
	$(RM) doc/index_classes.html
	$(RM) doc/index_exceptions.html
	$(RM) doc/index_extensions.html
	$(RM) doc/index_methods.html
	$(RM) doc/index_module_types.html
	$(RM) doc/index_modules.html
	$(RM) doc/index_types.html
	$(RM) doc/index_values.html
	$(RM) doc/style.css
	$(RM) doc/type_Sdl3.APP.html
	$(RM) doc/type_Sdl3.Events.html
	$(RM) doc/type_Sdl3.MakeApp.html
	$(RM) doc/type_Sdl3.Render.html
	$(RM) doc/type_Sdl3.Scancode.html
	$(RM) doc/type_Sdl3.Surface.html
	$(RM) doc/type_Sdl3.Texture.html
	$(RM) doc/type_Sdl3.Timer.html
	$(RM) doc/type_Sdl3.html
	$(RMDIR) doc/

doc:
	mkdir -p doc
	ocamldoc -d doc -html sdl3.mli

PREFIX = $(shell ocamlc -where)/sdl3
CP = cp
install:
	mkdir -p $(PREFIX)
	$(CP)             \
	  dllsdl3_stub.so \
	  libsdl3_stub.a  \
	  sdl3.a          \
	  sdl3.cmi        \
	  sdl3.cmx        \
	  sdl3.cmxa       \
	  sdl3.cma        \
	  sdl3.mli        \
	  $(PREFIX)/
