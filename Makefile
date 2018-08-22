all:
	esy jbuilder build @install -j 8 --dev

toplevel: all
	esy jbuilder exec -- make -C try_it

clear:
	$(MAKE) -C try_it clear
.PHONY: all toplevel
