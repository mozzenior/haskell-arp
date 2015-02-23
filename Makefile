GHC := ghc

default: arp

arp:
	$(GHC) -o $@ $@.hs
PHONY += arp

clean:
	git clean -dxf

.PHONY: $(PHONY)
