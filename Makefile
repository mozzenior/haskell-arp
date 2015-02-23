GHC := ghc

default: my_arp

my_arp: arp.hs
	$(GHC) -o $@ $^
PHONY += my_arp

clean:
	git clean -dxf

.PHONY: $(PHONY)
