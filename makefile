app := wc32

COMP := fasm
srcfiles := wc32.asm
opts := -m 65536 -d FOR_OS=LINUX

all: $(app)

$(app): $(srcfiles) io-lin.asm
	$(COMP) $(opts) $(srcfiles)
	chmod +x $(app)
	ls -l $(app)

wc64: wc64.asm
	$(COMP) $(opts) wc64.asm
	chmod +x wc64
	ls -l wc64

r64: wc64
	./wc64

force: clean $(app)

clean:
	rm -f $(app)

run: $(app)
	./$(app)

bm:
	cat bm.txt | $(app)

test:
	cat test.txt | $(app)

bin: $(app)
	cp -u -p $(app) ~/.local/bin/
