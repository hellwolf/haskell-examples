build: dist/index.html dist/WasmComm.wasm dist/client.js

build:
	mkdir -p build
dist:
	mkdir -p dist

dist/WasmComm.wasm: WasmComm.hs dist build
	wasm32-wasi-ghc $< -o $@  \
		-no-hs-main -optl-mexec-model=reactor \
		-odir build -hidir dist -stubdir dist \
		-optl-Wl,--export=hs_init,--export=echo,--export=save,--export=load,--export=size

dist/client.js: client.js dist
	esbuild $< --bundle --sourcemap --outfile=$@ --format=esm

dist/index.html: index.html dist
	cp $< $@

clean:
	rm -rf build dist

.PHONY: build clean
