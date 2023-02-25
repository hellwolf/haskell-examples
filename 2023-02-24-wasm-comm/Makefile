build: dist/index.html dist/WasmComm.wasm dist/client.js

dev:
	nodemon -e js,hs -x "$(MAKE) build || exit 1"

serve:
	(cd dist; python -mhttp.server)

build/:
	mkdir -p build
dist/:
	mkdir -p dist

dist/WasmComm.wasm: WasmComm.hs Store.hs dist/ build/
	wasm32-wasi-ghc $< -o $@  \
		-no-hs-main -optl-mexec-model=reactor \
		-odir build -hidir dist -stubdir dist \
		-optl-Wl,--export=hs_init,--export=callocBuffer,--export=freeBuffer,--export=echo,--export=save,--export=load,--export=size

dist/client.js: client.js dist/
	npx esbuild $< --bundle --sourcemap --outfile=$@ --format=esm

dist/index.html: index.html dist/
	cp $< $@

clean:
	rm -rf build dist

.PHONY: build dev serve clean
