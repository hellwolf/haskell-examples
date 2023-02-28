Browser to Haskell Wasm Reactor Module Communication Example
============================================================

## Setup Using Nix

```
nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz
```

Read more about GHC Wasm: https://gitlab.haskell.org/ghc/ghc-wasm-meta.

## Run the Demo

**Build**

```
$ make
```

**Serve**

Using python http.server:

```
$ make serve
```

**Test**

Visit http://localhost:8000/ from your browser and open console.

You should see the output:

```
echo: hello world client.js:67:12
size before 0 client.js:68:12
size after 2 client.js:71:12
a= 42 client.js:72:12
b= 21 client.js:73:12
c= <empty string>
```

## What Was Done

- A toy Store.hs providing a wasm-independent save2/load2/size2 API globally.
- A WasmComm.hs module that exports Store.hs module through FFI.
- It uses wasm32-wasi-ghc to build a wasm reactor module that exports for Javascript VM to use.
- Browser client library uses [browser_wasi_shim](https://github.com/bjorn3/browser_wasi_shim) to communicate with the
  module.

### Javascript String Hurdle

The biggest challenge is that in order to pass Javascript string between the Wasm Module and Javascript VM, we need to play
with [the linear memory model of wasm](https://developer.mozilla.org/en-US/docs/WebAssembly/JavaScript_interface/Memory).

This example creates a few ad-hoc wrapper function to help with the alloc/free memory management convention for the
functions exported by the Haskell wasm reactor module.

Here is an example of how to wrap the Haskell save function as `store_save` for Javascript usage:

```haskell
foreign export ccall save :: CString -> CString -> IO ()
```

```javascript
function store_load(k) {
    return fromCString(withCString(k, k => inst.exports.load(k)));
}
```

Check the client.js for more information about how `fromCString` and `withCString` is implemented.
