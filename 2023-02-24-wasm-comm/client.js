/* global wasm, wasi, inst */
import { WASI } from "@bjorn3/browser_wasi_shim";

////////////////////////////////////////////////////////////////////////////////
// Haskell Wasm Utilities
////////////////////////////////////////////////////////////////////////////////

function bufferAt(pos, len) {
    return new Uint8Array(inst.exports.memory.buffer, pos, len);
}

function cstringBufferAt(cstr) {
    let b = new Uint8Array(inst.exports.memory.buffer, cstr);
    let l = b.findIndex(i => i == 0, b);
    return bufferAt(cstr, l);
}

function withCStrings(strs, op) {
    const cstrs = strs.map(str => {
        const s = new TextEncoder().encode(str);
        const l = s.length + 1;
        const p = inst.exports.callocBuffer(l);
        const b = new bufferAt(p, l);
        b.set(s);
        return p;
    });
    const r = op(cstrs);
    strs.forEach(inst.exports.freeBuffer);
    return r;
}

function withCString(str, op) {
    return withCStrings([str], strs => op(strs[0]));
}

function fromCString(cstr) {
    const s = new TextDecoder("utf8").decode(cstringBufferAt(cstr));
    inst.exports.freeBuffer(cstr);
    return s;
}

////////////////////////////////////////////////////////////////////////////////
// Application APIs
////////////////////////////////////////////////////////////////////////////////

function echo(str) {
    return fromCString(withCString(str, cstr => inst.exports.echo(cstr)));
}

function store_size() {
    return inst.exports.size();
}

function store_save(k, v) {
    withCStrings([k,v], a => inst.exports.save(a[0], a[1]));
}

function store_load(k) {
    return fromCString(withCString(k, k => inst.exports.load(k)));
}

////////////////////////////////////////////////////////////////////////////////
// Application Logic
////////////////////////////////////////////////////////////////////////////////

function test() {
    console.log("echo:", echo("hello world"));
    console.log("size before", store_size());
    store_save("a", "42");
    store_save("b", "21");
    console.log("size after", store_size());
    console.log("a=", store_load("a"));
    console.log("b=", store_load("b"));
    console.log("c=", store_load("c"));
}

(async function () {
    // load Haskell Wasm Reactor Module
    window.wasm = await WebAssembly.compileStreaming(fetch("WasmComm.wasm"));
    window.wasi = new WASI([], ["LC_ALL=en_US.utf-8"], [/* fds */]);
    window.inst = await WebAssembly.instantiate(wasm, {
        "wasi_snapshot_preview1": wasi.wasiImport,
    });

    // initialize Haskell Wasm Reactor Module
    wasi.initialize(inst);
    inst.exports.hs_init(0, 0);

    test();
})()
