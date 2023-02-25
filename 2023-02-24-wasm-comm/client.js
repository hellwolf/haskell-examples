import { WASI, File, PreopenDirectory } from "@bjorn3/browser_wasi_shim";


function fromCString(inst, pos, size) {
    const b = new Uint8Array(inst.exports.memory.buffer, pos, size);
    console.log("fromCString!!", inst.exports.memory.buffer, pos, size, b);
    return new TextDecoder("utf8").decode(b);
}

(async function () {
    const wasm = await WebAssembly.compileStreaming(fetch("WasmComm.wasm"));
    const wasi = new WASI([], ["LC_ALL=en_US.utf-8"], [/* fds */]);
    const inst = await WebAssembly.instantiate(wasm, {
        "wasi_snapshot_preview1": wasi.wasiImport,
    });
    wasi.inst = inst;
    inst.exports._initialize();
    inst.exports.hs_init(0, 0);
    console.log("echo:", fromCString(inst, inst.exports.echo("hello"), 6));
    console.log("size before", inst.exports.size());
    inst.exports.save("a", "42");
    inst.exports.save("b", "21");
    console.log("size after", inst.exports.size());
    console.log("a=", fromCString(inst, inst.exports.load("a"), 10));
    console.log("b=", inst.exports.load("b"));
    console.log("c=", inst.exports.load("c"));
    window.inst = inst;
    window.wasi = wasi;
})()
