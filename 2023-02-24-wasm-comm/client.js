import { WASI, File, PreopenDirectory } from "@bjorn3/browser_wasi_shim";

(async function () {
    const wasm = await WebAssembly.compileStreaming(fetch("WasmComm.wasm"));
    const wasi = new WASI([], ["LC_ALL=en_US.utf-8"], [/* fds */]);
    const inst = await WebAssembly.instantiate(wasm, {
        "wasi_snapshot_preview1": wasi.wasiImport,
    });
    console.log(inst);
    try {
        wasi.start(inst);
    } catch (e) {
        //console.warn(e);
    }
    inst.exports.hs_init(0, 0);
    console.log("echo: ", inst.exports.echo("hello"));
    console.log("size before", inst.exports.size());
    inst.exports.save("a", "42");
    inst.exports.save("b", "21");
    console.log("size after", inst.exports.size());
    console.log("a=", inst.exports.load("a"));
    console.log("b=", inst.exports.load("b"));
    console.log("c=", inst.exports.load("c"));
    window.inst = inst;
    window.wasi = wasi;
})()
