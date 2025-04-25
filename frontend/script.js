// Assuming your WASM file is named "mymodule.wasm"
async function loadWasmModule() {
    // Fetch the WASM file
    const wasmResponse = await fetch('../target/lib/cljcc-lib-wasm.js.wasm');
    const wasmBuffer = await wasmResponse.arrayBuffer();

    // Instantiate the WASM module
    const wasmResult = await WebAssembly.instantiate(wasmBuffer, {
        env: {
            // Any imported functions needed by your WASM module
            // For example, if your Java code uses console logging:
            printString: function(ptr, len) {
                // Implementation to handle string output
                const bytes = new Uint8Array(memory.buffer, ptr, len);
                const string = new TextDecoder('utf8').decode(bytes);
                console.log(string);
                document.getElementById('output').innerText += string + '\n';
            }
        }
    });

    // Store the instance and memory
    const instance = wasmResult.instance;
    const memory = instance.exports.memory;

    // Call the exported function that returns a string
    // For example, if your Java code exports a function called "getString"
    const stringPointer = instance.exports.getString();

    // Now we need to read the string from memory
    // This depends on how your GraalVM WASM image handles strings
    // Typically you would:
    // 1. Get the length of the string (via another export or a convention)
    const stringLength = instance.exports.getStringLength(); // If this is exported

    // 2. Convert the pointer and length to a JavaScript string
    const bytes = new Uint8Array(memory.buffer, stringPointer, stringLength);
    const result = new TextDecoder('utf8').decode(bytes);

    // 3. Display the result
    document.getElementById('output').innerText = result;

    return instance;
}

// Load the WASM module when the page loads
window.onload = function() {
    loadWasmModule().catch(err => {
        console.error("Failed to load WASM module:", err);
        document.getElementById('output').innerText = "Error: " + err.message;
    });
};
