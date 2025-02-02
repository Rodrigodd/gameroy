class WasmAudioProcessor extends AudioWorkletProcessor {
    constructor() {
        super();
        this.buffers = [];
        this.sampleIndex = 0;
        this.sampleCount = 0;
        const MAX_BUFFER_SIZE = 2048;

        this.port.onmessage = (event) => {
            let left = new Float32Array(event.data[0]);
            let right = new Float32Array(event.data[1]);

            if (this.sampleCount > MAX_BUFFER_SIZE) {
                // drop the buffer to prevent buffering too much
                return;
            }

            this.buffers.push([left, right]);
            this.sampleCount += left.length;
            // console.log(this.sampleCount);
        };
    }

    process(_inputs, outputs) {
        if (this.buffers.length === 0) return true;

        let leftOutput = outputs[0][0];
        let rightOutput = outputs[0][1];
        let outputLength = leftOutput.length;
        let outputIndex = 0;

        if (outputLength > this.sampleCount) {
            this.sampleCount = 0;
        } else {
            this.sampleCount -= outputLength;
        }

        let buffer = this.buffers[0];
        while (outputIndex < outputLength) {
            let leftBuffer = buffer[0];
            let rightBuffer = buffer[1];

            let remainingSamples = leftBuffer.length - this.sampleIndex;
            let copySize = Math.min(outputLength - outputIndex, remainingSamples);

            leftOutput.set(leftBuffer.subarray(this.sampleIndex, this.sampleIndex + copySize), outputIndex);
            rightOutput.set(rightBuffer.subarray(this.sampleIndex, this.sampleIndex + copySize), outputIndex);

            this.sampleIndex += copySize;
            outputIndex += copySize;

            if (this.sampleIndex >= leftBuffer.length) {
                this.sampleIndex = 0;
                this.buffers.shift();
                buffer = this.buffers[0];
                if (!buffer) break;
            }
        }

        return true;
    }
}

registerProcessor('wasm-audio-processor', WasmAudioProcessor);
