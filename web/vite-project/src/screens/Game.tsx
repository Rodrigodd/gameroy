import { Item } from "../interfaces"
import { run_frame, load_rom, initSync, take_audio_buffer } from "../../pkg/gameroy_vite";
import { useEffect, useRef } from "react";

export interface GameProps {
  item: Item | null;
  onBack: () => void;
}

interface GameCanvasProps {
  item: Item;
}

const useAnimationFrame = (callback: (deltaTime: number) => void) => {
  // Use useRef for mutable variables that we want to persist
  // without triggering a re-render on their change
  const requestRef = useRef<number | undefined>(undefined);
  const previousTimeRef = useRef<number | undefined>(undefined);

  useEffect(() => {
    const animate = (time: DOMHighResTimeStamp) => {
      if (previousTimeRef.current != undefined) {
        const deltaTime = time - previousTimeRef.current;
        callback(deltaTime / 1000.0)
      }
      previousTimeRef.current = time;
      requestRef.current = requestAnimationFrame(animate);
    }

    requestRef.current = requestAnimationFrame(animate);
    return () => {
      if (requestRef.current != null)
        cancelAnimationFrame(requestRef.current)
    };
  }, [callback]); // Make sure the effect runs only once
}

const SAMPLE_RATE = 44100;
const GAIN = 0.001
let audioContext: AudioContext | null = null;
let playAudioSamples: (samples: Float32Array[]) => void = () => { return; };

const initAudioProcessor = async () => {
  if (audioContext != null)
    return;
  audioContext = new AudioContext({ sampleRate: SAMPLE_RATE, latencyHint: 'playback' });
  audioContext.destination.channelCount = 2;
  await audioContext.audioWorklet.addModule('/src/audioProcessor.js');
  const processorNode = new AudioWorkletNode(audioContext, 'wasm-audio-processor', { outputChannelCount: [2] });
  console.log(processorNode);
  processorNode.connect(audioContext.destination);

  await audioContext.resume();

  playAudioSamples = (samples: Float32Array[]) => {
    processorNode.port.postMessage([samples[0].buffer, samples[1].buffer], [samples[0].buffer, samples[1].buffer]);
  };
}

const GameCanvas = ({ item }: GameCanvasProps) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const bufferRef = useRef<Uint8Array | null>(null)

  useEffect(() => {
    const load = async () => {
      const wasm = await fetch("/pkg/gameroy_vite_bg.wasm");
      initSync(await wasm.arrayBuffer());
      const buffer = await item.file.arrayBuffer();
      const rom = new Uint8Array(buffer);
      await initAudioProcessor();
      bufferRef.current = rom;
      load_rom(rom);
    };
    void load();
  }, [item]);

  useAnimationFrame((delta) => {
    const canvas = canvasRef.current
    if (canvas == null) return
    if (bufferRef.current == null) return

    try {
      if (delta > 0.050) {
        console.warn(`Frame took too long: ${delta * 1000.0}ms`)
        delta = 0.016666;
      }
      const frame = run_frame(delta);
      const context = canvas.getContext('2d')
      const imageData = new ImageData(new Uint8ClampedArray(frame.buffer), 160, 144)
      context?.putImageData(imageData, 0, 0);

      const samples = take_audio_buffer(GAIN);
      playAudioSamples(samples);
    } catch (error) {
      console.error(error)
    }
  })

  return <canvas ref={canvasRef} width={160} height={144} />
}

export const Game = ({ item, onBack }: GameProps) => {
  if (item == null)
    return <div className="detail">No item selected</div>

  return (
    <div className="detail">
      <button onClick={onBack}>🔙 Back</button>
      <h2>{item.title}</h2>
      <GameCanvas item={item} />
    </div>
  );
};
