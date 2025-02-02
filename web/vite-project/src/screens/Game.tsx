import { Item } from "../interfaces"
import { run_frame, load_rom, initSync } from "../../pkg/gameroy_vite";
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

const GameCanvas = ({ item }: GameCanvasProps) => {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  const bufferRef = useRef<Uint8Array | null>(null)

  useEffect(() => {
    const load = async () => {
      const wasm = await fetch("/pkg/gameroy_vite_bg.wasm");
      initSync(await wasm.arrayBuffer());
      const buffer = await item.file.arrayBuffer();
      const rom = new Uint8Array(buffer);
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
      if (delta > 0.02000) {
        console.warn(`Frame took too long: ${delta}ms`)
        delta = 0.01666
      }
      const frame = run_frame(delta);
      const context = canvas.getContext('2d')
      const imageData = new ImageData(new Uint8ClampedArray(frame.buffer), 160, 144)
      context?.putImageData(imageData, 0, 0);
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
