import { Item } from "../interfaces";
import {
  run_for,
  get_last_screen,
  get_ppu_background,
  get_ppu_window,
  get_ppu_tiles,
  load_rom,
  initSync,
  take_audio_buffer,
  set_joypad,
  save_state,
  load_state,
  reset,
} from "../../pkg/gameroy_vite";
import { useEffect, useRef, useState } from "react";
import "../App.css";
import { Menu, MenuItem } from "../components/Menu";
import { saveStateToOPFS, loadStateFromOPFS } from "../utils/opfsUtils";
import ResizablePanel from "../components/Resizable";
import { Tab, TabPanel } from "../components/Tab";
import "./Game.css";
import HexViewer from "../components/HexViewer";

export interface GameProps {
  item: Item | null;
  onBack: () => void;
}

interface GameCanvasProps {
  item: Item;
  isLoaded: boolean;
}

const useAnimationFrame = (callback: (deltaTime: number) => void) => {
  const requestRef = useRef<number | undefined>(undefined);
  const previousTimeRef = useRef<number | undefined>(undefined);

  useEffect(() => {
    const animate = (time: DOMHighResTimeStamp) => {
      if (previousTimeRef.current != undefined) {
        const deltaTime = time - previousTimeRef.current;
        callback(deltaTime / 1000.0);
      }
      previousTimeRef.current = time;
      requestRef.current = requestAnimationFrame(animate);
    };

    requestRef.current = requestAnimationFrame(animate);
    return () => {
      if (requestRef.current != null) cancelAnimationFrame(requestRef.current);
    };
  }, [callback]);
};

const SAMPLE_RATE = 44100;
const GAIN = 0.001;
let audioContext: AudioContext | null = null;
let playAudioSamples: (samples: Float32Array[]) => void = () => {
  return;
};

const initAudioProcessor = async () => {
  if (audioContext != null) return;
  audioContext = new AudioContext({
    sampleRate: SAMPLE_RATE,
    latencyHint: "playback",
  });
  audioContext.destination.channelCount = 2;
  await audioContext.audioWorklet.addModule("/src/audioProcessor.js");
  const processorNode = new AudioWorkletNode(
    audioContext,
    "wasm-audio-processor",
    { outputChannelCount: [2] },
  );

  processorNode.connect(audioContext.destination);

  await audioContext.resume();

  playAudioSamples = (samples: Float32Array[]) => {
    processorNode.port.postMessage(
      [samples[0].buffer, samples[1].buffer],
      [samples[0].buffer, samples[1].buffer],
    );
  };
};

const GameCanvas = ({ item, isLoaded }: GameCanvasProps) => {
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const [joypadState, setJoypadState] = useState<number>(0);
  const joypadStateRef = useRef<number>(0);
  const [isFastFoward, setIsFastFoward] = useState<boolean>(false);
  const isFastFowardRef = useRef<boolean>(false);
  const fastForwardTaskRef = useRef<(() => Promise<void>) | null>(null);

  joypadStateRef.current = joypadState;
  isFastFowardRef.current = isFastFoward;

  useEffect(() => {
    window.onbeforeunload = () => {
      const array = save_state();
      void saveStateToOPFS(item, array);
      return "Save game before leaving?";
    };
    return () => {
      window.onbeforeunload = null;
    };
  }, [item]);

  if (isFastFoward && fastForwardTaskRef.current == null) {
    fastForwardTaskRef.current = async () => {
      while (isFastFowardRef.current) {
        set_joypad(joypadStateRef.current);
        run_for(0.16666);
        const samples = take_audio_buffer(GAIN);
        playAudioSamples(samples);
        await new Promise((resolve) => setTimeout(resolve, 0));
      }
      fastForwardTaskRef.current = null;
    };

    void fastForwardTaskRef.current();
  } else if (!isFastFoward && fastForwardTaskRef.current != null) {
    fastForwardTaskRef.current = null;
  }

  useEffect(() => {
    const keyMap: Record<string, number> = {
      ArrowRight: 0x01, // Right
      ArrowLeft: 0x02, // Left
      ArrowUp: 0x04, // Up
      ArrowDown: 0x08, // Down
      KeyA: 0x10, // A
      KeyS: 0x20, // B
      Backspace: 0x40, // Select
      Enter: 0x80, // Start
    };

    const handleKeyDown = (event: KeyboardEvent) => {
      if (keyMap[event.code] !== undefined) {
        setJoypadState((prev) => prev | keyMap[event.code]);
      } else if (event.code === "ShiftLeft") {
        setIsFastFoward(true);
      }
    };

    const handleKeyUp = (event: KeyboardEvent) => {
      if (keyMap[event.code] !== undefined) {
        setJoypadState((prev) => prev & ~keyMap[event.code]);
      } else if (event.code === "ShiftLeft") {
        setIsFastFoward(false);
      }
    };

    window.addEventListener("keydown", handleKeyDown);
    window.addEventListener("keyup", handleKeyUp);
    return () => {
      window.removeEventListener("keydown", handleKeyDown);
      window.removeEventListener("keyup", handleKeyUp);
    };
  }, []);

  useAnimationFrame((delta) => {
    const canvas = canvasRef.current;
    if (!canvas || !isLoaded) return;
    try {
      if (!isFastFowardRef.current) {
        if (delta > 0.05) {
          console.warn(`Frame took too long: ${delta * 1000.0}ms`);
          delta = 0.016666;
        }
        set_joypad(joypadState);
        run_for(delta);
      }
      const frame = get_last_screen();
      const context = canvas.getContext("2d");
      const imageData = new ImageData(
        new Uint8ClampedArray(frame.buffer),
        160,
        144,
      );
      context?.putImageData(imageData, 0, 0);
      const samples = take_audio_buffer(GAIN);
      playAudioSamples(samples);
    } catch (error) {
      console.error(error);
    }
  });

  return <canvas ref={canvasRef} width={160} height={144} id="game-canvas" />;
};

const PixelCanvas = ({
  get_pixels,
  size: [width, height],
}: {
  get_pixels: () => Uint32Array;
  size: number[];
}) => {
  const canvasRef = useRef<HTMLCanvasElement>(null);

  useAnimationFrame(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    try {
      const pixels = get_pixels();
      const context = canvas.getContext("2d");
      const imageData = new ImageData(
        new Uint8ClampedArray(pixels.buffer),
        width,
        height,
      );
      context?.putImageData(imageData, 0, 0);
    } catch (error) {
      console.error("Failed to render PPU debug:", error);
    }
  });

  return (
    <canvas
      ref={canvasRef}
      width={width}
      height={height}
      className="pixel-canvas"
      style={{ width: `${width}px`, height: `${height}px` }}
    />
  );
};

const PpuDebug = ({ isLoaded }: { isLoaded: boolean }) => {
  if (!isLoaded) return <div>Loading...</div>;

  return (
    <div id="ppu-debug">
      <h2>Tiles</h2>
      <PixelCanvas
        get_pixels={() => get_ppu_tiles(0b11100100)}
        size={[128, 192]}
      />
      <h2>Background</h2>
      <PixelCanvas get_pixels={get_ppu_background} size={[256, 256]} />
      <h2>Window</h2>
      <PixelCanvas get_pixels={get_ppu_window} size={[256, 256]} />
    </div>
  );
};

export const Game = ({ item, onBack }: GameProps) => {
  const [isLoaded, setIsLoaded] = useState<boolean>(false);
  const [data, setData] = useState<Uint8Array | null>(null);

  useEffect(() => {
    const load = async () => {
      if (!item) return;
      const wasm = await fetch("/pkg/gameroy_vite_bg.wasm");
      initSync(await wasm.arrayBuffer());
      const buffer = await item.file.arrayBuffer();
      const rom = new Uint8Array(buffer);
      setData(rom);
      await initAudioProcessor();
      load_rom(rom);
      setIsLoaded(true);

      const state = await loadStateFromOPFS(item);
      if (state) {
        try {
          load_state(new Uint8Array(state));
        } catch (error) {
          console.warn("Failed to load state:", error);
        }
      }
    };
    void load();
  }, [item]);

  if (!isLoaded) return <div>Loading...</div>;
  if (!item) return <div className="detail">No item selected</div>;

  const onBackClick = () => {
    const array = save_state();
    void saveStateToOPFS(item, array);
    onBack();
  };

  const menuItems: MenuItem[] = [
    { label: "Back", action: "back" },
    { label: "Restart", action: "reset" },
  ];

  const handleMenuSelect = (action: string) => {
    if (action === "reset") {
      reset();
    } else if (action === "back") {
      onBackClick();
    }
  };

  return (
    <div className="detail">
      <header className="header">
        <button onClick={onBackClick}>🔙 Back</button>
        <h2>{item.title}</h2>
        <Menu items={menuItems} onSelect={handleMenuSelect} />
      </header>
      <div className="flex-row" style={{ flex: 1 }}>
        <GameCanvas item={item} isLoaded={isLoaded} />
        <ResizablePanel
          corner="bottom-left"
          resizeAxis="horizontal"
          className="panel"
        >
          <TabPanel>
            <Tab label="Tiles">
              <PpuDebug isLoaded={isLoaded} />
            </Tab>
            <Tab label="Sprites">
              <h1> TODO: Implement sprite debug </h1>
            </Tab>
            <Tab label="Memory">
              {data && (
                <HexViewer
                  data={data}
                  symbols={[
                    {
                      address: 0x0040,
                      name: "INTR V-Blank",
                      size: 1,
                      color: "#a00",
                    },
                    {
                      address: 0x0048,
                      name: "INTR STAT",
                      size: 1,
                      color: "#a00",
                    },
                    {
                      address: 0x0050,
                      name: "INTR Timer",
                      size: 1,
                      color: "#a00",
                    },
                    {
                      address: 0x0058,
                      name: "INTR Serial",
                      size: 1,
                      color: "#a00",
                    },
                    {
                      address: 0x0060,
                      name: "INTR Joypad",
                      size: 1,
                      color: "#a00",
                    },
                    {
                      address: 0x0104,
                      name: "header - logo",
                      size: 0x134 - 0x104,
                      color: "#070",
                    },
                    {
                      address: 0x0134,
                      name: "header - title",
                      size: 0x143 - 0x134,
                      color: "#0a0",
                    },
                    {
                      address: 0x0143,
                      name: "header - cgb",
                      size: 1,
                      color: "#070",
                    },
                    {
                      address: 0x0146,
                      name: "header - sgb",
                      size: 1,
                      color: "#0a0",
                    },
                    {
                      address: 0x0147,
                      name: "header - cartridge type",
                      size: 1,
                      color: "#070",
                    },
                    {
                      address: 0x0148,
                      name: "header - rom size",
                      size: 1,
                      color: "#0a0",
                    },
                    {
                      address: 0x0149,
                      name: "header - ram size",
                      size: 1,
                      color: "#070",
                    },
                    {
                      address: 0x014c,
                      name: "header - version",
                      size: 1,
                      color: "#0a0",
                    },
                    {
                      address: 0x014d,
                      name: "header - header checksum",
                      size: 1,
                      color: "#070",
                    },
                    {
                      address: 0x014e,
                      name: "header - global checksum",
                      size: 2,
                      color: "#0a0",
                    },
                  ]}
                />
              )}
            </Tab>
          </TabPanel>
        </ResizablePanel>
      </div>
    </div>
  );
};
