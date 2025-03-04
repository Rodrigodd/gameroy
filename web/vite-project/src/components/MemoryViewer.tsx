import React, { useRef, useState } from "react";
import HexViewer, { HexViewerElement } from "./HexViewer";

interface MemoryViewerProps {
  rom: Uint8Array | null;
  isLoaded: boolean;
}

const GREEN1 = "#040";
const GREEN2 = "#060";
const RED = "#700";

const symbols = [
  { address: 0x0040, name: "INTR V-Blank", size: 1, color: RED },
  { address: 0x0048, name: "INTR STAT", size: 1, color: RED },
  { address: 0x0050, name: "INTR Timer", size: 1, color: RED },
  { address: 0x0058, name: "INTR Serial", size: 1, color: RED },
  { address: 0x0060, name: "INTR Joypad", size: 1, color: RED },
  {
    address: 0x0104,
    name: "header - logo",
    size: 0x134 - 0x104,
    color: GREEN1,
  },
  {
    address: 0x0134,
    name: "header - title",
    size: 0x143 - 0x134,
    color: GREEN2,
  },
  { address: 0x0143, name: "header - cgb", size: 1, color: GREEN1 },
  { address: 0x0146, name: "header - sgb", size: 1, color: GREEN2 },
  { address: 0x0147, name: "header - cartridge type", size: 1, color: GREEN1 },
  { address: 0x0148, name: "header - rom size", size: 1, color: GREEN2 },
  { address: 0x0149, name: "header - ram size", size: 1, color: GREEN1 },
  { address: 0x014c, name: "header - version", size: 1, color: GREEN2 },
  { address: 0x014d, name: "header - header checksum", size: 1, color: GREEN1 },
  { address: 0x014e, name: "header - global checksum", size: 2, color: GREEN2 },
];

const MemoryViewer: React.FC<MemoryViewerProps> = ({ rom, isLoaded }) => {
  const [jumpAddress, setJumpAddress] = useState("");
  const hexViewerRef = useRef<HexViewerElement | null>(null);

  if (!isLoaded) return <div>Loading...</div>;

  const handleJump = () => {
    console.log(jumpAddress);
    const address = parseInt(jumpAddress, 16);
    hexViewerRef.current?.jumpToAddress(address);
  };

  return (
    <div>
      <div style={{ marginBottom: "10px" }}>
        <input
          type="text"
          value={jumpAddress}
          onChange={(e) => setJumpAddress(e.target.value)}
          onKeyDown={(e) => {
            e.stopPropagation();
            if (e.key === "Enter") {
              handleJump();
            }
          }}
          onKeyUp={(e) => e.stopPropagation()}
          placeholder="Enter address (hex)"
          style={{ marginRight: "10px" }}
        />
        <button onClick={handleJump}>Jump</button>
      </div>
      <HexViewer
        ref={hexViewerRef}
        length={rom?.length ?? 0}
        section={(s, e) => {
          return { data: rom?.slice(s, e) ?? new Uint8Array(0), symbols };
        }}
      />
    </div>
  );
};

export default MemoryViewer;
