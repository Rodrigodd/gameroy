import React, { useRef, useState, useMemo } from "react";
import { useVirtualizer } from "@tanstack/react-virtual";

const BYTES_PER_ROW = 16;
const ROW_HEIGHT = 20;

interface Symbol {
  name: string;
  address: number;
  size: number;
  color: string;
}

interface HexViewerProps {
  data: Uint8Array;
  symbols: Symbol[];
}

const HexViewer: React.FC<HexViewerProps> = ({ data, symbols }) => {
  const parentRef = useRef<HTMLDivElement>(null);
  const [jumpAddress, setJumpAddress] = useState("");
  const rowCount = Math.ceil(data.length / BYTES_PER_ROW);

  const addressPad = useMemo(
    () => Math.max(4, Math.ceil(Math.log2(data.length) / 4)),
    [data.length],
  );

  const virtualizer = useVirtualizer({
    count: rowCount,
    getScrollElement: () => parentRef.current,
    estimateSize: () => ROW_HEIGHT,
  });

  const formatHex = (byte: number): string =>
    byte.toString(16).padStart(2, "0").toUpperCase();
  const formatAscii = (byte: number): string =>
    byte == 32
      ? "\u2009"
      : byte >= 32 && byte < 127
        ? String.fromCharCode(byte)
        : ".";

  const handleJump = () => {
    const address = parseInt(jumpAddress, 16);
    if (!isNaN(address)) {
      console.log("Jumping to address:", address);
      const rowIndex = Math.floor(address / BYTES_PER_ROW);
      virtualizer.scrollToIndex(rowIndex, { align: "start" });
    }
  };

  return (
    <div style={{ fontFamily: "monospace" }}>
      <div style={{ marginBottom: "10px" }}>
        <input
          type="text"
          value={jumpAddress}
          onChange={(e) => setJumpAddress(e.target.value)}
          placeholder="Enter address (hex)"
          style={{ marginRight: "10px" }}
        />
        <button onClick={handleJump}>Jump</button>
      </div>
      <div ref={parentRef} style={{ height: "500px", overflow: "auto" }}>
        <div
          style={{ height: virtualizer.getTotalSize(), position: "relative" }}
        >
          {virtualizer.getVirtualItems().map((virtualRow) => {
            const start = virtualRow.index * BYTES_PER_ROW;
            const end = start + BYTES_PER_ROW;
            const rowBytes = data.slice(start, end);
            const rowSymbols = symbols.filter(
              (s) => s.address < end && s.address + s.size > start,
            );
            rowSymbols.sort((a, b) => a.address - b.address);

            const segments: {
              start: number;
              end: number;
              symbol?: Symbol;
              truncatedStart: boolean;
              truncatedEnd: boolean;
            }[] = [];
            let cursor = start;
            for (const sym of rowSymbols) {
              const symStart = Math.max(sym.address, start);
              const symEnd = Math.min(sym.address + sym.size, end);
              if (cursor < symStart) {
                segments.push({
                  start: cursor,
                  end: symStart,
                  truncatedStart: false,
                  truncatedEnd: false,
                });
              }
              segments.push({
                start: symStart,
                end: symEnd,
                symbol: sym,
                truncatedStart: symStart > sym.address,
                truncatedEnd: symEnd < sym.address + sym.size,
              });
              cursor = symEnd;
            }
            if (cursor < end) {
              segments.push({
                start: cursor,
                end: end,
                truncatedStart: false,
                truncatedEnd: false,
              });
            }

            return (
              <div
                key={virtualRow.index}
                style={{
                  position: "absolute",
                  top: virtualRow.start,
                  height: ROW_HEIGHT,
                  width: "100%",
                  display: "flex",
                  alignItems: "center",
                }}
              >
                <div style={{ marginRight: 10 }}>
                  {start.toString(16).padStart(addressPad, "0").toUpperCase()}:
                </div>
                <div style={{ flex: 1, display: "flex", gap: "4px" }}>
                  {segments.map((seg, i) => (
                    <span
                      key={i}
                      style={{
                        backgroundColor: seg.symbol
                          ? seg.symbol.color
                          : "transparent",
                        borderTopLeftRadius: seg.truncatedStart ? "0" : "3px",
                        borderBottomLeftRadius: seg.truncatedStart
                          ? "0"
                          : "3px",
                        borderTopRightRadius: seg.truncatedEnd ? "0" : "3px",
                        borderBottomRightRadius: seg.truncatedEnd ? "0" : "3px",
                        position: "relative",
                        display: "flex",
                        gap: "4px",
                      }}
                      title={
                        seg.symbol
                          ? `${seg.symbol.name} @ 0x${seg.symbol.address.toString(16).toUpperCase()}`
                          : ""
                      }
                    >
                      {Array.from(data.slice(seg.start, seg.end)).map(
                        (byte, j) => (
                          <span key={j}>{formatHex(byte)}</span>
                        ),
                      )}
                    </span>
                  ))}
                </div>
                <div
                  style={{ marginLeft: "10px", display: "flex", gap: "0px" }}
                >
                  {segments.map((seg, i) => (
                    <span
                      key={i}
                      style={{
                        backgroundColor: seg.symbol
                          ? seg.symbol.color
                          : "transparent",
                        borderTopLeftRadius: seg.truncatedStart ? "0" : "3px",
                        borderBottomLeftRadius: seg.truncatedStart
                          ? "0"
                          : "3px",
                        borderTopRightRadius: seg.truncatedEnd ? "0" : "3px",
                        borderBottomRightRadius: seg.truncatedEnd ? "0" : "3px",
                        position: "relative",
                        display: "flex",
                      }}
                    >
                      {Array.from(data.slice(seg.start, seg.end)).map(
                        (byte, j) => (
                          <span key={j}>{formatAscii(byte)}</span>
                        ),
                      )}
                    </span>
                  ))}
                </div>
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
};

export default HexViewer;
