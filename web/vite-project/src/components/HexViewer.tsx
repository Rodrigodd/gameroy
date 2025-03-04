import React, {
  useRef,
  useState,
  useMemo,
  useEffect,
  useImperativeHandle,
  useCallback,
  memo,
} from "react";
import { useVirtualizer } from "@tanstack/react-virtual";

const BYTES_PER_ROW = 16;
const ROW_HEIGHT = 20;

interface Symbol {
  name: string;
  address: number;
  size: number;
  color: string;
}

interface HexViewerSection {
  data: Uint8Array;
  symbols: Symbol[];
}

export interface HexViewerElement {
  jumpToAddress: (address: number) => void;
}

interface HexRowProps {
  section: (start: number, end: number) => HexViewerSection;
  virtualRowIndex: number;
  virtualRowStart: number;
  addressPad: number;
  setHoveredByte: React.Dispatch<React.SetStateAction<number | null>>;
  setSelection: React.Dispatch<
    React.SetStateAction<{ start: number; end: number } | null>
  >;
  setDragging: (drag: boolean) => void;
  hoveredByte: number | null;
  selectionStart: number;
  selectionEnd: number;
}

const formatHex = (byte: number): string =>
  byte.toString(16).padStart(2, "0").toUpperCase();
const formatAscii = (byte: number): string =>
  byte == 32
    ? "\u2009"
    : byte >= 32 && byte < 127
      ? String.fromCharCode(byte)
      : ".";

const HexRow = memo<HexRowProps>(
  ({
    virtualRowIndex,
    virtualRowStart,
    section,
    addressPad,
    setHoveredByte,
    setSelection,
    setDragging,
    hoveredByte,
    selectionStart,
    selectionEnd,
  }: HexRowProps) => {
    const startOffset = virtualRowIndex * BYTES_PER_ROW;
    const endOffset = startOffset + BYTES_PER_ROW;
    const { data, symbols } = useMemo(
      () => section(startOffset, endOffset),
      [section, startOffset, endOffset],
    );
    console.log("Rerendering HexRow at index", virtualRowIndex);

    const start = virtualRowIndex * BYTES_PER_ROW;
    const end = start + BYTES_PER_ROW;
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
        key={virtualRowIndex}
        style={{
          position: "absolute",
          top: virtualRowStart,
          height: ROW_HEIGHT,
          width: "100%",
          display: "flex",
          alignItems: "center",
        }}
      >
        <div style={{ marginRight: 10 }}>
          {start.toString(16).padStart(addressPad, "0").toUpperCase()}:
        </div>
        <div style={{ flex: 1, display: "flex" }}>
          {segments.map((seg, i) => (
            <span
              key={i}
              style={{
                backgroundColor: seg.symbol ? seg.symbol.color : "transparent",
                borderTopLeftRadius: seg.truncatedStart ? "0" : "3px",
                borderBottomLeftRadius: seg.truncatedStart ? "0" : "3px",
                borderTopRightRadius: seg.truncatedEnd ? "0" : "3px",
                borderBottomRightRadius: seg.truncatedEnd ? "0" : "3px",
                position: "relative",
                display: "flex",
              }}
              title={
                seg.symbol
                  ? `${seg.symbol.name} @ 0x${seg.symbol.address.toString(16).toUpperCase()}`
                  : ""
              }
            >
              {Array.from(
                data.slice(seg.start - startOffset, seg.end - startOffset),
              ).map((byte, j) => (
                <span
                  key={j}
                  onMouseEnter={() => setHoveredByte(seg.start + j)}
                  onMouseLeave={() => setHoveredByte(null)}
                  onMouseDown={() => {
                    setSelection(null);
                    setDragging(true);
                  }}
                  onMouseMove={() => {
                    const byteIndex = seg.start + j;
                    setSelection((prev) => {
                      return {
                        start: prev?.start ?? byteIndex,
                        end: byteIndex + 1,
                      };
                    });
                  }}
                  style={{
                    backgroundColor:
                      hoveredByte == seg.start + j
                        ? "rgb(150, 200, 150, 0.4)"
                        : seg.start + j >= selectionStart &&
                            seg.start + j < selectionEnd
                          ? "rgb(150, 200, 150, 0.2)"
                          : "transparent",
                    padding: "0px 2px",
                  }}
                >
                  {formatHex(byte)}
                </span>
              ))}
            </span>
          ))}
        </div>
        <div style={{ marginLeft: "10px", display: "flex", gap: "0px" }}>
          {segments.map((seg, i) => (
            <span
              key={i}
              style={{
                backgroundColor: seg.symbol ? seg.symbol.color : "transparent",
                borderTopLeftRadius: seg.truncatedStart ? "0" : "3px",
                borderBottomLeftRadius: seg.truncatedStart ? "0" : "3px",
                borderTopRightRadius: seg.truncatedEnd ? "0" : "3px",
                borderBottomRightRadius: seg.truncatedEnd ? "0" : "3px",
                position: "relative",
                display: "flex",
              }}
            >
              {Array.from(
                data.slice(seg.start - startOffset, seg.end - startOffset),
              ).map((byte, j) => (
                <span
                  key={j}
                  style={{
                    backgroundColor:
                      hoveredByte == seg.start + j
                        ? "rgb(150, 200, 150, 0.4)"
                        : seg.start + j >= selectionStart &&
                            seg.start + j < selectionEnd
                          ? "rgb(150, 200, 150, 0.2)"
                          : "transparent",
                  }}
                  onMouseEnter={() => setHoveredByte(seg.start + j)}
                  onMouseLeave={() => setHoveredByte(null)}
                  onMouseDown={() => {
                    setSelection(null);
                    setDragging(true);
                  }}
                  onMouseMove={() => {
                    const byteIndex = seg.start + j;
                    setSelection((prev) => {
                      return {
                        start: prev?.start ?? byteIndex,
                        end: byteIndex + 1,
                      };
                    });
                  }}
                >
                  {formatAscii(byte)}
                </span>
              ))}
            </span>
          ))}
        </div>
      </div>
    );
  },
);
HexRow.displayName = "HexRow";

interface HexViewerProps {
  section: (start: number, end: number) => HexViewerSection;
  length: number;
  ref?: React.Ref<HexViewerElement>;
}

const HexViewer: React.FC<HexViewerProps> = ({ section, length, ref }) => {
  const parentRef = useRef<HTMLDivElement>(null);
  const rowCount = Math.ceil(length / BYTES_PER_ROW);
  const [hoveredByte, setHoveredByte] = useState<number | null>(null);
  const dragging = useRef(false);
  const [selection, setSelection] = useState<{
    start: number;
    end: number;
  } | null>(null);

  const addressPad = useMemo(
    () => Math.max(4, Math.ceil(Math.log2(length) / 4)),
    [length],
  );

  const [selectionStart, selectionEnd] = useMemo(() => {
    if (selection == null) {
      return [0, 0];
    }
    return [
      Math.min(selection.start, selection.end),
      Math.max(selection.start, selection.end),
    ];
  }, [selection]);

  const virtualizer = useVirtualizer({
    count: rowCount,
    getScrollElement: useCallback(() => parentRef.current, []),
    estimateSize: useCallback(() => ROW_HEIGHT, []),
  });

  useImperativeHandle(ref, () => ({
    jumpToAddress: (address: number) => {
      console.log("Jumping to address", address);
      if (!isNaN(address)) {
        const rowIndex = Math.floor(address / BYTES_PER_ROW);
        virtualizer.scrollToIndex(rowIndex, { align: "start" });
      }
    },
  }));

  useEffect(() => {
    const callback = () => (dragging.current = false);
    window.addEventListener("mouseup", callback);
    return () => {
      window.removeEventListener("mouseup", callback);
    };
  }, []);

  const setSelectionOnMove = useCallback(
    (sel: React.SetStateAction<{ start: number; end: number } | null>) => {
      if (sel == null) {
        setSelection(null);
        return;
      }
      if (dragging.current) {
        setSelection(sel);
      }
    },
    [],
  );

  const setDragging = useCallback((drag: boolean) => {
    dragging.current = drag;
  }, []);

  const virtualItems = virtualizer.getVirtualItems();

  return (
    <div style={{ fontFamily: "monospace" }}>
      <div ref={parentRef} style={{ height: "500px", overflow: "auto" }}>
        <div
          style={{ height: virtualizer.getTotalSize(), position: "relative" }}
        >
          {virtualItems.map((virtualRow) => {
            const startOffset = virtualRow.index * BYTES_PER_ROW;
            const endOffset = Math.min(
              (virtualRow.index + 1) * BYTES_PER_ROW,
              length,
            );
            const isInSelection =
              startOffset < selectionEnd && endOffset > selectionStart;
            return (
              <HexRow
                key={virtualRow.index}
                section={section}
                virtualRowIndex={virtualRow.index}
                virtualRowStart={virtualRow.start}
                addressPad={addressPad}
                setHoveredByte={setHoveredByte}
                setSelection={setSelectionOnMove}
                setDragging={setDragging}
                hoveredByte={
                  hoveredByte &&
                  hoveredByte >= startOffset &&
                  hoveredByte < endOffset
                    ? hoveredByte
                    : null
                }
                selectionStart={isInSelection ? selectionStart : 0}
                selectionEnd={isInSelection ? selectionEnd : 0}
              />
            );
          })}
        </div>
      </div>
    </div>
  );
};

export default HexViewer;
