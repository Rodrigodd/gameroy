import React, { useState, useRef, useEffect } from "react";

interface ResizablePanelProps {
  children: React.ReactNode;
  corner?: "bottom-right" | "bottom-left" | "top-right" | "top-left";
  resizeAxis?: "both" | "horizontal" | "vertical";
  style?: React.CSSProperties;
  className?: string;
  handleStyle?: React.CSSProperties;
}

interface Size {
  width: number;
  height: number;
}

interface MousePosition {
  x: number;
  y: number;
}

const ResizablePanel: React.FC<ResizablePanelProps> = ({
  children,
  corner = "bottom-right",
  resizeAxis = "both",
  style = {},
  handleStyle = {},
  className,
}) => {
  const panelRef = useRef<HTMLDivElement>(null);
  const [size, setSize] = useState<Size>({ width: 300, height: 200 });
  const resizing = useRef<boolean>(false);
  const startMouse = useRef<MousePosition | null>(null);
  const startSize = useRef<Size | null>(null);

  const handleMouseDown = (e: React.MouseEvent<HTMLDivElement>) => {
    resizing.current = true;
    startMouse.current = { x: e.screenX, y: e.screenY };
    startSize.current = size;
  };

  const handleMouseMove = (e: MouseEvent) => {
    if (!resizing.current || !startMouse.current || !startSize.current) return;

    const deltaX = e.screenX - startMouse.current.x;
    const deltaY = e.screenY - startMouse.current.y;

    let newWidth = startSize.current.width;
    let newHeight = startSize.current.height;

    if (resizeAxis !== "vertical") {
      if (corner.includes("right")) {
        newWidth = Math.max(150, startSize.current.width + deltaX);
      } else {
        newWidth = Math.max(150, startSize.current.width - deltaX);
      }
    }

    if (resizeAxis !== "horizontal") {
      if (corner.includes("bottom")) {
        newHeight = Math.max(100, startSize.current.height + deltaY);
      } else {
        newHeight = Math.max(100, startSize.current.height - deltaY);
      }
    }

    setSize({ width: newWidth, height: newHeight });
  };

  const handleMouseUp = () => {
    resizing.current = false;
    startMouse.current = null;
    startSize.current = null;
  };

  useEffect(() => {
    window.addEventListener("mousemove", handleMouseMove);
    window.addEventListener("mouseup", handleMouseUp);
    return () => {
      window.removeEventListener("mousemove", handleMouseMove);
      window.removeEventListener("mouseup", handleMouseUp);
    };
  }, []);

  const cornerStyles: Record<string, React.CSSProperties> = {
    "bottom-right": { bottom: 0, right: 0, cursor: "nwse-resize" },
    "bottom-left": { bottom: 0, left: 0, cursor: "nesw-resize" },
    "top-right": { top: 0, right: 0, cursor: "nesw-resize" },
    "top-left": { top: 0, left: 0, cursor: "nwse-resize" },
  };

  const clipPaths: Record<string, string> = {
    "bottom-right": "polygon(100% 0, 100% 100%, 0 100%)",
    "bottom-left": "polygon(0 0, 100% 100%, 0 100%)",
    "top-right": "polygon(100% 0, 100% 100%, 0 0)",
    "top-left": "polygon(0 0, 100% 0, 0 100%)",
  };

  return (
    <div
      ref={panelRef}
      style={{
        width: resizeAxis !== "vertical" ? size.width : "auto",
        height: resizeAxis !== "horizontal" ? size.height : "auto",
        position: "relative",
        overflow: "hidden",
        display: "flex",
        flexDirection: "column",
        ...style,
      }}
      className={className}
    >
      <div
        style={{
          flex: 1,
          overflow: "auto",
        }}
      >
        {children}
      </div>
      <div
        style={{
          position: "absolute",
          width: "16px",
          height: "16px",
          clipPath: clipPaths[corner],
          backgroundColor: "var(--color-bright-green)",
          ...cornerStyles[corner],
          ...handleStyle,
        }}
        onMouseDown={handleMouseDown}
      ></div>
    </div>
  );
};

export default ResizablePanel;
