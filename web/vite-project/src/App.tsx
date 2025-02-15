import React, { useState, useEffect, useRef } from "react";
import "./App.css";
import { Item } from "./interfaces";
import { Game } from "./screens/Game";
import {
  saveFileToOPFS,
  loadFilesFromOPFS,
  deleteFileFromOPFS,
} from "./utils/opfsUtils";

const initialItems: Item[] = [];

const Header = ({
  onFileSelect,
}: {
  onFileSelect: (newItem: Item) => void;
}) => {
  const handleFileSelect = (event: React.ChangeEvent<HTMLInputElement>) => {
    const file = event.target.files?.[0];
    if (file) {
      void saveFileToOPFS(file).then((newItem) => {
        if (newItem) onFileSelect(newItem);
      });
    }
  };

  return (
    <header className="header">
      <h3 className="title">Web App Mockup</h3>
      <div className="buttons">
        <button>📁</button>
        <button onClick={() => document.getElementById("fileInput")?.click()}>
          📄
        </button>
        <button>🔍</button>
        <input
          type="file"
          id="fileInput"
          style={{ display: "none" }}
          onChange={handleFileSelect}
        />
      </div>
    </header>
  );
};

interface ListItemProps {
  item: Item;
  onClick: (item: Item) => void;
  onRemove: (item: Item) => void;
}

const ListItem = ({ item, onClick, onRemove }: ListItemProps) => {
  const [menuOpen, setMenuOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  const menuOpenRef = useRef(menuOpen);

  menuOpenRef.current = menuOpen;

  // Close menu if clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        menuOpenRef.current &&
        menuRef.current &&
        !menuRef.current.contains(event.target as Node)
      ) {
        event.stopPropagation();
        setMenuOpen(false);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  const handleMenuClick = (event: React.MouseEvent, action: string) => {
    console.log("Menu Clicked: ", action);
    event.stopPropagation();
    setMenuOpen(false); // Close menu after action
    if (action === "play") {
      onClick(item);
    } else if (action === "remove") {
      onRemove(item);
    }
  };

  const menuOnClick = (event: React.MouseEvent) => {
    event.stopPropagation();
    setMenuOpen(!menuOpen);
  };

  return (
    <div onClick={() => onClick(item)} className="item">
      <div className="thumbnail"></div>
      <div className="info">
        <h3>{item.title}</h3>
        <p>Last Played: {item.lastPlayed}</p>
        <p className="size">Size: {item.size}</p>
      </div>

      {/* Menu Button & Dropdown */}
      <div className="menu-container" ref={menuRef}>
        <button className="menu-button" onClick={menuOnClick}>
          ⋮
        </button>

        {menuOpen && (
          <div className="menu-dropdown">
            <button onClick={(e) => handleMenuClick(e, "play")}>▶ Play</button>
            <button onClick={(e) => handleMenuClick(e, "remove")}>
              🗑 Remove
            </button>
            <button onClick={(e) => handleMenuClick(e, "clear-save")}>
              💾 Clear Save
            </button>
          </div>
        )}
      </div>
    </div>
  );
};

interface MainProps {
  items: Item[];
  setItems: React.Dispatch<React.SetStateAction<Item[]>>;
  onItemClick: (item: Item) => void;
}

const RomList = ({ items, setItems, onItemClick }: MainProps) => {
  const [isDragging, setIsDragging] = useState(false);

  const handleDragOver = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = () => {
    setIsDragging(false);
  };

  const addItem = (newItem: Item) => {
    setItems((prevItems) => [...prevItems, newItem]);
  };

  const removeItem = (removedItem: Item) => {
    setItems((prevItems) =>
      prevItems.filter((item) => item.title !== removedItem.title),
    );
  };

  const handleDrop = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(false);
    const files = event.dataTransfer.files;
    if (files.length > 0) {
      void saveFileToOPFS(files[0]).then((newItem) => {
        if (newItem) addItem(newItem);
      });
    }
  };

  const handleRemoveItem = (item: Item) => {
    removeItem(item);
    void deleteFileFromOPFS(item.title);
  };

  return (
    <main
      className="main"
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
    >
      {isDragging && <div className="drop-message">Drop Here</div>}
      <div className="item-list">
        {items.map((item, index) => (
          <ListItem
            onClick={onItemClick}
            onRemove={handleRemoveItem}
            key={index}
            item={item}
          />
        ))}
      </div>
    </main>
  );
};

const App = () => {
  const [items, setItems] = useState(initialItems);
  const [selectedItem, setSelectedItem] = useState<Item | null>(null);

  useEffect(() => {
    void loadFilesFromOPFS().then(setItems);
  }, []);

  const addItem = (newItem: Item) => {
    setItems((prevItems) => [...prevItems, newItem]);
  };

  return (
    <div style={{ height: "100%" }}>
      <div className={"container" + (selectedItem == null ? " hidden" : "")}>
        <Game item={selectedItem} onBack={() => setSelectedItem(null)} />
      </div>
      <div className={"container" + (selectedItem != null ? " hidden" : "")}>
        <Header onFileSelect={addItem} />
        <RomList
          items={items}
          setItems={setItems}
          onItemClick={setSelectedItem}
        />
      </div>
    </div>
  );
};

export default App;
