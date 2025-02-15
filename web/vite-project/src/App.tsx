import React, { useState, useEffect } from "react";
import "./App.css";
import { Item } from "./interfaces";
import { Game } from "./screens/Game";
import { saveFileToOPFS, loadFilesFromOPFS } from "./utils/opfsUtils";

const initialItems: Item[] = [];

const Header = ({ onFileSelect }: { onFileSelect: (newItem: Item) => void }) => {
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
}

const ListItem = ({ item, onClick }: ListItemProps) => {
  return (
    <div onClick={() => onClick(item)} className="item">
      <div className="thumbnail"></div>
      <div className="info">
        <h3>{item.title}</h3>
        <p>Last Played: {item.lastPlayed}</p>
        <p className="size">Size: {item.size}</p>
      </div>
    </div>
  );
};

interface MainProps {
  items: Item[];
  onDrop: (newItem: Item) => void;
  onItemClick: (item: Item) => void;
}

const RomList = ({ items, onDrop, onItemClick }: MainProps) => {
  const [isDragging, setIsDragging] = useState(false);

  const handleDragOver = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = () => {
    setIsDragging(false);
  };

  const handleDrop = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(false);
    const files = event.dataTransfer.files;
    if (files.length > 0) {
      void saveFileToOPFS(files[0]).then((newItem) => {
        if (newItem) onDrop(newItem);
      });
    }
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
          <ListItem onClick={onItemClick} key={index} item={item} />
        ))}
      </div>
    </main>
  );
};

const App = () => {
  const [items, setItems] = useState(initialItems);
  const [selectedItem, setSelectedItem] = useState<Item | null>(null);

  useEffect(() => {
    loadFilesFromOPFS().then(setItems);
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
        <RomList items={items} onDrop={addItem} onItemClick={setSelectedItem} />
      </div>
    </div>
  );
};

export default App;
