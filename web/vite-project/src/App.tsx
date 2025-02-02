import React, { useState } from "react";
import "./App.css";
import { Item } from "./interfaces";
import { Game } from "./screens/Game";

const initialItems: Item[] = [
  // { title: "Item Title 1", lastPlayed: "2024-09-15", size: "1024 KiB" },
  // { title: "Item Title 2", lastPlayed: "2024-09-10", size: "512 KiB" },
];

const Header = () => {
  return (
    <header className="header">
      <h3 className="title">Web App Mockup</h3>
      <div className="buttons">
        <button>📁</button>
        <button>📄</button>
        <button>🔍</button>
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
      const file = files[0];
      const newItem = {
        title: file.name,
        lastPlayed: new Date().toISOString().split("T")[0],
        size: `${(file.size / 1024).toFixed(2)} KiB`,
        file: file,
      };
      onDrop(newItem);
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

  const addItem = (newItem: Item) => {
    setItems((prevItems) => [...prevItems, newItem]);
  };

  return (
    <div style={{ height: '100%' }}>
      <div className={"container" + (selectedItem == null ? " hidden" : "")}>
        <Game item={selectedItem} onBack={() => setSelectedItem(null)} />
      </div>
      <div className={"container" + (selectedItem != null ? " hidden" : "")}>
        <Header />
        <RomList items={items} onDrop={addItem} onItemClick={setSelectedItem} />
      </div>
    </div>
  );
};

export default App;
