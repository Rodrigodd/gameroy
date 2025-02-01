import React, { useState } from "react";
import "./App.css";

interface Item {
  title: string;
  lastPlayed: string;
  size: string;
}

const initialItems: Item[] = [
  { title: "Item Title 1", lastPlayed: "2024-09-15", size: "1024 KiB" },
  { title: "Item Title 2", lastPlayed: "2024-09-10", size: "512 KiB" },
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

interface ItemProps {
  item: Item;
  onClick: (item: Item) => void;
}

const Item = ({ item, onClick }: ItemProps) => {
  return (
    <div onDoubleClick={() => onClick(item)} className="item">
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
}

const Main = ({ items, onDrop }: MainProps) => {
  const [isDragging, setIsDragging] = useState(false);

  const handleDragOver = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(true);
    console.log("Drag over detected");
  };

  const handleDragLeave = () => {
    setIsDragging(false);
    console.log("Drag leave detected");
  };

  const handleDrop = (event: React.DragEvent) => {
    event.preventDefault();
    setIsDragging(false);
    console.log("Drop event detected");
    const files = event.dataTransfer.files;
    if (files.length > 0) {
      console.log("File dropped:", files[0].name);
      const newItem = {
        title: files[0].name,
        lastPlayed: new Date().toISOString().split("T")[0],
        size: `${(files[0].size / 1024).toFixed(2)} KiB`,
      };
      onDrop(newItem);
    }
  };

  const onItemClick = (item: Item) => {
    console.log("Item clicked:", item.title);
  }

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
          <Item onClick={onItemClick} key={index} item={item} />
        ))}
      </div>
    </main>
  );
};

const App = () => {
  const [items, setItems] = useState(initialItems);

  const addItem = (newItem: Item) => {
    console.log("Adding new item:", newItem);
    setItems((prevItems) => [...prevItems, newItem]);
  };

  return (
    <div className="container">
      <Header />
      <Main items={items} onDrop={addItem} />
    </div>
  );
};

export default App;
