import { useEffect, useRef, useState } from "react";

export interface MenuButton {
  separator?: false;
  label: string;
  action: string;
}

export interface MenuSeparator {
  separator: true;
}

export type MenuItem = MenuButton | MenuSeparator;

export interface MenuProps {
  items: MenuItem[];
  onSelect: (action: string) => void;
}

export const Menu = ({ items, onSelect }: MenuProps) => {
  const [menuOpen, setMenuOpen] = useState(false);
  const menuRef = useRef<HTMLDivElement>(null);
  const menuOpenRef = useRef(menuOpen);
  menuOpenRef.current = menuOpen;

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (
        menuOpenRef.current &&
        menuRef.current &&
        !menuRef.current.contains(event.target as Node)
      ) {
        setMenuOpen(false);
      }
    };

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  const handleMenuClick = (event: React.MouseEvent, action: string) => {
    event.stopPropagation();
    setMenuOpen(false);
    onSelect(action);
  };

  return (
    <div className="menu-container" ref={menuRef}>
      <button
        className="menu-button"
        onClick={(e) => {
          e.stopPropagation();
          setMenuOpen(!menuOpen);
        }}
      >
        ⋮
      </button>
      {menuOpen && (
        <div className="menu-dropdown">
          {items.map((item, index) =>
            item.separator ? (
              <hr key={index} className="menu-separator" />
            ) : (
              <button
                key={index}
                onClick={(e) => handleMenuClick(e, item.action)}
              >
                {item.label}
              </button>
            ),
          )}
        </div>
      )}
    </div>
  );
};
