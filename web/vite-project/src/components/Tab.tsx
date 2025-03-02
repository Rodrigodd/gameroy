import { useState, ReactNode } from "react";

interface TabProps {
  label: string;
  children: ReactNode;
}

interface TabPanelProps {
  children: React.ReactElement<TabProps>[];
}

const TabPanel: React.FC<TabPanelProps> = ({ children }: TabPanelProps) => {
  const [activeIndex, setActiveIndex] = useState(0);

  console.log(activeIndex);

  return (
    <div className="tab-panel">
      <div className="tab-header">
        {children.map((tab, index) => (
          <button
            key={index}
            className={
              activeIndex === index ? "tab-button active" : "tab-button"
            }
            onClick={() => setActiveIndex(index)}
          >
            {tab.props.label}
          </button>
        ))}
      </div>
      <div className="tab-content">
        {children.map((tab, index) => {
          console.log(activeIndex === index);
          return (
            <div
              key={index + 1000}
              className="tab-panel-content"
              style={{ display: activeIndex === index ? "block" : "none" }}
            >
              {tab.props.children}
            </div>
          );
        })}
      </div>
    </div>
  );
};

const Tab: React.FC<TabProps> = ({ children }: TabProps) => {
  return <>{children}</>;
};

export { TabPanel, Tab };
