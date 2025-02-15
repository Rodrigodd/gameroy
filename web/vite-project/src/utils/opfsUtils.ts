import { Item } from "../interfaces";

// Save a file to OPFS and return an Item object
export async function saveFileToOPFS(file: File): Promise<Item | null> {
  try {
    const root = await navigator.storage.getDirectory();
    const fileHandle = await root.getFileHandle(file.name, { create: true });
    const writable = await fileHandle.createWritable();

    await writable.write(file);
    await writable.close();

    return {
      title: file.name,
      lastPlayed: new Date().toISOString().split("T")[0],
      size: `${(file.size / 1024).toFixed(2)} KiB`,
      file,
    };
  } catch (error) {
    console.error("Failed to save file to OPFS:", error);
    return null;
  }
}

// Load all files stored in OPFS and return them as an array of Item objects
export async function loadFilesFromOPFS(): Promise<Item[]> {
  try {
    const root = await navigator.storage.getDirectory();
    const items: Item[] = [];

    for await (const [key, entry] of root.entries()) {
      console.log(key, entry);
      if (entry.kind === "file") {
        const fileEntry = entry as FileSystemFileHandle;
        const file = await fileEntry.getFile();
        items.push({
          title: file.name,
          lastPlayed: new Date().toISOString().split("T")[0],
          size: `${(file.size / 1024).toFixed(2)} KiB`,
          file,
        });
      }
    }

    return items;
  } catch (error) {
    console.error("Failed to load files from OPFS:", error);
    return [];
  }
}
