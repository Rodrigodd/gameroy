import { Item } from "../interfaces";

function crc32(buffer: ArrayBuffer): string {
  const table = new Uint32Array(256);

  // Initialize the CRC32 table
  for (let i = 0; i < 256; i++) {
    let crc = i;
    for (let j = 0; j < 8; j++) {
      crc = (crc & 1) ? (0xedb88320 ^ (crc >>> 1)) : (crc >>> 1);
    }
    table[i] = crc;
  }

  let crc = 0xffffffff;
  const view = new Uint8Array(buffer);

  for (let i = 0; i < view.length; i++) {
    crc = (crc >>> 8) ^ table[(crc ^ view[i]) & 0xff];
  }

  return ((crc ^ 0xffffffff) >>> 0).toString(16).padStart(8, "0");
}

// Save a file to OPFS and return an Item object
export async function saveFileToOPFS(file: File): Promise<Item | null> {
  try {
    const root = await navigator.storage.getDirectory();
    const fileHandle = await root.getFileHandle(file.name, { create: true });
    const writable = await fileHandle.createWritable();

    await writable.write(file);
    await writable.close();

    const crc = await new Response(file).arrayBuffer().then(crc32);

    return {
      title: file.name,
      lastPlayed: new Date().toISOString().split("T")[0],
      size: `${(file.size / 1024).toFixed(2)} KiB`,
      file,
      crc32: crc,
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
        const crc = await new Response(file).arrayBuffer().then(crc32);
        items.push({
          title: file.name,
          lastPlayed: new Date().toISOString().split("T")[0],
          size: `${(file.size / 1024).toFixed(2)} KiB`,
          file,
          crc32: crc,
        });
      }
    }

    return items;
  } catch (error) {
    console.error("Failed to load files from OPFS:", error);
    return [];
  }
}

export async function deleteFileFromOPFS(fileName: string): Promise<void> {
  try {
    const root = await navigator.storage.getDirectory();
    await root.removeEntry(fileName);
    console.log(`Deleted: ${fileName}`);
  } catch (error) {
    console.error("Failed to delete file:", error);
  }
}
