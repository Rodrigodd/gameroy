import { Item } from "../interfaces";

type Database = Record<string, string>;

let db: Database | null = null;

// Open database.json file, and load it into a crc32->name map.
async function getDatabase(): Promise<Database> {
  if (db) {
    return db;
  }


  const json: unknown = await fetch("/src/assets/database.json").then((res) => res.json());
  if (!Array.isArray(json)) {
    throw new Error("Database is not an array");
  }

  db = {};
  for (const item_ of json) {
    const item: unknown = item_;
    if (!(item && typeof item === "object")) {
      throw new Error("Item is not an object: " + JSON.stringify(item));
    }
    if (!("crc" in item && "name" in item)) {
      // throw new Error("No crc or name in item: " + JSON.stringify(item));
      console.log("No crc or name in item: " + JSON.stringify(item));
      continue;
    }
    const crc: unknown = item.crc;
    const title: unknown = item.name;
    if (typeof crc !== "string" || typeof title !== "string") {
      throw new Error("Invalid database.json");
    }

    db[crc.toLowerCase()] = title;
  }

  return db;
}

export async function getThumbnailUri(item: Item): Promise<string | null> {
  const database = await getDatabase();
  const title = database[item.crc32];

  if (!title) {
    return null;
  }

  return `https://thumbnails.libretro.com/Nintendo%20-%20Game%20Boy/Named_Boxarts/${title}.png`;
}
