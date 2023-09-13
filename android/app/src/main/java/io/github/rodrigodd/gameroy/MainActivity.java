package io.github.rodrigodd.gameroy;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicInteger;

import android.app.Activity;
import android.app.NativeActivity;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.provider.DocumentsContract;
import android.util.Log;
import android.webkit.WebView;

public class MainActivity extends NativeActivity {

    static {
        System.loadLibrary("gameroy_android");
    }
    
    private static final int FILE_PICKER_CODE = 123;
    private static final String TAG = "gameroy";


    private static final AtomicInteger nextID = new AtomicInteger(1);

    // TODO: make this persiste Activity destruction.
    private Hashtable<Integer, Long> callback_ptrs = new Hashtable<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    // NOTE: this assumes that pointers have 8 bytes or less.
    public void launchFilePicker(long callback_ptr) {
        // if a request is made every second, this will overflow in 68 years, and subsequent
        // requests will fail.
        int request_id = nextID.incrementAndGet();

        Log.d(TAG, "launchFilePicker");
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT); 
        intent.setType("*/*"); 
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        Intent chooser = intent.createChooser(intent, "Select a .gb Rom File");
        callback_ptrs.put(request_id, callback_ptr);
        startActivityForResult(
            chooser,
            request_id
        );
    }

    // NOTE: this assumes that pointers have 8 bytes or less.
    public void launchFolderPicker(long callback_ptr) {
        // if a request is made every second, this will overflow in 68 years, and subsequent
        // requests will fail.
        int request_id = nextID.incrementAndGet();

        Log.d(TAG, "launchFilePicker");
        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE); 
        intent.addCategory(Intent.CATEGORY_DEFAULT);
        Intent chooser = intent.createChooser(intent, "Choose folder to list roms");
        callback_ptrs.put(request_id, callback_ptr);
        startActivityForResult(
            chooser,
            request_id
        );
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        Long callback_ptr = callback_ptrs.get(requestCode);
        if (callback_ptr != null) {
            callback_ptrs.remove(requestCode);
            Log.d(TAG, "onActivityResult");
            if (resultCode == RESULT_OK) {
                Uri uri = data.getData();
                Log.d(TAG, "File Uri: " + uri.toString());

                String uriString = uri.toString();
                Log.d(TAG, "Encode Uri: " + uriString);
                filePickerResult(callback_ptr.longValue(), uriString);
            } else {
                filePickerResult(callback_ptr.longValue(), null);
            }
        }
    }

    /// If `bytes` is 0, read the entiry file. Otherwise read only the first `bytes` bytes.
    public ByteBuffer readUri(String uriString, int bytes) {
        Log.d(TAG, "Read Uri: " + uriString);
        Uri uri = Uri.parse(uriString);
        try {
            InputStream in = getContentResolver().openInputStream(uri);
            ByteBuffer buffer = ByteBuffer.allocateDirect(bytes == 0 ? in.available() : bytes);
            Channels.newChannel(in).read(buffer);
            buffer.position(0);

            return buffer;
        } catch (IOException ex) {
            Log.e(TAG, "error reading uri: " + ex.toString());
            return null;
        }
    }

    public String[] listChild(String uriString) {
        Log.d(TAG, "List Child of Uri: " + uriString);
        Uri treeUri = Uri.parse(uriString);
        ContentResolver resolver = getContentResolver();

        int takeFlags = Intent.FLAG_GRANT_READ_URI_PERMISSION;
        resolver.takePersistableUriPermission(treeUri, takeFlags);

        Uri childrenUri = DocumentsContract.buildChildDocumentsUriUsingTree(
            treeUri, DocumentsContract.getTreeDocumentId(treeUri));

        ArrayList<String> uris = new ArrayList<>();
        Cursor c = null;

        try {
            String[] projections = new String[] {
                DocumentsContract.Document.COLUMN_DOCUMENT_ID,
                DocumentsContract.Document.COLUMN_DISPLAY_NAME
            };

            // I could not make the query sorting work, but the rom list ui will sort it anyway.
            c = resolver.query(childrenUri, projections, null, null, null);

            if (c != null && c.moveToFirst()) {
                do {
                    String documentId = c.getString(0);
                    String name = c.getString(1);

                    if (!name.endsWith(".gb")) continue;

                    String documentUri = DocumentsContract.buildDocumentUriUsingTree(
                            treeUri, documentId).toString();

                    uris.add(documentUri);

                } while(c.moveToNext());
            }
        } catch(Exception ex) {
            Log.e(TAG, "error listing child of uri: " + ex.toString());
            uris = null;
        } finally {
            if (c!=null) c.close();
        }

        if (uris == null) return null;
        return uris.toArray(new String[0]);
    }

    public void saveRam(String filename, ByteBuffer data) {
        try {
            FileOutputStream fos = openFileOutput(filename, MODE_PRIVATE);
            Channels.newChannel(fos).write(data);
        } catch (IOException ex) {
            Log.d(TAG, "error saving ram: " + ex.toString());
        }
    }

    public ByteBuffer loadRam(String filename) {
        try {
            FileInputStream fis = openFileInput(filename);
            ByteBuffer buffer = ByteBuffer.allocateDirect(fis.available());
            Channels.newChannel(fis).read(buffer);
            buffer.position(0);

            return buffer;
        } catch (IOException ex) {
            Log.d(TAG, "error saving ram: " + ex.toString());
            return null;
        }
    }

    public long getFileDate(String filename) {
        try {
            File file = new File(getFilesDir(), filename);
            // Get time in milliseconds since epoch.
            return file.lastModified();
        } catch (SecurityException ex) {
            Log.d(TAG, "error getting file date: " + ex.toString());
            return 0;
        }

    }

    final String preference_file_key = "io.github.rodrigodd.gameroy.PREFERENCE_FILE_KEY";

    public void savePreferences(String config) {
        SharedPreferences sharedPref = getSharedPreferences(preference_file_key, MODE_PRIVATE);
        sharedPref.edit()
            .putString("config", config)
            .apply();
    }

    public String loadPreferences() {
        SharedPreferences sharedPref = getSharedPreferences(preference_file_key, MODE_PRIVATE);
        return sharedPref.getString("config", null);
    }
    
    public static class ViewWeb extends Activity {
        @Override
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.webview);  
            WebView wv;  
            wv = (WebView) findViewById(R.id.webview);  
            wv.loadUrl("file:///android_asset/license.html");
        }
    }
    
    public void showLicenses() {
        Intent intent = new Intent(this, ViewWeb.class);
        startActivity(intent);
    }


    private native void filePickerResult(long callback_ptr, String uri);
}
