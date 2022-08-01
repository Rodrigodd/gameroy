package io.github.rodrigodd.gameroy;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicInteger;
import java.nio.channels.Channels;
import java.nio.ByteBuffer;

import android.app.NativeActivity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.net.Uri;

public class MainActivity extends NativeActivity {

    static {
        System.loadLibrary("gameroy_android");
    }
    
    private static final int FILE_PICKER_CODE = 123;
    private static final String TAG = "gameroy";


    private static final AtomicInteger nextID = new AtomicInteger(1);

    // TODOo: make this persiste Activity destruction.
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

    public ByteBuffer readUri(String uriString) {
        Uri uri = Uri.parse(uriString);
        try {
            InputStream in = getContentResolver().openInputStream(uri);
            ByteBuffer buffer = ByteBuffer.allocateDirect(in.available());
            Channels.newChannel(in).read(buffer);
            buffer.position(0);

            return buffer;
        } catch (IOException ex) {
            Log.d(TAG, "error reading uri: " + ex.toString());
            return null;
        }
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

    private native void filePickerResult(long callback_ptr, String uri);
}
