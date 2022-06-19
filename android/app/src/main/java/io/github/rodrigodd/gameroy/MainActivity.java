package io.github.rodrigodd.gameroy;

import android.app.NativeActivity;
import android.os.Bundle;

public class MainActivity extends NativeActivity {

    static {
        System.loadLibrary("gameroy_android");
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        helloWorld();
    }

    private native void helloWorld();
}
