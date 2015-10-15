import android.hardware.input.InputManager;
import android.os.SystemClock;
import android.util.Log;
import android.view.InputDevice;
import android.view.KeyCharacterMap;
import android.view.KeyEvent;
import android.view.MotionEvent;

import android.net.LocalServerSocket;
import android.net.LocalSocket;
import android.view.InputEvent;
import dalvik.system.DexClassLoader;
import java.io.File;
import java.io.FileDescriptor;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Command that sends key events to the device, either by their keycode, or by
 * desired character output.
 */

public class Input {
    private native boolean checkPerm(FileDescriptor fd);
    private native static int getUid();
    private static String optCacheDirName;
    static {
        System.loadLibrary("t1wrench-jni");
        optCacheDirName = "/data/data/com.android.shell/t1wrench." + String.format("%d", getUid());
    }

    private static final String TAG = "Input";
    private static final String INVALID_ARGUMENTS = "Error: Invalid arguments for command: ";

    private static final Map<String, Integer> SOURCES = new HashMap<String, Integer>() {{
        put("keyboard", InputDevice.SOURCE_KEYBOARD);
        put("dpad", InputDevice.SOURCE_DPAD);
        put("gamepad", InputDevice.SOURCE_GAMEPAD);
        put("touchscreen", InputDevice.SOURCE_TOUCHSCREEN);
        put("mouse", InputDevice.SOURCE_MOUSE);
        put("stylus", InputDevice.SOURCE_STYLUS);
        put("trackball", InputDevice.SOURCE_TRACKBALL);
        put("touchpad", InputDevice.SOURCE_TOUCHPAD);
        put("touchnavigation", InputDevice.SOURCE_TOUCH_NAVIGATION);
        put("joystick", InputDevice.SOURCE_JOYSTICK);
    }};


    /**
     * Command-line entry point.
     *
     * @param args The command-line arguments
     */

    private class T1Thread extends Thread {
        LocalSocket t1socket;
        T1Thread(LocalSocket localSock) {
            t1socket = localSock;
        }

        public void run() {
            try {
                InputStream in = t1socket.getInputStream();
                OutputStream out = t1socket.getOutputStream();

                InputStreamReader ir = new InputStreamReader(in);
                OutputStreamWriter ow = new OutputStreamWriter(out);

                StringBuilder sb = new StringBuilder();
                char[] buffer = new char[1];
                while (ir.read(buffer) == 1) {
                    if (buffer[0] == '\n') {
                        if (doT1Command(sb.toString()) < 0) {
                            ir.close();
                            ow.close();
                            break;
                        } else {
                            ow.write("input ok\n");
                            ow.flush();
                        }
                        sb.setLength(0);
                    } else {
                        sb.append(buffer);
                    }
                }
            } catch (Throwable e) {
                System.out.println("Error " + e.getMessage());
                e.printStackTrace();
            }
        }
    }
    private static void loadAm(String jarFile) throws Exception {
        System.err.println("before classLoader");
        DexClassLoader classLoader = new DexClassLoader(jarFile, optCacheDirName, null, mInput.getClass().getClassLoader());
        System.err.println("after classLoader");
        Class<?> amClass = classLoader.loadClass("com.android.commands.am.Am");
        if (amClass != null) {
            Constructor<?> amConstructor = amClass.getConstructor();
            mAm = amConstructor.newInstance();
            mAmRun = mAm.getClass().getMethod("run", String[].class);
        }
    }

    private static void initAm() {
        if (mAm == null) {
            try {
                loadAm("/system/framework/am.jar");
            } catch (ClassNotFoundException e) {
                String jarFile = "/data/data/com.android.shell/am.jar";
                try {
                    loadAm(jarFile);
                } catch (Throwable e2) {
                    System.out.println("Error " + e2.getMessage());
                    e2.printStackTrace();
                }
            } catch (Throwable e) {
                System.out.println("Error " + e.getMessage());
                e.printStackTrace();
            }
        }
    }

    public static void main(String[] args) {
        File t1OptimizeDir = new File(optCacheDirName);
        t1OptimizeDir.mkdir();

        mInput = new Input();
        initAm();
        mInput.startServer();
    }

    private void startServer() {
        try {
            LocalServerSocket t1WrenchServer = new LocalServerSocket("T1Wrench");
            while (true) {
                LocalSocket t1socket = t1WrenchServer.accept();
                if (!checkPerm(t1socket.getFileDescriptor())) {
                    System.err.println("socket permission denied");
                    t1socket.close();
                    continue;
                }
                T1Thread t1Thread = new T1Thread(t1socket);
                t1Thread.start();
            }
        }
        catch (Throwable e) {
            System.out.println("Error " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static Input mInput;
    private static Object mAm;
    private static Method mAmRun;
    private static synchronized int doT1Command(String cmd) {
        System.err.println("cmd is " + cmd);
        String[] args = cmd.split("\\s+");
        if (args.length == 0)
            return 0;
        String args0 = args[0];
        args = Arrays.copyOfRange(args, 1, args.length);
        if (args0.equals("input")) {
            mInput.run(args);
        } else if (args0.equals("am")) {
            try {
                initAm();
                mAmRun.invoke(mAm, (Object)args);
            } catch (Throwable e) {
                System.out.println("Error " + e.getMessage());
                e.printStackTrace();
                mAm = null;
            }
        } else if (args0.equals("exit")) {
            return -1;
        }
        return 0;
    }

    private void run(String[] args) {
        if (args.length < 1) {
            showUsage();
            return;
        }

        int index = 0;
        String command = args[index];
        int inputSource = InputDevice.SOURCE_UNKNOWN;
        if (SOURCES.containsKey(command)) {
            inputSource = SOURCES.get(command);
            index++;
            command = args[index];
        }
        final int length = args.length - index;

        try {
            if (command.equals("text")) {
                if (length == 2) {
                    inputSource = getSource(inputSource, InputDevice.SOURCE_KEYBOARD);
                    sendText(inputSource, args[index+1]);
                    return;
                }
            } else if (command.equals("keyevent")) {
                if (length >= 2) {
                    final boolean longpress = "--longpress".equals(args[index + 1]);
                    final int start = longpress ? index + 2 : index + 1;
                    inputSource = getSource(inputSource, InputDevice.SOURCE_KEYBOARD);
                    if (length > start) {
                        for (int i = start; i < length; i++) {
                            int keyCode = KeyEvent.keyCodeFromString(args[i]);
                            if (keyCode == KeyEvent.KEYCODE_UNKNOWN) {
                                keyCode = KeyEvent.keyCodeFromString("KEYCODE_" + args[i]);
                            }
                            sendKeyEvent(inputSource, keyCode, longpress);
                        }
                        return;
                    }
                }
            } else if (command.equals("tap")) {
                if (length == 3) {
                    inputSource = getSource(inputSource, InputDevice.SOURCE_TOUCHSCREEN);
                    sendTap(inputSource, Float.parseFloat(args[index+1]),
                            Float.parseFloat(args[index+2]));
                    return;
                }
            } else if (command.equals("swipe")) {
                int duration = -1;
                inputSource = getSource(inputSource, InputDevice.SOURCE_TOUCHSCREEN);
                switch (length) {
                    case 6:
                        duration = Integer.parseInt(args[index+5]);
                    case 5:
                        sendSwipe(inputSource,
                                Float.parseFloat(args[index+1]), Float.parseFloat(args[index+2]),
                                Float.parseFloat(args[index+3]), Float.parseFloat(args[index+4]),
                                duration);
                        return;
                }
            } else if (command.equals("press")) {
                inputSource = getSource(inputSource, InputDevice.SOURCE_TRACKBALL);
                if (length == 1) {
                    sendTap(inputSource, 0.0f, 0.0f);
                    return;
                }
            } else if (command.equals("roll")) {
                inputSource = getSource(inputSource, InputDevice.SOURCE_TRACKBALL);
                if (length == 3) {
                    sendMove(inputSource, Float.parseFloat(args[index+1]),
                            Float.parseFloat(args[index+2]));
                    return;
                }
            } else {
                System.err.println("Error: Unknown command: " + command);
                showUsage();
                return;
            }
        } catch (NumberFormatException ex) {
        }
        System.err.println(INVALID_ARGUMENTS + command);
        showUsage();
    }

    /**
     * Convert the characters of string text into key event's and send to
     * device.
     *
     * @param text is a string of characters you want to input to the device.
     */
    private void sendText(int source, String text) {

        StringBuffer buff = new StringBuffer(text);

        boolean escapeFlag = false;
        for (int i=0; i<buff.length(); i++) {
            if (escapeFlag) {
                escapeFlag = false;
                if (buff.charAt(i) == 's') {
                    buff.setCharAt(i, ' ');
                    buff.deleteCharAt(--i);
                }
            }
            if (buff.charAt(i) == '%') {
                escapeFlag = true;
            }
        }

        char[] chars = buff.toString().toCharArray();

        KeyCharacterMap kcm = KeyCharacterMap.load(KeyCharacterMap.VIRTUAL_KEYBOARD);
        KeyEvent[] events = kcm.getEvents(chars);
        for(int i = 0; i < events.length; i++) {
            KeyEvent e = events[i];
            if (source != e.getSource()) {
                e.setSource(source);
            }
            injectKeyEvent(e);
        }
    }

    private void sendKeyEvent(int inputSource, int keyCode, boolean longpress) {
        long now = SystemClock.uptimeMillis();
        injectKeyEvent(new KeyEvent(now, now, KeyEvent.ACTION_DOWN, keyCode, 0, 0,
                KeyCharacterMap.VIRTUAL_KEYBOARD, 0, 0, inputSource));
        if (longpress) {
            injectKeyEvent(new KeyEvent(now, now, KeyEvent.ACTION_DOWN, keyCode, 1, 0,
                    KeyCharacterMap.VIRTUAL_KEYBOARD, 0, KeyEvent.FLAG_LONG_PRESS,
                    inputSource));
        }
        injectKeyEvent(new KeyEvent(now, now, KeyEvent.ACTION_UP, keyCode, 0, 0,
                KeyCharacterMap.VIRTUAL_KEYBOARD, 0, 0, inputSource));
    }

    private void sendTap(int inputSource, float x, float y) {
        long now = SystemClock.uptimeMillis();
        injectMotionEvent(inputSource, MotionEvent.ACTION_DOWN, now, x, y, 1.0f);
        injectMotionEvent(inputSource, MotionEvent.ACTION_UP, now, x, y, 0.0f);
    }

    private void sendSwipe(int inputSource, float x1, float y1, float x2, float y2, int duration) {
        if (duration < 0) {
            duration = 300;
        }
        long now = SystemClock.uptimeMillis();
        injectMotionEvent(inputSource, MotionEvent.ACTION_DOWN, now, x1, y1, 1.0f);
        long startTime = now;
        long endTime = startTime + duration;
        while (now < endTime) {
            long elapsedTime = now - startTime;
            float alpha = (float) elapsedTime / duration;
            injectMotionEvent(inputSource, MotionEvent.ACTION_MOVE, now, lerp(x1, x2, alpha),
                    lerp(y1, y2, alpha), 1.0f);
            now = SystemClock.uptimeMillis();
        }
        injectMotionEvent(inputSource, MotionEvent.ACTION_UP, now, x2, y2, 0.0f);
    }

    /**
     * Sends a simple zero-pressure move event.
     *
     * @param inputSource the InputDevice.SOURCE_* sending the input event
     * @param dx change in x coordinate due to move
     * @param dy change in y coordinate due to move
     */
    private void sendMove(int inputSource, float dx, float dy) {
        long now = SystemClock.uptimeMillis();
        injectMotionEvent(inputSource, MotionEvent.ACTION_MOVE, now, dx, dy, 0.0f);
    }

    private void injectKeyEvent(KeyEvent event) {
        injectEvent(event);
    }

    private void injectEvent(InputEvent event) {
        try {
            Log.i(TAG, "injectKeyEvent: " + event);
            Field inject_input_event_mode_wait_for_finish = InputManager.class.getField("INJECT_INPUT_EVENT_MODE_WAIT_FOR_FINISH");
            Method inst = InputManager.class.getDeclaredMethod("getInstance");
            InputManager im = (InputManager)(inst.invoke(null));

            Method injectInputEvent = InputManager.class.getDeclaredMethod("injectInputEvent", InputEvent.class, Integer.TYPE);
            injectInputEvent.invoke(im, event, inject_input_event_mode_wait_for_finish.getInt(null));

        }
        catch (Throwable e) {
            System.out.println("Error " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * Builds a MotionEvent and injects it into the event stream.
     *
     * @param inputSource the InputDevice.SOURCE_* sending the input event
     * @param action the MotionEvent.ACTION_* for the event
     * @param when the value of SystemClock.uptimeMillis() at which the event happened
     * @param x x coordinate of event
     * @param y y coordinate of event
     * @param pressure pressure of event
     */
    private void injectMotionEvent(int inputSource, int action, long when, float x, float y, float pressure) {
        final float DEFAULT_SIZE = 1.0f;
        final int DEFAULT_META_STATE = 0;
        final float DEFAULT_PRECISION_X = 1.0f;
        final float DEFAULT_PRECISION_Y = 1.0f;
        final int DEFAULT_DEVICE_ID = 0;
        final int DEFAULT_EDGE_FLAGS = 0;
        MotionEvent event = MotionEvent.obtain(when, when, action, x, y, pressure, DEFAULT_SIZE,
                                               DEFAULT_META_STATE, DEFAULT_PRECISION_X, DEFAULT_PRECISION_Y, DEFAULT_DEVICE_ID,
                                               DEFAULT_EDGE_FLAGS);
        event.setSource(inputSource);
        Log.i(TAG, "injectMotionEvent: " + event);
        injectEvent(event);
    }

    private static final float lerp(float a, float b, float alpha) {
        return (b - a) * alpha + a;
    }

    private static final int getSource(int inputSource, int defaultSource) {
        return inputSource == InputDevice.SOURCE_UNKNOWN ? defaultSource : inputSource;
    }

    private void showUsage() {
        System.err.println("Usage: input [<source>] <command> [<arg>...]");
        System.err.println();
        System.err.println("The sources are: ");
        for (String src : SOURCES.keySet()) {
            System.err.println("      " + src);
        }
        System.err.println();
        System.err.println("The commands and default sources are:");
        System.err.println("      text <string> (Default: touchscreen)");
        System.err.println("      keyevent [--longpress] <key code number or name> ..."
                + " (Default: keyboard)");
        System.err.println("      tap <x> <y> (Default: touchscreen)");
        System.err.println("      swipe <x1> <y1> <x2> <y2> [duration(ms)]"
                + " (Default: touchscreen)");
        System.err.println("      press (Default: trackball)");
        System.err.println("      roll <dx> <dy> (Default: trackball)");
    }
}
