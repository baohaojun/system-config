package com.bhj.setclip;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import android.app.Notification;
import android.app.PendingIntent.CanceledException;
import android.app.PendingIntent;
import android.net.LocalServerSocket;
import android.net.LocalSocket;
import android.net.LocalSocketAddress;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.service.notification.NotificationListenerService.*;
import android.service.notification.NotificationListenerService;
import android.service.notification.StatusBarNotification;
import android.system.Os;
import android.util.Log;
import android.widget.Toast;
import com.Wrench.Input;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.HashMap;
import java.util.Set;
import org.json.JSONException;
import org.json.JSONObject;

public class WrenchNotificationHelper extends NotificationListenerService {
    private Handler mHandler;
    private static final int gotCommandFromWrench = 1;
    private static final int gotNewNotification = 2;

    private HashMap<LocalSocket, Integer> mSocketMap = new HashMap<LocalSocket, Integer>();

    private synchronized void addNewSocket(LocalSocket sock) {
        mSocketMap.put(sock, mSocketMap.size());
    }

    private synchronized LocalSocket getSocket(int sockNumber) {
        for (LocalSocket sock : mSocketMap.keySet()) {
            if (mSocketMap.get(sock).equals(sockNumber)) {
                return sock;
            }
        }
        return null;
    }

    private synchronized int getSocketInt (LocalSocket sockParam) {
        for (LocalSocket sock : mSocketMap.keySet()) {
            if (sock == sockParam) {
                return mSocketMap.get (sock);
            }
        }
        return -1;
    }

    private synchronized void delAllSocks () {
        for (LocalSocket sock : mSocketMap.keySet ()) {
            try {
                sock.close ();
            } catch (IOException e) {
                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 70), e);
            }
        }
        mSocketMap.clear();
    }

    private synchronized void delSocket(LocalSocket sock) {
        if (mSocketMap.containsKey(sock)) {
            mSocketMap.remove(sock);
            try {
                sock.close();
            } catch (IOException e) {
                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 61), e);
            }
        } else {
            Log.e("bhj", String.format("%s:%d: sock already deleted", "WrenchNotificationHelper.java", 64));
        }
    }

    private synchronized Set<LocalSocket> allSockets() {
        return mSocketMap.keySet();
    }

    private boolean mShouldExit = false;

    private synchronized boolean shouldExit() {
        return mShouldExit;
    }

    private synchronized void setShouldExit() {
        mShouldExit = true;
    }

    private void listStatusBarNotifications(LocalSocket sock) {
        StatusBarNotification[] notifications = getActiveNotifications();
        try {
            BufferedWriter writer =
                new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));

            for (StatusBarNotification sbn : notifications) {
                Bundle extra = makeBundleFromSbn(sbn);
                String joString = joStringFromBundle(extra);
                writer.write(joString + "\n");
            }
            writer.flush();
        } catch (IOException e) {
            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 49), e);
            delSocket (sock);
            try {
                sock.close ();
            } catch (IOException eClose) {
                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 117), eClose);
            }
        }
    }

    private Bundle makeBundleFromSbn(StatusBarNotification sbn) {
        Notification n = sbn.getNotification();
        Bundle extra = new Bundle(n.extras);

        if (n.tickerText != null) {
            extra.putString("tickerText", n.tickerText.toString());
        }
        
        extra.putString("key", sbn.getKey());
        extra.putString("pkg", sbn.getPackageName());

        return extra;
    }

    private String joStringFromBundle(Bundle extra) {
        String title = extra.getCharSequence(Notification.EXTRA_TITLE, "").toString();
        String text = extra.getCharSequence(Notification.EXTRA_TEXT, "").toString();
        String key = extra.getString("key");
        String pkg = extra.getString("pkg");
        String ticker = extra.getString("tickerText", "");

        JSONObject jo = new JSONObject();

        try {
            jo.put("key", key);
            jo.put("pkg", pkg);
            jo.put("title", title);
            jo.put("text", text);
            jo.put("ticker", ticker);
        } catch (JSONException e) {
            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 65), e);
        }
        return jo.toString();
    }


    private void clickNotification(String line) {
        String key = line.replaceAll("click ", "");
        key = key.replaceAll("\n", "");

        StatusBarNotification[] notifications = getActiveNotifications(new String[] {key});
        Log.e("bhj", String.format("%s:%d: notifications: %d, key %s", "WrenchNotificationHelper.java", 51, notifications.length, key));
        for (StatusBarNotification sbn : notifications) {
            Notification n = sbn.getNotification();
            PendingIntent i = n.contentIntent;
            if (i != null) {
                try {
                    Log.e("bhj", String.format("%s:%d: can send intent", "WrenchNotificationHelper.java", 93));
                    i.send();
                    break;
                } catch (CanceledException e) {
                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 64), e);
                }

            } else {
                if (n.actions == null) {
                    Log.e("bhj", String.format("%s:%d: has no action ", "WrenchNotificationHelper.java", 53));
                    continue;
                }
                for (Notification.Action action : n.actions) {
                    PendingIntent i2 = action.actionIntent;
                    if (i2 != null) {
                        try {
                            i2.send();
                        } catch (CanceledException e) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 57), e);
                        }

                    }
                }
            }
        }
    }

    LocalServerSocket WrenchServer;
    private boolean mShouldSkip = false;
    private boolean mConnected = false;

    @Override
    public void onCreate()  {
        super.onCreate();
        if (Os.getuid() > 20000) {
            // This if-clause is to prevent Smartisan's clone app
            // feature from trying to start another
            // NotificationListener.  The "clone app" feature is the
            // one that allow you to open 2 wechat/qq etc.
            mShouldSkip = true;
            Log.e("bhj", String.format("%s:%d: should not run notification listener for %d", "WrenchNotificationHelper.java", 207, Os.getuid()));
            return;
        } else {
            Log.e("bhj", String.format("%s:%d: onCreate", "WrenchNotificationHelper.java", 211));
        }

        mHandler = new Handler() {
                public void handleMessage(Message msg) {
                    if (!mConnected) {
                        return;
                    }
                    switch (msg.what) {
                    case gotCommandFromWrench:
                        {
                            Bundle extra = msg.getData();
                            String line = extra.getString("line");
                            int sockNumber = extra.getInt("sock");
                            LocalSocket sock = getSocket (sockNumber);
                            Log.e("bhj", String.format("%s:%d: line is %s", "WrenchNotificationHelper.java", 46, line));
                            if (line.matches("^click .*")) {
                                clickNotification(line);
                            } else if (line.matches("^list") && sock != null) {
                                listStatusBarNotifications(sock);
                            }
                        }
                        break;
                    case gotNewNotification:
                        for (LocalSocket sock : allSockets ()) {
                            try {
                                BufferedWriter lockedWriter = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream()));
                                if (lockedWriter != null) {
                                    Bundle extra = msg.getData();


                                    String joString = joStringFromBundle(extra);
                                    joString = joString.replaceAll("\n", " ");
                                    lockedWriter.write(joString + "\n");
                                    lockedWriter.flush();

                                } else {
                                    Log.e("bhj", String.format("%s:%d: Writer is null in gotNewNotification", "WrenchNotificationHelper.java", 57));
                                }
                            } catch (IOException e) {
                                delSocket (sock);
                                try {
                                    sock.close ();
                                } catch (IOException eClose) {
                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 52), eClose);
                                }
                            }
                        }

                        break;
                    default:
                        super.handleMessage(msg);
                    }
                }
            };

        new Thread(new Runnable() {
                @Override
                public void run() {
                    Log.e("bhj", String.format("%s:%d: new thread at %d", "WrenchNotificationHelper.java", 148, this.hashCode()));
                    try {
                        WrenchServer = new LocalServerSocket("WrenchNotifications");

                    } catch(IOException e) {
                        try {
                            LocalSocket closeSocket = new LocalSocket();
                            Log.e("bhj", String.format("%s:%d: before connect", "WrenchNotificationHelper.java", 156));
                            closeSocket.connect(new LocalSocketAddress("WrenchNotifications"));
                            Log.e("bhj", String.format("%s:%d: after connect", "WrenchNotificationHelper.java", 158));

                            BufferedReader reader =
                                new BufferedReader(new InputStreamReader(closeSocket.getInputStream()));
                            BufferedWriter writer =
                                new BufferedWriter(new OutputStreamWriter(closeSocket.getOutputStream()));

                            try {
                                reader.readLine();

                                writer.write("close yourself\n");
                                writer.flush();
                                Log.e("bhj", String.format("%s:%d: after write", "WrenchNotificationHelper.java", 166));
                                reader.readLine();
                            } catch (IOException e3) {
                                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 173), e3);
                            } finally {
                                reader.close();
                                writer.close();
                                closeSocket.close();
                            }

                            WrenchServer = new LocalServerSocket("WrenchNotifications");
                        } catch (IOException e2) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 169), e2);
                            return;
                        }
                    }


                    while (!shouldExit()) {
                        LocalSocket notificationSocket = null;
                        try {
                            notificationSocket = WrenchServer.accept();
                            if (!Input.checkPerm(notificationSocket.getFileDescriptor())) {
                                Log.e("bhj", String.format("%s:%d: only socket from shell is allowed", "WrenchNotificationHelper.java", 47));
                                try {
                                    notificationSocket.close();
                                } catch (IOException eClose) {
                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 267), eClose);
                                } finally {
                                    notificationSocket = null;
                                }
                                continue;
                            }
                        } catch (IOException eAccept) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 263), eAccept);
                            setShouldExit();
                            try {
                                notificationSocket.close();
                            } catch (IOException eClose) {
                                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 267), eClose);
                            }

                            for (LocalSocket sock : allSockets()) {
                                try {
                                    sock.close();
                                } catch (IOException eCloseSock) {
                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 275), eCloseSock);
                                }
                            }
                        }
                        try {
                            if (notificationSocket != null) {
                                final LocalSocket sockParam = notificationSocket;
                                addNewSocket (notificationSocket);

                                final BufferedReader locReader =
                                    new BufferedReader(new InputStreamReader(sockParam.getInputStream()));
                                final BufferedWriter locWriter =
                                    new BufferedWriter(new OutputStreamWriter(sockParam.getOutputStream()));

                                try {
                                    locWriter.write("notification ready\n");
                                    locWriter.flush();
                                    String line = locReader.readLine();
                                    Log.e("bhj", String.format("%s:%d: got first line: %s", "WrenchNotificationHelper.java", 325, line));
                                    if ("close yourself".equals (line)) {
                                        WrenchServer.close();
                                        setShouldExit ();
                                        delAllSocks ();
                                        return;
                                    }
                                } catch (IOException eReadCloseLine) {
                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 308), eReadCloseLine);
                                }


                                new Thread (new Runnable () {
                                        @Override
                                        public void run() {

                                            String line;
                                            try {
                                                while ((line = locReader.readLine()) != null) {
                                                    // writer.write("got a line: " + line + "\n");
                                                    Log.e("bhj", String.format("%s:%d: got line: %s", "WrenchNotificationHelper.java", 200, line));
                                                    Message msg = new Message();
                                                    msg.what = gotCommandFromWrench;
                                                    Bundle payload = new Bundle();
                                                    payload.putString("line", line);
                                                    payload.putInt("sock", getSocketInt (sockParam));
                                                    msg.setData(payload);
                                                    mHandler.sendMessage(msg);
                                                }
                                            } catch (IOException eWhile) {
                                                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 352), eWhile);
                                            } finally {
                                                Log.e("bhj", String.format("%s:%d: remote closed? got into finally", "WrenchNotificationHelper.java", 377));
                                                delSocket (sockParam);
                                                try {
                                                    sockParam.close();
                                                } catch (IOException eFinally) {
                                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 358), eFinally);
                                                }
                                            }
                                        }
                                    }).start ();
                            } 
                        } catch (IOException eLoop) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 377), eLoop);
                        }
                    }
                }
            }).start();
    }

    @Override
    public void onNotificationRemoved(StatusBarNotification sbn)  {
        Log.e("bhj", String.format("%s:%d: onNotificationRemoved()", "WrenchNotificationHelper.java", 22));
    }

    @Override
    public void onNotificationPosted(StatusBarNotification sbn)  {
        if (mShouldSkip) {
            return;
        }

        if (!mConnected) {
            return;
        }
        
        String pkg = sbn.getPackageName();
        if ("android".equals(pkg) ||
            "com.github.shadowsocks".equals(pkg) ||
            "com.bhj.setclip".equals(pkg) ||
            "com.android.systemui".equals(pkg)) {
            return;
        }

        Log.e("bhj", String.format("%s:%d: got message from %s", "WrenchNotificationHelper.java", 406, pkg));

        Bundle extra = makeBundleFromSbn(sbn);
        Message msg = new Message();
        msg.what = gotNewNotification;
        msg.setData(extra);
        mHandler.sendMessage(msg);
    }

    @Override
    public void onListenerConnected()
    {
        super.onListenerConnected();
        mConnected = true;
        Log.e("bhj", String.format("%s:%d: onListenerConnected(%d)", "WrenchNotificationHelper.java", 48, this.hashCode()));
    }
}
