package com.bhj.setclip;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import android.app.Notification;
import android.app.PendingIntent;
import android.app.PendingIntent.CanceledException;
import android.net.LocalServerSocket;
import android.net.LocalSocket;
import android.net.LocalSocketAddress;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.service.notification.NotificationListenerService;
import android.service.notification.NotificationListenerService.*;
import android.service.notification.StatusBarNotification;
import android.util.Log;
import android.widget.Toast;
import com.Wrench.Input;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import org.json.JSONException;
import org.json.JSONObject;

public class WrenchNotificationHelper extends NotificationListenerService {
    private static volatile BufferedReader reader = null;
    private static volatile BufferedWriter writer = null;
    private static volatile WrenchNotificationHelper activeHelper = null;
    private static volatile int threadHashCode = 0;
    private Handler mHandler;
    private static final int gotCommandFromWrench = 1;
    private static final int gotNewNotification = 2;
    @Override
    public void onCreate()  {
        super.onCreate();
        mHandler = new Handler() {
                public void handleMessage(Message msg) {
                    switch (msg.what) {
                    case gotCommandFromWrench:
                        {
                            Bundle extra = msg.getData();
                            String line = extra.getString("line");
                            Log.e("bhj", String.format("%s:%d: line is %s", "WrenchNotificationHelper.java", 46, line));
                            if (line.matches("^click .*")) {
                                String key = line.replaceAll("click ", "");
                                key = key.replaceAll("\n", "");
                                StatusBarNotification[] notifications = getActiveNotifications();
                                Log.e("bhj", String.format("%s:%d: notifications: %d, key %s", "WrenchNotificationHelper.java", 51, notifications.length, key));
                                for (StatusBarNotification sbn : notifications) {
                                    if (! sbn.getKey().equals(key)) {
                                        Log.e("bhj", String.format("%s:%d: sbn key: %s", "WrenchNotificationHelper.java", 54, sbn.getKey()));
                                        continue;
                                    }
                                    Notification n = sbn.getNotification();
                                    PendingIntent i = n.contentIntent;
                                    if (i != null) {
                                        try {
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
                        }
                        break;
                    case gotNewNotification:
                        Log.e("bhj", String.format("%s:%d: gotNewNotification", "WrenchNotificationHelper.java", 36));
                        BufferedWriter lockedWriter = null;
                        lockedWriter = writer;
                        if (lockedWriter != null) {
                            Bundle extra = msg.getData();
                            try {
                                CharSequence title = extra.getCharSequence(Notification.EXTRA_TITLE, "no title");
                                CharSequence text = extra.getCharSequence(Notification.EXTRA_TEXT, "no text");
                                String key = extra.getString("key");
                                String pkg = extra.getString("pkg");

                                JSONObject jo = new JSONObject();

                                try {
                                    jo.put("key", key);
                                    jo.put("pkg", pkg);
                                    jo.put("title", title.toString());
                                    jo.put("text", text.toString());
                                } catch (JSONException e) {
                                    Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 65), e);
                                }
                                String joString = jo.toString();
                                joString = joString.replaceAll("\n", " ");
                                lockedWriter.write(joString + "\n");
                                lockedWriter.flush();

                            } catch (IOException e) {
                                Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 52), e);
                            }
                        } else {
                            Log.e("bhj", String.format("%s:%d: Writer is null in gotNewNotification", "WrenchNotificationHelper.java", 57));
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
                    LocalServerSocket t1WrenchServer;

                    int myHashCode = this.hashCode();
                    Log.e("bhj", String.format("%s:%d: myHashCode is %d, threadHashCode = %d", "WrenchNotificationHelper.java", 199, myHashCode, threadHashCode));
                    threadHashCode = myHashCode;

                    try {
                        t1WrenchServer = new LocalServerSocket("WrenchNotifications");

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

                            t1WrenchServer = new LocalServerSocket("WrenchNotifications");
                        } catch (IOException e2) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 169), e2);
                            return;
                        }
                    }



                    while (true) {
                        LocalSocket notificationSocket = null;
                        try {
                            notificationSocket = t1WrenchServer.accept();
                            if (!Input.checkPerm(notificationSocket.getFileDescriptor())) {
                                Log.e("bhj", String.format("%s:%d: only socket from shell is allowed", "WrenchNotificationHelper.java", 47));
                                notificationSocket.close();
                                continue;
                            }
                            BufferedReader locReader = reader =
                                new BufferedReader(new InputStreamReader(notificationSocket.getInputStream()));
                            BufferedWriter locWriter = writer =
                                new BufferedWriter(new OutputStreamWriter(notificationSocket.getOutputStream()));
                            locWriter.write("notification ready\n");
                            locWriter.flush();
                            String line;
                            while ((line = locReader.readLine()) != null) {
                                // writer.write("got a line: " + line + "\n");
                                Log.e("bhj", String.format("%s:%d: got line: %s", "WrenchNotificationHelper.java", 200, line));
                                if (line.equals("close yourself")) {
                                    Log.e("bhj", String.format("%s:%d: got close", "WrenchNotificationHelper.java", 194));
                                    t1WrenchServer.close();

                                    BufferedReader r = locReader;
                                    BufferedWriter w = locWriter;
                                    LocalSocket s = notificationSocket;
                                    notificationSocket = null;
                                    r.close();
                                    w.close();
                                    s.close();
                                    return;
                                }

                                if (myHashCode != threadHashCode) {
                                    Log.e("bhj", String.format("%s:%d: thread changed", "WrenchNotificationHelper.java", 194));
                                    break;
                                }


                                // StatusBarNotification[] notifications = getActiveNotifications();
                                // for (StatusBarNotification sn : notifications) {
                                //     Notification n = sn.getNotification();
                                //     Bundle extra = n.extras;
                                //     CharSequence title = extra.getCharSequence(Notification.EXTRA_TITLE, "no title");
                                //     CharSequence text = extra.getCharSequence(Notification.EXTRA_TEXT, "no text");
                                //     locWriter.write("got a notification: key: " + sn.getKey() + ", title: " + title + ", text: " + text + "\n");
                                // }
                                // locWriter.flush();

                                Message msg = new Message();
                                msg.what = gotCommandFromWrench;
                                Bundle payload = new Bundle();
                                payload.putString("line", line);
                                msg.setData(payload);
                                mHandler.sendMessage(msg);
                            }
                            locReader.close();
                            locReader = null;
                            locWriter.close();
                            locWriter = null;
                            notificationSocket.close();
                            notificationSocket = null;
                        } catch(Exception e) {
                            Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 28), e);
                            if (reader != null) {
                                try {
                                    reader.close();
                                } catch (Exception x) {
                                }
                            }
                            if (writer != null) {
                                try {
                                    writer.close();
                                } catch (Exception x) {

                                }
                            }
                            if (notificationSocket != null) {
                                try {
                                    notificationSocket.close();
                                } catch (Exception x) {

                                }
                            }
                            reader = null;
                            writer = null;
                            notificationSocket = null;
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
        Notification n = sbn.getNotification();
        Bundle extra = new Bundle(n.extras);
        extra.putString("key", sbn.getKey());
        extra.putString("pkg", sbn.getPackageName());
        CharSequence text = extra.getCharSequence(Notification.EXTRA_TEXT, "no text");
        CharSequence title = extra.getCharSequence(Notification.EXTRA_TITLE, "no title");
        Log.e("bhj", String.format("%s:%d: new notificaton: %s (%s) ", "WrenchNotificationHelper.java", 34, title, text));

        Message msg = new Message();
        msg.what = gotNewNotification;
        msg.setData(extra);
        mHandler.sendMessage(msg);
    }

    @Override
    public void onListenerConnected()
    {
        super.onListenerConnected();
        Log.e("bhj", String.format("%s:%d: onListenerConnected(%d)", "WrenchNotificationHelper.java", 48, this.hashCode()));
    }
}
