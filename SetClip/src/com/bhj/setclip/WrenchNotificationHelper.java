package com.bhj.setclip;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import android.app.Notification;
import android.net.LocalServerSocket;
import android.net.LocalSocket;
import android.os.Bundle;
import android.service.notification.NotificationListenerService;
import android.service.notification.NotificationListenerService.*;
import android.service.notification.StatusBarNotification;
import android.util.Log;
import android.widget.Toast;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class WrenchNotificationHelper extends NotificationListenerService {
    private LocalSocket notificationSocket;
    @Override
    public void onCreate()  {
        super.onCreate();
        Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 17));
        new Thread(new Runnable() {
                @Override
                public void run() {
                    LocalServerSocket t1WrenchServer;
                    try {
                        t1WrenchServer = new LocalServerSocket("T1WrenchNotifications");
                    } catch(Exception e) {
                        Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 34), e);
                        return;
                    }

                    while (true) {
                        notificationSocket = null;
                        BufferedReader reader = null;
                        BufferedWriter writer = null;
                        try {
                            notificationSocket = t1WrenchServer.accept();
                            reader =
                                new BufferedReader(new InputStreamReader(notificationSocket.getInputStream()));
                            writer =
                                new BufferedWriter(new OutputStreamWriter(notificationSocket.getOutputStream()));
                            String line;
                            while ((line = reader.readLine()) != null) {
                                writer.write("got a line: " + line + "\n");

                                StatusBarNotification[] notifications = getActiveNotifications();
                                for (StatusBarNotification sn : notifications) {
                                    Notification n = sn.getNotification();
                                    Bundle extra = n.extras;
                                    CharSequence title = extra.getCharSequence(Notification.EXTRA_TITLE, "no title");
                                    CharSequence text = extra.getCharSequence(Notification.EXTRA_TEXT, "no text");
                                    writer.write("got a notification: title: " + title + ", text: " + text + "\n");
                                }
                                writer.flush();
                            }
                            reader.close();
                            writer.close();
                            notificationSocket.close();
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
        Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 22));
    }

    @Override
    public void onNotificationPosted(StatusBarNotification sbn)  {
        Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 34));
    }
}
