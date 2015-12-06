package com.bhj.setclip;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import android.service.notification.NotificationListenerService;
import android.service.notification.NotificationListenerService.*;
import android.service.notification.StatusBarNotification;
import android.util.Log;
import android.widget.Toast;

public class WrenchNotificationHelper extends NotificationListenerService {
    @Override
    public void onCreate()  {
        super.onCreate();
        Log.e("bhj", String.format("%s:%d: ", "WrenchNotificationHelper.java", 17));
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
