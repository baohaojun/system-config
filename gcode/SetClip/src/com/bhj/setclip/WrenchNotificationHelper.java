package com.bhj.setclip;

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

import android.service.notification.NotificationListenerService;
import android.service.notification.NotificationListenerService.*;
import android.service.notification.StatusBarNotification;
import android.widget.Toast;

public class WrenchNotificationHelper extends NotificationListenerService {
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        Toast.makeText(this, "hello world", Toast.LENGTH_SHORT).show();
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
    }

    @Override
    public void onNotificationRemoved(StatusBarNotification sbn)  {
    }

    @Override
    public void onNotificationPosted(StatusBarNotification sbn)  {
    }
}
