package com.baohaojun.crossdict;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class PowerReceiver extends BroadcastReceiver {
    public PowerReceiver() {
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        if (Intent.ACTION_POWER_CONNECTED.equals(action)) {
            Log.e("bhj", String.format("%s:%d: ", "PowerReceiver.java", 15));
            ClipMonService.setPowerConnected(true);
        } else if (Intent.ACTION_POWER_DISCONNECTED.equals(action)) {
            ClipMonService.setPowerConnected(false);
        }
    }
}
