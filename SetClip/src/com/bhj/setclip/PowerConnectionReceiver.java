package com.bhj.setclip;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.BatteryManager;
import android.util.Log;
import java.io.File;
import java.io.IOException;

public class PowerConnectionReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS, -1);
        boolean isCharging = status == BatteryManager.BATTERY_STATUS_CHARGING ||
                            status == BatteryManager.BATTERY_STATUS_FULL;

        int chargePlug = intent.getIntExtra(BatteryManager.EXTRA_PLUGGED, -1);
        boolean usbCharge = chargePlug == BatteryManager.BATTERY_PLUGGED_USB;
        boolean acCharge = chargePlug == BatteryManager.BATTERY_PLUGGED_AC;
        File flagFile = new File("/sdcard/Wrench/usb_online");
        try {
            if (intent.getAction() == Intent.ACTION_POWER_DISCONNECTED) {
                flagFile.delete();
            } else if (intent.getAction() == Intent.ACTION_POWER_CONNECTED) {
                flagFile.createNewFile();
            } else {
                Log.e("bhj", String.format("%s:%d: ", "PowerConnectionReceiver.java", 27));
            }
        } catch (IOException e) {
            Log.e("bhj", String.format("%s:%d: ", "PowerConnectionReceiver.java", 30), e);
        }
    }
}

