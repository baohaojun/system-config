package ${packageName};

import android.app.Service;
import android.content.Intent;
import android.os.IBinder;

public class ${className} extends Service {
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
}
