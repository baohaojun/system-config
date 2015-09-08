package com.bhj.setclip;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningTaskInfo;
import android.app.Notification;
import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.ContentProvider;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.Environment;
import android.os.IBinder;
import android.provider.ContactsContract;
import android.provider.ContactsContract.CommonDataKinds.Phone;
import android.provider.ContactsContract.Contacts.Entity;
import android.provider.ContactsContract.Data;
import android.provider.ContactsContract.RawContacts;
import android.telephony.TelephonyManager;
import android.util.Log;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class PutClipService extends Service {
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate()  {
        super.onCreate();
        startForeground(1, new Notification());
        mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
    }

    private static String myClipStr;

    private String getTask() {
        ActivityManager am = (ActivityManager) getSystemService(ACTIVITY_SERVICE);
        RunningTaskInfo foregroundTaskInfo = am.getRunningTasks(1).get(0);
        return foregroundTaskInfo .topActivity.getPackageName();
    }

    private void writeFile(String str) throws IOException {
        FileWriter f = new FileWriter(new File(sdcard, "putclip.txt.1"));
        f.write(str);
        f.close();
        File txt = new File(sdcard, "putclip.txt.1");
        txt.renameTo(new File(sdcard, "putclip.txt"));

        writeFile(str, new File(sdcard, "putclip.txt.1"));
    }

    private void writeFile(String str, File file) throws IOException {
        FileWriter f = new FileWriter(file);
        f.write(str);
        f.close();
        File txt = new File(sdcard, "putclip.txt.1");
        txt.renameTo(new File(sdcard, "putclip.txt"));
    }

    ClipboardManager mClipboard;
    ClipboardManager.OnPrimaryClipChangedListener mClipListner;
    boolean mClipboardHooked = false;

    boolean mWatchingClipboard = true;

    private void startMonitorClipboard() {
        mWatchingClipboard = true;

        if (mClipboardHooked) {
            return;
        }

        if (mClipListner == null) {
            mClipListner = new ClipboardManager.OnPrimaryClipChangedListener() {
                    public void onPrimaryClipChanged() {
                        if (!mWatchingClipboard) {
                            return;
                        }
                        ClipData clip = mClipboard.getPrimaryClip();
                        if (clip != null) {
                            ClipData.Item item = clip.getItemAt(0);
                            CharSequence text = item.getText();
                            if (text == null) {
                                return;
                            }

                            String str = text.toString();

                            if (!str.equals(PutClipService.myClipStr)) {
                                Intent crossDictIntent = new Intent();
                                crossDictIntent.setClassName("com.baohaojun.crossdict", "com.baohaojun.crossdict.CrossDictActivity")
                                    .putExtra("android.intent.extra.TEXT", str)
                                    .setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                startActivity(crossDictIntent);
                            }
                        }
                    }
                };
        }
        if (!mClipboardHooked) {
            mClipboard.addPrimaryClipChangedListener(mClipListner);
            mClipboardHooked = true;
        }
    }

    @Override
    public int onStartCommand(Intent intent,  int flags,  int startId)  {
        try {
            if (intent == null) {
                return START_STICKY;
            }

            String picName = intent.getStringExtra("picture");
            if (picName != null) {
                Uri picUri = Uri.parse("file://" + picName);
                sendBroadcast(new Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, picUri));
            } else if (intent.getIntExtra("watch-clipboard", 0) == 1) {
                startMonitorClipboard();
                try {
                    writeFile("hello", new File(sdcard, ".t1wrench-watching-clipboard.txt"));
                } catch (Throwable e) {
                    Log.e("bhj", String.format("%s:%d: ", "PutClipService.java", 122), e);
                }

            } else if (intent.getIntExtra("stop-watch-clipboard", 0) == 1) {
                mWatchingClipboard = false;
                new File(sdcard, ".t1wrench-watching-clipboard.txt").delete();
            } else if (intent.getIntExtra("gettask", 0) == 1) {
                String foregroundTaskPackageName = getTask();
                writeFile(foregroundTaskPackageName);
            } else if (intent.getIntExtra("getclip", 0) == 1) {
                String str = mClipboard.getPrimaryClip().getItemAt(0).getText().toString();
                writeFile(str);
            } else if (intent.getIntExtra("getphone", 0) == 1) {
                TelephonyManager tMgr =(TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
                String mPhoneNumber = tMgr.getLine1Number();
                writeFile(mPhoneNumber);
            } else if (intent.getIntExtra("listcontacts", 0) == 1) {
                String data2 = intent.getStringExtra("data2");
                String data3 = intent.getStringExtra("data3");

                if (data2 == null) {
                    data2 = "微信";
                }
                if (data3 == null) {
                    data3 = "发送消息";
                }

                ContentResolver resolver = getContentResolver();
                if (resolver == null) {
                    return START_STICKY;
                }

                String[] projection = new String[] {
                    Data.DATA1,
                };
                String selection = ContactsContract.Data.DATA2 + "=?" + " AND " +
                    ContactsContract.Data.DATA3 + "=?";
                String[] selectArgs = new String[] {
                    data2, data3,
                };

                Cursor dc = resolver.query(ContactsContract.Data.CONTENT_URI,
                                           projection,
                                           selection,
                                           selectArgs,
                                           null);

                try {
                    File txt = new File(sdcard, "listcontacts.txt.1");
                    FileWriter f = new FileWriter(txt);
                    if (dc.moveToFirst()) {
                        do {
                            f.write(normalizedPhone(dc.getString(0)) + "\n");
                        } while (dc.moveToNext());
                    }
                    f.close();
                    txt.renameTo(new File(sdcard, "listcontacts.txt"));
                } finally {
                    dc.close();
                }
            } else if (intent.getIntExtra("share-to-note", 0) == 1) {
                FileReader f = new FileReader(new File(sdcard, "putclip.txt"));
                char[] buffer = new char[1024 * 1024];
                int n = f.read(buffer);
                String str = new String(buffer, 0, n);
                File putclip = new File(sdcard, "putclip.txt");
                putclip.delete();

                Intent notesIntent = new Intent();
                notesIntent.setClassName("com.smartisanos.notes", "com.smartisanos.notes.NotesActivity");
                notesIntent.setAction(Intent.ACTION_SEND);
                notesIntent.setType("text/plain");
                notesIntent.putExtra(Intent.EXTRA_TEXT, str);
                notesIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                startActivity(notesIntent);
            } else if (intent.getIntExtra("getapk", 0) == 1) {
                try {
                    PackageManager pm = getPackageManager();
                    PackageInfo pi = pm.getPackageInfo("com.bhj.setclip", 0);

                    FileWriter f = new FileWriter(new File(sdcard, "setclip-apk.txt.1"));
                    f.write(String.format("CLASSPATH=%s LD_LIBRARY_PATH=%s", pi.applicationInfo.sourceDir, pi.applicationInfo.nativeLibraryDir));
                    f.close();
                    new File(sdcard, "setclip-apk.txt.1").renameTo(new File(sdcard, "setclip-apk.txt"));
                }
                catch (Throwable e) {
                    Log.e("bhj", String.format("%s:%d: ", "PutClipService.java", 134), e);
                }

                if (new File(sdcard, ".t1wrench-watching-clipboard.txt").exists()) {
                    startMonitorClipboard();
                } else {
                    mWatchingClipboard = false;
                }

            } else if (intent.getIntExtra("getcontact", 0) == 1) {
                String contactNumber = intent.getStringExtra("contact");
                if (contactNumber == null) {
                    return START_STICKY;
                }

                ContentResolver resolver = getContentResolver();
                if (resolver == null) {
                    return START_STICKY;
                }

                // get the last 4 bytes of number, compare.
                // if not? compare the whole thing? or not?

                String last4 = "%" + contactNumber.substring(contactNumber.length() - 4);
                String selection = ContactsContract.Data.DATA2 + "=?" + " AND " +
                    ContactsContract.Data.DATA3 + "=?" + " AND " +
                    ContactsContract.Data.DATA1 + " like ?";

                Cursor dataCursor = resolver.query(ContactsContract.Data.CONTENT_URI,
                                                   new String[] {Data._ID, Data.DATA1, Data.DATA2, Data.DATA3},
                                                   selection,
                                                   new String[] {"微信", "发送消息", last4},
                                                   null);

                try {
                    if (dataCursor.moveToFirst()) {
                        do {
                            String phone = dataCursor.getString(1);
                            if (normalizedPhone(phone).equals(normalizedPhone(contactNumber))) {
                                Intent mmIntent = new Intent();
                                mmIntent.setClassName("com.tencent.mm", "com.tencent.mm.plugin.accountsync.ui.ContactsSyncUI");
                                mmIntent.setType("vnd.android.cursor.item/vnd.com.tencent.mm.chatting.profile");
                                mmIntent.setData(Uri.parse("content://com.android.contacts/data/" + dataCursor.getLong(0)));
                                mmIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                startActivity(mmIntent);
                                break;
                            }
                        } while (dataCursor.moveToNext());
                    }
                } finally {
                    dataCursor.close();
                }
            } else {
                FileReader f = new FileReader(new File(sdcard, "putclip.txt"));
                char[] buffer = new char[1024 * 1024];
                int n = f.read(buffer);
                myClipStr = new String(buffer, 0, n);
                mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", myClipStr));
                File putclip = new File(sdcard, "putclip.txt");
                putclip.delete();
            }
        } catch (Exception e) {
            Log.e("bhj", String.format("%s:%d: ", "PutClipService.java", 77), e);
        }
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
    }

    private String normalizedPhone(String phone) {
        String res = phone.replaceAll("[-+ ]", "");
        if (res.length() >= 11) {
            res = res.substring(res.length() - 11);
        }
        return res;
    }

    private static File sdcard = Environment.getExternalStorageDirectory();
}
