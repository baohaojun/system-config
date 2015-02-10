package com.bhj.setclip;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningTaskInfo;
import android.app.Service;
import android.content.ClipboardManager;
import android.content.ClipData;
import android.content.ContentProvider;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
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

    private String getTask() {
        ActivityManager am = (ActivityManager) getSystemService(ACTIVITY_SERVICE);
        RunningTaskInfo foregroundTaskInfo = am.getRunningTasks(1).get(0);
        return foregroundTaskInfo .topActivity.getPackageName();
    }

    private void writeFile(String str) throws IOException {
        FileWriter f = new FileWriter(new File(Environment.getExternalStorageDirectory(), "putclip.txt.1"));
        f.write(str);
        f.close();
        File txt = new File(Environment.getExternalStorageDirectory(), "putclip.txt.1");
        txt.renameTo(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
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
            } else if (intent.getIntExtra("gettask", 0) == 1) {
                String foregroundTaskPackageName = getTask();
                writeFile(foregroundTaskPackageName);
            } else if (intent.getIntExtra("getclip", 0) == 1) {
                ClipboardManager mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
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
                    File txt = new File(Environment.getExternalStorageDirectory(), "listcontacts.txt.1");
                    FileWriter f = new FileWriter(txt);
                    if (dc.moveToFirst()) {
                        do {
                            f.write(normalizedPhone(dc.getString(0)) + "\n");
                        } while (dc.moveToNext());
                    }
                    f.close();
                    txt.renameTo(new File(Environment.getExternalStorageDirectory(), "listcontacts.txt"));
                } finally {
                    dc.close();
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
                FileReader f = new FileReader(new File(Environment.getExternalStorageDirectory(), "putclip.txt"));
                char[] buffer = new char[1024 * 1024];
                int n = f.read(buffer);
                String str = new String(buffer, 0, n);
                ClipboardManager mClipboard;
                mClipboard = (ClipboardManager)getSystemService(CLIPBOARD_SERVICE);
                mClipboard.setPrimaryClip(ClipData.newPlainText("Styled Text", str));
                File putclip = new File(Environment.getExternalStorageDirectory(), "putclip.txt");
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
}
