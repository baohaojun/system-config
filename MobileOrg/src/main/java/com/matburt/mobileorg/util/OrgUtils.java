package com.matburt.mobileorg.util;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.NetworkInfo.State;
import android.preference.PreferenceManager;
import android.text.TextUtils;
import android.util.Log;
import android.widget.ArrayAdapter;
import android.widget.Spinner;

import android.database.Cursor;
import android.net.Uri;
import android.os.Environment;
import android.provider.MediaStore;
import com.matburt.mobileorg.OrgData.OrgFile;
import com.matburt.mobileorg.OrgData.OrgNode;
import com.matburt.mobileorg.R;
import com.matburt.mobileorg.Synchronizers.Synchronizer;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

public class OrgUtils {
	
	public static String getTimestamp() {
		SimpleDateFormat sdf = new SimpleDateFormat("[yyyy-MM-dd EEE HH:mm]");		
		return sdf.format(new Date());
	}

    
    public static void setupSpinnerWithEmpty(Spinner spinner, ArrayList<String> data,
			String selection) {
		data.add("");
		setupSpinner(spinner, data, selection);
    }
    
	public static void setupSpinner(Spinner spinner, ArrayList<String> data,
			String selection) {		
		if(!TextUtils.isEmpty(selection) && !data.contains(selection))
			data.add(selection);
		
		ArrayAdapter<String> adapter = new ArrayAdapter<String>(spinner.getContext(),
				android.R.layout.simple_spinner_item, data);
		adapter.setDropDownViewResource(R.layout.edit_spinner_layout);
		spinner.setAdapter(adapter);
		int pos = data.indexOf(selection);
		if (pos < 0) {
			pos = 0;
		}
		spinner.setSelection(pos, true);
	}

    private static OrgNode getCaptureIntentImageContents(ContentResolver resolver, Intent intent) {
        String action = intent.getAction();
        OrgNode node = new OrgNode();
        if (! Intent.ACTION_SEND.equals(action)) {
            return node;
        }

        Uri imageUri = (Uri) intent.getParcelableExtra(Intent.EXTRA_STREAM);

        String[] projection = {MediaStore.MediaColumns.DATA};
        String sourceFilename = null;

        Cursor metaCursor = resolver.query(imageUri, projection, null, null, null);
        if (metaCursor != null) {
            try {
                if (metaCursor.moveToFirst()) {
                    sourceFilename = metaCursor.getString(0);
                }
            } catch (Exception e) {
                Log.e("bhj", String.format("%s:%d: ", "OrgUtils.java", 94), e);
            } finally {
                metaCursor.close();
            }
        }

        if (sourceFilename == null) {
            node.setPayload("[[invalid image file]]");
            return node;
        }
        
        File sdcard = Environment.getExternalStorageDirectory();
        File MobileOrgDir = new File(sdcard, "MobileOrg");
        File MobileOrgImgDir = new File(MobileOrgDir, "images");

        File sourceFile = new File(sourceFilename);
        File imgFile = new File(MobileOrgImgDir, sourceFile.getName());

        MobileOrgImgDir.mkdirs();

        BufferedInputStream bis = null;
        BufferedOutputStream bos = null;

        try {
            bis = new BufferedInputStream(resolver.openInputStream(imageUri));
            bos = new BufferedOutputStream(new FileOutputStream(imgFile, false));
            byte[] buf = new byte[1024];
            bis.read(buf);
            do {
                bos.write(buf);
            } while(bis.read(buf) != -1);
        } catch (IOException e) {
            Log.e("bhj", String.format("%s:%d: ", "OrgUtils.java", 103), e);
        } finally {
            try {
                if (bis != null) bis.close();
                if (bos != null) bos.close();
            } catch (IOException e) {
                Log.e("bhj", String.format("%s:%d: ", "OrgUtils.java", 109), e);
            }
        }

        node.setPayload("[[./images/" + sourceFile.getName() + "]]");
        return node;
    }

	public static OrgNode getCaptureIntentContents(ContentResolver resolver, Intent intent) {
        String type = intent.getType();
        if (type != null && type.startsWith("image/")) {
            return getCaptureIntentImageContents(resolver, intent);
        }
        
		String subject = intent
				.getStringExtra("android.intent.extra.SUBJECT");
		String text = intent.getStringExtra("android.intent.extra.TEXT");

		if(text != null && subject != null) {
			subject = "[[" + text + "][" + subject + "]]";
			text = "";
		}
		
		if(subject == null)
			subject = "";
		if(text == null)
			text = "";

		OrgNode node = new OrgNode();
		node.name = subject;
		node.setPayload(text);
		return node;
	}
	

	public static long getNodeFromPath(String path, ContentResolver resolver) throws OrgFileNotFoundException {
		String filename = path.substring("file://".length(), path.length());
		
		// TODO Handle links to headings instead of simply stripping it out
		if(filename.indexOf(":") > -1)
			filename = filename.substring(0, filename.indexOf(":"));
				
		OrgFile file = new OrgFile(filename, resolver);
		return file.nodeId;
	}
	
	public static void announceSyncDone(Context context) {
		Intent intent = new Intent(Synchronizer.SYNC_UPDATE);
		intent.putExtra(Synchronizer.SYNC_DONE, true);
		context.sendBroadcast(intent);
	}
	
	public static void announceSyncStart(Context context) {
		Intent intent = new Intent(Synchronizer.SYNC_UPDATE);
		intent.putExtra(Synchronizer.SYNC_START, true);
		context.sendBroadcast(intent);
	}
	
	public static void announceSyncUpdateProgress(int progress, Context context) {
		Intent intent = new Intent(Synchronizer.SYNC_UPDATE);
		intent.putExtra(Synchronizer.SYNC_PROGRESS_UPDATE, progress);
		context.sendBroadcast(intent);
	}

    public static String getStringFromResource(int resource, Context context) {
        InputStream is = context.getResources().openRawResource(resource);
        BufferedReader br = new BufferedReader(new InputStreamReader(is));
        String readLine = null;
        String contents = "";

        try {
            // While the BufferedReader readLine is not null 
            while ((readLine = br.readLine()) != null) {
                contents += readLine + "\n";
            }

            // Close the InputStream and BufferedReader
            is.close();
            br.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
        return contents;
    }
    

    public static class SortIgnoreCase implements Comparator<Object> {
        public int compare(Object o1, Object o2) {
            String s1 = (String) o1;
            String s2 = (String) o2;
            return s1.toLowerCase().compareTo(s2.toLowerCase());
        }
    }
    
    public static void setTheme(Activity activity) {
    	String themeName = PreferenceUtils.getThemeName();
    	
    	if(themeName.equals("Dark"))
    		activity.setTheme(R.style.Theme_MobileOrg_Dark);
    	else 
    		activity.setTheme(R.style.Theme_MobileOrg_Light);
    }
    
	public static byte[] serializeObject(Object o) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();

		try {
			ObjectOutput out = new ObjectOutputStream(bos);
			out.writeObject(o);
			out.close();

			byte[] buf = bos.toByteArray();

			return buf;
		} catch (IOException ioe) {
			Log.e("serializeObject", "error", ioe);

			return null;
		}
	}

	public static Object deserializeObject(byte[] b) {
		try {
			ObjectInputStream in = new ObjectInputStream(
					new ByteArrayInputStream(b));
			Object object = in.readObject();
			in.close();

			return object;
		} catch (ClassNotFoundException cnfe) {
			Log.e("deserializeObject", "class not found error", cnfe);

			return null;
		} catch (IOException ioe) {
			Log.e("deserializeObject", "io error", ioe);

			return null;
		}
	}
	
	
	/**
	 * @param keyID the ID of the StringArray that contains the labels
	 * @param valID the ID of the StringArray that contains the values
	 * @param value the value to search for
	 */
	public static String lookUpValueFromArray(Context context, int keyID, int valID, String value) {
		String[] keys = context.getResources().getStringArray(keyID);
		String[] values = context.getResources().getStringArray(valID);
		for (int i = 0; i < values.length; i++) {
			if (values[i].equals(value)) {
				return keys[i];
			}
		}
		return null;
	}
	
	
	public static boolean isWifiOnline(Context context) {
		ConnectivityManager conMan = (ConnectivityManager) context
				.getSystemService(Context.CONNECTIVITY_SERVICE);

		State wifi = conMan.getNetworkInfo(ConnectivityManager.TYPE_WIFI)
				.getState();

		if (wifi == NetworkInfo.State.CONNECTED)
			return true;
		else
			return false;
	}

	public static boolean isMobileOnline(Context context) {
		ConnectivityManager conMan = (ConnectivityManager) context
				.getSystemService(Context.CONNECTIVITY_SERVICE);

		State mobile = conMan.getNetworkInfo(ConnectivityManager.TYPE_MOBILE)
				.getState();

		if (mobile == NetworkInfo.State.CONNECTED)
			return true;
		else
			return false;
	}
	
	
	public static boolean isNetworkOnline(Context context) {
		SharedPreferences prefs = PreferenceManager
				.getDefaultSharedPreferences(context);
		boolean wifiOnly = prefs.getBoolean(
				context.getResources().getString(R.string.key_syncWifiOnly),
				false);

		if (wifiOnly)
			return OrgUtils.isWifiOnline(context);
		else
			return OrgUtils.isWifiOnline(context)
					|| OrgUtils.isMobileOnline(context);
	}
}
