package com.mrvl.autotest.project;

import android.app.Activity;

import java.lang.Integer;
import android.content.Context;
import android.content.Intent;
import android.location.Criteria;
import android.location.GpsStatus;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;
import com.mrvl.autotest.R;

public class PGpsTest extends Activity {
	private static String TAG = "PGpsTest";
    private LocationManager mLocationManager;
    private TextView nmeatext, locationtext, gmttime, vstate, astate, ttftime,
            snf, height;
    private String nmea_text = "";
    private int i = 0;
    private long k;
    private int count = 0;
    private int location_tick = 0;
    private String num_total = "";
    private int num_state = 0;
    private String snf_state = "";

    private String buf[] = { "", "", "", "", "", "", "", "", "", "", "", "",
            "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
            "", "", "" };

    private String buff[] = { "01", "02", "03", "04", "05", "06", "07", "08",
            "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
            "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
            "31", "32" };

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.project_gps);

        nmeatext = (TextView) findViewById(R.id.NMEA_data);
        locationtext = (TextView) findViewById(R.id.latitude_and_longitude);
        gmttime = (TextView) findViewById(R.id.gmt_time);
        vstate = (TextView) findViewById(R.id.visible_satellite);
        astate = (TextView) findViewById(R.id.available_satellite);
        ttftime = (TextView) findViewById(R.id.time_to_first_fix);
        snf = (TextView) findViewById(R.id.State_data);
        height = (TextView) findViewById(R.id.height_and_sudu);
        mLocationManager = (LocationManager) getSystemService(Context.LOCATION_SERVICE);

        Criteria criteria = new Criteria();
        criteria.setAccuracy(Criteria.ACCURACY_FINE);
        criteria.setAltitudeRequired(false);
        criteria.setBearingRequired(false);
        criteria.setCostAllowed(true);
        criteria.setPowerRequirement(Criteria.POWER_LOW);
        // String providers=mLocationManager.getBestProvider(criteria,true);
        // Location location=mLocationManager.getLastKnownLocation(providers);
        // updateWithNewLocation(location);

        String provider = LocationManager.GPS_PROVIDER;
        mLocationManager.requestLocationUpdates(provider, 0, 0,
                locationListener);
        mLocationManager.addNmeaListener(nmeaListener);
        k = System.currentTimeMillis();
        if (mLocationManager.isProviderEnabled(mLocationManager.GPS_PROVIDER) != true) {
            Intent callGPSSettingIntent = new Intent(
                    android.provider.Settings.ACTION_LOCATION_SOURCE_SETTINGS);
            startActivity(callGPSSettingIntent);
        }
    }

    @Override
    protected void onDestroy() {
        // TODO Auto-generated method stub
        super.onDestroy();
        mLocationManager.removeUpdates(locationListener);
        mLocationManager.removeNmeaListener(nmeaListener);

    }

    public String formatLongToTimeStr(Long l) {

        int hour = 0;
        int minute = 0;
        int second = 0;

        second = l.intValue() / 1000;

        if (second >= 60) {
            minute = second / 60;
            second = second % 60;
        }
        if (minute >= 60) {
            hour = minute / 60;
            minute = minute % 60;
        }
        Integer h = new Integer(hour);
        Integer m = new Integer(minute);
        Integer s = new Integer(second);
        return (h.toString() + getResources().getString(R.string.hh) + m.toString() + getResources().getString(R.string.mm)
                + s.toString() + getResources().getString(R.string.ss));
    }

    private final GpsStatus.NmeaListener nmeaListener = new GpsStatus.NmeaListener() {
        public void onNmeaReceived(long timestamp, String nmea) {
            updateNmeaStatus(timestamp, nmea);
        }
    };

    private final LocationListener locationListener = new LocationListener() {
        public void onLocationChanged(Location location) {
            Log.w("TestGps", "now location changed");
            updateWithNewLocation(location);
        }

        public void onProviderDisabled(String provider) {
            Log.w("TestGps", "now provider disable");
        }

        public void onProviderEnabled(String provider) {
            Log.w("TestGps", "now provider enable");
        }

        public void onStatusChanged(String provider, int status, Bundle extras) {
            Log.w("TestGps", "now provider status changed" + status);
        }
    };

    private void updateNmeaStatus(Long timestamp, String nmea) {

        String gmt_text = getResources().getString(R.string.gtm_time);
        String v_state = getResources().getString(R.string.v_state);
        String a_state = getResources().getString(R.string.a_state);
        String ttf_time = getResources().getString(R.string.ttf_time);

        int num2 = 0;
        int num3 = 0;
        int num_tick;

        String[] sPlit = nmea.split(",");

        if (sPlit[0].equals("$GPGGA")) {
            Integer i = new Integer(sPlit[7]);
            int j = i.intValue();

            if (location_tick == 1 && count == 0) {
                long k_time = System.currentTimeMillis();
                k = k_time - k;

                ttf_time = ttf_time + formatLongToTimeStr(k);

                ttftime.setText(ttf_time);
                count++;

            }
            if (j >= 4) {
                location_tick = 1;
            }
            gmt_text = gmt_text + sPlit[1].substring(0, 2) + getResources().getString(R.string.hh)
                    + sPlit[1].substring(2, 4) + getResources().getString(R.string.mm) + sPlit[1].substring(4)
                    + getResources().getString(R.string.ss);
            a_state = a_state + sPlit[7];
            gmttime.setText(gmt_text);
            astate.setText(a_state);
        }

        else if (sPlit[0].equals("$GPGSV")) {
            num_total = sPlit[3];

            Integer num22 = new Integer(sPlit[2]);
            num2 = num22.intValue();
            Integer num33 = new Integer(sPlit[3]);
            num3 = num33.intValue();

            if (num3 != 0) {
                if (num2 == 1) {
                    num_state = num3;
                    snf_state = "";

                    for (int t = 0; t < buf.length; t++)
                        buf[t] = "";

                }
                if (num_state - 4 > 0) {
                    num_tick = 4;
                    num_state = num_state - 4;
                } else {
                    num_tick = num_state;
                }
                for (int t1 = 0; t1 < num_tick; t1++)
                    for (int t = 0; t < buff.length; t++)
                        if (sPlit[4 + t1 * 4].equals(buff[t]))
                            buf[t] = getResources().getString(R.string.sl_no)+ sPlit[4 + t1 * 4] + getResources().getString(R.string.sl_elevation)
                                    + sPlit[5 + t1 * 4] + getResources().getString(R.string.sl_azimuth)
                                    + sPlit[6 + t1 * 4] + getResources().getString(R.string.sign)
                                    + sPlit[7 + t1 * 4] + getResources().getString(R.string.nn);

            } else {
                snf_state = "";
                for (int t = 0; t < buf.length; t++)
                    buf[t] = "";
            }

        } else {
            v_state = v_state + num_total;
            vstate.setText(v_state);
            snf_state = "";
            for (int t = 0; t < buf.length; t++)
                snf_state = snf_state + buf[t];
            snf.setText(getResources().getString(R.string.State_data)+"\n"+snf_state);

            num_state = 0;
        }

        if (i < 6) {
            if (i == 0) {
                if (sPlit[0].equals("$GPGGA")) {
                    nmea_text = nmea_text + nmea + getResources().getString(R.string.nn);
                    i++;
                } else
                    nmea_text = "";
            } else {
                nmea_text = nmea_text + nmea + getResources().getString(R.string.nn);
                i++;
            }
        } else {
        	Log.d("TestGps", "nNNNMMMMAMAMAMAMAM= "+getResources().getString(R.string.NMEA_data));
            nmeatext.setText(getResources().getString(R.string.NMEA_data)+"\n"+nmea_text);
            i = 0;
            nmea_text = "";
        }
    }

    private void updateWithNewLocation(Location location) {
        String latLongString;
        String height_s = null;
        double lat, hei, su, lng = 0;
        if (location != null) {
            lat = location.getLatitude();
            lng = location.getLongitude();
            hei = location.getAltitude();
            su = location.getSpeed();

            height_s = getResources().getString(R.string.high) + hei
                    + getResources().getString(R.string.velocity) + su;
            latLongString = getResources().getString(R.string.latitude) + lat
                    + getResources().getString(R.string.precision) + lng;
        } else {
            latLongString = getResources().getString(R.string.err);
        }
        // if(location_tick==1)
        // {
        locationtext.setText(latLongString);
        height.setText(height_s);
        if (lng > 0)
            location_tick = 1;
        // }
    }

}
