package com.matburt.mobileorg.Gui.Capture;

import android.app.Activity;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TableLayout;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.view.ViewGroup.LayoutParams;
import android.widget.AbsListView;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import com.actionbarsherlock.app.SherlockFragment;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.matburt.mobileorg.Gui.Capture.DateTableRow.DateTableRowListener;
import com.matburt.mobileorg.OrgData.OrgNodePayload;
import com.matburt.mobileorg.R;
import java.io.File;
import java.util.ArrayList;

public class ImagesFragment extends SherlockFragment {

    private ListView imagesView;
    private Context mContext;
    private OnImagesModifiedListener mListener;

	public interface OnImagesModifiedListener {
		public void onImagesModified();
	}
	
	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
        mContext = activity.getApplicationContext();
		try {
            mListener = (OnImagesModifiedListener) activity;
        } catch (ClassCastException e) {
            throw new ClassCastException(activity.toString() + " must implement OnImagesModifiedListener");
        }
	}
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
        Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 55));
		setHasOptionsMenu(true);
		this.imagesView = new ListView(getActivity());
        imagesView.setAdapter(new PhotoAdapter(mContext));
		return imagesView;
	}
	
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		EditHost host = (EditHost) getActivity();
		
		if(savedInstanceState != null)
			restoreInstanceState(savedInstanceState);
		else
			setupImages();
		
		try {
			EditActivity activity = (EditActivity) host;
			activity.invalidateOptionsMenu();
		}
		catch (ClassCastException e) {}
    }

	public void setupImages() {
	}

    public void restoreInstanceState(Bundle savedInstanceState) {
		if(savedInstanceState != null) {
		}
	}


	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
	}
	
	public void setModifable(boolean enabled) {
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		inflater.inflate(R.menu.edit_images, menu);
		super.onCreateOptionsMenu(menu, inflater);
	}
	
	@Override
	public void onPrepareOptionsMenu(Menu menu) {
		super.onPrepareOptionsMenu(menu);
	}


	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_nodeedit_addimage:
            Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 200));
			return true;

		default:
			return super.onOptionsItemSelected(item);
		}
	}

    public class PhotoAdapter extends BaseAdapter {

        private ArrayList<String> mPhotos = new ArrayList<String>();
        
        public PhotoAdapter(Context c) {
            mContext = c;
            addPhotos("images/Screenshot_2017-01-14-19-28-35-102.png");
        }

        public int getCount() {
            return mPhotos.size();
        }

        public Object getItem(int position) {
            return position;
        }

        public long getItemId(int position) {
            return position;
        }

        public View getView(int position, View convertView, ViewGroup parent) {
            // Make an ImageView to show a photo
            Log.e("bhj", String.format("%s:%d: ", "ImagesFragment.java", 142));
            ImageView i = new ImageView(mContext);
            if (mPhotos.size() <= position) {
                return i;
            }
            
            File imgFile = new  File("/sdcard/MobileOrg/" + mPhotos.get(position));

            if (!imgFile.exists()) {
                return i;
            }
            
            Bitmap myBitmap = BitmapFactory.decodeFile(imgFile.getAbsolutePath());
            i.setImageBitmap(myBitmap);

            i.setAdjustViewBounds(true);
            i.setLayoutParams(new AbsListView.LayoutParams(
                                  LayoutParams.WRAP_CONTENT, 100));
            // Give it a nice background
            return i;
        }

        private Context mContext;

        public void addPhotos(String img) {
            mPhotos.add(img);
            notifyDataSetChanged();
        }

    }

}
