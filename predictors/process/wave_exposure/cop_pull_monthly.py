import copernicusmarine
import calendar
import xarray as xr
import os

for j in range(2010, 2024):

  yr = j
  
  for i in range(1, 13):
    mth = i
    mth = str(mth).zfill(2)
    min_day = str(1).zfill(2)
    max_day = calendar.monthrange(yr, i)[1]
    st = str(yr) + "-" + mth + "-" + min_day + "T00:00:00"
    et = str(yr) + "-" + mth + "-" + str(max_day) + "T23:59:59"
    #et = yr + "-12-31T23:59:59"
    of = "cmems_wav_" + str(yr) + "_" + mth + ".nc"
    od = "inputs/temp/" + str(yr)
    copernicusmarine.subset(
      dataset_id="cmems_mod_bal_wav_my_PT1H-i",
      variables=["VHM0"],
      minimum_longitude=9.013799667358398,
      maximum_longitude=30.20800018310547,
      minimum_latitude=53.0082893371582,
      maximum_latitude=65.90809631347656,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )
    
    fp = od + "/" + of
    ofp = od + "/" + "cmems_wav_" + str(yr) + "_" + mth + "_month_mean.nc"
    ds = xr.open_dataset(fp)
    monthly_mean = ds['VHM0'].mean(dim='time', keep_attrs=True)
    monthly_mean.to_netcdf(ofp)
    xr.Dataset.close(ds)
    xr.Dataset.close(monthly_mean)
    os.remove(fp)
    print('Monthly mean for month ' + str(mth) + ' saved to ' + ofp)
  
