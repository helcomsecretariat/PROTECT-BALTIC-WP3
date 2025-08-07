import copernicusmarine
import calendar

#r = [str(i) for i in range(2021,2025, 1)]
yr = 2024
r = range(11, 12)

for i in r:
    mth = i
    mth = str(mth).zfill(2)
    min_day = str(1).zfill(2)
    max_day = calendar.monthrange(yr, i)[1]
    st = str(yr) + "-" + mth + "-" + min_day + "T00:00:00"
    et = str(yr) + "-" + mth + "-" + str(max_day) + "T23:59:59"
    #et = yr + "-12-31T23:59:59"
    of = "OCEANCOLOUR_BAL_BGC_HR_L4_" + str(yr) + "_" + mth + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="cmems_obs_oc_bal_bgc_tur-spm-chl_nrt_l4-hr-mosaic_P1D-m",
      variables=["TUR"],
      minimum_longitude=8.999825296995105,
      maximum_longitude=31.000174703004888,
      minimum_latitude=53.00046296296296,
      maximum_latitude=65.99953703703704,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

