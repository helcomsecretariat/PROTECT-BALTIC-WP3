import copernicusmarine

r = [str(i) for i in range(2010,2024, 1)]

for i in r:
    yr = i
    st = yr + "-01-01T00:00:00"
    et = yr + "-12-31T23:59:59"
    of = "DMI_BAL_SST_" + yr + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="DMI_BAL_SST_L4_REP_OBSERVATIONS_010_016",
      variables=["analysed_sst"],
      #variables=["analysis_error"],
      minimum_longitude=-10,
      maximum_longitude=30,
      minimum_latitude=48,
      maximum_latitude=66,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

