import copernicusmarine

r = [str(i) for i in range(2010,2024, 1)]

for i in r:
    yr = i
    st = yr + "-01-01T00:00:00"
    et = yr + "-12-31T23:59:59"
    of = "bal_plankton_multi_" + yr + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="cmems_obs-oc_bal_bgc-plankton_my_l3-multi-1km_P1D",
      variables=["CHL", "MICRO", "NANO", "PICO"],
      minimum_longitude=9.252659797668457,
      maximum_longitude=30.24734115600586,
      minimum_latitude=53.251346588134766,
      maximum_latitude=65.8486557006836,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

