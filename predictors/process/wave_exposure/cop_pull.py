import copernicusmarine

#r = [str(i) for i in range(2010,2024, 1)]
r = [str(i) for i in range(2019,2024, 1)]

for i in r:
    yr = i
    st = yr + "-01-01T00:00:00"
    et = yr + "-12-31T23:59:59"
    of = "cmems_wav_" + yr + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="cmems_mod_bal_wav_my_PT1H-i",
      variables=["VCMX", "VHM0"],
      minimum_longitude=9.013799667358398,
      maximum_longitude=30.20800018310547,
      minimum_latitude=53.0082893371582,
      maximum_latitude=65.90809631347656,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

