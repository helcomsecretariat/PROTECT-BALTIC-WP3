import copernicusmarine

r = [str(i) for i in range(2010,2024, 1)]

for i in r:
    yr = i
    st = yr + "-01-01T00:00:00"
    et = yr + "-12-31T23:59:59"
    of = "cmems_mod_bal_bgc_" + yr + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="cmems_mod_bal_bgc_my_P1M-m",
      variables=["nh4"],
      minimum_longitude=9.041532516479492,
      maximum_longitude=30.20798683166504,
      minimum_latitude=53.00829315185547,
      maximum_latitude=65.89141845703125,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

