import copernicusmarine

r = [str(i) for i in range(2010,2024, 1)]

for i in r:
    yr = i
    st = yr + "-01-01T00:00:00"
    et = yr + "-12-31T23:59:59"
    of = "cmems_seaice_" + yr + ".nc"
    od = "inputs/temp"
  
    copernicusmarine.subset(
      dataset_id="cmems_obs-si_bal_seaice-conc_my_1km",
      variables=["ice_concentration"],
      minimum_longitude=9,
      maximum_longitude=31,
      minimum_latitude=53.20000076293945,
      maximum_latitude=66.19999694824219,
      start_datetime=st,
      end_datetime=et,
      output_filename = of,
      output_directory = od
    )

