import calendar
import time as t
from datetime import date

import cdsapi
from dateutil.relativedelta import relativedelta

c = cdsapi.Client()

PARAMS = {
    'type': 'forecast',
    'format': 'netcdf_zip',
    'variable': 'particulate_matter_2.5um',
    'time': '00:00',
    'date': '2020-01-01/2021-01-01',
    'leadtime_hour': '0',
    "area": [5.28, -74, -17, -44]
}

START_DATE = date(2020, 1, 1)
END_DATE = date(2020, 12, 31)
TARGET = ''
times = ['00:00', '12:00']

dt = START_DATE

while dt < END_DATE:
    params = PARAMS.copy()
    dt_end = dt.replace(day=calendar.monthrange(dt.year, dt.month)[1])
    params["date"] = f'{dt.strftime("%Y-%m-%d")}/{dt_end.strftime("%Y-%m-%d")}'
    for time in times:
        params["time"] = time
        if time == "00:00:00":
            params["leadtime_hour"] = ["6", "9"]
        else:
            params["leadtime_hour"] = ["0", "3", "6", "9", "12", "15"]

        TARGET = f"./Data/Raw/CAMS/NRT/cams_{params['date'].replace('/', '-')}_{params['time'].replace(':', '-')}_.zip"
        print(f'Retriving {time} for {params["date"]}')
        c.retrieve(
            'cams-global-atmospheric-composition-forecasts',
            params,
            TARGET)
        t.sleep(10)
        print(f'Done {time} for {params["date"]}')
    print(f'Done {dt}')
    dt += relativedelta(months=1)
